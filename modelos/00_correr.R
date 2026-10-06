# ==============================================================================
# PRiSMA · Modelos de predicción del riesgo de reincidencia
#
# Cada señal de reincidencia va por separado y cruzada con el tipo de delito
# en el que la persona reincidió.
#
#   señal (rearresto, reimputación, caso abierto, rearresto policial)
#     x tipo de delito de la reincidencia (cualquiera, propiedad, violento,
#       drogas y armas)
#   = 16 respuestas, estimadas con dos motores sobre una ventana de un año.
#
#
# Este es el único archivo que se ejecuta. Todo lo demás vive en R/ y se carga
# desde acá. Para cambiar parámetros: R/01_config.R.
#
#   Rscript 00_correr.R           desde la terminal
#   source("00_correr.R")         desde RStudio, con el proyecto abierto
# ==============================================================================

RAIZ <- local({
  argumentos <- commandArgs(trailingOnly = FALSE)
  archivo <- sub("^--file=", "", argumentos[grep("^--file=", argumentos)])
  if (length(archivo) > 0) dirname(normalizePath(archivo)) else getwd()
})

if (!dir.exists(file.path(RAIZ, "R"))) {
  stop("No encuentro la carpeta R/. Abrí el proyecto en la carpeta PRiSMA ",
       "o corré el script con Rscript desde ahí. RAIZ actual: ", RAIZ)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(caret)
  library(pROC)
})

MODULOS <- c(
  "01_config.R", "02_utilidades.R", "03_carga.R", "04_historial.R",
  "05_panel.R", "06_motores.R", "07_entrenamiento.R", "08_metricas.R",
  "09_graficos.R", "10_comparativas.R", "11_robustez.R"
)

for (modulo in MODULOS) {
  ruta <- file.path(RAIZ, "R", modulo)
  if (!file.exists(ruta)) stop("Falta el módulo: ", modulo)
  source(ruta, encoding = "UTF-8")
}

if (!file.exists(RUTA_BASE)) {
  stop("No existe el archivo:\n  ", RUTA_BASE,
       "\nRevisá RUTA_BASE en R/01_config.R. Parquet en esa carpeta:\n  ",
       paste(list.files(dirname(RUTA_BASE), pattern = "\\.parquet$"),
             collapse = "\n  "))
}

theme_set(tema_tesis())
carpeta(SALIDAS)
carpeta(CACHE)

separador("PRiSMA · CORRIDA COMPLETA")
paso("salidas en: %s", SALIDAS)
paso("ventanas: %s días",
     paste(vapply(VENTANAS, function(v) v$dias, numeric(1)), collapse = " y "))
paso("respuestas: %s", paste(unname(RESPUESTAS), collapse = ", "))
paso("estratos por delito: %s", if (ESTRATIFICAR_POR_DELITO) "sí" else "no")
paso("caché: %s", if (USAR_CACHE) "encendido" else "apagado")
if (MODO_RAPIDO) paso("MODO_RAPIDO encendido: malla mínima y submuestra")

cluster <- NULL
if (USAR_PARALELO && requireNamespace("doParallel", quietly = TRUE) && NUCLEOS > 1) {
  cluster <- parallel::makePSOCKcluster(NUCLEOS)
  doParallel::registerDoParallel(cluster)
  paso("paralelo: %d núcleos", NUCLEOS)
  on.exit({
    parallel::stopCluster(cluster)
    foreach::registerDoSEQ()
  }, add = TRUE)
} else if (USAR_PARALELO) {
  paso('sin doParallel: corre secuencial   install.packages("doParallel")')
}

# ---- 1. Base -----------------------------------------------------------------

separador("1. BASE, HISTORIAL Y PREDICTORES")

clave_base <- sprintf("base_preparada_%s",
                      format(file.mtime(RUTA_BASE), "%Y%m%d%H%M%S"))

base <- con_cache(clave_base, {
  datos <- cargar_base()
  tope <- attr(datos, "tope_datos")
  cat("\n")
  datos <- agregar_historial(datos)
  datos <- agregar_predictores(datos)
  list(datos = datos, tope_datos = tope,
       predictores = attr(datos, "predictores"))
})

paso("predictores: %d", length(base$predictores))
cat(paste0("     ", base$predictores, collapse = "\n"), "\n")

cat("\n  Fuera del modelo a propósito:\n")
cat("    · TipoMedida, que es la decisión que el modelo busca informar\n")
cat("    · las variables de historial de la base, que cuentan también los\n")
cat("      procesos posteriores a la captura y filtran la respuesta\n")

MOTORES_ACTIVOS <- motores_disponibles(MOTORES)

# ---- 2. Una corrida por ventana, y dentro, una por grupo de delito -----------

desempenos <- list()
resumenes <- list()

for (ventana in VENTANAS) {

  separador(sprintf("VENTANA DE %d DÍAS", ventana$dias))

  datos <- armar_respuestas(base$datos, ventana, base$tope_datos)

  resumen <- resumir_respuestas(datos, ventana)
  resumenes[[as.character(ventana$dias)]] <- resumen

  anio_fin <- anio_fin_efectivo(ventana, base$tope_datos)
  armado <- armar_panel(datos, base$predictores, anio_fin)

  tablas_ventana <- carpeta_ventana(ventana$dias, ESTRATO_TODOS, "tablas")
  guardar_tabla(resumen, "tabla_respuestas", tablas_ventana)

  estratos <- estratos_del_panel(armado$panel, tablas_ventana)
  motor_ganador <- NULL

  for (estrato in estratos$activos) {

    es_todos <- estrato == ESTRATO_TODOS

    separador(sprintf("  ventana %d · %s", ventana$dias, estrato))

    tablas   <- carpeta_ventana(ventana$dias, estrato, "tablas")
    graficos <- carpeta_ventana(ventana$dias, estrato, "graficos")
    guardado <- carpeta_ventana(ventana$dias, estrato, "modelos")

    panel_estrato <- filtrar_estrato(armado$panel, estrato)

    # Dentro de un grupo, las dummies actual_<grupo> quedan constantes: se
    # descartan acá con el mismo criterio que usa armar_panel.
    predictores <- armado$predictores[
      vapply(panel_estrato[armado$predictores],
             function(x) dplyr::n_distinct(x) > 1, logical(1))
    ]

    paso("casos: %s · predictores: %d",
         miles(nrow(panel_estrato)), length(predictores))

    particion <- particionar(panel_estrato, ventana, anio_fin, tablas)

    # Los motores se comparan solo en el panel completo. En los grupos corre el
    # ganador, salvo que MOTORES_POR_ESTRATO diga lo contrario.
    motores <- if (es_todos || MOTORES_POR_ESTRATO == "todos") {
      MOTORES_ACTIVOS
    } else {
      motor_ganador
    }

    cat("\n")
    modelos <- entrenar_todo(particion, predictores, motores, ventana, estrato)

    if (is.null(modelos)) next

    cat("\n")
    desempeno <- evaluar_modelos(modelos, particion$evaluacion, ventana, estrato)
    desempenos[[paste(ventana$dias, estrato)]] <- desempeno

    if (es_todos || MOTORES_POR_ESTRATO == "todos") {
      salida_comparativa_motores(desempeno, tablas, graficos, ventana)
      salida_curvas_pr(modelos, particion$evaluacion, graficos, ventana)
    }

    mejores <- mejores_motores(desempeno)
    guardar_tabla(
      mejores |>
        select(Senal, Tipo, Motor, AUC, F1, `F1 máximo`, Precisión,
               Sensibilidad) |>
        mutate(across(where(is.numeric), ~ round(.x, 4))),
      "tabla_mejor_motor", tablas, mostrar = FALSE)

    motor_ref <- if (paste(MOTOR_REFERENCIA, RESPUESTA_PRINCIPAL, sep = "|")
                     %in% names(modelos)) {
      MOTOR_REFERENCIA
    } else {
      mejores$motor_id[mejores$respuesta_id == RESPUESTA_PRINCIPAL][1]
    }

    if (es_todos) {
      motor_ganador <- motor_ref
      paso("motor elegido para los grupos de delito: %s",
           etiqueta_motor(motor_ganador))
    }

    evaluacion <- agregar_riesgos(particion$evaluacion, modelos, motor_ref)

    # Desempeño completo en 0.5 y en Youden, las 16 filas.
    guardar_tabla(
      desempeno |>
        filter(motor_id == motor_ref) |>
        select(Senal, Tipo, AUC, Exactitud, Sensibilidad, Especificidad,
               F1, `Umbral Youden`, `Exactitud (Y)`, `Sensibilidad (Y)`,
               `Especificidad (Y)`) |>
        mutate(across(where(is.numeric), ~ round(.x, 4))),
      "tabla_desempeno", tablas, mostrar = FALSE)

    # La Tabla 6 estilo PRiSMA, con el documento al lado, y el panorama.
    salida_tabla_prisma(desempeno, motor_ref, tablas)

    # --- Lo común: lo que cruza señales o usa solo la respuesta principal ---
    cat("\n")
    salida_roc_por_tipo(evaluacion, graficos, ventana)
    salida_auc_anual(evaluacion, graficos, ventana)
    salida_otorgamiento(evaluacion, graficos, ventana)
    salida_deciles(evaluacion, graficos, ventana)
    salida_errores_politica(evaluacion, tablas)

    # --- Una carpeta por señal de reincidencia, con sus cuatro tipos adentro ---
    if (es_todos) {
      for (senal_id in names(SENALES)) {
        tablas_senal   <- carpeta_senal(ventana$dias, senal_id, "tablas")
        graficos_senal <- carpeta_senal(ventana$dias, senal_id, "graficos")
        ids <- ids_de_senal(senal_id)

        salida_tabla_senal(desempeno, motor_ref, tablas_senal, senal_id)
        salida_roc_senal(evaluacion, graficos_senal, ventana, senal_id)
        salida_auc_anual_senal(evaluacion, graficos_senal, ventana, senal_id)
        salida_calibracion(evaluacion, graficos_senal, ventana, ids,
                           etiqueta = SENALES[[senal_id]])
        salida_importancia(modelos, motor_ref, graficos_senal, ventana, ids,
                           etiqueta = SENALES[[senal_id]])
      }
      paso("una carpeta por señal en %s",
           file.path(SALIDAS, sprintf("ventana_%d", ventana$dias)))
    }

    # La robustez de partición aleatoria se corre solo en el panel completo:
    # mide un problema del diseño, no del grupo de delito.
    if (CORRER_ROBUSTEZ && es_todos) {
      cat("\n")
      correr_robustez(panel_estrato, predictores, motor_ref, desempeno,
                      tablas, ventana, estrato)
    }

    guardar_modelos(modelos, evaluacion, guardado, ventana)
  }
}

# ---- 3. Comparativas ---------------------------------------------------------

separador("COMPARATIVAS")

destino_tablas <- carpeta_comparativas("tablas")
destino_graficos <- carpeta_comparativas("graficos")

salida_resumen_ventanas(resumenes, destino_tablas)
salida_comparativa_ventanas(desempenos, destino_tablas, destino_graficos)
salida_comparativa_estratos(desempenos, destino_tablas, destino_graficos)

guardar_sesion(SALIDAS)

separador("LISTO")
paso("todo en %s", SALIDAS)
paso("modelos estimados: %d", sum(vapply(desempenos, nrow, numeric(1))))

# ==============================================================================
# Notas metodológicas
#
# 1. La partición es temporal, no aleatoria. La reincidencia se define con
#    eventos posteriores: al sortear al azar, esos eventos quedan a ambos lados
#    y el modelo aprende del futuro de los casos que después tiene que predecir.
#    La validación cruzada es de origen móvil por año, por la misma razón.
#
# 2. Los modelos van separados por grupo de delito de la captura. Es una
#    pregunta distinta de la del modelo global: no es qué predice la
#    reincidencia en general, sino si el riesgo de quien fue capturado por
#    hurto se predice con las mismas variables y con la misma precisión que el
#    de quien fue capturado por un delito violento.
#
# 3. Comparar F1 entre grupos tiene una trampa: cada grupo tiene su propia tasa
#    base de reincidencia y el F1 depende de ella. El AUC no, así que para
#    comparar entre grupos conviene mirar el gráfico de AUC. La tabla de
#    estratos trae las tasas de cada uno.
#
# 4. Dentro de un grupo, las dummies actual_<grupo> quedan constantes y se
#    descartan solas. Las previos_<grupo> se quedan: alguien capturado por
#    hurto puede tener antecedentes violentos, y eso es información.
#
# 5. El corte en 2020 deja la pandemia del lado del entrenamiento. Entre 2020 y
#    2021 los juzgados estuvieron cerrados o a media máquina.
#
# 6. Las variables de historial de la base son totales de toda la vida de la
#    persona e incluyen los procesos posteriores a la captura. Quien reincide
#    tiene por definición más procesos, así que usarlas es circular. Se
#    recalculan hacia atrás en 04_historial.R.
#
# 7. La pena del delito no se toma de pena_delito cuando viene del delito
#    sentenciado, porque la sentencia es posterior a la captura.
#
# 8. Etiquetas selectivas. Casi la mitad de la base no tiene ventana de
#    seguimiento: son los casos sin boleta de salida y sin fecha de medida. El
#    panel son los que recibieron medida o salieron con boleta, que no es una
#    mitad aleatoria de la población.
#
# 9. Las dos ventanas no son directamente comparables. Con 730 días la tasa de
#    reincidencia es más alta y el conjunto de evaluación es más chico: con
#    datos hasta junio de 2026, un año evalúa hasta 2025 y dos años hasta 2024.
#
# 10. joven es una dummy, no la edad en años. Recoge parte del efecto que en el
#     documento original capturaba la edad, pero pierde la forma de la
#     relación, que no es lineal ni monótona.
# ==============================================================================
