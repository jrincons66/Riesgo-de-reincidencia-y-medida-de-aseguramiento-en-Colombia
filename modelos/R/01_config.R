# ==============================================================================
# 01. Configuración
# Todo lo que se toca está acá. Los demás archivos no tienen números sueltos.
# ==============================================================================

# ---- Rutas -------------------------------------------------------------------

DESCARGAS <- "C:/Users/DiDi/Downloads"

RUTA_BASE <- file.path(DESCARGAS, "base_historial_escalonado.parquet")
SALIDAS   <- file.path(DESCARGAS, "salidas_modelos")
CACHE     <- file.path(SALIDAS, "_cache")

# ---- La ventana --------------------------------------------------------------
# Solo un año. La de 730 días queda acá comentada: para volver a correrla
# alcanza con descomentar la línea y el resto del pipeline la toma solo.

VENTANAS <- list(
  list(dias = 365, sufijo = "", columna_fin = "fecha_fin_seguimiento")
  # , list(dias = 730, sufijo = "_2", columna_fin = "fecha_fin_seguimiento_2")
)

# ---- Delitos -----------------------------------------------------------------

GRUPOS <- list(
  "Delitos contra la propiedad" = c("239", "240", "244", "246", "249", "250"),
  "Crimen violento" = c(
    "103", "104A", "104B", "105", "106", "108", "111", "119", "128", "135",
    "138", "139", "139A", "141", "141A", "141B", "205", "206", "208", "209",
    "210A", "213", "213A", "214", "217", "218", "219A", "229", "229A", "230",
    "230A"
  ),
  "Drogas y armas" = c("376", "365", "366")
)

GRUPO_RESTO  <- "Otros"
ORDEN_GRUPOS <- c(names(GRUPOS), GRUPO_RESTO)

# ---- Las respuestas ----------------------------------------------------------
# Cada señal de reincidencia va por separado, no unida, y cada una se cruza con
# el tipo de delito de la reincidencia. Son 4 x 4 = 16 respuestas.
#
# La pregunta que responde el cruce no es solo si la persona vuelve a caer,
# sino si vuelve a caer por algo violento, que es lo que le importa a quien
# decide la medida.

SENALES <- c(
  rearresto      = "Rearresto judicial",
  reimputacion   = "Reimputación",
  caso_abierto   = "Caso abierto",
  rearresto_poli = "Rearresto policial"
)

TIPOS <- c(
  general   = "Cualquier delito",
  economico = "Delitos contra la propiedad",
  violento  = "Crimen violento",
  otros     = "Drogas y armas"
)

# A qué grupo de GRUPOS corresponde cada tipo. "general" no filtra por delito.
GRUPO_DE_TIPO <- c(general = NA_character_,
                   economico = names(GRUPOS)[1],
                   violento  = names(GRUPOS)[2],
                   otros     = names(GRUPOS)[3])

RESPUESTAS <- character(0)
META_RESPUESTAS <- list()

for (senal in names(SENALES)) {
  for (tipo in names(TIPOS)) {
    id <- paste0("y_", senal, "_", tipo)
    RESPUESTAS[id] <- sprintf("%s · %s", SENALES[[senal]], TIPOS[[tipo]])
    META_RESPUESTAS[[id]] <- list(senal = senal, tipo = tipo)
  }
}

RESPUESTA_PRINCIPAL <- "y_rearresto_general"

# Los números de la Tabla 6 de PRiSMA, para ponerlos al lado de los propios.
# Son los del documento: XGBoost, partición aleatoria 70/30, validación
# cruzada de cinco grupos. Su única señal de reincidencia es la nueva captura
# en la base judicial, que es el rearresto de acá.
PRISMA_TABLA6 <- data.frame(
  tipo      = c("general", "economico", "violento", "otros"),
  AUC       = c(0.7735, 0.8874, 0.7406, 0.7765),
  Exactitud = c(0.7168, 0.7964, 0.6725, 0.6698),
  stringsAsFactors = FALSE
)

# La señal que corresponde a la de PRiSMA. La tabla comparativa usa esta fila
# de la grilla; las otras tres van como robustez.
SENAL_PRISMA <- "rearresto"

# Los gráficos que no se pueden facetear en 16 paneles (importancia,
# calibración) se hacen solo para estas: la fila de PRiSMA, o sea el rearresto
# legalizado por los cuatro tipos de delito.
RESPUESTAS_DESTACADAS <- paste0("y_", SENAL_PRISMA, "_", names(TIPOS))

# Calibración: puntos por reincidentes, no por casos. Cada punto es una tasa
# observada y lo que la hace estable es cuántos positivos tiene detrás.
CALIBRACION_LIMITE <- 1          # tope de los dos ejes; "auto" lo ajusta a los datos
CALIBRACION_POSITIVOS_POR_PUNTO <- 30
CALIBRACION_MINIMO_PUNTOS <- 20
CALIBRACION_MAXIMO_PUNTOS <- 400

# ---- Estratos por tipo de delito ---------------------------------------------
# Opcional y apagado por defecto. Además de los cuatro modelos de PRiSMA,
# vuelve a correrlos dentro de cada grupo de delito de la captura. Responde una
# pregunta distinta: si el riesgo de quien fue capturado por hurto se predice
# igual de bien que el de quien fue capturado por un delito violento. Sirve
# como sección de heterogeneidad, no como resultado principal.

ESTRATIFICAR_POR_DELITO <- FALSE
ESTRATO_TODOS <- "Todos los delitos"

# "mejor" usa el motor ganador del panel completo en cada grupo.
# "todos" estima los cuatro motores también dentro de cada grupo.
MOTORES_POR_ESTRATO <- "mejor"

# Un grupo con pocos positivos no da para estimar nada. Los que no lleguen se
# saltan avisando y quedan reportados en la tabla de estratos.
MINIMO_CASOS_ESTRATO <- 2000

# ---- Partición temporal ------------------------------------------------------
# La base va de 2007 a junio de 2026.
#
# El corte en 2020 deja el año de la pandemia del lado del entrenamiento. Los
# juzgados estuvieron cerrados o a media máquina entre 2020 y 2021, así que hay
# una discontinuidad estructural ahí: entrenar hasta 2019 y evaluar desde 2021
# sería aprender de un mundo y evaluar en otro. Si se prefiere que los dos lados
# sean post pandemia, subir esto a 2021.

ANIO_CORTE          <- 2020   # entrena hasta acá
ANIO_FIN_EVALUACION <- 2026   # evalúa hasta acá, con el ajuste de abajo
ANIO_INICIO_CV      <- 2012   # primer pliegue de origen móvil

# Un caso necesita que su ventana cierre dentro de los datos para saber si
# reincidió. Con 365 días el último año evaluable se corre a 2025 y con 730 a
# 2024. El ajuste es automático y queda reportado en la tabla de partición.
AJUSTAR_FIN_POR_VENTANA <- TRUE

# ---- Predictores -------------------------------------------------------------
# Estas columnas de la base son totales de toda la historia de la persona, no
# de lo anterior a la captura: quien reincide tiene por definición más procesos,
# y ese proceso futuro es justo lo que hay que predecir. En la muestra la media
# de n_procesos_cedula es 1,93 entre los que no reinciden y 6,67 entre los que
# sí. Quedan fuera y se reemplazan por las versiones hacia atrás que calcula
# 04_historial.R. Se pueden encender para ver el tamaño del efecto, pero los
# resultados de esa corrida no sirven para nada más.

COLUMNAS_CON_FUGA <- c(
  "penas_totales_cedula", "n_delitos_cedula", "n_procesos_cedula",
  "penas_acumuladas_radicado", "delitos_acumulados_radicado"
)

INCLUIR_COLUMNAS_CON_FUGA <- FALSE

# La medida de aseguramiento tampoco entra: es la decisión que el modelo busca
# informar y contra la cual se compara.
COLUMNA_MEDIDA   <- "TipoMedida"
VALOR_INTRAMURAL <- 1
BINS_RIESGO      <- 20

# ---- Motores -----------------------------------------------------------------

# Solo los dos que ganaron la comparación de F1. glmnet y rpart quedan en el
# catálogo por si hace falta rehacerla, pero no se estiman en cada corrida.
MOTORES <- c("ranger", "xgboost")

MOTOR_REFERENCIA  <- "xgboost"
METRICA_SELECCION <- "F1 máximo"

# ---- Peso y tamaño -----------------------------------------------------------

PESOS_DE_CLASE   <- TRUE
MINIMO_POSITIVOS <- 100
MAXIMO_BUSQUEDA  <- 40000    # tope de filas para la búsqueda; NULL usa todo

MODO_RAPIDO       <- FALSE
FILAS_MODO_RAPIDO <- 10000

# ---- Paralelo ----------------------------------------------------------------

USAR_PARALELO <- TRUE
NUCLEOS <- max(1, parallel::detectCores() - 1)

# ---- Caché -------------------------------------------------------------------
# Lo caro es el entrenamiento. Cada modelo se guarda por separado, así que
# agregar un motor no vuelve a estimar los anteriores. Para forzar todo de
# nuevo: limpiar_cache() o USAR_CACHE en FALSE.

USAR_CACHE <- TRUE

# ---- Robustez ----------------------------------------------------------------

CORRER_ROBUSTEZ          <- TRUE
PROPORCION_ENTRENAMIENTO <- 0.70

# ---- Semilla -----------------------------------------------------------------

SEMILLA <- 20260101
Sys.setenv(TZ = "UTC")
set.seed(SEMILLA)
options(dplyr.summarise.inform = FALSE)
