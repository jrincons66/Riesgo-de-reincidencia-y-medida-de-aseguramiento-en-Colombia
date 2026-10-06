# ==============================================================================
# 05. Panel y partición temporal
#
# El corte es por año de captura: el modelo aprende con el pasado y se evalúa
# con el futuro. Una partición aleatoria filtraría información, porque la
# reincidencia de un caso se define con eventos posteriores que estarían en el
# otro conjunto.
# ==============================================================================

# Las respuestas ya vienen de Python. Acá solo se eligen las columnas de la
# ventana y se marca qué casos son observables.
armar_respuestas <- function(datos, ventana, tope_datos) {

  columna_fin <- ventana$columna_fin
  columnas_senal <- paste0(names(SENALES), ventana$sufijo)
  names(columnas_senal) <- names(SENALES)

  faltan <- setdiff(columnas_senal, names(datos))
  if (length(faltan) > 0) {
    stop("Faltan señales de reincidencia en la base: ",
         paste(faltan, collapse = ", "))
  }

  # --- En qué delito reincidió ---
  # Aproximación: el grupo de la siguiente captura de la misma cédula, si cae
  # dentro de la ventana. Cada señal se dispara por un evento distinto y
  # pueden ser radicados diferentes, así que este grupo es el del proceso
  # siguiente, no necesariamente el del evento que disparó cada señal. Para
  # hacerlo exacto habría que guardar en Python el radicado que disparó cada
  # marca y traerse su delito.
  datos <- datos |>
    dplyr::arrange(IDENTIFICACION, FECHA) |>
    dplyr::group_by(IDENTIFICACION) |>
    dplyr::mutate(
      .fecha_siguiente = dplyr::lead(FECHA),
      .grupo_siguiente = dplyr::lead(as.character(grupo_delito))
    ) |>
    dplyr::ungroup()

  dentro <- !is.na(datos$.fecha_siguiente) &
    !is.na(datos$fecha_inicio_seguimiento) &
    datos$.fecha_siguiente >= datos$fecha_inicio_seguimiento &
    datos$.fecha_siguiente <= datos[[columna_fin]]

  datos$grupo_reincidencia <- dplyr::if_else(dentro, datos$.grupo_siguiente,
                                             NA_character_)

  # --- Las 16 respuestas ---
  for (id in names(RESPUESTAS)) {
    meta <- META_RESPUESTAS[[id]]
    senal <- datos[[columnas_senal[[meta$senal]]]]
    marca <- !is.na(senal) & senal == 1

    grupo <- GRUPO_DE_TIPO[[meta$tipo]]
    datos[[id]] <- if (is.na(grupo)) {
      as.integer(marca)
    } else {
      as.integer(marca & datos$grupo_reincidencia %in% grupo)
    }
  }

  # --- Observabilidad ---
  con_dato <- Reduce(`|`, lapply(columnas_senal,
                                 function(c) !is.na(datos[[c]])))
  alguna_marca <- Reduce(`|`, lapply(columnas_senal,
                                     function(c) !is.na(datos[[c]]) &
                                       datos[[c]] == 1))
  ventana_cerrada <- !is.na(datos[[columna_fin]]) &
    datos[[columna_fin]] <= tope_datos

  datos$tiene_ventana <- con_dato & !is.na(datos$fecha_inicio_seguimiento)
  datos$observable <- datos$tiene_ventana & (ventana_cerrada | alguna_marca)

  paso("ventana de %d días: %s con ventana, %s observables",
       ventana$dias, miles(sum(datos$tiene_ventana)),
       miles(sum(datos$observable)))

  # Cuántas reincidencias se quedan sin delito identificable: son las que la
  # señal detectó pero sin una captura judicial posterior dentro de la ventana.
  # Cuentan en la fila "cualquier delito" y en ninguna de las otras tres.
  cobertura <- dplyr::tibble(
    Senal = unname(SENALES),
    Marca = vapply(names(SENALES), function(x) {
      c <- datos[[columnas_senal[[x]]]]
      sum(!is.na(c) & c == 1 & datos$observable)
    }, numeric(1)),
    `Con delito identificado` = vapply(names(SENALES), function(x) {
      c <- datos[[columnas_senal[[x]]]]
      sum(!is.na(c) & c == 1 & datos$observable &
            !is.na(datos$grupo_reincidencia))
    }, numeric(1))
  ) |>
    dplyr::mutate(`% identificado` = round(100 * `Con delito identificado` /
                                             pmax(Marca, 1), 1))

  cat("\n")
  print(as.data.frame(cobertura), row.names = FALSE)
  paso("lo que no se identifica cuenta solo en la fila de 'cualquier delito'")

  datos |> dplyr::select(-dplyr::any_of(c(".fecha_siguiente", ".grupo_siguiente")))
}


resumir_respuestas <- function(datos, ventana) {
  observables <- datos$observable

  dplyr::tibble(
    Ventana = sprintf("%d días", ventana$dias),
    Senal = vapply(names(RESPUESTAS),
                   function(id) SENALES[[META_RESPUESTAS[[id]]$senal]],
                   character(1)),
    Tipo = vapply(names(RESPUESTAS),
                  function(id) TIPOS[[META_RESPUESTAS[[id]]$tipo]],
                  character(1)),
    Casos = sum(observables),
    Reinciden = vapply(names(RESPUESTAS),
                       function(id) sum(datos[[id]][observables], na.rm = TRUE),
                       numeric(1)),
    `Tasa (%)` = round(100 * Reinciden / Casos, 2)
  )
}


# Con 730 días un caso necesita dos años de seguimiento, así que el último año
# evaluable se corre hacia atrás.
anio_fin_efectivo <- function(ventana, tope_datos) {
  if (!AJUSTAR_FIN_POR_VENTANA) return(ANIO_FIN_EVALUACION)

  ultimo <- as.integer(format(tope_datos - ventana$dias, "%Y"))
  ajustado <- min(ANIO_FIN_EVALUACION, ultimo)

  if (ajustado < ANIO_FIN_EVALUACION) {
    paso("el fin de evaluación baja de %d a %d: con %d días el seguimiento no cierra",
         ANIO_FIN_EVALUACION, ajustado, ventana$dias)
  }
  ajustado
}

armar_panel <- function(datos, predictores, anio_fin) {

  panel <- datos |>
    dplyr::filter(observable, anio <= anio_fin) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(predictores),
                                ~ tidyr::replace_na(as.numeric(.x), 0)))

  constantes <- predictores[
    vapply(panel[predictores], function(x) dplyr::n_distinct(x) < 2, logical(1))
  ]

  if (length(constantes) > 0) {
    paso("se descartan por no tener variación: %s",
         paste(constantes, collapse = ", "))
    predictores <- setdiff(predictores, constantes)
  }

  paso("panel: %s casos · %d predictores",
       miles(nrow(panel)), length(predictores))

  list(panel = panel, predictores = predictores)
}

# Los estratos que se van a correr, con el conteo de cada uno. Los que no
# llegan al mínimo se marcan y no se estiman.
estratos_del_panel <- function(panel, destino) {

  if (!ESTRATIFICAR_POR_DELITO) {
    return(list(activos = ESTRATO_TODOS, tabla = NULL))
  }

  conteo <- panel |>
    dplyr::count(grupo_delito, name = "casos") |>
    dplyr::mutate(
      grupo_delito = as.character(grupo_delito),
      `Reincide (%)` = vapply(grupo_delito, function(g) {
        round(100 * mean(panel[[RESPUESTA_PRINCIPAL]][panel$grupo_delito == g]), 2)
      }, numeric(1)),
      Estimado = ifelse(casos >= MINIMO_CASOS_ESTRATO, "sí", "no")
    ) |>
    dplyr::arrange(dplyr::desc(casos))

  tabla <- dplyr::bind_rows(
    dplyr::tibble(grupo_delito = ESTRATO_TODOS, casos = nrow(panel),
                  `Reincide (%)` = round(100 * mean(panel[[RESPUESTA_PRINCIPAL]]), 2),
                  Estimado = "sí"),
    conteo
  ) |>
    dplyr::rename(Estrato = grupo_delito, Casos = casos)

  guardar_tabla(tabla, "tabla_estratos", destino)

  activos <- c(ESTRATO_TODOS, conteo$grupo_delito[conteo$Estimado == "sí"])

  saltados <- conteo$grupo_delito[conteo$Estimado == "no"]
  if (length(saltados) > 0) {
    paso("grupos con menos de %s casos, no se estiman: %s",
         miles(MINIMO_CASOS_ESTRATO), paste(saltados, collapse = ", "))
  }

  list(activos = activos, tabla = tabla)
}


filtrar_estrato <- function(panel, estrato) {
  if (estrato == ESTRATO_TODOS) return(panel)
  panel |> dplyr::filter(as.character(grupo_delito) == estrato)
}


particionar <- function(panel, ventana, anio_fin, destino) {

  entrenamiento <- panel |> dplyr::filter(anio <= ANIO_CORTE)
  evaluacion    <- panel |> dplyr::filter(anio > ANIO_CORTE, anio <= anio_fin)

  if (nrow(evaluacion) == 0) {
    stop(sprintf("Ventana de %d días: no queda año de evaluación. Bajá ANIO_CORTE.",
                 ventana$dias))
  }

  reparto <- dplyr::tibble(
    Conjunto = c("Entrenamiento", "Evaluación"),
    Años = c(sprintf("%d-%d", min(entrenamiento$anio), ANIO_CORTE),
             sprintf("%d-%d", ANIO_CORTE + 1, anio_fin)),
    Casos = c(nrow(entrenamiento), nrow(evaluacion)),
    `% del panel` = round(100 * c(nrow(entrenamiento), nrow(evaluacion)) /
                            nrow(panel), 1),
    `Reincide (%)` = round(100 * c(mean(entrenamiento[[RESPUESTA_PRINCIPAL]]),
                                   mean(evaluacion[[RESPUESTA_PRINCIPAL]])), 2)
  )

  guardar_tabla(reparto, "tabla_particion", destino)

  compartidas <- length(intersect(entrenamiento$IDENTIFICACION,
                                  evaluacion$IDENTIFICACION))
  paso("personas en los dos conjuntos: %s (no es fuga: el historial solo mira atrás)",
       miles(compartidas))

  # Pliegues de origen móvil: cada uno entrena con todo lo anterior a un año y
  # valida en ese año.
  anios_cv <- seq(ANIO_INICIO_CV, ANIO_CORTE)
  indices_cv <- lapply(anios_cv, function(a) which(entrenamiento$anio < a))
  indices_validacion <- lapply(anios_cv, function(a) which(entrenamiento$anio == a))

  names(indices_cv) <- paste0("hasta_", anios_cv - 1)
  names(indices_validacion) <- paste0("valida_", anios_cv)

  validos <- lengths(indices_cv) > 0 & lengths(indices_validacion) > 0

  guardar_tabla(
    dplyr::tibble(
      Pliegue = seq_along(anios_cv),
      `Entrena hasta` = anios_cv - 1,
      `Valida en` = anios_cv,
      `Casos entrenamiento` = lengths(indices_cv),
      `Casos validación` = lengths(indices_validacion),
      Usado = ifelse(validos, "sí", "no")
    ),
    "tabla_pliegues_cv", destino, mostrar = FALSE)

  list(entrenamiento = entrenamiento, evaluacion = evaluacion,
       indices_cv = indices_cv[validos],
       indices_validacion = indices_validacion[validos],
       anios_cv = anios_cv[validos], anio_fin = anio_fin)
}
