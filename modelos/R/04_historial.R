# ==============================================================================
# 04. Historial previo y predictores
#
# Las variables de historial que trae la base son totales de toda la vida de la
# persona e incluyen lo que pasa después de la captura. Acá se recalculan
# mirando solo hacia atrás, que es lo único que un juez tendría sobre la mesa
# el día de la audiencia.
# ==============================================================================

# Solo lo estrictamente anterior a la fecha de captura. Las capturas del mismo
# día no cuentan entre sí.
acumulado_previo <- function(tabla, columna) {
  tabla |>
    dplyr::group_by(IDENTIFICACION) |>
    dplyr::mutate(.inc = cumsum(tidyr::replace_na({{ columna }}, 0))) |>
    dplyr::group_by(IDENTIFICACION, FECHA) |>
    dplyr::mutate(.prev = max(.inc) - sum(tidyr::replace_na({{ columna }}, 0))) |>
    dplyr::ungroup() |>
    dplyr::pull(.prev)
}

agregar_historial <- function(datos) {

  # --- Gravedad del delito actual ---
  # pena_delito viene del delito sentenciado en una parte de las filas, y la
  # sentencia es posterior a la captura. La tabla artículo -> pena se arma
  # solo con las filas donde la pena salió del delito por el que capturaron.
  if (!all(c("pena_delito", "fuente_pena") %in% names(datos))) {
    stop("La base no trae pena_delito y fuente_pena.")
  }

  tabla_penas <- datos |>
    dplyr::filter(fuente_pena == "delito de la captura", !is.na(pena_delito)) |>
    dplyr::group_by(articulo_captura) |>
    dplyr::summarise(pena_delito_actual = stats::median(pena_delito, na.rm = TRUE))

  datos <- datos |> dplyr::left_join(tabla_penas, by = "articulo_captura")

  paso("gravedad del delito asignada al %.1f%% de los casos (desde %d artículos)",
       100 * mean(!is.na(datos$pena_delito_actual)), nrow(tabla_penas))

  # --- Eventos por caso, para acumular hacia atrás ---
  datos <- datos |>
    dplyr::mutate(
      .uno = 1,
      .imputado = as.integer(!is.na(FechaImputacion)),
      .condenado = if ("TipoSentencia" %in% names(datos)) {
        as.integer(!is.na(TipoSentencia) & TipoSentencia == 1)
      } else 0L,
      .dias_detenido = if (all(c("FechaBoletaDetencion", "FechaBoletaSalida")
                               %in% names(datos))) {
        pmax(as.numeric(FechaBoletaSalida - FechaBoletaDetencion), 0)
      } else NA_real_,
      .pena_caso = dplyr::coalesce(pena_delito_actual, 0)
    )

  datos <- datos |>
    dplyr::mutate(
      capturas_previas      = acumulado_previo(datos, .uno),
      imputaciones_previas  = acumulado_previo(datos, .imputado),
      condenas_previas      = acumulado_previo(datos, .condenado),
      dias_detenido_previos = acumulado_previo(datos, .dias_detenido),
      historial_penas       = acumulado_previo(datos, .pena_caso)
    )

  for (grupo in ORDEN_GRUPOS) {
    auxiliar <- datos |> dplyr::mutate(.marca = as.integer(grupo_delito == grupo))
    datos[[paste0("previos_", make.names(grupo))]] <- acumulado_previo(auxiliar, .marca)
  }

  paso("casos con al menos una captura previa: %.1f%%",
       100 * mean(datos$capturas_previas > 0))

  # Contraste con las columnas de la base, para dejar el tamaño de la fuga
  # documentado en la corrida.
  if ("n_procesos_cedula" %in% names(datos)) {
    comparacion <- dplyr::tibble(
      Variable = c("capturas_previas (hacia atrás)", "n_procesos_cedula (total)"),
      Media = c(mean(datos$capturas_previas), mean(datos$n_procesos_cedula)),
      Máximo = c(max(datos$capturas_previas), max(datos$n_procesos_cedula))
    )
    cat("\n")
    print(as.data.frame(comparacion), row.names = FALSE, digits = 3)
    paso("la de la base cuenta también los procesos posteriores: por eso es mayor")
  }

  datos |> dplyr::select(-dplyr::starts_with("."))
}

agregar_predictores <- function(datos) {

  for (grupo in ORDEN_GRUPOS) {
    datos[[paste0("actual_", make.names(grupo))]] <-
      as.integer(datos$grupo_delito == grupo)
  }

  datos <- datos |>
    dplyr::mutate(
      es_hombre = as.integer(stringr::str_detect(
        stringr::str_to_upper(dplyr::coalesce(GENERO, "")), "^M")),
      joven = as.integer(joven),
      sin_pena_tipificada = as.integer(is.na(pena_delito_actual)),
      pena_delito_actual = tidyr::replace_na(pena_delito_actual, 0)
    )

  predictores <- c(
    "joven", "es_hombre",
    "capturas_previas", "imputaciones_previas", "condenas_previas",
    "historial_penas", "dias_detenido_previos",
    "pena_delito_actual", "sin_pena_tipificada",
    paste0("previos_", make.names(ORDEN_GRUPOS)),
    paste0("actual_", make.names(ORDEN_GRUPOS))
  )

  if (INCLUIR_COLUMNAS_CON_FUGA) {
    predictores <- c(predictores, intersect(COLUMNAS_CON_FUGA, names(datos)))
    paso("ATENCIÓN: las columnas con fuga están encendidas, los resultados")
    paso("de esta corrida no son interpretables como capacidad predictiva")
  }

  attr(datos, "predictores") <- intersect(predictores, names(datos))
  datos
}
