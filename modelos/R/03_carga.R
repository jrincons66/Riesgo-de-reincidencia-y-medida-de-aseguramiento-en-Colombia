# ==============================================================================
# 03. Carga de la base
#
# La base ya viene procesada desde Python: fechas limpias, ventanas de
# seguimiento armadas y las ocho respuestas calculadas. Acá no se reconstruye
# nada de eso, solo se verifica y se arma el grupo de delito.
# ==============================================================================

COLUMNAS_FECHA <- c("FECHA", "FechaLegalizacion", "FechaImputacion",
                    "FechaMedida", "FechaAcusacion", "FechaSentencia",
                    "FechaBoletaDetencion", "FechaBoletaSalida",
                    "fecha_evento_radicado", "fecha_inicio_seguimiento",
                    "fecha_fin_seguimiento", "fecha_fin_seguimiento_2")

cargar_base <- function(ruta = RUTA_BASE) {

  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop('Falta el paquete arrow para leer el parquet: install.packages("arrow")')
  }

  datos <- arrow::read_parquet(ruta)

  paso("fuente: %s", basename(ruta))
  paso("%s filas x %d columnas",
       miles(nrow(datos)), ncol(datos))

  respuestas <- unlist(lapply(VENTANAS, function(v) paste0(names(RESPUESTAS), v$sufijo)))
  finales <- vapply(VENTANAS, function(v) v$columna_fin, character(1))

  requeridas <- c("IDENTIFICACION", "radicado", "FECHA", "GENERO", "joven",
                  COLUMNA_MEDIDA, "k_delito", "fecha_inicio_seguimiento",
                  "origen_ventana", respuestas, finales)

  faltantes <- setdiff(requeridas, names(datos))
  if (length(faltantes) > 0) {
    stop(paste("Faltan columnas en la base:", paste(faltantes, collapse = ", ")))
  }

  fechas <- intersect(COLUMNAS_FECHA, names(datos))

  datos <- datos |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(fechas), as.Date),
      IDENTIFICACION = as.character(IDENTIFICACION),
      radicado = as.character(radicado),
      anio = as.integer(format(FECHA, "%Y"))
    ) |>
    dplyr::filter(!is.na(IDENTIFICACION), !is.na(FECHA)) |>
    dplyr::arrange(IDENTIFICACION, FECHA)

  # El artículo sale de k_delito, que Python ya normalizó (sin tildes, en
  # mayúsculas). Si no estuviera, se cae a CONDUCTA, que sí trae tildes.
  datos <- datos |>
    dplyr::mutate(
      articulo_captura = stringr::str_match(k_delito, "^ARTICULO\\s+([0-9]+[A-Z]?)")[, 2],
      articulo_captura = dplyr::if_else(
        is.na(articulo_captura) & "CONDUCTA" %in% names(datos),
        stringr::str_match(stringr::str_to_upper(
          stringi_sin_tildes(CONDUCTA)), "ART[I]CULO\\s+([0-9]+[A-Z]?)")[, 2],
        articulo_captura
      ),
      grupo_delito = dplyr::case_when(
        articulo_captura %in% GRUPOS[[1]] ~ names(GRUPOS)[1],
        articulo_captura %in% GRUPOS[[2]] ~ names(GRUPOS)[2],
        articulo_captura %in% GRUPOS[[3]] ~ names(GRUPOS)[3],
        TRUE ~ GRUPO_RESTO
      ),
      grupo_delito = factor(grupo_delito, levels = ORDEN_GRUPOS)
    )

  paso("artículo identificado en el %.1f%% de los casos",
       100 * mean(!is.na(datos$articulo_captura)))

  print(datos |>
          dplyr::count(grupo_delito, name = "casos") |>
          dplyr::mutate(`%` = round(100 * casos / sum(casos), 1)) |>
          as.data.frame(), row.names = FALSE)

  tope_datos <- max(datos$FECHA, na.rm = TRUE)

  paso("periodo: %d-%d   tope de los datos: %s",
       min(datos$anio), max(datos$anio), format(tope_datos, "%Y-%m-%d"))
  paso("casos: %s · personas: %s",
       miles(nrow(datos)),
       miles(dplyr::n_distinct(datos$IDENTIFICACION)))

  # Sin ventana no hay respuesta posible. Son los casos sin boleta de salida y
  # sin fecha de medida, que en la muestra son casi la mitad de la base y
  # todos con TipoMedida = 0. No es un faltante aleatorio: el panel de
  # modelado son los que recibieron medida o salieron con boleta.
  reparto <- datos |>
    dplyr::count(origen_ventana, name = "casos") |>
    dplyr::mutate(`%` = round(100 * casos / sum(casos), 1))
  cat("\n")
  print(as.data.frame(reparto), row.names = FALSE)

  attr(datos, "tope_datos") <- tope_datos
  datos
}

# Quitar tildes sin depender de stringi, que no siempre está instalado.
stringi_sin_tildes <- function(x) {
  chartr("ÁÉÍÓÚÜÑáéíóúüñ", "AEIOUUNaeiouun", x)
}
