# ==============================================================================
# 12. Robustez y guardado
# ==============================================================================

# Partición aleatoria por persona: mide cuánto se infla el desempeño al ignorar
# el orden temporal. El sorteo es por cédula, no por caso: si dos procesos de la
# misma persona cayeran a lados distintos, el historial de uno filtraría al otro.

correr_robustez <- function(panel, predictores, motor, desempeno, destino,
                            ventana, estrato = ESTRATO_TODOS) {

  set.seed(SEMILLA)
  personas <- unique(panel$IDENTIFICACION)
  en_entrenamiento <- sample(personas,
                             floor(PROPORCION_ENTRENAMIENTO * length(personas)))

  entrena <- panel |> dplyr::filter(IDENTIFICACION %in% en_entrenamiento)
  evalua  <- panel |> dplyr::filter(!IDENTIFICACION %in% en_entrenamiento)

  tope <- if (MODO_RAPIDO) FILAS_MODO_RAPIDO else MAXIMO_BUSQUEDA
  if (!is.null(tope) && nrow(entrena) > tope) {
    set.seed(SEMILLA)
    entrena <- entrena[sample(seq_len(nrow(entrena)), tope), ]
  }

  control <- caret::trainControl(
    method = "cv", number = 5, classProbs = TRUE,
    summaryFunction = caret::twoClassSummary, allowParallel = TRUE
  )

  modelo <- con_cache(
    sprintf("robustez_v%d_%s_%s_%s", ventana$dias, nombre_corto(estrato), motor,
            huella(predictores, ANIO_CORTE, PROPORCION_ENTRENAMIENTO)),
    entrenar_modelo(RESPUESTA_PRINCIPAL, entrena, predictores, motor, control)
  )

  if (is.null(modelo)) return(invisible(NULL))

  aleatorio <- metricas_binarias(evalua[[columna_y(RESPUESTA_PRINCIPAL)]],
                                 predecir(modelo, evalua))

  temporal <- desempeno |>
    dplyr::filter(motor_id == motor, respuesta_id == RESPUESTA_PRINCIPAL) |>
    dplyr::select(AUC, Exactitud, Sensibilidad, Especificidad, F1, `F1 máximo`)

  comparacion <- dplyr::bind_rows(
    temporal |> dplyr::mutate(Partición = sprintf("Temporal (hasta %d / %d en adelante)",
                                                  ANIO_CORTE, ANIO_CORTE + 1)),
    aleatorio |> dplyr::select(AUC, Exactitud, Sensibilidad, Especificidad,
                               F1, `F1 máximo`) |>
      dplyr::mutate(Partición = sprintf("Aleatoria por persona (%d/%d)",
                                        round(100 * PROPORCION_ENTRENAMIENTO),
                                        round(100 * (1 - PROPORCION_ENTRENAMIENTO))))
  ) |>
    dplyr::select(Partición, dplyr::everything()) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

  guardar_tabla(comparacion, "tabla_robustez_particion", destino)
  paso("la diferencia entre las dos filas mide cuánto se infla el desempeño")
  paso("cuando se ignora el orden temporal de los datos")

  invisible(comparacion)
}

# ---- Guardado ----------------------------------------------------------------

guardar_modelos <- function(modelos, evaluacion, destino, ventana) {

  saveRDS(modelos, file.path(destino, sprintf("modelos_v%d.rds", ventana$dias)))

  columnas <- intersect(
    c("IDENTIFICACION", "radicado", "anio", "grupo_delito", COLUMNA_MEDIDA,
      "joven", "grupo_reincidencia", names(RESPUESTAS)),
    names(evaluacion)
  )

  predicciones <- evaluacion |>
    dplyr::select(dplyr::all_of(columnas), dplyr::starts_with("riesgo_"))

  readr::write_excel_csv(
    predicciones,
    file.path(destino, sprintf("predicciones_evaluacion_v%d.csv", ventana$dias))
  )

  paso("modelos y predicciones guardados en %s", destino)
}

guardar_sesion <- function(destino) {
  salida <- file.path(destino, "sesion.txt")
  capture <- utils::capture.output(utils::sessionInfo())
  writeLines(c(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "", capture), salida)
}
