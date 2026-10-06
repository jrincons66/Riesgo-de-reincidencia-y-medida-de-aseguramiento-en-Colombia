# ==============================================================================
# 09. Métricas
#
# Con clases desbalanceadas el AUC se queda corto y la exactitud engaña: un
# modelo que predice "no reincide" siempre puede tener 90% de exactitud. Por eso
# la comparación entre motores se hace con F1, que castiga tanto marcar de más
# como marcar de menos.
# ==============================================================================

# F1 en todos los umbrales posibles, con sumas acumuladas en vez de un ciclo.
#
# Solo se corta donde la probabilidad cambia de valor. Con empates un umbral no
# puede separar dos casos que tienen el mismo puntaje, y cortar en la mitad de
# un grupo empatado daría un F1 que ningún umbral alcanza de verdad. Importa
# sobre todo con el árbol, que devuelve un puñado de probabilidades distintas.
curva_f1 <- function(observado, probabilidad) {
  orden <- order(probabilidad, decreasing = TRUE)
  y <- observado[orden]
  p <- probabilidad[orden]

  ultimo_del_empate <- c(p[-length(p)] != p[-1], TRUE)

  verdaderos <- cumsum(y)[ultimo_del_empate]
  positivos_predichos <- seq_along(y)[ultimo_del_empate]
  p <- p[ultimo_del_empate]
  positivos_reales <- sum(y)

  precision <- verdaderos / positivos_predichos
  recall <- verdaderos / positivos_reales
  f1 <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))

  list(umbral = p, precision = precision, recall = recall, f1 = f1)
}

# Área bajo la curva de precisión y recall, por trapecios. Con eventos raros es
# más informativa que el AUC.
auc_precision_recall <- function(curva) {
  orden <- order(curva$recall)
  r <- curva$recall[orden]
  p <- curva$precision[orden]
  sum(diff(r) * (utils::head(p, -1) + utils::tail(p, -1)) / 2)
}

# Sensibilidad, especificidad y compañía en un umbral dado.
metricas_en_umbral <- function(observado, probabilidad, umbral) {
  predicho <- as.integer(probabilidad >= umbral)

  vp <- sum(predicho == 1 & observado == 1)
  fp <- sum(predicho == 1 & observado == 0)
  fn <- sum(predicho == 0 & observado == 1)

  sensibilidad <- vp / (vp + fn)
  especificidad <- mean(predicho[observado == 0] == 0)

  list(
    exactitud = mean(predicho == observado),
    sensibilidad = sensibilidad,
    especificidad = especificidad,
    precision = if (vp + fp == 0) 0 else vp / (vp + fp)
  )
}

# El umbral que maximiza sensibilidad + especificidad. Con eventos raros, el
# 0.5 da sensibilidad cero y exactitud altísima, que es lo que le pasó al
# modelo violento en la primera corrida. PRiSMA reporta exactitudes de 0,67 a
# 0,80, o sea que tampoco usó 0.5: este umbral reproduce ese perfil.
umbral_youden <- function(observado, probabilidad) {
  curva <- pROC::roc(observado, probabilidad, quiet = TRUE)
  indice <- which.max(curva$sensitivities + curva$specificities)
  curva$thresholds[indice]
}


metricas_binarias <- function(observado, probabilidad, umbral = 0.5) {

  observado <- as.integer(observado)

  if (dplyr::n_distinct(observado) < 2) {
    return(dplyr::tibble(AUC = NA_real_, `AUC PR` = NA_real_, Exactitud = NA_real_,
                         Precisión = NA_real_, Sensibilidad = NA_real_,
                         Especificidad = NA_real_, F1 = NA_real_,
                         `F1 máximo` = NA_real_, `Umbral F1 máx` = NA_real_,
                         Brier = NA_real_, `Error tipo I` = NA_real_,
                         `Error tipo II` = NA_real_, `Umbral Youden` = NA_real_,
                         `Exactitud (Y)` = NA_real_, `Sensibilidad (Y)` = NA_real_,
                         `Especificidad (Y)` = NA_real_,
                         `Error tipo I (Y)` = NA_real_,
                         `Error tipo II (Y)` = NA_real_))
  }

  curva_roc <- pROC::roc(observado, probabilidad, quiet = TRUE)
  curva <- curva_f1(observado, probabilidad)
  mejor <- which.max(curva$f1)

  predicho <- as.integer(probabilidad >= umbral)

  verdaderos_positivos <- sum(predicho == 1 & observado == 1)
  falsos_positivos <- sum(predicho == 1 & observado == 0)
  falsos_negativos <- sum(predicho == 0 & observado == 1)

  precision <- if (verdaderos_positivos + falsos_positivos == 0) 0 else
    verdaderos_positivos / (verdaderos_positivos + falsos_positivos)
  sensibilidad <- verdaderos_positivos / (verdaderos_positivos + falsos_negativos)
  especificidad <- mean(predicho[observado == 0] == 0)
  f1 <- if (precision + sensibilidad == 0) 0 else
    2 * precision * sensibilidad / (precision + sensibilidad)

  youden <- umbral_youden(observado, probabilidad)
  en_youden <- metricas_en_umbral(observado, probabilidad, youden)

  dplyr::tibble(
    AUC = as.numeric(pROC::auc(curva_roc)),
    `AUC PR` = auc_precision_recall(curva),
    Exactitud = mean(predicho == observado),
    Precisión = precision,
    Sensibilidad = sensibilidad,
    Especificidad = especificidad,
    F1 = f1,
    # El F1 en 0.5 depende del umbral, que es arbitrario. El F1 máximo dice de
    # qué es capaz el modelo si el umbral se elige bien, y es el que hace
    # comparables motores que devuelven probabilidades en escalas distintas.
    `F1 máximo` = curva$f1[mejor],
    `Umbral F1 máx` = curva$umbral[mejor],
    Brier = mean((probabilidad - observado) ^ 2),
    `Error tipo I` = 1 - especificidad,
    `Error tipo II` = 1 - sensibilidad,
    # Las mismas cuatro en el umbral de Youden, para la tabla estilo PRiSMA.
    `Umbral Youden` = youden,
    `Exactitud (Y)` = en_youden$exactitud,
    `Sensibilidad (Y)` = en_youden$sensibilidad,
    `Especificidad (Y)` = en_youden$especificidad,
    `Error tipo I (Y)` = 1 - en_youden$especificidad,
    `Error tipo II (Y)` = 1 - en_youden$sensibilidad
  )
}

predecir <- function(modelo, datos) {

  faltantes <- setdiff(modelo$predictores, names(datos))
  if (length(faltantes) > 0) {
    stop("El modelo de ", etiqueta_motor(modelo$motor), " / ",
         RESPUESTAS[[modelo$respuesta]], " se entrenó con columnas que ya no ",
         "están en el panel:\n  ", paste(faltantes, collapse = ", "),
         "\nEs un modelo viejo del caché. Corré limpiar_cache() y volvé a ",
         "estimar.")
  }

  stats::predict(modelo$ajuste,
                 newdata = as.data.frame(datos[, modelo$predictores, drop = FALSE]),
                 type = "prob")[, "si"]
}

evaluar_modelos <- function(modelos, evaluacion, ventana,
                            estrato = ESTRATO_TODOS) {

  filas <- lapply(modelos, function(modelo) {
    probabilidad <- predecir(modelo, evaluacion)

    dplyr::bind_cols(
      dplyr::tibble(
        Ventana = ventana$dias,
        Estrato = estrato,
        Senal = SENALES[[META_RESPUESTAS[[modelo$respuesta]]$senal]],
        Tipo = TIPOS[[META_RESPUESTAS[[modelo$respuesta]]$tipo]],
        Motor = etiqueta_motor(modelo$motor),
        motor_id = modelo$motor,
        Modelo = RESPUESTAS[[modelo$respuesta]],
        respuesta_id = modelo$respuesta
      ),
      metricas_binarias(evaluacion[[columna_y(modelo$respuesta)]], probabilidad)
    )
  })

  dplyr::bind_rows(filas) |>
    dplyr::arrange(respuesta_id, dplyr::desc(.data[[METRICA_SELECCION]]))
}

# El mejor motor de cada respuesta según METRICA_SELECCION.
mejores_motores <- function(desempeno) {
  desempeno |>
    dplyr::group_by(respuesta_id) |>
    dplyr::slice_max(.data[[METRICA_SELECCION]], n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
}

# Probabilidades del motor elegido, pegadas a la evaluación para los gráficos.
agregar_riesgos <- function(evaluacion, modelos, motor) {
  for (respuesta in names(RESPUESTAS)) {
    clave <- paste(motor, respuesta, sep = "|")
    if (!is.null(modelos[[clave]])) {
      evaluacion[[paste0("riesgo_", respuesta)]] <- predecir(modelos[[clave]], evaluacion)
    }
  }
  evaluacion
}
