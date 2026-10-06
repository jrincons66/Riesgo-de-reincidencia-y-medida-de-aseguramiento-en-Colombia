# ==============================================================================
# 10. Gráficos y tablas de resultados
# Una función por salida. Todas reciben el destino como argumento: no escriben
# en ninguna ruta fija.
# ==============================================================================

# Con 16 respuestas ningún gráfico entra en un solo panel: casi todos se
# facetean por señal, de modo que cada panel tiene las cuatro curvas de tipo
# de delito de esa señal. Los que ni así entran se limitan a
# RESPUESTAS_DESTACADAS.
senal_de <- function(id) SENALES[[META_RESPUESTAS[[id]]$senal]]
tipo_de <- function(id) TIPOS[[META_RESPUESTAS[[id]]$tipo]]

con_riesgo <- function(evaluacion, ids = names(RESPUESTAS)) {
  ids[paste0("riesgo_", ids) %in% names(evaluacion)]
}

# Las cuatro respuestas de una señal, en el orden de TIPOS.
ids_de_senal <- function(senal_id) paste0("y_", senal_id, "_", names(TIPOS))


# ---- Curvas ROC --------------------------------------------------------------
# Las cuatro juntas y una por panel. La versión de paneles se lee mejor cuando
# las curvas se cruzan, que es lo que pasa cuando un modelo es muy bueno y
# otro apenas supera el azar.

puntos_roc <- function(evaluacion, ids) {
  disponibles <- con_riesgo(evaluacion, ids)
  if (length(disponibles) == 0) return(NULL)

  puntos <- dplyr::bind_rows(lapply(disponibles, function(id) {
    observado <- evaluacion[[id]]
    if (dplyr::n_distinct(observado) < 2) return(NULL)

    curva <- pROC::roc(observado, evaluacion[[paste0("riesgo_", id)]],
                       quiet = TRUE)
    area <- as.numeric(pROC::auc(curva))
    indices <- unique(round(seq(1, length(curva$sensitivities),
                                length.out = min(1500,
                                                 length(curva$sensitivities)))))
    dplyr::tibble(
      senal_id = META_RESPUESTAS[[id]]$senal,
      Senal = senal_de(id), Tipo = tipo_de(id), area = area,
      sensibilidad = curva$sensitivities[indices],
      uno_menos_especificidad = 1 - curva$specificities[indices]
    )
  }))

  if (nrow(puntos) == 0) return(NULL)
  puntos |>
    dplyr::mutate(Senal = factor(Senal, levels = unname(SENALES)),
                  Tipo = factor(Tipo, levels = unname(TIPOS)))
}

base_roc <- function(datos) {
  ggplot2::ggplot(datos, ggplot2::aes(uno_menos_especificidad, sensibilidad)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dotted",
                         colour = "grey70") +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "1 - especificidad (falsos positivos)",
                  y = "Sensibilidad (verdaderos positivos)")
}

# ---- El cruzado: un panel por tipo de delito, las cuatro señales adentro.
# Va en la carpeta común. Responde cuál forma de medir se predice mejor.
salida_roc_por_tipo <- function(evaluacion, destino, ventana) {

  puntos <- puntos_roc(evaluacion, names(RESPUESTAS))
  if (is.null(puntos)) return(invisible(NULL))

  puntos <- puntos |> dplyr::mutate(Curva = sprintf("%s (%.3f)", Senal, area))

  grafico <- base_roc(puntos) +
    ggplot2::geom_line(ggplot2::aes(linetype = Curva, group = Curva),
                       colour = "grey20", linewidth = 0.45) +
    ggplot2::facet_wrap(~ Tipo) +
    ggplot2::guides(linetype = ggplot2::guide_legend(nrow = 4)) +
    ggplot2::labs(
      title = "Curvas ROC por tipo de delito",
      subtitle = sprintf("Cada panel compara las cuatro señales · ventana de %d días",
                         ventana$dias),
      caption = "El AUC va entre paréntesis en la leyenda. La diagonal es una predicción al azar."
    )

  guardar_grafico(grafico, "grafico_roc_por_tipo", destino, ancho = 8, alto = 9.5)

  guardar_tabla(
    puntos |> dplyr::distinct(Senal, Tipo, area) |>
      dplyr::mutate(AUC = round(area, 4)) |> dplyr::select(Senal, Tipo, AUC),
    "tabla_auc", destino, mostrar = FALSE)

  invisible(puntos)
}

# ---- El de una señal: las cuatro curvas de tipo de delito, juntas y por
# panel. Va en la carpeta de esa señal.
salida_roc_senal <- function(evaluacion, destino, ventana, senal_id) {

  puntos <- puntos_roc(evaluacion, ids_de_senal(senal_id))
  if (is.null(puntos)) return(invisible(NULL))

  puntos <- puntos |> dplyr::mutate(Curva = sprintf("%s (AUC = %.3f)", Tipo, area))
  etiqueta <- SENALES[[senal_id]]

  juntas <- base_roc(puntos) +
    ggplot2::geom_line(ggplot2::aes(linetype = Curva, group = Curva),
                       colour = "grey20", linewidth = 0.5) +
    ggplot2::guides(linetype = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::labs(
      title = "Curvas ROC de los cuatro modelos",
      subtitle = sprintf("%s · conjunto de evaluación · ventana de %d días",
                         etiqueta, ventana$dias),
      caption = "La diagonal representa una predicción al azar."
    )
  guardar_grafico(juntas, "grafico_roc", destino, ancho = 6.5, alto = 6.5)

  paneles <- base_roc(puntos) +
    ggplot2::geom_line(ggplot2::aes(linetype = Curva, group = Curva),
                       colour = "grey20", linewidth = 0.5, show.legend = FALSE) +
    ggplot2::facet_wrap(~ Curva) +
    ggplot2::labs(
      title = "Curvas ROC de los cuatro modelos",
      subtitle = sprintf("Un panel por modelo · %s · ventana de %d días",
                         etiqueta, ventana$dias),
      caption = "La diagonal representa una predicción al azar."
    )
  guardar_grafico(paneles, "grafico_roc_paneles", destino, ancho = 7, alto = 7)

  guardar_tabla(
    puntos |> dplyr::distinct(Tipo, area) |>
      dplyr::mutate(AUC = round(area, 4)) |> dplyr::select(Modelo = Tipo, AUC),
    "tabla_auc", destino, mostrar = FALSE)

  invisible(puntos)
}


# ---- AUC año por año ---------------------------------------------------------

auc_por_anio <- function(evaluacion, ids) {
  disponibles <- con_riesgo(evaluacion, ids)
  if (length(disponibles) == 0) return(NULL)

  auc_segura <- function(observado, riesgo) {
    if (length(unique(observado[!is.na(observado)])) < 2) return(NA_real_)
    tryCatch(as.numeric(pROC::auc(pROC::roc(observado, riesgo, quiet = TRUE))),
             error = function(e) NA_real_)
  }

  tabla <- dplyr::bind_rows(lapply(disponibles, function(id) {
    evaluacion |>
      dplyr::group_by(anio) |>
      dplyr::summarise(
        AUC = auc_segura(.data[[id]], .data[[paste0("riesgo_", id)]]),
        Casos = dplyr::n(),
        Positivos = sum(.data[[id]] == 1, na.rm = TRUE)
      ) |>
      dplyr::filter(!is.na(AUC)) |>
      dplyr::mutate(Senal = senal_de(id), Tipo = tipo_de(id))
  }))

  if (nrow(tabla) == 0) return(NULL)
  tabla |>
    dplyr::mutate(Senal = factor(Senal, levels = unname(SENALES)),
                  Tipo = factor(Tipo, levels = unname(TIPOS)),
                  `Tasa (%)` = round(100 * Positivos / Casos, 2))
}

base_auc_anual <- function(datos) {
  ggplot2::ggplot(datos, ggplot2::aes(factor(anio), AUC)) +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dotted", colour = "grey50") +
    ggplot2::scale_y_continuous(limits = c(0.4, 1)) +
    ggplot2::labs(x = "Año de la captura", y = "AUC",
                  caption = "La línea punteada marca el desempeño de una predicción al azar.")
}

# Cruzado: panel por tipo, una línea por señal. Carpeta común.
salida_auc_anual <- function(evaluacion, destino, ventana) {
  tabla <- auc_por_anio(evaluacion, names(RESPUESTAS))
  if (is.null(tabla)) return(invisible(NULL))

  guardar_tabla(tabla |> dplyr::select(Senal, Tipo, Año = anio, Casos, Positivos,
                                       `Tasa (%)`, AUC),
                "tabla_auc_anual", destino, mostrar = FALSE)

  grafico <- base_auc_anual(tabla) +
    ggplot2::geom_line(ggplot2::aes(group = Senal, linetype = Senal), colour = "grey30") +
    ggplot2::geom_point(ggplot2::aes(shape = Senal), colour = "grey20", size = 1.5) +
    ggplot2::facet_wrap(~ Tipo) +
    ggplot2::labs(title = "Estabilidad del modelo en el tiempo",
                  subtitle = sprintf("AUC en cada año de evaluación · ventana de %d días",
                                     ventana$dias))
  guardar_grafico(grafico, "grafico_auc_anual", destino, ancho = 8, alto = 7)
}

# De una señal: un panel, una línea por tipo de delito. Carpeta de la señal.
salida_auc_anual_senal <- function(evaluacion, destino, ventana, senal_id) {
  tabla <- auc_por_anio(evaluacion, ids_de_senal(senal_id))
  if (is.null(tabla)) return(invisible(NULL))

  guardar_tabla(tabla |> dplyr::select(Modelo = Tipo, Año = anio, Casos, Positivos,
                                       `Tasa (%)`, AUC),
                "tabla_auc_anual", destino, mostrar = FALSE)

  grafico <- base_auc_anual(tabla) +
    ggplot2::geom_line(ggplot2::aes(group = Tipo, linetype = Tipo), colour = "grey30") +
    ggplot2::geom_point(ggplot2::aes(shape = Tipo), colour = "grey20", size = 1.8) +
    ggplot2::labs(title = "Estabilidad del modelo en el tiempo",
                  subtitle = sprintf("%s · AUC en cada año de evaluación · ventana de %d días",
                                     SENALES[[senal_id]], ventana$dias))
  guardar_grafico(grafico, "grafico_auc_anual", destino)
}


# ---- Calibración -------------------------------------------------------------

salida_calibracion <- function(evaluacion, destino, ventana,
                               ids = RESPUESTAS_DESTACADAS, etiqueta = NULL) {

  disponibles <- con_riesgo(evaluacion, ids)
  if (length(disponibles) == 0) return(invisible(NULL))

  calibracion <- dplyr::bind_rows(lapply(disponibles, function(id) {
    columna <- paste0("riesgo_", id)
    positivos <- sum(evaluacion[[id]] == 1, na.rm = TRUE)

    n_grupos <- max(CALIBRACION_MINIMO_PUNTOS,
                    min(CALIBRACION_MAXIMO_PUNTOS,
                        round(positivos / CALIBRACION_POSITIVOS_POR_PUNTO)))

    evaluacion |>
      dplyr::mutate(grupo = dplyr::ntile(.data[[columna]], n_grupos)) |>
      dplyr::group_by(grupo) |>
      dplyr::summarise(predicho = mean(.data[[columna]]),
                       observado = mean(.data[[id]]),
                       casos = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(Modelo = sprintf("%s (%d puntos)", tipo_de(id), n_grupos),
                    orden = match(META_RESPUESTAS[[id]]$tipo, names(TIPOS)))
  }))

  calibracion <- calibracion |>
    dplyr::mutate(Modelo = stats::reorder(Modelo, orden))

  tope <- if (identical(CALIBRACION_LIMITE, "auto")) {
    max(0.05, ceiling(20 * max(c(calibracion$predicho,
                                 calibracion$observado))) / 20)
  } else {
    CALIBRACION_LIMITE
  }

  fuera <- sum(calibracion$predicho > tope | calibracion$observado > tope)
  if (fuera > 0) paso("calibración: %d puntos quedan fuera del tope %.2f", fuera, tope)

  # Los dos ejes con la misma escala y coord_equal: la diagonal sale a 45
  # grados y se ve de una si un punto está por encima o por debajo.
  grafico <- ggplot2::ggplot(calibracion, ggplot2::aes(predicho, observado)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         colour = "grey50") +
    ggplot2::geom_point(size = 0.9, colour = "grey20", alpha = 0.6) +
    ggplot2::scale_x_continuous(limits = c(0, tope),
                                breaks = scales::pretty_breaks(4)) +
    ggplot2::scale_y_continuous(limits = c(0, tope),
                                breaks = scales::pretty_breaks(4)) +
    ggplot2::coord_equal() +
    ggplot2::facet_wrap(~ Modelo) +
    ggplot2::labs(
      title = "Riesgo predicho frente a reincidencia observada",
      subtitle = sprintf("%s · cada punto es un grupo de percentiles · ventana de %d días",
                         etiqueta %||% SENALES[[SENAL_PRISMA]], ventana$dias),
      x = "Riesgo predicho", y = "Reincidencia observada",
      caption = sprintf(paste(
        "Los dos ejes van de 0 a %s con la misma escala. Cada punto agrupa",
        "unos %d reincidentes,\nasí que los modelos con menos positivos",
        "tienen menos puntos. La diagonal marca la calibración perfecta."),
        tope, CALIBRACION_POSITIVOS_POR_PUNTO)
    )

  guardar_grafico(grafico, "grafico_calibracion", destino, ancho = 7.5, alto = 7.5)

  guardar_tabla(
    calibracion |>
      dplyr::count(Modelo, name = "puntos") |>
      dplyr::mutate(Modelo = as.character(Modelo)),
    "tabla_calibracion_puntos", destino, mostrar = FALSE)

  invisible(calibracion)
}


# ---- Importancia de las variables --------------------------------------------
# Ordenadas de mayor a menor dentro de cada panel, que es lo que hace que el
# gráfico se lea de una pasada.

salida_importancia <- function(modelos, motor, destino, ventana,
                               ids = RESPUESTAS_DESTACADAS, etiqueta = NULL) {

  del_motor <- Filter(function(m) m$motor == motor && m$respuesta %in% ids, modelos)
  if (length(del_motor) == 0) return(invisible(NULL))

  importancias <- dplyr::bind_rows(lapply(del_motor, function(modelo) {
    imp <- tryCatch(caret::varImp(modelo$ajuste)$importance,
                    error = function(e) NULL)

    if (is.null(imp)) {
      paso("%s: el motor no reporta importancia de variables",
           RESPUESTAS[[modelo$respuesta]])
      return(NULL)
    }

    # Algunos motores devuelven una columna por clase: se toma la del positivo.
    valores <- if ("si" %in% names(imp)) imp[["si"]] else imp[[1]]

    dplyr::tibble(variable = rownames(imp), importancia = valores) |>
      dplyr::arrange(dplyr::desc(importancia)) |>
      dplyr::mutate(
        Modelo = if (is.null(etiqueta)) RESPUESTAS[[modelo$respuesta]]
                 else tipo_de(modelo$respuesta),
        Puesto = dplyr::row_number()
      )
  }))

  if (nrow(importancias) == 0) return(invisible(NULL))

  # La tabla sale completa y ya ordenada por modelo y de mayor a menor.
  guardar_tabla(
    importancias |>
      dplyr::select(Modelo, Puesto, Variable = variable, Importancia = importancia) |>
      dplyr::mutate(Importancia = round(Importancia, 2)),
    "tabla_importancia", destino, mostrar = FALSE)

  grafico <- importancias |>
    dplyr::filter(Puesto <= 12) |>
    dplyr::mutate(etiqueta = ordenar_dentro(variable, importancia, Modelo)) |>
    ggplot2::ggplot(ggplot2::aes(importancia, etiqueta)) +
    ggplot2::geom_col(fill = "grey35", width = 0.7) +
    ggplot2::facet_wrap(~ Modelo, scales = "free_y") +
    escala_ordenada() +
    ggplot2::labs(
      title = "Variables más importantes en cada modelo",
      subtitle = sprintf("%s%s · ventana de %d días · de mayor a menor importancia",
                         if (is.null(etiqueta)) "" else paste0(etiqueta, " · "),
                         etiqueta_motor(motor), ventana$dias),
      x = "Importancia relativa (0-100)", y = NULL
    )

  guardar_grafico(grafico, "grafico_importancia", destino, ancho = 8, alto = 6.5)
  invisible(importancias)
}

# ---- Tasa de otorgamiento frente al riesgo predicho --------------------------
# El gráfico nuevo. Muestra, a lo largo de la escala de riesgo del modelo de
# crimen general, qué proporción de casos recibió medida de aseguramiento, con
# la distribución de casos por detrás para que se vea dónde hay masa y dónde
# la tasa se calcula con cuatro casos.

salida_otorgamiento <- function(evaluacion, destino, ventana,
                                columna_riesgo = paste0("riesgo_", RESPUESTA_PRINCIPAL)) {

  if (!columna_riesgo %in% names(evaluacion)) return(invisible(NULL))
  if (!COLUMNA_MEDIDA %in% names(evaluacion)) {
    paso("no está la columna %s: se salta el gráfico de otorgamiento", COLUMNA_MEDIDA)
    return(invisible(NULL))
  }

  medida <- evaluacion[[COLUMNA_MEDIDA]]

  tramos <- evaluacion |>
    dplyr::mutate(
      .riesgo = .data[[columna_riesgo]],
      .cualquiera = as.integer(!is.na(medida) & medida > 0),
      .intramural = as.integer(!is.na(medida) & medida == VALOR_INTRAMURAL),
      tramo = cut(.riesgo, breaks = seq(0, 1, length.out = BINS_RIESGO + 1),
                  include.lowest = TRUE)
    ) |>
    dplyr::group_by(tramo) |>
    dplyr::summarise(
      centro = mean(.riesgo),
      Casos = dplyr::n(),
      `Cualquier medida` = 100 * mean(.cualquiera),
      `Solo intramural` = 100 * mean(.intramural),
      `Reincidió` = 100 * mean(.data[[columna_y(RESPUESTA_PRINCIPAL)]])
    ) |>
    dplyr::filter(!is.na(tramo))

  if (nrow(tramos) == 0) return(invisible(NULL))

  tramos <- tramos |>
    dplyr::mutate(densidad = 100 * Casos / sum(Casos))

  guardar_tabla(
    tramos |>
      dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2))) |>
      dplyr::rename(`Tramo de riesgo` = tramo, `% de casos` = densidad),
    "tabla_otorgamiento_por_riesgo", destino, mostrar = FALSE)

  series <- tramos |>
    dplyr::select(centro, `Cualquier medida`, `Solo intramural`, `Reincidió`) |>
    tidyr::pivot_longer(-centro, names_to = "serie", values_to = "porcentaje")

  escala_densidad <- max(tramos$densidad, na.rm = TRUE)
  factor_densidad <- if (escala_densidad > 0) 100 / escala_densidad else 1

  grafico <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = tramos,
      ggplot2::aes(centro, densidad * factor_densidad),
      fill = "grey88", colour = NA,
      width = 1 / BINS_RIESGO * 0.95
    ) +
    ggplot2::geom_line(
      data = series,
      ggplot2::aes(centro, porcentaje, linetype = serie, group = serie),
      colour = "grey25"
    ) +
    ggplot2::geom_point(
      data = series,
      ggplot2::aes(centro, porcentaje, shape = serie),
      colour = "grey15", size = 1.7
    ) +
    ggplot2::scale_y_continuous(
      name = "Casos con medida de aseguramiento (%)",
      limits = c(0, 100),
      sec.axis = ggplot2::sec_axis(
        ~ . / factor_densidad, name = "Distribución de los casos (% del total)")
    ) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title = "Distribución de la tasa de otorgamiento según el riesgo predicho",
      subtitle = sprintf(
        "Modelo de crimen general · ventana de %d días · conjunto de evaluación",
        ventana$dias),
      x = "Riesgo predicho de reincidencia",
      caption = paste(
        "Las barras grises son el porcentaje de casos en cada tramo de riesgo",
        "(eje derecho).\nSi la decisión siguiera al riesgo, las líneas de medida",
        "subirían de izquierda a derecha."
      )
    )

  guardar_grafico(grafico, "grafico_otorgamiento_vs_riesgo", destino,
                  ancho = 7, alto = 4.8)
  invisible(tramos)
}

# ---- Deciles extremos y decisión observada -----------------------------------

salida_deciles <- function(evaluacion, destino, ventana) {

  riesgo <- paste0("riesgo_", RESPUESTA_PRINCIPAL)
  if (!riesgo %in% names(evaluacion)) return(invisible(NULL))

  deciles <- evaluacion |>
    dplyr::mutate(decil = dplyr::ntile(.data[[riesgo]], 10)) |>
    dplyr::filter(decil %in% c(1, 10)) |>
    dplyr::group_by(decil) |>
    dplyr::summarise(
      Casos = dplyr::n(),
      `Riesgo estimado` = mean(.data[[riesgo]]),
      `Reincide (%)` = 100 * mean(.data[[columna_y(RESPUESTA_PRINCIPAL)]]),
      `Capturas previas` = mean(capturas_previas),
      `Historial de penas` = mean(historial_penas),
      `Jóvenes (%)` = 100 * mean(joven),
      `Con medida (%)` = 100 * mean(.data[[COLUMNA_MEDIDA]] > 0, na.rm = TRUE),
      `Intramural (%)` = 100 * mean(.data[[COLUMNA_MEDIDA]] == VALOR_INTRAMURAL,
                                    na.rm = TRUE)
    ) |>
    dplyr::mutate(decil = dplyr::if_else(decil == 1, "Decil menos riesgoso",
                                         "Decil más riesgoso"))

  guardar_tabla(deciles |> dplyr::rename(Decil = decil), "tabla7_deciles", destino)

  por_decil <- evaluacion |>
    dplyr::mutate(decil = dplyr::ntile(.data[[riesgo]], 10)) |>
    dplyr::group_by(decil) |>
    dplyr::summarise(
      `Recibió medida intramural` = 100 * mean(.data[[COLUMNA_MEDIDA]] ==
                                                 VALOR_INTRAMURAL, na.rm = TRUE),
      Reincidió = 100 * mean(.data[[columna_y(RESPUESTA_PRINCIPAL)]])
    ) |>
    tidyr::pivot_longer(-decil, names_to = "serie", values_to = "porcentaje")

  grafico <- ggplot2::ggplot(por_decil, ggplot2::aes(
      decil, porcentaje, linetype = serie, shape = serie)) +
    ggplot2::geom_line(colour = "grey30") +
    ggplot2::geom_point(colour = "grey20", size = 1.8) +
    ggplot2::scale_x_continuous(breaks = 1:10) +
    ggplot2::labs(
      title = "La medida de aseguramiento frente al riesgo predicho",
      subtitle = sprintf("Por decil de riesgo · ventana de %d días", ventana$dias),
      x = "Decil de riesgo predicho", y = "Porcentaje de casos",
      caption = paste("Si la decisión siguiera al riesgo, las dos series",
                      "crecerían juntas de izquierda a derecha.")
    )

  guardar_grafico(grafico, "grafico_medida_vs_riesgo", destino)
}

# ---- Errores frente a la decisión observada ----------------------------------
# Se mantiene fijo el número de medidas intramurales efectivamente otorgadas y
# se pregunta a quiénes se les habrían asignado según el riesgo.

salida_errores_politica <- function(evaluacion, destino) {

  riesgo <- paste0("riesgo_", RESPUESTA_PRINCIPAL)
  if (!riesgo %in% names(evaluacion)) return(invisible(NULL))

  observado <- as.integer(evaluacion[[COLUMNA_MEDIDA]] == VALOR_INTRAMURAL)
  observado[is.na(observado)] <- 0L
  n_intramural <- sum(observado)

  if (n_intramural == 0 || n_intramural >= nrow(evaluacion)) return(invisible(NULL))

  umbral <- sort(evaluacion[[riesgo]], decreasing = TRUE)[n_intramural]
  recomendado <- as.integer(evaluacion[[riesgo]] >= umbral)

  errores <- dplyr::tibble(
    `Umbral de riesgo` = umbral,
    `Medidas intramurales otorgadas` = n_intramural,
    `Error tipo I: con medida, bajo riesgo` = sum(observado == 1 & recomendado == 0),
    `Error tipo II: sin medida, alto riesgo` = sum(observado == 0 & recomendado == 1),
    `Coinciden` = sum(observado == recomendado)
  ) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "Indicador",
                        values_to = "Valor")

  guardar_tabla(errores, "tabla_errores_politica", destino)
}
