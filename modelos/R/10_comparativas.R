# ==============================================================================
# 11. Comparativas
#
# Dos preguntas distintas:
#   · qué motor predice mejor, dentro de una misma ventana
#   · qué cambia al pasar de un año a dos años de seguimiento
# ==============================================================================

# ---- Dentro de una ventana ---------------------------------------------------

salida_comparativa_motores <- function(desempeno, destino_tablas, destino_graficos,
                                       ventana) {

  dias <- ventana$dias

  tabla <- desempeno |>
    dplyr::select(Senal, Tipo, Motor, AUC, `AUC PR`, Precisión, Sensibilidad,
                  F1, `F1 máximo`, `Umbral F1 máx`, Brier) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

  guardar_tabla(tabla, "tabla_comparativa_motores", destino_tablas)

  # Con 16 respuestas no entran 16 paneles: se facetea por señal y cada panel
  # compara los motores dentro de los cuatro tipos de delito.
  grafico <- desempeno |>
    dplyr::mutate(Senal = factor(Senal, levels = unname(SENALES)),
                  Tipo = factor(Tipo, levels = unname(TIPOS))) |>
    ggplot2::ggplot(ggplot2::aes(`F1 máximo`, Tipo, fill = Motor)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75),
                      width = 0.7, colour = "grey30", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = c("grey35", "grey75")) +
    ggplot2::facet_wrap(~ Senal, scales = "free_x") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::labs(
      title = "Comparación de motores por F1",
      subtitle = sprintf("Ventana de %d días · conjunto de evaluación", dias),
      x = "F1 máximo", y = NULL,
      caption = paste("F1 en el umbral que lo maximiza. El umbral de cada motor",
                      "va en la tabla:\ncomparar en 0.5 castigaría a los motores",
                      "peor calibrados por razones ajenas a su capacidad.")
    )

  guardar_grafico(grafico, "grafico_comparativa_motores", destino_graficos,
                  ancho = 9, alto = 7)

  invisible(tabla)
}

salida_curvas_pr <- function(modelos, evaluacion, destino, ventana,
                             respuesta = RESPUESTA_PRINCIPAL) {

  del_modelo <- Filter(function(m) m$respuesta == respuesta, modelos)
  if (length(del_modelo) == 0) return(invisible(NULL))

  observado <- evaluacion[[columna_y(respuesta)]]
  if (dplyr::n_distinct(observado) < 2) return(invisible(NULL))

  puntos <- dplyr::bind_rows(lapply(del_modelo, function(modelo) {
    curva <- curva_f1(observado, predecir(modelo, evaluacion))
    indices <- unique(round(seq(1, length(curva$recall), length.out = 300)))

    dplyr::tibble(
      Motor = etiqueta_motor(modelo$motor),
      recall = curva$recall[indices],
      precision = curva$precision[indices]
    )
  }))

  grafico <- ggplot2::ggplot(puntos, ggplot2::aes(
      recall, precision, linetype = Motor, group = Motor)) +
    ggplot2::geom_hline(yintercept = mean(observado), linetype = "dotted",
                        colour = "grey50") +
    ggplot2::geom_line(colour = "grey25") +
    ggplot2::labs(
      title = "Precisión y recall por motor",
      subtitle = sprintf("%s · ventana de %d días", RESPUESTAS[[respuesta]],
                         ventana$dias),
      x = "Recall (sensibilidad)", y = "Precisión",
      caption = paste("La línea punteada es la tasa base: la precisión de",
                      "marcar a todo el mundo como reincidente.")
    )

  guardar_grafico(grafico, "grafico_precision_recall", destino, ancho = 6.5, alto = 4.5)
}

# ---- Entre ventanas ----------------------------------------------------------

salida_comparativa_ventanas <- function(desempenos, destino_tablas,
                                       destino_graficos) {

  todo <- dplyr::bind_rows(desempenos)

  if ("Estrato" %in% names(todo)) {
    todo <- todo |> dplyr::filter(Estrato == ESTRATO_TODOS)
  }

  # Con una sola ventana no hay nada que comparar.
  if (dplyr::n_distinct(todo$Ventana) < 2) return(invisible(NULL))

  tabla <- todo |>
    dplyr::select(Ventana, Motor, Modelo, AUC, F1, `F1 máximo`, Precisión,
                  Sensibilidad) |>
    dplyr::arrange(Modelo, Motor, Ventana) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

  guardar_tabla(tabla, "tabla_todos_los_modelos", destino_tablas, mostrar = FALSE)

  # El mismo motor en las dos ventanas, lado a lado.
  ancho <- todo |>
    dplyr::select(Motor, Modelo, Ventana, `F1 máximo`) |>
    tidyr::pivot_wider(names_from = Ventana, values_from = `F1 máximo`,
                       names_prefix = "F1 a ") |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

  # La diferencia siempre se lee ventana larga menos ventana corta, sin depender
  # del orden en que hayan quedado las columnas.
  columnas_f1 <- sort(grep("^F1 a ", names(ancho), value = TRUE))
  if (length(columnas_f1) == 2) {
    ancho$Diferencia <- round(ancho[[columnas_f1[2]]] - ancho[[columnas_f1[1]]], 4)
  }

  guardar_tabla(ancho, "tabla_f1_por_ventana", destino_tablas)

  grafico <- todo |>
    dplyr::mutate(Ventana = factor(sprintf("%d días", Ventana))) |>
    ggplot2::ggplot(ggplot2::aes(Motor, `F1 máximo`, fill = Ventana)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75),
                      width = 0.7, colour = "grey30", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = c("grey35", "grey75")) +
    ggplot2::facet_wrap(~ Modelo, scales = "free_y") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "F1 por motor y ventana de seguimiento",
      subtitle = "Un año frente a dos años",
      x = NULL, y = "F1 máximo",
      caption = paste(
        "Ojo al comparar: con dos años de seguimiento la tasa de reincidencia",
        "es más alta y\nel conjunto de evaluación es más chico, así que parte",
        "de la diferencia no es del modelo."
      )
    )

  guardar_grafico(grafico, "grafico_f1_por_ventana", destino_graficos,
                  ancho = 8, alto = 6)

  invisible(tabla)
}

# ---- La Tabla 6, como la de PRiSMA ------------------------------------------
# La fila de rearresto de la grilla, con las seis métricas del documento en el
# umbral de Youden, y al lado los números de PRiSMA. Las otras tres señales van
# en una segunda tabla con la misma estructura, como robustez a la definición.

salida_tabla_prisma <- function(desempeno, motor, destino) {

  columnas <- c("AUC", "Exactitud (Y)", "Sensibilidad (Y)", "Especificidad (Y)",
                "Error tipo I (Y)", "Error tipo II (Y)", "Umbral Youden")
  nombres <- c("AUC", "Exactitud", "Sensibilidad", "Especificidad",
               "Error tipo I", "Error tipo II", "Umbral")

  base <- desempeno |>
    dplyr::filter(motor_id == motor) |>
    dplyr::mutate(
      tipo = vapply(respuesta_id, function(id) META_RESPUESTAS[[id]]$tipo,
                    character(1)),
      senal = vapply(respuesta_id, function(id) META_RESPUESTAS[[id]]$senal,
                     character(1))
    )

  # --- Tabla principal: la señal de PRiSMA, lado a lado ---
  principal <- base |>
    dplyr::filter(senal == SENAL_PRISMA) |>
    dplyr::select(tipo, Tipo, dplyr::all_of(columnas)) |>
    stats::setNames(c("tipo", "Modelo", nombres)) |>
    dplyr::left_join(
      PRISMA_TABLA6 |>
        dplyr::rename(`AUC PRiSMA` = AUC, `Exactitud PRiSMA` = Exactitud),
      by = "tipo") |>
    dplyr::mutate(
      `Diferencia AUC` = AUC - `AUC PRiSMA`,
      tipo = factor(tipo, levels = names(TIPOS))
    ) |>
    dplyr::arrange(tipo) |>
    dplyr::select(Modelo, AUC, `AUC PRiSMA`, `Diferencia AUC`, Exactitud,
                  `Exactitud PRiSMA`, Sensibilidad, Especificidad,
                  `Error tipo I`, `Error tipo II`, Umbral) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

  cat("\n")
  paso("Tabla 6, con la señal de PRiSMA (%s) y el motor %s:",
       SENALES[[SENAL_PRISMA]], etiqueta_motor(motor))
  guardar_tabla(principal, "tabla6_prisma", destino)

  # --- Robustez: las otras tres señales con la misma estructura ---
  robustez <- base |>
    dplyr::filter(senal != SENAL_PRISMA) |>
    dplyr::mutate(tipo = factor(tipo, levels = names(TIPOS)),
                  Senal = factor(Senal, levels = unname(SENALES))) |>
    dplyr::arrange(Senal, tipo) |>
    dplyr::select(Senal, Modelo = Tipo, dplyr::all_of(columnas)) |>
    stats::setNames(c("Senal", "Modelo", nombres)) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

  guardar_tabla(robustez, "tabla6_robustez_por_senal", destino, mostrar = FALSE)

  # --- El AUC de las cuatro señales lado a lado, un vistazo ---
  panorama <- base |>
    dplyr::mutate(tipo = factor(tipo, levels = names(TIPOS)),
                  Senal = factor(Senal, levels = unname(SENALES))) |>
    dplyr::select(Senal, tipo, AUC) |>
    tidyr::pivot_wider(names_from = tipo, values_from = AUC) |>
    dplyr::arrange(Senal) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

  names(panorama)[-1] <- unname(TIPOS[names(panorama)[-1]])

  prisma_fila <- dplyr::tibble(Senal = "PRiSMA (documento)") |>
    dplyr::bind_cols(
      stats::setNames(as.list(PRISMA_TABLA6$AUC), unname(TIPOS[PRISMA_TABLA6$tipo]))
    )

  panorama <- dplyr::bind_rows(prisma_fila, panorama |>
                                 dplyr::mutate(Senal = as.character(Senal)))

  cat("\n")
  paso("AUC por señal y tipo de delito, con PRiSMA en la primera fila:")
  guardar_tabla(panorama, "tabla_auc_panorama", destino)

  invisible(principal)
}


# ---- La tabla de una señal ---------------------------------------------------
# Cuatro filas, una por tipo de delito, con las seis métricas de PRiSMA en el
# umbral de Youden. Para la señal de PRiSMA se agregan sus columnas al lado.

salida_tabla_senal <- function(desempeno, motor, destino, senal_id) {

  columnas <- c("AUC", "Exactitud (Y)", "Sensibilidad (Y)", "Especificidad (Y)",
                "Error tipo I (Y)", "Error tipo II (Y)", "Umbral Youden")
  nombres <- c("AUC", "Exactitud", "Sensibilidad", "Especificidad",
               "Error tipo I", "Error tipo II", "Umbral")

  tabla <- desempeno |>
    dplyr::filter(motor_id == motor) |>
    dplyr::mutate(
      tipo = vapply(respuesta_id, function(id) META_RESPUESTAS[[id]]$tipo, character(1)),
      senal = vapply(respuesta_id, function(id) META_RESPUESTAS[[id]]$senal, character(1))
    ) |>
    dplyr::filter(senal == senal_id) |>
    dplyr::mutate(tipo = factor(tipo, levels = names(TIPOS))) |>
    dplyr::arrange(tipo) |>
    dplyr::select(tipo, Modelo = Tipo, dplyr::all_of(columnas)) |>
    stats::setNames(c("tipo", "Modelo", nombres))

  if (senal_id == SENAL_PRISMA) {
    tabla <- tabla |>
      dplyr::left_join(
        PRISMA_TABLA6 |>
          dplyr::rename(`AUC PRiSMA` = AUC, `Exactitud PRiSMA` = Exactitud),
        by = "tipo") |>
      dplyr::mutate(`Diferencia AUC` = AUC - `AUC PRiSMA`) |>
      dplyr::select(Modelo, AUC, `AUC PRiSMA`, `Diferencia AUC`, Exactitud,
                    `Exactitud PRiSMA`, Sensibilidad, Especificidad,
                    `Error tipo I`, `Error tipo II`, Umbral)
  } else {
    tabla <- tabla |> dplyr::select(-tipo)
  }

  tabla <- tabla |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

  cat("\n")
  paso("%s · %s:", SENALES[[senal_id]], etiqueta_motor(motor))
  guardar_tabla(tabla, "tabla6", destino)
  invisible(tabla)
}


# ---- Entre grupos de delito --------------------------------------------------
# Es la comparación que responde la pregunta de fondo: ¿se predice igual de
# bien el riesgo en un hurto que en un delito violento?

salida_comparativa_estratos <- function(desempenos, destino_tablas,
                                        destino_graficos) {

  todo <- dplyr::bind_rows(desempenos)

  if (dplyr::n_distinct(todo$Estrato) < 2) return(invisible(NULL))

  tabla <- todo |>
    dplyr::select(Ventana, Estrato, Modelo, Motor, AUC, `F1 máximo`,
                  Precisión, Sensibilidad) |>
    dplyr::arrange(Modelo, Estrato, Ventana) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

  guardar_tabla(tabla, "tabla_por_grupo_de_delito", destino_tablas)

  # F1 de cada grupo, con las dos ventanas lado a lado.
  ancho <- todo |>
    dplyr::select(Estrato, Modelo, Ventana, `F1 máximo`) |>
    tidyr::pivot_wider(names_from = Ventana, values_from = `F1 máximo`,
                       names_prefix = "F1 a ") |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

  guardar_tabla(ancho, "tabla_f1_por_grupo_y_ventana", destino_tablas)

  grafico <- todo |>
    dplyr::mutate(Ventana = factor(sprintf("%d días", Ventana)),
                  etiqueta = ordenar_dentro(Estrato, `F1 máximo`, Modelo)) |>
    ggplot2::ggplot(ggplot2::aes(`F1 máximo`, etiqueta, fill = Ventana)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75),
                      width = 0.7, colour = "grey30", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = c("grey35", "grey75")) +
    ggplot2::facet_wrap(~ Modelo, scales = "free_y") +
    escala_ordenada() +
    ggplot2::labs(
      title = "Capacidad predictiva por grupo de delito",
      subtitle = "F1 máximo en el conjunto de evaluación",
      x = "F1 máximo", y = NULL,
      caption = paste(
        "Cada grupo tiene su propia tasa base de reincidencia, así que un F1",
        "más alto no significa\nsolo un mejor modelo. La tabla de estratos trae",
        "las tasas para poder descontarlo."
      )
    )

  guardar_grafico(grafico, "grafico_f1_por_grupo_de_delito", destino_graficos,
                  ancho = 8.5, alto = 6.5)

  # AUC por grupo, que no depende de la tasa base y por eso es más comparable
  # entre grupos que el F1.
  grafico_auc <- todo |>
    dplyr::mutate(Ventana = factor(sprintf("%d días", Ventana)),
                  etiqueta = ordenar_dentro(Estrato, AUC, Modelo)) |>
    ggplot2::ggplot(ggplot2::aes(AUC, etiqueta, fill = Ventana)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75),
                      width = 0.7, colour = "grey30", linewidth = 0.2) +
    ggplot2::geom_vline(xintercept = 0.5, linetype = "dotted", colour = "grey40") +
    ggplot2::scale_fill_manual(values = c("grey35", "grey75")) +
    ggplot2::facet_wrap(~ Modelo, scales = "free_y") +
    escala_ordenada() +
    ggplot2::labs(
      title = "AUC por grupo de delito",
      subtitle = "El AUC no depende de la tasa base, así que compara mejor entre grupos",
      x = "AUC", y = NULL,
      caption = "La línea punteada marca el desempeño de una predicción al azar."
    )

  guardar_grafico(grafico_auc, "grafico_auc_por_grupo_de_delito",
                  destino_graficos, ancho = 8.5, alto = 6.5)

  invisible(tabla)
}


salida_resumen_ventanas <- function(resumenes, destino_tablas) {
  tabla <- dplyr::bind_rows(resumenes) |>
    dplyr::select(Ventana, Senal, Tipo, Casos, Reinciden, `Tasa (%)`)
  guardar_tabla(tabla, "tabla_respuestas_por_ventana", destino_tablas,
                mostrar = FALSE)
}
