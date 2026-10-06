# ==============================================================================
# 08. Entrenamiento
#
# Un modelo por cada combinación de motor y respuesta. Cada uno se guarda en
# caché por separado: si se agrega un motor nuevo, los que ya estaban no se
# vuelven a estimar.
# ==============================================================================

# El identificador de la respuesta y el nombre de la columna en el panel son
# lo mismo (y_rearresto_general, y_reimputacion_violento, etc.). La función
# queda por claridad y por si algún día vuelven a diferir.
columna_y <- function(respuesta) respuesta


entrenar_modelo <- function(respuesta, datos, predictores, motor, control) {

  columna <- columna_y(respuesta)
  positivos <- sum(datos[[columna]] == 1, na.rm = TRUE)

  if (positivos < MINIMO_POSITIVOS) {
    paso("%s / %s: solo %d positivos, no se estima",
         etiqueta_motor(motor), RESPUESTAS[[respuesta]], positivos)
    return(NULL)
  }

  configuracion <- CATALOGO_MOTORES[[motor]]

  x <- as.data.frame(datos[, predictores, drop = FALSE])
  y <- factor(datos[[columna]], levels = c(0, 1), labels = c("no", "si"))

  peso_positivo <- if (PESOS_DE_CLASE) sum(y == "no") / sum(y == "si") else 1
  pesos <- ifelse(y == "si", peso_positivo, 1)

  argumentos <- list(
    x = x, y = y,
    method = configuracion$metodo,
    metric = "ROC",
    trControl = control,
    tuneGrid = malla_de(motor)
  )

  if (!is.null(configuracion$preproceso)) {
    argumentos$preProcess <- configuracion$preproceso
  }

  # naive_bayes y knn no aceptan pesos de observación en caret. El campo va en
  # el catálogo y no se compara contra el método, porque el método puede ser una
  # lista (el envoltorio propio de XGBoost) y no un nombre.
  if (isTRUE(configuracion$acepta_pesos %||% TRUE)) {
    argumentos$weights <- pesos
  }

  if (!is.null(configuracion$extra)) {
    argumentos <- c(argumentos, configuracion$extra)
  }

  set.seed(SEMILLA)

  ajustado <- tryCatch(
    suppressWarnings(do.call(caret::train, argumentos)),
    error = function(e) {
      paso("%s / %s: falló (%s)", etiqueta_motor(motor), RESPUESTAS[[respuesta]],
           conditionMessage(e))
      NULL
    }
  )

  if (is.null(ajustado)) return(NULL)

  list(
    ajuste = ajustado,
    motor = motor,
    respuesta = respuesta,
    predictores = predictores,
    positivos = positivos
  )
}

# Submuestra para la búsqueda, respetando el orden temporal de los pliegues.
preparar_busqueda <- function(particion) {

  entrenamiento <- particion$entrenamiento
  tope <- if (MODO_RAPIDO) FILAS_MODO_RAPIDO else MAXIMO_BUSQUEDA

  control <- caret::trainControl(
    method = "cv",
    index = particion$indices_cv,
    indexOut = particion$indices_validacion,
    classProbs = TRUE,
    summaryFunction = caret::twoClassSummary,
    savePredictions = "final",
    allowParallel = TRUE
  )

  if (is.null(tope) || nrow(entrenamiento) <= tope) {
    return(list(datos = entrenamiento, control = control))
  }

  set.seed(SEMILLA)
  filas <- sort(sample(seq_len(nrow(entrenamiento)), tope))
  submuestra <- entrenamiento[filas, ]

  control$index <- lapply(particion$anios_cv,
                          function(a) which(submuestra$anio < a))
  control$indexOut <- lapply(particion$anios_cv,
                             function(a) which(submuestra$anio == a))

  paso("búsqueda de hiperparámetros sobre %s casos", miles(tope))

  list(datos = submuestra, control = control)
}

entrenar_todo <- function(particion, predictores, motores, ventana,
                          estrato = ESTRATO_TODOS) {

  dias <- ventana$dias

  busqueda <- preparar_busqueda(particion)
  modelos <- list()

  # La huella entra en la clave: si cambian los predictores o el ano de corte,
  # los modelos viejos dejan de reusarse solos.
  firma <- huella(predictores, ANIO_CORTE, MAXIMO_BUSQUEDA, PESOS_DE_CLASE)
  etiqueta_estrato <- nombre_corto(estrato)

  for (motor in motores) {
    for (respuesta in names(RESPUESTAS)) {

      clave <- sprintf("modelo_v%d_%s_%s_%s_%s", dias, etiqueta_estrato,
                       motor, respuesta, firma)

      ajustado <- con_cache(clave, {
        paso("estimando %s / %s ...", etiqueta_motor(motor), RESPUESTAS[[respuesta]])
        cronometro(
          sprintf("%s / %s", motor, respuesta),
          entrenar_modelo(respuesta, busqueda$datos, predictores, motor,
                          busqueda$control)
        )
      })

      if (!is.null(ajustado)) {
        modelos[[paste(motor, respuesta, sep = "|")]] <- ajustado
      }
    }
  }

  if (length(modelos) == 0) {
    paso("ventana %d / %s: no se pudo estimar ningún modelo", dias, estrato)
    return(NULL)
  }

  paso("modelos estimados: %d", length(modelos))
  modelos
}
