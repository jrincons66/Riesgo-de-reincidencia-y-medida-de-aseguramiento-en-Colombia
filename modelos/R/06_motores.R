# ==============================================================================
# 07. Catálogo de motores
#
# Cada motor es una entrada con su método de caret, su malla y el paquete que
# necesita. Agregar uno nuevo es agregar un elemento a esta lista: el resto del
# pipeline lo recoge solo.
# ==============================================================================

# ------------------------------------------------------------------------------
# Envoltorio propio de XGBoost
#
# El metodo "xgbTree" que trae caret no funciona con xgboost 2.1 en adelante:
# xgboost cambio el objeto del modelo por uno que no admite que le asignen
# campos, y caret hace exactamente eso (modelFit$xNames <- colnames(x)). El
# error que sale es "ALTLIST classes must provide a Set_elt method".
#
# Esto llama a xgboost directamente y guarda el modelo dentro de una lista
# comun, que si acepta asignaciones. nthread = 1 deja el paralelismo en manos
# de caret, que es quien reparte los pliegues.
# ------------------------------------------------------------------------------

XGB_PROPIO <- list(

  label = "XGBoost",
  library = "xgboost",
  type = "Classification",

  parameters = data.frame(
    parameter = c("nrounds", "max_depth", "eta", "gamma", "colsample_bytree",
                  "min_child_weight", "subsample"),
    class = rep("numeric", 7),
    label = c("nrounds", "max_depth", "eta", "gamma", "colsample_bytree",
              "min_child_weight", "subsample"),
    stringsAsFactors = FALSE
  ),

  grid = function(x, y, len = NULL, search = "grid") {
    expand.grid(nrounds = 150, max_depth = 3, eta = 0.15, gamma = 0,
                colsample_bytree = 0.8, min_child_weight = 10, subsample = 0.8)
  },

  loop = NULL,

  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    matriz <- as.matrix(x)
    etiquetas <- as.numeric(y) - 1

    datos <- if (!is.null(wts)) {
      xgboost::xgb.DMatrix(matriz, label = etiquetas, weight = wts)
    } else {
      xgboost::xgb.DMatrix(matriz, label = etiquetas)
    }

    parametros <- list(
      objective = "binary:logistic", eval_metric = "logloss",
      max_depth = param$max_depth, eta = param$eta, gamma = param$gamma,
      colsample_bytree = param$colsample_bytree,
      min_child_weight = param$min_child_weight,
      subsample = param$subsample, nthread = 1
    )

    modelo <- xgboost::xgb.train(params = parametros, data = datos,
                                 nrounds = param$nrounds, verbose = 0)

    list(booster = modelo, niveles = lev, columnas = colnames(matriz))
  },

  predict = function(modelFit, newdata, submodels = NULL) {
    p <- stats::predict(
      modelFit$booster,
      as.matrix(as.data.frame(newdata)[, modelFit$columnas, drop = FALSE])
    )
    factor(ifelse(p > 0.5, modelFit$niveles[2], modelFit$niveles[1]),
           levels = modelFit$niveles)
  },

  prob = function(modelFit, newdata, submodels = NULL) {
    p <- stats::predict(
      modelFit$booster,
      as.matrix(as.data.frame(newdata)[, modelFit$columnas, drop = FALSE])
    )
    salida <- data.frame(1 - p, p)
    names(salida) <- modelFit$niveles
    salida
  },

  varImp = function(object, ...) {
    importancia <- xgboost::xgb.importance(model = object$booster)
    valores <- stats::setNames(rep(0, length(object$columnas)), object$columnas)
    if (!is.null(importancia) && nrow(importancia) > 0) {
      valores[importancia$Feature] <- importancia$Gain
    }
    if (max(valores) > 0) valores <- 100 * valores / max(valores)
    data.frame(Overall = as.numeric(valores), row.names = names(valores))
  },

  sort = function(x) x[order(x$nrounds, x$max_depth, x$eta), ],
  levels = function(x) x$niveles
)


CATALOGO_MOTORES <- list(

  glmnet = list(
    etiqueta = "Logística penalizada",
    metodo = "glmnet",
    paquete = "glmnet",
    # Referencia lineal. Sirve de piso: si un modelo de árboles no le gana,
    # la no linealidad no estaba aportando nada.
    malla = expand.grid(alpha = c(0, 0.5, 1),
                        lambda = 10 ^ seq(-4, -1, length.out = 4)),
    malla_rapida = expand.grid(alpha = 1, lambda = 0.001),
    preproceso = c("center", "scale")
  ),

  rpart = list(
    etiqueta = "Árbol de decisión",
    metodo = "rpart",
    paquete = "rpart",
    # Interpretable y rápido, pero inestable. Va más como contraste que como
    # candidato serio.
    malla = expand.grid(cp = c(0.0005, 0.001, 0.005, 0.01)),
    malla_rapida = expand.grid(cp = 0.001),
    preproceso = NULL
  ),

  ranger = list(
    etiqueta = "Bosque aleatorio",
    metodo = "ranger",
    paquete = "ranger",
    malla = expand.grid(mtry = c(3, 5, 8), splitrule = "gini",
                        min.node.size = c(10, 50)),
    malla_rapida = expand.grid(mtry = 5, splitrule = "gini", min.node.size = 50),
    preproceso = NULL,
    extra = list(importance = "impurity")
  ),

  # El "xgbTree" de caret esta roto con xgboost 2.1+. Este usa el envoltorio
  # propio de arriba y acepta la misma malla.
  xgboost = list(
    etiqueta = "Gradient boosting (XGBoost)",
    metodo = XGB_PROPIO,
    paquete = "xgboost",
    # Es el motor del documento original.
    malla = expand.grid(nrounds = c(150, 300), max_depth = c(3, 6),
                        eta = c(0.05, 0.15), gamma = 0, colsample_bytree = 0.8,
                        min_child_weight = 10, subsample = 0.8),
    malla_rapida = expand.grid(nrounds = 150, max_depth = 3, eta = 0.15, gamma = 0,
                               colsample_bytree = 0.8, min_child_weight = 10,
                               subsample = 0.8),
    preproceso = NULL
  ),

  gbm = list(
    etiqueta = "Gradient boosting (GBM)",
    metodo = "gbm",
    paquete = "gbm",
    malla = expand.grid(n.trees = c(150, 300), interaction.depth = c(3, 5),
                        shrinkage = c(0.05, 0.1), n.minobsinnode = 10),
    malla_rapida = expand.grid(n.trees = 150, interaction.depth = 3,
                               shrinkage = 0.1, n.minobsinnode = 10),
    preproceso = NULL,
    extra = list(verbose = FALSE)
  ),

  naive_bayes = list(
    etiqueta = "Naive Bayes",
    metodo = "naive_bayes",
    paquete = "naivebayes",
    malla = expand.grid(laplace = 0, usekernel = c(FALSE, TRUE), adjust = 1),
    malla_rapida = expand.grid(laplace = 0, usekernel = TRUE, adjust = 1),
    preproceso = NULL,
    acepta_pesos = FALSE
  ),

  knn = list(
    etiqueta = "K vecinos",
    metodo = "knn",
    paquete = "caret",
    # Caro en bases grandes: conviene dejarlo fuera de MOTORES salvo que se
    # quiera el contraste.
    malla = expand.grid(k = c(25, 50, 100)),
    malla_rapida = expand.grid(k = 50),
    preproceso = c("center", "scale"),
    acepta_pesos = FALSE
  )
)

# Los que piden un paquete que no está se saltan avisando, no tumban la corrida.
motores_disponibles <- function(pedidos = MOTORES) {

  desconocidos <- setdiff(pedidos, names(CATALOGO_MOTORES))
  if (length(desconocidos) > 0) {
    paso("motores que no están en el catálogo y se ignoran: %s",
         paste(desconocidos, collapse = ", "))
    pedidos <- intersect(pedidos, names(CATALOGO_MOTORES))
  }

  disponibles <- character(0)

  for (nombre in pedidos) {
    paquete <- CATALOGO_MOTORES[[nombre]]$paquete
    if (requireNamespace(paquete, quietly = TRUE)) {
      disponibles <- c(disponibles, nombre)
    } else {
      paso('falta el paquete "%s": se salta el motor %s   install.packages("%s")',
           paquete, nombre, paquete)
    }
  }

  if (length(disponibles) == 0) {
    stop("No quedó ningún motor disponible. Instalá al menos uno de: ",
         paste(pedidos, collapse = ", "))
  }

  paso("motores que se van a estimar: %s", paste(disponibles, collapse = ", "))
  disponibles
}

etiqueta_motor <- function(nombre) {
  if (is.null(CATALOGO_MOTORES[[nombre]])) return(nombre)
  CATALOGO_MOTORES[[nombre]]$etiqueta
}

malla_de <- function(nombre) {
  motor <- CATALOGO_MOTORES[[nombre]]
  if (MODO_RAPIDO && !is.null(motor$malla_rapida)) motor$malla_rapida else motor$malla
}
