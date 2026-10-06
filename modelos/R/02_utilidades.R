# ==============================================================================
# 02. Utilidades
# Tema de gráficos, guardado, caché y mensajes. Nada de lógica del modelo.
# ==============================================================================

# ---- Mensajes ----------------------------------------------------------------

# En R 4.4 esto ya viene en base, pero acá no se puede asumir la versión.
`%||%` <- function(x, y) if (is.null(x)) y else x


separador <- function(titulo) {
  cat("\n", strrep("=", 78), "\n", titulo, "\n", strrep("=", 78), "\n\n", sep = "")
}

paso <- function(...) cat("  ", sprintf(...), "\n", sep = "")

# Separador de miles. Hay que declarar tambien el decimal: si los dos son el
# punto, format() lanza un aviso en cada llamada.
miles <- function(x) {
  format(round(as.numeric(x)), big.mark = ".", decimal.mark = ",",
         trim = TRUE, scientific = FALSE)
}

# Huella corta de un conjunto de valores. Sirve para que la clave del cache
# cambie sola cuando cambian los predictores, la malla o el ano de corte: si no,
# un modelo viejo se reusa contra un panel nuevo y falla al predecir, o peor,
# predice con las variables equivocadas sin avisar.
huella <- function(...) {
  texto <- paste(sort(as.character(unlist(list(...)))), collapse = "|")
  codigos <- utf8ToInt(texto)
  suma <- sum(codigos * seq_along(codigos)) %% 99991
  sprintf("%05d", abs(suma))
}


cronometro <- function(etiqueta, expresion) {
  inicio <- Sys.time()
  valor <- force(expresion)
  segundos <- as.numeric(difftime(Sys.time(), inicio, units = "secs"))
  paso("%s: %.1f s", etiqueta, segundos)
  invisible(valor)
}

# ---- Carpetas ----------------------------------------------------------------

# Cada ventana escribe en su propia carpeta, y las comparaciones entre ventanas
# van aparte. Así no se pisan archivos entre corridas.
carpeta <- function(...) {
  ruta <- file.path(...)
  dir.create(ruta, showWarnings = FALSE, recursive = TRUE)
  ruta
}

# Nombre de carpeta a partir de una etiqueta con tildes y espacios.
nombre_corto <- function(texto) {
  limpio <- chartr("ÁÉÍÓÚÜÑáéíóúüñ", "AEIOUUNaeiouun", texto)
  limpio <- tolower(gsub("[^A-Za-z0-9]+", "_", limpio))
  gsub("^_|_$", "", limpio)
}

carpeta_ventana <- function(ventana, estrato = ESTRATO_TODOS,
                            tipo = c("tablas", "graficos", "modelos")) {
  tipo <- match.arg(tipo)
  carpeta(SALIDAS, sprintf("ventana_%d", ventana), nombre_corto(estrato), tipo)
}

# Una carpeta por señal de reincidencia, con los cuatro tipos de delito adentro.
carpeta_senal <- function(ventana, senal_id, tipo = c("tablas", "graficos")) {
  tipo <- match.arg(tipo)
  carpeta(SALIDAS, sprintf("ventana_%d", ventana),
          nombre_corto(SENALES[[senal_id]]), tipo)
}

carpeta_comparativas <- function(tipo = c("tablas", "graficos")) {
  tipo <- match.arg(tipo)
  carpeta(SALIDAS, "comparativas", tipo)
}

# ---- Caché -------------------------------------------------------------------
# expresion llega como promesa: si el archivo existe no se evalúa nunca, que es
# justamente lo que hace rápida la segunda corrida.

con_cache <- function(clave, expresion, activo = USAR_CACHE) {
  archivo <- file.path(carpeta(CACHE), paste0(clave, ".rds"))

  if (activo && file.exists(archivo)) {
    paso("caché: %s", basename(archivo))
    return(readRDS(archivo))
  }

  valor <- force(expresion)
  saveRDS(valor, archivo)
  valor
}

limpiar_cache <- function(patron = NULL) {
  archivos <- list.files(CACHE, pattern = patron, full.names = TRUE)
  file.remove(archivos)
  paso("borrados %d archivos de caché", length(archivos))
}

# ---- Guardado ----------------------------------------------------------------

guardar_tabla <- function(tabla, archivo, destino, mostrar = TRUE) {
  # Los nombres de columna del código no llevan ñ ni tildes que puedan chocar
  # entre codificaciones en Windows; se ponen recién acá, al escribir.
  names(tabla) <- sub("^Senal$", "Se\u00f1al", names(tabla))
  readr::write_excel_csv(tabla, file.path(destino, paste0(archivo, ".csv")))
  if (mostrar) print(as.data.frame(tabla), row.names = FALSE, digits = 4)
  invisible(tabla)
}

guardar_grafico <- function(grafico, archivo, destino, ancho = 6.5, alto = 4.2) {
  ggplot2::ggsave(file.path(destino, paste0(archivo, ".png")), grafico,
                  width = ancho, height = alto, dpi = 300, bg = "white")
  invisible(grafico)
}

# ---- Estilo ------------------------------------------------------------------
# Sobrio, en escala de grises, pensado para imprimirse en una tesis.

tema_tesis <- function(base_size = 11) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "grey92", linewidth = 0.3),
      panel.border = ggplot2::element_rect(colour = "grey60", linewidth = 0.4),
      plot.title = ggplot2::element_text(size = base_size + 1, face = "bold",
                                         margin = ggplot2::margin(b = 3)),
      plot.subtitle = ggplot2::element_text(size = base_size - 1, colour = "grey30",
                                            margin = ggplot2::margin(b = 8)),
      plot.caption = ggplot2::element_text(size = base_size - 3, colour = "grey40",
                                           hjust = 0, margin = ggplot2::margin(t = 8)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey95", colour = "grey60",
                                               linewidth = 0.4),
      axis.title = ggplot2::element_text(size = base_size - 1)
    )
}

ESCALA_GRIS <- c("grey20", "grey45", "grey65", "grey80")

# ---- Orden dentro de facetas -------------------------------------------------
# ggplot ordena un factor una sola vez para todo el gráfico, así que en un
# facet_wrap las barras de un panel salen ordenadas por los valores de otro.
# Estas dos funciones son el truco de tidytext sin la dependencia: se pega el
# panel al nombre para ordenar y se quita al momento de dibujar la etiqueta.

ordenar_dentro <- function(variable, por, dentro, separador = "___") {
  nuevo <- paste(variable, dentro, sep = separador)
  stats::reorder(nuevo, por)
}

escala_ordenada <- function(separador = "___", ...) {
  ggplot2::scale_y_discrete(
    labels = function(x) gsub(paste0(separador, ".*$"), "", x), ...
  )
}
