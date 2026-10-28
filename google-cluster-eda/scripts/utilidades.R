# =============================================================================
# utilidades.R — Funciones auxiliares compartidas por todos los scripts
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(arrow)
  library(ggplot2)
  library(scales)
})

# -----------------------------------------------------------------------------
# Comprobación de directorio de trabajo
# -----------------------------------------------------------------------------
verificar_raiz_proyecto <- function() {
  if (!file.exists("config.R") || !dir.exists("scripts")) {
    stop("El directorio de trabajo debe ser la raíz del proyecto ",
         "(la carpeta que contiene config.R). Abre google-cluster-eda.Rproj ",
         "en RStudio o usa setwd().", call. = FALSE)
  }
  invisible(TRUE)
}

crear_carpetas <- function() {
  for (r in RUTAS) dir.create(r, showWarnings = FALSE, recursive = TRUE)
  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# Códigos del esquema v3 (clusterdata_trace_format_v3.proto)
# -----------------------------------------------------------------------------
ETIQUETAS_EVENTO <- c(
  "0" = "SUBMIT", "1" = "QUEUE", "2" = "ENABLE", "3" = "SCHEDULE",
  "4" = "EVICT", "5" = "FAIL", "6" = "FINISH", "7" = "KILL",
  "8" = "LOST", "9" = "UPDATE_PENDING", "10" = "UPDATE_RUNNING"
)

ETIQUETAS_TIPO_COLECCION <- c("0" = "JOB", "1" = "ALLOC_SET")

ETIQUETAS_CLASE_PLANIF <- c(
  "0" = "0 MOST_INSENSITIVE", "1" = "1 INSENSITIVE",
  "2" = "2 SENSITIVE", "3" = "3 MOST_SENSITIVE"
)

# Niveles de prioridad. Rangos del cuaderno oficial de análisis (free 0-99,
# beb 100-115, mid 116-119, prod >= 120) separando monitoring (>= 360) como en
# Tirmazi et al. (2020), "Borg: the Next Generation".
NIVELES_PRIORIDAD <- c("free", "beb", "mid", "prod", "monitoring")

asignar_nivel <- function(prioridad) {
  nivel <- dplyr::case_when(
    is.na(prioridad)                       ~ NA_character_,
    prioridad >= 0   & prioridad <= 99     ~ "free",
    prioridad >= 100 & prioridad <= 115    ~ "beb",
    prioridad >= 116 & prioridad <= 119    ~ "mid",
    prioridad >= 120 & prioridad <= 359    ~ "prod",
    prioridad >= 360                       ~ "monitoring",
    TRUE                                   ~ NA_character_
  )
  factor(nivel, levels = NIVELES_PRIORIDAD)
}

etiquetar <- function(codigo, etiquetas) {
  out <- unname(etiquetas[as.character(codigo)])
  out[is.na(out) & !is.na(codigo)] <- paste0("codigo_", codigo[is.na(out) & !is.na(codigo)])
  factor(out, levels = unique(c(unname(etiquetas), sort(unique(out[!out %in% etiquetas])))))
}

# -----------------------------------------------------------------------------
# Lectura de SQL con parámetros {{nombre}}
# -----------------------------------------------------------------------------
leer_sql <- function(archivo, parametros = list()) {
  sql <- paste(readLines(file.path(RUTAS$sql, archivo), warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  for (p in names(parametros)) {
    sql <- gsub(paste0("{{", p, "}}"), as.character(parametros[[p]]), sql, fixed = TRUE)
  }
  if (grepl("{{", sql, fixed = TRUE)) {
    stop("Quedan parámetros sin reemplazar en ", archivo, call. = FALSE)
  }
  sql
}

# -----------------------------------------------------------------------------
# Lectura flexible de tablas crudas: Parquet si existe, si no CSV
# -----------------------------------------------------------------------------
leer_tabla_raw <- function(nombre_base) {
  ruta_parquet <- file.path(RUTAS$raw, paste0(nombre_base, ".parquet"))
  ruta_csv     <- file.path(RUTAS$raw, paste0(nombre_base, ".csv"))
  if (file.exists(ruta_parquet)) {
    message("Leyendo ", ruta_parquet)
    return(tibble::as_tibble(arrow::read_parquet(ruta_parquet)))
  }
  if (file.exists(ruta_csv)) {
    message("Leyendo ", ruta_csv)
    # Los identificadores se leen como texto: superan la precisión de double.
    cabecera <- names(readr::read_csv(ruta_csv, n_max = 0, show_col_types = FALSE))
    tipos <- readr::cols(.default = readr::col_guess())
    for (id in intersect(c("collection_id", "machine_id"), cabecera)) {
      tipos$cols[[id]] <- readr::col_character()
    }
    return(readr::read_csv(ruta_csv, show_col_types = FALSE, col_types = tipos,
                           guess_max = 100000))
  }
  stop("No se encontró ", ruta_parquet, " ni ", ruta_csv,
       ". Ejecuta la extracción (01) o coloca los archivos exportados en data/raw/.",
       call. = FALSE)
}

exportar_tabla <- function(df, nombre_base, carpeta = RUTAS$processed,
                           csv = CONFIG$exportar_csv, parquet = TRUE) {
  if (parquet) arrow::write_parquet(df, file.path(carpeta, paste0(nombre_base, ".parquet")))
  if (csv) readr::write_csv(df, file.path(carpeta, paste0(nombre_base, ".csv")), na = "")
  invisible(df)
}

guardar_resultado <- function(df, nombre_base) {
  readr::write_csv(df, file.path(RUTAS$results, paste0(nombre_base, ".csv")), na = "")
  invisible(df)
}

# -----------------------------------------------------------------------------
# Registro de transformaciones (trazabilidad de la limpieza)
# -----------------------------------------------------------------------------
.registro <- new.env()
.registro$pasos <- tibble::tibble(paso = integer(), tabla = character(),
                                  accion = character(), justificacion = character(),
                                  filas_antes = numeric(), filas_despues = numeric())

registrar <- function(tabla, accion, justificacion, filas_antes = NA, filas_despues = NA) {
  .registro$pasos <- dplyr::bind_rows(.registro$pasos, tibble::tibble(
    paso = nrow(.registro$pasos) + 1L, tabla = tabla, accion = accion,
    justificacion = justificacion,
    filas_antes = as.numeric(filas_antes), filas_despues = as.numeric(filas_despues)
  ))
  message(sprintf("[%02d] %s: %s", nrow(.registro$pasos), tabla, accion))
  invisible(NULL)
}

# -----------------------------------------------------------------------------
# Estadística descriptiva
# -----------------------------------------------------------------------------
asimetria <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x); s <- stats::sd(x)
  if (s == 0) return(NA_real_)
  (n / ((n - 1) * (n - 2))) * sum(((x - m) / s)^3)
}

resumen_numerico <- function(x) {
  x_ok <- x[is.finite(x)]
  q <- if (length(x_ok) > 0) stats::quantile(x_ok, c(0.25, 0.5, 0.75, 0.95, 0.99), names = FALSE)
       else rep(NA_real_, 5)
  tibble::tibble(
    n_total     = length(x),
    n_validos   = length(x_ok),
    n_faltantes = sum(is.na(x)),
    media       = if (length(x_ok)) mean(x_ok) else NA_real_,
    mediana     = q[2],
    desv_est    = if (length(x_ok) > 1) stats::sd(x_ok) else NA_real_,   # muestral (n - 1)
    varianza    = if (length(x_ok) > 1) stats::var(x_ok) else NA_real_,  # muestral (n - 1)
    cv          = if (length(x_ok) > 1 && mean(x_ok) != 0) stats::sd(x_ok) / mean(x_ok) else NA_real_,
    minimo      = if (length(x_ok)) min(x_ok) else NA_real_,
    q1          = q[1],
    q3          = q[3],
    p95         = q[4],
    p99         = q[5],
    maximo      = if (length(x_ok)) max(x_ok) else NA_real_,
    rango_iqr   = q[3] - q[1],
    asimetria   = asimetria(x_ok)
  )
}

# Marca atípicos con la regla de Tukey (1,5 × IQR). No elimina nada.
marcar_atipico_iqr <- function(x, k = 1.5) {
  q <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  iqr <- q[2] - q[1]
  !is.na(x) & (x < q[1] - k * iqr | x > q[2] + k * iqr)
}

# -----------------------------------------------------------------------------
# Tema y exportación de gráficos
# -----------------------------------------------------------------------------
PALETA_NIVEL <- c(free = "#4C78A8", beb = "#F58518", mid = "#54A24B",
                  prod = "#E45756", monitoring = "#B279A2")

tema_proyecto <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(
      plot.title       = element_text(face = "bold", size = base + 3),
      plot.subtitle    = element_text(colour = "grey30"),
      plot.caption     = element_text(colour = "grey45", size = base - 3, hjust = 0),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      strip.text       = element_text(face = "bold")
    )
}

PIE_FUENTE <- function() {
  paste0("Fuente: Google Cluster Data 2019 (trazas v3), celda ", CONFIG$celda,
         if (identical(CONFIG$fuente_datos, "simulado")) " — DATOS SIMULADOS, solo prueba técnica" else "",
         ". Licencia CC-BY 4.0.")
}

guardar_grafico <- function(grafico, nombre, ancho = CONFIG$ancho_cm, alto = CONFIG$alto_cm) {
  ruta_png <- file.path(RUTAS$plots, paste0(nombre, ".png"))
  ggsave(ruta_png, grafico, width = ancho, height = alto, units = "cm",
         dpi = CONFIG$dpi, bg = "white")
  if (isTRUE(CONFIG$exportar_pdf)) {
    # cairo_pdf maneja tildes y eñes; si no está disponible se usa pdf().
    dispositivo <- if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else grDevices::pdf
    ggsave(file.path(RUTAS$plots, paste0(nombre, ".pdf")), grafico,
           width = ancho, height = alto, units = "cm", device = dispositivo)
  }
  message("Gráfico guardado: ", ruta_png)
  invisible(grafico)
}

etiqueta_n <- function(n) format(n, big.mark = ".", decimal.mark = ",", scientific = FALSE)
