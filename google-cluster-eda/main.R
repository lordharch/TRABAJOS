# =============================================================================
# main.R — Ejecuta el flujo completo en el orden correcto.
#
# Uso: abrir google-cluster-eda.Rproj en RStudio y ejecutar
#        source("main.R")
#      o desde una terminal en la raíz del proyecto:
#        Rscript main.R
# El origen de los datos se controla con CONFIG$fuente_datos en config.R.
# =============================================================================

source("config.R")
source("scripts/utilidades.R")
verificar_raiz_proyecto()
crear_carpetas()

ejecutar <- function(script) {
  message("\n==================== ", script, " ====================")
  inicio <- Sys.time()
  # Entorno nuevo por script: evita dependencias ocultas entre pasos.
  sys.source(script, envir = new.env(parent = globalenv()), chdir = FALSE)
  message(sprintf("%s terminado en %.1f s", script,
                  as.numeric(difftime(Sys.time(), inicio, units = "secs"))))
}

# 1. Obtención de datos crudos -------------------------------------------------
switch(CONFIG$fuente_datos,
  bigquery = ejecutar("scripts/01_extraer_bigquery.R"),
  simulado = ejecutar("scripts/01b_datos_simulados.R"),
  local    = message("Usando archivos existentes en data/raw/ (fuente 'local')."),
  stop("CONFIG$fuente_datos debe ser 'bigquery', 'local' o 'simulado'.")
)

# 2-5. Procesamiento y análisis -----------------------------------------------
ejecutar("scripts/02_importar_limpiar.R")
ejecutar("scripts/03_estadistica_descriptiva.R")
ejecutar("scripts/04_graficos.R")
ejecutar("scripts/05_preparacion_modelado.R")

# 6. Informe final (requiere Quarto CLI: https://quarto.org/docs/get-started/) --
if (nzchar(Sys.which("quarto")) && requireNamespace("quarto", quietly = TRUE)) {
  message("\n==================== Informe Quarto ====================")
  quarto::quarto_render(file.path(RUTAS$reporte, "informe.qmd"))
  message("Informe generado: reporte/informe.html")
} else {
  message("Quarto no está disponible: abre reporte/informe.qmd en RStudio y pulsa 'Render'.")
}

message("\nFlujo completo finalizado.")
