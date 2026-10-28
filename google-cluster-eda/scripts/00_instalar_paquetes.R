# =============================================================================
# 00_instalar_paquetes.R — Instala (solo si faltan) los paquetes del proyecto
# Ejecutar una vez:  source("scripts/00_instalar_paquetes.R")
# =============================================================================

paquetes <- c(
  # Manipulación e importación
  "tidyverse",   # dplyr, tidyr, readr, ggplot2, tibble, purrr, stringr, forcats
  "arrow",       # lectura/escritura Parquet
  "lubridate",   # utilidades de tiempo (se usa solo para duraciones, ver informe)
  "jsonlite",
  # Visualización
  "scales",
  "hexbin",      # requerido por geom_hex()
  # Acceso a Google BigQuery desde R
  "bigrquery",
  "DBI",
  # Informe
  "knitr",
  "rmarkdown",
  "quarto"       # interfaz R para Quarto (requiere tener instalado Quarto CLI)
)

faltantes <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltantes) > 0) {
  message("Instalando: ", paste(faltantes, collapse = ", "))
  install.packages(faltantes, repos = "https://cloud.r-project.org")
} else {
  message("Todos los paquetes ya están instalados.")
}

# Versión mínima recomendada de R: 4.1 (pipe nativo |> y funciones anónimas \(x)).
if (getRversion() < "4.1.0") {
  warning("Se recomienda R >= 4.1.0. Versión actual: ", getRversion())
}
