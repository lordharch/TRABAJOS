# =============================================================================
# 01_extraer_bigquery.R — Consulta las trazas 2019 en BigQuery y guarda los
# resultados (muestra y agregados) en data/raw/ como Parquet.
#
# Requisitos previos (ver README):
#   1. Proyecto de Google Cloud propio (con facturación o en modo sandbox) y la
#      API de BigQuery habilitada.
#   2. Variable de entorno GCP_BILLING_PROJECT o CONFIG$proyecto_facturacion.
#   3. Autenticación: la primera vez se abrirá el navegador (bigrquery::bq_auth()).
#
# Cada consulta se valida primero con una ejecución en seco (dry run) que estima
# los bytes a procesar; si supera CONFIG$max_gb_por_consulta, se detiene.
# =============================================================================

source("config.R")
source("scripts/utilidades.R")
verificar_raiz_proyecto()
crear_carpetas()

suppressPackageStartupMessages(library(bigrquery))

if (CONFIG$proyecto_facturacion == "") {
  stop("Define el proyecto de facturación: Sys.setenv(GCP_BILLING_PROJECT = 'mi-proyecto') ",
       "o edita CONFIG$proyecto_facturacion en config.R.", call. = FALSE)
}

# Autenticación interactiva (token en caché tras la primera vez).
bq_auth()

parametros_sql <- list(
  proyecto_datos = CONFIG$proyecto_datos,
  celda          = CONFIG$celda,
  modulo_muestra = CONFIG$modulo_muestra
)

# -----------------------------------------------------------------------------
# Ejecuta una consulta con control de coste y devuelve un tibble
# -----------------------------------------------------------------------------
ejecutar_consulta <- function(archivo_sql, n_max = Inf) {
  sql <- leer_sql(archivo_sql, parametros_sql)

  bytes <- as.numeric(bq_perform_query_dry_run(sql, billing = CONFIG$proyecto_facturacion))
  gb <- bytes / 1e9
  message(sprintf("%s: ejecución en seco estima %.2f GB procesados.", archivo_sql, gb))
  if (gb > CONFIG$max_gb_por_consulta) {
    stop(sprintf(paste0("La consulta %s procesaría %.1f GB (> %s GB permitidos). ",
                        "Aumenta CONFIG$max_gb_por_consulta conscientemente o reduce la consulta."),
                 archivo_sql, gb, CONFIG$max_gb_por_consulta), call. = FALSE)
  }

  tabla_resultado <- bq_project_query(CONFIG$proyecto_facturacion, sql)
  n_filas <- as.numeric(bq_table_nrow(tabla_resultado))
  message(sprintf("%s: %s filas en el resultado.", archivo_sql, etiqueta_n(n_filas)))
  if (n_filas > n_max) {
    stop(sprintf(paste0("El resultado tiene %s filas (> %s). Aumenta CONFIG$modulo_muestra ",
                        "para una muestra más pequeña, o exporta a Cloud Storage (ver README)."),
                 etiqueta_n(n_filas), etiqueta_n(n_max)), call. = FALSE)
  }

  # bigint = "numeric": las marcas temporales (microsegundos, ~2,7e12 como máximo
  # en un mes) caben con exactitud en double. Los IDs ya llegan como texto.
  datos <- bq_table_download(tabla_resultado, bigint = "numeric", quiet = FALSE)
  tibble::as_tibble(datos)
}

# -----------------------------------------------------------------------------
# 0. Esquema real de las tablas (documentación y verificación)
# -----------------------------------------------------------------------------
esquema <- tryCatch(ejecutar_consulta("00_esquema_tablas.sql"), error = function(e) {
  stop("No se pudo leer INFORMATION_SCHEMA (", conditionMessage(e), "). Verifica el esquema ",
       "manualmente con: bq show --schema --format=prettyjson ",
       "google.com:google-cluster-data:clusterdata_2019_", CONFIG$celda, ".instance_usage",
       call. = FALSE)
})
write_csv(esquema, file.path(RUTAS$raw, "esquema_tablas.csv"))

columnas_requeridas <- c(
  "instance_usage.start_time", "instance_usage.end_time",
  "instance_usage.average_usage.cpus", "instance_usage.average_usage.memory",
  "instance_usage.maximum_usage.cpus", "instance_usage.alloc_collection_id",
  "instance_events.priority", "instance_events.type", "instance_events.time",
  "instance_events.scheduling_class", "instance_events.resource_request.cpus",
  "machine_events.capacity.cpus"
)
presentes <- paste(esquema$table_name, esquema$field_path, sep = ".")
faltan <- setdiff(columnas_requeridas, presentes)
if (length(faltan) > 0) {
  stop("El esquema de la celda no contiene: ", paste(faltan, collapse = ", "),
       ". Revisa data/raw/esquema_tablas.csv antes de continuar.", call. = FALSE)
}
message("Esquema verificado: todas las columnas requeridas existen.")
print(dplyr::filter(esquema, paste(table_name, field_path, sep = ".") %in% columnas_requeridas))

# -----------------------------------------------------------------------------
# 1. Capacidad de la celda (población)
# -----------------------------------------------------------------------------
capacidad <- ejecutar_consulta("01_capacidad_celda.sql")
write_csv(capacidad, file.path(RUTAS$raw, "capacidad_celda.csv"))

# -----------------------------------------------------------------------------
# 2. Muestra de registros de uso por instancia
# -----------------------------------------------------------------------------
muestra <- ejecutar_consulta("02_muestra_uso_instancias.sql", n_max = CONFIG$max_filas_descarga)
arrow::write_parquet(muestra, file.path(RUTAS$raw, "muestra_uso_instancias.parquet"))

# -----------------------------------------------------------------------------
# 3. Agregado horario por nivel de prioridad (población)
# -----------------------------------------------------------------------------
agregado <- ejecutar_consulta("03_agregado_horario_nivel.sql")
arrow::write_parquet(agregado, file.path(RUTAS$raw, "agregado_horario_nivel.parquet"))

# -----------------------------------------------------------------------------
# 4. Conteo de eventos por tipo y nivel (población)
# -----------------------------------------------------------------------------
eventos <- ejecutar_consulta("04_conteo_eventos.sql")
write_csv(eventos, file.path(RUTAS$raw, "conteo_eventos.csv"))

# -----------------------------------------------------------------------------
# Metadatos de la extracción (reproducibilidad)
# -----------------------------------------------------------------------------
metadatos <- tibble::tibble(
  fuente            = "bigquery",
  proyecto_datos    = CONFIG$proyecto_datos,
  dataset           = paste0("clusterdata_2019_", CONFIG$celda),
  celda             = CONFIG$celda,
  modulo_muestra    = CONFIG$modulo_muestra,
  filas_muestra     = nrow(muestra),
  fecha_extraccion  = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  version_bigrquery = as.character(utils::packageVersion("bigrquery"))
)
write_csv(metadatos, file.path(RUTAS$raw, "metadatos_extraccion.csv"))
message("Extracción finalizada. Archivos en ", RUTAS$raw)
