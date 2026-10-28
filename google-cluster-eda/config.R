# =============================================================================
# config.R — Parámetros globales del proyecto
# Proyecto: Análisis exploratorio de Google Cluster Data 2019 (trazas Borg v3)
# Todos los scripts cargan este archivo. Modifica aquí, no en cada script.
# =============================================================================

CONFIG <- list(

  # ---------------------------------------------------------------------------
  # Origen de los datos crudos (data/raw/):
  #   "bigquery" -> ejecuta las consultas SQL de sql/ desde R (bigrquery) y
  #                 guarda los resultados en data/raw/.
  #   "local"    -> usa archivos ya exportados manualmente a data/raw/
  #                 (por ejemplo, desde la consola de BigQuery o Cloud Storage).
  #   "simulado" -> genera datos SINTÉTICOS con el mismo esquema, solo para
  #                 probar que el flujo funciona. NO sirven para conclusiones.
  # ---------------------------------------------------------------------------
  fuente_datos = "simulado",

  # Proyecto de Google Cloud que paga (o usa el sandbox gratuito) las consultas.
  # Se recomienda definirlo como variable de entorno en ~/.Renviron:
  #   GCP_BILLING_PROJECT=mi-proyecto-gcp
  proyecto_facturacion = Sys.getenv("GCP_BILLING_PROJECT", unset = ""),

  # Proyecto público que aloja las trazas y celda a analizar (a, b, ..., h).
  proyecto_datos = "google.com:google-cluster-data",
  celda = "a",

  # Muestreo determinista por colección (job): se conserva una de cada
  # `modulo_muestra` colecciones según FARM_FINGERPRINT(collection_id).
  # 1000 -> ~0,1 % de las colecciones con TODOS sus registros de uso.
  # Ajusta tras revisar el tamaño estimado con la ejecución en seco (dry run).
  modulo_muestra = 1000,

  # Límite de seguridad: si la ejecución en seco estima más bytes que este
  # valor, la consulta NO se ejecuta. 1 TB/mes es el nivel gratuito de BigQuery.
  max_gb_por_consulta = 500,

  # Máximo de filas a descargar en R desde BigQuery para la muestra.
  max_filas_descarga = 5e6,

  # Exportar además a CSV los datos procesados (la versión Parquet siempre se crea).
  exportar_csv = TRUE,

  # Tamaño de submuestra para gráficos de dispersión (legibilidad y rendimiento).
  n_puntos_dispersion = 50000,

  # Sección opcional de modelado: entrena modelos base SOLO si es TRUE.
  # Las métricas se calculan con los datos reales al ejecutar; nunca se
  # escriben a mano.
  entrenar_modelo_base = TRUE,

  # Proporción temporal para la partición cronológica entrenamiento/prueba.
  proporcion_entrenamiento = 0.8,

  # Resolución de los gráficos exportados.
  dpi = 300,
  ancho_cm = 20,
  alto_cm = 12,
  exportar_pdf = TRUE,

  semilla = 2019
)

# Rutas del proyecto (relativas a la raíz, que debe ser el directorio de trabajo)
RUTAS <- list(
  sql        = "sql",
  raw        = file.path("data", "raw"),
  processed  = file.path("data", "processed"),
  plots      = "plots",
  results    = "results",
  reporte    = "reporte"
)
