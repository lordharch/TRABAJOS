-- =============================================================================
-- 00_esquema_tablas.sql
-- Verifica el esquema REAL (nombres, rutas anidadas y tipos) de las tablas
-- de la celda elegida antes de ejecutar el resto de consultas.
-- Coste: consultar INFORMATION_SCHEMA es muy barato (metadatos, no datos).
-- Alternativa por línea de comandos:
--   bq show --schema --format=prettyjson google.com:google-cluster-data:clusterdata_2019_a.instance_usage
-- =============================================================================
SELECT
  table_name,
  field_path,
  data_type
FROM `{{proyecto_datos}}`.clusterdata_2019_{{celda}}.INFORMATION_SCHEMA.COLUMN_FIELD_PATHS
WHERE table_name IN ('instance_usage', 'instance_events', 'machine_events', 'collection_events')
ORDER BY table_name, field_path;
