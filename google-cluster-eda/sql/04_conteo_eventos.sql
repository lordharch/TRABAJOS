-- =============================================================================
-- 04_conteo_eventos.sql
-- Frecuencia POBLACIONAL de eventos de instancia por tipo de evento, nivel de
-- prioridad y tipo de colección. Resultado pequeño (decenas de filas).
-- type: enum EventType del proto v3 (0 SUBMIT, 1 QUEUE, 2 ENABLE, 3 SCHEDULE,
-- 4 EVICT, 5 FAIL, 6 FINISH, 7 KILL, 8 LOST, 9 UPDATE_PENDING, 10 UPDATE_RUNNING).
-- La etiqueta se asigna en R.
-- =============================================================================
SELECT
  type            AS tipo_evento,
  CASE
    WHEN priority BETWEEN 0   AND 99  THEN 'free'
    WHEN priority BETWEEN 100 AND 115 THEN 'beb'
    WHEN priority BETWEEN 116 AND 119 THEN 'mid'
    WHEN priority BETWEEN 120 AND 359 THEN 'prod'
    WHEN priority >= 360              THEN 'monitoring'
    ELSE 'sin_prioridad'
  END             AS nivel,
  collection_type,
  COUNT(*)        AS n_eventos
FROM `{{proyecto_datos}}`.clusterdata_2019_{{celda}}.instance_events
WHERE (alloc_collection_id IS NULL OR alloc_collection_id = 0)
GROUP BY tipo_evento, nivel, collection_type
ORDER BY tipo_evento, nivel, collection_type;
