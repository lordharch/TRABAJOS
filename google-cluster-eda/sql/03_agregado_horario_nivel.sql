-- =============================================================================
-- 03_agregado_horario_nivel.sql
-- Agregación POBLACIONAL (todas las instancias de la celda) del uso de CPU y
-- memoria por hora de traza y nivel de prioridad. Basada en
-- query_per_instance_usage_priority + query_per_tier_utilization_time_series
-- del cuaderno oficial.
--
-- * hora_indice = FLOOR(start_time / 3,6e9): horas completas transcurridas desde
--   el origen temporal de la traza (start_time está en microsegundos). NO es una
--   hora de reloj ni una fecha de calendario.
-- * Solo ventanas de al menos 5 minutos (como en el cuaderno), de modo que cada
--   instancia aporta como máximo 12 ventanas por hora y la utilización media de
--   la celda puede calcularse en R como SUM(uso) / (12 * capacidad).
-- * Se excluyen instancias dentro de alloc sets (alloc_collection_id <> 0).
-- =============================================================================
WITH uso AS (
  SELECT
    CAST(FLOOR(start_time / (1e6 * 60 * 60)) AS INT64) AS hora_indice,
    collection_id,
    instance_index,
    average_usage.cpus   AS cpu,
    average_usage.memory AS mem
  FROM `{{proyecto_datos}}`.clusterdata_2019_{{celda}}.instance_usage
  WHERE (alloc_collection_id IS NULL OR alloc_collection_id = 0)
    AND (end_time - start_time) >= (5 * 60 * 1e6)
),
prioridades AS (
  SELECT
    collection_id,
    instance_index,
    MAX(priority) AS prioridad
  FROM `{{proyecto_datos}}`.clusterdata_2019_{{celda}}.instance_events
  WHERE (alloc_collection_id IS NULL OR alloc_collection_id = 0)
  GROUP BY collection_id, instance_index
)
SELECT
  u.hora_indice,
  CASE
    WHEN p.prioridad BETWEEN 0   AND 99  THEN 'free'
    WHEN p.prioridad BETWEEN 100 AND 115 THEN 'beb'
    WHEN p.prioridad BETWEEN 116 AND 119 THEN 'mid'
    WHEN p.prioridad BETWEEN 120 AND 359 THEN 'prod'
    WHEN p.prioridad >= 360              THEN 'monitoring'
    ELSE 'sin_prioridad'
  END                       AS nivel,
  COUNT(*)                  AS n_registros,
  APPROX_COUNT_DISTINCT(CONCAT(CAST(u.collection_id AS STRING), '-',
                               CAST(u.instance_index AS STRING)))
                            AS n_instancias_aprox,
  SUM(u.cpu)                AS suma_cpu_ncu,
  SUM(u.mem)                AS suma_mem_nmu,
  AVG(u.cpu)                AS media_cpu_ncu,
  AVG(u.mem)                AS media_mem_nmu,
  MAX(u.cpu)                AS max_cpu_ncu,
  MAX(u.mem)                AS max_mem_nmu
FROM uso AS u
LEFT JOIN prioridades AS p
  ON  u.collection_id  = p.collection_id
  AND u.instance_index = p.instance_index
GROUP BY hora_indice, nivel
ORDER BY hora_indice, nivel;
