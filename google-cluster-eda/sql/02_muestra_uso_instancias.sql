-- =============================================================================
-- 02_muestra_uso_instancias.sql
-- Muestra representativa de registros de uso (ventanas de ~5 min) por instancia,
-- enriquecida con atributos de la instancia tomados de instance_events.
--
-- Diseño de la muestra:
--   * Muestreo determinista por COLECCIÓN (job): se conserva una colección de
--     cada {{modulo_muestra}} según FARM_FINGERPRINT(collection_id). Se guardan
--     TODOS los registros de uso de las colecciones seleccionadas, lo que
--     conserva la estructura temporal de cada tarea. Es reproducible: la misma
--     consulta devuelve siempre las mismas colecciones.
--   * Se excluyen los registros de instancias que corren dentro de un alloc set
--     (alloc_collection_id no nulo y distinto de 0), igual que en el cuaderno
--     oficial, para no mezclar reservas con tareas.
--   * NO se filtra por duración de la ventana: la variable derivada
--     ventana_completa se construye en R y se documenta allí.
--
-- Tablas y columnas (verificadas en clusterdata_trace_format_v3.proto y en el
-- cuaderno oficial):
--   instance_usage : start_time, end_time, collection_id, instance_index,
--                    machine_id, alloc_collection_id, collection_type,
--                    average_usage.{cpus,memory}, maximum_usage.{cpus,memory},
--                    assigned_memory, page_cache_memory, cycles_per_instruction,
--                    memory_accesses_per_instruction, sample_rate
--   instance_events: time, type, collection_id, instance_index, priority,
--                    scheduling_class, alloc_collection_id,
--                    resource_request.{cpus,memory}
-- Comprueba los tipos con sql/00_esquema_tablas.sql (p. ej., que `type` y
-- `scheduling_class` sean INT64) antes de ejecutar.
--
-- Coste: la consulta lee las columnas seleccionadas de las dos tablas completas
-- de la celda (el filtro por hash no reduce los bytes escaneados). Haz SIEMPRE
-- una ejecución en seco (dry run) para conocer los bytes antes de ejecutarla.
-- =============================================================================
WITH uso AS (
  SELECT
    start_time,
    end_time,
    collection_id,
    instance_index,
    machine_id,
    collection_type,
    average_usage.cpus               AS cpu_prom,
    average_usage.memory             AS mem_prom,
    maximum_usage.cpus               AS cpu_max,
    maximum_usage.memory             AS mem_max,
    assigned_memory                  AS mem_asignada,
    page_cache_memory                AS mem_cache_paginas,
    cycles_per_instruction           AS ciclos_por_instruccion,
    memory_accesses_per_instruction  AS accesos_mem_por_instruccion,
    sample_rate                      AS tasa_muestreo
  FROM `{{proyecto_datos}}`.clusterdata_2019_{{celda}}.instance_usage
  WHERE (alloc_collection_id IS NULL OR alloc_collection_id = 0)
    AND MOD(ABS(FARM_FINGERPRINT(CAST(collection_id AS STRING))), {{modulo_muestra}}) = 0
),
eventos AS (
  SELECT
    collection_id,
    instance_index,
    MAX(priority)                AS prioridad,
    MAX(scheduling_class)        AS clase_planificacion,
    MAX(resource_request.cpus)   AS cpu_solicitada,
    MAX(resource_request.memory) AS mem_solicitada,
    COUNT(*)                     AS n_eventos,
    -- Marcas temporales extremas (0 o el máximo de INT64) se excluyen de forma
    -- defensiva al calcular tiempos de vida; ver la sección de tiempo del
    -- documento "Google cluster-usage traces v3".
    MIN(IF(type = 3 AND time > 0 AND time < 9223372036854775807, time, NULL))
                                 AS t_primer_schedule,
    MAX(IF(type IN (4, 5, 6, 7, 8) AND time > 0 AND time < 9223372036854775807, time, NULL))
                                 AS t_ultimo_terminal,
    ARRAY_AGG(type ORDER BY time DESC LIMIT 1)[OFFSET(0)]
                                 AS ultimo_tipo_evento
  FROM `{{proyecto_datos}}`.clusterdata_2019_{{celda}}.instance_events
  WHERE (alloc_collection_id IS NULL OR alloc_collection_id = 0)
    AND MOD(ABS(FARM_FINGERPRINT(CAST(collection_id AS STRING))), {{modulo_muestra}}) = 0
  GROUP BY collection_id, instance_index
)
SELECT
  -- Los identificadores INT64 se convierten a texto: superan la precisión
  -- exacta de un double en R (2^53).
  CAST(u.collection_id AS STRING) AS collection_id,
  u.instance_index,
  CAST(u.machine_id AS STRING)    AS machine_id,
  u.start_time,
  u.end_time,
  u.collection_type,
  u.cpu_prom,
  u.mem_prom,
  u.cpu_max,
  u.mem_max,
  u.mem_asignada,
  u.mem_cache_paginas,
  u.ciclos_por_instruccion,
  u.accesos_mem_por_instruccion,
  u.tasa_muestreo,
  e.prioridad,
  e.clase_planificacion,
  e.cpu_solicitada,
  e.mem_solicitada,
  e.n_eventos,
  e.t_primer_schedule,
  e.t_ultimo_terminal,
  e.ultimo_tipo_evento
FROM uso AS u
LEFT JOIN eventos AS e
  ON  u.collection_id  = e.collection_id
  AND u.instance_index = e.instance_index;
