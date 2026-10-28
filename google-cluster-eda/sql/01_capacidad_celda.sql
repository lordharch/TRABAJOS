-- =============================================================================
-- 01_capacidad_celda.sql
-- Capacidad total de la celda: suma, sobre todas las máquinas, de la capacidad
-- máxima registrada en machine_events. Misma lógica que query_cell_capacity del
-- cuaderno oficial clusterdata_analysis_colab.ipynb.
-- Unidades: CPU en NCU (Normalized Compute Units), memoria en unidades
-- normalizadas (NMU). Ambas escaladas respecto a la máquina más grande de la traza.
-- Población: todas las máquinas de la celda (no es una muestra).
-- =============================================================================
SELECT
  COUNT(*)        AS n_maquinas,
  SUM(cpu_cap)    AS capacidad_cpu_ncu,
  SUM(memory_cap) AS capacidad_memoria_nmu
FROM (
  SELECT
    machine_id,
    MAX(capacity.cpus)   AS cpu_cap,
    MAX(capacity.memory) AS memory_cap
  FROM `{{proyecto_datos}}`.clusterdata_2019_{{celda}}.machine_events
  GROUP BY machine_id
);
