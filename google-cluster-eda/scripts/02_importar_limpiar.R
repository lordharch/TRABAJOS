# =============================================================================
# 02_importar_limpiar.R — Importación, inspección, control de calidad,
# construcción de variables derivadas y exportación de datos procesados.
#
# Entradas (data/raw/):
#   muestra_uso_instancias.(parquet|csv)  registros de uso de la muestra
#   agregado_horario_nivel.(parquet|csv)  agregado poblacional por hora y nivel
#   conteo_eventos.(csv|parquet)          frecuencias poblacionales de eventos
#   capacidad_celda.csv                   capacidad total de la celda
# Salidas:
#   data/processed/uso_procesado.*        nivel registro (ventana de uso)
#   data/processed/instancias_resumen.*   nivel instancia (una fila por tarea)
#   data/processed/agregado_horario.*     serie horaria poblacional con utilización
#   data/processed/eventos.*              eventos etiquetados
#   results/calidad_*.csv, results/registro_transformaciones.csv
# =============================================================================

source("config.R")
source("scripts/utilidades.R")
verificar_raiz_proyecto()
crear_carpetas()

# =============================================================================
# 1. IMPORTACIÓN
# =============================================================================
uso_raw       <- leer_tabla_raw("muestra_uso_instancias")
agregado_raw  <- leer_tabla_raw("agregado_horario_nivel")
eventos_raw   <- leer_tabla_raw("conteo_eventos")
capacidad_raw <- leer_tabla_raw("capacidad_celda")

registrar("muestra_uso_instancias", "Importación", "Lectura de data/raw/",
          filas_despues = nrow(uso_raw))

# Asegurar tipos: IDs como texto; el resto numérico.
uso_raw <- uso_raw |>
  mutate(
    collection_id = as.character(collection_id),
    machine_id    = as.character(machine_id),
    across(c(instance_index, start_time, end_time, collection_type, cpu_prom, mem_prom,
             cpu_max, mem_max, mem_asignada, mem_cache_paginas, ciclos_por_instruccion,
             accesos_mem_por_instruccion, tasa_muestreo, prioridad, clase_planificacion,
             cpu_solicitada, mem_solicitada, n_eventos, t_primer_schedule,
             t_ultimo_terminal, ultimo_tipo_evento), as.numeric)
  )

# =============================================================================
# 2. INSPECCIÓN: dimensiones, nombres, tipos y faltantes
# =============================================================================
inspeccionar <- function(df, nombre) {
  tibble(
    tabla       = nombre,
    variable    = names(df),
    tipo_r      = vapply(df, function(x) class(x)[1], character(1)),
    n_filas     = nrow(df),
    n_faltantes = vapply(df, function(x) sum(is.na(x)), numeric(1)),
    pct_faltantes = round(100 * n_faltantes / nrow(df), 3),
    n_infinitos = vapply(df, function(x) if (is.numeric(x)) sum(is.infinite(x)) else 0, numeric(1)),
    n_distintos = vapply(df, function(x) dplyr::n_distinct(x), numeric(1))
  )
}

inspeccion <- bind_rows(
  inspeccionar(uso_raw, "muestra_uso_instancias"),
  inspeccionar(agregado_raw, "agregado_horario_nivel"),
  inspeccionar(eventos_raw, "conteo_eventos"),
  inspeccionar(capacidad_raw, "capacidad_celda")
)
guardar_resultado(inspeccion, "calidad_inspeccion_variables")
message("Dimensiones de la muestra: ", nrow(uso_raw), " filas x ", ncol(uso_raw), " columnas")
dplyr::glimpse(uso_raw)

# =============================================================================
# 3. CONTROL DE CALIDAD
# =============================================================================
claves <- c("collection_id", "instance_index", "machine_id", "start_time")

controles <- uso_raw |>
  mutate(
    dup_exacto   = duplicated(uso_raw),
    dup_clave    = duplicated(uso_raw[, claves]),
    cpu_faltante = is.na(cpu_prom),
    mem_faltante = is.na(mem_prom),
    no_finito    = !is.finite(cpu_prom) & !is.na(cpu_prom) |
                   !is.finite(mem_prom) & !is.na(mem_prom),
    negativo     = (!is.na(cpu_prom) & cpu_prom < 0) | (!is.na(mem_prom) & mem_prom < 0),
    ventana_inval = is.na(start_time) | is.na(end_time) | end_time <= start_time,
    # Valores normalizados: la máquina mayor de la traza vale 1; un uso medio de
    # una sola instancia > 1 sería físicamente imposible.
    supera_1     = (!is.na(cpu_prom) & cpu_prom > 1) | (!is.na(mem_prom) & mem_prom > 1),
    max_menor_prom = !is.na(cpu_max) & !is.na(cpu_prom) & cpu_max < cpu_prom,
    sin_prioridad  = is.na(prioridad)
  )

resumen_calidad <- tibble(
  control = c("Duplicados exactos", "Duplicados por clave (colección, instancia, máquina, inicio)",
              "CPU media faltante", "Memoria media faltante", "Valores no finitos (Inf/NaN)",
              "Valores negativos de CPU o memoria", "Ventana inválida (fin <= inicio o faltante)",
              "CPU o memoria media > 1 (unidad normalizada)",
              "CPU máxima < CPU media (incoherencia)", "Sin prioridad (no encontrada en instance_events)"),
  n = c(sum(controles$dup_exacto), sum(controles$dup_clave), sum(controles$cpu_faltante),
        sum(controles$mem_faltante), sum(controles$no_finito), sum(controles$negativo),
        sum(controles$ventana_inval), sum(controles$supera_1), sum(controles$max_menor_prom),
        sum(controles$sin_prioridad))
) |>
  mutate(pct = round(100 * n / nrow(uso_raw), 4))
guardar_resultado(resumen_calidad, "calidad_controles")
print(resumen_calidad)

# Rangos observados de las variables de recursos (verificación de unidades)
rangos <- uso_raw |>
  summarise(across(c(cpu_prom, mem_prom, cpu_max, mem_max, mem_asignada, cpu_solicitada,
                     mem_solicitada),
                   list(min = ~min(.x, na.rm = TRUE), max = ~max(.x, na.rm = TRUE)),
                   .names = "{.col}__{.fn}")) |>
  pivot_longer(everything(), names_to = c("variable", "estadistico"), names_sep = "__") |>
  pivot_wider(names_from = estadistico, values_from = value)
guardar_resultado(rangos, "calidad_rangos_recursos")

# =============================================================================
# 4. LIMPIEZA (solo errores demostrables; los valores extremos se CONSERVAN)
# =============================================================================
n0 <- nrow(uso_raw)
uso <- uso_raw[!controles$dup_exacto, ]
registrar("uso", "Eliminar duplicados exactos",
          "Filas idénticas en todas las columnas: artefacto de exportación o unión.",
          n0, nrow(uso))

n0 <- nrow(uso)
uso <- uso |> filter(!is.na(cpu_prom), !is.na(mem_prom), is.finite(cpu_prom), is.finite(mem_prom))
registrar("uso", "Eliminar registros sin CPU o memoria media",
          "Sin la variable principal no puede usarse en el análisis de recursos.", n0, nrow(uso))

n0 <- nrow(uso)
uso <- uso |> filter(cpu_prom >= 0, mem_prom >= 0, !is.na(start_time), !is.na(end_time),
                     end_time > start_time)
registrar("uso", "Eliminar valores negativos y ventanas inválidas",
          "Uso negativo o fin <= inicio son imposibles por definición.", n0, nrow(uso))

# Los registros con valor > 1 NO se eliminan: se marcan para revisión.
registrar("uso", "Marcar (no eliminar) uso > 1 y atípicos",
          "Pueden ser comportamiento real o error; se analizan en 04 (gráfico de atípicos).",
          nrow(uso), nrow(uso))

# =============================================================================
# 5. VARIABLES DERIVADAS (documentadas en docs/diccionario_datos.csv)
# =============================================================================
uso <- uso |>
  mutate(
    # --- Tiempo: microsegundos desde el origen de la traza -> horas/días relativos.
    # No se convierte a fecha de calendario: la traza no publica una marca
    # absoluta que permita hacerlo con certeza.
    duracion_ventana_s = (end_time - start_time) / 1e6,
    ventana_completa   = duracion_ventana_s >= 300,
    hora_traza         = start_time / 3.6e9,
    hora_indice        = floor(hora_traza),
    dia_indice         = floor(hora_traza / 24),
    hora_del_ciclo     = hora_indice %% 24,   # hora relativa dentro del ciclo de 24 h
    # --- Categorías
    nivel_prioridad    = asignar_nivel(prioridad),
    tipo_coleccion     = etiquetar(collection_type, ETIQUETAS_TIPO_COLECCION),
    clase_planif       = etiquetar(clase_planificacion, ETIQUETAS_CLASE_PLANIF),
    estado_final       = etiquetar(ultimo_tipo_evento, ETIQUETAS_EVENTO),
    # --- Métricas derivadas
    ratio_pico_media_cpu = if_else(cpu_prom > 0, cpu_max / cpu_prom, NA_real_),
    uso_rel_solicitud_cpu = if_else(!is.na(cpu_solicitada) & cpu_solicitada > 0,
                                    cpu_prom / cpu_solicitada, NA_real_),
    uso_rel_solicitud_mem = if_else(!is.na(mem_solicitada) & mem_solicitada > 0,
                                    mem_prom / mem_solicitada, NA_real_),
    # --- Banderas (no eliminan registros)
    flag_supera_1     = cpu_prom > 1 | mem_prom > 1,
    flag_atipico_cpu  = marcar_atipico_iqr(log10(pmax(cpu_prom, 1e-9))),
    flag_atipico_mem  = marcar_atipico_iqr(log10(pmax(mem_prom, 1e-9)))
  )
registrar("uso", "Crear variables temporales relativas",
          "hora_traza = start_time / 3,6e9; dia_indice y hora_del_ciclo derivados. Sin fechas de calendario.",
          nrow(uso), nrow(uso))
registrar("uso", "Crear categorías",
          "nivel_prioridad (rangos del cuaderno oficial + monitoring >= 360); etiquetas de enums del proto v3.",
          nrow(uso), nrow(uso))
registrar("uso", "Crear métricas derivadas y banderas de atípicos",
          "Ratio pico/media, uso relativo a lo solicitado; atípicos por Tukey sobre log10.",
          nrow(uso), nrow(uso))

# Selección final de variables de análisis (nivel registro)
uso_procesado <- uso |>
  select(collection_id, instance_index, machine_id, start_time, end_time,
         duracion_ventana_s, ventana_completa, hora_traza, hora_indice, dia_indice,
         hora_del_ciclo, cpu_prom, mem_prom, cpu_max, mem_max, mem_asignada,
         mem_cache_paginas, ciclos_por_instruccion, accesos_mem_por_instruccion,
         tasa_muestreo, prioridad, nivel_prioridad, clase_planif, tipo_coleccion,
         estado_final, cpu_solicitada, mem_solicitada, n_eventos, ratio_pico_media_cpu,
         uso_rel_solicitud_cpu, uso_rel_solicitud_mem, t_primer_schedule,
         t_ultimo_terminal, flag_supera_1, flag_atipico_cpu, flag_atipico_mem)

# =============================================================================
# 6. TABLA A NIVEL INSTANCIA (evita que tareas largas dominen las comparaciones)
# =============================================================================
instancias_resumen <- uso_procesado |>
  group_by(collection_id, instance_index) |>
  summarise(
    n_ventanas        = n(),
    nivel_prioridad   = first(nivel_prioridad),
    prioridad         = first(prioridad),
    clase_planif      = first(clase_planif),
    estado_final      = first(estado_final),
    cpu_prom_media    = mean(cpu_prom),
    mem_prom_media    = mean(mem_prom),
    cpu_max_obs       = max(cpu_max, na.rm = TRUE),
    mem_max_obs       = max(mem_max, na.rm = TRUE),
    cpu_solicitada    = first(cpu_solicitada),
    mem_solicitada    = first(mem_solicitada),
    hora_inicio_traza = min(hora_traza),
    t_primer_schedule = first(t_primer_schedule),
    t_ultimo_terminal = first(t_ultimo_terminal),
    .groups = "drop"
  ) |>
  mutate(
    cpu_max_obs = if_else(is.finite(cpu_max_obs), cpu_max_obs, NA_real_),
    mem_max_obs = if_else(is.finite(mem_max_obs), mem_max_obs, NA_real_),
    # Duración de vida: del primer SCHEDULE al último evento terminal (horas).
    duracion_vida_h = if_else(!is.na(t_primer_schedule) & !is.na(t_ultimo_terminal) &
                                t_ultimo_terminal >= t_primer_schedule,
                              (t_ultimo_terminal - t_primer_schedule) / 3.6e9, NA_real_)
  )
registrar("instancias_resumen", "Agregar a nivel instancia",
          "Medias por (collection_id, instance_index) para comparar grupos sin ponderar por duración.",
          nrow(uso_procesado), nrow(instancias_resumen))

# =============================================================================
# 7. AGREGADO HORARIO POBLACIONAL: utilización relativa a la capacidad
# =============================================================================
capacidad_cpu <- capacidad_raw$capacidad_cpu_ncu[1]
capacidad_mem <- capacidad_raw$capacidad_memoria_nmu[1]

agregado_horario <- agregado_raw |>
  mutate(
    hora_indice    = as.numeric(hora_indice),
    nivel_prioridad = factor(nivel, levels = c(NIVELES_PRIORIDAD, "sin_prioridad")),
    dia_indice     = floor(hora_indice / 24),
    hora_del_ciclo = hora_indice %% 24,
    # Utilización media de la celda en la hora (como en el cuaderno oficial):
    # suma de usos de ventanas de 5 min / (12 ventanas x capacidad total).
    util_cpu = suma_cpu_ncu / (12 * capacidad_cpu),
    util_mem = suma_mem_nmu / (12 * capacidad_mem)
  ) |>
  select(-nivel)

n_horas <- dplyr::n_distinct(agregado_horario$hora_indice)
registrar("agregado_horario", "Calcular utilización relativa a la capacidad",
          "util = suma / (12 x capacidad de la celda); serie poblacional por hora relativa.",
          nrow(agregado_raw), nrow(agregado_horario))

# Horas incompletas en los extremos de la traza (menos registros) se marcan.
agregado_horario <- agregado_horario |>
  group_by(hora_indice) |>
  mutate(n_registros_hora = sum(n_registros)) |>
  ungroup() |>
  mutate(flag_hora_extremo = hora_indice == min(hora_indice) | hora_indice == max(hora_indice))

# =============================================================================
# 8. EVENTOS ETIQUETADOS
# =============================================================================
eventos <- eventos_raw |>
  mutate(
    tipo_evento_cod = as.integer(tipo_evento),
    tipo_evento     = etiquetar(tipo_evento_cod, ETIQUETAS_EVENTO),
    nivel_prioridad = factor(nivel, levels = c(NIVELES_PRIORIDAD, "sin_prioridad")),
    tipo_coleccion  = etiquetar(collection_type, ETIQUETAS_TIPO_COLECCION),
    n_eventos       = as.numeric(n_eventos)
  ) |>
  select(tipo_evento_cod, tipo_evento, nivel_prioridad, tipo_coleccion, n_eventos)

# =============================================================================
# 9. EXPORTACIÓN
# =============================================================================
exportar_tabla(uso_procesado, "uso_procesado")
exportar_tabla(instancias_resumen, "instancias_resumen")
exportar_tabla(agregado_horario, "agregado_horario")
exportar_tabla(eventos, "eventos")
readr::write_csv(capacidad_raw, file.path(RUTAS$processed, "capacidad_celda.csv"))
guardar_resultado(.registro$pasos, "registro_transformaciones")

message(sprintf("Procesado: %s registros de uso, %s instancias, %s horas de serie poblacional.",
                etiqueta_n(nrow(uso_procesado)), etiqueta_n(nrow(instancias_resumen)),
                etiqueta_n(n_horas)))
