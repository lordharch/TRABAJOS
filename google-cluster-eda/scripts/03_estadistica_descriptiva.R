# =============================================================================
# 03_estadistica_descriptiva.R — Tablas de estadística descriptiva (results/)
#
# Población / muestra:
#   * Tablas "muestra_*": calculadas sobre la MUESTRA de colecciones (1 de cada
#     CONFIG$modulo_muestra). Son estimaciones; la desviación estándar y la
#     varianza son muestrales (denominador n - 1).
#   * Tablas "poblacion_*": calculadas sobre agregados que BigQuery obtuvo con
#     TODOS los registros de la celda (frecuencias de eventos, serie horaria).
# =============================================================================

source("config.R")
source("scripts/utilidades.R")
verificar_raiz_proyecto()

uso        <- arrow::read_parquet(file.path(RUTAS$processed, "uso_procesado.parquet"))
instancias <- arrow::read_parquet(file.path(RUTAS$processed, "instancias_resumen.parquet"))
agregado   <- arrow::read_parquet(file.path(RUTAS$processed, "agregado_horario.parquet"))
eventos    <- arrow::read_parquet(file.path(RUTAS$processed, "eventos.parquet"))

vars_num <- c("cpu_prom", "mem_prom", "cpu_max", "mem_max", "mem_asignada",
              "ciclos_por_instruccion", "accesos_mem_por_instruccion",
              "ratio_pico_media_cpu", "uso_rel_solicitud_cpu", "uso_rel_solicitud_mem",
              "duracion_ventana_s")

# -----------------------------------------------------------------------------
# 1. Resumen numérico global (nivel registro)
# -----------------------------------------------------------------------------
desc_global <- purrr::map_dfr(vars_num, function(v) {
  dplyr::bind_cols(tibble(variable = v), resumen_numerico(uso[[v]]))
})
guardar_resultado(desc_global, "muestra_descriptivos_registro")

# -----------------------------------------------------------------------------
# 2. Resumen numérico por nivel de prioridad (CPU y memoria, nivel registro)
# -----------------------------------------------------------------------------
desc_nivel <- uso |>
  filter(!is.na(nivel_prioridad)) |>
  select(nivel_prioridad, cpu_prom, mem_prom) |>
  pivot_longer(c(cpu_prom, mem_prom), names_to = "variable", values_to = "valor") |>
  group_by(nivel_prioridad, variable) |>
  group_modify(~ resumen_numerico(.x$valor)) |>
  ungroup()
guardar_resultado(desc_nivel, "muestra_descriptivos_por_nivel")

# Nivel instancia: medias por tarea (cada instancia pesa lo mismo)
desc_inst_nivel <- instancias |>
  filter(!is.na(nivel_prioridad)) |>
  select(nivel_prioridad, cpu_prom_media, mem_prom_media, duracion_vida_h, n_ventanas) |>
  pivot_longer(-nivel_prioridad, names_to = "variable", values_to = "valor") |>
  group_by(nivel_prioridad, variable) |>
  group_modify(~ resumen_numerico(.x$valor)) |>
  ungroup()
guardar_resultado(desc_inst_nivel, "muestra_descriptivos_instancia_por_nivel")

# -----------------------------------------------------------------------------
# 3. Frecuencias absolutas y relativas
# -----------------------------------------------------------------------------
frecuencias <- function(df, var, unidad) {
  df |>
    count(categoria = .data[[var]], name = "frecuencia_abs") |>
    mutate(variable = var, unidad = unidad,
           frecuencia_rel = frecuencia_abs / sum(frecuencia_abs),
           porcentaje = round(100 * frecuencia_rel, 2),
           frecuencia_acum = cumsum(frecuencia_rel)) |>
    mutate(categoria = as.character(categoria)) |>
    select(variable, unidad, categoria, frecuencia_abs, frecuencia_rel, porcentaje, frecuencia_acum)
}

freq_muestra <- bind_rows(
  frecuencias(uso, "nivel_prioridad", "registros"),
  frecuencias(instancias, "nivel_prioridad", "instancias"),
  frecuencias(instancias, "estado_final", "instancias"),
  frecuencias(instancias, "clase_planif", "instancias"),
  frecuencias(uso, "ventana_completa", "registros")
)
guardar_resultado(freq_muestra, "muestra_frecuencias")

# Poblacional: eventos por tipo y por nivel
freq_eventos_tipo <- eventos |>
  group_by(tipo_evento) |>
  summarise(frecuencia_abs = sum(n_eventos), .groups = "drop") |>
  mutate(frecuencia_rel = frecuencia_abs / sum(frecuencia_abs),
         porcentaje = round(100 * frecuencia_rel, 2)) |>
  arrange(desc(frecuencia_abs))
guardar_resultado(freq_eventos_tipo, "poblacion_frecuencia_eventos_tipo")

freq_eventos_nivel <- eventos |>
  group_by(nivel_prioridad) |>
  summarise(frecuencia_abs = sum(n_eventos), .groups = "drop") |>
  mutate(frecuencia_rel = frecuencia_abs / sum(frecuencia_abs),
         porcentaje = round(100 * frecuencia_rel, 2))
guardar_resultado(freq_eventos_nivel, "poblacion_frecuencia_eventos_nivel")

# Tabla de contingencia estado final x nivel (muestra, instancias)
contingencia <- instancias |>
  filter(!is.na(nivel_prioridad)) |>
  count(nivel_prioridad, estado_final) |>
  group_by(nivel_prioridad) |>
  mutate(pct_en_nivel = round(100 * n / sum(n), 2)) |>
  ungroup()
guardar_resultado(contingencia, "muestra_contingencia_estado_nivel")

# -----------------------------------------------------------------------------
# 4. Valores faltantes por variable (datos procesados)
# -----------------------------------------------------------------------------
faltantes <- tibble(
  variable    = names(uso),
  n_faltantes = vapply(uso, function(x) sum(is.na(x)), numeric(1))
) |>
  mutate(pct_faltantes = round(100 * n_faltantes / nrow(uso), 3)) |>
  arrange(desc(n_faltantes))
guardar_resultado(faltantes, "muestra_faltantes_por_variable")

# -----------------------------------------------------------------------------
# 5. Correlaciones (Pearson y Spearman, casos completos por pares)
# Spearman es más adecuada por la fuerte asimetría de CPU y memoria.
# -----------------------------------------------------------------------------
vars_cor <- c("cpu_prom", "mem_prom", "cpu_max", "mem_max", "mem_asignada",
              "cpu_solicitada", "mem_solicitada", "ciclos_por_instruccion", "prioridad")
# Se excluyen variables casi vacías o constantes (la correlación no está definida).
vars_cor <- vars_cor[vapply(vars_cor, function(v) {
  sum(!is.na(uso[[v]])) > 2 && isTRUE(stats::sd(uso[[v]], na.rm = TRUE) > 0)
}, logical(1))]
m <- as.matrix(uso[, vars_cor])

matriz_a_tabla <- function(mc, metodo) {
  as.data.frame(as.table(mc)) |>
    as_tibble() |>
    rename(var_x = Var1, var_y = Var2, correlacion = Freq) |>
    mutate(metodo = metodo)
}
cor_pearson  <- stats::cor(m, use = "pairwise.complete.obs", method = "pearson")
cor_spearman <- stats::cor(m, use = "pairwise.complete.obs", method = "spearman")
correlaciones <- bind_rows(matriz_a_tabla(cor_pearson, "pearson"),
                           matriz_a_tabla(cor_spearman, "spearman"))
guardar_resultado(correlaciones, "muestra_correlaciones")

# Prueba de correlación CPU-memoria (Spearman). Con n grande casi cualquier
# correlación es "significativa": interpreta el tamaño del coeficiente.
sub <- uso |> select(cpu_prom, mem_prom) |> filter(!is.na(cpu_prom), !is.na(mem_prom))
set.seed(CONFIG$semilla)
if (nrow(sub) > 100000) sub <- dplyr::slice_sample(sub, n = 100000)
prueba <- suppressWarnings(stats::cor.test(sub$cpu_prom, sub$mem_prom, method = "spearman"))
guardar_resultado(tibble(
  prueba = "Spearman CPU media vs memoria media",
  n_usado = nrow(sub), rho = unname(prueba$estimate), p_valor = prueba$p.value
), "muestra_prueba_correlacion_cpu_mem")

# -----------------------------------------------------------------------------
# 6. Serie horaria poblacional: utilización total de la celda
# -----------------------------------------------------------------------------
serie_total <- agregado |>
  group_by(hora_indice) |>
  summarise(util_cpu = sum(util_cpu), util_mem = sum(util_mem),
            n_registros = sum(n_registros), .groups = "drop")
desc_serie <- bind_rows(
  bind_cols(tibble(variable = "util_cpu_celda_horaria"), resumen_numerico(serie_total$util_cpu)),
  bind_cols(tibble(variable = "util_mem_celda_horaria"), resumen_numerico(serie_total$util_mem))
)
guardar_resultado(desc_serie, "poblacion_descriptivos_serie_horaria")

picos <- serie_total |>
  arrange(desc(util_cpu)) |>
  slice_head(n = 10) |>
  mutate(dia_indice = floor(hora_indice / 24), hora_del_ciclo = hora_indice %% 24)
guardar_resultado(picos, "poblacion_top10_horas_pico_cpu")

# -----------------------------------------------------------------------------
# 7. Atípicos: resumen y registros más extremos
# -----------------------------------------------------------------------------
resumen_atipicos <- uso |>
  group_by(nivel_prioridad) |>
  summarise(n = n(),
            atipicos_cpu = sum(flag_atipico_cpu), pct_atipicos_cpu = round(100 * mean(flag_atipico_cpu), 2),
            atipicos_mem = sum(flag_atipico_mem), pct_atipicos_mem = round(100 * mean(flag_atipico_mem), 2),
            supera_1 = sum(flag_supera_1), .groups = "drop")
guardar_resultado(resumen_atipicos, "muestra_resumen_atipicos")

top_atipicos <- uso |>
  arrange(desc(cpu_prom)) |>
  slice_head(n = 25) |>
  select(collection_id, instance_index, hora_traza, nivel_prioridad, cpu_prom, cpu_max,
         mem_prom, cpu_solicitada, estado_final)
guardar_resultado(top_atipicos, "muestra_top25_cpu")

message("Estadística descriptiva exportada a ", RUTAS$results)
