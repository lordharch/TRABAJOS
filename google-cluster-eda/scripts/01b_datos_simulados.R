# =============================================================================
# 01b_datos_simulados.R — Genera datos SINTÉTICOS con el mismo esquema que las
# exportaciones de BigQuery (data/raw/), para probar el flujo completo sin
# acceso a Google Cloud.
#
# ADVERTENCIA: estos datos NO provienen de Google Cluster Data. Sirven solo
# para verificar que los scripts se ejecutan. Ninguna cifra, gráfico ni
# conclusión obtenida con ellos debe presentarse como resultado del análisis.
# Para el trabajo final usa CONFIG$fuente_datos = "bigquery" o "local".
# =============================================================================

source("config.R")
source("scripts/utilidades.R")
verificar_raiz_proyecto()
crear_carpetas()

set.seed(CONFIG$semilla)

us_ventana     <- 300e6            # 5 minutos en microsegundos
n_ventanas_mes <- 31 * 24 * 12     # ventanas de 5 min en 31 días
n_colecciones  <- 600

# -----------------------------------------------------------------------------
# Colecciones (jobs)
# -----------------------------------------------------------------------------
nivel_col <- sample(NIVELES_PRIORIDAD, n_colecciones, replace = TRUE,
                    prob = c(0.35, 0.30, 0.05, 0.25, 0.05))
prioridad_col <- vapply(nivel_col, function(nv) {
  switch(nv,
         free       = sample(c(0L, 25L), 1),
         beb        = sample(c(103L, 105L, 107L, 110L, 115L), 1),
         mid        = sample(116:119, 1),
         prod       = sample(c(120L, 200L), 1),
         monitoring = 360L)
}, integer(1))

colecciones <- tibble::tibble(
  collection_id   = format(4e11 + sample.int(1e8, n_colecciones), scientific = FALSE, trim = TRUE),
  nivel           = nivel_col,
  prioridad       = prioridad_col,
  collection_type = 0L,
  n_instancias    = 1L + stats::rgeom(n_colecciones, 0.15),
  clase_planificacion = dplyr::case_when(
    nivel_col %in% c("prod", "monitoring") ~ sample(2:3, n_colecciones, replace = TRUE),
    nivel_col == "mid"                     ~ sample(1:2, n_colecciones, replace = TRUE),
    TRUE                                   ~ sample(0:1, n_colecciones, replace = TRUE)
  ),
  cpu_solicitada = stats::rlnorm(n_colecciones, log(0.02), 0.9),
  mem_solicitada = stats::rlnorm(n_colecciones, log(0.015), 0.9)
)

# -----------------------------------------------------------------------------
# Instancias (tasks)
# -----------------------------------------------------------------------------
instancias <- colecciones |>
  tidyr::uncount(n_instancias, .id = "instance_index") |>
  dplyr::mutate(
    instance_index = instance_index - 1L,
    machine_id     = format(1e9 + sample.int(12000, dplyr::n(), replace = TRUE),
                            scientific = FALSE, trim = TRUE),
    ventana_inicio = sample.int(n_ventanas_mes, dplyr::n(), replace = TRUE) - 1L,
    n_ventanas     = 1L + ifelse(nivel %in% c("prod", "monitoring"),
                                 stats::rgeom(dplyr::n(), 0.005),
                                 stats::rgeom(dplyr::n(), 0.03)),
    n_ventanas     = pmin(n_ventanas, n_ventanas_mes - ventana_inicio),
    # Uso base: fracción de lo solicitado (las tareas suelen usar menos de lo pedido)
    cpu_base       = cpu_solicitada * stats::rbeta(dplyr::n(), 2, 4),
    mem_base       = mem_solicitada * stats::rbeta(dplyr::n(), 4, 3),
    sigue_activa   = (ventana_inicio + n_ventanas) >= n_ventanas_mes,
    ultimo_tipo_evento = dplyr::case_when(
      sigue_activa ~ 3L,
      TRUE ~ sample(c(6L, 7L, 5L, 4L, 8L), dplyr::n(), replace = TRUE,
                    prob = c(0.55, 0.30, 0.07, 0.07, 0.01))
    ),
    n_eventos = 3L + stats::rpois(dplyr::n(), 1)
  )

# -----------------------------------------------------------------------------
# Registros de uso (ventanas de 5 minutos)
# -----------------------------------------------------------------------------
uso <- instancias |>
  tidyr::uncount(n_ventanas, .id = "k") |>
  dplyr::mutate(
    ventana    = ventana_inicio + k - 1L,
    start_time = 600e6 + ventana * us_ventana,
    hora_rel   = (start_time / 3.6e9) %% 24,
    ciclo      = 1 + 0.35 * sin(2 * pi * (hora_rel - 6) / 24),
    cpu_prom   = cpu_base * ciclo * stats::rlnorm(dplyr::n(), 0, 0.35),
    mem_prom   = mem_base * stats::rlnorm(dplyr::n(), 0, 0.10),
    cpu_max    = cpu_prom * (1 + stats::rexp(dplyr::n(), 1.5)),
    mem_max    = mem_prom * (1 + stats::rexp(dplyr::n(), 8))
  ) |>
  dplyr::group_by(collection_id, instance_index) |>
  dplyr::mutate(
    ultima = dplyr::row_number() == dplyr::n(),
    end_time = start_time + ifelse(ultima & !sigue_activa,
                                   stats::runif(dplyr::n(), 10, 300) * 1e6, us_ventana),
    t_primer_schedule = min(start_time),
    t_ultimo_terminal = ifelse(sigue_activa, NA_real_, max(end_time))
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    mem_asignada                = mem_solicitada * 1.1,
    mem_cache_paginas           = mem_prom * stats::runif(dplyr::n(), 0, 0.2),
    ciclos_por_instruccion      = ifelse(stats::runif(dplyr::n()) < 0.1, NA_real_,
                                         stats::rlnorm(dplyr::n(), log(1.2), 0.3)),
    accesos_mem_por_instruccion = ifelse(is.na(ciclos_por_instruccion), NA_real_,
                                         stats::rlnorm(dplyr::n(), log(0.005), 0.5)),
    tasa_muestreo               = 1
  )

muestra <- uso |>
  dplyr::select(collection_id, instance_index, machine_id, start_time, end_time,
                collection_type, cpu_prom, mem_prom, cpu_max, mem_max, mem_asignada,
                mem_cache_paginas, ciclos_por_instruccion, accesos_mem_por_instruccion,
                tasa_muestreo, prioridad, clase_planificacion, cpu_solicitada,
                mem_solicitada, n_eventos, t_primer_schedule, t_ultimo_terminal,
                ultimo_tipo_evento)

# Problemas de calidad deliberados para ejercitar los controles de 02:
idx_dup <- sample.int(nrow(muestra), 200)
muestra <- dplyr::bind_rows(muestra, muestra[idx_dup, ])
muestra$cpu_prom[sample.int(nrow(muestra), 20)] <- NA_real_

arrow::write_parquet(muestra, file.path(RUTAS$raw, "muestra_uso_instancias.parquet"))

# -----------------------------------------------------------------------------
# Agregado horario y capacidad (derivados de los datos simulados)
# -----------------------------------------------------------------------------
agregado <- uso |>
  dplyr::filter((end_time - start_time) >= us_ventana) |>
  dplyr::mutate(hora_indice = floor(start_time / 3.6e9)) |>
  dplyr::group_by(hora_indice, nivel) |>
  dplyr::summarise(
    n_registros        = dplyr::n(),
    n_instancias_aprox = dplyr::n_distinct(collection_id, instance_index),
    suma_cpu_ncu       = sum(cpu_prom),
    suma_mem_nmu       = sum(mem_prom),
    media_cpu_ncu      = mean(cpu_prom),
    media_mem_nmu      = mean(mem_prom),
    max_cpu_ncu        = max(cpu_prom),
    max_mem_nmu        = max(mem_prom),
    .groups = "drop"
  )
arrow::write_parquet(agregado, file.path(RUTAS$raw, "agregado_horario_nivel.parquet"))

totales_hora <- agregado |>
  dplyr::group_by(hora_indice) |>
  dplyr::summarise(cpu = sum(suma_cpu_ncu), mem = sum(suma_mem_nmu))
capacidad <- tibble::tibble(
  n_maquinas            = 12000,
  capacidad_cpu_ncu     = max(totales_hora$cpu) / 12 / 0.85,
  capacidad_memoria_nmu = max(totales_hora$mem) / 12 / 0.80
)
readr::write_csv(capacidad, file.path(RUTAS$raw, "capacidad_celda.csv"))

# -----------------------------------------------------------------------------
# Conteo de eventos simulado
# -----------------------------------------------------------------------------
eventos <- instancias |>
  dplyr::transmute(nivel, collection_type, ultimo_tipo_evento,
                   n_queue = stats::rpois(dplyr::n(), 0.5)) |>
  dplyr::group_by(nivel, collection_type) |>
  dplyr::summarise(
    `0` = dplyr::n(), `1` = sum(n_queue), `2` = sum(n_queue), `3` = dplyr::n(),
    `4` = sum(ultimo_tipo_evento == 4L), `5` = sum(ultimo_tipo_evento == 5L),
    `6` = sum(ultimo_tipo_evento == 6L), `7` = sum(ultimo_tipo_evento == 7L),
    `8` = sum(ultimo_tipo_evento == 8L),
    `10` = round(dplyr::n() * 0.2),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(-c(nivel, collection_type), names_to = "tipo_evento",
                      values_to = "n_eventos") |>
  dplyr::mutate(tipo_evento = as.integer(tipo_evento)) |>
  dplyr::filter(n_eventos > 0) |>
  dplyr::select(tipo_evento, nivel, collection_type, n_eventos)
readr::write_csv(eventos, file.path(RUTAS$raw, "conteo_eventos.csv"))

readr::write_csv(tibble::tibble(
  fuente = "simulado", proyecto_datos = "NINGUNO (datos sintéticos)",
  dataset = "NINGUNO", celda = CONFIG$celda, modulo_muestra = NA,
  filas_muestra = nrow(muestra),
  fecha_extraccion = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  version_bigrquery = NA
), file.path(RUTAS$raw, "metadatos_extraccion.csv"))

message("Datos SIMULADOS generados en ", RUTAS$raw, " (", etiqueta_n(nrow(muestra)), " filas de uso).")
