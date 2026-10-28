# =============================================================================
# 05_preparacion_modelado.R — Preparación (opcional) para modelado predictivo
#
# Problema propuesto: estimar el uso medio de CPU de una instancia en la
# siguiente ventana de 5 minutos (cpu_prom en t) a partir de:
#   (a) atributos conocidos al enviar la tarea (prioridad, clase de
#       planificación, recursos solicitados),
#   (b) variables temporales relativas (hora del ciclo, edad de la instancia),
#   (c) historia reciente de la propia instancia (CPU en t-1, media t-1..t-3).
#
# Prevención de fugas de información:
#   * Partición CRONOLÓGICA: entrenamiento = primeras CONFIG$proporcion_entrenamiento
#     del tiempo de traza; prueba = resto. Nunca se usa el futuro para predecir.
#   * Se EXCLUYEN como predictores las variables calculadas con el objetivo o
#     simultáneas a él: cpu_max, ratio_pico_media_cpu, uso_rel_solicitud_cpu,
#     mem_prom/mem_max de la misma ventana, ciclos_por_instruccion; y las que
#     usan información futura de la instancia: estado_final, n_eventos,
#     t_ultimo_terminal, duracion_vida_h.
#
# Las métricas solo se escriben si el modelo se entrena aquí con datos reales
# (CONFIG$entrenar_modelo_base = TRUE). Con datos simulados no tienen valor
# interpretativo.
# =============================================================================

source("config.R")
source("scripts/utilidades.R")
verificar_raiz_proyecto()

uso <- arrow::read_parquet(file.path(RUTAS$processed, "uso_procesado.parquet"))

# -----------------------------------------------------------------------------
# 1. Construcción de la tabla de modelado
# -----------------------------------------------------------------------------
datos_modelo <- uso |>
  filter(ventana_completa) |>
  arrange(collection_id, instance_index, start_time) |>
  group_by(collection_id, instance_index) |>
  mutate(
    separacion_prev_s = (start_time - lag(start_time)) / 1e6,
    # Retardo 1 solo si la ventana previa es contigua (<= 10 min de separación)
    cpu_lag1 = if_else(!is.na(separacion_prev_s) & separacion_prev_s <= 600,
                       lag(cpu_prom, 1), NA_real_),
    cpu_media_lag_1_3 = (lag(cpu_prom, 1) + lag(cpu_prom, 2) + lag(cpu_prom, 3)) / 3,
    edad_instancia_h = (start_time - min(start_time)) / 3.6e9
  ) |>
  ungroup() |>
  transmute(
    collection_id, instance_index, hora_traza,
    objetivo_cpu = cpu_prom,
    nivel_prioridad, prioridad, clase_planif,
    cpu_solicitada, mem_solicitada,
    hora_del_ciclo = factor(hora_del_ciclo, levels = 0:23),
    edad_instancia_h, cpu_lag1, cpu_media_lag_1_3
  )

predictores <- c("nivel_prioridad", "prioridad", "clase_planif", "cpu_solicitada",
                 "mem_solicitada", "hora_del_ciclo", "edad_instancia_h", "cpu_lag1",
                 "cpu_media_lag_1_3")

variables_modelo <- tibble(
  variable = c("objetivo_cpu", predictores,
               "cpu_max", "ratio_pico_media_cpu", "uso_rel_solicitud_cpu", "mem_prom",
               "ciclos_por_instruccion", "estado_final", "n_eventos", "duracion_vida_h"),
  rol = c("objetivo", rep("predictor", length(predictores)), rep("excluida", 8)),
  justificacion = c(
    "CPU media de la ventana t (NCU).",
    "Nivel derivado de la prioridad, conocida al enviar la tarea.",
    "Prioridad numérica, conocida al enviar la tarea.",
    "Clase de latencia, conocida al enviar la tarea.",
    "CPU solicitada al planificador (conocida antes de ejecutar).",
    "Memoria solicitada (conocida antes de ejecutar).",
    "Hora relativa del ciclo de 24 h (calendario no disponible).",
    "Tiempo transcurrido desde la primera ventana observada (pasado).",
    "CPU de la ventana anterior contigua (pasado).",
    "Media de CPU de las 3 ventanas anteriores (pasado).",
    "Se mide en la misma ventana que el objetivo (simultánea).",
    "Calculada con el objetivo (cpu_max / cpu_prom): fuga directa.",
    "Calculada con el objetivo (cpu_prom / cpu_solicitada): fuga directa.",
    "Simultánea al objetivo; solo usable con retardo.",
    "Simultánea al objetivo; solo usable con retardo.",
    "Se conoce al terminar la instancia (información futura).",
    "Cuenta eventos de toda la vida de la instancia (información futura).",
    "Depende del final de la instancia (información futura).")
)
guardar_resultado(variables_modelo, "modelado_variables")

# -----------------------------------------------------------------------------
# 2. Partición cronológica
# -----------------------------------------------------------------------------
t_min <- min(datos_modelo$hora_traza)
t_max <- max(datos_modelo$hora_traza)
corte_h <- t_min + CONFIG$proporcion_entrenamiento * (t_max - t_min)

datos_modelo <- datos_modelo |>
  mutate(particion = if_else(hora_traza < corte_h, "entrenamiento", "prueba"))
exportar_tabla(datos_modelo, "datos_modelado", csv = FALSE)

datos_modelo$caso_completo <- stats::complete.cases(datos_modelo[, c("objetivo_cpu", predictores)])
particion <- datos_modelo |>
  group_by(particion) |>
  summarise(n_registros = n(), n_completos = sum(caso_completo),
            n_instancias = n_distinct(collection_id, instance_index), .groups = "drop") |>
  mutate(corte_hora_traza = corte_h)
guardar_resultado(particion, "modelado_particion")
print(particion)

# -----------------------------------------------------------------------------
# 3. Entrenamiento y evaluación de modelos base (opcional)
# -----------------------------------------------------------------------------
if (!isTRUE(CONFIG$entrenar_modelo_base)) {
  message("CONFIG$entrenar_modelo_base = FALSE: solo se preparó la tabla de modelado.")
} else {
  completos <- datos_modelo |> filter(caso_completo)
  entrena <- completos |> filter(particion == "entrenamiento") |> droplevels()
  prueba  <- completos |> filter(particion == "prueba")

  # Niveles de factores no vistos en entrenamiento no pueden predecirse
  for (f in c("nivel_prioridad", "clase_planif", "hora_del_ciclo")) {
    prueba <- prueba |> filter(.data[[f]] %in% levels(entrena[[f]]))
    prueba[[f]] <- factor(as.character(prueba[[f]]), levels = levels(entrena[[f]]))
  }

  if (nrow(entrena) < 100 || nrow(prueba) < 100) {
    warning("Datos insuficientes para entrenar/evaluar (entrenamiento = ", nrow(entrena),
            ", prueba = ", nrow(prueba), ").")
  } else {
    metricas <- function(modelo, y, yhat) {
      tibble(modelo = modelo, n_prueba = length(y),
             MAE  = mean(abs(y - yhat)),
             RMSE = sqrt(mean((y - yhat)^2)),
             MedAE = stats::median(abs(y - yhat)))
    }

    # Base 1: media global de entrenamiento
    p_media <- rep(mean(entrena$objetivo_cpu), nrow(prueba))

    # Base 2: media por nivel de prioridad (entrenamiento)
    medias_nivel <- entrena |> group_by(nivel_prioridad) |>
      summarise(m = mean(objetivo_cpu), .groups = "drop")
    p_nivel <- medias_nivel$m[match(prueba$nivel_prioridad, medias_nivel$nivel_prioridad)]

    # Base 3: persistencia (la CPU de t es la de t-1)
    p_persistencia <- prueba$cpu_lag1

    # Modelo lineal (regresión múltiple) con los predictores permitidos.
    # Se ajusta sobre log10 porque la CPU es muy asimétrica; luego se invierte.
    eps <- 1e-6
    formula_lm <- stats::as.formula(paste(
      "log10(objetivo_cpu + eps) ~ nivel_prioridad + clase_planif + log10(cpu_solicitada + eps) +",
      "log10(mem_solicitada + eps) + hora_del_ciclo + edad_instancia_h +",
      "log10(cpu_lag1 + eps) + log10(cpu_media_lag_1_3 + eps)"))
    modelo_lm <- stats::lm(formula_lm, data = entrena)
    p_lm <- pmax(10^stats::predict(modelo_lm, newdata = prueba) - eps, 0)

    tabla_metricas <- bind_rows(
      metricas("Base: media global", prueba$objetivo_cpu, p_media),
      metricas("Base: media por nivel", prueba$objetivo_cpu, p_nivel),
      metricas("Base: persistencia (t-1)", prueba$objetivo_cpu, p_persistencia),
      metricas("Regresión lineal log10", prueba$objetivo_cpu, p_lm)
    ) |>
      mutate(
        mejora_MAE_vs_persistencia_pct = round(100 * (1 - MAE / MAE[modelo == "Base: persistencia (t-1)"]), 2),
        datos = CONFIG$fuente_datos,
        nota = "Métricas calculadas en esta ejecución sobre la partición de prueba cronológica."
      )
    guardar_resultado(tabla_metricas, "modelado_metricas")
    print(tabla_metricas)

    coefs <- tibble::as_tibble(summary(modelo_lm)$coefficients, rownames = "termino")
    guardar_resultado(coefs, "modelado_coeficientes_lm")
  }
}
message("Preparación de modelado finalizada.")
