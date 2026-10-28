# =============================================================================
# 04_graficos.R — Análisis exploratorio visual con ggplot2
#
# Cada gráfico se documenta con: pregunta analítica, variables, datos usados
# (muestra o población) e instrucciones de interpretación. Se guardan en plots/
# como PNG (300 ppp) y PDF, y se listan en results/indice_graficos.csv, que el
# informe Quarto utiliza para mostrarlos en orden.
#
# Nota sobre escalas: el uso de CPU y memoria por ventana está fuertemente
# sesgado a la derecha (muchas tareas pequeñas, pocas muy grandes). Por eso la
# mayoría de los gráficos de distribución usan escala log10; los valores
# exactamente 0 no pueden representarse en log y se informan en el subtítulo.
# =============================================================================

source("config.R")
source("scripts/utilidades.R")
verificar_raiz_proyecto()
crear_carpetas()
theme_set(tema_proyecto())

uso        <- arrow::read_parquet(file.path(RUTAS$processed, "uso_procesado.parquet"))
instancias <- arrow::read_parquet(file.path(RUTAS$processed, "instancias_resumen.parquet"))
agregado   <- arrow::read_parquet(file.path(RUTAS$processed, "agregado_horario.parquet"))
eventos    <- arrow::read_parquet(file.path(RUTAS$processed, "eventos.parquet"))

set.seed(CONFIG$semilla)

# Índice de gráficos (se va completando)
indice <- tibble(id = character(), archivo = character(), titulo = character(),
                 pregunta = character(), variables = character(), datos = character(),
                 interpretacion = character())

# Extrae el título de un ggplot de forma compatible con ggplot2 3.x y 4.x
titulo_grafico <- function(p, defecto) {
  t <- tryCatch({
    if (exists("get_labs", where = asNamespace("ggplot2"), inherits = FALSE)) {
      ggplot2::get_labs(p)$title
    } else {
      p$labels$title
    }
  }, error = function(e) NULL)
  if (is.null(t) || !nzchar(t)) defecto else as.character(t)
}

graficar <- function(id, nombre, pregunta, variables, datos, interpretacion, construir,
                     ancho = CONFIG$ancho_cm, alto = CONFIG$alto_cm) {
  resultado <- tryCatch({
    p <- construir()
    if (is.null(p)) {
      message("Omitido ", id, ": datos insuficientes.")
      NULL
    } else {
      guardar_grafico(p, nombre, ancho, alto)
      p
    }
  }, error = function(e) {
    warning("No se pudo generar ", id, " (", nombre, "): ", conditionMessage(e), call. = FALSE)
    NULL
  })
  if (!is.null(resultado)) {
    indice <<- bind_rows(indice, tibble(
      id = id, archivo = paste0(nombre, ".png"),
      titulo = titulo_grafico(resultado, nombre),
      pregunta = pregunta, variables = variables, datos = datos,
      interpretacion = interpretacion))
  }
  invisible(resultado)
}

# Subconjuntos comunes
uso_pos_cpu <- uso |> filter(cpu_prom > 0)
uso_pos_mem <- uso |> filter(mem_prom > 0)
n_cero_cpu  <- sum(uso$cpu_prom == 0)
n_cero_mem  <- sum(uso$mem_prom == 0)
uso_niv     <- uso |> filter(!is.na(nivel_prioridad))

sub_disp <- uso |> filter(cpu_prom > 0, mem_prom > 0)
if (nrow(sub_disp) > CONFIG$n_puntos_dispersion) {
  sub_disp <- slice_sample(sub_disp, n = CONFIG$n_puntos_dispersion)
}

serie_total <- agregado |>
  group_by(hora_indice) |>
  summarise(util_cpu = sum(util_cpu), util_mem = sum(util_mem),
            n_registros = sum(n_registros), .groups = "drop") |>
  mutate(dia_indice = floor(hora_indice / 24), hora_del_ciclo = hora_indice %% 24)

serie_nivel <- agregado |> filter(!is.na(nivel_prioridad))

COLORES_SERIE <- c(PALETA_NIVEL, sin_prioridad = "grey60", total = "black")
lab_x_cpu <- "CPU media por ventana (NCU, escala log10)"
lab_x_mem <- "Memoria media por ventana (NMU, escala log10)"
sub_ceros <- function(n0, n) sprintf("n = %s registros con valor > 0; %s registros con valor 0 no se muestran en escala log.",
                                     etiqueta_n(n), etiqueta_n(n0))

# =============================================================================
# G01. Histograma del uso de CPU
# =============================================================================
graficar(
  "G01", "01_histograma_cpu",
  pregunta = "¿Cómo se distribuye el uso medio de CPU de las ventanas de 5 minutos?",
  variables = "cpu_prom (instance_usage.average_usage.cpus)",
  datos = "Muestra, nivel registro",
  interpretacion = paste(
    "Cada barra cuenta registros en un intervalo de CPU (en escala log10, cada marca es 10 veces la anterior).",
    "Una distribución unimodal y ancha indica que la mayoría de ventanas usa una fracción muy pequeña de una",
    "máquina y que existe una cola de ventanas con consumos varios órdenes de magnitud mayores."),
  construir = function() {
    ggplot(uso_pos_cpu, aes(x = cpu_prom)) +
      geom_histogram(bins = 60, fill = "#4C78A8", colour = "white", linewidth = 0.1) +
      scale_x_log10(labels = label_log()) +
      scale_y_continuous(labels = label_number(big.mark = ".")) +
      labs(title = "Distribución del uso medio de CPU",
           subtitle = sub_ceros(n_cero_cpu, nrow(uso_pos_cpu)),
           x = lab_x_cpu, y = "Frecuencia absoluta (registros)", caption = PIE_FUENTE())
  }
)

# =============================================================================
# G02. Polígono de frecuencias del uso de CPU
# =============================================================================
graficar(
  "G02", "02_poligono_frecuencias_cpu",
  pregunta = "¿Cuál es la forma de la distribución de frecuencias de la CPU por intervalos?",
  variables = "cpu_prom",
  datos = "Muestra, nivel registro",
  interpretacion = paste(
    "Une los puntos medios de los mismos 60 intervalos del histograma. Facilita ver la moda, la asimetría",
    "y posibles modas secundarias (grupos de tareas con consumos característicos)."),
  construir = function() {
    ggplot(uso_pos_cpu, aes(x = cpu_prom)) +
      geom_freqpoly(bins = 60, colour = "#4C78A8", linewidth = 0.8) +
      geom_point(stat = "bin", bins = 60, aes(y = after_stat(count)), colour = "#4C78A8", size = 0.9) +
      scale_x_log10(labels = label_log()) +
      scale_y_continuous(labels = label_number(big.mark = ".")) +
      labs(title = "Polígono de frecuencias del uso medio de CPU",
           subtitle = "Mismos intervalos que el histograma (60 intervalos en escala log10)",
           x = lab_x_cpu, y = "Frecuencia absoluta (registros)", caption = PIE_FUENTE())
  }
)

# =============================================================================
# G03. Polígono de frecuencias comparativo por nivel de prioridad
# =============================================================================
graficar(
  "G03", "03_poligono_frecuencias_cpu_por_nivel",
  pregunta = "¿Difiere la forma de la distribución de CPU entre niveles de prioridad?",
  variables = "cpu_prom, nivel_prioridad (derivada de instance_events.priority)",
  datos = "Muestra, nivel registro",
  interpretacion = paste(
    "Se usa densidad (área = 1 por grupo) en lugar de frecuencia absoluta para comparar formas aunque los",
    "niveles tengan tamaños muy distintos. Un desplazamiento a la derecha indica mayor consumo típico."),
  construir = function() {
    d <- uso_pos_cpu |> filter(!is.na(nivel_prioridad))
    if (n_distinct(d$nivel_prioridad) < 2) return(NULL)
    ggplot(d, aes(x = cpu_prom, colour = nivel_prioridad)) +
      geom_freqpoly(aes(y = after_stat(density)), bins = 50, linewidth = 0.8) +
      scale_x_log10(labels = label_log()) +
      scale_colour_manual(values = PALETA_NIVEL, drop = TRUE) +
      labs(title = "Distribución de CPU por nivel de prioridad",
           subtitle = "Polígonos de frecuencia normalizados (densidad sobre log10 de la CPU)",
           x = lab_x_cpu, y = "Densidad", colour = "Nivel de prioridad", caption = PIE_FUENTE())
  }
)

# =============================================================================
# G04. Histograma del uso de memoria
# =============================================================================
graficar(
  "G04", "04_histograma_memoria",
  pregunta = "¿Cómo se distribuye el uso medio de memoria por ventana?",
  variables = "mem_prom (instance_usage.average_usage.memory)",
  datos = "Muestra, nivel registro",
  interpretacion = paste(
    "Compárese con G01: si la distribución de memoria es más estrecha que la de CPU, la memoria varía",
    "menos entre ventanas y tareas (suele ser más estable que la CPU)."),
  construir = function() {
    ggplot(uso_pos_mem, aes(x = mem_prom)) +
      geom_histogram(bins = 60, fill = "#54A24B", colour = "white", linewidth = 0.1) +
      scale_x_log10(labels = label_log()) +
      scale_y_continuous(labels = label_number(big.mark = ".")) +
      labs(title = "Distribución del uso medio de memoria",
           subtitle = sub_ceros(n_cero_mem, nrow(uso_pos_mem)),
           x = lab_x_mem, y = "Frecuencia absoluta (registros)", caption = PIE_FUENTE())
  }
)

# =============================================================================
# G05. Gráfico de sectores: proporción de registros por nivel de prioridad
# =============================================================================
graficar(
  "G05", "05_sectores_nivel_prioridad",
  pregunta = "¿Qué proporción de los registros de uso corresponde a cada nivel de prioridad?",
  variables = "nivel_prioridad",
  datos = "Muestra, nivel registro",
  interpretacion = paste(
    "Cada sector es la fracción de ventanas de uso de un nivel. Refleja dónde se concentra la actividad",
    "medida (número de ventanas), no el consumo total. Con 5 categorías como máximo la lectura es clara."),
  construir = function() {
    d <- uso_niv |> count(nivel_prioridad) |>
      mutate(pct = n / sum(n),
             etiqueta = sprintf("%s\n%.1f %%", nivel_prioridad, 100 * pct))
    if (nrow(d) > 6) return(NULL)
    n_na <- sum(is.na(uso$nivel_prioridad))
    ggplot(d, aes(x = "", y = n, fill = nivel_prioridad)) +
      geom_col(width = 1, colour = "white") +
      coord_polar(theta = "y") +
      geom_text(aes(label = ifelse(pct >= 0.03, etiqueta, "")),
                position = position_stack(vjust = 0.5), size = 3.2) +
      scale_fill_manual(values = PALETA_NIVEL) +
      labs(title = "Proporción de registros por nivel de prioridad",
           subtitle = sprintf("n = %s registros; %s sin prioridad excluidos. Sectores < 3 %% sin etiqueta.",
                              etiqueta_n(sum(d$n)), etiqueta_n(n_na)),
           fill = "Nivel", x = NULL, y = NULL, caption = PIE_FUENTE()) +
      theme_void(base_size = 11) +
      theme(plot.title = element_text(face = "bold", size = 14),
            plot.subtitle = element_text(colour = "grey30"),
            plot.caption = element_text(colour = "grey45", size = 8, hjust = 0),
            legend.position = "right")
  }
)

# =============================================================================
# G06. Gráfico de barras: eventos por tipo (población)
# =============================================================================
graficar(
  "G06", "06_barras_eventos_por_tipo",
  pregunta = "¿Qué tipos de eventos de instancia son más frecuentes en la celda?",
  variables = "tipo_evento (instance_events.type)",
  datos = "Población (todos los eventos de la celda, agregados en BigQuery)",
  interpretacion = paste(
    "Las barras ordenadas permiten ver el peso de cada tipo. La relación entre FINISH, KILL, FAIL y EVICT",
    "describe cómo terminan las tareas; muchas SUBMIT/QUEUE frente a SCHEDULE indican reintentos o espera."),
  construir = function() {
    d <- eventos |> group_by(tipo_evento) |> summarise(n = sum(n_eventos), .groups = "drop")
    ggplot(d, aes(x = reorder(tipo_evento, n), y = n)) +
      geom_col(fill = "#4C78A8") +
      geom_text(aes(label = label_number(scale_cut = cut_short_scale())(n)), hjust = -0.1, size = 3) +
      coord_flip() +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                         expand = expansion(mult = c(0, 0.12))) +
      labs(title = "Eventos de instancia por tipo",
           subtitle = sprintf("Total: %s eventos (sin instancias dentro de alloc sets)", etiqueta_n(sum(d$n))),
           x = "Tipo de evento", y = "Número de eventos", caption = PIE_FUENTE())
  }
)

# =============================================================================
# G07. Barras porcentuales: estado final de las instancias por nivel
# =============================================================================
graficar(
  "G07", "07_barras_porcentuales_estado_por_nivel",
  pregunta = "¿Cómo termina cada instancia (estado final) según su nivel de prioridad?",
  variables = "estado_final (último instance_events.type), nivel_prioridad",
  datos = "Muestra, nivel instancia",
  interpretacion = paste(
    "Cada barra suma 100 %. Compara la composición relativa: por ejemplo, si los niveles bajos tienen",
    "más EVICT, son desalojados para dar recursos a tareas de mayor prioridad. SCHEDULE como estado final",
    "suele indicar instancias aún en ejecución al cierre de la traza."),
  construir = function() {
    d <- instancias |> filter(!is.na(nivel_prioridad), !is.na(estado_final))
    ggplot(d, aes(x = nivel_prioridad, fill = estado_final)) +
      geom_bar(position = "fill", colour = "white", linewidth = 0.2) +
      scale_y_continuous(labels = label_percent()) +
      scale_fill_brewer(palette = "Set2") +
      labs(title = "Composición del estado final de las instancias por nivel",
           subtitle = sprintf("n = %s instancias de la muestra", etiqueta_n(nrow(d))),
           x = "Nivel de prioridad", y = "Porcentaje de instancias", fill = "Último evento",
           caption = PIE_FUENTE())
  }
)

# =============================================================================
# G08. Diagrama de cajas de CPU por nivel
# =============================================================================
graficar(
  "G08", "08_cajas_cpu_por_nivel",
  pregunta = "¿Cómo varían mediana, cuartiles y dispersión de la CPU entre niveles?",
  variables = "cpu_prom, nivel_prioridad",
  datos = "Muestra, nivel registro",
  interpretacion = paste(
    "La línea central es la mediana; la caja va de Q1 a Q3 (calculados sobre log10). Los bigotes llegan a",
    "1,5 × IQR y los puntos fuera son atípicos según Tukey dentro de cada grupo. El rombo es la media",
    "geométrica, menos sensible a la asimetría."),
  construir = function() {
    d <- uso_niv |> filter(cpu_prom > 0)
    ggplot(d, aes(x = nivel_prioridad, y = cpu_prom, fill = nivel_prioridad)) +
      geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.15, width = 0.6) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 2.5, fill = "white") +
      scale_y_log10(labels = label_log()) +
      scale_fill_manual(values = PALETA_NIVEL, guide = "none") +
      labs(title = "CPU media por nivel de prioridad",
           subtitle = "Diagrama de cajas en escala log10; rombo = media de log10 (media geométrica)",
           x = "Nivel de prioridad", y = "CPU media por ventana (NCU, log10)", caption = PIE_FUENTE())
  }
)

# =============================================================================
# G09. Diagrama de cajas de memoria por nivel
# =============================================================================
graficar(
  "G09", "09_cajas_memoria_por_nivel",
  pregunta = "¿Cómo se compara la distribución de memoria entre niveles de prioridad?",
  variables = "mem_prom, nivel_prioridad",
  datos = "Muestra, nivel registro",
  interpretacion = "Igual que G08 para la memoria. Cajas más estrechas indican consumos más homogéneos dentro del nivel.",
  construir = function() {
    d <- uso_niv |> filter(mem_prom > 0)
    ggplot(d, aes(x = nivel_prioridad, y = mem_prom, fill = nivel_prioridad)) +
      geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.15, width = 0.6) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 2.5, fill = "white") +
      scale_y_log10(labels = label_log()) +
      scale_fill_manual(values = PALETA_NIVEL, guide = "none") +
      labs(title = "Memoria media por nivel de prioridad",
           subtitle = "Diagrama de cajas en escala log10; rombo = media geométrica",
           x = "Nivel de prioridad", y = "Memoria media por ventana (NMU, log10)", caption = PIE_FUENTE())
  }
)

# =============================================================================
# G10. Dispersión CPU frente a memoria
# =============================================================================
graficar(
  "G10", "10_dispersion_cpu_memoria",
  pregunta = "¿Existe relación entre el uso de CPU y el de memoria en una misma ventana?",
  variables = "cpu_prom, mem_prom, nivel_prioridad",
  datos = sprintf("Submuestra aleatoria de hasta %s registros", etiqueta_n(CONFIG$n_puntos_dispersion)),
  interpretacion = paste(
    "Ambos ejes en log10. Una nube alargada en diagonal indica relación positiva (tareas grandes en CPU",
    "también lo son en memoria); bandas horizontales o verticales revelan tareas con un recurso fijo."),
  construir = function() {
    ggplot(sub_disp, aes(x = cpu_prom, y = mem_prom, colour = nivel_prioridad)) +
      geom_point(alpha = 0.25, size = 0.5) +
      scale_x_log10(labels = label_log()) +
      scale_y_log10(labels = label_log()) +
      scale_colour_manual(values = PALETA_NIVEL, na.value = "grey60") +
      guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
      labs(title = "Uso de CPU frente a uso de memoria",
           subtitle = sprintf("Submuestra de %s registros, escalas log10", etiqueta_n(nrow(sub_disp))),
           x = lab_x_cpu, y = lab_x_mem, colour = "Nivel", caption = PIE_FUENTE())
  }
)

# =============================================================================
# G11. Gráfico de densidad de CPU y memoria
# =============================================================================
graficar(
  "G11", "11_densidad_cpu_memoria",
  pregunta = "¿Qué forma tienen las distribuciones de CPU y memoria y cómo difieren por nivel?",
  variables = "cpu_prom, mem_prom, nivel_prioridad",
  datos = "Muestra, nivel registro",
  interpretacion = paste(
    "Estimación de densidad por núcleo sobre log10. Picos marcados indican consumos típicos; curvas",
    "desplazadas a la derecha, niveles que consumen más."),
  construir = function() {
    d <- uso_niv |>
      select(nivel_prioridad, CPU = cpu_prom, Memoria = mem_prom) |>
      pivot_longer(c(CPU, Memoria), names_to = "recurso", values_to = "valor") |>
      filter(valor > 0)
    ggplot(d, aes(x = valor, fill = nivel_prioridad, colour = nivel_prioridad)) +
      geom_density(alpha = 0.15, linewidth = 0.6, adjust = 1.2) +
      facet_wrap(~ recurso, scales = "free") +
      scale_x_log10(labels = label_log()) +
      scale_fill_manual(values = PALETA_NIVEL) +
      scale_colour_manual(values = PALETA_NIVEL) +
      labs(title = "Densidad del uso de CPU y memoria por nivel",
           subtitle = "Densidad kernel sobre log10 del valor (unidades normalizadas NCU / NMU)",
           x = "Uso medio por ventana (unidad normalizada, log10)", y = "Densidad",
           fill = "Nivel", colour = "Nivel", caption = PIE_FUENTE())
  }
)

# =============================================================================
# G12. Gráfico de correlación (Spearman)
# =============================================================================
graficar(
  "G12", "12_matriz_correlacion",
  pregunta = "¿Qué variables numéricas están asociadas entre sí y con qué intensidad?",
  variables = "cpu_prom, mem_prom, cpu_max, mem_max, mem_asignada, cpu_solicitada, mem_solicitada, ciclos_por_instruccion, prioridad",
  datos = "Muestra, nivel registro (pares completos)",
  interpretacion = paste(
    "Coeficiente de Spearman (por rangos), robusto a la asimetría y a relaciones monótonas no lineales.",
    "Valores cercanos a ±1 indican asociación fuerte. Correlaciones altas entre variable media y máxima",
    "son esperables por construcción y no aportan información nueva."),
  construir = function() {
    cor_tab <- readr::read_csv(file.path(RUTAS$results, "muestra_correlaciones.csv"),
                               show_col_types = FALSE) |>
      filter(metodo == "spearman")
    orden <- unique(cor_tab$var_x)
    cor_tab <- cor_tab |> mutate(var_x = factor(var_x, levels = orden),
                                 var_y = factor(var_y, levels = rev(orden)))
    ggplot(cor_tab, aes(x = var_x, y = var_y, fill = correlacion)) +
      geom_tile(colour = "white") +
      geom_text(aes(label = sprintf("%.2f", correlacion)), size = 2.8) +
      scale_fill_gradient2(low = "#3B6FB6", mid = "white", high = "#C8423B",
                           midpoint = 0, limits = c(-1, 1)) +
      labs(title = "Matriz de correlación de Spearman",
           subtitle = "Variables numéricas de uso, solicitud y prioridad",
           x = NULL, y = NULL, fill = "rho", caption = PIE_FUENTE()) +
      theme(axis.text.x = element_text(angle = 40, hjust = 1), panel.grid = element_blank())
  }, ancho = 20, alto = 16
)

# =============================================================================
# G13. Serie temporal del uso promedio de CPU (población)
# =============================================================================
graficar(
  "G13", "13_serie_temporal_cpu",
  pregunta = "¿Cómo evoluciona la utilización de CPU de la celda a lo largo de la traza? ¿Dónde están los picos?",
  variables = "util_cpu = suma(cpu) / (12 × capacidad CPU), hora_indice, nivel_prioridad",
  datos = "Población (agregado horario de todas las instancias)",
  interpretacion = paste(
    "Eje x en días desde el origen de la traza (no son fechas). Oscilaciones regulares de 24 h indican",
    "ciclo diario; los puntos marcan las 5 horas de mayor utilización total. Las líneas por nivel muestran",
    "qué tipo de carga explica los picos."),
  construir = function() {
    picos <- serie_total |> slice_max(util_cpu, n = 5)
    ggplot() +
      geom_line(data = serie_nivel, aes(x = hora_indice / 24, y = util_cpu, colour = nivel_prioridad),
                linewidth = 0.35) +
      geom_line(data = serie_total, aes(x = hora_indice / 24, y = util_cpu, colour = "total"),
                linewidth = 0.55) +
      geom_point(data = picos, aes(x = hora_indice / 24, y = util_cpu), colour = "red", size = 2) +
      scale_colour_manual(values = COLORES_SERIE) +
      scale_y_continuous(labels = label_percent()) +
      labs(title = "Utilización media de CPU de la celda por hora",
           subtitle = "Total y por nivel de prioridad; puntos rojos = 5 horas pico",
           x = "Días desde el inicio de la traza", y = "Utilización de CPU (% de la capacidad)",
           colour = "Serie", caption = PIE_FUENTE())
  }, ancho = 24
)

# =============================================================================
# G14. Serie temporal del uso promedio de memoria (población)
# =============================================================================
graficar(
  "G14", "14_serie_temporal_memoria",
  pregunta = "¿Cómo evoluciona la utilización de memoria de la celda?",
  variables = "util_mem = suma(memoria) / (12 × capacidad memoria), hora_indice, nivel_prioridad",
  datos = "Población (agregado horario)",
  interpretacion = paste(
    "Compárese con G13: si la memoria muestra menos oscilación diaria que la CPU, su consumo depende",
    "menos de la carga instantánea (la memoria se reserva y se mantiene)."),
  construir = function() {
    picos <- serie_total |> slice_max(util_mem, n = 5)
    ggplot() +
      geom_line(data = serie_nivel, aes(x = hora_indice / 24, y = util_mem, colour = nivel_prioridad),
                linewidth = 0.35) +
      geom_line(data = serie_total, aes(x = hora_indice / 24, y = util_mem, colour = "total"),
                linewidth = 0.55) +
      geom_point(data = picos, aes(x = hora_indice / 24, y = util_mem), colour = "red", size = 2) +
      scale_colour_manual(values = COLORES_SERIE) +
      scale_y_continuous(labels = label_percent()) +
      labs(title = "Utilización media de memoria de la celda por hora",
           subtitle = "Total y por nivel de prioridad; puntos rojos = 5 horas pico",
           x = "Días desde el inicio de la traza", y = "Utilización de memoria (% de la capacidad)",
           colour = "Serie", caption = PIE_FUENTE())
  }, ancho = 24
)

# =============================================================================
# G15. Mapa de calor temporal (día × hora relativa)
# =============================================================================
graficar(
  "G15", "15_mapa_calor_cpu_dia_hora",
  pregunta = "¿Hay un patrón repetido por hora del ciclo diario y cambia entre días?",
  variables = "util_cpu total, dia_indice, hora_del_ciclo",
  datos = "Población (agregado horario); resolución de 1 h, suficiente para este mapa",
  interpretacion = paste(
    "Cada celda es una hora. Columnas más claras repetidas en todos los días indican un ciclo diario;",
    "filas atípicas indican días con carga anómala. La hora es relativa al origen de la traza, no hora local."),
  construir = function() {
    d <- serie_total |> filter(hora_indice >= 0)
    if (n_distinct(d$dia_indice) < 2) return(NULL)
    ggplot(d, aes(x = hora_del_ciclo, y = dia_indice, fill = util_cpu)) +
      geom_tile() +
      scale_fill_viridis_c(labels = label_percent(), option = "magma") +
      scale_x_continuous(breaks = seq(0, 23, 3), expand = c(0, 0)) +
      scale_y_reverse(breaks = seq(0, max(d$dia_indice), 5), expand = c(0, 0)) +
      labs(title = "Mapa de calor de la utilización de CPU",
           subtitle = "Día (fila) × hora relativa del ciclo de 24 h (columna)",
           x = "Hora del ciclo (horas desde el origen de la traza, módulo 24)",
           y = "Día desde el inicio de la traza", fill = "Utilización CPU", caption = PIE_FUENTE()) +
      theme(panel.grid = element_blank(), legend.position = "right")
  }, alto = 16
)

# =============================================================================
# G16. Gráfico de valores atípicos
# =============================================================================
graficar(
  "G16", "16_valores_atipicos_cpu",
  pregunta = "¿Dónde se concentran las observaciones extremas de CPU y son plausibles?",
  variables = "cpu_prom, flag_atipico_cpu (Tukey sobre log10), nivel_prioridad",
  datos = "Muestra, nivel registro (atípicos submuestreados a 20.000 como máximo)",
  interpretacion = paste(
    "Puntos rojos: registros atípicos según Tukey (1,5 × IQR) sobre log10 de TODA la muestra. No se",
    "eliminaron; si son consistentes con la CPU solicitada y el nivel (p. ej., tareas prod grandes), son",
    "comportamiento válido. Valores > 1 NCU por instancia serían sospechosos (ver results/calidad_controles.csv)."),
  construir = function() {
    d <- uso_niv |> filter(cpu_prom > 0)
    atip <- d |> filter(flag_atipico_cpu)
    if (nrow(atip) > 20000) atip <- slice_sample(atip, n = 20000)
    ggplot(d, aes(x = nivel_prioridad, y = cpu_prom)) +
      geom_boxplot(outlier.shape = NA, fill = "grey92", width = 0.55) +
      geom_jitter(data = atip, colour = "#E45756", alpha = 0.3, size = 0.6, width = 0.18, height = 0) +
      geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
      annotate("text", x = 0.6, y = 1, label = "1 NCU", vjust = -0.5, hjust = 0, size = 3, colour = "grey30") +
      scale_y_log10(labels = label_log()) +
      labs(title = "Valores atípicos del uso de CPU",
           subtitle = sprintf("%s registros atípicos (%.2f %% de la muestra), marcados pero no eliminados",
                              etiqueta_n(sum(d$flag_atipico_cpu)), 100 * mean(d$flag_atipico_cpu)),
           x = "Nivel de prioridad", y = "CPU media por ventana (NCU, log10)", caption = PIE_FUENTE())
  }
)

# =============================================================================
# G17. Comparación del consumo por prioridad (nivel instancia, IC 95 %)
# =============================================================================
graficar(
  "G17", "17_comparacion_consumo_por_nivel",
  pregunta = "¿Consumen en promedio más recursos las instancias de mayor prioridad?",
  variables = "cpu_prom_media, mem_prom_media (medias por instancia), nivel_prioridad",
  datos = "Muestra, nivel instancia (cada tarea pesa lo mismo)",
  interpretacion = paste(
    "Barras = media aritmética por nivel con intervalo de confianza del 95 % (media ± 1,96 EE); X = mediana.",
    "Si media >> mediana, pocas instancias muy grandes elevan la media. Las instancias de una misma",
    "colección no son independientes, por lo que los IC son aproximados (optimistas)."),
  construir = function() {
    d <- instancias |>
      filter(!is.na(nivel_prioridad)) |>
      select(nivel_prioridad, CPU = cpu_prom_media, Memoria = mem_prom_media) |>
      pivot_longer(-nivel_prioridad, names_to = "recurso", values_to = "valor") |>
      group_by(nivel_prioridad, recurso) |>
      summarise(n = n(), media = mean(valor), mediana = median(valor),
                ee = stats::sd(valor) / sqrt(n()), .groups = "drop") |>
      mutate(li = pmax(media - 1.96 * ee, 0), ls = media + 1.96 * ee)
    ggplot(d, aes(x = nivel_prioridad, y = media, fill = nivel_prioridad)) +
      geom_col(width = 0.65) +
      geom_errorbar(aes(ymin = li, ymax = ls), width = 0.2, na.rm = TRUE) +
      geom_point(aes(y = mediana), shape = 4, size = 2.5, stroke = 1) +
      facet_wrap(~ recurso, scales = "free_y") +
      scale_fill_manual(values = PALETA_NIVEL, guide = "none") +
      labs(title = "Consumo medio por instancia según nivel de prioridad",
           subtitle = "Media ± IC 95 % (barras) y mediana (X); unidades normalizadas NCU / NMU",
           x = "Nivel de prioridad", y = "Uso medio por instancia (unidad normalizada)",
           caption = PIE_FUENTE())
  }
)

# =============================================================================
# G18. Dispersión con línea de tendencia
# =============================================================================
graficar(
  "G18", "18_dispersion_tendencia_cpu_memoria",
  pregunta = "¿Cuál es la tendencia de la relación CPU–memoria y varía por nivel?",
  variables = "cpu_prom, mem_prom, nivel_prioridad",
  datos = sprintf("Submuestra de hasta %s registros", etiqueta_n(CONFIG$n_puntos_dispersion)),
  interpretacion = paste(
    "Recta de mínimos cuadrados sobre log10(memoria) ~ log10(CPU) por nivel: su pendiente es una",
    "elasticidad (pendiente 1 = proporcionalidad). Es descriptiva, no un modelo predictivo validado."),
  construir = function() {
    d <- sub_disp |> filter(!is.na(nivel_prioridad))
    rho <- suppressWarnings(stats::cor(d$cpu_prom, d$mem_prom, method = "spearman"))
    ggplot(d, aes(x = cpu_prom, y = mem_prom)) +
      geom_point(alpha = 0.15, size = 0.4, colour = "grey40") +
      geom_smooth(aes(colour = nivel_prioridad), method = "lm", formula = y ~ x, se = TRUE,
                  linewidth = 0.9) +
      facet_wrap(~ nivel_prioridad) +
      scale_x_log10(labels = label_log()) +
      scale_y_log10(labels = label_log()) +
      scale_colour_manual(values = PALETA_NIVEL, guide = "none") +
      labs(title = "Relación CPU–memoria con tendencia lineal (log–log)",
           subtitle = sprintf("Spearman global en la submuestra: rho = %.2f", rho),
           x = lab_x_cpu, y = lab_x_mem, caption = PIE_FUENTE())
  }, ancho = 24, alto = 16
)

# =============================================================================
# GRÁFICOS ADICIONALES justificados por los datos disponibles
# =============================================================================

# G19. Densidad bidimensional (hexágonos) con todos los registros
graficar(
  "G19", "19_hexbin_cpu_memoria",
  pregunta = "¿Dónde se concentra la mayoría de registros en el plano CPU–memoria?",
  variables = "cpu_prom, mem_prom",
  datos = "Muestra completa, nivel registro",
  interpretacion = "Complementa G10 sin submuestreo: el color indica cuántos registros caen en cada hexágono (escala log).",
  construir = function() {
    if (!requireNamespace("hexbin", quietly = TRUE)) return(NULL)
    d <- uso |> filter(cpu_prom > 0, mem_prom > 0)
    ggplot(d, aes(x = cpu_prom, y = mem_prom)) +
      geom_hex(bins = 60) +
      scale_x_log10(labels = label_log()) +
      scale_y_log10(labels = label_log()) +
      scale_fill_viridis_c(trans = "log10", labels = label_number(big.mark = ".")) +
      labs(title = "Concentración de registros en el plano CPU–memoria",
           subtitle = sprintf("n = %s registros", etiqueta_n(nrow(d))),
           x = lab_x_cpu, y = lab_x_mem, fill = "Registros", caption = PIE_FUENTE()) +
      theme(legend.position = "right")
  }
)

# G20. Función de distribución empírica (ECDF) de CPU por nivel
graficar(
  "G20", "20_ecdf_cpu_por_nivel",
  pregunta = "¿Qué porcentaje de ventanas está por debajo de cada nivel de CPU, por prioridad?",
  variables = "cpu_prom, nivel_prioridad",
  datos = "Muestra, nivel registro",
  interpretacion = "La curva más a la derecha domina en consumo. Permite leer percentiles directamente (p. ej., la mediana en y = 0,5).",
  construir = function() {
    d <- uso_niv |> filter(cpu_prom > 0)
    ggplot(d, aes(x = cpu_prom, colour = nivel_prioridad)) +
      stat_ecdf(linewidth = 0.8) +
      geom_hline(yintercept = c(0.5, 0.9), linetype = "dotted", colour = "grey50") +
      scale_x_log10(labels = label_log()) +
      scale_y_continuous(labels = label_percent()) +
      scale_colour_manual(values = PALETA_NIVEL) +
      labs(title = "Distribución acumulada empírica de la CPU por nivel",
           subtitle = "Líneas punteadas: percentiles 50 y 90",
           x = lab_x_cpu, y = "Proporción acumulada de registros", colour = "Nivel",
           caption = PIE_FUENTE())
  }
)

# G21. Uso real frente a recursos solicitados (nivel instancia)
graficar(
  "G21", "21_uso_vs_solicitud_cpu",
  pregunta = "¿Usan las tareas la CPU que solicitan al planificador?",
  variables = "cpu_solicitada (instance_events.resource_request.cpus), cpu_prom_media",
  datos = "Muestra, nivel instancia",
  interpretacion = paste(
    "La diagonal discontinua es uso = solicitud. Puntos por debajo indican sobreasignación (se pide más de",
    "lo que se usa), base de la sobresuscripción en clústeres; por encima, uso superior a lo solicitado."),
  construir = function() {
    d <- instancias |> filter(cpu_solicitada > 0, cpu_prom_media > 0, !is.na(nivel_prioridad))
    if (nrow(d) < 10) return(NULL)
    ggplot(d, aes(x = cpu_solicitada, y = cpu_prom_media, colour = nivel_prioridad)) +
      geom_point(alpha = 0.35, size = 0.8) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      scale_x_log10(labels = label_log()) +
      scale_y_log10(labels = label_log()) +
      scale_colour_manual(values = PALETA_NIVEL) +
      guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
      labs(title = "CPU utilizada frente a CPU solicitada por instancia",
           subtitle = sprintf("n = %s instancias; mediana uso/solicitud = %.2f", etiqueta_n(nrow(d)),
                              median(d$cpu_prom_media / d$cpu_solicitada)),
           x = "CPU solicitada (NCU, log10)", y = "CPU media utilizada (NCU, log10)",
           colour = "Nivel", caption = PIE_FUENTE())
  }
)

# G22. Áreas apiladas: contribución de cada nivel a la utilización total
graficar(
  "G22", "22_area_apilada_cpu_por_nivel",
  pregunta = "¿Qué nivel de prioridad aporta más a la utilización total de CPU en cada momento?",
  variables = "util_cpu por nivel, hora_indice",
  datos = "Población (agregado horario)",
  interpretacion = "La altura total es la utilización de la celda; cada banda, la parte de un nivel. Complementa G13 mostrando composición.",
  construir = function() {
    ggplot(serie_nivel, aes(x = hora_indice / 24, y = util_cpu, fill = nivel_prioridad)) +
      geom_area(position = "stack", alpha = 0.9) +
      scale_fill_manual(values = COLORES_SERIE) +
      scale_y_continuous(labels = label_percent()) +
      labs(title = "Composición de la utilización de CPU por nivel de prioridad",
           subtitle = "Áreas apiladas, resolución horaria",
           x = "Días desde el inicio de la traza", y = "Utilización de CPU (% de la capacidad)",
           fill = "Nivel", caption = PIE_FUENTE())
  }, ancho = 24
)

# G23. Perfil del ciclo de 24 h (media y banda P10–P90 entre días)
graficar(
  "G23", "23_perfil_ciclo_24h",
  pregunta = "¿Cuál es el perfil típico de utilización a lo largo del ciclo de 24 horas?",
  variables = "util_cpu, util_mem, hora_del_ciclo",
  datos = "Población (agregado horario), excluyendo las horas extremas incompletas",
  interpretacion = "La línea es la media entre días para cada hora relativa; la banda, la variabilidad entre días (P10–P90).",
  construir = function() {
    d <- serie_total |>
      filter(hora_indice > min(hora_indice), hora_indice < max(hora_indice)) |>
      select(hora_del_ciclo, CPU = util_cpu, Memoria = util_mem) |>
      pivot_longer(-hora_del_ciclo, names_to = "recurso", values_to = "util") |>
      group_by(hora_del_ciclo, recurso) |>
      summarise(media = mean(util), p10 = stats::quantile(util, 0.1),
                p90 = stats::quantile(util, 0.9), .groups = "drop")
    ggplot(d, aes(x = hora_del_ciclo, y = media, colour = recurso, fill = recurso)) +
      geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.2, colour = NA) +
      geom_line(linewidth = 0.9) +
      scale_x_continuous(breaks = seq(0, 23, 3)) +
      scale_y_continuous(labels = label_percent()) +
      scale_colour_manual(values = c(CPU = "#4C78A8", Memoria = "#54A24B")) +
      scale_fill_manual(values = c(CPU = "#4C78A8", Memoria = "#54A24B")) +
      labs(title = "Perfil de utilización según la hora del ciclo de 24 h",
           subtitle = "Media entre días y banda P10–P90 (hora relativa al origen de la traza)",
           x = "Hora del ciclo", y = "Utilización (% de la capacidad)",
           colour = "Recurso", fill = "Recurso", caption = PIE_FUENTE())
  }
)

# G24. Duración de vida de las instancias según su estado final
graficar(
  "G24", "24_duracion_vida_por_estado",
  pregunta = "¿Cuánto viven las instancias según cómo terminan?",
  variables = "duracion_vida_h (último evento terminal − primer SCHEDULE), estado_final",
  datos = "Muestra, nivel instancia con ambos eventos observados",
  interpretacion = paste(
    "Escala log10 en horas. Las instancias aún activas al final de la traza no tienen evento terminal",
    "y quedan fuera (censura por la derecha): las duraciones largas están subrepresentadas."),
  construir = function() {
    d <- instancias |> filter(!is.na(duracion_vida_h), duracion_vida_h > 0, !is.na(estado_final))
    if (nrow(d) < 10) return(NULL)
    ggplot(d, aes(x = reorder(estado_final, duracion_vida_h, FUN = median), y = duracion_vida_h)) +
      geom_boxplot(fill = "#B279A2", alpha = 0.5, outlier.size = 0.4, outlier.alpha = 0.3) +
      scale_y_log10(labels = label_number(accuracy = 0.01, big.mark = ".")) +
      labs(title = "Duración de vida de las instancias por estado final",
           subtitle = sprintf("n = %s instancias con SCHEDULE y evento terminal observados", etiqueta_n(nrow(d))),
           x = "Último evento", y = "Duración de vida (horas, log10)", caption = PIE_FUENTE())
  }
)

# =============================================================================
# Índice de gráficos
# =============================================================================
guardar_resultado(indice, "indice_graficos")
message(sprintf("Se generaron %d gráficos en %s.", nrow(indice), RUTAS$plots))
