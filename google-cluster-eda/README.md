# Análisis exploratorio de Google Cluster Data 2019 con R

Proyecto reproducible de estadística aplicada que analiza el consumo de CPU y memoria y el
comportamiento temporal de las cargas de trabajo de las trazas públicas de Google (Borg, versión v3,
ocho celdas, mayo de 2019), consultando una muestra y agregados en BigQuery sin descargar el
conjunto completo.

## 1. Fuentes oficiales

| Recurso | Enlace |
|---|---|
| Repositorio oficial | https://github.com/google/cluster-data |
| Descripción de la traza 2019 | https://github.com/google/cluster-data/blob/master/ClusterData2019.md |
| Esquema (protocol buffer v3) | https://github.com/google/cluster-data/blob/master/clusterdata_trace_format_v3.proto |
| Cuaderno oficial de análisis (consultas de referencia) | https://github.com/google/cluster-data/blob/master/clusterdata_analysis_colab.ipynb |
| Documento técnico *Google cluster-usage traces v3* | enlazado desde ClusterData2019.md (Google Drive) |
| Lista de correo | https://groups.google.com/g/googleclusterdata-discuss |
| Licencia | CC-BY 4.0 |

### Qué se verificó y qué no

Verificado en las fuentes oficiales (octubre de 2026):

* La traza 2019 cubre 8 celdas de Borg en mayo de 2019, ocupa unos 2,4 TiB comprimidos y **solo
  está disponible a través de BigQuery** (no hay descarga directa de archivos).
* Proyecto `google.com:google-cluster-data`, *datasets* `clusterdata_2019_a` … `clusterdata_2019_h`,
  tablas `machine_events`, `collection_events`, `instance_events`, `instance_usage`
  (tal como las usa el cuaderno oficial).
* Nombres y tipos de los campos usados, códigos de los *enums* (`EventType`, `CollectionType`,
  `LatencySensitivity`) y unidades: CPU en NCU y memoria normalizada; tiempos en microsegundos desde
  el inicio de la traza; ventanas de uso de ~300 s (definidos en el `.proto`).
* Rangos de prioridad del cuaderno oficial (free 0–99, beb 100–115, mid 116–119, prod ≥ 120) y el
  nivel *monitoring* (≥ 360) descrito en Tirmazi et al. (2020).

No verificado directamente (indícalo así si lo citas):

* El documento técnico v3 está alojado en Google Drive y no pude abrirlo; los detalles del origen
  temporal exacto y de valores centinela (0 / máximo de INT64) se tratan de forma **defensiva** en
  las consultas. Consulta la sección de tiempo de ese documento.
* Los tipos BigQuery exactos (p. ej., que `type` y `scheduling_class` sean `INT64`) se comprueban al
  ejecutar `sql/00_esquema_tablas.sql`; el script de extracción se detiene si falta alguna columna.
* El coste exacto en bytes de cada consulta: depende de la tabla y se mide con la ejecución en seco.
* **El código no se ejecutó en el entorno donde se escribió** (no había R disponible). Se incluye un
  generador de datos simulados con el mismo esquema para que compruebes el flujo completo en tu
  equipo antes de usar BigQuery.

## 2. Estructura

```
google-cluster-eda/
├── google-cluster-eda.Rproj      # abre el proyecto en RStudio (fija el directorio de trabajo)
├── main.R                        # ejecuta TODO el flujo en orden
├── config.R                      # parámetros: fuente de datos, celda, muestreo, límites de coste
├── sql/
│   ├── 00_esquema_tablas.sql     # verifica columnas y tipos reales (INFORMATION_SCHEMA)
│   ├── 01_capacidad_celda.sql    # capacidad total de CPU y memoria (población)
│   ├── 02_muestra_uso_instancias.sql  # muestra por colecciones + atributos de instancia
│   ├── 03_agregado_horario_nivel.sql  # uso horario por nivel de prioridad (población)
│   └── 04_conteo_eventos.sql     # eventos por tipo y nivel (población)
├── scripts/
│   ├── 00_instalar_paquetes.R
│   ├── utilidades.R              # funciones comunes, tema gráfico, códigos de enums
│   ├── 01_extraer_bigquery.R     # ejecuta SQL desde R con control de coste
│   ├── 01b_datos_simulados.R     # datos SINTÉTICOS para probar el flujo
│   ├── 02_importar_limpiar.R     # inspección, calidad, variables derivadas, exportación
│   ├── 03_estadistica_descriptiva.R
│   ├── 04_graficos.R             # 24 gráficos ggplot2 (PNG 300 ppp + PDF)
│   └── 05_preparacion_modelado.R # tabla de modelado, partición cronológica, modelos base
├── docs/diccionario_datos.csv    # diccionario de datos (originales y derivadas)
├── reporte/informe.qmd           # informe final Quarto
├── data/raw/                     # exportaciones de BigQuery (o simuladas)
├── data/processed/               # datos limpios (Parquet y CSV)
├── plots/                        # gráficos
└── results/                      # tablas estadísticas, controles de calidad, métricas
```

## 3. Instalación

1. Instala **R ≥ 4.1** y **RStudio**.
2. Instala **Quarto CLI** (https://quarto.org/docs/get-started/) para renderizar el informe
   (RStudio reciente ya lo incluye).
3. Abre `google-cluster-eda.Rproj` y ejecuta:

```r
source("scripts/00_instalar_paquetes.R")
```

## 4. Prueba rápida sin Google Cloud (datos simulados)

Con `fuente_datos = "simulado"` (valor por defecto en `config.R`):

```r
source("main.R")
```

Se generan datos sintéticos, todas las tablas, los gráficos y `reporte/informe.html`, que muestra un
aviso de **datos simulados**. Sirve solo para confirmar que todo funciona: no presentes esas cifras.

## 5. Acceso a los datos reales en BigQuery

### 5.1 Configuración de Google Cloud (una vez)

1. Entra en https://console.cloud.google.com/ con tu cuenta de Google y crea un proyecto
   (p. ej., `mi-proyecto-borg`). Anota su **ID de proyecto**.
2. Habilita la **API de BigQuery** (*APIs y servicios → Biblioteca → BigQuery API*).
3. Facturación: puedes usar el **sandbox de BigQuery** (sin tarjeta, con cuota gratuita mensual de
   consultas) o asociar una cuenta de facturación. Las consultas se cobran a **tu** proyecto según
   los bytes procesados.
4. Comprueba el acceso abriendo en la consola de BigQuery una consulta de prueba, por ejemplo
   `sql/01_capacidad_celda.sql` sustituyendo `{{proyecto_datos}}` por `google.com:google-cluster-data`
   y `{{celda}}` por `a`. Si obtienes un error de permisos, revisa las condiciones de acceso del
   documento v3 o consulta la lista de correo.
5. Define el proyecto de facturación en `~/.Renviron` (luego reinicia R):

```
GCP_BILLING_PROJECT=mi-proyecto-borg
```

### 5.2 Control de coste

* Antes de ejecutar, la consola de BigQuery muestra “Esta consulta procesará X GB”. Desde R,
  `01_extraer_bigquery.R` hace una ejecución en seco (`bq_perform_query_dry_run`) y **aborta** si la
  estimación supera `CONFIG$max_gb_por_consulta`.
* Desde la terminal: `bq query --use_legacy_sql=false --dry_run < consulta.sql`.
* Las consultas 02 y 03 recorren `instance_usage` completa de la celda (solo las columnas
  seleccionadas): son las más caras. El muestreo por hash reduce las filas devueltas, **no** los
  bytes escaneados.

### 5.3 Opción A — Extracción automática desde R

En `config.R`: `fuente_datos = "bigquery"`, elige `celda` y `modulo_muestra`, y ejecuta
`source("main.R")`. La primera vez se abrirá el navegador para autorizar `bigrquery`.
Se guardan en `data/raw/`: `esquema_tablas.csv`, `capacidad_celda.csv`,
`muestra_uso_instancias.parquet`, `agregado_horario_nivel.parquet`, `conteo_eventos.csv` y
`metadatos_extraccion.csv`.

Si la muestra supera `CONFIG$max_filas_descarga`, aumenta `modulo_muestra` (p. ej., 2000 = 0,05 %
de las colecciones) o usa la opción B.

### 5.4 Opción B — Consola de BigQuery y exportación manual

1. Pega cada consulta de `sql/` en la consola sustituyendo `{{proyecto_datos}}`,
   `{{celda}}` y `{{modulo_muestra}}`.
2. Resultados pequeños (01, 03, 04): *Guardar resultados → CSV (archivo local)*.
3. Resultados grandes (02): guarda el resultado en una tabla de tu proyecto y expórtalo a Cloud
   Storage en Parquet:

```sql
EXPORT DATA OPTIONS (
  uri = 'gs://MI_BUCKET/borg/muestra_uso_instancias-*.parquet',
  format = 'PARQUET',
  overwrite = true
) AS
SELECT * FROM `mi-proyecto-borg.mi_dataset.muestra_uso_instancias`;
```

```bash
gsutil -m cp "gs://MI_BUCKET/borg/muestra_uso_instancias-*.parquet" data/raw/
```

   Si se generan varios archivos, combínalos en R:
   `arrow::write_parquet(arrow::open_dataset("data/raw", format = "parquet") |> dplyr::collect(), "data/raw/muestra_uso_instancias.parquet")`
   (mueve antes los fragmentos a una subcarpeta para no mezclarlos con otros Parquet).
4. Nombra los archivos exactamente como en la opción A, crea `metadatos_extraccion.csv` con las
   columnas `fuente,proyecto_datos,dataset,celda,modulo_muestra,filas_muestra,fecha_extraccion,version_bigrquery`
   y pon `fuente_datos = "local"`.

## 6. Ejecución

```r
source("main.R")          # desde RStudio, con el proyecto abierto
```

```bash
Rscript main.R            # desde la terminal, en la raíz del proyecto
```

Orden: extracción (01 / 01b) → limpieza (02) → descriptivos (03) → gráficos (04) →
modelado (05) → informe (`reporte/informe.html`). Cada script puede ejecutarse por separado en ese
orden.

## 7. Gráficos generados

| Id | Gráfico | Pregunta | Datos |
|---|---|---|---|
| G01 | Histograma de CPU | Distribución del uso medio de CPU | muestra |
| G02 | Polígono de frecuencias de CPU | Forma de la distribución por intervalos | muestra |
| G03 | Polígono comparativo por nivel | ¿Difiere la CPU entre prioridades? | muestra |
| G04 | Histograma de memoria | Distribución del uso de memoria | muestra |
| G05 | Sectores por nivel | Proporción de registros por prioridad (≤ 5 categorías) | muestra |
| G06 | Barras de eventos | Frecuencia por tipo de evento | población |
| G07 | Barras porcentuales | Estado final de las instancias por nivel | muestra |
| G08 | Cajas de CPU | Mediana, cuartiles y atípicos por nivel | muestra |
| G09 | Cajas de memoria | Igual para memoria | muestra |
| G10 | Dispersión CPU–memoria | Relación entre recursos | submuestra |
| G11 | Densidad | Forma de CPU y memoria por nivel | muestra |
| G12 | Correlación (Spearman) | Asociación entre variables numéricas | muestra |
| G13 | Serie de CPU | Evolución horaria y picos | población |
| G14 | Serie de memoria | Evolución horaria y picos | población |
| G15 | Mapa de calor día × hora | Patrón diario | población |
| G16 | Atípicos | Observaciones extremas (marcadas, no eliminadas) | muestra |
| G17 | Comparación por prioridad | Media ± IC 95 % y mediana por nivel | muestra (instancias) |
| G18 | Dispersión con tendencia | Recta log–log por nivel | submuestra |
| G19 | Hexbin | Concentración en el plano CPU–memoria | muestra |
| G20 | ECDF | Percentiles de CPU por nivel | muestra |
| G21 | Uso vs. solicitud | Sobreasignación de CPU | muestra (instancias) |
| G22 | Áreas apiladas | Aporte de cada nivel a la utilización | población |
| G23 | Perfil 24 h | Ciclo diario medio con banda P10–P90 | población |
| G24 | Duración de vida | Vida de las instancias por estado final | muestra (instancias) |

El código de cada gráfico, con su pregunta, variables e interpretación, está en
`scripts/04_graficos.R`; el informe los muestra con esa misma documentación.

## 8. Decisiones metodológicas clave

* **Tiempo.** `hora_traza = start_time / 3,6e9` (µs → horas desde el origen). Solo se derivan días y
  hora del ciclo de 24 h relativos; **no** se convierten a fechas de calendario.
* **Prioridad.** Se usa la prioridad máxima por instancia (como el cuaderno oficial).
* **Alloc sets.** Se excluyen las instancias dentro de *alloc sets* (`alloc_collection_id ≠ 0`).
* **Valores extremos.** Se marcan (Tukey sobre log10 y uso > 1) y se analizan; no se eliminan.
* **Muestra vs. población.** Las tablas `muestra_*` estiman sobre la muestra (desviación estándar
  muestral, n − 1); las `poblacion_*` vienen de agregados sobre toda la celda.
* **Modelado.** Partición cronológica; se excluyen predictores con fuga (calculados con el objetivo
  o con información futura). Las métricas solo existen si el script entrena los modelos.

## 9. Solución de problemas

* *“El directorio de trabajo debe ser la raíz del proyecto”*: abre el `.Rproj` o usa `setwd()`.
* *Error de autenticación en `bq_auth()`*: ejecuta `bigrquery::bq_deauth()` y vuelve a autorizar.
* *La ejecución en seco supera el límite*: es intencional; revisa el tamaño, usa otra celda o sube
  `max_gb_por_consulta` sabiendo el coste.
* *`geom_hex` no aparece*: instala `hexbin`.
