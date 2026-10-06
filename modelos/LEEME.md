# PRiSMA · modelos de reincidencia

Cuatro definiciones de reincidencia, cuatro motores y dos ventanas de
seguimiento, sobre `base_con_seguimiento.parquet`.

## Cómo se corre

```r
source("00_correr.R")     # desde RStudio, con el proyecto abierto acá
```

```bash
Rscript 00_correr.R       # desde la terminal, parado en esta carpeta
```

`00_correr.R` es lo único que se ejecuta. Antes de la primera corrida hay que
ajustar `DESCARGAS` en `R/01_config.R`.

## Estructura

```
PRiSMA/
├── 00_correr.R              orquestador: lo único que se ejecuta
├── LEEME.md
└── R/
    ├── 01_config.R          todos los parámetros
    ├── 02_utilidades.R      tema, guardado, caché, mensajes
    ├── 03_carga.R           lectura, verificación y grupo de delito
    ├── 04_historial.R       historial hacia atrás y predictores
    ├── 05_panel.R           respuestas por ventana y partición temporal
    ├── 06_motores.R         catálogo de motores y mallas
    ├── 07_entrenamiento.R   estimación con caché por modelo
    ├── 08_metricas.R        AUC, F1, precisión, recall, Brier
    ├── 09_graficos.R        una función por salida
    ├── 10_comparativas.R    entre motores y entre ventanas
    └── 11_robustez.R        partición aleatoria y guardado
```

## Las 16 respuestas

Cada señal de reincidencia va por separado, no unida, y cada una se cruza con
el tipo de delito en el que la persona reincidió.

|  | Cualquier delito | Propiedad | Violento | Drogas y armas |
|---|---|---|---|---|
| Rearresto judicial | ✓ | ✓ | ✓ | ✓ |
| Reimputación | ✓ | ✓ | ✓ | ✓ |
| Caso abierto | ✓ | ✓ | ✓ | ✓ |
| Rearresto policial | ✓ | ✓ | ✓ | ✓ |

Con dos motores son 32 modelos sobre una ventana de un año.

El tipo de delito sale de la siguiente captura de la misma cédula, si cae
dentro de la ventana. Es una aproximación: cada señal se dispara por un evento
distinto y pueden ser radicados diferentes, así que ese grupo es el del proceso
siguiente y no necesariamente el del evento que disparó cada marca. Para
hacerlo exacto habría que guardar en Python el radicado que disparó cada señal.
La corrida imprime, por señal, qué porcentaje de las marcas tiene delito
identificado; lo que no se identifica cuenta solo en la fila de "cualquier
delito".

## Motores

Solo `ranger` y `xgboost`, que son los que ganaron la comparación de F1.
`glmnet` y `rpart` siguen en el catálogo de `R/06_motores.R` por si hay que
rehacerla, pero no se estiman en cada corrida.

## La ventana

Solo 365 días. La de 730 está comentada en `VENTANAS`, en `R/01_config.R`:
descomentarla la vuelve a activar y el resto del pipeline la toma solo.

## Qué produce

```
salidas_modelos/ventana_365/
├── rearresto_judicial/     tabla 6 (con PRiSMA al lado), ROC, calibración,
│                           importancia y AUC anual: los 4 tipos de delito
├── reimputacion/           lo mismo para esta señal
├── caso_abierto/
├── rearresto_policial/
└── todos_los_delitos/      lo común: partición, modelos, desempeño de las 16,
                            panorama, ROC cruzado, deciles, otorgamiento,
                            errores de política, robustez
```

Cada carpeta de señal tiene los cuatro tipos de delito adentro. La de
`rearresto_judicial` es la comparable con PRiSMA y trae sus números al lado.

La tabla principal es
`ventana_365/todos_los_delitos/tablas/tabla_desempeno.csv`: 16 filas con señal,
tipo de delito, AUC, exactitud, sensibilidad, especificidad y los dos tipos de
error.

Los gráficos se facetean por señal, con las cuatro curvas de tipo de delito en
cada panel. La calibración y la importancia de variables se limitan a las
cuatro respuestas de "cualquier delito" (`RESPUESTAS_DESTACADAS`), porque 16
paneles no se leen.

## Las dos ventanas

Las respuestas ya vienen calculadas de Python: sin sufijo son las de un año y
con `_2` las de dos. Acá no se reconstruye nada, solo se elige el juego de
columnas. Las dos corridas son independientes y producen carpetas separadas.

Con datos hasta junio de 2026, la ventana de un año puede evaluar hasta 2025 y
la de dos años hasta 2024, porque el seguimiento tiene que cerrar dentro de los
datos. El ajuste es automático y queda reportado en `tabla_particion.csv`.

## Qué quedó fuera del modelo, y por qué

`TipoMedida` es la decisión que el modelo busca informar, así que no puede ser
un insumo. Entra solo en los gráficos de otorgamiento.

`n_procesos_cedula`, `n_delitos_cedula`, `penas_totales_cedula`,
`penas_acumuladas_radicado` y `delitos_acumulados_radicado` son totales de toda
la historia de la persona e incluyen los procesos posteriores a la captura.
Quien reincide tiene por definición más procesos, así que usarlas como
predictores es circular. Se recalculan hacia atrás en `04_historial.R`.

Para medir el tamaño del efecto se pueden encender con
`INCLUIR_COLUMNAS_CON_FUGA <- TRUE`, pero esa corrida sirve solo para eso.

## El caché

Cada modelo se guarda por separado, así que volver a correr solo estima lo que
falta y agregar un motor no vuelve a estimar los anteriores. El caché de la
base se invalida solo cuando cambia la fecha del parquet. El de los modelos no:
si se cambia un predictor, una malla o `ANIO_CORTE`, hay que borrarlo.

```r
limpiar_cache()                 # todo
limpiar_cache("^modelo_v730")   # solo la ventana de dos años
```

Para probar que corre de punta a punta antes de dejarlo toda la noche:
`MODO_RAPIDO <- TRUE`.

## XGBoost

El método `xgbTree` que trae caret no funciona con xgboost 2.1 en adelante:
xgboost cambió el objeto del modelo por uno que no admite asignaciones y caret
hace exactamente eso, así que sale `ALTLIST classes must provide a Set_elt
method`. `R/06_motores.R` incluye un envoltorio propio que llama a xgboost
directamente, y el motor se llama `xgboost` en vez de `xgbTree`.

Si el envoltorio diera problemas, `gbm` está en el catálogo y hace lo mismo:
cambiar `"xgboost"` por `"gbm"` en `MOTORES`.

## Paquetes

```r
install.packages(c("arrow", "dplyr", "tidyr", "readr", "stringr", "ggplot2",
                   "caret", "pROC", "glmnet", "rpart", "ranger", "xgboost",
                   "doParallel"))
```

Los motores cuyo paquete falte se saltan avisando, no tumban la corrida.
