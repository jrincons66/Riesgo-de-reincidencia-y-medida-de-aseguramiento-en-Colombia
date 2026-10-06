# Riesgo de reincidencia y medida de aseguramiento en Colombia

Construcción de un panel de capturas y actuaciones judiciales (2006–2026) y
estimación de modelos de riesgo de reincidencia, para contrastar el riesgo
predicho contra la medida de aseguramiento que efectivamente se impuso.

El repositorio tiene dos partes. `panel/` va de las descargas crudas a una base
de modelado, en Python. `modelos/` estima y evalúa los modelos sobre esa base,
en R.

> [!IMPORTANT]
> Las bases de datos no están en este repositorio y no deben subirse. Contienen
> nombres y números de cédula de 2,24 millones de personas vinculadas a procesos
> penales. Ver [Datos](#datos).

---

## Qué hay acá

```
.
├── panel/                 Python · de las descargas crudas a la base de modelado
│   ├── 0_Union_fuentes.ipynb
│   ├── 1_Extraccion_actuaciones.ipynb
│   ├── 2_Base_final.ipynb
│   ├── 3_EDA_embudo.ipynb
│   ├── 4_Tiempos_procesales.ipynb
│   ├── 5_Ventanas_reincidencia.ipynb
│   ├── 6_Historial_penas.ipynb
│   └── 7_Variable_joven.ipynb
│
└── modelos/               R · estimación y evaluación de los modelos de riesgo
    ├── 00_correr.R        orquesta la corrida completa
    ├── LEEME.md
    └── R/                 01_config … 11_robustez
```

`base_historial_escalonado.parquet`, que sale del paso 6 del panel, es la
bisagra: es lo último que escribe Python y lo primero que lee R.

## Las fuentes

Dos registros descargados a partir del número de radicado: el **registro de
capturas** y el **registro de actuaciones procesales**. Cada uno viene partido en
dos archivos porque la descarga se hizo en dos tandas.

---

## 1. `panel/` · Construcción del panel

Los scripts se corren en orden. Cada uno lee lo que dejó el anterior y guarda un
archivo que alimenta al siguiente.

| Script | Lee | Guarda |
|---|---|---|
| `0_Union_fuentes` | `capturas_with_status[2].parquet`, `actuaciones[2].parquet` | `capturas_2006_2026.parquet`, `actuaciones_2006_2026.parquet` |
| `1_Extraccion_actuaciones` | `actuaciones_2006_2026.parquet` | `resultado_variables.parquet` |
| `2_Base_final` | capturas + `resultado_variables` + `Penas_delitos_ajustado` | `base_final.parquet`, `muestra_revision.csv` |
| `3_EDA_embudo` | `base_final.parquet` | tablas y árboles del embudo procesal |
| `4_Tiempos_procesales` | `base_final.parquet` | Excel de tiempos, base ajustada |
| `5_Ventanas_reincidencia` | base ajustada | `base_con_seguimiento.parquet` |
| `6_Historial_penas` | `base_con_seguimiento.parquet` | `base_historial_escalonado.parquet` |
| `7_Variable_joven` | `base_historial_escalonado.parquet` | la misma base, con `joven` |

**`0_Union_fuentes`** pega las dos tandas de cada fuente. En capturas el año 2019
aparece en los dos archivos y la versión válida es la de la segunda tanda, así
que del primer archivo se corta todo lo que sea de 2019 en adelante y del segundo
se deja hasta julio de 2026. Las actuaciones se concatenan completas. Quedan dos
bases de 2006 a 2026, y las actuaciones suman algo más de 11 millones de filas.

**`1_Extraccion_actuaciones`** es la parte gruesa. La base de actuaciones no trae
variables, trae texto: una fila por cada anotación que el juzgado escribió en el
expediente. El script recorre esos 11 millones de filas con expresiones regulares
y devuelve una fila por radicado con las fechas de legalización de captura,
imputación, medida de aseguramiento, boleta de detención, boleta de salida,
acusación, juicio oral y sentencia, más el juzgado, el tipo de medida
(domiciliaria o intramural), el delito imputado, si hubo preacuerdo y el sentido
del fallo. Quedan cerca de 594 mil radicados con información procesal.

Dos decisiones de implementación. La búsqueda se hace sobre el texto de la
actuación y la anotación unidos y normalizados, sin tildes y en minúscula, porque
la misma información aparece en uno o en otro campo según el juzgado. Y antes de
marcar que hubo medida de aseguramiento el script revisa un listado de
expresiones de negación ("se niega la medida", "no se impone", y un centenar más)
y aísla la parte resolutiva del texto, para no leer como impuesta una medida que
el juez negó.

**`2_Base_final`** une las tres fuentes: capturas con actuaciones por radicado, y
ese resultado con la tabla de penas por artículo. La base queda en formato largo,
una fila por persona, radicado y delito: 3,7 millones de filas, 2,24 millones de
personas y 592 mil radicados. También saca una muestra de 200 filas estratificada
por tipo de sentencia y tipo de medida, que es la que se revisó a mano para
validar la extracción de texto.

**`3_EDA_embudo`** arma el embudo procesal: cuántos casos llegan a cada etapa y
cuántos se caen en el camino, sobre el total y sobre cada grupo de delito. La
unidad es el caso, o sea una persona dentro de un proceso, porque un radicado
puede cobijar a varias personas y una persona puede tener varios radicados.

**`4_Tiempos_procesales`** calcula las nueve duraciones entre etapas, de captura a
legalización, de legalización a imputación y así hasta sentencia, con mínimo,
máximo, promedio, mediana y N, para el total y para delitos contra la propiedad,
homicidios y delitos sexuales. Las duraciones negativas se excluyen, porque una
fecha final anterior a la inicial es un dato inconsistente y no una duración, y
después se excluye el 1% más alto de cada variable. Se excluye y no se trunca:
reemplazar por el valor del percentil sería inventar el dato. El umbral se
calcula una sola vez sobre la base total y se aplica igual a las cuatro tablas,
para que se midan con la misma vara. El tiempo de captura a boleta de salida se
calcula solo para los casos con medida intramural, porque en los demás no hay
reclusión de la cual salir.

**`5_Ventanas_reincidencia`** construye la variable dependiente. Para cada persona
que sale se mira una ventana de 365 días y se marca si en ese plazo aparece otro
radicado distinto a su nombre. Según cuál fecha del otro radicado caiga dentro de
la ventana salen cuatro señales: rearresto legalizado, reimputación, caso abierto
y rearresto policial.

El problema es desde cuándo contar. Cuando hay boleta de salida la ventana
arranca ahí. Cuando no la hay, porque el expediente registró la medida pero no la
salida, se imputa sumándole a la fecha de la medida el promedio de días entre
legalización y boleta de salida. Ese promedio no es uno solo para toda la base:
se calcula por celdas de casos parecidos, combinando juzgado, delito y municipio,
y a cada caso se le asigna el de su celda. Si la celda más específica no tiene
casos suficientes se baja un escalón, se suelta el juzgado y después el
municipio, hasta el promedio global que queda como último recurso. Cada caso
guarda con qué escalón se resolvió (`nivel_match`) y de qué tamaño era su celda
(`n_celda_match`), para poder auditarlo.

El script trae además un bloque de reparación de fechas imposibles, que son
errores de digitación del expediente, años como 3018 o 7201. Se corrigen dentro
del mismo radicado usando las demás fechas del proceso como ancla, y lo que no se
puede reparar queda nulo.

**`6_Historial_penas`** le pega a cada caso el historial de la persona. Hace dos
cálculos: el total acumulado de la cédula, y la suma escalonada, que recorre los
radicados en orden cronológico y a cada uno le asigna solo lo que venía de antes.
La pena de cada delito sale de la tabla de penas por artículo, y el script deja
registrada la fuente, si es del delito sentenciado o del delito de la captura,
para poder distinguirlas. Quedan 766 mil filas y 549 mil cédulas.

**`7_Variable_joven`** agrega una aproximación a la edad a partir del número de
dígitos de la cédula, que en Colombia se asigna por rangos según el año de
nacimiento.

---

## 2. `modelos/` · Modelos de riesgo

### El diseño

Se estiman **dieciséis respuestas**, que son cuatro señales de reincidencia
cruzadas con cuatro tipos de delito. Las señales vienen del paso 5 del panel
(rearresto legalizado, reimputación, caso abierto y rearresto policial) y el tipo
es el del delito en el que la persona reincidió: cualquier delito, delitos contra
la propiedad, crimen violento, y drogas y armas. Cada una se estima con dos
motores, bosque aleatorio (`ranger`) y gradient boosting (`xgboost`), para no
depender de un solo algoritmo. La ventana es de 365 días en todas.

Las de rearresto policial por tipo de delito no se pueden estimar: esa fuente
solo identifica el delito en el 63% de los casos, así que de esa señal queda
únicamente el modelo de cualquier delito.

El corte es **temporal y no aleatorio**: entrena hasta 2020 y evalúa de 2021 en
adelante. Partir al azar dejaría casos de 2024 entrenando un modelo que después
predice casos de 2019, que es justo lo contrario de lo que se quiere: el modelo
tiene que servir para decidir sobre una captura con información que ya existía en
ese momento. La validación cruzada es de origen móvil por la misma razón
(Bergmeir y Benítez, 2012). El fin de evaluación está puesto en 2026 pero el
código lo baja solo a 2025, porque con 365 días el seguimiento del último año no
alcanza a cerrar.

### Qué hace cada archivo

`00_correr.R` es el único que se ejecuta. Carga la configuración, arma la base
una vez y después recorre la corrida: por cada ventana y cada estrato entrena los
modelos, calcula las métricas y escribe una carpeta por señal de reincidencia con
sus cuatro tipos de delito adentro. Al final corre las comparativas que cruzan
señales y motores.

| Archivo | Qué hace |
|---|---|
| `R/01_config.R` | Todo lo que se toca está acá: rutas, ventana, grupos de delito, años de corte, motores, umbrales. Los demás archivos no tienen números sueltos. |
| `R/02_utilidades.R` | Mensajes de consola, carpetas de salida, caché, guardado de tablas y gráficos, y el tema de ggplot. Nada de lógica del modelo. |
| `R/03_carga.R` | Lee el parquet que dejó Python, verifica las columnas de fecha y arma el grupo de delito. No reconstruye nada de lo que ya viene hecho. |
| `R/04_historial.R` | Recalcula las variables de historial mirando **solo hacia atrás**. Es el archivo que resuelve la fuga de información; ver [Decisiones](#decisiones-que-condicionan-la-lectura). |
| `R/05_panel.R` | Cruza cada señal con el delito de la reincidencia para formar las 16 respuestas, decide qué casos son observables, arma el panel y hace la partición temporal. |
| `R/06_motores.R` | Catálogo de motores: cada uno es una entrada con su método de `caret`, su malla de hiperparámetros y su paquete. Incluye un envoltorio propio de XGBoost. Agregar un motor es agregar un elemento a la lista. |
| `R/07_entrenamiento.R` | Entrena un modelo por cada combinación de motor y respuesta, y cachea cada uno por separado para que agregar un motor no obligue a reestimar los que ya estaban. |
| `R/08_metricas.R` | AUC, AUC-PR, F1 en todos los umbrales, índice de Youden y Brier. La comparación entre motores se hace con F1 y no con exactitud, porque con clases desbalanceadas un modelo que siempre dice "no reincide" acierta el 90%. |
| `R/09_graficos.R` | Una función por salida: curvas ROC, AUC año por año, calibración por percentiles, importancia de variables, y el contraste entre la tasa de otorgamiento de medida y el riesgo predicho, con sus deciles extremos. |
| `R/10_comparativas.R` | Las tablas que cruzan: qué motor predice mejor dentro de una ventana, qué cambia entre ventanas, y el resumen por señal y por tipo de delito. |
| `R/11_robustez.R` | Vuelve a partir los datos al azar, pero sorteando por cédula y no por caso, para medir cuánto se infla el desempeño si se ignora el orden temporal. Guarda también los modelos y la sesión. |

---

## Decisiones que condicionan la lectura

**Lo otorgado, no lo solicitado.** La base registra la medida que se impuso, no la
que el fiscal pidió. Un caso sin medida puede ser uno donde no se solicitó o uno
donde el juez la negó, y la fuente no los distingue. Esto afecta la lectura del
embudo y del contraste final.

**Etiquetas selectivas.** El riesgo solo se observa en quienes salieron. De los que
quedaron detenidos no se sabe si habrían reincidido, así que el modelo se entrena
sobre una muestra seleccionada por la propia decisión que se quiere evaluar. Es
el problema de *selective labels* (Kleinberg et al., 2018). No se resuelve con
estos datos, pero acota lo que se puede concluir.

**Fuga de información.** Las variables de historial calculadas sobre toda la vida
de la persona (número de procesos, de delitos y penas totales) predicen la
reincidencia casi por definición, porque incluyen los procesos posteriores a la
captura que se está evaluando: el número de procesos por cédula promediaba 1,93
entre los no reincidentes y 6,67 entre los reincidentes. Esas columnas se
excluyen (`COLUMNAS_CON_FUGA` en la configuración) y el historial se reconstruye
estrictamente hacia atrás en `R/04_historial.R`. Con la pena pasa lo mismo: la
del delito sentenciado es información posterior a la captura, así que la tabla de
penas por artículo se armó solo con los delitos de la captura.

**Nulos y no ceros.** Los casos sin ventana de seguimiento quedan en nulo. Son los
que no tienen ni boleta de salida ni fecha de medida: si no hay desde dónde
contar los 365 días, marcarlos como cero diría que no hubo reincidencia cuando en
realidad no se pudo mirar.

**Variables del expediente, no de la persona.** En un proceso con varias personas
capturadas, las variables procesales se atribuyen a todas. Si el juez impuso
medida a uno solo de tres capturados, la base no lo distingue.

**Un nulo no siempre es una etapa que no ocurrió.** Puede haber ocurrido sin quedar
registrada, así que los porcentajes del embudo son un piso.

---

## Cómo correr

Python 3.11 con `pandas`, `numpy`, `pyarrow` y `openpyxl`; R 4.3 con `caret`,
`ranger`, `xgboost`, `pROC`, `ggplot2`, `dplyr` y `doParallel`.

Primero los notebooks de `panel/` en orden. Las rutas de entrada y salida están
al principio de cada uno, en la celda de configuración, y apuntan por defecto a
la carpeta de descargas del usuario.

Después, del lado de R, se ajusta `DESCARGAS` en `modelos/R/01_config.R` —es la
única ruta que hay que tocar, las demás se derivan de ella— y se ejecuta
`modelos/00_correr.R` completo. Los resultados quedan en `salidas_modelos`, por
fuera del repositorio.

## Datos

Las bases no están versionadas, por tamaño y porque contienen datos personales
identificables: `IDENTIFICACION`, `NOMBRES` y `APELLIDOS` de 2,24 millones de
personas vinculadas a procesos penales, con el delito y el resultado del proceso.

Dos cosas que se desprenden de eso:

1. El `.gitignore` excluye `*.parquet`, `*.csv` y `*.xlsx`. Conviene revisarlo
   antes de cada commit.
2. Los notebooks están guardados **sin outputs**, y así deben quedar. Siete
   celdas terminan en `.head()` o `.tail()`: si se corren y se comitean con los
   resultados a la vista, el `.ipynb` queda con nombres y cédulas reales dentro
   del archivo de texto. `jupyter nbconvert --clear-output --inplace panel/*.ipynb`
   antes de commitear.

## Referencias

- Bergmeir, C. y Benítez, J. M. (2012). On the use of cross-validation for time
  series predictor evaluation. *Information Sciences*, 191, 192–213.
- Kleinberg, J., Lakkaraju, H., Leskovec, J., Ludwig, J. y Mullainathan, S.
  (2018). Human decisions and machine predictions. *The Quarterly Journal of
  Economics*, 133(1), 237–293.
