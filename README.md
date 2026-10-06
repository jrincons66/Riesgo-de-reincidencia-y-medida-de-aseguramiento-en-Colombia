# Riesgo de reincidencia y medida de aseguramiento en Colombia

Construcción de un panel de capturas y actuaciones judiciales (2006–2026) y
estimación de modelos de riesgo de reincidencia, para contrastar el riesgo
predicho contra la medida de aseguramiento que efectivamente se impuso.

> [!IMPORTANT]
> Las bases de datos no están en este repositorio y no deben subirse. Contienen
> nombres y números de cédula de 2,24 millones de personas vinculadas a procesos
> penales. Ver [Datos](#datos).

---

## Qué hay acá

```
.
├── panel/        notebooks de Python: de las descargas crudas a la base de modelado
└── modelos/      proyecto de R: estimación y evaluación de los modelos de riesgo
```

Los scripts de `panel/` se corren en orden. Cada uno lee lo que dejó el anterior
y guarda un archivo que alimenta al siguiente.

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

`base_historial_escalonado.parquet` es la base que entra a los modelos.

## Las fuentes

Dos registros descargados a partir del número de radicado: el **registro de
capturas** y el **registro de actuaciones procesales**. Cada uno viene partido en
dos archivos porque la descarga se hizo en dos tandas.

---

## 1. Construcción del panel

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
La escalonada es la que entra al modelo, porque el total de toda la vida incluye
lo que pasó después de la captura y sería información del futuro. La pena de cada
delito sale de la tabla de penas por artículo, y el script deja registrada la
fuente, si es del delito sentenciado o del delito de la captura, para poder
distinguirlas. Quedan 766 mil filas y 549 mil cédulas.

**`7_Variable_joven`** agrega una aproximación a la edad a partir del número de
dígitos de la cédula, que en Colombia se asigna por rangos según el año de
nacimiento.

---

## 2. Modelos de riesgo

El proyecto de R lee `base_historial_escalonado.parquet` y está partido en pasos
numerados, con `00_` como punto de entrada y una *run directory*: cada corrida
guarda sus propios modelos, tablas y gráficos con su fecha, así que se puede
volver a una corrida anterior sin reestimar todo.

**Qué se estima.** Dieciséis modelos, que son cuatro señales de reincidencia por
cuatro tipos de delito. Las señales son las del paso 5: rearresto legalizado,
reimputación, caso abierto y rearresto policial. Los tipos de delito son
cualquier delito, delitos contra la propiedad, crimen violento, y drogas y armas.
Cada combinación se estima con dos motores, bosque aleatorio (`ranger`) y
gradient boosting (`xgboost`), para tener un contraste de desempeño y no depender
de un solo algoritmo. La ventana es de 365 días en todos.

De las dieciséis combinaciones, las de rearresto policial por tipo de delito no
se pueden estimar: esa fuente solo identifica el delito en el 63% de los casos,
así que de esa señal queda únicamente el modelo de cualquier delito.

**Partición temporal y no aleatoria.** El entrenamiento va hasta 2020 y la
evaluación de 2021 a 2026. Partir al azar dejaría casos de 2024 entrenando un
modelo que después predice casos de 2019, que es justo lo que no se quiere: el
modelo tiene que servir para decidir sobre una captura con información que ya
existía en ese momento. La validación cruzada es de origen móvil por la misma
razón (Bergmeir y Benítez, 2012).

**Métricas.** Para cada modelo: AUC, AUC-PR, F1 en el umbral de 0,5 y el F1
máximo con el umbral que lo alcanza, índice de Youden, Brier, y la calibración
por percentiles de riesgo. Como la reincidencia es un evento poco frecuente, el
AUC-PR y la calibración dicen más que el AUC solo, y los modelos van con pesos de
clase. Sale también el AUC año por año y la importancia de variables ordenada de
mayor a menor, por señal y tipo de delito.

**El contraste.** El gráfico que cierra el argumento es la distribución de la tasa
de otorgamiento de medida de aseguramiento contra el riesgo predicho. Ahí se ve
si la decisión que efectivamente se tomó se ordena o no por el riesgo estimado,
que es la pregunta de la tesis.

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
excluyeron y el historial se reconstruyó estrictamente hacia atrás, que es la
suma escalonada del paso 6. Con la pena pasa lo mismo: la del delito sentenciado
es información posterior a la captura, así que la tabla de penas por artículo se
armó solo con los delitos de la captura.

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

Las rutas de entrada y salida están al principio de cada notebook, en la celda de
configuración, y apuntan por defecto a la carpeta de descargas del usuario
(`Path.home() / "Downloads"`). Hay que ajustarlas a donde estén los archivos.
Después se corren los notebooks de `panel/` en orden y por último el `00_` de
`modelos/`.

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
