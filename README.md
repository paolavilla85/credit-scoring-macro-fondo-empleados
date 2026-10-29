# Credit scoring con variables macroeconómicas en un fondo de empleados

Código en R del trabajo final de la Maestría en Analítica e Inteligencia de Negocios
(Universidad del Valle): *Evaluación del desempeño de modelos de riesgo crediticio bajo
la incorporación de variables del entorno macroeconómico en una entidad de economía
solidaria*.

El trabajo estima modelos de puntuación de crédito (credit scoring) para un fondo de
empleados colombiano y evalúa si agregar indicadores macroeconómicos rezagados
(resumidos en índices por bloque mediante análisis de componentes principales) mejora
la capacidad de predecir el incumplimiento. Parte del modelo de Bermúdez Vera,
Mosquera Restrepo y Manotas-Duque (2025), *Risks*, 13(2), 20.

## Datos

**Los datos no se incluyen en este repositorio.** La base de crédito contiene
información confidencial de los asociados del fondo de empleados. Para reproducir el
análisis, crea una carpeta `datos/` en la raíz del proyecto con:

| Archivo | Contenido |
|---|---|
| `datos/BD_CONSUMO_2020.xlsx` | Base de créditos de consumo (hoja `Data`), una fila por asociado |
| `datos/Variables macro 2005 a 2023.xlsx` | Indicadores macroeconómicos trimestrales (Banco de la República y DANE) |

Los nombres de las columnas esperadas están definidos en `codigo/00_config.R`.

## Requisitos

R 4.x con los paquetes `readxl`, `dplyr`, `tidyr`, `stringr`, `lubridate`, `pROC`,
`ggplot2`, `purrr`, `tibble`, `cluster`, `randomForest`, `xgboost`, `e1071`, `nnet` y
`rpart`. Los scripts instalan los que falten.

## Ejecución

Abre R con la carpeta raíz del proyecto como directorio de trabajo y ejecuta los scripts
en orden. Cada uno carga `codigo/00_config.R` (rutas, paquetes, semilla y funciones
comunes) y guarda sus salidas en `resultados/`.

| Script | Qué hace |
|---|---|
| `00_config.R` | Configuración: rutas, paquetes, semilla (2026), variables y funciones de modelado |
| `01_carga_validacion.R` | Carga y validación de la base |
| `02_cruce_macro.R` | Cruce de cada crédito con los indicadores macroeconómicos rezagados 3 y 6 meses |
| `03_ingenieria.R` | Índices macroeconómicos por bloque (ACP sobre trimestres únicos) y conjuntos de predictores |
| `04_modelos.R` | Razón de verosimilitud (modelos anidados), AIC/BIC y Wald de cada índice; referencia con SMOTE |
| `05_validacion.R` | 200 particiones 80/20 pareadas, validación por trimestres, DeLong, Gini y d de Cohen |
| `06_tecnicas.R` | Las seis técnicas del artículo de referencia |
| `07_benchmark.R` | Comparación con y sin SMOTE (200 particiones 70/30) |
| `08_sintonizacion.R` | Ajuste de hiperparámetros de Bosque Aleatorio y XGBoost por validación cruzada |
| `09_segmentacion.R` | Perfiles por k-medias y bandas de riesgo por quintiles del puntaje |
| `10_figuras.R` | Figuras del documento |
| `11_acp_unico.R` | Comparación de los índices por bloque con un ACP único |

Con la misma semilla y los mismos datos se obtienen las cifras, tablas y figuras del
documento.

## Autora

Karen Paola Cadena Villalobos. Directores: Iván Mauricio Bermúdez Vera y Jaime
Mosquera Restrepo.
