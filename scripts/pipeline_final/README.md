# Pipeline final de la tesis

Versión del 9 de octubre de 2026, con las figuras y la paleta actualizadas el 12 de octubre. Reemplaza, para el artículo, las cadenas `codigo/`, `codigo_v2/` y `paralelo_v3/`, que siguen en su lugar sin cambios. Las salidas van a `output_final/` y el pipeline no escribe nada en `data/`, `output_mapha/` ni `output_paralelo_v3/`.

## Cómo correrlo

```r
setwd("/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/pipeline_final")
source("run_pipeline.R")
```

Para regenerar solo las figuras (por ejemplo, tras cambiar el tema gráfico), fijar `SOLO_FIGURAS <- TRUE` en `run_pipeline.R`: corre `07` y `08` sobre las salidas ya guardadas.

Las rutas son relativas a la carpeta `pipeline_final`, así que basta copiar `pipeline_final/` y `data/` a otro computador para reproducir los resultados.

## Insumos externos (lo único que el pipeline lee fuera de sí mismo)

| Archivo en `data/` | Fuente |
|---|---|
| `fondecyt/data_fondecyt.xlsx` | Encuesta *Class, Personal Networks and Political Attitudes* (Fondecyt), con las variables `isco_mainjob`, `class16_r` y `weight_m` construidas por el equipo |
| `casen/casen_2024.RData` | CASEN 2024, microdatos públicos (Ministerio de Desarrollo Social y Familia) |
| `esco/broaderRelationsSkillPillar_es.csv`, `esco/skillsHierarchy_es.csv` | ESCO, descarga oficial en español |
| `mapha/v206/` (`occupations.csv`, `occupationsSkills.csv`, `skills.csv`, `skillsHierarchy.csv`) | MAPHA v2.06 |
| `isei/isei08_ganzeboom2010.csv` | ISEI-08 de Ganzeboom (2010), tabla de DIGCLASS en enteros; `02` la verifica contra DIGCLASS |

Las decisiones de codificación que antes vivían en archivos sueltos están escritas en el código: las correcciones ISCO en `00_config.R` (`CORRECCIONES_ISCO`) y las 27 posiciones en `02` (`POSICIONES`). Las comparaciones con `output_mapha/` y `output_paralelo_v3/` son solo validaciones; con `VALIDAR_CONTRA_ANTERIOR <- FALSE` el pipeline no lee nada fuera de `data/` y `output_final/`.

## Nota metodológica: TENS

La ocupación 3221 (técnicos en enfermería, TENS) no tiene habilidades registradas en ESCO ni en MAPHA. Tanto a los encuestados TENS como a la posición Q0116 se les imputa el perfil de habilidades de 5321 (auxiliares de enfermería). Su clase (semiprofesionales socioculturales) y su ISEI (48) son los de 3221. Esto se declara en la sección de métodos.

Todo lo que se imprime queda en `output_final/log_pipeline.txt`. Cada script lee sus insumos de `output_final/intermedios/` y los escribe ahí, así que se puede correr cualquiera por separado una vez que corrieron los anteriores.

Paquetes necesarios: tidyverse, readxl, haven, survey, igraph, ggraph, tidygraph, ggrepel, ragg y DIGCLASS (`remotes::install_github("cimentadaj/DIGCLASS")`). glmmTMB es opcional y solo se usa en la robustez R7.

## Scripts

| Script | Qué hace | Reemplaza a |
|---|---|---|
| `00_config.R` | Rutas, parámetros, decisiones y utilidades; carga el tema gráfico | encabezados repetidos en cada script |
| `00_tema_graficos.R` | Estilo gráfico común (`theme_tesis()`, paleta Navy-carmesí, estructura BeautifulFigures, `guardar_fig()`) | `codigo_v2/000_tema_tesis.R` |
| `01_espacio_habilidades.R` | Universo de la RM (CASEN 2024), matriz MAPHA, B (RCA > 1), red φ, Leiden, distancias | `codigo/00`, `codigo_v2/02`, `codigo_v2/08`, sección 2 de `112` |
| `02_crosswalk_posiciones.R` | 27 posiciones: ISCO, código de habilidades, ISEI, clase, tamaño | `111` |
| `03_base_analitica.R` | Clase e ISEI de ego, díadas, indicadores de red, muestra analítica | `codigo_v2/01`, parte de `codigo_v2/04`, `107` (no está en la carpeta), `112` |
| `04_descriptivos.R` | Tabla 2, quién conoce a quién, medidas por clase, 27 posiciones, trabajo y red | `120`, `121`, `122`, sección 2 de `128` |
| `05_modelos.R` | OE1, M1 a M5 (H1, H2 y H3), pendientes por clase, robustez R1 a R7 | `113`, `114`, `123`, `128` |
| `06_linea_base.R` | Línea base estructural N3 | `116`, `117`, `119`, sección 6 de `128` |
| `07_figuras_articulo.R` | Figuras del artículo (prefijo `art_`) desde las salidas guardadas, incluido el mapa de habilidades y su variante con las 27 posiciones; no reestima | — |
| `08_grafo_bimodal.R` | Redes bimodales ocupación-categoría: ejemplo (5 posiciones), 27 posiciones del GP (red y dos filas), seis paneles sobre el mapa, universo completo y su matriz | — |
| `run_pipeline.R` | Corre todo (01 a 08) y guarda el registro | `run_paralelo.R` |

Quedan fuera del artículo y, por lo tanto, del pipeline: el CA y el MCA (`03`, `R3`), la comparación con ESCO (`codigo/`), la diversidad de habilidades, el modelo de ingreso (`126`), la grilla exploratoria (`115`), las figuras conceptuales de la presentación (`127`) y las particiones EGP y terciles (`97`).

## Colores

Desde la noche del 12 de octubre de 2026, todas las figuras usan la paleta "Navy-carmesí" (Coolors, tendencias) sobre una estructura inspirada en BeautifulFigures: marco completo, grilla tenue, leyenda arriba. Las comunidades de habilidades tienen el mismo color en todas las figuras (`PALETA_COM`):

| Comunidad | Color |
|---|---|
| Gestión y cuidado | Celeste `#669bbc` |
| Técnico-material | Carmesí `#c1121f` |
| Analítico-simbólico | Azul marino `#003049` |
| Bio-ambiental-legal | Naranja `#ee9b00` (Golden Orange; puntos con borde azul marino) |

Azul marino para el foco del análisis, carmesí para contrastes (por ejemplo, clases manuales), celeste para apoyo, fondos y línea en cero, crema `#fdf0d5` como punto medio de las matrices divergentes. En las redes bimodales, las ocupaciones van en azul marino. Celeste y azul marino se distinguen por luminosidad; el naranja, por tono. La versión anterior (Kit Digital UC) quedó respaldada en `_respaldo_2026-10-12b/`.

## Interruptor principal

`ISEI_CON_CODIGO_ORIGINAL <- TRUE` (en `00_config.R`). El ISEI y la clase de ego se calculan con el código del equipo Fondecyt, y el código corregido (3221 a 5321) se usa solo para las habilidades. Con `FALSE` se reproducen las cifras del 7 de octubre. Ver `AUDITORIA_2026-10-09.md`.

## Validaciones incorporadas

Cada script imprime líneas `[OK]` o `[DIFIERE]` al compararse con el pipeline anterior:
- modularidad (0,388) y partición de `codigo_v2/08`;
- posiciones por clase (5, 1, 6, 3, 1, 6, 5);
- muestra (971), similitud de habilidades y homogeneidad, idénticas a `112`;
- H1 (0,402 y 7,3 pp);
- vínculos de la red bimodal completa iguales a las celdas de B (6.831) y categorías por posición iguales a `02` (en `08`).

Si alguna dice DIFIERE, hay que revisarla antes de usar los resultados.
