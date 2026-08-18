# Más allá del prestigio — pipeline de análisis

Tesis Magíster en Sociología, PUC Chile — Trajan Pirkovic Palma.
Tutora: Andrea Canales Hernández. Co-tutor: Gabriel Otero Cabrol.

Este repo contiene el pipeline de R que produce todos los indicadores y
modelos de la tesis, a partir de la encuesta Fondecyt "Class, Personal
Networks and Political Attitudes" (N=1.251, RM, 2025), la taxonomía ESCO,
ISCO-08, la tabla ISEI-08 de Ganzeboom (2010) y CASEN 2024.

## Cómo correr el pipeline completo

Los scripts son autocontenidos y **deben correrse en este orden**. Cada uno
lee sus insumos desde `output/intermediate/` y falla con un mensaje claro
(`stopifnot`) si falta algo de una etapa anterior.

| # | Script | Qué hace | Insumo previo |
|---|---|---|---|
| 01 | `01_preprocesar_encuesta.R` | Prepara la encuesta Fondecyt, arma `isco_ego4`/`isco_padre4` | — |
| 02 | `02_matriz_habilidades_L2.R` | Matriz ISCO×L2 (habilidades esenciales, ESCO) | 01 |
| 03 | `03_ca_habilidades_isei.R` | CA ponderado por población RM, ISEI de ego/origen (Ganzeboom) | 01, 02 |
| 04 | `04_indicadores_red.R` | Indicadores de red: SH_ip, SP_ip, Div_ego, Div_Red, Rango_P, shares de comunidad | 01, 02/03, 08, 10 |
| 05 | `05_modelos.R` | Modelos H1 a H4b (glmmTMB + svyglm) | 04 |
| 06 | `06_visualizaciones.R` | Forest plot, tablas H2/H3/H4, panel de distribuciones | 05 |
| 07 | `07_lectura_resultados.R` | Mapa CA, tablas de lectura por hipótesis | 03, 05 |
| 08 | `08_comunidades_por_ocupacion.R` | Detección Leiden + shares de comunidad por ocupación (RM) | 01, 02 |
| 09 | `09_tabla_grados_comunidad.R` | Grado de cada categoría dentro de su comunidad | 08 |
| 10 | `10_cierre_estructural.R` | Indicador de cierre estructural blando (H4) | 08 |
| 11 | `11_modelos_habilidades_origen.R` | Contraste total/directo H2-H3 (complementario a 05) | 04 |

`robustez/` contiene los chequeos de sensibilidad (comparación Leiden vs.
Louvain, bootstrap, nivel L3, MCA) — no forman parte del camino crítico,
pero sustentan las decisiones metodológicas documentadas en cada script
productivo.

## Hallazgo central

Louvain/Leiden sobre la red de complementariedad de habilidades (phi,
RCA>1) detecta 4 comunidades estables: **Dirección-servicio**,
**Técnico-manual**, **Analítico-digital-simbólico** y **Bio-ambiental-legal**.
Esta última es simultáneamente la más cohesionada internamente y la más
dispersa en estatus (ISEI) — evidencia estructural de que la similitud de
contenido de habilidades no es reductible a la similitud de prestigio.

## Hipótesis

- **H1 / H1b**: la similitud de habilidades ego-posición predice acquaintanceship
  en el generador de posiciones, más allá del prestigio (`05_modelos.R`, M0-M2).
- **H2**: la clase de origen predice la diversidad de habilidades de la red.
- **H3**: la composición de comunidades de la red corresponde a la de la
  ocupación propia de ego, controlando por homofilia de prestigio.
  ⚠️ *Los 4 shares de comunidad son datos composicionales (suman 1); los
  resultados actuales (4 `svyglm` paralelos) son válidos para exploración
  pero requieren regresión Dirichlet o transformación log-ratio antes de
  reportarse como finales — ver nota en `05_modelos.R`, Decisión 3.*
- **H4 / H4b**: la clase de origen predice el cierre estructural de la red,
  moderado por el peso bio-ambiental-legal de la ocupación de ego.

## Convenciones del pipeline

- Rutas absolutas fijas (`RUTA_SCRIPTS_BASE`), nunca `dirname()`.
- Todos los scripts son autocontenidos: leen de `intermediate/` y `data/`,
  nunca asumen objetos ya en memoria de otro script.
- Fail-fast: `stopifnot()` con mensajes claros al inicio de cada script,
  nunca fallos silenciosos.
- Correcciones ISCO CASEN→ESCO centralizadas en un solo archivo:
  `data/correcciones_isco_casen.csv`. Nunca se re-implementan en otro lado.
- Los IDs de comunidad de Leiden se anclan por contenido (categorías ancla
  documentadas en `08_comunidades_por_ocupacion.R`), no por el orden
  arbitrario que devuelve el algoritmo.
- Sin causal language en el texto de la tesis (diseño transversal): usar
  "se asocia con", "predice", "varía según"; nunca "efecto de", "impacto de".

## Estado del pipeline (última auditoría: 18-ago-2026)

Todos los bugs de cálculo detectados en la auditoría del 17/18-ago-2026 ya
están corregidos en los scripts de este repo (CA ponderado por RM restaurado
como fuente única, indexación de `Rango_P`/`Estatus_Max` por `match()`,
verificación de completitud de shares de red, anclaje por contenido de
comunidades, seed del bootstrap fuera del loop). El detalle completo de cada
bug y su corrección está documentado en los comentarios de cabecera y en el
bloque `DECISIONES METODOLÓGICAS` al final de cada script afectado.

Pendiente antes de redactar resultados finales de H3: resolver la
composicionalidad de los 4 shares (ver arriba).
