# Habilidades laborales, clase y estatus en el cierre social de las redes personales

**Reproducción de la desigualdad social en la Región Metropolitana**

Tesis de Magíster en Sociología, Pontificia Universidad Católica de Chile.
Trajan Pirkovic Palma. Profesora guía: Andrea Canales Hernández. Cotutor: Gabriel Otero Cabrol.
Fondecyt de Iniciación N° 11240249, *Class, personal networks and political attitudes*.

## Última versión

📄 **[Informe de avance, 13 de octubre de 2026 (PDF)](articulo/Pirkovic_tesis_avance_2026-10-13.pdf)**

Fuente Quarto del artículo en [`articulo/`](articulo/). El registro de cambios
respecto de la versión anterior está en
[`articulo/CAMBIOS_2026-10-12.md`](articulo/CAMBIOS_2026-10-12.md).

## Estructura del repositorio

| Carpeta | Contenido | Fecha |
|---|---|---|
| [`articulo/`](articulo/) | Manuscrito (Quarto + LaTeX): capítulos, bibliografía, preámbulo, cifras en R y PDF compilado | 13-oct-2026 |
| [`presentaciones/pres_comision_2026-10-07/`](presentaciones/pres_comision_2026-10-07/) | Presentación de avances ante la comisión | 07-oct-2026 |
| [`presentaciones/pres_comision_2026-08-19/`](presentaciones/pres_comision_2026-08-19/) | Presentación de avance ante la comisión y guía de estudio H1/H1b | 19-ago-2026 |
| [`scripts/pipeline_final/`](scripts/pipeline_final/) | Pipeline de análisis en R vigente (01 a 08 + `run_pipeline.R`), con README y auditoría | 12-oct-2026 |
| [`scripts/output_final/`](scripts/output_final/) | Salidas agregadas del pipeline que lee el artículo: tablas, figuras `art_*` y registro de la corrida | 12-oct-2026 |

## Reproducir

1. **Pipeline.** Copiar los insumos externos a `scripts/data/` (ver
   [`scripts/pipeline_final/README.md`](scripts/pipeline_final/README.md)) y, en R,
   `source("scripts/pipeline_final/run_pipeline.R")`. Las salidas quedan en
   `scripts/output_final/`.
2. **Artículo.** Desde `articulo/`: `quarto render` (genera `_salida/tesis.pdf`).
   Lee cifras, tablas y figuras desde `../scripts/output_final/`; como esas
   salidas están versionadas, el artículo compila sin volver a correr el pipeline.
   Detalles en [`articulo/README.md`](articulo/README.md).

Versiones anteriores del código: la rama
[`pipeline-agosto-2026`](../../tree/pipeline-agosto-2026) conserva la cadena
`codigo/` (01 a 11 y robustez) usada hasta la presentación del 7 de octubre.

## Datos

Los microdatos de la encuesta Fondecyt (N = 1.251, RM, 2025) y los demás
insumos de `scripts/data/` no se versionan en este repositorio. Tampoco las
bases a nivel de encuestado (`scripts/output_final/intermedios/`, `*.rds`): se
regeneran al correr el pipeline. Lo versionado en `output_final/` son solo
resultados agregados.
