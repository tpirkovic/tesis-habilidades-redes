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
| [`codigo/`](codigo/) | Pipeline de análisis en R (versión de agosto), con su propio README | 20-ago-2026 |

## Compilar el artículo

Desde `articulo/`: `quarto render` (genera `_salida/tesis.pdf`). El artículo lee
cifras, tablas y figuras desde `../scripts/output_final/`, producido por el
pipeline vigente (`scripts/pipeline_final/run_pipeline.R`), por lo que antes de
compilar hay que correr ese pipeline. Detalles en
[`articulo/README.md`](articulo/README.md).

## Datos

Los microdatos de la encuesta Fondecyt (N = 1.251, RM, 2025) no se versionan
en este repositorio. Las salidas intermedias de R (`*.rds`, `output/`) tampoco:
se regeneran al correr el pipeline.
