# =============================================================================
# 00_extraer_ocupaciones_rm_casen.R
# PASO 0 del pipeline: extrae del microdato crudo de CASEN 2024 el listado y
# frecuencia expandida de ocupaciones (CIUO-08 a 4 dígitos) en la Región
# Metropolitana. Produce ocupaciones_rm_casen2024.csv, insumo que leen
# 03_ca_habilidades_isei.R (Acción 1) y 08_comunidades_por_ocupacion.R
# (Acción 1) para construir el universo ocupacional RM.
# Tesis Magíster en Sociología — PUC Chile | Trajan Pirkovic Palma
#
# Variables usadas (según Libro_de_codigos_Casen_2024.xlsx, hoja "O" y "Factor"):
#   region      -> 13 = Región Metropolitana de Santiago
#   activ       -> condición de actividad: 1 Ocupados, 2 Desocupados, 3 Inactivos.
#                  oficio4_08 sólo existe para activ == 1 (a diferencia de la
#                  ENE, CASEN no trae ocupación para desocupados/inactivos).
#   oficio4_08  -> Oficio (4 dígitos) CIUO-08, haven_labelled: cada código trae
#                  su glosa (descriptor) incorporada, ej. 2310 = "Profesores de
#                  la educación superior". No requiere el Anexo 1 del libro de
#                  códigos como tabla de búsqueda aparte.
#   expr        -> factor de expansión regional (raking, Censo 2017).
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(labelled)
})

DATA_PATH   <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data/casen/casen_2024.RData"
OUTPUT_PATH <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output/ocupaciones_rm_casen2024.csv"

load(DATA_PATH)  # objeto: casen_2024

# Ocupados/as (activ == 1) de la Región Metropolitana (region == 13).
ocupados_rm <- casen_2024 |>
  filter(to_character(region) == "Región Metropolitana de Santiago", to_character(activ) == "Ocupados") |>
  mutate(
    oficio_codigo    = as.integer(oficio4_08),
    oficio_descriptor = as.character(as_factor(oficio4_08))
  )

tabla_ocupaciones <- ocupados_rm |>
  count(oficio_codigo, oficio_descriptor, wt = expr, name = "personas_expandidas") |>
  mutate(porcentaje = 100 * personas_expandidas / sum(personas_expandidas)) |>
  arrange(desc(personas_expandidas))

cat("Ocupaciones distintas (CIUO-08, 4 dígitos) presentes en la RM:", nrow(tabla_ocupaciones), "\n")
print(tabla_ocupaciones, n = Inf)
if (interactive()) View(tabla_ocupaciones)

write_csv(tabla_ocupaciones, OUTPUT_PATH)
cat("Guardado:", OUTPUT_PATH, "\n")
