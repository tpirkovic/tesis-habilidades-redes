# =============================================================================
# 02_matriz_habilidades_L2.R
# Etapa 2 del pipeline: matriz ISCO-08 x categoría de habilidad ESCO (Nivel 2).
# Tesis Magíster en Sociología — PUC Chile | Trajan Pirkovic Palma
#
# AUTOCONTENIDO: lee solo los CSV de ESCO. No depende de la etapa 01.
#
# ACCIONES QUE EJECUTA ESTE SCRIPT, EN ORDEN:
#   1. Lee occupationSkillRelations_es.csv (ocupación -> habilidad).
#   2. Lee occupations_es.csv y extrae el código ISCO de 4 dígitos.
#   3. Une (1) con (2) por occupationUri para tener isco4 en cada relación.
#   4. Lee broaderRelationsSkillPillar_es.csv y skillsHierarchy_es.csv.
#   5. Construye el mapeo habilidad -> grupo de habilidad (SkillGroup).
#   6. Construye el mapeo grupo -> categoría L2 (colgando de L2 o de L3).
#   7. Cruza (5) con (6) para obtener habilidad -> categoría L2 final.
#   8. Filtra solo relationType == "essential".
#   9. Cruza (8) con (7) para asignar categoría L2 a cada relación esencial.
#   10. Cuenta habilidades por ocupación×categoría y pivotea a matriz ancha.
#   11. Guarda la matriz y las tablas intermedias.
# =============================================================================

suppressPackageStartupMessages({ library(tidyverse) })

ESCO_DIR         <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data/esco"
INTERMEDIATE_DIR <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output/intermediate"
dir.create(INTERMEDIATE_DIR, showWarnings = FALSE, recursive = TRUE)

# ── ACCIÓN 1: leer relaciones ocupación-habilidad (todo como texto) ─────────
osr <- read_csv(file.path(ESCO_DIR, "occupationSkillRelations_es.csv"),
                col_types = cols(.default = "c"), show_col_types = FALSE)

# ── ACCIÓN 2: leer ocupaciones y extraer el código ISCO de 4 dígitos ────────
# iscoGroup viene como texto/numérico variable; se fuerza a entero (isco4).
# Se descartan las filas sin isco4 (ocupaciones sin grupo ISCO asignado).
occ_es <- read_csv(file.path(ESCO_DIR, "occupations_es.csv"),
                   col_types = cols(.default = "c"), show_col_types = FALSE) |>
  mutate(isco4 = as.integer(iscoGroup)) |>
  select(conceptUri, isco4) |>
  filter(!is.na(isco4))

# ── ACCIÓN 3: unir (1)+(2) por occupationUri = conceptUri ────────────────────
# many-to-many porque una misma ocupación puede aparecer en varias filas de osr
# (una por cada habilidad relacionada).
osr <- osr |> left_join(occ_es, by = c("occupationUri" = "conceptUri"),
                         relationship = "many-to-many")

# ── ACCIÓN 4: leer las tablas de jerarquía de habilidades ───────────────────
br <- read_csv(file.path(ESCO_DIR, "broaderRelationsSkillPillar_es.csv"),
               col_types = cols(.default = "c"), show_col_types = FALSE)

skill_hier <- read_csv(file.path(ESCO_DIR, "skillsHierarchy_es.csv"),
                        col_types = cols(.default = "c"), show_col_types = FALSE)

# ── ACCIÓN 5: habilidad individual -> su SkillGroup ─────────────────────────
# Se filtra broaderRelationsSkillPillar a las filas donde el concepto es una
# habilidad concreta (KnowledgeSkillCompetence) y su "broader" es un SkillGroup
# (descarta relaciones habilidad->habilidad u otras jerarquías).
skill_to_group <- br |>
  filter(conceptType == "KnowledgeSkillCompetence", broaderType == "SkillGroup") |>
  select(skill_uri = conceptUri, group_uri = broaderUri) |>
  distinct()

# ── ACCIÓN 6: SkillGroup -> categoría L2 ────────────────────────────────────
# Un grupo puede colgar directo de L2, o de L3 (que a su vez cuelga de L2).
# Se arman ambos mapeos y se apilan con bind_rows(); distinct(group_uri, ...)
# evita duplicar un mismo grupo si aparece en ambas fuentes.
group_to_L2 <- bind_rows(
  skill_hier |> filter(!is.na(`Level 3 URI`), !is.na(`Level 2 code`)) |>
    select(group_uri = `Level 3 URI`, L2_code = `Level 2 code`,
           L2_label = `Level 2 preferred term`),
  skill_hier |> filter(!is.na(`Level 2 URI`), !is.na(`Level 2 code`)) |>
    select(group_uri = `Level 2 URI`, L2_code = `Level 2 code`,
           L2_label = `Level 2 preferred term`)
) |> distinct(group_uri, .keep_all = TRUE)

# ── ACCIÓN 7: cruzar (5)+(6) para tener habilidad -> categoría L2 final ─────
skill_L2_map <- skill_to_group |>
  left_join(group_to_L2, by = "group_uri") |>
  filter(!is.na(L2_code)) |>
  distinct(skill_uri, L2_code, .keep_all = TRUE)

# ── ACCIÓN 8: filtrar SOLO relaciones "essential" ───────────────────────────
# Se descartan las relaciones "optional": el análisis se centra en el núcleo
# de habilidades que DEFINE cada ocupación, no en lo posible-pero-no-definitorio.
osr_essential <- osr |> filter(relationType == "essential", !is.na(isco4))

# ── ACCIÓN 9: asignar categoría L2 a cada relación esencial ─────────────────
osr_L2 <- osr_essential |>
  left_join(skill_L2_map, by = c("skillUri" = "skill_uri"),
            relationship = "many-to-many") |>
  filter(!is.na(L2_code))

# ── ACCIÓN 10: contar habilidades por ocupación×categoría y pivotear ───────
# count() da, para cada isco4, cuántas habilidades esenciales caen en cada
# L2_code. pivot_wider() convierte eso en una matriz: filas=ocupación,
# columnas=categoría L2, celdas=conteo (0 si no hay ninguna).
mat_wide <- osr_L2 |>
  count(isco4, L2_code, name = "n_skills") |>
  pivot_wider(names_from = L2_code, values_from = n_skills, values_fill = 0) |>
  column_to_rownames("isco4")

cat("Matriz ISCO × L2:", nrow(mat_wide), "×", ncol(mat_wide), "\n")

# ── ACCIÓN 11: guardar la matriz y las tablas intermedias ───────────────────
# osr_L2 y osr_essential se guardan también porque la etapa 04 los necesita
# para calcular Div_ego y Div_Red (diversidad de habilidades).
saveRDS(mat_wide, file.path(INTERMEDIATE_DIR, "matriz_isco_L2.rds"))
saveRDS(osr_L2,   file.path(INTERMEDIATE_DIR, "osr_L2.rds"))
saveRDS(osr_essential, file.path(INTERMEDIATE_DIR, "osr_essential.rds"))

cat("Guardado: intermediate/matriz_isco_L2.rds, osr_L2.rds, osr_essential.rds\n")
cat("=== FIN ETAPA 02 ===\n")
