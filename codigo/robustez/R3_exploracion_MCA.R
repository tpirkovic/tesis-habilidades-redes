# =============================================================================
# exploracion_MCA_completa.R
# Validez divergente: MCA sobre el Generador de Posiciones (a la Alecu et al.
# 2022) vs. CA de habilidades — objetivo 1.3 de P1 ("más allá del prestigio").
# Tesis Magíster en Sociología — PUC Chile | Trajan Pirkovic Palma
#
# Script AUTOCONTENIDO: fusiona y reemplaza a exploracion_MCA_lazos_v2.R +
# exploracion_correlatos_MCA_1.R + exploracion_ajuste_TamGrupo.R (consolidado
# el 20-jul-2026). No depende de ningún objeto en sesión de otro script: lee
# la encuesta, ESCO, isei08_ganzeboom2010.csv, gp_crosswalk.csv y (si existe)
# tam_grupo_p_casen2024_RM.csv directamente desde disco, y recalcula su propia
# CA de habilidades igual como hacen deteccion_comunidades_habilidades.R y
# deteccion_comunidades_L3.R — así cada script de evidencia es reproducible
# de forma independiente.
#
# QUÉ HACE:
#   PASO 1  → Reconstruir CA de habilidades (ISCO×L2) y gp_coords/isei, igual
#             que en el pipeline principal, pero desde cero.
#   PASO 2  → Reconstruir isei_ego_hat por ego (join Ganzeboom + respaldo).
#   PASO 3  → Recodificación adaptativa del GP y MCA (Alecu et al. 2022).
#   PASO 4  → eta² por variable + comparación a nivel EGO (CA vs. MCA).
#   PASO 5  → Correlatos de MCA_Dim1 (Ext, ISEI, educ, etc.) — regresión.
#   PASO 6  → Ext_ajustado por TamGrupo_p (CASEN) — ¿el dominio de Ext es un
#             artefacto de prevalencia poblacional? (opcional, con respaldo
#             gracioso si tam_grupo_p_casen2024_RM.csv no está disponible).
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse); library(readxl); library(haven); library(labelled)
  library(FactoMineR)
})
set.seed(2025)

FONDECYT_DIR <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data/fondecyt"
ESCO_DIR     <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data/esco"
DATA_DIR     <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data"
OUT_DIR      <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output"

# =============================================================================
# PASO 1 — CA de habilidades (ISCO×L2), reconstruido desde cero
# =============================================================================
cat("=== PASO 1: CA de habilidades ===\n")

osr <- read_csv(file.path(ESCO_DIR, "occupationSkillRelations_es.csv"),
                col_types = cols(.default = "c"), show_col_types = FALSE)
occ_es <- read_csv(file.path(ESCO_DIR, "occupations_es.csv"),
                   col_types = cols(.default = "c"), show_col_types = FALSE) |>
  mutate(isco4 = as.integer(iscoGroup)) |> select(conceptUri, isco4) |> filter(!is.na(isco4))
osr <- osr |> left_join(occ_es, by = c("occupationUri" = "conceptUri"), relationship = "many-to-many")

br         <- read_csv(file.path(ESCO_DIR, "broaderRelationsSkillPillar_es.csv"),
                        col_types = cols(.default = "c"), show_col_types = FALSE)
skill_hier <- read_csv(file.path(ESCO_DIR, "skillsHierarchy_es.csv"),
                        col_types = cols(.default = "c"), show_col_types = FALSE)

skill_to_group <- br |> filter(conceptType == "KnowledgeSkillCompetence", broaderType == "SkillGroup") |>
  select(skill_uri = conceptUri, group_uri = broaderUri) |> distinct()
group_to_L2 <- bind_rows(
  skill_hier |> filter(!is.na(`Level 3 URI`), !is.na(`Level 2 code`)) |>
    select(group_uri = `Level 3 URI`, L2_code = `Level 2 code`),
  skill_hier |> filter(!is.na(`Level 2 URI`), !is.na(`Level 2 code`)) |>
    select(group_uri = `Level 2 URI`, L2_code = `Level 2 code`)
) |> distinct(group_uri, .keep_all = TRUE)
skill_L2_map <- skill_to_group |> left_join(group_to_L2, by = "group_uri") |>
  filter(!is.na(L2_code)) |> distinct(skill_uri, L2_code, .keep_all = TRUE)

osr_essential <- osr |> filter(relationType == "essential", !is.na(isco4))
osr_L2 <- osr_essential |> left_join(skill_L2_map, by = c("skillUri" = "skill_uri"),
                                      relationship = "many-to-many") |> filter(!is.na(L2_code))

mat_wide <- osr_L2 |> count(isco4, L2_code, name = "n_skills") |>
  pivot_wider(names_from = L2_code, values_from = n_skills, values_fill = 0) |>
  column_to_rownames("isco4")
mat_ca <- mat_wide[rowSums(mat_wide) > 0, colSums(mat_wide) > 0]
ca_result <- CA(mat_ca, ncp = 5, graph = FALSE)

ca_coords <- as.data.frame(ca_result$row$coord) |> rownames_to_column("isco4") |>
  mutate(isco4 = as.integer(isco4)) |> rename(Dim1 = `Dim 1`, Dim2 = `Dim 2`)

gp_crosswalk <- read_csv(file.path(DATA_DIR, "gp_crosswalk.csv"), show_col_types = FALSE)
gp_coords <- gp_crosswalk |> left_join(ca_coords |> select(isco4, Dim1, Dim2), by = "isco4") |>
  rename(Dim1_p = Dim1, Dim2_p = Dim2)
cat("Posiciones GP con coords CA:", sum(!is.na(gp_coords$Dim1_p)), "de 27\n")

# =============================================================================
# PASO 2 — Preprocesar encuesta + ISEI de ego (join Ganzeboom + respaldo)
# =============================================================================
cat("\n=== PASO 2: Preprocesar encuesta e ISEI de ego ===\n")

gp_vars <- paste0("Q0", sprintf("%03d", 101:127))
raw <- read_excel(file.path(FONDECYT_DIR, "data_fondecyt.xlsx"), sheet = "datos") |>
  mutate(across(where(is.labelled), as_factor)) |>
  mutate(
    educ = NA_integer_,   # no se usa educación ordinal detallada acá; ver 01_preprocesar_encuesta.R
    sexo = as.character(Q68),
    edad = as.numeric(Q70),
    isco_ego  = as.integer(isco_mainjob),
    isco_ego  = if_else(is.na(isco_ego), as.integer(Q4402), isco_ego),
    isco_ego4 = as.integer(substr(as.character(isco_ego), 1, 4))
  )
raw <- raw |> filter(!is.na(isco_ego4))

isei08_path <- file.path(ESCO_DIR, "isei08_ganzeboom2010.csv")
if (!file.exists(isei08_path)) isei08_path <- file.path(FONDECYT_DIR, "isei08_ganzeboom2010.csv")
isei08 <- read_csv(isei08_path, col_types = cols(isco08 = "c", isei08 = "d")) |> mutate(isco08 = as.integer(isco08))

isei_from_dim1 <- lm(isei ~ Dim1_p, data = gp_coords)

df_ego_isei <- raw |>
  select(ID, isco_ego4, sexo, edad) |>
  left_join(ca_coords |> rename(isco_ego4 = isco4), by = "isco_ego4") |>
  rename(Dim1_ego = Dim1, Comp_ego = Dim2) |>
  left_join(isei08 |> rename(isco_ego4 = isco08, isei_directo = isei08), by = "isco_ego4") |>
  mutate(
    isei_regresion = predict(isei_from_dim1, newdata = data.frame(Dim1_p = Dim1_ego)),
    isei_ego_hat    = coalesce(isei_directo, isei_regresion)
  )
cat("ISEI de ego — join directo:", sum(!is.na(df_ego_isei$isei_directo)),
    "| respaldo regresión:", sum(is.na(df_ego_isei$isei_directo) & !is.na(df_ego_isei$isei_regresion)), "\n")

# =============================================================================
# PASO 3 — Recodificación adaptativa del GP + MCA (Alecu et al. 2022)
# =============================================================================
cat("\n=== PASO 3: MCA sobre el Generador de Posiciones ===\n")

recode_adaptativo <- function(x) {
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  positivos <- x_num[x_num >= 1 & !is.na(x_num)]
  med <- if (length(positivos) < 10) 0 else median(positivos, na.rm = TRUE)
  categ <- case_when(
    x_num == 0                ~ "No conoce",
    x_num >= 1 & x_num <= med ~ "Conoce bajo",
    x_num > med                ~ "Conoce alto",
    TRUE                       ~ NA_character_
  )
  factor(categ, levels = c("No conoce", "Conoce bajo", "Conoce alto"))
}

mca_input <- raw |> mutate(across(all_of(gp_vars), recode_adaptativo, .names = "{.col}_mca")) |>
  select(ID, all_of(paste0(gp_vars, "_mca")))
names(mca_input)[-1] <- gp_vars

frecuencias <- mca_input |> select(-ID) |>
  pivot_longer(everything(), names_to = "posicion", values_to = "categoria") |>
  filter(!is.na(categoria)) |> count(posicion, categoria) |>
  group_by(posicion) |> mutate(pct = round(100 * n / sum(n), 1)) |> ungroup()

bajo_umbral <- frecuencias |> filter(pct < 5) |> arrange(pct)
vars_suplementarias <- unique(bajo_umbral$posicion)
vars_activas <- setdiff(gp_vars, vars_suplementarias)
cat("Variables activas:", length(vars_activas), "| suplementarias:", length(vars_suplementarias), "\n")

mca_activos <- mca_input |> filter(rowSums(is.na(across(all_of(vars_activas)))) <= 3)
cat("Egos retenidos para MCA:", nrow(mca_activos), "de", nrow(mca_input), "\n")

mca_df  <- mca_activos |> select(-ID) |> as.data.frame()
idx_sup <- match(vars_suplementarias, names(mca_df)); idx_sup <- idx_sup[!is.na(idx_sup)]
mca_result <- if (length(idx_sup) > 0) {
  MCA(mca_df, ncp = 5, quali.sup = idx_sup, graph = FALSE, na.method = "NA")
} else {
  MCA(mca_df, ncp = 5, graph = FALSE, na.method = "NA")
}

eig_tabla <- as.data.frame(mca_result$eig)
Q <- length(vars_activas)
eig_tabla$benzecri <- ifelse(eig_tabla$eigenvalue > 1/Q,
                              ((Q/(Q-1)) * (eig_tabla$eigenvalue - 1/Q))^2, 0)
eig_tabla$benzecri_pct <- 100 * eig_tabla$benzecri / sum(eig_tabla$benzecri)
cat("Varianza MCA (Benzécri) — Dim1:", round(eig_tabla$benzecri_pct[1], 1), "%\n")

mca_coords_ego <- as.data.frame(mca_result$ind$coord) |>
  bind_cols(ID = mca_activos$ID) |> select(ID, MCA_Dim1 = `Dim 1`, MCA_Dim2 = `Dim 2`)

# =============================================================================
# PASO 4 — Comparación a nivel EGO: CA de habilidades vs. MCA de lazos
# =============================================================================
cat("\n=== PASO 4: Comparación a nivel ego (CA vs. MCA) ===\n")

comparacion_ego <- df_ego_isei |> select(ID, Dim1_ego, Comp_ego, isei_ego_hat) |>
  inner_join(mca_coords_ego, by = "ID")

cat("Correlación ISEI ego vs. MCA_Dim1:",
    round(cor(comparacion_ego$isei_ego_hat, comparacion_ego$MCA_Dim1, use = "complete.obs"), 3), "\n")
cat("Correlación Dim1_ego (habilidades) vs. MCA_Dim1:",
    round(cor(comparacion_ego$Dim1_ego, comparacion_ego$MCA_Dim1, use = "complete.obs"), 3), "\n")
cat("Correlación Comp_ego (habilidades, orientación sectorial) vs. MCA_Dim2:",
    round(cor(comparacion_ego$Comp_ego, comparacion_ego$MCA_Dim2, use = "complete.obs"), 3), "\n")

write_csv(comparacion_ego, file.path(OUT_DIR, "comparacion_ego_CA_vs_MCA.csv"))

# =============================================================================
# PASO 5 — Correlatos de MCA_Dim1: ¿qué lo organiza si no son las habilidades?
# =============================================================================
cat("\n=== PASO 5: Correlatos de MCA_Dim1 ===\n")

gp_r_vars_local <- paste0(gp_vars, "_recL")
recode_gp <- function(x) {
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  case_when(x_num == 0 ~ 0L, x_num == 1 ~ 1L, x_num >= 2 & x_num <= 4 ~ 3L,
            x_num >= 5 & x_num <= 7 ~ 6L, x_num >= 8 & x_num <= 10 ~ 9L,
            x_num >= 11 & x_num <= 15 ~ 13L, x_num >= 16 ~ 16L, TRUE ~ NA_integer_)
}
raw_r <- raw |> mutate(across(all_of(gp_vars), recode_gp, .names = "{.col}_recL"))
gp_matrix_r <- raw_r |> select(all_of(gp_r_vars_local)) |> as.matrix()
raw_r$Ext <- rowSums(gp_matrix_r, na.rm = TRUE)

calc_isei_stats <- function(row_counts) {
  conocidos <- row_counts > 0 & !is.na(row_counts)
  if (sum(conocidos) == 0) return(c(rango = NA_real_, max_isei = NA_real_))
  iseis <- gp_crosswalk$isei[conocidos]
  c(rango = max(iseis) - min(iseis), max_isei = max(iseis))
}
isei_stats     <- t(apply(gp_matrix_r, 1, calc_isei_stats))
raw_r$Rango_P     <- isei_stats[, "rango"]
raw_r$Estatus_Max <- isei_stats[, "max_isei"]

correlatos <- df_ego_isei |> select(ID, sexo, edad, isei_ego_hat) |>
  left_join(raw_r |> select(ID, Ext, Rango_P, Estatus_Max), by = "ID") |>
  inner_join(mca_coords_ego, by = "ID") |>
  mutate(sexo_num = if_else(sexo == "Hombre", 1L, 0L))

vars_candidatas <- c("Ext", "isei_ego_hat", "Rango_P", "Estatus_Max", "edad", "sexo_num")
correlatos_sc <- correlatos |> mutate(across(all_of(vars_candidatas), ~ as.numeric(scale(.))))
m_dim1 <- lm(MCA_Dim1 ~ Ext + isei_ego_hat + Rango_P + Estatus_Max + edad + sexo_num, data = correlatos_sc)
cat("R² MCA_Dim1 ~ correlatos:", round(summary(m_dim1)$r.squared, 3), "\n")
print(round(summary(m_dim1)$coefficients, 3))

# =============================================================================
# PASO 6 — Ext_ajustado por TamGrupo_p (CASEN): ¿artefacto de prevalencia?
# =============================================================================
cat("\n=== PASO 6: Ext_ajustado por TamGrupo_p ===\n")

tam_path <- file.path(DATA_DIR, "tam_grupo_p_casen2024_RM.csv")
if (file.exists(tam_path)) {
  tam_grupo_p <- read_csv(tam_path, show_col_types = FALSE)
  gp_tam <- gp_crosswalk |> left_join(tam_grupo_p |> select(isco4, TamGrupo_p), by = "isco4") |>
    mutate(peso_raro = -log(TamGrupo_p))

  base_ext <- raw_r |> select(ID, all_of(gp_r_vars_local)) |>
    pivot_longer(cols = all_of(gp_r_vars_local), names_to = "gp_var", values_to = "n_conocidos") |>
    mutate(var_orig = str_remove(gp_var, "_recL")) |>
    left_join(gp_tam |> select(var, peso_raro), by = c("var_orig" = "var")) |>
    group_by(ID) |>
    summarise(Ext_ajustado = sum(n_conocidos * peso_raro, na.rm = TRUE), .groups = "drop")

  correlatos_v2 <- correlatos |> left_join(base_ext, by = "ID")
  cat("Correlación Ext vs. Ext_ajustado:",
      round(cor(correlatos_v2$Ext, correlatos_v2$Ext_ajustado, use = "complete.obs"), 3), "\n")
  cat("Correlación Ext_ajustado vs. MCA_Dim1:",
      round(cor(correlatos_v2$Ext_ajustado, correlatos_v2$MCA_Dim1, use = "complete.obs"), 3), "\n")

  write_csv(correlatos_v2, file.path(OUT_DIR, "correlatos_MCA_TamGrupo.csv"))
} else {
  warning("tam_grupo_p_casen2024_RM.csv no encontrado en data/ — PASO 6 omitido. ",
          "Correr extraccion_TamGrupo_CASEN.R localmente y ubicar su salida en data/.")
}

cat("\nGuardado: comparacion_ego_CA_vs_MCA.csv",
    if (file.exists(tam_path)) ", correlatos_MCA_TamGrupo.csv" else "", "\n")
cat("=== FIN: EXPLORACIÓN MCA COMPLETA ===\n")
