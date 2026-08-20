# =============================================================================
# 03_ca_habilidades_isei.R
# Etapa 3 del pipeline: Análisis de Correspondencias (CA) sobre el universo
# ocupacional de la RM (CASEN 2024, ponderado por población), y arquitectura
# ISEI (ego y origen) vía join oficial Ganzeboom (2010) con regresión
# Dim1->ISEI solo como respaldo.
# Tesis Magíster en Sociología — PUC Chile | Trajan Pirkovic Palma
#
# VERSIÓN 2026-08-18. FUSIÓN de dos scripts que habían quedado separados:
#   - El "03" original (join ISEI Ganzeboom, respaldo por regresión) corría
#     el CA sobre el universo ESCO GLOBAL, sin ponderar.
#   - El "03b" (20-jul-2026) re-estimó el CA sobre el universo RM (CASEN
#     2024), ponderado por población vía row.w -- la decisión metodológica
#     VIGENTE -- pero escribía a un archivo distinto (ca_coords_RM.rds) que
#     04_indicadores_red.R nunca leía.
# Un re-run de 01->02->03 el 07-ago-2026 sobrescribió silenciosamente la
# versión ponderada con la global, sin que nada lo advirtiera (bug detectado
# y corregido el 18-ago-2026 con un parche puntual, 00_reconciliar_ca_
# ponderado.R). Esta fusión reemplaza ese parche: el CA ponderado por RM
# vuelve a ser la ÚNICA fuente que este script puede producir, así que un
# re-run completo del pipeline ya no puede volver a introducir el mismo bug.
#
# CAMBIO ADICIONAL (fix del 18-ago-2026): la corrección ISCO
# (correcciones_isco_casen.csv) ahora se aplica al isco_ego4/isco_padre4
# ANTES de proyectar sobre el espacio CA. En la versión vieja del "03" esto
# faltaba -- se aplicaba en 04 y en 15, pero no aquí -- por lo que la
# cobertura de Dim1_ego quedaba en 93.5% en vez del 95.1% correcto.
#
# CAMBIO ADICIONAL (19-ago-2026): se agrega la Acción 14, que exporta y
# muestra en RStudio dos visualizaciones del espacio CA: el universo
# completo de 391 ocupaciones de la RM, y las 27 posiciones del generador
# de posiciones solas. Ambas usan ggrepel para las etiquetas.
#
# AUTOCONTENIDO: lee sus insumos desde disco.
#
# ACCIONES QUE EJECUTA ESTE SCRIPT, EN ORDEN:
#   1.  Lee el universo ocupacional RM (CASEN 2024) y limpia códigos no válidos.
#   2.  Reconstruye/reutiliza la matriz ISCO×L2 global (idéntica a 02).
#   3.  Restringe esa matriz a las ocupaciones RM con match ESCO.
#   4.  Corre CA() ponderado por población (row.w).
#   5.  Proyecta las 27 posiciones del GP sobre esa geometría.
#   6.  Ajusta la regresión ISEI~Dim1 (respaldo, NO fuente primaria).
#   7.  Lee la tabla oficial Ganzeboom (isei08_ganzeboom2010.csv).
#   8.  Aplica la corrección ISCO a isco_ego4/isco_padre4 (fix 18-ago-2026).
#   9.  Proyecta el ISCO corregido de cada ego sobre el espacio CA.
#   10. Calcula el ISEI del ego: join directo + respaldo de regresión.
#   11. Calcula el ISEI de origen (ISCO del padre): mismo procedimiento.
#   12. Reporta cobertura de cada fuente e imprime diagnósticos.
#   13. Guarda coordenadas e indicadores ISEI a disco (ca_coords.rds,
#       ego_ca_isei.rds -- mismos nombres de archivo y de columna que
#       04_indicadores_red.R ya espera, sin necesidad de ningún parche).
#   14. Exporta y muestra en RStudio las visualizaciones del espacio CA
#       (391 ocupaciones RM y 27 posiciones del GP).
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse); library(FactoMineR); library(ggrepel)
})
set.seed(2025)

ESCO_DIR         <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data/esco"
FONDECYT_DIR     <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data/fondecyt"
DATA_DIR         <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data"
INTERMEDIATE_DIR <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output/intermediate"
OUT_DIR          <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output"

stopifnot(
  file.exists(file.path(INTERMEDIATE_DIR, "encuesta_prep.rds")),
  file.exists(file.path(INTERMEDIATE_DIR, "matriz_isco_L2.rds")),
  file.exists(file.path(DATA_DIR, "ocupaciones_rm_casen2024.csv")),
  file.exists(file.path(DATA_DIR, "correcciones_isco_casen.csv")),
  file.exists(file.path(DATA_DIR, "gp_crosswalk.csv"))
)

# =============================================================================
# ACCIÓN 1 · Leer y limpiar el universo ocupacional RM (CASEN 2024)
# =============================================================================
cat("=== ACCIÓN 1: Universo ocupacional RM (CASEN 2024) ===\n")

casen_rm_raw <- read_csv(file.path(DATA_DIR, "ocupaciones_rm_casen2024.csv"),
                          col_types = cols(oficio_codigo = "c"), show_col_types = FALSE)

# Se descartan: Fuerzas Armadas (códigos a 3 dígitos) y códigos de no
# respuesta CASEN. Juntos pesan 0.11% de la población.
casen_rm <- casen_rm_raw |>
  filter(nchar(oficio_codigo) == 4) |>
  mutate(isco4 = as.integer(oficio_codigo))

pct_excluido_limpieza <- sum(casen_rm_raw$porcentaje) - sum(casen_rm$porcentaje)
cat("Filas originales:", nrow(casen_rm_raw), "| válidas (4 dígitos):", nrow(casen_rm), "\n")
cat("Población excluida por limpieza (FFAA + no respuesta):",
    round(pct_excluido_limpieza, 3), "%\n")

# Correcciones ISCO: CIUO-08.CL (CASEN) -> ISCO-08 internacional (ESCO).
# Fuente única, correcciones_isco_casen.csv (ver ese archivo para el detalle
# y la justificación caso a caso; dos correcciones semánticamente forzadas
# -2247, 2248- fueron explícitamente rechazadas y quedan excluidas).
correcciones <- read_csv(file.path(DATA_DIR, "correcciones_isco_casen.csv"), show_col_types = FALSE)

casen_rm <- casen_rm |>
  left_join(correcciones |> select(isco4_casen, isco4_corregido, certeza),
            by = c("isco4" = "isco4_casen")) |>
  mutate(isco4_original = isco4,
         isco4 = coalesce(isco4_corregido, isco4)) |>
  select(-isco4_corregido)

cat("Correcciones ISCO aplicadas:", sum(!is.na(casen_rm$certeza)), "filas\n")

# Dos correcciones convergen al mismo código: se agrupan y suman antes de
# seguir, para no duplicar filas con el mismo isco4 en la matriz.
n_antes_dedup <- nrow(casen_rm)
casen_rm <- casen_rm |>
  group_by(isco4) |>
  summarise(oficio_descriptor   = paste(unique(oficio_descriptor), collapse = " / "),
            personas_expandidas = sum(personas_expandidas),
            porcentaje          = sum(porcentaje),
            .groups = "drop")
if (nrow(casen_rm) < n_antes_dedup) {
  cat("Filas fusionadas por convergencia de corrección:", n_antes_dedup - nrow(casen_rm), "\n")
}

# =============================================================================
# ACCIÓN 2 · Reconstruir/reutilizar la matriz ISCO×L2 global (idéntica a 02)
# =============================================================================
cat("\n=== ACCIÓN 2: Matriz ISCO×L2 (ESCO completo) ===\n")

mat_wide_global <- readRDS(file.path(INTERMEDIATE_DIR, "matriz_isco_L2.rds"))
cat("Matriz global:", nrow(mat_wide_global), "ocupaciones ×", ncol(mat_wide_global), "categorías\n")

# =============================================================================
# ACCIÓN 3 · Restringir la matriz a las ocupaciones RM con match ESCO
# =============================================================================
cat("\n=== ACCIÓN 3: Restringir matriz al universo RM ===\n")

isco_disponibles_esco <- as.integer(rownames(mat_wide_global))
isco_rm_con_match      <- intersect(casen_rm$isco4, isco_disponibles_esco)
isco_rm_sin_match      <- setdiff(casen_rm$isco4, isco_disponibles_esco)

pct_sin_match <- casen_rm |> filter(isco4 %in% isco_rm_sin_match) |> pull(porcentaje) |> sum()
pct_con_match <- casen_rm |> filter(isco4 %in% isco_rm_con_match) |> pull(porcentaje) |> sum()

cat("Ocupaciones RM con match ESCO:", length(isco_rm_con_match),
    "(", round(pct_con_match, 1), "% de la población ocupada )\n")
cat("Ocupaciones RM SIN match ESCO:", length(isco_rm_sin_match),
    "(", round(pct_sin_match, 1), "% -- EXCLUIDAS, declarar como limitación )\n")

mat_rm <- mat_wide_global[as.character(isco_rm_con_match), , drop = FALSE]
mat_rm <- mat_rm[, colSums(mat_rm) > 0, drop = FALSE]
cat("Matriz RM final:", nrow(mat_rm), "ocupaciones ×", ncol(mat_rm), "categorías\n")

# =============================================================================
# ACCIÓN 4 · CA ponderado por población (row.w) -- DECISIÓN VIGENTE (20-jul-2026)
# =============================================================================
cat("\n=== ACCIÓN 4: CA ponderado por población ===\n")

pesos_pob <- casen_rm |>
  filter(isco4 %in% as.integer(rownames(mat_rm))) |>
  arrange(match(isco4, as.integer(rownames(mat_rm)))) |>
  pull(porcentaje)

stopifnot("Los pesos deben alinear fila a fila con mat_rm" = length(pesos_pob) == nrow(mat_rm))

ca_result <- CA(mat_rm, ncp = 5, graph = FALSE, row.w = pesos_pob)

ca_coords <- as.data.frame(ca_result$row$coord) |>
  rownames_to_column("isco4") |>
  mutate(isco4 = as.integer(isco4)) |>
  rename(Dim1 = `Dim 1`, Dim2 = `Dim 2`)

varianza <- ca_result$eig[, "percentage of variance"]
cat("Varianza CA (ponderado RM) -- Dim1:", round(varianza[1], 1),
    "% | Dim2:", round(varianza[2], 1), "% | acumulada:", round(sum(varianza[1:2]), 1), "%\n")

# =============================================================================
# ACCIÓN 5 · Proyectar las 27 posiciones del GP sobre el espacio CA
# =============================================================================
gp_crosswalk <- read_csv(file.path(DATA_DIR, "gp_crosswalk.csv"), show_col_types = FALSE)

gp_coords <- gp_crosswalk |>
  left_join(ca_coords |> select(isco4, Dim1, Dim2), by = "isco4") |>
  rename(Dim1_p = Dim1, Dim2_p = Dim2)

n_gp_con_coord <- sum(!is.na(gp_coords$Dim1_p))
cat("\n=== ACCIÓN 5: Proyección del GP ===\n")
cat("Posiciones GP con coords CA:", n_gp_con_coord, "de 27\n")
if (n_gp_con_coord < 27) {
  cat("Posiciones SIN coordenada (su ISCO no está en las", nrow(mat_rm), "ocupaciones RM con match ESCO):\n")
  print(gp_coords |> filter(is.na(Dim1_p)) |> select(var, label, isco4))
}
cat("Correlación Dim1-ISEI (GP):", round(cor(gp_coords$Dim1_p, gp_coords$isei, use = "complete.obs"), 3), "\n")

# =============================================================================
# ACCIÓN 6 · Regresión ISEI~Dim1 -- SOLO RESPALDO, no fuente primaria
# =============================================================================
isei_from_dim1 <- lm(isei ~ Dim1_p, data = gp_coords)
cat("ISEI ~ Dim1 R² (diagnóstico/respaldo, ya NO es la fuente primaria):",
    round(summary(isei_from_dim1)$r.squared, 3), "\n")

# =============================================================================
# ACCIÓN 7 · Leer la tabla oficial Ganzeboom (2010)
# =============================================================================
isei08_path <- file.path(ESCO_DIR, "isei08_ganzeboom2010.csv")
if (!file.exists(isei08_path)) isei08_path <- file.path(FONDECYT_DIR, "isei08_ganzeboom2010.csv")
isei08 <- read_csv(isei08_path, col_types = cols(isco08 = "c", isei08 = "d")) |>
  mutate(isco08 = as.integer(isco08))

# =============================================================================
# ACCIÓN 8 · Corrección ISCO a isco_ego4/isco_padre4 -- FIX 18-ago-2026
# =============================================================================
# Antes de proyectar sobre el espacio CA, se aplica la misma corrección de
# codigo ISCO (correcciones_isco_casen.csv) que ya usan 04_indicadores_red.R
# (Acción 5) y 15_indicador_cierre_estructural.R (Decisión D7). En la
# version vieja de este script (pre-fusión) esto faltaba, y la tasa de match
# de Dim1_ego quedaba en 93.5% en vez del 95.1% correcto -- los 17 casos
# ISCO 3221->5321 no se recodificaban antes del join.
cat("\n=== ACCIÓN 8: Corrección ISCO de ego/origen ===\n")

df_analitica <- readRDS(file.path(INTERMEDIATE_DIR, "encuesta_prep.rds"))

corr_map <- correcciones |>
  transmute(isco4_orig = suppressWarnings(as.integer(isco4_casen)),
            isco4_corr = suppressWarnings(as.integer(isco4_corregido))) |>
  filter(!is.na(isco4_orig))

df_analitica <- df_analitica |>
  left_join(corr_map, by = c("isco_ego4" = "isco4_orig")) |>
  mutate(isco_ego4_corr = as.character(coalesce(isco4_corr, as.integer(isco_ego4)))) |>
  select(-isco4_corr) |>
  left_join(corr_map, by = c("isco_padre4" = "isco4_orig")) |>
  mutate(isco_padre4_corr = as.character(coalesce(isco4_corr, as.integer(isco_padre4)))) |>
  select(-isco4_corr)

n_recod_ego   <- sum(df_analitica$isco_ego4_corr   != as.character(df_analitica$isco_ego4),   na.rm = TRUE)
n_recod_padre <- sum(df_analitica$isco_padre4_corr != as.character(df_analitica$isco_padre4), na.rm = TRUE)
cat("Casos recodificados por corrección ISCO -- ego:", n_recod_ego, "| origen:", n_recod_padre, "\n")

ca_coords_chr <- ca_coords |> mutate(isco4 = as.character(isco4))
isei08_chr    <- isei08 |> mutate(isco08 = as.character(isco08))

stopifnot(
  "isco_ego4_corr debe ser character antes del join" = is.character(df_analitica$isco_ego4_corr),
  "ca_coords_chr$isco4 debe ser character antes del join" = is.character(ca_coords_chr$isco4)
)

# =============================================================================
# ACCIÓN 9 · Proyectar el ISCO corregido de cada ego sobre el espacio CA
# =============================================================================
ego_coords <- df_analitica |>
  select(ID, isco_ego4_corr) |>
  left_join(ca_coords_chr |> rename(isco_ego4_corr = isco4), by = "isco_ego4_corr") |>
  rename(Dim1_ego = Dim1, Dim2_ego = Dim2)

# =============================================================================
# ACCIÓN 10-11 · ISEI del ego y de origen -- join directo (primario) + respaldo
# =============================================================================
df_ego_ca <- df_analitica |>
  select(ID, isco_ego4_corr, isco_padre4_corr) |>
  left_join(ego_coords |> select(ID, Dim1_ego, Dim2_ego), by = "ID") |>
  rename(Comp_ego = Dim2_ego) |>
  left_join(isei08_chr |> rename(isco_ego4_corr = isco08, isei_ego_directo = isei08),
            by = "isco_ego4_corr") |>
  mutate(
    isei_ego_regresion = predict(isei_from_dim1, newdata = data.frame(Dim1_p = Dim1_ego)),
    isei_ego_hat        = coalesce(isei_ego_directo, isei_ego_regresion)
  ) |>
  left_join(ca_coords_chr |> select(isco4, Dim1) |>
              rename(isco_padre4_corr = isco4, Dim1_padre = Dim1),
            by = "isco_padre4_corr") |>
  left_join(isei08_chr |> rename(isco_padre4_corr = isco08, ISEI_orig_directo = isei08),
            by = "isco_padre4_corr") |>
  mutate(
    ISEI_orig_regresion = predict(isei_from_dim1, newdata = data.frame(Dim1_p = Dim1_padre)),
    ISEI_orig_hat        = coalesce(ISEI_orig_directo, ISEI_orig_regresion)
  ) |>
  rename(isco_padre4 = isco_padre4_corr)

# =============================================================================
# ACCIÓN 12 · Reportar cobertura de cada fuente
# =============================================================================
cat("\n=== ACCIÓN 12: Cobertura ===\n")
cat("ISEI de ego -- join directo:", sum(!is.na(df_ego_ca$isei_ego_directo)),
    "| vía respaldo de regresión:",
    sum(is.na(df_ego_ca$isei_ego_directo) & !is.na(df_ego_ca$isei_ego_regresion)), "\n")
cat("ISEI de origen -- join directo:", sum(!is.na(df_ego_ca$ISEI_orig_directo)),
    "| vía respaldo de regresión:",
    sum(is.na(df_ego_ca$ISEI_orig_directo) & !is.na(df_ego_ca$ISEI_orig_regresion)),
    "| sin dato:", sum(is.na(df_ego_ca$ISEI_orig_hat)), "\n")
cat("Egos con Dim1_ego (coordenada RM) no-NA:", sum(!is.na(df_ego_ca$Dim1_ego)),
    "de", nrow(df_ego_ca),
    sprintf("(%.1f%%)", 100 * mean(!is.na(df_ego_ca$Dim1_ego))), "\n")

# =============================================================================
# ACCIÓN 13 · Guardar salidas
# =============================================================================
# Mismos nombres de archivo y de columna que 04_indicadores_red.R ya espera
# (Dim1/Dim2, no Dim1_RM/Dim2_RM) -- con esta fusión, 04 no necesita ningún
# cambio ni ningún script de reconciliación aparte.
saveRDS(
  list(ca_coords = ca_coords, gp_coords = gp_coords, isei_from_dim1 = isei_from_dim1),
  file.path(INTERMEDIATE_DIR, "ca_coords.rds")
)
saveRDS(
  df_ego_ca |> select(ID, Dim1_ego, Comp_ego, isei_ego_hat, isco_padre4,
                       Dim1_padre, ISEI_orig_hat),
  file.path(INTERMEDIATE_DIR, "ego_ca_isei.rds")
)

reporte_cobertura <- tibble(
  item = c("Ocupaciones CASEN RM originales", "Válidas (4 dígitos, sin FFAA/no-respuesta)",
           "Correcciones ISCO aplicadas al universo (224x->226x y semánticas)",
           "Con match ESCO tras corrección (incluidas en el CA)", "Sin match ESCO (excluidas)",
           "% población cubierta", "% población excluida (limitación a declarar)",
           "Casos ego recodificados por corrección ISCO",
           "Cobertura final Dim1_ego"),
  valor = c(nrow(casen_rm_raw), n_antes_dedup,
            sum(!is.na(correcciones$isco4_casen)),
            length(isco_rm_con_match), length(isco_rm_sin_match),
            round(pct_con_match, 2), round(pct_sin_match, 2),
            n_recod_ego,
            sprintf("%.1f%%", 100 * mean(!is.na(df_ego_ca$Dim1_ego))))
)
write_csv(reporte_cobertura, file.path(OUT_DIR, "reporte_cobertura_CA_RM.csv"))

cat("\nGuardado: intermediate/ca_coords.rds, ego_ca_isei.rds, reporte_cobertura_CA_RM.csv\n")
cat("=== FIN ETAPA 03 ===\n")

# =============================================================================
# ACCIÓN 14 · Visualizaciones del espacio CA: 391 ocupaciones y 27 del GP
# =============================================================================
cat("\n=== ACCIÓN 14: Visualizaciones del espacio CA ===\n")

GRAFICOS_DIR <- file.path(OUT_DIR, "graficos")
if (!dir.exists(GRAFICOS_DIR)) dir.create(GRAFICOS_DIR, recursive = TRUE)

# Base para el gráfico de 391: coordenadas CA + descriptor CASEN + ISEI
# oficial (Ganzeboom). Se marca cuál de las 391 corresponde a una de las 27
# posiciones del GP, para resaltarlas con otro color y ser las únicas
# etiquetadas -- etiquetar las 391 es ilegible incluso con repulsión.
occ_plot_data <- ca_coords |>
  left_join(casen_rm |> select(isco4, oficio_descriptor), by = "isco4") |>
  left_join(isei08 |> rename(isco4 = isco08, isei = isei08), by = "isco4") |>
  mutate(es_gp = isco4 %in% gp_crosswalk$isco4)

cat("Ocupaciones con ISEI para el gráfico de 391:",
    sum(!is.na(occ_plot_data$isei)), "de", nrow(occ_plot_data), "\n")

etiquetas_gp <- occ_plot_data |>
  filter(es_gp) |>
  left_join(gp_crosswalk |> select(isco4, label), by = "isco4") |>
  mutate(label = coalesce(label, oficio_descriptor))

# --- Gráfico 1: las 391 ocupaciones del universo RM ------------------------
p_ca_391 <- ggplot(occ_plot_data, aes(x = Dim1, y = Dim2)) +
  geom_point(aes(size = isei, color = es_gp), alpha = 0.55) +
  geom_text_repel(
    data = etiquetas_gp, aes(label = label),
    size = 3, max.overlaps = Inf, min.segment.length = 0,
    segment.color = "grey60", seed = 2025
  ) +
  scale_color_manual(values = c(`FALSE` = "#9AA5B1", `TRUE` = "#173F8A"),
                      guide = "none") +
  scale_size_continuous(name = "ISEI\n(prestigio)", range = c(1, 8)) +
  labs(
    title    = "Espacio de habilidades (CA): 391 ocupaciones de la RM",
    subtitle = "Cercanía = perfiles de habilidades similares · Tamaño = prestigio (ISEI) · Azul = 27 posiciones del generador",
    x = "Dim1  (socio-cognitivo \u2194 manual)",
    y = "Dim2  (orientación sectorial)"
  ) +
  theme_minimal(base_size = 14)

# --- Gráfico 2: las 27 posiciones del generador, solas ---------------------
gp_plot_data <- gp_coords |> filter(!is.na(Dim1_p))

p_ca_27 <- ggplot(gp_plot_data, aes(x = Dim1_p, y = Dim2_p, size = isei)) +
  geom_point(alpha = 0.6, color = "#173F8A") +
  geom_text_repel(aes(label = label), size = 3.5, max.overlaps = Inf,
                   min.segment.length = 0, segment.color = "grey60", seed = 2025) +
  scale_size_continuous(name = "ISEI\n(prestigio)", range = c(2, 12)) +
  labs(
    title    = "Espacio de habilidades (CA): 27 ocupaciones del generador de posiciones",
    subtitle = "Cercanía = perfiles de habilidades similares · Tamaño = prestigio (ISEI)",
    x = "Dim1  (socio-cognitivo \u2194 manual)",
    y = "Dim2  (orientación sectorial)"
  ) +
  theme_minimal(base_size = 14)

# Mostrar ambos en el panel de Plots de RStudio
print(p_ca_391)
print(p_ca_27)

# Exportar
ggsave(file.path(GRAFICOS_DIR, "fig_CA_espacio_391.png"), p_ca_391,
       width = 12, height = 9, dpi = 300, bg = "white")
ggsave(file.path(GRAFICOS_DIR, "fig_CA_espacio_27_GP.png"), p_ca_27,
       width = 10, height = 8, dpi = 300, bg = "white")

cat("\nGuardado: output/graficos/fig_CA_espacio_391.png, fig_CA_espacio_27_GP.png\n")
cat("=== FIN ACCIÓN 14 ===\n")

# =============================================================================
# DECISIONES METODOLÓGICAS
# =============================================================================
# D1. El CA se estima sobre el universo ocupacional RM (CASEN 2024),
#     ponderado por población vía row.w, NO sobre el universo ESCO global.
#     Decisión del 20-jul-2026 (ver justificación completa en el 03b
#     original, archivado en _archivo/). Alabdulkareem et al. (2018), el
#     precedente metodológico central, usa igualmente el universo
#     ocupacional de UN país (EE.UU., O*NET), no un catálogo supranacional.
# D2. Limitación irreducible: el contenido de habilidades por ocupación
#     sigue siendo de origen europeo (ESCO). Restringir el universo a la RM
#     no resuelve esto, solo corrige qué ocupaciones entran a la geometría
#     y con qué peso relativo.
# D3. Dos correcciones ISCO propuestas en su momento (2247->2267, 2248->2269)
#     fueron rechazadas por no sostenerse semánticamente. Quedan excluidas
#     del universo (0.4% de la población), no forzadas.
# D4. FIX 2026-08-18: la corrección ISCO se aplica ahora a isco_ego4 e
#     isco_padre4 ANTES de proyectar sobre el espacio CA (Acción 8), no
#     después. Esta fusión reemplaza el parche puntual
#     00_reconciliar_ca_ponderado.R (archivado en _archivo/), que corregía
#     los .rds ya generados pero no el script que los produce -- un re-run
#     completo del pipeline sin este fix habría vuelto a perder la
#     corrección.
# D5. Se exportan dos visualizaciones del espacio CA: el universo completo
#     de 391 ocupaciones (con solo las 27 del GP etiquetadas, por
#     legibilidad) y las 27 del GP solas. Ambas usan ggrepel para evitar el
#     solapamiento de etiquetas que tenía la versión anterior. Guardadas en
#     output/graficos/.
# =============================================================================
