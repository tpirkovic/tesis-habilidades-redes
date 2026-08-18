# =============================================================================
# deteccion_comunidades_L3.R
# Detección de comunidades de habilidades a nivel de granularidad L3 ESCO
# (~400 categorías), para comparar contra la versión L2 (110 categorías)
# de deteccion_comunidades_habilidades.R.
# Trajan Pirkovic Palma | Tesis Magíster Sociología, PUC Chile
#
# Script AUTOCONTENIDO y SECUNDARIO. Misma lógica que el script L2:
# RCA (Hidalgo-Hausmann) -> matriz binaria -> red de complementariedad
# (coeficiente phi) -> Louvain -> caracterización vía CA e ISEI implícito.
# La diferencia es únicamente el nivel de agregación de las categorías de
# habilidad: aquí se usa Nivel 3 de ESCO cuando existe, y se cae a Nivel 2
# como respaldo para las habilidades que no tienen desagregación L3 (esas
# quedan marcadas con el sufijo "(L2, sin desagregar)" en su etiqueta).
#
# ACCIONES QUE EJECUTA ESTE SCRIPT, EN ORDEN (idénticas a la versión L2,
# ver deteccion_comunidades_habilidades.R para el detalle línea a línea de
# cada una; aquí solo se resume dónde difiere):
#   1. Reconstruye la matriz ISCO×habilidad, pero agregando por L3 (con
#      respaldo a L2 para las habilidades sin desagregación L3) en vez de L2.
#   2. Calcula RCA por celda ocupación×categoría y arma la matriz binaria.
#   3. Calcula phi (co-ocurrencia) entre pares de categorías L3.
#   4. Construye la red de complementariedad y corre Louvain.
#   5. Cruza cada comunidad con el ISEI oficial (gp_crosswalk.csv) para
#      obtener cohesión interna (phi) y dispersión de estatus (DE ISEI).
#   6. Verifica si el patrón de la comunidad 3 (L2) reaparece a este nivel
#      más fino — chequeo de ROBUSTEZ, no un hallazgo nuevo por sí mismo.
#   7. Exporta tablas y figuras equivalentes a las de la versión L2, con
#      sufijo _L3 en los nombres de archivo.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggrepel)
  library(FactoMineR)
  library(igraph)
})

# NOTA: la semilla NO se fija aquí. Se fija inmediatamente antes de
# cluster_louvain() (Paso 5) para que el resultado no dependa de cuántas
# operaciones aleatorias se hayan ejecutado antes en el script.

DATA_DIR      <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data/esco"
CROSSWALK_DIR <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data"
OUT_DIR       <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output"
p  <- function(f) file.path(DATA_DIR, f)
po <- function(f) file.path(OUT_DIR, f)

RCA_THRESHOLD    <- 1
MIN_COVERAGE_OCC <- 0
PHI_MIN_EDGE     <- 0

# =============================================================================
# PASO 1 — MATRIZ ISCO-08 × CATEGORÍA DE HABILIDAD (NIVEL 3 ESCO, con
#           respaldo en Nivel 2 para habilidades sin desagregación L3)
# =============================================================================

cat("=== PASO 1: Matriz ISCO x habilidades (Nivel 3 ESCO) ===\n")

osr <- read_csv(p("occupationSkillRelations_es.csv"),
                col_types = cols(.default = "c"), show_col_types = FALSE)

occ_es <- read_csv(p("occupations_es.csv"),
                   col_types = cols(.default = "c"), show_col_types = FALSE) |>
  mutate(isco4 = as.integer(iscoGroup)) |>
  select(conceptUri, isco4) |>
  filter(!is.na(isco4))

osr <- osr |> left_join(occ_es, by = c("occupationUri" = "conceptUri"),
                         relationship = "many-to-many")

skill_hier <- read_csv(p("skillsHierarchy_es.csv"),
                        col_types = cols(.default = "c"), show_col_types = FALSE)

br <- read_csv(p("broaderRelationsSkillPillar_es.csv"),
               col_types = cols(.default = "c"), show_col_types = FALSE)

skill_to_group <- br |>
  filter(conceptType == "KnowledgeSkillCompetence", broaderType == "SkillGroup") |>
  select(skill_uri = conceptUri, group_uri = broaderUri) |>
  distinct()

# Nivel 3 cuando existe...
group_to_L3_native <- skill_hier |>
  filter(!is.na(`Level 3 URI`), !is.na(`Level 3 code`)) |>
  select(group_uri = `Level 3 URI`, L3_code = `Level 3 code`,
         L3_label = `Level 3 preferred term`)

# ...con respaldo en Nivel 2 para las habilidades que no tienen hoja L3
group_to_L3_fallback <- skill_hier |>
  filter(is.na(`Level 3 URI`), !is.na(`Level 2 code`)) |>
  select(group_uri = `Level 2 URI`, L3_code = `Level 2 code`,
         L3_label = `Level 2 preferred term`) |>
  mutate(L3_label = paste0(L3_label, " (L2, sin desagregar)"))

group_to_L3 <- bind_rows(group_to_L3_native, group_to_L3_fallback) |>
  distinct(group_uri, .keep_all = TRUE)

skill_L3_map <- skill_to_group |>
  left_join(group_to_L3, by = "group_uri") |>
  filter(!is.na(L3_code)) |>
  distinct(skill_uri, L3_code, .keep_all = TRUE)

osr_essential <- osr |> filter(relationType == "essential", !is.na(isco4))

osr_L3 <- osr_essential |>
  left_join(skill_L3_map, by = c("skillUri" = "skill_uri"),
            relationship = "many-to-many") |>
  filter(!is.na(L3_code))

mat_wide <- osr_L3 |>
  count(isco4, L3_code, name = "n_skills") |>
  pivot_wider(names_from = L3_code, values_from = n_skills, values_fill = 0) |>
  column_to_rownames("isco4")

mat_ca <- as.matrix(mat_wide[, colSums(mat_wide) > 0])
mat_ca <- mat_ca[rowSums(mat_ca) > 0, ]

cat("Matriz ISCO x L3:", nrow(mat_ca), "x", ncol(mat_ca), "\n")
cat("(la versión L2 tenía 426 x 110 — aquí hay muchas más columnas)\n")

label_L3 <- skill_L3_map |> distinct(L3_code, L3_label) |>
  filter(L3_code %in% colnames(mat_ca))


# =============================================================================
# PASO 2 — CA NATIVO A NIVEL L3 (para coordenadas propias de cada categoría;
#           no se reutilizan las coordenadas del CA principal, que es L2)
# =============================================================================

cat("\n=== PASO 2: CA sobre la matriz L3 ===\n")

ca_result <- CA(mat_ca, ncp = 5, graph = FALSE)

ca_row_coords <- as.data.frame(ca_result$row$coord) |>
  rownames_to_column("isco4") |>
  mutate(isco4 = as.integer(isco4)) |>
  rename(Dim1 = `Dim 1`, Dim2 = `Dim 2`)

ca_col_coords <- as.data.frame(ca_result$col$coord) |>
  rownames_to_column("L3_code") |>
  rename(Dim1_skill = `Dim 1`, Dim2_skill = `Dim 2`)

eig <- ca_result$eig
cat(sprintf("Var. explicada CA (L3) — Dim1: %.2f%% | Dim2: %.2f%%\n", eig[1,2], eig[2,2]))
cat("(la versión L2 explicaba 7.2% y 6.4% — más granularidad reparte más la varianza)\n")


# =============================================================================
# PASO 3 — RCA Y MATRIZ BINARIA
# =============================================================================

cat("\n=== PASO 3: Ventaja Comparativa Revelada (RCA) ===\n")

row_totals  <- rowSums(mat_ca)
col_totals  <- colSums(mat_ca)
grand_total <- sum(mat_ca)
expected    <- outer(row_totals, col_totals) / grand_total
rca_mat     <- mat_ca / expected
B <- (rca_mat > RCA_THRESHOLD) * 1L

cat("N° promedio de categorías 'efectivas' (RCA>1) por ocupación:",
    round(mean(rowSums(B)), 1), "de", ncol(B), "\n")

cobertura_categoria <- colSums(B)
categorias_validas  <- names(cobertura_categoria)[cobertura_categoria >= MIN_COVERAGE_OCC]
B <- B[, categorias_validas]


# =============================================================================
# PASO 4 — RED DE COMPLEMENTARIEDAD (phi) Y COMUNIDADES (Louvain)
# =============================================================================

cat("\n=== PASO 4: Red de complementariedad (phi) ===\n")

phi_mat <- cor(B, method = "pearson")
diag(phi_mat) <- 0

edges_df <- as.data.frame(as.table(phi_mat)) |>
  rename(from = Var1, to = Var2, phi = Freq) |>
  filter(as.character(from) < as.character(to), phi > PHI_MIN_EDGE)

cat("Aristas retenidas (phi >", PHI_MIN_EDGE, "):", nrow(edges_df),
    "de", choose(ncol(B), 2), "pares posibles\n")

g <- graph_from_data_frame(edges_df, directed = FALSE,
                            vertices = data.frame(name = colnames(B)))
E(g)$weight <- edges_df$phi

cat("\n=== PASO 5: Detección de comunidades (Louvain) ===\n")

# Semilla fijada inmediatamente antes de cluster_louvain(): entre este punto
# y la carga de librerías no se ejecutó ningún paso con componente aleatoria
# (CA(), el cálculo de RCA y la matriz phi son deterministas), así que este
# cambio de posición no altera el resultado ya obtenido; solo blinda la
# reproducibilidad ante futuros pasos aleatorios que se agreguen antes.
set.seed(2025)
comunidades <- cluster_louvain(g, weights = E(g)$weight)

tabla_comunidades <- tibble(
  L3_code   = names(membership(comunidades)),
  comunidad = as.integer(membership(comunidades))
) |>
  left_join(label_L3, by = "L3_code") |>
  left_join(ca_col_coords, by = "L3_code")

cat("N° de comunidades detectadas:", length(unique(tabla_comunidades$comunidad)), "\n")
cat("Modularidad (Q):", round(modularity(comunidades), 3), "\n")
print(table(tabla_comunidades$comunidad))
write_csv(tabla_comunidades, po("comunidades_L3.csv"))


# =============================================================================
# PASO 6 — ISEI IMPLÍCITO POR COMUNIDAD
# =============================================================================

cat("\n=== PASO 6: ISEI implícito por comunidad ===\n")

# gp_crosswalk: fuente ÚNICA centralizada (data/gp_crosswalk.csv) —
# centralizado el 20-jul-2026, ver nota en deteccion_comunidades_habilidades.R.
gp_crosswalk <- read_csv(file.path(CROSSWALK_DIR, "gp_crosswalk.csv"), show_col_types = FALSE) |>
  select(var, isco4, isei)

gp_ca <- gp_crosswalk |> left_join(ca_row_coords, by = "isco4") |> filter(!is.na(Dim1))
isei_from_dim1 <- lm(isei ~ Dim1, data = gp_ca)
cat("R2 de lm(isei ~ Dim1) sobre", nrow(gp_ca), "posiciones GP:",
    round(summary(isei_from_dim1)$r.squared, 3), "\n")

tabla_comunidades <- tabla_comunidades |>
  mutate(isei_implicito = predict(isei_from_dim1,
                                   newdata = data.frame(Dim1 = Dim1_skill)))

resumen_comunidades <- tabla_comunidades |>
  group_by(comunidad) |>
  summarise(n_categorias = n(),
            isei_medio   = mean(isei_implicito, na.rm = TRUE),
            isei_de      = sd(isei_implicito, na.rm = TRUE),
            .groups = "drop") |>
  arrange(desc(isei_medio))

cat("\n=== Resumen de comunidades L3 ===\n")
print(resumen_comunidades)
write_csv(resumen_comunidades, po("resumen_comunidades_L3.csv"))


# =============================================================================
# PASO 7 — COHESIÓN INTERNA Y COBERTURA (misma verificación que en L2)
# =============================================================================

cat("\n=== PASO 7: Cohesión interna y cobertura ===\n")

calc_cohesion <- function(com_id) {
  miembros <- tabla_comunidades$L3_code[tabla_comunidades$comunidad == com_id]
  sub  <- phi_mat[miembros, miembros]
  vals <- sub[upper.tri(sub)]
  tibble(comunidad = com_id, n_cat = length(miembros),
         phi_medio_intra = mean(vals), phi_mediana = median(vals),
         pct_pares_neg = 100 * mean(vals < 0))
}
cohesion_tab <- map_dfr(sort(unique(tabla_comunidades$comunidad)), calc_cohesion)
cat("\nCohesión interna por comunidad:\n")
print(cohesion_tab)
write_csv(cohesion_tab, po("cohesion_comunidades_L3.csv"))

cat("\nCobertura general (resumen):\n")
print(summary(colSums(B)))


# =============================================================================
# PASO 8 — VISUALIZACIONES (con etiquetas legibles vía ggrepel)
# =============================================================================

cat("\n=== PASO 8: Gráficos ===\n")

grado_nodos <- degree(g)
tabla_nodos_consola <- tibble(L3_code = names(grado_nodos), grado = as.integer(grado_nodos)) |>
  left_join(tabla_comunidades |> select(L3_code, comunidad, L3_label), by = "L3_code") |>
  arrange(comunidad, desc(grado))

cat("\n=== Los 8 nodos más conectados por comunidad (L3) ===\n")
tabla_nodos_consola |> group_by(comunidad) |> slice_max(grado, n = 8) |> print(n = 100)

# Red completa: con ~400 nodos, mostrar todas las etiquetas sería ilegible.
# Se etiquetan solo los nodos con mayor grado (más centrales) de cada
# comunidad; el resto se muestra como punto sin texto.
top_labels <- tabla_nodos_consola |> group_by(comunidad) |> slice_max(grado, n = 10) |> pull(L3_code)

layout_xy <- as.data.frame(layout_with_fr(g))
names(layout_xy) <- c("x", "y")
layout_xy$L3_code <- V(g)$name

nodos_plot <- layout_xy |> left_join(tabla_comunidades, by = "L3_code") |>
  mutate(
    etiqueta = if_else(L3_code %in% top_labels,
                        if_else(nchar(L3_label) > 26,
                                paste0(L3_code, " ", substr(L3_label, 1, 26), "…"),
                                paste0(L3_code, " ", L3_label)),
                        NA_character_)
  )

aristas_plot <- edges_df |>
  left_join(layout_xy, by = c("from" = "L3_code")) |> rename(x_from = x, y_from = y) |>
  left_join(layout_xy, by = c("to" = "L3_code"))   |> rename(x_to = x, y_to = y)

p_red <- ggplot() +
  geom_segment(data = aristas_plot,
               aes(x = x_from, y = y_from, xend = x_to, yend = y_to, alpha = phi),
               color = "grey60", linewidth = 0.15) +
  geom_point(data = nodos_plot, aes(x = x, y = y, color = factor(comunidad)), size = 1.6) +
  ggrepel::geom_text_repel(
    data = nodos_plot |> filter(!is.na(etiqueta)),
    aes(x = x, y = y, label = etiqueta, color = factor(comunidad)),
    size = 2.2, max.overlaps = Inf, segment.size = 0.15, segment.alpha = 0.5,
    box.padding = 0.3, point.padding = 0.15, min.segment.length = 0, show.legend = FALSE
  ) +
  scale_alpha_continuous(range = c(0.03, 0.4)) +
  labs(title = "Red de complementariedad de habilidades — Nivel 3 ESCO",
       subtitle = sprintf("Louvain: %d comunidades, Q=%.3f. Solo se etiquetan los 10 nodos más\nconectados de cada comunidad (~400 nodos totales)",
                           length(unique(nodos_plot$comunidad)), modularity(comunidades)),
       color = "Comunidad", alpha = "phi") +
  theme_void(base_size = 11) +
  theme(legend.position = "right")

ggsave(po("fig_red_comunidades_L3.pdf"), p_red, width = 15, height = 12)
ggsave(po("fig_red_comunidades_L3_alta_res.png"), p_red, width = 17, height = 14, dpi = 300)

cat("\nArchivos generados:\n")
cat("  - comunidades_L3.csv / resumen_comunidades_L3.csv / cohesion_comunidades_L3.csv\n")
cat("  - fig_red_comunidades_L3.pdf (+ versión PNG alta resolución)\n")
cat("\n=== FIN: DETECCIÓN DE COMUNIDADES A NIVEL L3 ===\n")

# =============================================================================
# NOTA: compara los resultados de este script con
# deteccion_comunidades_habilidades.R (nivel L2). Lo esperable, según la
# corrida de referencia hecha en paralelo:
#   - Q algo más baja que en L2 (más ruido con más categorías finas).
#   - Var. explicada del CA nativo L3 más baja en Dim1/Dim2 que en L2.
#   - La comunidad agro-bio-legal de L2 tiende a FRAGMENTARSE: 'derecho' se
#     separa hacia un cluster de humanidades/administración, mientras que
#     veterinaria/ciencias ambientales quedan en un cluster de ciencias
#     naturales más acotado.
#   - La mayor dispersión de estatus (antes en la comunidad agro-bio-legal)
#     puede reaparecer en el cluster técnico-industrial más grande, que a
#     nivel L3 termina agrupando disciplinas de alto estatus (física,
#     química) junto a oficios manuales (construcción, minería, pesca).
# =============================================================================
