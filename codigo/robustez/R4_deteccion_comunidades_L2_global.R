# =============================================================================
# deteccion_comunidades_habilidades.R
# Detección de comunidades de habilidades vía red de complementariedad
# (método de Alabdulkareem et al. 2018, "Unpacking the polarization of
#  workplace skills")
# Trajan Pirkovic Palma | Tesis Magíster Sociología, PUC Chile
#
# Script AUTOCONTENIDO y SECUNDARIO: no depende de pipeline_CA_habilidades.R
# ni de objetos en memoria. Reconstruye lo que necesita desde cero.
#
# QUÉ HACE (siguiendo a Alabdulkareem et al. 2018):
#   1. Construye la matriz ocupación (ISCO-08) × categoría de habilidad ESCO
#      (Nivel 2, 110 categorías — mismo nivel que la versión vigente del
#      pipeline principal al 13 de julio de 2026).
#   2. Calcula la Ventaja Comparativa Revelada (RCA) de cada categoría en
#      cada ocupación, siguiendo la lógica de Hidalgo & Hausmann (2009) que
#      Alabdulkareem et al. adoptan para definir qué habilidades son
#      "efectivamente" usadas por una ocupación (RCA > 1 → habilidad efectiva).
#   3. A partir de la matriz binaria de habilidades efectivas, construye la
#      red de COMPLEMENTARIEDAD entre categorías de habilidad: el peso de la
#      arista entre dos categorías es el coeficiente phi (correlación de
#      Pearson para variables binarias) de su co-ocurrencia entre ocupaciones.
#      Este es el mismo tipo de red que Alabdulkareem llama "skill network".
#   4. Aplica detección de comunidades (Louvain, maximización de modularidad)
#      sobre esa red — el mismo procedimiento que usan los autores para
#      identificar agrupamientos de habilidades.
#   5. Compara las comunidades detectadas con el espacio de Análisis de
#      Correspondencias (CA) ya usado en la tesis: el CA entrega coordenadas
#      no solo para las ocupaciones (filas) sino también para las categorías
#      de habilidad (columnas), lo que permite verificar si las comunidades
#      de la red coinciden espacialmente con la estructura del CA.
#   6. Caracteriza cada comunidad por su ISEI implícito promedio (vía el
#      mismo modelo auxiliar lm(isei ~ Dim1) del pipeline principal), para
#      replicar el hallazgo central de Alabdulkareem: los clusters de
#      habilidades difieren sistemáticamente en su nivel de prestigio/salario
#      asociado ("polarización").
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggrepel)   # etiquetas de texto que se auto-separan para no solaparse
  library(readr)
  library(FactoMineR)
  library(igraph)
})

# NOTA: la semilla NO se fija aquí. Se fija inmediatamente antes de
# cluster_louvain() (Paso 5) para que el resultado no dependa de cuántas
# operaciones aleatorias se hayan ejecutado antes en el script.

DATA_DIR      <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data/esco"   # CSV de ESCO
CROSSWALK_DIR <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data"        # gp_crosswalk.csv (fuente única)
OUT_DIR       <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output"
p  <- function(f) file.path(DATA_DIR, f)
po <- function(f) file.path(OUT_DIR, f)

# ── PARÁMETROS AJUSTABLES ─────────────────────────────────────────────────
RCA_THRESHOLD    <- 1     # umbral estándar de Hidalgo-Hausmann para "habilidad efectiva"
MIN_COVERAGE_OCC <- 0     # mínimo de ocupaciones con RCA>1 para retener una categoría
                          # (poner en 30, p.ej., para replicar el filtro L2f=89 usado antes)
PHI_MIN_EDGE     <- 0     # umbral mínimo de phi para retener una arista (0 = solo positivas)

# =============================================================================
# PASO 1 — MATRIZ ISCO-08 × CATEGORÍA DE HABILIDAD (NIVEL 2 ESCO)
# =============================================================================

cat("=== PASO 1: Matriz ISCO x habilidades (Nivel 2 ESCO) ===\n")

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

# IMPORTANTE: una habilidad individual (skillUri en occupationSkillRelations)
# NO es directamente un "Level 3 URI" de skillsHierarchy — son dos tipos de
# concepto ESCO distintos. Hay que pasar primero por su SkillGroup inmediato
# (broaderRelationsSkillPillar_es.csv), exactamente igual que en el pipeline
# principal. Omitir este paso intermedio deja casi todo sin emparejar y
# produce una matriz degenerada (de ahí el error de CA() reportado).
br <- read_csv(p("broaderRelationsSkillPillar_es.csv"),
               col_types = cols(.default = "c"), show_col_types = FALSE)

# Paso A: habilidad individual -> su SkillGroup padre directo
skill_to_group <- br |>
  filter(conceptType == "KnowledgeSkillCompetence", broaderType == "SkillGroup") |>
  select(skill_uri = conceptUri, group_uri = broaderUri) |>
  distinct()

# Paso B: SkillGroup -> su categoría L2 (puede ser L3 con L2 encima, o L2 directo)
group_to_L2 <- bind_rows(
  skill_hier |> filter(!is.na(`Level 3 URI`), !is.na(`Level 2 code`)) |>
    select(group_uri = `Level 3 URI`, L2_code = `Level 2 code`,
           L2_label = `Level 2 preferred term`),
  skill_hier |> filter(!is.na(`Level 2 URI`), !is.na(`Level 2 code`)) |>
    select(group_uri = `Level 2 URI`, L2_code = `Level 2 code`,
           L2_label = `Level 2 preferred term`)
) |> distinct(group_uri, .keep_all = TRUE)

# Paso C: unir A y B para tener directamente habilidad -> L2
skill_L2_map <- skill_to_group |>
  left_join(group_to_L2, by = "group_uri") |>
  filter(!is.na(L2_code)) |>
  distinct(skill_uri, L2_code, .keep_all = TRUE)

# Solo relaciones "essential" (igual que en el pipeline principal)
osr_essential <- osr |> filter(relationType == "essential", !is.na(isco4))

osr_L2 <- osr_essential |>
  left_join(skill_L2_map, by = c("skillUri" = "skill_uri"),
            relationship = "many-to-many") |>
  filter(!is.na(L2_code))

mat_wide <- osr_L2 |>
  count(isco4, L2_code, name = "n_skills") |>
  pivot_wider(names_from = L2_code, values_from = n_skills, values_fill = 0) |>
  column_to_rownames("isco4")

mat_ca <- as.matrix(mat_wide[, colSums(mat_wide) > 0])
mat_ca <- mat_ca[rowSums(mat_ca) > 0, ]

cat("Matriz ISCO x L2:", nrow(mat_ca), "x", ncol(mat_ca), "\n")

# Etiquetas legibles de cada categoría L2 (para los gráficos)
label_L2 <- skill_L2_map |> distinct(L2_code, L2_label) |>
  filter(L2_code %in% colnames(mat_ca))


# =============================================================================
# PASO 2 — CA (mismo espacio del pipeline principal) PARA COMPARACIÓN
# =============================================================================

cat("\n=== PASO 2: CA sobre la misma matriz, para comparar con las comunidades ===\n")

ca_result <- CA(mat_ca, ncp = 5, graph = FALSE)

# Coordenadas de FILAS (ocupaciones ISCO) — se usan más abajo para el ISEI implícito
ca_row_coords <- as.data.frame(ca_result$row$coord) |>
  rownames_to_column("isco4") |>
  mutate(isco4 = as.integer(isco4)) |>
  rename(Dim1 = `Dim 1`, Dim2 = `Dim 2`)

# Coordenadas de COLUMNAS (categorías de habilidad L2) — el CA proyecta ambos
# conjuntos en el mismo espacio, por lo que esto permite comparar directamente
# la posición espacial de cada categoría con la comunidad de red a la que
# pertenece.
ca_col_coords <- as.data.frame(ca_result$col$coord) |>
  rownames_to_column("L2_code") |>
  rename(Dim1_skill = `Dim 1`, Dim2_skill = `Dim 2`)

eig <- ca_result$eig
cat(sprintf("Var. explicada CA — Dim1: %.1f%% | Dim2: %.1f%%\n", eig[1,2], eig[2,2]))


# =============================================================================
# PASO 3 — VENTAJA COMPARATIVA REVELADA (RCA) Y MATRIZ BINARIA DE
#           "HABILIDADES EFECTIVAS" (Hidalgo & Hausmann 2009; Alabdulkareem
#           et al. 2018 la usan exactamente así para definir el bipartito
#           ocupación-habilidad antes de proyectar la red de habilidades)
# =============================================================================

cat("\n=== PASO 3: Ventaja Comparativa Revelada (RCA) ===\n")

# RCA_oc = (n_oc / sum_c n_oc) / (sum_o n_oc / sum_{o,c} n_oc)
# Es decir: participación de la categoría c en el perfil de la ocupación o,
# relativa a la participación de esa categoría en el total de la economía
# ocupacional. RCA > 1 significa que la ocupación usa esa categoría de
# habilidad "más de lo esperado" dado su peso agregado.

row_totals   <- rowSums(mat_ca)
col_totals   <- colSums(mat_ca)
grand_total  <- sum(mat_ca)

expected <- outer(row_totals, col_totals) / grand_total
rca_mat  <- mat_ca / expected

B <- (rca_mat > RCA_THRESHOLD) * 1L   # matriz binaria de habilidades "efectivas"

n_efectivas_prom <- mean(rowSums(B))
cat("N° promedio de categorías 'efectivas' (RCA>1) por ocupación:",
    round(n_efectivas_prom, 1), "de", ncol(B), "\n")

cobertura_categoria <- colSums(B)
categorias_validas  <- names(cobertura_categoria)[cobertura_categoria >= MIN_COVERAGE_OCC]
B <- B[, categorias_validas]
cat("Categorías retenidas tras filtro de cobertura (>=", MIN_COVERAGE_OCC,
    "ocupaciones):", ncol(B), "de", ncol(mat_ca), "\n")


# =============================================================================
# PASO 4 — RED DE COMPLEMENTARIEDAD DE HABILIDADES (coeficiente phi)
# =============================================================================

cat("\n=== PASO 4: Red de complementariedad (coeficiente phi) ===\n")

# El coeficiente phi es la correlación de Pearson aplicada a dos variables
# binarias. Para cada par de categorías (i, j), phi_ij mide si tienden a
# ser "efectivas" en las mismas ocupaciones más de lo esperado por azar.
# Esta es la definición operacional de complementariedad de Alabdulkareem
# et al. (2018): dos habilidades son complementarias si co-ocurren como
# "efectivas" en el mismo conjunto de ocupaciones más de lo esperado.

phi_mat <- cor(B, method = "pearson")   # equivalente al coef. phi para 0/1
diag(phi_mat) <- 0

cat("Rango de phi:", round(range(phi_mat), 3), "\n")

# Construir el grafo: nodos = categorías de habilidad, aristas = phi > umbral
edges_df <- as.data.frame(as.table(phi_mat)) |>
  rename(from = Var1, to = Var2, phi = Freq) |>
  filter(as.character(from) < as.character(to),   # evitar duplicados (grafo no dirigido)
         phi > PHI_MIN_EDGE)

cat("Aristas retenidas (phi >", PHI_MIN_EDGE, "):", nrow(edges_df),
    "de", choose(ncol(B), 2), "pares posibles\n")

g <- graph_from_data_frame(edges_df, directed = FALSE,
                            vertices = data.frame(name = colnames(B)))
E(g)$weight <- edges_df$phi


# =============================================================================
# PASO 5 — DETECCIÓN DE COMUNIDADES (Louvain / maximización de modularidad)
# =============================================================================

cat("\n=== PASO 5: Detección de comunidades (Louvain) ===\n")

# Semilla fijada inmediatamente antes de cluster_louvain(): entre este punto
# y la carga de librerías no se ejecutó ningún paso con componente aleatoria
# (CA(), el cálculo de RCA y la matriz phi son deterministas), así que este
# cambio de posición no altera el resultado ya obtenido; solo blinda la
# reproducibilidad ante futuros pasos aleatorios que se agreguen antes.
set.seed(2025)
comunidades <- cluster_louvain(g, weights = E(g)$weight)

tabla_comunidades <- tibble(
  L2_code   = names(membership(comunidades)),
  comunidad = as.integer(membership(comunidades))
) |>
  left_join(label_L2, by = "L2_code") |>
  left_join(ca_col_coords, by = "L2_code")

cat("N° de comunidades detectadas:", length(unique(tabla_comunidades$comunidad)), "\n")
cat("Modularidad (Q):", round(modularity(comunidades), 3), "\n")
cat("Tamaño de cada comunidad:\n")
print(table(tabla_comunidades$comunidad))

cat("\nCategorías representativas por comunidad (hasta 8 por comunidad):\n")
tabla_comunidades |>
  group_by(comunidad) |>
  slice_head(n = 8) |>
  select(comunidad, L2_code, L2_label) |>
  print(n = 100)

write_csv(tabla_comunidades, po("comunidades_habilidades.csv"))


# =============================================================================
# PASO 6 — CARACTERIZACIÓN: ¿LAS COMUNIDADES DIFIEREN EN PRESTIGIO IMPLÍCITO?
#          (replica el hallazgo central de "polarización" de Alabdulkareem)
# =============================================================================

cat("\n=== PASO 6: ISEI implícito por comunidad (polarización) ===\n")

# Crosswalk de las 27 posiciones del GP con ISEI-08 (mismo usado en el
# pipeline principal), para entrenar el modelo auxiliar isei ~ Dim1.
# gp_crosswalk: fuente ÚNICA centralizada (data/gp_crosswalk.csv), la misma
# que usan pipeline_CA_habilidades.R y deteccion_comunidades_L3.R. Antes esta
# tabla vivía hardcodeada por separado en cada script — se centralizó el
# 20-jul-2026 para eliminar el riesgo de que una corrección (ej. los códigos
# ISCO de Q0104/Q0116) quedara aplicada en un script y no en otro.
gp_crosswalk <- read_csv(file.path(CROSSWALK_DIR, "gp_crosswalk.csv"), show_col_types = FALSE) |>
  select(var, isco4, isei)
# NOTA (13 julio 2026): ISEI actualizado con los valores oficiales de
# Ganzeboom (2010) (isei08_ganzeboom2010.csv). Los valores previos eran
# aproximaciones expertas, no la fuente oficial. Esto solo cambia el
# "ISEI implícito" estimado para cada comunidad de habilidades (vía
# lm(isei ~ Dim1)); no cambia la detección de comunidades en sí, que
# depende únicamente de la red de complementariedad (RCA + phi + Louvain),
# no del ISEI.

gp_ca <- gp_crosswalk |> left_join(ca_row_coords, by = "isco4") |> filter(!is.na(Dim1))
isei_from_dim1 <- lm(isei ~ Dim1, data = gp_ca)
cat("R2 de lm(isei ~ Dim1) sobre", nrow(gp_ca), "posiciones GP:",
    round(summary(isei_from_dim1)$r.squared, 3), "\n")

# Aplicar el mismo eje (Dim1) a las categorías de habilidad para estimar
# su "ISEI implícito": el prestigio típico de las ocupaciones que más
# recurren a esa categoría de habilidad.
tabla_comunidades <- tabla_comunidades |>
  mutate(isei_implicito = predict(isei_from_dim1,
                                   newdata = data.frame(Dim1 = Dim1_skill)))

resumen_comunidades <- tabla_comunidades |>
  group_by(comunidad) |>
  summarise(
    n_categorias    = n(),
    isei_medio      = mean(isei_implicito, na.rm = TRUE),
    isei_de         = sd(isei_implicito, na.rm = TRUE),
    Dim1_medio      = mean(Dim1_skill, na.rm = TRUE),
    Dim2_medio      = mean(Dim2_skill, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(isei_medio))

cat("\n=== Resumen de comunidades ordenado por ISEI implícito ===\n")
print(resumen_comunidades)
write_csv(resumen_comunidades, po("resumen_comunidades.csv"))


# =============================================================================
# PASO 7 — VISUALIZACIONES
# =============================================================================

cat("\n=== PASO 7: Gráficos ===\n")

# ── PASO EXTRA — VER EN CONSOLA: tabla legible de nodos (código, nombre,
#    comunidad, grado en la red). Útil para leer las 110 etiquetas como texto
#    en vez de tener que hacer zoom en el gráfico.
grado_nodos <- degree(g)

tabla_nodos_consola <- tibble(
  L2_code   = names(grado_nodos),
  grado     = as.integer(grado_nodos)
) |>
  left_join(tabla_comunidades |> select(L2_code, comunidad, L2_label),
            by = "L2_code") |>
  arrange(comunidad, desc(grado))

cat("\n=== Tabla de nodos por comunidad (ordenados por grado en la red) ===\n")
print(tabla_nodos_consola, n = nrow(tabla_nodos_consola))

# Vista rápida: solo los 5 nodos más conectados de cada comunidad (los
# "hub" que probablemente definen mejor el sentido sustantivo del cluster)
cat("\n=== Los 5 nodos más conectados por comunidad ===\n")
tabla_nodos_consola |>
  group_by(comunidad) |>
  slice_max(grado, n = 5) |>
  print(n = 100)


layout_xy <- as.data.frame(layout_with_fr(g))
names(layout_xy) <- c("x", "y")
layout_xy$L2_code <- V(g)$name

nodos_plot <- layout_xy |> left_join(tabla_comunidades, by = "L2_code")

aristas_plot <- edges_df |>
  left_join(layout_xy, by = c("from" = "L2_code")) |>
  rename(x_from = x, y_from = y) |>
  left_join(layout_xy, by = c("to" = "L2_code")) |>
  rename(x_to = x, y_to = y)

# Etiqueta corta y legible por nodo: código + primeras palabras del nombre
# (el nombre completo de algunas categorías es muy largo para caber en el
# gráfico; se trunca a ~28 caracteres y se añade "…" si se cortó).
nodos_plot <- nodos_plot |>
  mutate(
    etiqueta = if_else(
      nchar(L2_label) > 28,
      paste0(L2_code, " ", substr(L2_label, 1, 28), "…"),
      paste0(L2_code, " ", L2_label)
    )
  )

p_red <- ggplot() +
  geom_segment(data = aristas_plot,
               aes(x = x_from, y = y_from, xend = x_to, yend = y_to, alpha = phi),
               color = "grey60", linewidth = 0.2) +
  geom_point(data = nodos_plot, aes(x = x, y = y, color = factor(comunidad)),
             size = 2.5) +
  ggrepel::geom_text_repel(
    data = nodos_plot,
    aes(x = x, y = y, label = etiqueta, color = factor(comunidad)),
    size = 2.3, max.overlaps = Inf, segment.size = 0.15, segment.alpha = 0.5,
    box.padding = 0.25, point.padding = 0.15, min.segment.length = 0,
    show.legend = FALSE
  ) +
  scale_alpha_continuous(range = c(0.05, 0.5)) +
  labs(title = "Red de complementariedad de habilidades (Alabdulkareem et al. 2018)",
       subtitle = sprintf("Louvain: %d comunidades, modularidad Q=%.3f",
                           length(unique(nodos_plot$comunidad)), modularity(comunidades)),
       color = "Comunidad", alpha = "phi") +
  theme_void(base_size = 11) +
  theme(legend.position = "right")

# Tamaño ampliado (14x11) porque con 110 etiquetas el gráfico necesita más
# espacio para que ggrepel pueda separarlas sin amontonarlas.
ggsave(po("fig_red_comunidades_habilidades.pdf"), p_red, width = 14, height = 11)
ggsave(po("fig_red_comunidades_habilidades_alta_res.png"), p_red, width = 16, height = 13, dpi = 300)

# 7b. Comparación espacial: comunidades de red proyectadas en el espacio CA --
p_ca_comunidades <- ggplot(tabla_comunidades,
                            aes(x = Dim1_skill, y = Dim2_skill, color = factor(comunidad))) +
  geom_point(size = 2.5, alpha = 0.85) +
  labs(title = "Comunidades de habilidades proyectadas en el espacio CA",
       subtitle = "¿Coinciden los clusters de la red con la estructura del CA?",
       x = "Dim1 (CA, categorías de habilidad)",
       y = "Dim2 (CA, categorías de habilidad)",
       color = "Comunidad (Louvain)") +
  theme_minimal(base_size = 12)

ggsave(po("fig_comunidades_en_espacio_CA.pdf"), p_ca_comunidades, width = 9, height = 7)

# 7c. Polarización: ISEI implícito por comunidad -----------------------------
p_polarizacion <- ggplot(tabla_comunidades,
                          aes(x = reorder(factor(comunidad), isei_implicito, FUN = median),
                              y = isei_implicito, fill = factor(comunidad))) +
  geom_boxplot(show.legend = FALSE, alpha = 0.8) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
  labs(title = "Polarización: ISEI implícito por comunidad de habilidades",
       subtitle = "Réplica del hallazgo central de Alabdulkareem et al. (2018)",
       x = "Comunidad (Louvain)", y = "ISEI implícito (vía Dim1 del CA)") +
  theme_minimal(base_size = 12)

ggsave(po("fig_polarizacion_comunidades.pdf"), p_polarizacion, width = 9, height = 6)

# =============================================================================
# PASO 8 — VERIFICACIÓN CRÍTICA: ¿SON LAS COMUNIDADES SEÑAL O RUIDO?
#          (agregado tras la revisión del 13 de julio; verifica que la
#           comunidad agro-bio-legal no sea artefacto de baja cobertura)
# =============================================================================

cat("\n=== PASO 8: Verificación crítica de las comunidades ===\n")

# 8a. Cobertura ocupacional de cada categoría --------------------------------
# ¿Cuántas ocupaciones tienen RCA>1 en cada categoría? Categorías con muy
# pocas ocupaciones "activas" producen coeficientes phi inestables: con pocos
# unos en ambas columnas, una sola coincidencia puede inflar la correlación.
cobertura_tab <- tibble(
  L2_code   = colnames(B),
  cobertura = colSums(B)
) |>
  left_join(tabla_comunidades |> select(L2_code, comunidad, L2_label),
            by = "L2_code") |>
  arrange(comunidad, desc(cobertura))

cat("\nDistribución general de cobertura (n ocupaciones con RCA>1 por categoría):\n")
print(summary(cobertura_tab$cobertura))

cat("\nCobertura por comunidad (mediana y rango):\n")
cobertura_tab |>
  group_by(comunidad) |>
  summarise(n_cat = n(),
            cobertura_mediana = median(cobertura),
            cobertura_min = min(cobertura),
            cobertura_max = max(cobertura),
            .groups = "drop") |>
  print()

write_csv(cobertura_tab, po("cobertura_categorias.csv"))

# 8b. Cohesión interna: phi promedio dentro de cada comunidad ----------------
# Si una comunidad fuera ruido, su phi promedio interno debería ser BAJO
# relativo a las demás. Verificación clave para la comunidad agro-bio-legal:
# en la corrida del 13 de julio resultó ser la MÁS cohesionada (phi=0.193),
# descartando la hipótesis de artefacto.
calc_cohesion <- function(com_id) {
  miembros <- tabla_comunidades$L2_code[tabla_comunidades$comunidad == com_id]
  sub      <- phi_mat[miembros, miembros]
  vals     <- sub[upper.tri(sub)]
  tibble(
    comunidad       = com_id,
    n_cat           = length(miembros),
    phi_medio_intra = mean(vals),
    phi_mediana     = median(vals),
    phi_min         = min(vals),
    pct_pares_neg   = 100 * mean(vals < 0)
  )
}

cohesion_tab <- map_dfr(sort(unique(tabla_comunidades$comunidad)), calc_cohesion)

cat("\nCohesión interna por comunidad (phi entre pares intra-comunidad):\n")
print(cohesion_tab)
write_csv(cohesion_tab, po("cohesion_comunidades.csv"))

# 8c. Dispersión de estatus dentro de cada comunidad -------------------------
# El contraste clave del hallazgo: cohesión de habilidades alta + dispersión
# de estatus alta en la misma comunidad = complementariedad NO reductible a
# similitud de prestigio.
contraste_tab <- cohesion_tab |>
  left_join(resumen_comunidades |> select(comunidad, isei_medio, isei_de),
            by = "comunidad") |>
  arrange(desc(phi_medio_intra))

cat("\nContraste cohesión de habilidades vs. dispersión de estatus:\n")
print(contraste_tab)
write_csv(contraste_tab, po("contraste_cohesion_estatus.csv"))

cat("\nArchivos generados:\n")
cat("  - comunidades_habilidades.csv (asignación de cada categoría a su comunidad)\n")
cat("  - resumen_comunidades.csv (caracterización por comunidad)\n")
cat("  - cobertura_categorias.csv (n ocupaciones RCA>1 por categoría)\n")
cat("  - cohesion_comunidades.csv (phi promedio intra-comunidad)\n")
cat("  - contraste_cohesion_estatus.csv (tabla central del hallazgo)\n")
cat("  - fig_red_comunidades_habilidades.pdf\n")
cat("  - fig_comunidades_en_espacio_CA.pdf\n")
cat("  - fig_polarizacion_comunidades.pdf\n")
cat("\n=== FIN: DETECCIÓN DE COMUNIDADES DE HABILIDADES ===\n")

# =============================================================================
# NOTAS METODOLÓGICAS
# =============================================================================
# 1. RCA_THRESHOLD=1 y PHI_MIN_EDGE=0 son los valores por defecto más
#    cercanos a Hidalgo-Hausmann/Alabdulkareem, pero conviene correr un
#    análisis de sensibilidad (p.ej. PHI_MIN_EDGE = 0.1, 0.2) para verificar
#    que las comunidades no cambian drásticamente con el umbral.
# 2. MIN_COVERAGE_OCC=0 usa las 110 categorías completas (nivel vigente del
#    pipeline). Si las comunidades resultan inestables por categorías con
#    muy baja cobertura ocupacional, subir este valor (p.ej. a 30) replica
#    el filtro L2f=89 usado en la robustez de julio.
# 3. El "ISEI implícito" de una categoría de habilidad es una extrapolación:
#    el modelo isei~Dim1 se entrena sobre 27 ocupaciones (posiciones del GP)
#    y se aplica sobre la coordenada Dim1 de las categorías de habilidad.
#    Es razonable porque Dim1 es un eje compartido en el mismo espacio CA,
#    pero hereda la misma fragilidad ya señalada en el informe ejecutivo
#    (R² bajo, n pequeño) — no tratar el ISEI implícito por comunidad como
#    una medida precisa, sino como una tendencia ordinal.
# =============================================================================
