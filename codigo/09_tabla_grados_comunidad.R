# =============================================================================
# 14_tabla_grados_completa.R
#
# Calcula el grado (n de conexiones phi>0) de las 110 categorias de habilidad
# en la red RM, y las ordena de mayor a menor DENTRO de cada una de las 4
# comunidades (particion Leiden ya validada). Complementa el top-5 por
# comunidad ya calculado en 13_imputar_comunidades_ocupacion.R con el listado
# completo, para poder discriminar con mas detalle dentro de cada comunidad.
#
# Script AUTOCONTENIDO. Misma construccion de mat_rm y misma particion que
# 11/12/13 (verbatim de 06_robustez_comunidades.R). SALIDA: consola + CSV.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(igraph)
})
options(dplyr.summarise.inform = FALSE)

DATA_DIR      <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data/esco"
CROSSWALK_DIR <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data"
OUT_DIR       <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output"
p  <- function(f) file.path(DATA_DIR, f)
pc <- function(f) file.path(CROSSWALK_DIR, f)
po <- function(f) file.path(OUT_DIR, "comunidades_por_ocupacion", f)
dir.create(po(""), showWarnings = FALSE, recursive = TRUE)

COL_ISCO_RM        <- "oficio_codigo"
COL_ISCO_CORR_ORIG <- "isco4_casen"
COL_ISCO_CORR_NEW  <- "isco4_corregido"
RCA_THRESHOLD    <- 1
MIN_COVERAGE_OCC <- 0
SOLO_ESENCIALES  <- TRUE
PHI_MIN_EDGE     <- 0
SEMILLA          <- 2025

# Etiquetas ya bautizadas (ver reporte_bautizo_comunidades.pdf). El numero de
# comunidad (1-4) depende del orden interno de igraph en esta corrida; se
# reasigna mas abajo haciendo match por contenido (categoria ancla), no por
# ID, siguiendo la convencion ya establecida en el resto del pipeline.
ETIQUETAS <- c(
  "coordinacion_cuidado"  = "Coordinacion-cuidado",
  "tecnico_manual"        = "Tecnico-manual",
  "analitico_simbolico"   = "Analitico-simbolico",
  "agro_bio_legal"        = "Agro-bio-legal"
)
ANCLA_COORDINACION <- "S4.9"   # tomar decisiones
ANCLA_TECNICO      <- "053"    # ciencias fisicas
ANCLA_ANALITICO    <- "S2.3"   # gestionar informacion
ANCLA_AGRO         <- "081"    # agricultura

leer <- function(ruta) {
  if (!file.exists(ruta)) stop(sprintf("No existe el archivo:\n  %s", ruta), call. = FALSE)
  suppressWarnings(read_csv(ruta, col_types = cols(.default = "c"),
                            show_col_types = FALSE, progress = FALSE))
}

# =============================================================================
# 1. RECONSTRUCCION DE LA MATRIZ RM Y LA RED (verbatim de 06/13)
# =============================================================================

cat("\n=== 1. RECONSTRUCCION DE LA MATRIZ Y LA RED RM ===\n")

osr        <- leer(p("occupationSkillRelations_es.csv"))
occ_raw    <- leer(p("occupations_es.csv"))
skill_hier <- leer(p("skillsHierarchy_es.csv"))
br         <- leer(p("broaderRelationsSkillPillar_es.csv"))
rm_casen   <- leer(pc("ocupaciones_rm_casen2024.csv"))
corr       <- leer(pc("correcciones_isco_casen.csv"))

skill_to_group <- br %>%
  filter(conceptType == "KnowledgeSkillCompetence", broaderType == "SkillGroup") %>%
  select(skill_uri = conceptUri, group_uri = broaderUri) %>% distinct()

group_to_L2 <- bind_rows(
  skill_hier %>% filter(!is.na(`Level 3 URI`), !is.na(`Level 2 code`)) %>%
    select(group_uri = `Level 3 URI`, L2_code = `Level 2 code`),
  skill_hier %>% filter(!is.na(`Level 2 URI`), !is.na(`Level 2 code`)) %>%
    select(group_uri = `Level 2 URI`, L2_code = `Level 2 code`)
) %>% distinct(group_uri, .keep_all = TRUE)

# etiqueta legible de cada categoria L2, para la tabla final
L2_labels <- bind_rows(
  skill_hier %>% filter(!is.na(`Level 3 code`)) %>%
    select(L2_code = `Level 3 code`, L2_label = `Level 3 preferred term`),
  skill_hier %>% filter(!is.na(`Level 2 code`)) %>%
    select(L2_code = `Level 2 code`, L2_label = `Level 2 preferred term`)
) %>% distinct(L2_code, .keep_all = TRUE)

skill_cat_map <- skill_to_group %>%
  left_join(group_to_L2, by = "group_uri") %>%
  filter(!is.na(L2_code)) %>% distinct(skill_uri, L2_code)

occ_isco <- occ_raw %>%
  transmute(occupationUri = conceptUri, isco4 = suppressWarnings(as.integer(iscoGroup))) %>%
  filter(!is.na(isco4))

osr_f <- osr %>%
  filter(if (SOLO_ESENCIALES) relationType == "essential" else TRUE) %>%
  select(occupationUri, skillUri) %>% distinct()

hab_cat <- skill_cat_map %>%
  filter(skill_uri %in% osr_f$skillUri) %>%
  select(skillUri = skill_uri, L2_code)

mat_global <- osr_f %>%
  inner_join(hab_cat, by = "skillUri", relationship = "many-to-many") %>%
  inner_join(occ_isco, by = "occupationUri", relationship = "many-to-many") %>%
  count(isco4, L2_code, name = "n_skills") %>%
  pivot_wider(names_from = L2_code, values_from = n_skills, values_fill = 0) %>%
  column_to_rownames("isco4") %>% as.matrix()
mat_global <- mat_global[, colSums(mat_global) > 0, drop = FALSE]
mat_global <- mat_global[rowSums(mat_global) > 0, , drop = FALSE]

corr_map <- corr %>%
  transmute(isco4_orig = suppressWarnings(as.integer(.data[[COL_ISCO_CORR_ORIG]])),
            isco4_corr = suppressWarnings(as.integer(.data[[COL_ISCO_CORR_NEW]]))) %>%
  filter(!is.na(isco4_orig))

universo_rm <- rm_casen %>%
  mutate(isco4_orig = suppressWarnings(as.integer(.data[[COL_ISCO_RM]]))) %>%
  left_join(corr_map, by = "isco4_orig") %>%
  mutate(isco4 = coalesce(isco4_corr, isco4_orig)) %>%
  filter(!is.na(isco4)) %>% pull(isco4) %>% unique()

mat_rm <- mat_global[rownames(mat_global) %in% as.character(universo_rm), , drop = FALSE]
mat_rm <- mat_rm[, colSums(mat_rm) > 0, drop = FALSE]

esperado <- outer(rowSums(mat_rm), colSums(mat_rm)) / sum(mat_rm)
B <- (mat_rm / esperado > RCA_THRESHOLD) * 1L
B <- B[, colSums(B) >= MIN_COVERAGE_OCC, drop = FALSE]
var_ok <- apply(B, 2, function(x) length(unique(x)) > 1)
B <- B[, var_ok, drop = FALSE]

phi_mat <- cor(B, method = "pearson"); diag(phi_mat) <- 0
edges_df <- as.data.frame(as.table(phi_mat)) %>%
  rename(from = Var1, to = Var2, phi = Freq) %>%
  filter(as.character(from) < as.character(to), phi > PHI_MIN_EDGE)

g <- graph_from_data_frame(edges_df, directed = FALSE,
                            vertices = data.frame(name = colnames(B)))
E(g)$weight <- edges_df$phi

cat(sprintf("  Red RM: %d nodos, %d aristas\n", vcount(g), ecount(g)))

# =============================================================================
# 2. PARTICION LEIDEN Y REASIGNACION DE ETIQUETAS POR CONTENIDO
# =============================================================================

cat("\n=== 2. PARTICION LEIDEN Y ETIQUETAS ===\n")

set.seed(SEMILLA)
particion <- cluster_leiden(g, objective_function = "modularity",
                             weights = E(g)$weight, n_iterations = 10)
membership_L2 <- setNames(as.integer(membership(particion)), names(membership(particion)))

# mapa ID numerico (arbitrario) -> etiqueta bautizada, via las categorias ancla
id_a_etiqueta <- c(
  setNames(ETIQUETAS[["coordinacion_cuidado"]], membership_L2[[ANCLA_COORDINACION]]),
  setNames(ETIQUETAS[["tecnico_manual"]],       membership_L2[[ANCLA_TECNICO]]),
  setNames(ETIQUETAS[["analitico_simbolico"]],  membership_L2[[ANCLA_ANALITICO]]),
  setNames(ETIQUETAS[["agro_bio_legal"]],       membership_L2[[ANCLA_AGRO]])
)
stopifnot("Las 4 anclas deben caer en comunidades distintas" = length(unique(names(id_a_etiqueta))) == 4)

etiqueta_de_cada_categoria <- id_a_etiqueta[as.character(membership_L2)]
names(etiqueta_de_cada_categoria) <- names(membership_L2)  # recupera los L2_code como nombres

# =============================================================================
# 3. GRADO DE CADA CATEGORIA (TODAS, no solo el top-5)
# =============================================================================

grados <- degree(g)   # grado simple (n de conexiones), no ponderado por phi

tabla_completa <- tibble(
  L2_code = names(grados),
  grado = as.integer(grados)
) %>%
  left_join(L2_labels, by = "L2_code") %>%
  mutate(comunidad = etiqueta_de_cada_categoria[L2_code]) %>%
  arrange(comunidad, desc(grado)) %>%
  group_by(comunidad) %>%
  mutate(ranking_en_comunidad = row_number()) %>%
  ungroup() %>%
  select(comunidad, ranking_en_comunidad, L2_code, L2_label, grado)

cat(sprintf("\n  Total categorias: %d\n", nrow(tabla_completa)))
cat("\n=== TABLA COMPLETA, ORDENADA POR COMUNIDAD Y GRADO ===\n\n")
for (com in ETIQUETAS) {
  cat(sprintf("--- %s (n=%d) ---\n", com, sum(tabla_completa$comunidad == com)))
  print(as.data.frame(tabla_completa %>% filter(comunidad == com) %>%
                       select(ranking_en_comunidad, L2_code, L2_label, grado)),
        row.names = FALSE)
  cat("\n")
}

write_csv(tabla_completa, po("tabla_grados_completa_por_comunidad.csv"))
cat("Guardado en:", po("tabla_grados_completa_por_comunidad.csv"), "\n")

# =============================================================================
# DECISIONES METODOLOGICAS
# =============================================================================
# D1. El grado se calcula SIN ponderar por phi (degree() simple: cuenta
#     conexiones, no su fuerza). Esto es distinto del grado ponderado que
#     se podria calcular con strength(g, weights=E(g)$weight). Se eligio el
#     grado simple porque es lo que se uso en el top-5 de 13_imputar_
#     comunidades_ocupacion.R, para que ambas tablas sean comparables. Si se
#     quiere el grado ponderado (mas sensible a la fuerza de las conexiones,
#     no solo a su cantidad), es un cambio de una linea.
# D2. Las etiquetas de comunidad se asignan por CONTENIDO (categoria ancla),
#     no por el ID numerico que devuelve cluster_leiden(), porque ese ID es
#     arbitrario y puede cambiar entre corridas aunque la particion sea
#     identica (convencion ya establecida en el resto del pipeline).
# =============================================================================
