# =============================================================================
# R5_similitud_habilidades_red_vs_ca.R
#
# Robustez de H1/H1b: reemplaza SH_ip (similitud de habilidades ego-posicion,
# construida sobre coordenadas del CA) por dos alternativas fundadas en la
# MISMA red de complementariedad phi que ya sostiene la deteccion de
# comunidades (script 08). Motivacion: el CA ya no es la geometria que
# sustenta el hallazgo central de la tesis (bio-ambiental-legal); conviene
# que el indicador de homofilia de H1 use la misma estructura de red, no una
# reduccion dimensional distinta.
#
#   - PRINCIPAL  -- SH_ip_red: distancia geodesica ponderada por phi entre
#     las categorias esenciales (RCA>1) de la ocupacion de ego y las de cada
#     posicion del generador, sobre el mismo grafo que produce las 4
#     comunidades Leiden. Invertida (mayor distancia = menor similitud).
#   - SENSIBILIDAD -- SH_ip_com: similitud de coseno entre el vector de
#     shares de comunidad (share_com1..4) de ego y el de cada posicion,
#     reusando shares_rm/gp_shares ya calculados por el script 08. Colapsa
#     110 categorias a 4 numeros; es la version "gruesa" del mismo argumento.
#
# Este script NO corre el pipeline principal: reconstruye la red phi
# verbatim (identico a 08_comunidades_por_ocupacion.R, Secciones 1-2), y
# carga base_larga.rds (salida de 04_indicadores_red.R) para obtener SP_ip,
# SH_ip (CA, ya calculado), y los controles del modelo (educ, sexo, edad,
# TamGrupo_p, ID_f).
#
# SALIDA: consola (tabla comparativa de modelos + correlaciones entre las
# tres versiones de SH_ip) + CSV con base_larga aumentada.
#
# Script AUTOCONTENIDO salvo por la dependencia declarada de base_larga.rds.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(igraph)
  library(glmmTMB); library(modelsummary); library(gt)
})
options(dplyr.summarise.inform = FALSE)

# =============================================================================
# 0. RUTAS Y PARAMETROS
# =============================================================================

DATA_DIR         <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data/esco"
CROSSWALK_DIR     <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data"
INTERMEDIATE_DIR  <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output/intermediate"
COMUNIDADES_DIR   <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output/comunidades_por_ocupacion"
OUT_DIR           <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output/robustez"
p  <- function(f) file.path(DATA_DIR, f)
pc <- function(f) file.path(CROSSWALK_DIR, f)
po <- function(f) file.path(OUT_DIR, f)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

COL_ISCO_CORR_ORIG <- "isco4_casen"
COL_ISCO_CORR_NEW  <- "isco4_corregido"

RCA_THRESHOLD   <- 1
MIN_COVERAGE_OCC <- 0
SOLO_ESENCIALES <- TRUE
PHI_MIN_EDGE    <- 0

stopifnot(
  "Falta base_larga.rds (etapa 04). Correr 04_indicadores_red.R primero." =
    file.exists(file.path(INTERMEDIATE_DIR, "base_larga.rds")),
  "Falta imputacion_comunidades.rds (etapa 08)." =
    file.exists(file.path(COMUNIDADES_DIR, "imputacion_comunidades.rds"))
)

leer <- function(ruta) {
  if (!file.exists(ruta)) stop(sprintf("No existe el archivo:\n  %s", ruta), call. = FALSE)
  suppressWarnings(read_csv(ruta, col_types = cols(.default = "c"),
                             show_col_types = FALSE, progress = FALSE))
}

# =============================================================================
# 1. RECONSTRUCCION DE LA RED PHI (verbatim de 08, Secciones 1-2)
# =============================================================================

cat("\n=== 1. RECONSTRUCCION DE LA RED PHI (identica a 08) ===\n")

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
  mutate(isco4_orig = suppressWarnings(as.integer(oficio_codigo))) %>%
  left_join(corr_map, by = "isco4_orig") %>%
  mutate(isco4 = coalesce(isco4_corr, isco4_orig)) %>%
  filter(!is.na(isco4)) %>% pull(isco4) %>% unique()

mat_rm <- mat_global[rownames(mat_global) %in% as.character(universo_rm), , drop = FALSE]
mat_rm <- mat_rm[, colSums(mat_rm) > 0, drop = FALSE]

cat(sprintf("  Matriz RM: %d ocupaciones x %d categorias L2\n", nrow(mat_rm), ncol(mat_rm)))

construir_red_y_B <- function(mat, phi_min_edge = PHI_MIN_EDGE,
                               rca_threshold = RCA_THRESHOLD,
                               min_coverage = MIN_COVERAGE_OCC) {
  esperado <- outer(rowSums(mat), colSums(mat)) / sum(mat)
  B <- (mat / esperado > rca_threshold) * 1L
  B <- B[, colSums(B) >= min_coverage, drop = FALSE]
  var_ok <- apply(B, 2, function(x) length(unique(x)) > 1)
  B <- B[, var_ok, drop = FALSE]

  phi_mat <- cor(B, method = "pearson"); diag(phi_mat) <- 0
  edges_df <- as.data.frame(as.table(phi_mat)) %>%
    rename(from = Var1, to = Var2, phi = Freq) %>%
    filter(as.character(from) < as.character(to), phi > phi_min_edge)

  g <- graph_from_data_frame(edges_df, directed = FALSE,
                              vertices = data.frame(name = colnames(B)))
  E(g)$weight <- edges_df$phi
  list(grafo = g, B = B)
}

red_rm <- construir_red_y_B(mat_rm)
cat(sprintf("  Red RM: %d nodos, %d aristas | B: %d ocupaciones x %d categorias efectivas\n",
            vcount(red_rm$grafo), ecount(red_rm$grafo), nrow(red_rm$B), ncol(red_rm$B)))

n_comp <- components(red_rm$grafo)$no
if (n_comp > 1) {
  warning(sprintf(
    "La red phi tiene %d componentes desconectados: algunos pares ego-posicion \
podran quedar sin distancia geodesica finita (NA). Revisar antes de reportar.",
    n_comp))
}

# =============================================================================
# 2. MATRIZ DE DISTANCIAS GEODESICAS PONDERADAS POR PHI (opcion A, principal)
# =============================================================================
# Peso de arista = 1 - phi: mayor complementariedad (phi alto) => arista mas
# "corta". Se usa la MISMA red que alimenta Leiden (Sec. 3 de 08), no una
# version re-filtrada. Distancia entre categorias sin camino (componentes
# distintos) queda en Inf y se excluye del promedio par a par mas abajo.

cat("\n=== 2. MATRIZ DE DISTANCIAS GEODESICAS (peso = 1 - phi) ===\n")

E(red_rm$grafo)$dist_w <- 1 - E(red_rm$grafo)$weight
D_geo <- distances(red_rm$grafo, weights = E(red_rm$grafo)$dist_w)

cat(sprintf("  Matriz de distancias: %d x %d | pares no finitos: %d de %d\n",
            nrow(D_geo), ncol(D_geo), sum(!is.finite(D_geo)), length(D_geo)))

# Set de categorias esenciales (RCA>1) por ocupacion, para lookup O(1)
cats_por_isco <- apply(red_rm$B, 1, function(fila) names(which(fila == 1)))

distancia_media_par <- function(isco_ego, isco_pos) {
  ce <- cats_por_isco[[as.character(isco_ego)]]
  cp <- cats_por_isco[[as.character(isco_pos)]]
  if (is.null(ce) || is.null(cp) || length(ce) == 0 || length(cp) == 0) return(NA_real_)
  sub <- D_geo[ce, cp, drop = FALSE]
  sub <- sub[is.finite(sub)]
  if (length(sub) == 0) return(NA_real_)
  mean(sub)
}

# =============================================================================
# 3. SHARES DE COMUNIDAD (opcion B, sensibilidad) -- desde el script 08
# =============================================================================

cat("\n=== 3. SHARES DE COMUNIDAD (sensibilidad, desde script 08) ===\n")

comunidades <- readRDS(file.path(COMUNIDADES_DIR, "imputacion_comunidades.rds"))
shares_rm   <- comunidades$shares_rm
gp_shares   <- comunidades$gp_shares
n_com       <- comunidades$n_comunidades
share_cols  <- paste0("share_com", seq_len(n_com))

cos_sim <- function(a, b) {
  if (any(is.na(a)) || any(is.na(b))) return(NA_real_)
  na <- sqrt(sum(a^2)); nb <- sqrt(sum(b^2))
  if (na == 0 || nb == 0) return(NA_real_)
  sum(a * b) / (na * nb)
}

# =============================================================================
# 4. CARGA DE base_larga.rds Y CORRECCION DE isco_ego4
# =============================================================================
# base_larga$isco_ego4 (etapa 04) es el ISCO CRUDO de ego (no corregido: la
# correccion solo se aplicaba en 04 para el join con shares_rm, Accion 5, sin
# propagarse a base_larga). mat_rm/B estan indexados por isco4 YA CORREGIDO,
# asi que aqui hay que aplicar la misma correccion antes de cualquier join.

cat("\n=== 4. CARGA DE base_larga.rds Y CORRECCION DE isco_ego4 ===\n")

base_larga <- readRDS(file.path(INTERMEDIATE_DIR, "base_larga.rds"))

stopifnot(
  "base_larga debe tener isco_ego4, isco4 (posicion), SH_ip, SP_ip, ID_f, n_conocidos" =
    all(c("isco_ego4", "isco4", "SH_ip", "SP_ip", "ID_f", "n_conocidos") %in% names(base_larga))
)

base_larga <- base_larga %>%
  mutate(isco_ego4_int = suppressWarnings(as.integer(isco_ego4))) %>%
  left_join(corr_map, by = c("isco_ego4_int" = "isco4_orig")) %>%
  mutate(isco4_ego_corr = as.character(coalesce(isco4_corr, isco_ego4_int))) %>%
  select(-isco4_corr, -isco_ego4_int)

n_sin_match_B <- sum(!base_larga$isco4_ego_corr %in% rownames(red_rm$B))
cat(sprintf("  Diadas cuyo isco4_ego_corr NO esta en B (quedaran NA en SH_ip_red): %d de %d (%.1f%%)\n",
            n_sin_match_B, nrow(base_larga), 100 * n_sin_match_B / nrow(base_larga)))

# =============================================================================
# 5. CALCULO VECTORIZADO DE SH_ip_red Y SH_ip_com SOBRE PARES UNICOS
# =============================================================================
# En vez de aplicar fila a fila sobre las ~26.000 diadas, se calcula sobre
# los pares UNICOS (isco4_ego_corr x isco4 de posicion, que son solo 27
# posiciones), y se pega de vuelta con left_join. Mas rapido y mas facil de
# auditar (se puede inspeccionar pares_unicos directamente).

cat("\n=== 5. CALCULO DE SH_ip_red Y SH_ip_com SOBRE PARES UNICOS ===\n")

pares_unicos <- base_larga %>%
  distinct(isco4_ego_corr, isco4) %>%
  filter(!is.na(isco4_ego_corr), !is.na(isco4)) %>%
  # FIX: base_larga$isco4 llega como double (heredado de gp_coords en la
  # etapa 04); gp_shares$isco4 y shares_rm$isco4 son character (asi los deja
  # el script 08). Sin esta coercion, los left_join de mas abajo fallan por
  # tipos incompatibles (error visto en la primera corrida de este script).
  mutate(isco4 = as.character(as.integer(isco4)))

cat(sprintf("  Pares unicos ego-posicion a calcular: %d\n", nrow(pares_unicos)))

pares_unicos <- pares_unicos %>%
  rowwise() %>%
  mutate(dist_media = distancia_media_par(isco4_ego_corr, isco4)) %>%
  ungroup() %>%
  mutate(SH_ip_red = -dist_media)  # mismo signo que SH_ip original: mayor = mas similar

# --- shares de comunidad para el mismo set de isco4_ego_corr ---
shares_ego_tbl <- tibble(isco4_ego_corr = unique(pares_unicos$isco4_ego_corr)) %>%
  left_join(shares_rm %>% select(isco4, all_of(share_cols)) %>%
              rename_with(~ paste0(.x, "_e"), all_of(share_cols)),
            by = c("isco4_ego_corr" = "isco4"))

shares_pos_tbl <- gp_shares %>%
  select(isco4, all_of(share_cols)) %>%
  rename_with(~ paste0(.x, "_p"), all_of(share_cols)) %>%
  distinct(isco4, .keep_all = TRUE)

pares_unicos <- pares_unicos %>%
  left_join(shares_ego_tbl, by = "isco4_ego_corr") %>%
  left_join(shares_pos_tbl, by = "isco4") %>%
  rowwise() %>%
  mutate(SH_ip_com = cos_sim(
    c_across(all_of(paste0(share_cols, "_e"))),
    c_across(all_of(paste0(share_cols, "_p")))
  )) %>%
  ungroup()

cat(sprintf("  SH_ip_red calculado: %d de %d pares (resto NA por cobertura de red)\n",
            sum(!is.na(pares_unicos$SH_ip_red)), nrow(pares_unicos)))
cat(sprintf("  SH_ip_com calculado: %d de %d pares\n",
            sum(!is.na(pares_unicos$SH_ip_com)), nrow(pares_unicos)))

# --- pegar de vuelta a base_larga ---
# Mismo fix de tipo que en pares_unicos: isco4 debe quedar como character
# para que la llave del join calce.
base_larga <- base_larga %>%
  mutate(isco4 = as.character(as.integer(isco4))) %>%
  left_join(pares_unicos %>% select(isco4_ego_corr, isco4, SH_ip_red, SH_ip_com),
            by = c("isco4_ego_corr", "isco4"))

# =============================================================================
# 6. CORRELACIONES ENTRE LAS TRES VERSIONES DE SH_ip
# =============================================================================

cat("\n=== 6. CORRELACIONES ENTRE VERSIONES DE SH_ip ===\n")
cor_tbl <- base_larga %>%
  select(SH_ip, SH_ip_red, SH_ip_com) %>%
  cor(use = "pairwise.complete.obs")
print(round(cor_tbl, 3))

# =============================================================================
# 7. RE-AJUSTE DE MODELOS (mismo spec que H1/H1b, cambiando solo SH_ip)
# =============================================================================
# ATENCION -- SUPUESTO A VERIFICAR: la formula de abajo reproduce la
# especificacion descrita para la tabla H1/H1b (VD: n_conocidos, binomial
# negativa multinivel con intercepto aleatorio por ego, controles de
# educacion, tamano del grupo ocupacional, sexo y edad). Este script NO tiene
# acceso a 05_modelos.R, asi que la formula exacta (variables adicionales,
# uso de pesos muestrales, family=nbinom1 vs nbinom2) debe verificarse contra
# ese script antes de reportar los resultados a la comision. <-- AJUSTAR

cat("\n=== 7. MODELOS: SH_ip (CA) vs SH_ip_red (red) vs SH_ip_com (sensibilidad) ===\n")

f_base <- n_conocidos ~ SP_ip + educ + TamGrupo_p + sexo + edad + (1 | ID_f)

m0        <- glmmTMB(f_base, data = base_larga, family = nbinom2())
m1_ca     <- update(m0, . ~ . + SH_ip)
m1_red    <- update(m0, . ~ . + SH_ip_red)
m1_com    <- update(m0, . ~ . + SH_ip_com)
m2_red    <- update(m1_red, . ~ . + SH_ip_red:educ)
m2_com    <- update(m1_com, . ~ . + SH_ip_com:educ)

modelos <- list(
  "M0: solo prestigio"                = m0,
  "M1 CA (original)"                  = m1_ca,
  "M1 red (principal)"                = m1_red,
  "M2 red (+ interaccion educ.)"      = m2_red,
  "M1 comunidades (sensibilidad)"     = m1_com,
  "M2 comunidades (+ interaccion)"    = m2_com
)

tabla_modelos <- modelsummary(
  modelos,
  effects = "fixed", component = "cond",   # fix conocido para glmmTMB nbinom2 (ver notas de pipeline)
  stars = c('*' = .05, '**' = .01, '***' = .001),
  gof_omit = "AIC|BIC|Log.Lik",            # svyglm/glmmTMB no es ML puro comparable via AIC/BIC aqui
  output = "gt"
)
print(tabla_modelos)

# =============================================================================
# 8. EXPORTACION
# =============================================================================

write_csv(
  base_larga %>% select(ID, isco4_ego_corr, isco4, n_conocidos, SP_ip,
                         SH_ip, SH_ip_red, SH_ip_com, educ, sexo, edad, TamGrupo_p),
  po("base_larga_SH_ip_red_com.csv")
)
write_csv(pares_unicos, po("pares_unicos_SH_ip_red_com.csv"))
saveRDS(modelos, po("modelos_SH_ip_red_com.rds"))

cat("\nArchivos generados en", OUT_DIR, ":\n")
cat("  - base_larga_SH_ip_red_com.csv (base de diadas con las 3 versiones de SH_ip)\n")
cat("  - pares_unicos_SH_ip_red_com.csv (27 x n_ego_unicos, auditable directamente)\n")
cat("  - modelos_SH_ip_red_com.rds (lista de 6 modelos glmmTMB)\n")

cat("\n=== FIN: SH_ip RED vs CA vs COMUNIDADES ===\n")

# =============================================================================
# DECISIONES METODOLOGICAS
# =============================================================================
# 1. Peso de arista para la distancia geodesica = 1 - phi (no 1/phi): acota
#    el peso a [0, 1) dado que solo se conservan aristas con phi > 0
#    (PHI_MIN_EDGE = 0, identico a 08), evitando pesos negativos o
#    explosivos que 1/phi produciria cerca de phi -> 0.
# 2. La distancia ego-posicion se define como el PROMEDIO de las distancias
#    geodesicas par a par entre el set de categorias esenciales (RCA>1) de
#    ego y el de la posicion (todas contra todas), no la distancia minima ni
#    la del par de hubs. Es la generalizacion mas directa de "distancia
#    entre nodos del mapa" a comparar dos OCUPACIONES (cada una con varias
#    categorias), y es simetrica con como SH_ip (CA) promedia sobre
#    coordenadas ya agregadas por ocupacion.
# 3. Pares de categorias sin camino en la red (componentes desconectados)
#    se excluyen del promedio en vez de imputarse con un valor arbitrario
#    grande; si TODOS los pares para una diada quedan sin camino, SH_ip_red
#    queda NA para esa diada (se reporta el conteo en Seccion 4).
# 4. SH_ip_com (sensibilidad) usa similitud de COSENO sobre los 4 shares de
#    comunidad, no distancia euclidiana ni Bray-Curtis: coseno es invariante
#    a la magnitud del vector (relevante porque el numero de categorias
#    efectivas por ocupacion varia harto, de 7 a 29 en el GP) y solo compara
#    la FORMA del perfil de comunidades, que es lo sustantivo aqui.
# 5. La correccion de isco_ego4 (Seccion 4) es la MISMA correcciones_isco_
#    casen.csv que usa el pipeline principal (04, Accion 5; 08, matriz RM).
#    No se reaplica ninguna correccion adicional propia de este script.
# 6. PENDIENTE: la formula de la Seccion 7 es una reconstruccion razonada a
#    partir de la tabla H1/H1b ya reportada, no una copia de 05_modelos.R
#    (no disponible para este script). Verificar contra ese script antes de
#    reportar coeficientes/p-valores a la comision -- en particular: uso de
#    pesos muestrales (weight), variables de control adicionales, y family
#    (nbinom1 vs nbinom2).
# =============================================================================
