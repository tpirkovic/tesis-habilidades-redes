# =============================================================================
# 11_comparacion_leiden_louvain_RM.R
#
# PREGUNTA: ¿hay diferencias entre Louvain y Leiden al detectar comunidades
# de habilidades sobre las ocupaciones efectivamente presentes en CASEN-RM?
#
# Compara SOLO los dos algoritmos bajo la MISMA funcion objetivo
# (modularidad), que es la comparacion limpia entre metodos. CPM se excluye
# a proposito: no es "Leiden vs. Louvain", es una funcion objetivo distinta,
# y mezclarla confunde diferencias de algoritmo con diferencias de criterio.
#
# Universo principal: RM (filtro de pertenencia CASEN-RM, sin ponderar).
# Universo global: se incluye solo como referencia comparativa.
#
# Script AUTOCONTENIDO. La construccion de mat_global y mat_rm es VERBATIM
# la de 06_robustez_comunidades.R (Pasos 1-3 de ese script).
# SALIDA: consola + CSV.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(igraph)
})
options(dplyr.summarise.inform = FALSE)

# =============================================================================
# 0. RUTAS Y PARAMETROS (identicos a 06_robustez_comunidades.R)
# =============================================================================

DATA_DIR      <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data/esco"
CROSSWALK_DIR <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data"
OUT_DIR       <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output"
p  <- function(f) file.path(DATA_DIR, f)
pc <- function(f) file.path(CROSSWALK_DIR, f)
po <- function(f) file.path(OUT_DIR, "comparacion_leiden", f)
dir.create(po(""), showWarnings = FALSE, recursive = TRUE)

COL_ISCO_RM        <- "oficio_codigo"
COL_ISCO_CORR_ORIG <- "isco4_casen"
COL_ISCO_CORR_NEW  <- "isco4_corregido"

RCA_THRESHOLD    <- 1
MIN_COVERAGE_OCC <- 0
SOLO_ESENCIALES  <- TRUE
PHI_MIN_EDGE     <- 0     # baseline de la grilla de 06 (Decision R1 de ese script)

N_BOOTSTRAP <- 500
SEMILLA     <- 2025

NUCLEO_10  <- c("042","051","052","081","082","083","084","S6.4","S6.9","078")
ANCLA_AGRO <- "081"

stopifnot(
  "igraph debe tener cluster_leiden disponible (paquete igraph >= 1.2.7)" =
    "cluster_leiden" %in% getNamespaceExports("igraph")
)

leer <- function(ruta) {
  if (!file.exists(ruta)) stop(sprintf("No existe el archivo:\n  %s", ruta), call. = FALSE)
  x <- suppressWarnings(read_csv(ruta, col_types = cols(.default = "c"),
                                 show_col_types = FALSE, progress = FALSE))
  message(sprintf("  [OK] %-40s %7d filas | %2d cols", basename(ruta), nrow(x), ncol(x)))
  x
}

# =============================================================================
# 1. CARGA Y MAPEO (identico a 06)
# =============================================================================

cat("\n=== 1. CARGA Y MAPEO ===\n")

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

cat(sprintf("  Habilidades mapeadas : %d | Categorias L2 : %d\n",
            n_distinct(hab_cat$skillUri), n_distinct(hab_cat$L2_code)))

# =============================================================================
# 2. MATRIZ GLOBAL (identico a 06, Paso 2) — referencia
# =============================================================================

mat_global <- osr_f %>%
  inner_join(hab_cat, by = "skillUri", relationship = "many-to-many") %>%
  inner_join(occ_isco, by = "occupationUri", relationship = "many-to-many") %>%
  count(isco4, L2_code, name = "n_skills") %>%
  pivot_wider(names_from = L2_code, values_from = n_skills, values_fill = 0) %>%
  column_to_rownames("isco4") %>% as.matrix()
mat_global <- mat_global[, colSums(mat_global) > 0, drop = FALSE]
mat_global <- mat_global[rowSums(mat_global) > 0, , drop = FALSE]

cat(sprintf("\n  Matriz GLOBAL: %d ocupaciones ISCO-4 x %d categorias L2\n",
            nrow(mat_global), ncol(mat_global)))

# =============================================================================
# 3. MATRIZ RM (identico a 06, Paso 3) — UNIVERSO PRINCIPAL
# =============================================================================
# Filtro de PERTENENCIA: ocupaciones ISCO-4 que existen en CASEN-RM tras
# aplicar las correcciones ya validadas. NO se pondera por magnitud
# poblacional (Decision R3 de 06).

corr_map <- corr %>%
  transmute(isco4_orig = suppressWarnings(as.integer(.data[[COL_ISCO_CORR_ORIG]])),
            isco4_corr = suppressWarnings(as.integer(.data[[COL_ISCO_CORR_NEW]]))) %>%
  filter(!is.na(isco4_orig))

universo_rm <- rm_casen %>%
  mutate(isco4_orig = suppressWarnings(as.integer(.data[[COL_ISCO_RM]]))) %>%
  left_join(corr_map, by = "isco4_orig") %>%
  mutate(isco4 = coalesce(isco4_corr, isco4_orig)) %>%
  filter(!is.na(isco4)) %>%
  pull(isco4) %>% unique()

filas_en_rm <- rownames(mat_global) %in% as.character(universo_rm)
mat_rm <- mat_global[filas_en_rm, , drop = FALSE]
mat_rm <- mat_rm[, colSums(mat_rm) > 0, drop = FALSE]

cat(sprintf("\n  Matriz RM (filtrada, sin ponderar): %d ocupaciones x %d categorias\n",
            nrow(mat_rm), ncol(mat_rm)))
cat(sprintf("  Ocupaciones excluidas por no estar en CASEN-RM: %d\n",
            nrow(mat_global) - nrow(mat_rm)))

# =============================================================================
# 4. RED PHI (misma logica de RCA + phi de detectar() en 06)
# =============================================================================

construir_red_phi <- function(mat, phi_min_edge = PHI_MIN_EDGE,
                               rca_threshold = RCA_THRESHOLD,
                               min_coverage = MIN_COVERAGE_OCC) {
  esperado <- outer(rowSums(mat), colSums(mat)) / sum(mat)
  B <- (mat / esperado > rca_threshold) * 1L
  B <- B[, colSums(B) >= min_coverage, drop = FALSE]
  var_ok <- apply(B, 2, function(x) length(unique(x)) > 1)
  B <- B[, var_ok, drop = FALSE]

  if (ncol(B) < 3) return(NULL)

  phi_mat <- cor(B, method = "pearson"); diag(phi_mat) <- 0
  edges_df <- as.data.frame(as.table(phi_mat)) %>%
    rename(from = Var1, to = Var2, phi = Freq) %>%
    filter(as.character(from) < as.character(to), phi > phi_min_edge)

  if (nrow(edges_df) == 0) return(NULL)

  g <- graph_from_data_frame(edges_df, directed = FALSE,
                              vertices = data.frame(name = colnames(B)))
  E(g)$weight <- edges_df$phi
  g
}

# =============================================================================
# 5. LOS DOS METODOS (misma funcion objetivo: modularidad)
# =============================================================================

correr_louvain <- function(grafo, semilla = SEMILLA) {
  set.seed(semilla)
  particion <- cluster_louvain(grafo, weights = E(grafo)$weight)
  mem <- setNames(as.integer(membership(particion)), names(membership(particion)))
  list(membership = mem, Q = modularity(particion), n_comunidades = length(unique(mem)))
}

correr_leiden <- function(grafo, semilla = SEMILLA) {
  set.seed(semilla)
  particion <- cluster_leiden(grafo, objective_function = "modularity",
                               weights = E(grafo)$weight, n_iterations = 10)
  mem <- setNames(as.integer(membership(particion)), names(membership(particion)))
  list(membership = mem, Q = modularity(grafo, mem, weights = E(grafo)$weight),
       n_comunidades = length(unique(mem)))
}

# Misma logica de evaluar_nucleo() de 06: identificacion por contenido
# (ancla 081 + nucleo de 10 categorias), nunca por ID de comunidad.
evaluar_nucleo <- function(membership, nucleo = NUCLEO_10, ancla = ANCLA_AGRO) {
  presentes <- intersect(nucleo, names(membership))
  if (!(ancla %in% names(membership)) || length(presentes) == 0) {
    return(tibble(nucleo_recuperado = NA_character_, tam_comunidad_ancla = NA_integer_))
  }
  com_ancla <- membership[ancla]
  recuperado <- sum(membership[presentes] == com_ancla)
  tibble(nucleo_recuperado = sprintf("%d/%d", recuperado, length(nucleo)),
         tam_comunidad_ancla = sum(membership == com_ancla))
}

# =============================================================================
# 6. COMPARACION PUNTUAL
# =============================================================================

cat("\n=== 2. COMPARACION LOUVAIN VS. LEIDEN (misma funcion objetivo) ===\n")

comparar <- function(mat, etiqueta) {
  grafo <- construir_red_phi(mat)
  stopifnot("La red construida no debe ser NULL" = !is.null(grafo))

  louvain <- correr_louvain(grafo)
  leiden  <- correr_leiden(grafo)
  ari <- igraph::compare(louvain$membership, leiden$membership, method = "adjusted.rand")

  # cuantos nodos cambian de comunidad entre ambos metodos (tras alinear
  # etiquetas por solapamiento maximo, ya que los IDs son arbitrarios)
  tab <- table(louvain$membership, leiden$membership)
  n_coincidentes <- sum(apply(tab, 1, max))
  pct_coincidencia <- 100 * n_coincidentes / length(louvain$membership)

  resumen <- tibble(
    universo = etiqueta,
    n_ocupaciones = nrow(mat),
    n_nodos_red = vcount(grafo),
    n_aristas = ecount(grafo),
    metodo = c("Louvain", "Leiden"),
    n_comunidades = c(louvain$n_comunidades, leiden$n_comunidades),
    Q = round(c(louvain$Q, leiden$Q), 4)
  ) %>%
    bind_cols(bind_rows(evaluar_nucleo(louvain$membership),
                        evaluar_nucleo(leiden$membership)))

  cat(sprintf("\n--- Universo %s (%d ocupaciones, %d nodos, %d aristas) ---\n",
              etiqueta, nrow(mat), vcount(grafo), ecount(grafo)))
  print(as.data.frame(resumen %>% select(metodo, n_comunidades, Q,
                                          nucleo_recuperado, tam_comunidad_ancla)),
        row.names = FALSE)
  cat(sprintf("  ARI Louvain vs. Leiden : %.4f\n", ari))
  cat(sprintf("  Categorias en la misma comunidad en ambos metodos: %d de %d (%.1f%%)\n",
              n_coincidentes, length(louvain$membership), pct_coincidencia))

  # categorias que cambian de comunidad entre metodos
  if (pct_coincidencia < 100) {
    asignacion <- tibble(L2_code = names(louvain$membership),
                          com_louvain = louvain$membership,
                          com_leiden = leiden$membership[names(louvain$membership)])
    cat("  (ver CSV de asignacion para el detalle categoria por categoria)\n")
  } else {
    asignacion <- tibble(L2_code = names(louvain$membership),
                          com_louvain = louvain$membership,
                          com_leiden = leiden$membership[names(louvain$membership)])
    cat("  Los dos metodos producen particiones equivalentes.\n")
  }

  list(resumen = resumen, ari = ari, pct_coincidencia = pct_coincidencia,
       membership = list(louvain = louvain$membership, leiden = leiden$membership),
       asignacion = asignacion %>% mutate(universo = etiqueta))
}

res_rm     <- comparar(mat_rm, "RM")
res_global <- comparar(mat_global, "GLOBAL")

# =============================================================================
# 7. ROBUSTEZ BOOTSTRAP (500 replicas x 2 metodos x 2 universos)
# =============================================================================

cat(sprintf("\n=== 3. ROBUSTEZ BOOTSTRAP (%d replicas x 2 metodos x 2 universos) ===\n",
            N_BOOTSTRAP))

bootstrap_metodo <- function(mat, funcion_metodo, membership_referencia,
                              n_replicas = N_BOOTSTRAP, semilla = SEMILLA) {
  set.seed(semilla)
  filas <- rownames(mat); n <- length(filas)
  ari_vec <- rep(NA_real_, n_replicas)
  n_com_vec <- rep(NA_integer_, n_replicas)
  degenerados <- 0

  for (i in seq_len(n_replicas)) {
    muestra <- sample(filas, size = n, replace = TRUE)
    submat <- rowsum(mat[muestra, , drop = FALSE], group = muestra)

    subgrafo <- construir_red_phi(submat)
    if (is.null(subgrafo)) { degenerados <- degenerados + 1; next }

    particion <- tryCatch(funcion_metodo(subgrafo), error = function(e) NULL)
    if (is.null(particion) || particion$n_comunidades <= 1) {
      degenerados <- degenerados + 1; next
    }

    comunes <- intersect(names(membership_referencia), names(particion$membership))
    if (length(comunes) < 2) { degenerados <- degenerados + 1; next }

    ari_vec[i] <- igraph::compare(membership_referencia[comunes],
                                   particion$membership[comunes],
                                   method = "adjusted.rand")
    n_com_vec[i] <- particion$n_comunidades
  }

  validos <- ari_vec[!is.na(ari_vec)]
  tibble(
    ari_mediano = round(median(validos), 4),
    ari_ic95_inf = round(quantile(validos, 0.025), 4),
    ari_ic95_sup = round(quantile(validos, 0.975), 4),
    n_comunidades_moda = as.integer(names(sort(table(n_com_vec), decreasing = TRUE))[1]),
    prop_recupera_n_original = round(mean(n_com_vec == length(unique(membership_referencia)),
                                           na.rm = TRUE), 4),
    n_replicas_degeneradas = degenerados
  )
}

tabla_robustez <- bind_rows(
  bind_cols(tibble(universo = "RM", metodo = "Louvain"),
            bootstrap_metodo(mat_rm, correr_louvain, res_rm$membership$louvain)),
  bind_cols(tibble(universo = "RM", metodo = "Leiden"),
            bootstrap_metodo(mat_rm, correr_leiden, res_rm$membership$leiden)),
  bind_cols(tibble(universo = "GLOBAL", metodo = "Louvain"),
            bootstrap_metodo(mat_global, correr_louvain, res_global$membership$louvain)),
  bind_cols(tibble(universo = "GLOBAL", metodo = "Leiden"),
            bootstrap_metodo(mat_global, correr_leiden, res_global$membership$leiden))
)

cat("\n=== Robustez bootstrap ===\n")
print(as.data.frame(tabla_robustez), row.names = FALSE)

# =============================================================================
# 8. EXPORTAR
# =============================================================================

write_csv(bind_rows(res_rm$resumen, res_global$resumen), po("comparacion_louvain_leiden.csv"))
write_csv(bind_rows(res_rm$asignacion, res_global$asignacion), po("asignacion_categorias_por_metodo.csv"))
write_csv(tabla_robustez, po("robustez_louvain_leiden.csv"))

cat("\nArchivos generados en", po(""), ":\n")
cat("  - comparacion_louvain_leiden.csv\n")
cat("  - asignacion_categorias_por_metodo.csv (categoria por categoria, ambos metodos)\n")
cat("  - robustez_louvain_leiden.csv\n")
cat("\n=== FIN ===\n")

# =============================================================================
# DECISIONES METODOLOGICAS
# =============================================================================
# 1. Se compara UNICAMENTE Louvain vs. Leiden bajo la MISMA funcion objetivo
#    (modularidad). Esta es la comparacion limpia entre algoritmos: si se
#    cambia tambien la funcion objetivo (p.ej. a CPM), las diferencias
#    observadas ya no son atribuibles al algoritmo sino al criterio de
#    particion, y la comparacion deja de responder la pregunta planteada.
#    CPM se excluyo de este script por esa razon.
# 2. La construccion de mat_global y mat_rm es VERBATIM la de
#    06_robustez_comunidades.R (Pasos 1-3), incluido el filtro de pertenencia
#    RM sin ponderacion poblacional (Decision R3 de ese script). No se
#    reimplemento nada: se copio para eliminar riesgo de discrepancia.
# 3. construir_red_phi() replica la logica de RCA + phi de detectar() en 06,
#    factorizada aparte para poder alternar Louvain y Leiden sobre la MISMA
#    red sin recalcular RCA/phi de forma independiente por metodo.
# 4. PHI_MIN_EDGE = 0, la fila baseline de la grilla de 06 (Decision R1). La
#    sensibilidad al umbral de phi es responsabilidad de 06; este script
#    compara algoritmos a umbral fijo, no umbrales.
# 5. La misma semilla (2025) se fija inmediatamente antes de cada llamada a
#    cluster_louvain()/cluster_leiden(), de modo que cualquier diferencia
#    observada sea atribuible al algoritmo y no a la inicializacion aleatoria.
# 6. El bootstrap remuestrea las filas ISCO-4 de cada matriz por separado y
#    reconstruye la red completa en cada replica (mismo diseno de
#    05_bootstrap_estabilidad_comunidades.R), extendido a Leiden y aplicado
#    tambien al universo RM.
# 7. La identificacion del nucleo agro-bio-legal usa el ancla "081" y el
#    nucleo de 10 categorias documentado, nunca el ID numerico de comunidad
#    (no es estable entre corridas ni entre algoritmos).
# =============================================================================
