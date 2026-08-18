# =============================================================================
# 12_diagnostico_correccion_bootstrap.R
#
# PROBLEMA DETECTADO: en 11_comparacion_leiden_louvain_RM.R, la fila de
# Louvain en el universo RM produjo un intervalo de confianza degenerado
# (min = mediana = max = 0.8169), lo que implica que las 500 replicas
# arrojaron el mismo valor de ARI.
#
# HIPOTESIS: set.seed() se ejecuta DENTRO de correr_louvain()/correr_leiden(),
# es decir, dentro del loop del bootstrap. Cada llamada devuelve el generador
# aleatorio al estado de la semilla, por lo que la muestra de la iteracion
# siguiente parte siempre del mismo punto y el remuestreo cae en un punto
# fijo: todas las replicas terminan siendo la misma muestra.
#
# Este script: (1) verifica la hipotesis con evidencia directa, (2) corre la
# version corregida del bootstrap, (3) compara ambos resultados.
#
# AUTOCONTENIDO. Reutiliza la construccion de matrices de
# 06_robustez_comunidades.R. SALIDA: consola + CSV.
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
po <- function(f) file.path(OUT_DIR, "comparacion_leiden", f)
dir.create(po(""), showWarnings = FALSE, recursive = TRUE)

COL_ISCO_RM        <- "oficio_codigo"
COL_ISCO_CORR_ORIG <- "isco4_casen"
COL_ISCO_CORR_NEW  <- "isco4_corregido"

RCA_THRESHOLD    <- 1
MIN_COVERAGE_OCC <- 0
SOLO_ESENCIALES  <- TRUE
PHI_MIN_EDGE     <- 0

N_BOOTSTRAP <- 500
SEMILLA     <- 2025

leer <- function(ruta) {
  if (!file.exists(ruta)) stop(sprintf("No existe el archivo:\n  %s", ruta), call. = FALSE)
  suppressWarnings(read_csv(ruta, col_types = cols(.default = "c"),
                            show_col_types = FALSE, progress = FALSE))
}

# =============================================================================
# 1. RECONSTRUCCION DE MATRICES (verbatim de 06)
# =============================================================================

cat("\n=== 1. RECONSTRUCCION DE MATRICES ===\n")

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
  mutate(isco4_orig = suppressWarnings(as.integer(.data[[COL_ISCO_RM]]))) %>%
  left_join(corr_map, by = "isco4_orig") %>%
  mutate(isco4 = coalesce(isco4_corr, isco4_orig)) %>%
  filter(!is.na(isco4)) %>% pull(isco4) %>% unique()

mat_rm <- mat_global[rownames(mat_global) %in% as.character(universo_rm), , drop = FALSE]
mat_rm <- mat_rm[, colSums(mat_rm) > 0, drop = FALSE]

cat(sprintf("  GLOBAL: %d x %d | RM: %d x %d\n",
            nrow(mat_global), ncol(mat_global), nrow(mat_rm), ncol(mat_rm)))

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
# 2. DIAGNOSTICO: ¿se repiten las muestras del bootstrap?
# =============================================================================
# Reproduce la secuencia exacta de la version con bug (set.seed dentro del
# metodo) y registra el hash de cada muestra. Si la hipotesis es correcta,
# las muestras deben repetirse a partir de alguna iteracion.

cat("\n=== 2. DIAGNOSTICO DEL REMUESTREO ===\n")

# version CON bug: set.seed dentro
louvain_con_seed <- function(grafo, semilla = SEMILLA) {
  set.seed(semilla)
  particion <- cluster_louvain(grafo, weights = E(grafo)$weight)
  setNames(as.integer(membership(particion)), names(membership(particion)))
}
# version SIN bug: no toca la semilla
louvain_sin_seed <- function(grafo) {
  particion <- cluster_louvain(grafo, weights = E(grafo)$weight)
  setNames(as.integer(membership(particion)), names(membership(particion)))
}
leiden_sin_seed <- function(grafo) {
  particion <- cluster_leiden(grafo, objective_function = "modularity",
                               weights = E(grafo)$weight, n_iterations = 10)
  setNames(as.integer(membership(particion)), names(membership(particion)))
}

diagnosticar_muestras <- function(mat, funcion_metodo, etiqueta, n_replicas = 50) {
  set.seed(SEMILLA)
  filas <- rownames(mat); n <- length(filas)
  hashes <- character(n_replicas)
  for (i in seq_len(n_replicas)) {
    muestra <- sample(filas, size = n, replace = TRUE)
    hashes[i] <- paste(sort(table(muestra)), collapse = "-")  # firma de la muestra
    submat <- mat[muestra, , drop = FALSE]
    rownames(submat) <- make.unique(muestra)
    g <- construir_red_phi(submat)
    if (!is.null(g)) invisible(tryCatch(funcion_metodo(g), error = function(e) NULL))
  }
  n_unicas <- length(unique(hashes))
  cat(sprintf("  %-42s muestras distintas: %3d de %3d\n", etiqueta, n_unicas, n_replicas))
  invisible(n_unicas)
}

cat("\n  Con set.seed() DENTRO del metodo (version con bug):\n")
u1 <- diagnosticar_muestras(mat_rm, louvain_con_seed, "RM / Louvain (seed dentro)")

cat("\n  Sin set.seed() dentro del metodo (version corregida):\n")
u2 <- diagnosticar_muestras(mat_rm, louvain_sin_seed, "RM / Louvain (seed fuera)")
u3 <- diagnosticar_muestras(mat_rm, leiden_sin_seed, "RM / Leiden  (seed fuera)")

cat("\n  LECTURA: si la primera linea muestra muy pocas muestras distintas y las\n")
cat("  siguientes muestran ~50 de 50, la hipotesis queda confirmada.\n")

# =============================================================================
# 3. BOOTSTRAP CORREGIDO
# =============================================================================
# CORRECCION 1: la semilla se fija UNA sola vez, al inicio del bootstrap. Las
#   funciones de deteccion no la tocan. La reproducibilidad de todo el
#   bootstrap queda garantizada por esa unica semilla.
# CORRECCION 2: las ocupaciones muestreadas mas de una vez se mantienen como
#   FILAS SEPARADAS (via make.unique), no se suman con rowsum(). Sumarlas
#   colapsa la matriz y altera el calculo del RCA; el bootstrap estandar
#   trata cada extraccion como una unidad independiente.

cat("\n=== 3. BOOTSTRAP CORREGIDO ===\n")

bootstrap_corregido <- function(mat, funcion_metodo, membership_referencia,
                                 n_replicas = N_BOOTSTRAP, semilla = SEMILLA) {
  set.seed(semilla)   # UNICA vez, fuera del loop
  filas <- rownames(mat); n <- length(filas)

  ari_vec <- rep(NA_real_, n_replicas)
  n_com_vec <- rep(NA_integer_, n_replicas)
  degenerados <- 0

  for (i in seq_len(n_replicas)) {
    muestra <- sample(filas, size = n, replace = TRUE)
    submat <- mat[muestra, , drop = FALSE]
    rownames(submat) <- make.unique(muestra)   # filas repetidas se conservan

    subgrafo <- construir_red_phi(submat)
    if (is.null(subgrafo)) { degenerados <- degenerados + 1; next }

    particion <- tryCatch(funcion_metodo(subgrafo), error = function(e) NULL)
    if (is.null(particion) || length(unique(particion)) <= 1) {
      degenerados <- degenerados + 1; next
    }

    comunes <- intersect(names(membership_referencia), names(particion))
    if (length(comunes) < 2) { degenerados <- degenerados + 1; next }

    ari_vec[i] <- igraph::compare(membership_referencia[comunes], particion[comunes],
                                   method = "adjusted.rand")
    n_com_vec[i] <- length(unique(particion))
  }

  validos <- ari_vec[!is.na(ari_vec)]
  tibble(
    ari_mediano = round(median(validos), 4),
    ari_ic95_inf = round(unname(quantile(validos, 0.025)), 4),
    ari_ic95_sup = round(unname(quantile(validos, 0.975)), 4),
    ari_valores_distintos = length(unique(round(validos, 6))),
    n_comunidades_moda = as.integer(names(sort(table(n_com_vec), decreasing = TRUE))[1]),
    prop_recupera_n_original = round(mean(n_com_vec == length(unique(membership_referencia)),
                                           na.rm = TRUE), 4),
    n_replicas_degeneradas = degenerados
  )
}

# Particiones de referencia (aqui SI se fija semilla, para reproducibilidad
# del resultado puntual reportado)
referencia <- function(mat, metodo) {
  g <- construir_red_phi(mat)
  set.seed(SEMILLA)
  if (metodo == "louvain") louvain_sin_seed(g) else leiden_sin_seed(g)
}

ref_rm_lou  <- referencia(mat_rm, "louvain")
ref_rm_lei  <- referencia(mat_rm, "leiden")
ref_gl_lou  <- referencia(mat_global, "louvain")
ref_gl_lei  <- referencia(mat_global, "leiden")

tabla_corregida <- bind_rows(
  bind_cols(tibble(universo = "RM", metodo = "Louvain"),
            bootstrap_corregido(mat_rm, louvain_sin_seed, ref_rm_lou)),
  bind_cols(tibble(universo = "RM", metodo = "Leiden"),
            bootstrap_corregido(mat_rm, leiden_sin_seed, ref_rm_lei)),
  bind_cols(tibble(universo = "GLOBAL", metodo = "Louvain"),
            bootstrap_corregido(mat_global, louvain_sin_seed, ref_gl_lou)),
  bind_cols(tibble(universo = "GLOBAL", metodo = "Leiden"),
            bootstrap_corregido(mat_global, leiden_sin_seed, ref_gl_lei))
)

cat("\n=== Bootstrap corregido ===\n")
print(as.data.frame(tabla_corregida), row.names = FALSE)

cat("\n  VERIFICACION: la columna ari_valores_distintos debe ser alta (cientos).\n")
cat("  Si alguna fila muestra 1, el problema persiste y hay que revisar de nuevo.\n")

write_csv(tabla_corregida, po("robustez_louvain_leiden_CORREGIDO.csv"))
cat("\nGuardado en:", po("robustez_louvain_leiden_CORREGIDO.csv"), "\n")

# =============================================================================
# DECISIONES METODOLOGICAS
# =============================================================================
# D1. La semilla se fija UNA sola vez al inicio de cada bootstrap, nunca
#     dentro del loop. Fijarla dentro de la funcion de deteccion (como hacia
#     la version anterior) devuelve el generador aleatorio al mismo estado en
#     cada iteracion y hace que el remuestreo colapse: las replicas dejan de
#     ser independientes. Esto NO contradice la correccion del 2026-08-03 de
#     los scripts 08/09: ahi la semilla se fija antes de cluster_louvain()
#     porque se corre UNA sola deteccion; en un bootstrap la logica se
#     invierte.
# D2. Las ocupaciones muestreadas mas de una vez se conservan como filas
#     separadas via make.unique(), no se agregan con rowsum(). Sumar los
#     conteos de una misma ocupacion repetida altera los totales marginales
#     y por tanto el calculo del RCA, que es sensible a la estructura de
#     filas y columnas. El bootstrap estandar trata cada extraccion como una
#     unidad independiente.
# D3. Se agrega la columna ari_valores_distintos como control permanente: si
#     el bootstrap vuelve a colapsar, esa columna lo hace visible de
#     inmediato en vez de quedar oculto tras un intervalo degenerado.
# D4. Para las particiones de referencia (resultado puntual reportado) SI se
#     fija la semilla antes de la deteccion, porque ahi se ejecuta una sola
#     vez y la reproducibilidad exacta del resultado publicado es deseable.
# =============================================================================
