# =============================================================================
# 11_comparacion_leiden_louvain.R
#
# Comparacion de deteccion de comunidades: Louvain (vigente, igual que
# 08_deteccion_comunidades_L2.R) vs. Leiden (funcion objetivo modularity y
# funcion objetivo CPM), con robustez bootstrap.
#
# Script AUTOCONTENIDO: reconstruye la matriz ISCO x L2, el RCA y la red phi
# desde los CSV crudos de ESCO, exactamente con la misma logica de
# 08_deteccion_comunidades_L2.R (Pasos 1 a 4 replicados aqui como funciones),
# para que Louvain, Leiden-modularity y Leiden-CPM se comparen sobre la
# MISMA red, calculada de la MISMA forma. No depende de ningun .rds: ese
# script tampoco guarda ninguno, reconstruye todo en cada corrida.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(igraph)
})

# --- 0. Configuracion --------------------------------------------------------
# Mismas rutas y mismos parametros que 08_deteccion_comunidades_L2.R, para
# que la red de base sea identica.

DATA_DIR      <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data/esco"
OUT_DIR       <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output"
p  <- function(f) file.path(DATA_DIR, f)
po <- function(f) file.path(OUT_DIR, "comparacion_leiden", f)
dir.create(po(""), showWarnings = FALSE, recursive = TRUE)

RCA_THRESHOLD    <- 1
MIN_COVERAGE_OCC <- 0
PHI_MIN_EDGE     <- 0

N_BOOTSTRAP <- 500
SEMILLA <- 2025

# Categorias ancla del nucleo agro-bio-legal documentado (identificacion por
# contenido, nunca por ID de comunidad)
CATEGORIAS_ANCLA_AGRO <- c("042", "051", "052", "078", "081", "082", "083", "084", "S6.4", "S6.9")

stopifnot(
  "igraph debe tener cluster_leiden disponible (paquete igraph >= 1.2.7)" =
    "cluster_leiden" %in% getNamespaceExports("igraph")
)

# =============================================================================
# PASO 1 — MATRIZ ISCO-08 x CATEGORIA DE HABILIDAD (NIVEL 2 ESCO)
# Replica exacta del Paso 1 de 08_deteccion_comunidades_L2.R
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

br <- read_csv(p("broaderRelationsSkillPillar_es.csv"),
               col_types = cols(.default = "c"), show_col_types = FALSE)

skill_to_group <- br |>
  filter(conceptType == "KnowledgeSkillCompetence", broaderType == "SkillGroup") |>
  select(skill_uri = conceptUri, group_uri = broaderUri) |>
  distinct()

group_to_L2 <- bind_rows(
  skill_hier |> filter(!is.na(`Level 3 URI`), !is.na(`Level 2 code`)) |>
    select(group_uri = `Level 3 URI`, L2_code = `Level 2 code`,
           L2_label = `Level 2 preferred term`),
  skill_hier |> filter(!is.na(`Level 2 URI`), !is.na(`Level 2 code`)) |>
    select(group_uri = `Level 2 URI`, L2_code = `Level 2 code`,
           L2_label = `Level 2 preferred term`)
) |> distinct(group_uri, .keep_all = TRUE)

skill_L2_map <- skill_to_group |>
  left_join(group_to_L2, by = "group_uri") |>
  filter(!is.na(L2_code)) |>
  distinct(skill_uri, L2_code, .keep_all = TRUE)

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

# =============================================================================
# FUNCION: matriz de conteos -> red phi (replica exacta de los Pasos 3-4)
# =============================================================================
# Recibe la matriz de conteos ISCO x L2 (misma estructura que mat_ca) y
# devuelve el objeto igraph de complementariedad, exactamente con la misma
# logica de RCA + phi que 08_deteccion_comunidades_L2.R (Pasos 3 y 4). Se usa
# tanto sobre la muestra completa como sobre cada remuestra del bootstrap.

construir_red_phi <- function(matriz_conteos,
                               rca_threshold = RCA_THRESHOLD,
                               min_coverage = MIN_COVERAGE_OCC,
                               phi_min_edge = PHI_MIN_EDGE) {

  row_totals  <- rowSums(matriz_conteos)
  col_totals  <- colSums(matriz_conteos)
  grand_total <- sum(matriz_conteos)

  # columnas con suma cero rompen la division; se excluyen antes del RCA
  cols_validas <- col_totals > 0
  matriz_conteos <- matriz_conteos[, cols_validas, drop = FALSE]
  col_totals <- col_totals[cols_validas]

  expected <- outer(row_totals, col_totals) / grand_total
  rca_mat  <- matriz_conteos / expected

  B <- (rca_mat > rca_threshold) * 1L

  cobertura_categoria <- colSums(B)
  categorias_validas <- names(cobertura_categoria)[cobertura_categoria >= min_coverage]
  B <- B[, categorias_validas, drop = FALSE]

  # columnas constantes (todo 0 o todo 1) producen NA en cor(); se excluyen
  varianza_col <- apply(B, 2, var)
  B <- B[, varianza_col > 0, drop = FALSE]

  if (ncol(B) < 3) return(NULL)  # red degenerada, insuficiente para comunidades

  phi_mat <- cor(B, method = "pearson")
  diag(phi_mat) <- 0

  edges_df <- as.data.frame(as.table(phi_mat)) |>
    rename(from = Var1, to = Var2, phi = Freq) |>
    filter(as.character(from) < as.character(to), phi > phi_min_edge)

  if (nrow(edges_df) == 0) return(NULL)

  g <- graph_from_data_frame(edges_df, directed = FALSE,
                              vertices = data.frame(name = colnames(B)))
  E(g)$weight <- edges_df$phi
  g
}

red_completa <- construir_red_phi(mat_ca)
stopifnot("La red construida sobre la muestra completa no debe ser NULL" = !is.null(red_completa))
cat("Red phi construida: ", vcount(red_completa), " nodos, ", ecount(red_completa), " aristas\n", sep = "")

# =============================================================================
# PASO 2 — FUNCIONES DE DETECCION DE COMUNIDADES
# =============================================================================

correr_louvain <- function(grafo, semilla = SEMILLA) {
  set.seed(semilla)
  particion <- cluster_louvain(grafo, weights = E(grafo)$weight)
  mem <- membership(particion)
  list(membership = mem, Q = modularity(particion), n_comunidades = length(unique(mem)))
}

correr_leiden_mod <- function(grafo, semilla = SEMILLA) {
  set.seed(semilla)
  particion <- cluster_leiden(grafo, objective_function = "modularity",
                               weights = E(grafo)$weight, n_iterations = 10)
  mem <- membership(particion)
  list(membership = mem, Q = modularity(grafo, mem, weights = E(grafo)$weight),
       n_comunidades = length(unique(mem)))
}

correr_leiden_cpm <- function(grafo, resolucion, semilla = SEMILLA) {
  set.seed(semilla)
  particion <- cluster_leiden(grafo, objective_function = "CPM",
                               weights = E(grafo)$weight,
                               resolution_parameter = resolucion, n_iterations = 10)
  mem <- membership(particion)
  list(membership = mem, Q = modularity(grafo, mem, weights = E(grafo)$weight),
       n_comunidades = length(unique(mem)))
}

# --- Seleccion de resolucion CPM por sensibilidad ----------------------------
# CPM no tiene una resolucion "natural" como modularity. Se escanea una
# grilla y se elige la resolucion cuyo numero de comunidades es mas cercano
# al de Louvain (misma logica que la grilla PHI_MIN_EDGE de la robustez ya
# hecha para Louvain). Se reporta como decision metodologica, no como
# optimo encontrado por el algoritmo.

escanear_resolucion_cpm <- function(grafo, objetivo_n_comunidades,
                                     grilla = seq(0.01, 0.50, by = 0.01)) {
  filas <- lapply(grilla, function(r) {
    res <- tryCatch(correr_leiden_cpm(grafo, r), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    data.frame(resolucion = r, n_comunidades = res$n_comunidades, Q = res$Q)
  })
  tabla <- do.call(rbind, filas)
  tabla$dist_objetivo <- abs(tabla$n_comunidades - objetivo_n_comunidades)
  tabla[order(tabla$dist_objetivo, tabla$resolucion), ]
}

identificar_comunidad_ancla <- function(membership, categorias_ancla = CATEGORIAS_ANCLA_AGRO) {
  presentes <- intersect(categorias_ancla, names(membership))
  if (length(presentes) == 0) {
    return(data.frame(comunidad = NA, cobertura = NA, n_ancla_presentes = 0))
  }
  tabla <- table(membership[presentes])
  comunidad_mayoritaria <- names(tabla)[which.max(tabla)]
  data.frame(
    comunidad = comunidad_mayoritaria,
    cobertura = max(tabla) / length(presentes),
    n_ancla_presentes = length(presentes)
  )
}

# =============================================================================
# PASO 3 — COMPARACION PUNTUAL SOBRE LA MUESTRA COMPLETA
# =============================================================================

cat("\n=== PASO 3: Comparacion puntual Louvain vs. Leiden ===\n")

louvain    <- correr_louvain(red_completa)
leiden_mod <- correr_leiden_mod(red_completa)

grilla_cpm <- escanear_resolucion_cpm(red_completa, objetivo_n_comunidades = louvain$n_comunidades)
resolucion_elegida <- grilla_cpm$resolucion[1]
leiden_cpm <- correr_leiden_cpm(red_completa, resolucion_elegida)

ari_mod_vs_louvain <- igraph::compare(louvain$membership, leiden_mod$membership, method = "adjusted.rand")
ari_cpm_vs_louvain <- igraph::compare(louvain$membership, leiden_cpm$membership, method = "adjusted.rand")
ari_mod_vs_cpm     <- igraph::compare(leiden_mod$membership, leiden_cpm$membership, method = "adjusted.rand")

resumen <- data.frame(
  metodo = c("Louvain", "Leiden (modularity)", sprintf("Leiden (CPM, res=%.2f)", resolucion_elegida)),
  Q = c(louvain$Q, leiden_mod$Q, leiden_cpm$Q),
  n_comunidades = c(louvain$n_comunidades, leiden_mod$n_comunidades, leiden_cpm$n_comunidades),
  ari_vs_louvain = c(1, ari_mod_vs_louvain, ari_cpm_vs_louvain)
)

identificacion <- rbind(
  cbind(metodo = "Louvain", identificar_comunidad_ancla(louvain$membership)),
  cbind(metodo = "Leiden (modularity)", identificar_comunidad_ancla(leiden_mod$membership)),
  cbind(metodo = sprintf("Leiden (CPM, res=%.2f)", resolucion_elegida), identificar_comunidad_ancla(leiden_cpm$membership))
)

cat("\n=== Resumen de metodos ===\n"); print(resumen)
cat("\n=== Identificacion del nucleo agro-bio-legal por metodo ===\n"); print(identificacion)
cat("\nARI Leiden-modularity vs. Leiden-CPM (entre si, no contra Louvain):", round(ari_mod_vs_cpm, 3), "\n")

write_csv(resumen, po("tabla_comparacion_metodos.csv"))
write_csv(identificacion, po("tabla_identificacion_agro_bio_legal.csv"))
write_csv(grilla_cpm, po("grilla_resolucion_cpm.csv"))

# =============================================================================
# PASO 4 — ROBUSTEZ BOOTSTRAP (500 replicas x 3 metodos)
# =============================================================================
# Remuestrea con reemplazo las filas ISCO-4 de la matriz de conteos (no los
# nodos de la red ya construida), reconstruye la red phi en cada replica vía
# construir_red_phi(), y calcula ARI contra la particion de la muestra
# completa para cada metodo. Mismo diseno que
# 05_bootstrap_estabilidad_comunidades.R, extendido a los tres metodos.

cat("\n=== PASO 4: Robustez bootstrap (", N_BOOTSTRAP, " replicas x 3 metodos) ===\n", sep = "")

bootstrap_metodo <- function(matriz_conteos, funcion_metodo, membership_referencia,
                              n_replicas = N_BOOTSTRAP, semilla = SEMILLA) {
  set.seed(semilla)
  filas <- rownames(matriz_conteos)
  n <- length(filas)

  ari_vec <- rep(NA_real_, n_replicas)
  n_comunidades_vec <- rep(NA_integer_, n_replicas)
  degenerados <- 0

  for (i in seq_len(n_replicas)) {
    muestra <- sample(filas, size = n, replace = TRUE)
    submatriz <- matriz_conteos[muestra, , drop = FALSE]
    # sumar conteos de filas repetidas (mismo ISCO muestreado mas de una vez)
    submatriz <- rowsum(submatriz, group = rownames(submatriz))

    subgrafo <- construir_red_phi(submatriz)
    if (is.null(subgrafo)) { degenerados <- degenerados + 1; next }

    particion <- tryCatch(funcion_metodo(subgrafo), error = function(e) NULL)
    if (is.null(particion) || particion$n_comunidades <= 1) {
      degenerados <- degenerados + 1
      next
    }

    nodos_comunes <- intersect(names(membership_referencia), names(particion$membership))
    if (length(nodos_comunes) < 2) { degenerados <- degenerados + 1; next }

    ari_vec[i] <- igraph::compare(
      membership_referencia[nodos_comunes], particion$membership[nodos_comunes],
      method = "adjusted.rand"
    )
    n_comunidades_vec[i] <- particion$n_comunidades
  }

  ari_validos <- ari_vec[!is.na(ari_vec)]

  list(
    ari_mediano = median(ari_validos),
    ari_ic95 = quantile(ari_validos, c(0.025, 0.975)),
    n_comunidades_moda = as.integer(names(sort(table(n_comunidades_vec), decreasing = TRUE))[1]),
    prop_recupera_n_original = mean(n_comunidades_vec == length(unique(membership_referencia)), na.rm = TRUE),
    n_replicas_degeneradas = degenerados,
    n_replicas_validas = length(ari_validos)
  )
}

robustez <- list(
  louvain    = bootstrap_metodo(mat_ca, correr_louvain, louvain$membership),
  leiden_mod = bootstrap_metodo(mat_ca, correr_leiden_mod, leiden_mod$membership),
  leiden_cpm = bootstrap_metodo(mat_ca, function(g) correr_leiden_cpm(g, resolucion_elegida), leiden_cpm$membership)
)

tabla_robustez <- do.call(rbind, lapply(names(robustez), function(m) {
  data.frame(
    metodo = m,
    ari_mediano = robustez[[m]]$ari_mediano,
    ari_ic95_inf = robustez[[m]]$ari_ic95[1],
    ari_ic95_sup = robustez[[m]]$ari_ic95[2],
    n_comunidades_moda = robustez[[m]]$n_comunidades_moda,
    prop_recupera_n_original = robustez[[m]]$prop_recupera_n_original,
    n_replicas_degeneradas = robustez[[m]]$n_replicas_degeneradas,
    n_replicas_validas = robustez[[m]]$n_replicas_validas
  )
}))

cat("\n=== Robustez bootstrap por metodo ===\n"); print(tabla_robustez)

write_csv(tabla_robustez, po("tabla_robustez_bootstrap.csv"))

saveRDS(
  list(resumen = resumen, identificacion = identificacion, grilla_cpm = grilla_cpm,
       tabla_robustez = tabla_robustez,
       membership = list(louvain = louvain$membership, leiden_mod = leiden_mod$membership,
                          leiden_cpm = leiden_cpm$membership)),
  po("comparacion_leiden_louvain.rds")
)

cat("\nArchivos generados en", po(""), ":\n")
cat("  - tabla_comparacion_metodos.csv\n")
cat("  - tabla_identificacion_agro_bio_legal.csv\n")
cat("  - grilla_resolucion_cpm.csv\n")
cat("  - tabla_robustez_bootstrap.csv\n")
cat("  - comparacion_leiden_louvain.rds (todo lo anterior, para lectura posterior)\n")
cat("\n=== FIN: COMPARACION LOUVAIN VS. LEIDEN ===\n")

# =============================================================================
# DECISIONES METODOLOGICAS
# =============================================================================
# 1. La red de base se reconstruye con la MISMA logica de
#    08_deteccion_comunidades_L2.R (Pasos 1-4 replicados aqui verbatim,
#    factorizados en construir_red_phi()), para que Louvain, Leiden-modularity
#    y Leiden-CPM se comparen sobre exactamente la misma red y no exista
#    riesgo de discrepancia entre implementaciones.
# 2. Se prueban las dos funciones objetivo de Leiden disponibles en igraph:
#    modularity (comparable de forma directa con Louvain) y CPM, que no
#    sufre el limite de resolucion de modularity y es preferible en redes
#    densas como esta (revisar densidad de aristas resultante en consola;
#    ya senalada como debilidad del pipeline vigente).
# 3. CPM no tiene una resolucion "natural". Se escanea una grilla de 0.01 a
#    0.50 y se elige la resolucion cuyo numero de comunidades es mas cercano
#    al de Louvain, para que Q y ARI sean interpretables entre metodos. Esta
#    eleccion debe reportarse como decision metodologica explicita, no como
#    optimo hallado por el algoritmo.
# 4. El bootstrap remuestrea las filas ISCO-4 de la matriz de conteos (no los
#    nodos de la red ya agregada) y reconstruye la red completa en cada
#    replica, replicando el diseno de
#    05_bootstrap_estabilidad_comunidades.R, extendido a los tres metodos
#    para que la comparacion de robustez sea simetrica.
# 5. La identificacion del nucleo agro-bio-legal usa las categorias ancla ya
#    documentadas (042, 051, 052, 078, 081, 082, 083, 084, S6.4, S6.9), nunca
#    el ID numerico de comunidad, siguiendo la convencion ya establecida
#    (los IDs no son estables entre corridas ni entre algoritmos).
# 6. Este script es SOLO el universo global (identico al universo de
#    08_deteccion_comunidades_L2.R). Para extender la comparacion al universo
#    RM filtrado, se necesita el equivalente de 06_robustez_comunidades.R con
#    el mismo nivel de detalle de codigo que se tuvo aqui para 08 antes de
#    poder garantizar que no hay discrepancia de implementacion.
# =============================================================================
