# =============================================================================
# 13_imputar_comunidades_ocupacion.R
#
# Imputa, para cada ocupacion (generador de posiciones, universo CASEN-RM, y
# ocupacion de ego una vez integrada), la composicion de sus habilidades
# efectivas (RCA>1) en terminos de las 4 comunidades de habilidades ya
# detectadas. Responde la recomendacion de Gabriel Otero (tutoria de hace
# dos semanas): las comunidades deben imputarse a nivel de ocupacion, no
# quedar solo a nivel de categoria de habilidad.
#
# Ejemplo de lo que produce: para el ISCO 2211 (medico/a generalista), un
# vector como (share_c1=0.55, share_c2=0.30, share_c3=0.10, share_c4=0.05)
# que suma 1.
#
# Script AUTOCONTENIDO. La construccion de mat_rm es VERBATIM la de
# 06_robustez_comunidades.R (misma fuente ya usada en 11 y 12).
# SALIDA: consola + CSV.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(igraph)
  library(ggplot2); library(ggraph); library(tidygraph); library(FactoMineR)
  library(ggrepel)      # repel de etiquetas de texto en el mapa de red (Seccion 6)
  library(graphlayouts) # layout "stress", mas legible que "fr" para redes densas
})
options(dplyr.summarise.inform = FALSE)

# =============================================================================
# 0. RUTAS Y PARAMETROS
# =============================================================================

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

# DECISION: algoritmo base para la particion que se imputa a las ocupaciones.
# Leiden (modularity), sobre el universo RM. Ver Decision D1 al final. Para
# volver a Louvain como base, cambiar ALGORITMO_BASE a "louvain".
ALGORITMO_BASE <- "leiden"   # "leiden" o "louvain"

# ANCLAJE POR CONTENIDO (fix C2, 2026-08-18): igraph asigna IDs de comunidad
# arbitrarios (1, 2, 3, 4) que pueden cambiar de corrida a corrida sin que el
# CONTENIDO sustantivo cambie. Antes, share_com1..4 heredaba ese orden
# arbitrario, y 04_indicadores_red.R y 18_modelos_habilidades_origen.R
# asumian en silencio que share_com1 siempre era "Direccion-servicio", etc.
# Esta es la misma logica de anclaje ya validada y en uso en
# 14_tabla_grados_completa.R (ver ese script para el detalle de como se
# identificaron las anclas). Aqui se aplica ANTES de calcular shares_rm y
# gp_shares, remapeando los IDs de membership_L2 para que la comunidad 1
# quede SIEMPRE anclada a Direccion-servicio, la 2 a Tecnico-manual, etc.,
# sin importar el orden que haya devuelto igraph en esta corrida particular.
# Con este remapeo, 04 y 18 NO necesitan cambios: siguen leyendo
# share_com1..4 igual que antes, pero ahora esa numeracion es estable.
ETIQUETAS_COMUNIDAD <- c(
  "1" = "Direccion_servicio",
  "2" = "Tecnico_manual",
  "3" = "Analitico_digital_simbolico",
  "4" = "Bio_ambiental_legal"
)
ANCLA_1_DIRECCION <- "S4.9"   # tomar decisiones
ANCLA_2_TECNICO   <- "053"    # ciencias fisicas
ANCLA_3_ANALITICO <- "S2.3"   # gestionar informacion
ANCLA_4_AGRO      <- "081"    # agricultura

leer <- function(ruta) {
  if (!file.exists(ruta)) stop(sprintf("No existe el archivo:\n  %s", ruta), call. = FALSE)
  suppressWarnings(read_csv(ruta, col_types = cols(.default = "c"),
                            show_col_types = FALSE, progress = FALSE))
}

# =============================================================================
# 1. RECONSTRUCCION DE LA MATRIZ RM (verbatim de 06)
# =============================================================================

cat("\n=== 1. RECONSTRUCCION DE LA MATRIZ RM ===\n")

osr        <- leer(p("occupationSkillRelations_es.csv"))
occ_raw    <- leer(p("occupations_es.csv"))
skill_hier <- leer(p("skillsHierarchy_es.csv"))
br         <- leer(p("broaderRelationsSkillPillar_es.csv"))
rm_casen   <- leer(pc("ocupaciones_rm_casen2024.csv"))
corr       <- leer(pc("correcciones_isco_casen.csv"))
gp_crosswalk <- leer(pc("gp_crosswalk.csv"))

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

cat(sprintf("  Matriz RM: %d ocupaciones x %d categorias L2\n", nrow(mat_rm), ncol(mat_rm)))

# =============================================================================
# 2. RED PHI Y MATRIZ BINARIA B (misma logica de detectar() en 06)
# =============================================================================
# A diferencia de 11/12, aqui se necesita conservar B (no solo el grafo),
# porque B es la matriz de habilidades efectivas que se va a agregar por
# comunidad para cada ocupacion.

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
cat(sprintf("  Red RM: %d nodos, %d aristas | B: %d ocupaciones x %d categorias efectivas (alguna vez)\n",
            vcount(red_rm$grafo), ecount(red_rm$grafo), nrow(red_rm$B), ncol(red_rm$B)))

# =============================================================================
# 3. PARTICION BASE (Leiden o Louvain, segun ALGORITMO_BASE)
# =============================================================================

cat(sprintf("\n=== 2. PARTICION BASE: %s ===\n", toupper(ALGORITMO_BASE)))

set.seed(SEMILLA)
if (ALGORITMO_BASE == "leiden") {
  particion <- cluster_leiden(red_rm$grafo, objective_function = "modularity",
                               weights = E(red_rm$grafo)$weight, n_iterations = 10)
} else {
  particion <- cluster_louvain(red_rm$grafo, weights = E(red_rm$grafo)$weight)
}

membership_L2 <- setNames(as.integer(membership(particion)), names(membership(particion)))
n_comunidades <- length(unique(membership_L2))

# FIX C2: remapear los IDs arbitrarios de igraph a un orden fijo, anclado
# por contenido, ANTES de cualquier calculo posterior. Verifica primero que
# las 4 categorias ancla existan en esta corrida y caigan en 4 comunidades
# distintas (si Leiden encontrara un numero de comunidades distinto de 4,
# o si dos anclas cayeran en la misma comunidad, esto detiene la ejecucion
# en vez de asignar etiquetas incorrectas en silencio).
anclas <- c(ANCLA_1_DIRECCION, ANCLA_2_TECNICO, ANCLA_3_ANALITICO, ANCLA_4_AGRO)
anclas_faltantes <- setdiff(anclas, names(membership_L2))
if (length(anclas_faltantes) > 0) {
  stop(sprintf(
    "No se puede anclar por contenido: las categorias ancla %s no aparecen en membership_L2 de esta corrida (revisar si mat_rm perdio categorias por cobertura o RCA).",
    paste(anclas_faltantes, collapse = ", ")
  ))
}

id_original_de_ancla <- c(
  membership_L2[[ANCLA_1_DIRECCION]],
  membership_L2[[ANCLA_2_TECNICO]],
  membership_L2[[ANCLA_3_ANALITICO]],
  membership_L2[[ANCLA_4_AGRO]]
)
stopifnot(
  "Las 4 anclas deben caer en 4 comunidades distintas (revisar particion antes de continuar)" =
    length(unique(id_original_de_ancla)) == 4,
  "cluster_leiden/louvain debe devolver exactamente 4 comunidades para que el anclaje aplique" =
    n_comunidades == 4
)

mapa_reordenamiento <- setNames(1:4, as.character(id_original_de_ancla))
membership_L2 <- setNames(mapa_reordenamiento[as.character(membership_L2)], names(membership_L2))

cat("\n  Anclaje por contenido aplicado (fix C2):\n")
for (i in 1:4) {
  cat(sprintf("    share_com%d = %s (ancla: %s)\n",
              i, ETIQUETAS_COMUNIDAD[[as.character(i)]], anclas[i]))
}

cat(sprintf("  N. de comunidades: %d | Modularidad: %.4f\n",
            n_comunidades, modularity(red_rm$grafo, membership_L2, weights = E(red_rm$grafo)$weight)))
cat("  Tamano de cada comunidad:\n")
print(table(membership_L2))

# Categorias con mayor grado por comunidad (hub), como insumo para el
# bautizo sustantivo de las comunidades (tarea pendiente de Gabriel, no se
# resuelve en este script, pero esto ahorra tener que recalcularlo despues)
grados <- degree(red_rm$grafo)
tabla_hubs <- tibble(L2_code = names(grados), comunidad = membership_L2[names(grados)],
                     grado = grados) %>%
  arrange(comunidad, desc(grado)) %>%
  group_by(comunidad) %>% slice_head(n = 5) %>% ungroup()

cat("\n  5 categorias mas conectadas por comunidad (insumo para el bautizo sustantivo):\n")
print(as.data.frame(tabla_hubs), row.names = FALSE)
write_csv(tabla_hubs, po("hubs_por_comunidad.csv"))

# =============================================================================
# 4. FUNCION DE IMPUTACION: matriz B + particion -> shares por ocupacion
# =============================================================================

calcular_shares_comunidad <- function(B, membership_L2, n_comunidades) {
  categorias_con_comunidad <- intersect(colnames(B), names(membership_L2))
  categorias_sin_comunidad <- setdiff(colnames(B), names(membership_L2))
  if (length(categorias_sin_comunidad) > 0) {
    cat(sprintf("  Advertencia: %d categorias en B sin comunidad asignada (excluidas del calculo de shares): %s\n",
                length(categorias_sin_comunidad), paste(categorias_sin_comunidad, collapse = ", ")))
  }

  Bc <- B[, categorias_con_comunidad, drop = FALSE]
  com_de_cada_col <- membership_L2[categorias_con_comunidad]

  conteo_por_comunidad <- sapply(seq_len(n_comunidades), function(c) {
    cols_c <- categorias_con_comunidad[com_de_cada_col == c]
    if (length(cols_c) == 0) return(rep(0L, nrow(Bc)))
    if (length(cols_c) == 1) return(as.integer(Bc[, cols_c]))
    rowSums(Bc[, cols_c, drop = FALSE])
  })
  colnames(conteo_por_comunidad) <- paste0("n_categorias_com", seq_len(n_comunidades))
  rownames(conteo_por_comunidad) <- rownames(Bc)

  total <- rowSums(conteo_por_comunidad)
  shares <- conteo_por_comunidad / pmax(total, 1)  # evita division por 0
  colnames(shares) <- paste0("share_com", seq_len(n_comunidades))

  tibble(isco4 = rownames(Bc), n_categorias_efectivas_total = total) %>%
    bind_cols(as_tibble(shares)) %>%
    bind_cols(as_tibble(conteo_por_comunidad))
}

shares_rm <- calcular_shares_comunidad(red_rm$B, membership_L2, n_comunidades)

cat(sprintf("\n=== 3. SHARES CALCULADOS: %d ocupaciones RM ===\n", nrow(shares_rm)))
cat("  Primeras filas:\n")
print(head(as.data.frame(shares_rm)))

# =============================================================================
# 5. APLICACION A LAS 27 POSICIONES DEL GENERADOR (GP)
# =============================================================================
# gp_crosswalk.csv ya trae los codigos ISCO corregidos (fuente unica
# centralizada, ver notas del pipeline principal), asi que no hace falta
# aplicar correcciones de nuevo aqui.

cat("\n=== 4. IMPUTACION A LAS 27 POSICIONES DEL GENERADOR (GP) ===\n")

stopifnot(
  "gp_crosswalk.csv debe tener columnas var e isco4" =
    all(c("var", "isco4") %in% names(gp_crosswalk))
)

gp_shares <- gp_crosswalk %>%
  mutate(isco4 = as.character(as.integer(isco4))) %>%
  left_join(shares_rm, by = "isco4")

n_gp_sin_match <- sum(is.na(gp_shares$n_categorias_efectivas_total))
if (n_gp_sin_match > 0) {
  cat(sprintf("  Advertencia: %d de %d posiciones GP sin match en la matriz RM (revisar codigo ISCO):\n",
              n_gp_sin_match, nrow(gp_shares)))
  print(gp_shares %>% filter(is.na(n_categorias_efectivas_total)) %>% select(var, isco4))
} else {
  cat(sprintf("  Las %d posiciones GP matchearon correctamente contra la matriz RM.\n", nrow(gp_shares)))
}

write_csv(gp_shares, po("shares_comunidad_generador_posiciones.csv"))

# =============================================================================
# 6. TABLA COMPLETA PARA EL UNIVERSO CASEN-RM (391 ocupaciones)
# =============================================================================
# Para el universo RM, shares_rm YA ES la tabla completa (se calculo
# directamente sobre las 391 ocupaciones de mat_rm). Se agrega el label
# de ocupacion ESCO para que sea legible sin tener que cruzar por separado.

label_isco <- occ_raw %>%
  transmute(isco4 = suppressWarnings(as.character(as.integer(iscoGroup))),
            occupationLabel = preferredLabel) %>%
  filter(!is.na(isco4)) %>% distinct(isco4, .keep_all = TRUE)

shares_rm_legible <- shares_rm %>% left_join(label_isco, by = "isco4") %>%
  relocate(occupationLabel, .after = isco4)

write_csv(shares_rm_legible, po("shares_comunidad_casen_rm.csv"))

# =============================================================================
# 7. PUNTO DE INTEGRACION PARA LA OCUPACION DE EGO (Fondecyt)
# =============================================================================
# Este script NO tiene acceso a 01_preprocesar_encuesta.R, por lo que no
# puede leer directamente Q4501/Q4402 y armar el isco4 de ego. Lo que se dej
# a aqui es la funcion de imputacion ya lista para recibir ese vector una vez
# que exista en el pipeline principal.
#
# USO ESPERADO (a integrar en el script de indicadores, ej. 04_indicadores_red.R):
#
#   ego_isco4 <- <vector de isco4 de ego, ya armonizado y con las mismas
#                 correcciones de correcciones_isco_casen.csv aplicadas>
#   ego_shares <- tibble(isco4 = as.character(ego_isco4)) |>
#     left_join(shares_rm, by = "isco4")
#
# Si el isco4 de una ocupacion de ego no aparece en shares_rm (porque no
# esta en el universo CASEN-RM, o porque quedo fuera de mat_rm por baja
# cobertura), el left_join deja NA y hay que decidir como tratarlo: excluir
# el caso, o usar mat_global/shares del universo global como respaldo.

cat("\n=== 5. INTEGRACION PARA EGO: pendiente, ver comentario en el codigo (Paso 7) ===\n")
cat("  Este script no tiene el isco4 de ego armonizado. shares_rm queda\n")
cat("  disponible para un left_join directo una vez que ese vector exista.\n")

# =============================================================================

# =============================================================================
# 5bis. CARACTERIZACION POR COMUNIDAD: n, ISEI implicito y phi intra
# =============================================================================
# NUEVO (18-ago-2026). Produce la tabla de caracterizacion de las 4 comunidades
# que sustenta el hallazgo estructural: la comunidad Bio-ambiental-legal es
# simultaneamente la mas cohesionada en habilidades (mayor phi intra) y la mas
# dispersa en estatus (mayor DE de ISEI).
#
# IMPORTANTE -- por que el CA se re-estima aqui en vez de leer ca_coords.rds:
# 03_ca_habilidades_isei.R guarda las coordenadas de las OCUPACIONES (filas),
# no de las CATEGORIAS DE HABILIDAD (columnas), que es lo que hace falta para
# el ISEI implicito por comunidad. Ademas, el signo de los ejes de un CA es
# arbitrario entre corridas: si se entrenara la regresion isei~Dim1 con las
# coordenadas de 03 y se aplicara a coordenadas de columna de otra corrida,
# el ISEI implicito podria salir invertido sin ningun aviso. Por eso el CA se
# corre aqui completo (misma matriz mat_rm, misma ponderacion poblacional que
# 03) y la regresion se entrena y aplica DENTRO de esta misma geometria, que
# es internamente consistente por construccion.
#
# ADVERTENCIA A DECLARAR AL REPORTAR: el ISEI por comunidad es IMPLICITO, no
# medido. Se extrapola proyectando las categorias de habilidad sobre el eje
# Dim1 y aplicando una regresion isei~Dim1 ajustada sobre solo 27 posiciones
# del generador (R2 ~ 0.36). El contraste "mas cohesionada / mas dispersa en
# estatus" tiene entonces una pata solida (phi, medido directo sobre la matriz
# de complementariedad) y una debil (ISEI, extrapolado). Declararlo antes de
# interpretar, no despues.

cat("\n=== 5bis. CARACTERIZACION POR COMUNIDAD ===\n")

# ── Pesos poblacionales por ocupacion, alineados fila a fila con mat_rm ─────
pesos_rm <- rm_casen %>%
  mutate(isco4_orig = suppressWarnings(as.integer(.data[[COL_ISCO_RM]])),
         porcentaje = suppressWarnings(as.numeric(porcentaje))) %>%
  left_join(corr_map, by = "isco4_orig") %>%
  mutate(isco4 = coalesce(isco4_corr, isco4_orig)) %>%
  filter(!is.na(isco4), !is.na(porcentaje)) %>%
  group_by(isco4) %>%
  summarise(porcentaje = sum(porcentaje), .groups = "drop")

pesos_vec <- pesos_rm$porcentaje[match(as.integer(rownames(mat_rm)), pesos_rm$isco4)]

stopifnot(
  "Toda ocupacion de mat_rm debe tener peso poblacional CASEN-RM" =
    !any(is.na(pesos_vec)),
  "El vector de pesos debe alinear fila a fila con mat_rm" =
    length(pesos_vec) == nrow(mat_rm)
)

# ── CA ponderado (misma especificacion que 03_ca_habilidades_isei.R) ────────
ca_com <- FactoMineR::CA(mat_rm, ncp = 5, graph = FALSE, row.w = pesos_vec)

coords_ocupacion <- as.data.frame(ca_com$row$coord) %>%
  tibble::rownames_to_column("isco4") %>%
  mutate(isco4 = as.integer(isco4)) %>%
  rename(Dim1_occ = `Dim 1`, Dim2_occ = `Dim 2`)

coords_habilidad <- as.data.frame(ca_com$col$coord) %>%
  tibble::rownames_to_column("L2_code") %>%
  rename(Dim1_skill = `Dim 1`, Dim2_skill = `Dim 2`)

# ── Regresion isei ~ Dim1, entrenada sobre las 27 posiciones del generador ──
gp_ca <- gp_crosswalk %>%
  mutate(isco4 = suppressWarnings(as.integer(isco4)),
         isei  = suppressWarnings(as.numeric(isei))) %>%
  left_join(coords_ocupacion, by = "isco4") %>%
  filter(!is.na(Dim1_occ), !is.na(isei))

isei_from_dim1 <- lm(isei ~ Dim1_occ, data = gp_ca)
r2_isei <- summary(isei_from_dim1)$r.squared
r_dim1_isei <- cor(gp_ca$Dim1_occ, gp_ca$isei)

cat(sprintf("  Regresion isei ~ Dim1 sobre %d posiciones GP: R2 = %.3f | r(Dim1, ISEI) = %.3f\n",
            nrow(gp_ca), r2_isei, r_dim1_isei))
cat(sprintf("  r(Dim2, ISEI) = %.3f (contraste: el segundo eje NO sigue el prestigio)\n",
            cor(gp_ca$Dim2_occ, gp_ca$isei)))

# ── phi intra-comunidad: media de phi entre pares DENTRO de cada comunidad ──
# Se recalcula phi desde la misma matriz binaria B que uso la deteccion, para
# garantizar que es exactamente la misma medida (no una aproximacion).
phi_mat <- cor(red_rm$B, method = "pearson")
diag(phi_mat) <- NA

phi_intra_comunidad <- function(codigos) {
  if (length(codigos) < 2) return(NA_real_)
  sub <- phi_mat[codigos, codigos, drop = FALSE]
  mean(sub[upper.tri(sub)], na.rm = TRUE)
}

# ── Tabla final de caracterizacion ─────────────────────────────────────────
tabla_caracterizacion <- tibble(
  L2_code   = names(membership_L2),
  comunidad = as.integer(membership_L2)
) %>%
  left_join(coords_habilidad, by = "L2_code") %>%
  mutate(isei_implicito = predict(isei_from_dim1,
                                   newdata = data.frame(Dim1_occ = Dim1_skill))) %>%
  group_by(comunidad) %>%
  summarise(
    etiqueta     = ETIQUETAS_COMUNIDAD[as.character(first(comunidad))],
    n_categorias = n(),
    isei_medio   = mean(isei_implicito, na.rm = TRUE),
    isei_de      = sd(isei_implicito, na.rm = TRUE),
    phi_intra    = phi_intra_comunidad(L2_code),
    .groups = "drop"
  ) %>%
  arrange(comunidad)

cat("\n  Caracterizacion de las 4 comunidades (particion Leiden/RM vigente):\n")
print(as.data.frame(tabla_caracterizacion %>%
                      mutate(across(c(isei_medio, isei_de, phi_intra), ~ round(.x, 3)))),
      row.names = FALSE)

write_csv(tabla_caracterizacion, po("caracterizacion_comunidades.csv"))

# ── Verificacion explicita del hallazgo estructural ────────────────────────
com_mas_cohesionada <- tabla_caracterizacion$etiqueta[which.max(tabla_caracterizacion$phi_intra)]
com_mas_dispersa    <- tabla_caracterizacion$etiqueta[which.max(tabla_caracterizacion$isei_de)]

cat(sprintf("\n  Comunidad mas COHESIONADA (mayor phi intra): %s\n", com_mas_cohesionada))
cat(sprintf("  Comunidad mas DISPERSA en estatus (mayor DE de ISEI): %s\n", com_mas_dispersa))
if (com_mas_cohesionada == com_mas_dispersa) {
  cat("  -> El hallazgo estructural SE VERIFICA: la misma comunidad es la mas\n")
  cat("     cohesionada en habilidades y la mas dispersa en prestigio.\n")
} else {
  cat("  -> ATENCION: el hallazgo NO se verifica en esta corrida. La comunidad\n")
  cat("     mas cohesionada y la mas dispersa en estatus son distintas. Revisar\n")
  cat("     antes de reportar el argumento de cohesion/dispersion.\n")
}

# 6. MAPA DE RED: comunidades de habilidades segun Leiden/RM (NUEVO 18-ago-2026)
# =============================================================================
# Visualizacion de la red de complementariedad (phi, RCA>1) usada para la
# deteccion de comunidades, coloreada por la particion YA ANCLADA por
# contenido (fix C2, Seccion 3 de este mismo script) -- no la version antigua
# de robustez/R4_deteccion_comunidades_L2_global.R, que corre Louvain sobre
# el universo GLOBAL sin ponderar y por lo tanto no corresponde a los
# resultados vigentes de la tesis. Este es el mapa que va en la presentacion.
#
# REVISION 18-ago-2026 (c). Se etiquetan las 110 categorias (todas, no solo
# los hubs por comunidad de la version anterior) y se elimina el mapa
# interactivo (visNetwork) agregado en la revision (b): la version final
# vuelve a ser un unico PNG estatico, pero con nombre real (no codigo crudo)
# en cada nodo. Con 110 etiquetas simultaneas el declutter de aristas pasa a
# ser critico -- se sube el umbral de phi para el dibujo y se usa una fuente
# mas pequena con mayor fuerza de repulsion (ver D9 al final).

cat("\n=== 6. MAPA DE RED: comunidades Leiden/RM ===\n")

COLORES_COMUNIDAD <- c(
  "Direccion_servicio"          = "#173F8A",  # azul UC
  "Tecnico_manual"              = "#E07B39",
  "Analitico_digital_simbolico" = "#3A9B6F",
  "Bio_ambiental_legal"         = "#B23A48"
)

# --- 6.1 Diccionario de etiquetas legibles (codigo L2 -> nombre ESCO) -------
# skill_hier ya esta cargado (Seccion 1). Un mismo Level 2 code puede repetir
# nombre en varias filas (una por skill); distinct() se queda con una.
l2_labels <- skill_hier %>%
  filter(!is.na(`Level 2 code`), !is.na(`Level 2 preferred term`)) %>%
  distinct(`Level 2 code`, `Level 2 preferred term`) %>%
  rename(L2_code = `Level 2 code`, etiqueta_larga = `Level 2 preferred term`)

acortar_etiqueta <- function(x, n = 26) {
  x <- paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "\u2026"), x)
}

grados_red <- degree(red_rm$grafo)

nodos_red <- tibble(
  L2_code   = names(grados_red),
  grado     = grados_red,
  comunidad = ETIQUETAS_COMUNIDAD[as.character(membership_L2[names(grados_red)])]
) %>%
  left_join(l2_labels, by = "L2_code") %>%
  mutate(
    etiqueta_larga = coalesce(etiqueta_larga, L2_code),
    etiqueta_corta = acortar_etiqueta(etiqueta_larga),
    etiqueta_nodo  = sprintf("%s (%s)", etiqueta_corta, L2_code)
  )

n_sin_nombre <- sum(nodos_red$etiqueta_larga == nodos_red$L2_code)
if (n_sin_nombre > 0) {
  cat(sprintf("  Advertencia: %d categorias sin nombre en skillsHierarchy_es.csv (se muestra solo el codigo): %s\n",
              n_sin_nombre, paste(nodos_red$L2_code[nodos_red$etiqueta_larga == nodos_red$L2_code], collapse = ", ")))
}

# Se etiquetan TODOS los nodos de la red (110 categorias), a pedido -- ya no
# se restringe a los hubs por comunidad. anclas se mantiene como referencia
# (las 4 categorias que definen el nombre de cada comunidad), pero como todo
# el grafo lleva etiqueta ya no cumple una funcion de filtro aqui.
grafo_tidy <- as_tbl_graph(red_rm$grafo) |>
  activate(nodes) |>
  left_join(nodos_red, by = c("name" = "L2_code"))

# --- 6.2 Adelgazar aristas SOLO para el dibujo (no para el analisis) -------
# La red completa (phi>0 entre las 110 categorias) satura visualmente
# cualquier layout, y con TODOS los nodos ahora etiquetados el problema se
# agrava: el texto necesita espacio libre.
#
# FIX (18-ago-2026, d): un umbral GLOBAL de percentil de phi (como en las
# revisiones anteriores) no garantiza que CADA nodo conserve al menos una
# arista -- corta por un valor fijo de phi sin mirar nodo por nodo. Algunas
# categorias (ej. "103 servicios de seguridad", "S6.12 lavar y mantener
# textiles") tienen conexiones reales en red_rm$grafo, pero TODAS esas
# conexiones caen por debajo del umbral global del percentil 90, asi que el
# nodo queda sin ninguna arista visible y aparece "flotando" -- un artefacto
# puro del renderizado, no de la red (en las versiones con percentil mas
# bajo, 0.75/0.85, esto era menos probable porque se conservaba mas red en
# total, pero el riesgo seguia latente).
#
# La solucion es la UNION de dos criterios, no uno solo:
#   (a) el EDGE_PLOT_QUANTILE global de mayor phi (declutter general), y
#   (b) para cada nodo, sus EDGE_TOP_K_POR_NODO aristas de mayor phi propias
#       (un "backbone" minimo que garantiza conectividad nodo por nodo).
# Esto es la tecnica estandar de "nearest-neighbor backbone" para dibujar
# redes densas sin perder nodos perifericos. La deteccion de comunidades de
# la Seccion 2-3 sigue usando la red completa; esto es solo para el dibujo.
EDGE_PLOT_QUANTILE   <- 0.90
EDGE_TOP_K_POR_NODO  <- 2

edges_completas <- igraph::as_data_frame(red_rm$grafo, what = "edges") %>%
  as_tibble() %>% mutate(edge_id = row_number())

umbral_plot <- quantile(edges_completas$weight, EDGE_PLOT_QUANTILE)
en_top_global <- edges_completas$edge_id[edges_completas$weight >= umbral_plot]

# Para cada nodo, sus k aristas de mayor phi (viendo cada arista desde
# ambos extremos, porque el grafo no es dirigido).
vecinos_largo <- bind_rows(
  edges_completas %>% transmute(nodo = from, edge_id, weight),
  edges_completas %>% transmute(nodo = to,   edge_id, weight)
)
en_backbone_nodo <- vecinos_largo %>%
  group_by(nodo) %>%
  slice_max(order_by = weight, n = EDGE_TOP_K_POR_NODO, with_ties = FALSE) %>%
  ungroup() %>% pull(edge_id) %>% unique()

edge_ids_mantener <- union(en_top_global, en_backbone_nodo)
mantener_arista <- edges_completas$edge_id %in% edge_ids_mantener

grafo_tidy_plot <- grafo_tidy |>
  activate(edges) |>
  filter(mantener_arista)

# Verificacion explicita: con el backbone por nodo, ningun nodo deberia
# quedar sin al menos una arista en el dibujo. Si esto tronara, hay que
# revisar si algun nodo del grafo tiene grado 0 incluso en la red completa
# (deberia haber sido excluido antes, en construir_red_y_B()).
grados_en_plot <- degree(grafo_tidy_plot)
nodos_aislados_plot <- names(grados_en_plot)[grados_en_plot == 0]
stopifnot(
  "Quedan nodos sin ninguna arista en el mapa pese al backbone por nodo -- revisar EDGE_TOP_K_POR_NODO o si el nodo tiene grado 0 en red_rm$grafo" =
    length(nodos_aislados_plot) == 0
)

cat(sprintf("  Aristas mostradas en el mapa: %d de %d (top %.0f%% global de phi + backbone de %d aristas por nodo, solo para renderizado)\n",
            ecount(grafo_tidy_plot), ecount(red_rm$grafo), (1 - EDGE_PLOT_QUANTILE) * 100, EDGE_TOP_K_POR_NODO))
cat(sprintf("  Categorias rotuladas: %d de %d (todas) | nodos sin arista visible: %d\n",
            nrow(nodos_red), vcount(red_rm$grafo), length(nodos_aislados_plot)))

set.seed(2025)  # layout reproducible; no afecta la particion, solo el dibujo
p_red_comunidades <- ggraph(grafo_tidy_plot, layout = "stress") +
  geom_edge_link(aes(alpha = weight, width = weight), color = "grey70",
                  show.legend = FALSE) +
  scale_edge_width(range = c(0.1, 1.0)) +
  scale_edge_alpha(range = c(0.08, 0.55)) +
  geom_node_point(aes(size = grado, color = comunidad),
                   alpha = 0.9, stroke = 0.3) +
  geom_node_point(aes(size = grado), shape = 21, color = "white",
                   stroke = 0.25, alpha = 0.9, show.legend = FALSE) +
  ggrepel::geom_text_repel(
    aes(x = x, y = y, label = etiqueta_nodo),
    size = 2.2, color = "grey15", fontface = "plain",
    max.overlaps = Inf, force = 6, force_pull = 0.3,
    box.padding = 0.25, point.padding = 0.1,
    min.segment.length = 0, segment.size = 0.15, segment.color = "grey60",
    segment.alpha = 0.6, seed = 2025
  ) +
  scale_color_manual(
    values = COLORES_COMUNIDAD, name = "Comunidad (Leiden, RM)",
    labels = c(
      "Direccion_servicio"          = "Direcci\u00f3n y servicio",
      "Tecnico_manual"              = "T\u00e9cnico-manual",
      "Analitico_digital_simbolico" = "Anal\u00edtico-digital-simb\u00f3lico",
      "Bio_ambiental_legal"         = "Bio-ambiental-legal"
    )
  ) +
  scale_size_continuous(range = c(2, 10), name = "Grado\n(conexiones phi)") +
  guides(color = guide_legend(override.aes = list(size = 5), nrow = 2)) +
  labs(
    title    = "Red de complementariedad de habilidades (\u03c6, RCA>1)",
    subtitle = sprintf(
      "Partici\u00f3n Leiden sobre universo RM \u00b7 %d categor\u00edas ESCO L2 \u00b7 modularidad %.3f\nTodas las categor\u00edas rotuladas con nombre \u00b7 c\u00f3digo ESCO entre par\u00e9ntesis",
      vcount(red_rm$grafo),
      modularity(red_rm$grafo, membership_L2, weights = E(red_rm$grafo)$weight)
    ),
    caption = sprintf("Se dibuja el top %.0f%% de aristas por \u03c6 + las %d m\u00e1s fuertes de cada nodo (garantiza que todos los nodos queden conectados) \u00b7 la detecci\u00f3n de comunidades usa la red completa",
                       (1 - EDGE_PLOT_QUANTILE) * 100, EDGE_TOP_K_POR_NODO)
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle   = element_text(size = 10, hjust = 0.5, color = "grey35", lineheight = 1.15),
    plot.caption    = element_text(size = 8, hjust = 0.5, color = "grey55", margin = margin(t = 8)),
    legend.position = "bottom",
    legend.box      = "vertical",
    legend.title    = element_text(size = 10),
    legend.text     = element_text(size = 9),
    plot.margin     = margin(15, 20, 10, 20)
  )

ggsave(po("fig_red_comunidades_leiden_RM.png"), p_red_comunidades,
       width = 22, height = 18, dpi = 320, bg = "white")

cat("Guardado:", po("fig_red_comunidades_leiden_RM.png"), "\n")
cat("(", vcount(red_rm$grafo), "nodos,", ecount(grafo_tidy_plot), "de", ecount(red_rm$grafo),
    "aristas dibujadas,", nrow(nodos_red), "categorias rotuladas con nombre real )\n")

# Tabla auxiliar (codigo -> nombre completo, comunidad, grado) para pie de
# figura, anexo, o como diccionario de referencia rapida fuera del grafico.
write_csv(
  nodos_red %>% select(L2_code, etiqueta_larga, comunidad, grado) %>%
    arrange(comunidad, desc(grado)),
  po("leyenda_categorias_rotuladas.csv")
)

# =============================================================================
# 8. RESUMEN Y EXPORTACION FINAL
# =============================================================================

cat("\nArchivos generados en", po(""), ":\n")
cat("  - hubs_por_comunidad.csv (insumo para bautizar las comunidades)\n")
cat("  - shares_comunidad_generador_posiciones.csv (27 posiciones GP)\n")
cat("  - shares_comunidad_casen_rm.csv (391 ocupaciones CASEN-RM, con etiqueta legible)\n")
cat("  - fig_red_comunidades_leiden_RM.png (mapa de red, particion vigente)\n")
cat("  - caracterizacion_comunidades.csv (n, ISEI implicito, phi intra por comunidad)\n")

saveRDS(list(membership_L2 = membership_L2, shares_rm = shares_rm,
             gp_shares = gp_shares, n_comunidades = n_comunidades,
             algoritmo_base = ALGORITMO_BASE,
             etiquetas_comunidad = ETIQUETAS_COMUNIDAD,
             grafo = red_rm$grafo, B = red_rm$B),
        po("imputacion_comunidades.rds"))
# NUEVO (19-ago-2026, D11): se agrega el grafo phi (red_rm$grafo) y la matriz
# binaria B al .rds exportado. Antes, robustez/R5_similitud_habilidades_red_vs_ca.R
# reconstruia esta misma red desde cero (codigo duplicado). Ahora 04 y R5
# cargan el grafo ya construido aqui -- fuente unica, igual que ya se hace
# con correcciones_isco_casen.csv. Ver Decision D11 al final.

cat("\n=== FIN: IMPUTACION DE COMUNIDADES A NIVEL DE OCUPACION ===\n")

# =============================================================================
# DECISIONES METODOLOGICAS
# =============================================================================
# D1. Se usa Leiden (modularity) sobre el universo RM como particion base,
#     en vez de Louvain. Justificacion: la comparacion ya realizada
#     (11_comparacion_leiden_louvain_RM.R) mostro que ambos algoritmos son
#     equivalentes en RM (ARI puntual 0.948, ARI mediano bootstrap 0.79 vs.
#     0.76 corregido), y Leiden corrige un defecto documentado de Louvain
#     (comunidades internamente mal conectadas), por lo que no hay razon
#     para seguir usando el algoritmo mas antiguo como base de los
#     indicadores derivados. Cambiar ALGORITMO_BASE a "louvain" reproduce
#     todo con la particion anterior si se prefiere mantenerla.
# D2. El share de cada ocupacion en una comunidad se calcula como proporcion
#     de CATEGORIAS L2 efectivas (RCA>1) que caen en esa comunidad, no como
#     proporcion de habilidades individuales. Esto es consistente con la
#     definicion ya usada para Div_ego (conteo de categorias L2), no con
#     Complex_ego (que cuenta habilidades individuales). Si se prefiere
#     ponderar por habilidades individuales en vez de categorias, hay que
#     reemplazar la matriz binaria B por los conteos crudos de mat_rm antes
#     de agregar por comunidad.
# D3. Categorias de habilidad que quedan fuera de la red (por ejemplo, sin
#     varianza tras el filtro de RCA) no tienen comunidad asignada y se
#     excluyen del calculo de shares, con una advertencia explicita en
#     consola. Esto puede hacer que el share total de una ocupacion sea
#     ligeramente distinto de 1 si esa ocupacion dependia fuertemente de una
#     categoria excluida; revisar la advertencia antes de usar los shares en
#     los modelos.
# D4. Las 27 posiciones GP se cruzan usando el isco4 ya corregido de
#     gp_crosswalk.csv (fuente unica centralizada desde el 20 de julio), sin
#     aplicar correcciones adicionales aqui, para no duplicar logica de
#     correccion en dos lugares distintos.
# D5. La imputacion a la ocupacion de ego queda como punto de integracion
#     documentado (Paso 7), no ejecutado, porque este script no tiene acceso
#     al isco4 de ego ya armonizado (vive en 01_preprocesar_encuesta.R). Se
#     deja la funcion y el join listos para conectar directamente.
# D6. NUEVO (fix C2, 2026-08-18). Los IDs de comunidad que devuelve igraph
#     (cluster_leiden/cluster_louvain) son arbitrarios y pueden cambiar entre
#     corridas sin que el contenido sustantivo de las comunidades cambie.
#     Antes de este fix, share_com1..4 heredaba ese orden arbitrario
#     directamente, y tanto 04_indicadores_red.R como
#     18_modelos_habilidades_origen.R asumian en silencio que share_com1
#     era siempre "Direccion-servicio", etc. -- un supuesto nunca verificado.
#     Ahora membership_L2 se remapea ANTES de calcular shares_rm/gp_shares,
#     usando la misma logica de anclaje por contenido ya validada en
#     14_tabla_grados_completa.R (categorias ancla: S4.9 tomar decisiones ->
#     comunidad 1 Direccion-servicio; 053 ciencias fisicas -> comunidad 2
#     Tecnico-manual; S2.3 gestionar informacion -> comunidad 3
#     Analitico-digital-simbolico; 081 agricultura -> comunidad 4
#     Bio-ambiental-legal). Si en una corrida futura Leiden no encontrara
#     exactamente 4 comunidades, o si dos anclas cayeran en la misma
#     comunidad, el script se detiene con stop() en vez de exportar shares
#     con etiquetas incorrectas. Las etiquetas quedan ademas guardadas
#     explicitamente en imputacion_comunidades.rds (campo
#     etiquetas_comunidad) para que cualquier script aguas abajo pueda
#     verificarlas en vez de asumirlas.
# D7. NUEVO (18-ago-2026, para presentacion a comision). Se agrega
#     fig_red_comunidades_leiden_RM.png: mapa de la red de complementariedad
#     coloreado por la particion Leiden/RM ya anclada (Seccion 6). No
#     confundir con fig_red_comunidades_habilidades_alta_res.png, que
#     produce robustez/R4_deteccion_comunidades_L2_global.R -- ese es
#     Louvain sobre el universo GLOBAL, una particion de robustez/
#     comparacion, no la que sustenta los resultados vigentes de la tesis.
# D8. NUEVO (18-ago-2026, b). Dos cambios al mapa de la Seccion 6, ambos
#     puramente de renderizado (no tocan la deteccion de comunidades, que
#     sigue usando la red completa de la Seccion 2-3):
#     (i)  Las etiquetas de nodo pasan de codigo ESCO crudo (ej. "042") a
#          nombre real + codigo entre parentesis (ej. "Derecho (042)"),
#          usando skillsHierarchy_es.csv ("Level 2 preferred term"). Se
#          preserva el codigo porque el texto de la tesis y las tablas de
#          robustez (bootstrap, grilla de umbrales phi) citan las categorias
#          por codigo, no por nombre.
#     (ii) geom_edge_link ya no dibuja las ~3000+ aristas phi>0: se filtra al
#          25% de mayor peso (EDGE_PLOT_QUANTILE=0.75) solo para el dibujo,
#          porque con las ~5000 aristas completas el mapa se vuelve una
#          "bola de pelo" ilegible (ver fig_red_comunidades_leiden_RM.png
#          version anterior). Layout cambia de "fr" a "stress"
#          (graphlayouts), que separa mejor comunidades densas. Si se
#          necesita citar la cantidad exacta de aristas de la red analitica
#          completa, usar ecount(red_rm$grafo) (Seccion 2), no el grafico.
#     Se exporta ademas leyenda_categorias_rotuladas.csv (codigo, nombre
#     completo, comunidad, grado) como insumo para pie de figura o anexo si
#     se prefiere una leyenda separada en vez de nombre+codigo en el nodo.
# D9. NUEVO (18-ago-2026, c). Revertido el mapa interactivo agregado en D8-b:
#     a pedido, la version final vuelve a ser un unico PNG estatico, pero
#     ahora con las 110 categorias etiquetadas (antes solo 5-10 por
#     comunidad). Cambios especificos respecto de D8:
#     (i)  Se elimina la Seccion 6b (visNetwork) completa y las librerias
#          visNetwork/htmlwidgets del bloque de carga inicial. Ya no se
#          genera fig_red_comunidades_leiden_RM_interactivo.html.
#     (ii) mostrar_etiqueta / nodos_a_rotular (el filtro de hubs) se
#          eliminan: geom_text_repel ahora usa etiqueta_nodo directamente
#          para todos los nodos del grafo filtrado.
#     (iii) Para que 110 etiquetas simultaneas sigan siendo legibles:
#          EDGE_PLOT_QUANTILE sube a 0.90 (solo 10% de aristas con mayor phi
#          dibujadas, vs. 25% y 15% en revisiones previas), el tamano de
#          fuente de las etiquetas baja a 2.2, la fuerza de repulsion de
#          ggrepel sube (force=6), y el lienzo crece a 22x18" a 320 dpi. Aun
#          asi, con 110 nombres en un area finita algo de sobreposicion
#          residual es esperable en las zonas mas densas de cada comunidad;
#          si eso resulta un problema para la version final de la tesis,
#          la alternativa mas simple es imprimir a mayor tamano fisico (ej.
#          poster o anexo A3) en vez de reducir mas la fuente.
#     leyenda_categorias_rotuladas.csv ahora exporta las 110 categorias (ya
#     no un subconjunto de hubs), como diccionario completo codigo->nombre.
# D10. NUEVO (18-ago-2026, d). Fix a un artefacto reportado tras D9: con
#     EDGE_PLOT_QUANTILE como umbral GLOBAL de phi (top 10%), algunos nodos
#     quedaban SIN NINGUNA arista dibujada -- no porque estuvieran aislados
#     en la red real (red_rm$grafo), sino porque TODAS sus aristas
#     individuales caian por debajo del umbral global. Un umbral global no
#     garantiza minimo de grado por nodo, solo un total de aristas.
#     Fix: el filtro de aristas del dibujo pasa a ser la UNION de (a) el top
#     EDGE_PLOT_QUANTILE global por phi, y (b) para cada nodo, sus
#     EDGE_TOP_K_POR_NODO=2 aristas propias de mayor phi (un "backbone"
#     nodo-a-nodo que garantiza conectividad visible). Se agrega un
#     stopifnot() explicito despues del filtro que verifica que ningun nodo
#     quede con grado 0 en el grafico; si eso ocurriera, el script se
#     detiene en vez de exportar una figura con nodos flotantes en silencio.
#     Sigue siendo puramente de renderizado: la deteccion de comunidades de
#     la Seccion 2-3 no se toca.
# D11. NUEVO (19-ago-2026). imputacion_comunidades.rds ahora incluye el grafo
#     phi (red_rm$grafo) y la matriz binaria B, ademas de membership_L2 y
#     los shares. Motivo: SH_ip_red (distancia geodesica en la red phi,
#     especificacion principal de H1b desde el 19-ago-2026) se calcula en
#     04_indicadores_red.R a partir de este grafo. Antes de este cambio,
#     robustez/R5_similitud_habilidades_red_vs_ca.R reconstruia la red
#     completa por su cuenta (Secciones 1-2, verbatim de este script) solo
#     para tener acceso al grafo -- duplicacion que ahora se elimina. 08
#     sigue siendo la UNICA fuente que construye la red desde mat_rm.
#     NOTA DE RECONCILIACION (20-ago-2026): esta linea de trabajo (fix de
#     H1b/SH_ip_red, 19-ago) y la del rediseno del mapa de red (D8-D10,
#     18-ago b/c/d) se desarrollaron en paralelo sobre copias distintas del
#     mismo script y un push (commit 2a9eb11) sobrescribio la primera con la
#     segunda sin fusionarlas -- Seccion 5bis y el export de grafo/B quedaron
#     fuera del repo remoto pese a que la tesis (via SH_ip_red en 05) ya
#     dependia de ellos. Este commit fusiona ambas lineas. Ver auditoria del
#     20-ago-2026 para el detalle de como se detecto y reconstruyo.
# =============================================================================
