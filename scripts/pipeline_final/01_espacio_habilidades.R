################################################################################
# 01_espacio_habilidades.R   PIPELINE FINAL
#
# Construye el espacio de habilidades sobre el universo ocupacional de la Region
# Metropolitana. Reemplaza a codigo/00, codigo_v2/02, codigo_v2/08 y la seccion 2
# de paralelo_v3/112.
#
#   1. Universo RM: ocupaciones CIUO-08 de los ocupados de la RM (CASEN 2024),
#      con las correcciones CIUO-08.CL -> ISCO-08 (CORRECCIONES_ISCO, 00_config.R).
#   2. Matriz ocupacion x categoria L2: habilidades esenciales de MAPHA v2.06,
#      cada habilidad asignada a su categoria L2 de ESCO.
#   3. Matriz binaria B: categoria efectiva si su RCA > 1.
#   4. Red de complementariedad: phi (Pearson entre columnas de B), aristas phi > 0.
#   5. Comunidades: Leiden (modularidad), anclaje por contenido.
#   6. Distancias geodesicas entre categorias (pesos 1 - phi) y funcion de
#      similitud de habilidades entre dos ocupaciones.
#
# Salidas: intermedios/espacio_habilidades.rds | intermedios/frecuencias_casen_rm.csv
#          tablas/comunidades_particion.csv | tablas/comunidades_caracterizacion.csv
#          figuras/fig_mapa_habilidades (version de trabajo, con pie; la del
#          articulo es art_fig_mapa_habilidades, de 07, con la misma disposicion)
################################################################################

source("00_config.R", encoding = "UTF-8")
suppressPackageStartupMessages({ library(ggraph); library(tidygraph) })

leer <- function(ruta) {
  if (!file.exists(ruta)) stop("No existe: ", ruta, call. = FALSE)
  suppressWarnings(read_csv(ruta, col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE))
}

# ==== 1. Universo RM (CASEN 2024) ===================================================
titulo("1. Universo ocupacional RM (CASEN 2024)")
env <- new.env(); load(ARCH$casen, envir = env)
nm <- names(env)[vapply(names(env), function(n) is.data.frame(env[[n]]), logical(1))][1]
casen_rm <- env[[nm]] %>% select(region, activ, oficio4_08, expr) %>% zap_labels() %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.x)))) %>%
  filter(region == 13, activ == 1, !is.na(oficio4_08), oficio4_08 > 0, oficio4_08 < 9990)
rm(env); invisible(gc())
frec_casen <- casen_rm %>% transmute(isco = sprintf("%04d", as.integer(oficio4_08)), expr) %>%
  group_by(isco) %>% summarise(n_casen = n(), pond = sum(expr), .groups = "drop")
write_csv(frec_casen, file.path(DIR_INT, "frecuencias_casen_rm.csv"))

corr_map <- CORRECCIONES_ISCO %>% transmute(isco4_orig = isco_orig, isco4_corr = isco_corr)
universo_rm <- tibble(isco4_orig = as.integer(frec_casen$isco)) %>%
  left_join(corr_map, by = "isco4_orig") %>% mutate(isco4 = coalesce(isco4_corr, isco4_orig)) %>%
  pull(isco4) %>% unique()
cat(sprintf("Ocupados RM con codigo valido: %d (%.0f ponderados) | codigos: %d\n",
            nrow(casen_rm), sum(frec_casen$pond), length(universo_rm)))

# ==== 2. Matriz ocupacion x categoria L2 (MAPHA) ====================================
titulo("2. Matriz ocupacion x categoria L2 (MAPHA v2.06, habilidades esenciales)")
# Asignacion habilidad -> categoria L2 (igual que codigo_v2/02 y 08):
#  a) habilidades de origen ESCO: habilidad -> SkillGroup padre directo -> L2 (un salto);
#  b) habilidades de la curatoria Relink (sin URI ESCO): se sube por la jerarquia de
#     MAPHA hasta el primer grupo cuyo codigo o nombre corresponde a una L2 de ESCO.
construir_mapa_skill_L2 <- function(esco_dir, mapha_dir) {
  br    <- leer(file.path(esco_dir, "broaderRelationsSkillPillar_es.csv"))
  sh_es <- leer(file.path(esco_dir, "skillsHierarchy_es.csv"))
  skill_to_group <- br %>% filter(conceptType == "KnowledgeSkillCompetence", broaderType == "SkillGroup") %>%
    select(skill_uri = conceptUri, group_uri = broaderUri) %>% distinct()
  group_to_L2 <- bind_rows(
    sh_es %>% filter(!is.na(`Level 3 URI`), !is.na(`Level 2 code`)) %>% select(group_uri = `Level 3 URI`, L2_code = `Level 2 code`),
    sh_es %>% filter(!is.na(`Level 2 URI`), !is.na(`Level 2 code`)) %>% select(group_uri = `Level 2 URI`, L2_code = `Level 2 code`)) %>%
    distinct(group_uri, .keep_all = TRUE)
  skill_L2_esco <- skill_to_group %>% left_join(group_to_L2, by = "group_uri") %>%
    filter(!is.na(L2_code)) %>% distinct(skill_uri, L2_code)
  l2_validos <- unique(skill_L2_esco$L2_code)
  por_nombre <- sh_es %>% filter(!is.na(`Level 2 code`), !is.na(`Level 2 preferred term`)) %>%
    distinct(`Level 2 preferred term`, .keep_all = TRUE)
  l2_por_nombre <- setNames(por_nombre$`Level 2 code`, por_nombre$`Level 2 preferred term`)

  skl <- leer(file.path(mapha_dir, "skills.csv"))
  sh_m <- leer(file.path(mapha_dir, "skillsHierarchy.csv")) %>% filter(broaderType == "skillGroup")
  grupos <- skl %>% filter(isGroup == "True")
  cod_grupo <- setNames(grupos$code, grupos$id); nom_grupo <- setNames(grupos$name, grupos$id)
  padre <- sh_m %>% distinct(conceptId, .keep_all = TRUE); padre <- setNames(padre$broaderId, padre$conceptId)

  map_esco <- skl %>% filter(origin == "ESCO") %>% select(skillId = id, skill_uri = originId) %>%
    left_join(skill_L2_esco, by = "skill_uri") %>% select(skillId, L2_code) %>% filter(!is.na(L2_code))
  encontrar_L2 <- function(id, max_saltos = 6) {
    a <- id
    for (i in seq_len(max_saltos)) {
      cod <- unname(cod_grupo[a]); if (!is.na(cod) && cod %in% l2_validos) return(cod)
      nom <- unname(nom_grupo[a]); if (!is.na(nom) && nom %in% names(l2_por_nombre)) return(unname(l2_por_nombre[nom]))
      s <- unname(padre[a]); if (is.na(s) || identical(s, a)) return(NA_character_)
      a <- s
    }
    NA_character_
  }
  relink <- skl %>% filter(origin == "Curatoría Relink") %>% pull(id)
  map_relink <- tibble(skillId = relink, L2_code = vapply(relink, encontrar_L2, character(1))) %>% filter(!is.na(L2_code))
  cat(sprintf("  Habilidades asignadas a L2: ESCO %d de %d | Relink %d de %d\n",
              nrow(map_esco), sum(skl$origin %in% "ESCO"), nrow(map_relink), length(relink)))
  bind_rows(map_esco, map_relink) %>% distinct(skillId, .keep_all = TRUE)
}
skill_L2 <- construir_mapa_skill_L2(ARCH$esco_dir, ARCH$mapha_dir)

osk <- leer(file.path(ARCH$mapha_dir, "occupationsSkills.csv")) %>%
  filter(relationType == "essential") %>% select(occupationId, skillId) %>% distinct()
occ <- leer(file.path(ARCH$mapha_dir, "occupations.csv")) %>%
  transmute(occupationId = id, isco4 = suppressWarnings(as.integer(iscoGroup)), occ_label = name) %>% filter(!is.na(isco4))
mat_global <- osk %>% inner_join(skill_L2, by = "skillId", relationship = "many-to-many") %>%
  inner_join(occ %>% select(occupationId, isco4), by = "occupationId", relationship = "many-to-many") %>%
  count(isco4, L2_code) %>% pivot_wider(names_from = L2_code, values_from = n, values_fill = 0) %>%
  column_to_rownames("isco4") %>% as.matrix()
mat_rm <- mat_global[rownames(mat_global) %in% as.character(universo_rm), , drop = FALSE]
mat_rm <- mat_rm[, colSums(mat_rm) > 0, drop = FALSE]
cat(sprintf("Matriz RM: %d ocupaciones x %d categorias L2\n", nrow(mat_rm), ncol(mat_rm)))

# ==== 3 y 4. Matriz binaria B y red phi ==============================================
titulo("3. Matriz B (RCA > 1) y red de complementariedad phi")
esperado <- outer(rowSums(mat_rm), colSums(mat_rm)) / sum(mat_rm)
B <- (mat_rm / esperado > RCA_UMBRAL) * 1L
B <- B[, apply(B, 2, function(x) length(unique(x)) > 1), drop = FALSE]
phi <- cor(B); diag(phi) <- 0
aristas <- as.data.frame(as.table(phi)) %>% rename(from = Var1, to = Var2, phi = Freq) %>%
  mutate(from = as.character(from), to = as.character(to)) %>%
  filter(from < to, phi > PHI_MIN_ARISTA)
grafo <- graph_from_data_frame(aristas, directed = FALSE, vertices = data.frame(name = colnames(B)))
E(grafo)$weight <- aristas$phi
cat(sprintf("B: %d x %d | red: %d nodos, %d aristas, %d componente(s)\n",
            nrow(B), ncol(B), vcount(grafo), ecount(grafo), components(grafo)$no))

# ==== 5. Comunidades (Leiden) =========================================================
titulo("5. Comunidades de habilidades (Leiden, modularidad)")
set.seed(SEMILLA_LEIDEN)
part <- cluster_leiden(grafo, objective_function = "modularity", weights = E(grafo)$weight, n_iterations = 10)
memb <- setNames(as.integer(membership(part)), V(grafo)$name)
stopifnot("Leiden debe devolver 4 comunidades" = length(unique(memb)) == 4,
          "Las anclas deben caer en 4 comunidades distintas" = length(unique(memb[ANCLAS_COM])) == 4)
memb <- setNames(match(memb, memb[ANCLAS_COM]), names(memb))   # 1..4 en el orden de ANCLAS_COM
Q <- modularity(grafo, memb, weights = E(grafo)$weight)
cat(sprintf("Modularidad: %.4f | categorias por comunidad: %s\n", Q, paste(table(memb), collapse = ", ")))

if (VALIDAR_CONTRA_ANTERIOR && file.exists(ARCH$v2_comunidad)) {   # validacion contra la particion usada hasta ahora
  v2 <- readRDS(ARCH$v2_comunidad)$membership_L2
  comun <- intersect(names(v2), names(memb))
  cat(sprintf("Validacion: misma particion que codigo_v2/08 en %d de %d categorias\n",
              sum(v2[comun] == memb[comun]), length(comun)))
  chequeo("Modularidad (codigo_v2/08 reportaba 0,388)", round(Q, 3), 0.388, tol = 1e-6)
}

write_csv(tibble(L2_code = names(memb), comunidad = NOMBRES_COM[memb]), file.path(DIR_TAB, "comunidades_particion.csv"))

# Shares de cada ocupacion: proporcion de sus categorias efectivas en cada comunidad
shares <- sapply(1:4, function(k) rowSums(B[, names(memb)[memb == k], drop = FALSE])) / rowSums(B)
colnames(shares) <- paste0("share_com", 1:4)
shares <- as_tibble(shares) %>% mutate(isco4 = rownames(B), n_categorias = rowSums(B), .before = 1)

# Caracterizacion de las comunidades: tamano y phi medio dentro de la comunidad
carac <- map_dfr(1:4, function(k) {
  cs <- names(memb)[memb == k]; sub <- phi[cs, cs]
  tibble(comunidad = NOMBRES_COM[k], n_categorias = length(cs), phi_intra = mean(sub[upper.tri(sub)]),
         ancla = ANCLAS_COM[k])
})
print(carac)
write_csv(carac, file.path(DIR_TAB, "comunidades_caracterizacion.csv"))

# ==== 6. Distancias y similitud de habilidades ==========================================
# (las funciones sim_hab y sim_hab_centrada se redefinen en 03 a partir de D_geo,
# cats y d_auto; no se guardan funciones en el .rds para no arrastrar su entorno)
titulo("6. Distancias geodesicas y similitud de habilidades")
D_geo <- distances(grafo, weights = 1 - E(grafo)$weight)
cats <- setNames(lapply(seq_len(nrow(B)), function(i) colnames(B)[B[i, ] == 1]), rownames(B))
# Similitud de habilidades entre dos ocupaciones: menos la distancia geodesica
# media entre todas las categorias efectivas de una y de otra.
sim_hab <- function(a, b) {
  ca <- cats[[a]]; cb <- cats[[b]]
  if (is.null(ca) || is.null(cb) || !length(ca) || !length(cb)) return(NA_real_)
  -mean(D_geo[ca, cb])
}
# Version centrada (robustez R5): resta la dispersion interna de cada perfil,
# de modo que una ocupacion es siempre la mas parecida a si misma (distancia de
# energia: 2 d(a,b) - d(a,a) - d(b,b) >= 0).
d_auto <- vapply(cats, function(cc) if (length(cc)) mean(D_geo[cc, cc]) else NA_real_, numeric(1))
sim_hab_centrada <- function(a, b) {
  s <- sim_hab(a, b); if (is.na(s)) return(NA_real_)
  -(2 * (-s) - d_auto[[a]] - d_auto[[b]]) / 2
}

# ==== Figura: mapa de la red ============================================================
# La disposicion se calcula con TODAS las aristas (asi ninguna categoria queda
# suelta); solo se dibuja el 10% de aristas con mayor phi.
g_plot <- as_tbl_graph(grafo) %>% activate(nodes) %>%
  mutate(comunidad = factor(NOMBRES_COM[memb[name]], levels = NOMBRES_COM), grado = centrality_degree())
umbral_phi <- quantile(E(grafo)$weight, 0.90)
# Misma disposicion que 07 y 08 (fr, pesos phi, 2000 iteraciones, misma semilla).
set.seed(SEMILLA_LEIDEN)
fig_red <- ggraph(g_plot, layout = "fr", weights = weight, niter = 2000) +
  geom_edge_link(aes(filter = weight >= umbral_phi, alpha = weight), colour = PALETA_TESIS[["gris"]], show.legend = FALSE) +
  geom_node_point(aes(fill = comunidad, size = grado), shape = 21, colour = BORDE_PUNTOS, stroke = 0.3) +
  scale_fill_manual(values = PALETA_COM) + scale_size(range = c(1.2, 5), guide = "none") +
  guides(fill = guide_legend(nrow = 2, override.aes = list(size = 3))) +
  labs(caption = sprintf("%d categorias L2 de ESCO. Disposicion segun todas las aristas; se dibuja el 10%% con mayor phi.\nLeiden, modularidad %s.",
                         vcount(grafo), fnum(Q))) +
  theme_void(base_size = 11) + theme(legend.position = "bottom", legend.title = element_blank(),
                                     plot.caption = element_text(colour = PALETA_TESIS[["gris_oscuro"]], size = 8, hjust = 0))
guardar_fig(fig_red, file.path(DIR_FIG, "fig_mapa_habilidades"), w = 7, h = 6)

saveRDS(list(B = B, grafo = grafo, phi = phi, membership = memb, modularidad = Q, shares = shares,
             D_geo = D_geo, cats = cats, d_auto = d_auto,
             nombres_com = NOMBRES_COM, ocupaciones_mapha = occ),
        file.path(DIR_INT, "espacio_habilidades.rds"))
cat("\nGuardado: intermedios/espacio_habilidades.rds\n")

################################################################################
# DECISIONES METODOLOGICAS
# 1. Fuente de habilidades: MAPHA v2.06, solo relaciones "essential".
# 2. Habilidad -> L2: un salto para las habilidades ESCO (grupo padre directo);
#    las habilidades de la curatoria Relink no tienen URI ESCO y se asignan
#    subiendo por la jerarquia de MAPHA (1 a 4 saltos) hasta una L2 de ESCO.
# 3. Universo: ocupaciones de los ocupados de la RM en CASEN 2024 con las
#    correcciones CIUO-08.CL -> ISCO-08. La red no se pondera por poblacion: es
#    una propiedad del espacio ocupacional.
# 4. B: categoria efectiva si RCA > 1. phi = correlacion de Pearson entre
#    columnas de B; aristas con phi > 0; distancia = 1 - phi.
# 5. Leiden (modularidad), semilla fija inmediatamente antes. Los IDs se fijan
#    por contenido con las anclas S4.9, 053, S2.3 y 081.
# 6. Similitud de habilidades: menos la distancia geodesica media entre las
#    categorias efectivas de dos ocupaciones. Propiedad conocida: no siempre es
#    maxima para una ocupacion consigo misma (perfiles compactos atraen); por
#    eso se agrega la version centrada como robustez.
################################################################################
