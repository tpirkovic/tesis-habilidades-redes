################################################################################
# 08_grafo_bimodal.R   FIGURAS COMPLEMENTARIAS (las corre run_pipeline.R al final)
#
# Red bimodal ocupacion - categoria de habilidades. Un vinculo une una ocupacion
# con una categoria L2 de ESCO cuando la categoria es efectiva en la ocupacion
# (RCA > 1): la celda B[ocupacion, categoria] de 01_espacio_habilidades.R vale 1.
# Solo hay vinculos ocupacion - categoria (no se dibujan vinculos entre categorias
# ni entre ocupaciones).
#
# Estilo grafico de los grafos D31 (12_grafos_niveles_AB.R): fondo blanco,
# Fruchterman-Reingold, circulo = nivel A (ocupaciones, un solo color),
# cuadrado = nivel B (categorias, color de su comunidad de habilidades), vinculos
# grises finos, etiquetas pequenas, titulo + subtitulo con conteos, leyenda abajo.
# Colores: paleta Navy-carmesi (00_tema_graficos.R).
#
# No reestima nada: lee las salidas guardadas de 01 y 02 (igual que 07).
#
# Salidas (figuras/):
#   art_fig_bimodal_ejemplo   5 posiciones del GP y sus categorias, todo etiquetado:
#                             version con datos del esquema conceptual
#   art_fig_bimodal_gp        las 27 posiciones del generador de posiciones
#   art_fig_bimodal_completa  391 ocupaciones del universo RM x 112 categorias (anexo)
#   art_fig_bimodal_matriz    la misma red completa como matriz ordenada por
#                             comunidad (alternativa sin lineas para el anexo)
#   art_fig_bimodal_ejemplos  mapa de la Figura 1 repetido en seis paneles: en cada uno,
#                             los vinculos de una sola posicion del GP
#   art_fig_bimodal_27_filas  las 27 posiciones en dos filas (ocupaciones arriba,
#                             categorias abajo agrupadas por comunidad), como el esquema
# Salidas (tablas/):
#   resumen_bimodal.csv       conteos por red, para verificar y para las notas
#   bimodal_gp_vinculos.csv   vinculos de las 27 posiciones (anexo)
################################################################################

# Directorio de trabajo: la carpeta pipeline_final (igual que run_pipeline.R).
RUTA_POR_DEFECTO <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/pipeline_final"
if (!file.exists("00_config.R")) {
  if (!file.exists(file.path(RUTA_POR_DEFECTO, "00_config.R")))
    stop("No se encuentra pipeline_final en: ", RUTA_POR_DEFECTO)
  setwd(RUTA_POR_DEFECTO)
}
source("00_config.R", encoding = "UTF-8")
paquetes <- c("ggraph", "ggrepel", "ragg", "patchwork")
faltan_pkg <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan_pkg) > 0) install.packages(faltan_pkg, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(library(ggraph))

# ---- Configuracion ------------------------------------------------------------
POS_EJEMPLO <- c("Q0114", "Q0120",   # Medicos, Abogados
                 "Q0119", "Q0103",   # Electricistas, Mecanicos de autos
                 "Q0102")            # Profesores de educacion media o basica
COL_OCU  <- PALETA_TESIS[["azul_oscuro"]]   # nivel A: ocupaciones (azul marino)
COL_EDGE <- "#9C9C9C"                       # vinculos (UC gris oscuro B)
ETQ_NIVEL <- c(A = "Ocupación", B = "Categoría de habilidades (ESCO, nivel 2)")

# Parametros por figura: tamano de nodos (constante), transparencia y ancho de
# vinculos, etiquetas ("ocupaciones", "todas", "ninguna") y tamano en pulgadas
PAR <- list(
  ejemplo  = list(tam_a = 4.0, tam_b = 2.6, alfa = 0.70, ancho = 0.45, etiquetas = "todas",
                  tam_etq = 2.6, w = 9, h = 8,
                  titulo = "Ocupaciones vinculadas por sus categorías de habilidades"),
  gp       = list(tam_a = 3.2, tam_b = 1.8, alfa = 0.45, ancho = 0.30, etiquetas = "ocupaciones",
                  tam_etq = 2.3, w = 10, h = 10,
                  titulo = "Red bimodal de las ocupaciones del generador de posiciones"),
  completa = list(tam_a = 0.7, tam_b = 1.8, alfa = 0.05, ancho = 0.20, etiquetas = "ninguna",
                  tam_etq = 2, w = 10, h = 10,
                  titulo = "Red bimodal del universo ocupacional de la Región Metropolitana"))
DPI <- 400
TITULOS_EN_FIGURA <- FALSE   # FALSE: sin titulo ni subtitulo (van en la nota, como en 07)

# Paneles sobre el mapa (art_fig_bimodal_ejemplos): pares de posiciones cercanas
POS_PANELES <- c("Q0119", "Q0103",   # Electricistas | Mecanicos de autos
                 "Q0102", "Q0124",   # Profesores | Parvularios
                 "Q0105", "Q0120")   # Ingenieros | Abogados
ORDEN_COM <- c(2, 4, 1, 3)           # bloques de categorias en la figura de dos filas

# ---- Insumos -------------------------------------------------------------------
eh   <- readRDS(file.path(DIR_INT, "espacio_habilidades.rds"))
B    <- eh$B
memb <- eh$membership
cw   <- read_csv(file.path(DIR_INT, "crosswalk_gp27.csv"), col_types = cols(.default = col_character())) %>%
  mutate(isco_hab = as.character(as.integer(isco_habilidades)))
stopifnot("Categorias de B sin comunidad" = all(colnames(B) %in% names(memb)),
          "Codigos de las posiciones fuera de B" = all(cw$isco_hab %in% rownames(B)),
          "Posiciones desconocidas en POS_EJEMPLO" = all(POS_EJEMPLO %in% cw$posicion))

cap <- function(s) paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
sh_es <- suppressWarnings(read_csv(file.path(ARCH$esco_dir, "skillsHierarchy_es.csv"),
                                   col_types = cols(.default = "c"), show_col_types = FALSE)) %>%
  filter(!is.na(`Level 2 code`), !is.na(`Level 2 preferred term`)) %>% distinct(`Level 2 code`, .keep_all = TRUE)
nom_cat <- setNames(sh_es$`Level 2 preferred term`, sh_es$`Level 2 code`)
etq_cat <- function(cats) { n <- unname(nom_cat[cats]); n[is.na(n)] <- cats[is.na(n)]; str_trunc(cap(n), 40) }

# ---- Construccion de cada red (solo vinculos ocupacion - categoria) ----------------
construir_red <- function(filas, ids, etiquetas) {
  M <- B[filas, , drop = FALSE]; rownames(M) <- ids
  M <- M[, colSums(M) > 0, drop = FALSE]
  idx <- which(M == 1, arr.ind = TRUE)
  el  <- tibble(from = rownames(M)[idx[, 1]], to = colnames(M)[idx[, 2]])
  v <- bind_rows(
    tibble(name = ids, nivel = "A", comunidad = NA_character_, etiqueta = etiquetas),
    tibble(name = colnames(M), nivel = "B", comunidad = unname(NOMBRES_COM[memb[colnames(M)]]),
           etiqueta = etq_cat(colnames(M))))
  g <- graph_from_data_frame(el, directed = FALSE, vertices = v)
  stopifnot(ecount(g) == sum(M))
  list(g = g, M = M, el = el)
}

ej <- cw %>% filter(posicion %in% POS_EJEMPLO) %>% slice(match(POS_EJEMPLO, posicion))
redes <- list(
  ejemplo  = construir_red(ej$isco_hab, ej$posicion, ej$label),
  gp       = construir_red(cw$isco_hab, cw$posicion, cw$label),
  completa = construir_red(rownames(B), paste0("o", rownames(B)), NA_character_))

# ---- Resumen para verificar ------------------------------------------------------
resumen <- imap_dfr(redes, function(r, n) {
  M <- r$M
  compartidas <- sum(colSums(M) >= 2)
  tibble(red = n, ocupaciones = nrow(M), categorias = ncol(M), vinculos = sum(M),
         densidad = mean(M), categorias_por_ocupacion = mean(rowSums(M)),
         categorias_compartidas = compartidas, componentes = components(r$g)$no)
})
print(resumen %>% mutate(across(where(is.double), ~ round(.x, 3))), width = Inf)
chequeo("Vinculos de la red completa = celdas de B iguales a 1", resumen$vinculos[resumen$red == "completa"], sum(B))
chequeo("Categorias por posicion igual a n_categorias de 02",
        sum(rowSums(redes$gp$M) != as.integer(cw$n_categorias[match(rownames(redes$gp$M), cw$posicion)])), 0)
write_csv(resumen, file.path(DIR_TAB, "resumen_bimodal.csv"))
write_csv(redes$gp$el %>% transmute(posicion = from, ocupacion = cw$label[match(from, cw$posicion)], L2_code = to,
                                    categoria = etq_cat(to), comunidad = unname(NOMBRES_COM[memb[to]])) %>%
            arrange(posicion, comunidad, categoria),
          file.path(DIR_TAB, "bimodal_gp_vinculos.csv"))

# ---- Layout (igual que los scripts 12 y 13) ------------------------------------------
calcular_layout <- function(g) {
  set.seed(SEMILLA_LEIDEN)
  xy <- layout_components(g, layout = layout_with_fr, niter = 3000)
  rownames(xy) <- V(g)$name
  xy
}
layouts <- lapply(redes, function(r) calcular_layout(r$g))

# ---- Grafico ---------------------------------------------------------------------
graficar <- function(n) {
  g  <- redes[[n]]$g
  xy <- layouts[[n]]
  pr <- PAR[[n]]
  r  <- resumen[resumen$red == n, ]
  V(g)$etq <- switch(pr$etiquetas,
                     ocupaciones = ifelse(V(g)$nivel == "A", V(g)$etiqueta, NA),
                     todas       = V(g)$etiqueta,
                     ninguna     = NA_character_)
  V(g)$nivel     <- factor(V(g)$nivel, levels = c("A", "B"))
  V(g)$comunidad <- factor(V(g)$comunidad, levels = NOMBRES_COM)
  subt <- sprintf("%d ocupaciones, %d categorías de habilidades | vínculos ocupación-categoría = %s",
                  r$ocupaciones, r$categorias, formatC(r$vinculos, big.mark = ".", decimal.mark = ",", format = "d"))

  p <- ggraph(g, layout = "manual", x = xy[, 1], y = xy[, 2]) +
    geom_edge_link(edge_colour = COL_EDGE, edge_width = pr$ancho, edge_alpha = pr$alfa) +
    # Categorias primero; ocupaciones encima
    geom_node_point(aes(filter = nivel == "B", fill = comunidad), shape = 22,
                    size = pr$tam_b, colour = BORDE_PUNTOS, stroke = 0.25) +
    geom_node_point(aes(filter = nivel == "A", shape = nivel), fill = COL_OCU,
                    size = pr$tam_a, colour = "white", stroke = 0.3) +
    scale_shape_manual(name = "Nodos", values = c(A = 21), labels = ETQ_NIVEL["A"]) +
    scale_fill_manual(name = "Categorías por comunidad", values = PALETA_COM, drop = FALSE) +
    guides(shape = guide_legend(order = 1, override.aes = list(size = 4, fill = COL_OCU)),
           fill  = guide_legend(order = 2, nrow = 2, override.aes = list(shape = 22, size = 4))) +
    labs(title = if (TITULOS_EN_FIGURA) pr$titulo, subtitle = if (TITULOS_EN_FIGURA) subt) +
    theme_graph(base_family = "sans", background = "white") +
    theme(legend.position = "bottom", legend.box = "horizontal",
          plot.margin = margin(10, 20, 10, 20),
          plot.title = element_text(size = 15, face = "bold"),
          plot.subtitle = element_text(size = 9, colour = "grey30"))

  if (pr$etiquetas != "ninguna")
    p <- p + geom_node_text(aes(label = etq, fontface = ifelse(nivel == "A", "bold", "plain")),
                            size = pr$tam_etq, colour = "grey15",
                            repel = TRUE, max.overlaps = 60, segment.colour = "grey60",
                            segment.size = 0.2, min.segment.length = 0.2, na.rm = TRUE,
                            seed = SEMILLA_LEIDEN)
  p
}

guardar <- function(p, nombre, ancho, alto) {
  dev_png <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
  ggsave(file.path(DIR_FIG, paste0(nombre, ".png")), p, width = ancho, height = alto,
         dpi = DPI, bg = "white", device = dev_png)
  ggsave(file.path(DIR_FIG, paste0(nombre, ".pdf")), p, width = ancho, height = alto, device = pdf)
}

titulo("Redes bimodales ocupacion - categoria de habilidades")
for (n in names(redes)) {
  tryCatch({
    guardar(graficar(n), paste0("art_fig_bimodal_", n), PAR[[n]]$w, PAR[[n]]$h)
    cat("  [OK] art_fig_bimodal_", n, "\n", sep = "")
  }, error = function(e) cat("  [ERROR] art_fig_bimodal_", n, ": ", conditionMessage(e), "\n", sep = ""))
}

# ---- Matriz de la red completa (alternativa para el anexo) ---------------------------
# Filas: ocupaciones ordenadas por comunidad dominante y por su porcion en ella.
# Columnas: categorias ordenadas por comunidad y por numero de ocupaciones.
# Celda coloreada = categoria efectiva en la ocupacion (vinculo de la red bimodal).
tryCatch({
  shares <- as.matrix(eh$shares[, paste0("share_com", 1:4)]); rownames(shares) <- eh$shares$isco4
  dom <- max.col(shares, ties.method = "first")
  orden_f <- rownames(shares)[order(dom, -shares[cbind(seq_along(dom), dom)])]
  orden_c <- colnames(B)[order(memb[colnames(B)], -colSums(B))]
  mat <- as_tibble(as.table(B), .name_repair = "minimal") %>% setNames(c("ocupacion", "categoria", "v")) %>%
    filter(v == 1) %>%
    mutate(comunidad = factor(NOMBRES_COM[memb[as.character(categoria)]], levels = NOMBRES_COM),
           ocupacion = factor(ocupacion, levels = rev(orden_f)),
           categoria = factor(categoria, levels = orden_c))
  cortes_c <- cumsum(table(factor(memb[orden_c], levels = 1:4)))[1:3] + 0.5
  cortes_f <- nrow(B) - cumsum(table(factor(dom, levels = 1:4)))[1:3] + 0.5
  # Rotulos del eje vertical: solo las filas de las 27 posiciones del generador
  # (las demas ocupaciones del universo RM quedan sin nombre). Si dos posiciones
  # comparten codigo de habilidades, se unen sus nombres.
  etq_gp <- cw %>% group_by(isco_hab) %>% summarise(lab = paste(label, collapse = " / "), .groups = "drop")
  etq_gp <- setNames(str_trunc(etq_gp$lab, 45), etq_gp$isco_hab)
  # Los rotulos van fuera del panel, a la izquierda, separados con ggrepel solo en
  # vertical; un segmento une cada rotulo con su fila (evita que se monten).
  lab_gp <- tibble(isco = names(etq_gp), lab = unname(etq_gp),
                   y = match(names(etq_gp), levels(mat$ocupacion)))
  p_mat <- ggplot(mat, aes(categoria, ocupacion, fill = comunidad)) +
    geom_tile() +
    geom_vline(xintercept = cortes_c, colour = "grey40", linewidth = 0.3) +
    geom_hline(yintercept = cortes_f, colour = "grey40", linewidth = 0.3) +
    scale_fill_manual(values = PALETA_COM, drop = FALSE) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    ggrepel::geom_text_repel(data = lab_gp, aes(x = 0.5, y = y, label = lab), inherit.aes = FALSE,
                             hjust = 1, direction = "y", nudge_x = -6, xlim = c(-Inf, -1),
                             size = 2.3, colour = "grey15", segment.colour = "grey55",
                             segment.size = 0.2, min.segment.length = 0, box.padding = 0.12,
                             force = 0.6, max.overlaps = Inf, seed = SEMILLA_LEIDEN) +
    coord_cartesian(clip = "off") +
    labs(x = sprintf("%d categorías de habilidades, ordenadas por comunidad", ncol(B)),
         y = NULL,
         title = if (TITULOS_EN_FIGURA) "Matriz ocupación × categoría de habilidades",
         subtitle = if (TITULOS_EN_FIGURA) sprintf("Celda coloreada: categoría efectiva en la ocupación (RCA > 1) | %s vínculos",
                            formatC(sum(B), big.mark = ".", decimal.mark = ",", format = "d"))) +
    theme_tesis() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          plot.margin = margin(6, 8, 6, 150),
          panel.border = element_rect(fill = NA, colour = "grey40", linewidth = 0.3),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.subtitle = element_text(size = 9))
  guardar(p_mat, "art_fig_bimodal_matriz", 9.5, 10)
  cat("  [OK] art_fig_bimodal_matriz\n")
}, error = function(e) cat("  [ERROR] art_fig_bimodal_matriz:", conditionMessage(e), "\n"))

# ---- Paneles sobre el mapa: una posicion por panel ---------------------------------
# Las categorias quedan donde estan en la Figura 1 (coords_mapa). Las 391 ocupaciones
# del universo aparecen como contexto gris en el centroide de sus categorias; en cada
# panel se dibujan solo los vinculos de la posicion destacada, y sus categorias
# efectivas van en color pleno (las demas, atenuadas).
tryCatch({
  XY_CAT <- coords_mapa(eh$grafo)
  centro <- (B %*% XY_CAT[colnames(B), ]) / rowSums(B)
  nodos_cat <- tibble(name = rownames(XY_CAT), x = XY_CAT[, 1], y = XY_CAT[, 2],
                      comunidad = factor(NOMBRES_COM[memb[rownames(XY_CAT)]], levels = NOMBRES_COM))
  contexto <- tibble(x = centro[, 1], y = centro[, 2])
  panel_pos <- function(pos) {
    k <- cw$isco_hab[cw$posicion == pos]
    cats <- colnames(B)[B[k, ] == 1]
    o <- centro[k, ]
    nc <- nodos_cat %>% mutate(efectiva = name %in% cats)
    ggplot() +
      geom_point(data = contexto, aes(x, y), shape = 15, size = 0.5, colour = "grey85") +
      geom_segment(data = nc %>% filter(efectiva), aes(x = o[1], y = o[2], xend = x, yend = y, colour = comunidad),
                   alpha = 0.75, linewidth = 0.45, show.legend = FALSE) +
      geom_point(data = nc %>% filter(!efectiva), aes(x, y, fill = comunidad), shape = 21, size = 1.3,
                 colour = NA, alpha = 0.35, show.legend = FALSE) +
      geom_point(data = nc %>% filter(efectiva), aes(x, y, fill = comunidad), shape = 21, size = 2.3,
                 colour = BORDE_PUNTOS, stroke = 0.3) +
      annotate("point", x = o[1], y = o[2], shape = 22, size = 3.4, fill = COL_OCU, colour = "white", stroke = 0.5) +
      scale_fill_manual(values = PALETA_COM, drop = FALSE) +
      scale_colour_manual(values = PALETA_COM, drop = FALSE) +
      guides(fill = guide_legend(nrow = 1, override.aes = list(size = 3.5, alpha = 1))) +
      labs(title = str_wrap(sprintf("%s (%d categorías)", cw$label[cw$posicion == pos], length(cats)), 34)) +
      theme_void(base_size = 10) +
      theme(plot.title = element_text(size = 9.5, face = "bold", hjust = 0.5),
            legend.position = "bottom", legend.title = element_blank(),
            plot.margin = margin(4, 4, 4, 4))
  }
  paneles <- lapply(POS_PANELES, panel_pos)
  panel <- patchwork::wrap_plots(paneles, ncol = 2) + patchwork::plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  guardar(panel, "art_fig_bimodal_ejemplos", 7.5, 3.2 * ceiling(length(paneles) / 2) + 0.6)
  cat("  [OK] art_fig_bimodal_ejemplos\n")
}, error = function(e) cat("  [ERROR] art_fig_bimodal_ejemplos:", conditionMessage(e), "\n"))

# ---- Las 27 posiciones en dos filas ----------------------------------------------------
# Categorias abajo, en bloques por comunidad; ocupaciones arriba. Barridos de
# baricentro (Sugiyama) para reducir cruces.
tryCatch({
  M <- redes$gp$M
  com <- match(memb[colnames(M)], ORDEN_COM)
  xc <- rank(com * 1e6 + seq_along(com), ties.method = "first")
  for (i in 1:8) {
    xo <- rank(as.vector(M %*% xc) / rowSums(M), ties.method = "first")
    xc <- rank(com * 1e6 + as.vector(t(M) %*% xo) / colSums(M), ties.method = "first")
  }
  xc <- xc + (com - 1) * 2
  xo <- seq(min(xc), max(xc), length.out = length(xo))[xo]
  nom <- nombres_L2()
  n_o <- tibble(name = rownames(M), x = xo, y = 1, etq = str_trunc(cw$label[match(rownames(M), cw$posicion)], 40))
  n_c <- tibble(name = colnames(M), x = xc, y = 0, etq = str_trunc(unname(nom[colnames(M)]), 42),
                comunidad = factor(NOMBRES_COM[memb[colnames(M)]], levels = NOMBRES_COM))
  ar <- redes$gp$el %>% left_join(n_o %>% select(from = name, x, y), by = "from") %>%
    left_join(n_c %>% select(to = name, xend = x, yend = y, comunidad), by = "to")
  p27 <- ggplot() +
    geom_segment(data = ar, aes(x, y, xend = xend, yend = yend, colour = comunidad), alpha = 0.3,
                 linewidth = 0.3, show.legend = FALSE) +
    geom_point(data = n_c, aes(x, y, fill = comunidad), shape = 21, size = 1.9, colour = BORDE_PUNTOS, stroke = 0.25) +
    geom_point(data = n_o, aes(x, y), shape = 22, size = 2.8, fill = "grey88", colour = "grey25", stroke = 0.5) +
    geom_text(data = n_o, aes(x, y + 0.04, label = etq), angle = 40, hjust = 0, vjust = 0, size = 2.4, colour = "grey10") +
    geom_text(data = n_c, aes(x, y - 0.05, label = etq), angle = 90, hjust = 1, vjust = 0.5, size = 2, colour = "grey25") +
    scale_fill_manual(values = PALETA_COM, drop = FALSE) +
    scale_colour_manual(values = PALETA_COM, drop = FALSE) +
    scale_y_continuous(expand = expansion(add = c(1.2, 0.75))) +
    scale_x_continuous(expand = expansion(add = c(1, 12))) +
    coord_cartesian(clip = "off") +
    guides(fill = guide_legend(nrow = 1, override.aes = list(size = 3.5))) +
    theme_void(base_size = 11) +
    theme(legend.position = "bottom", legend.title = element_blank(), plot.margin = margin(6, 6, 6, 6))
  guardar(p27, "art_fig_bimodal_27_filas", 13, 8.5)
  cat("  [OK] art_fig_bimodal_27_filas\n")
}, error = function(e) cat("  [ERROR] art_fig_bimodal_27_filas:", conditionMessage(e), "\n"))

cat("Figuras guardadas en", normalizePath(DIR_FIG), "\n")

################################################################################
# DECISIONES
# 1. Vinculo = categoria efectiva (RCA > 1, matriz B de 01). Solo vinculos
#    ocupacion - categoria.
# 2. Nodos de habilidad = categorias L2 (112), coloreadas por comunidad (Leiden);
#    ocupaciones en un solo color. Tamano de nodos constante.
# 3. Disposicion Fruchterman-Reingold de cada red bimodal (igual que 12 y 13).
# 4. Las posiciones usan isco_habilidades (codigo agregado resuelto con CASEN;
#    Q0116 con el perfil de 5321), igual que la medida de similitud.
################################################################################
