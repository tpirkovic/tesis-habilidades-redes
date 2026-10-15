################################################################################
# 07_figuras_articulo.R   FIGURAS PARA EL ARTÍCULO (informe de avance)
#
# Rehace las seis figuras del artículo a partir de las salidas YA GUARDADAS del
# pipeline (output_final/tablas/*.csv e intermedios/espacio_habilidades.rds).
# No reestima nada ni modifica las salidas de los scripts 01 a 06.
#
# Colores: paleta Navy-carmesi (00_tema_graficos.R). En la red de complementariedad
# los puntos llevan borde fino para separarlos de las aristas.
# Ademas genera art_fig_mapa_posiciones (las 27 posiciones sobre el mapa).
#
# Cambios respecto de las figuras originales:
#   - etiquetas en español con tildes y coma decimal en los ejes;
#   - sin pies ni subtítulos dentro del gráfico (van como Nota en el artículo);
#   - leyendas completas y margen derecho para que no se corten.
# Guarda con prefijo art_ (art_fig_*.pdf y .png). El artículo usa la versión
# art_ si existe y, si no, la original (ver articulo/R/cifras.R, figura()).
#
# Uso: lo corre run_pipeline.R despues de 06, o por separado desde la carpeta
# pipeline_final:  Rscript 07_figuras_articulo.R
# Tarda unos segundos. Si una figura falla, las demás se generan igual.
################################################################################

source("00_config.R", encoding = "UTF-8")
suppressPackageStartupMessages({ library(ggraph); library(tidygraph) })
for (p in c("ggforce", "ggrepel")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")

coma  <- scales::label_number(decimal.mark = ",", big.mark = ".")
coma1 <- scales::label_number(accuracy = 0.1, decimal.mark = ",")
coma2 <- scales::label_number(accuracy = 0.01, decimal.mark = ",")
porc  <- scales::label_percent(accuracy = 1, decimal.mark = ",")

# Nombres de las clases en español para las figuras del artículo (solo en este
# script; 00_config.R conserva los nombres originales, que usan los demás).
CLASES7_CORTO <- c("Servicio superior", "Servicio inferior*", "Socioculturales",
                   "Administrativos", "Pequeños empresarios*", "Calificados", "No calificados")
corto <- setNames(CLASES7_CORTO, CLASES7)
leer_tab <- function(a) read_csv(file.path(DIR_TAB, a), show_col_types = FALSE)
sin_pie <- theme(plot.caption = element_blank(), plot.subtitle = element_blank(),
                 plot.margin = margin(6, 16, 6, 6))
hacer <- function(nombre, expr) {
  r <- tryCatch({ force(expr); cat("  [OK]", nombre, "\n") },
                error = function(e) cat("  [ERROR]", nombre, ":", conditionMessage(e), "\n"))
  invisible(r)
}

titulo("Figuras del artículo (prefijo art_)")

# ---- Figura: red de complementariedad -------------------------------------------
# Version 12-10-2026:
#   - tamano de nodos constante (el grado no se interpreta en el articulo);
#   - se dibuja el arbol de expansion maxima (cada categoria unida a su vecina mas
#     proxima: ningun nodo queda flotando) mas el 10% de aristas con mayor phi;
#   - aristas dentro de una comunidad en su color; entre comunidades, en gris;
#   - contorno y nombre de cada comunidad; etiquetas en las categorias mas
#     centrales de cada comunidad y en las principales categorias puente.
# La disposicion usa todas las aristas (coords_mapa, igual que 01 y 08).
MAPA <- list(
  hulls        = FALSE,  # contornos: ggforce descarta los nombres que no caben y deja solo uno
  n_centrales  = 4,      # categorias etiquetadas por comunidad (mayor fuerza dentro de ella)
  n_puentes    = 3,      # categorias puente etiquetadas (mayor proporcion de fuerza hacia otras comunidades)
  tam_nodo     = c(1.2, 5.5),  # rango de tamano segun el numero de relaciones (grado)
  tam_etq      = 2.4,
  largo_etq    = 34)

datos_mapa <- function(eh) {
  g <- eh$grafo; memb <- eh$membership
  xy <- coords_mapa(g)
  el <- igraph::as_data_frame(g, what = "edges")
  arbol <- igraph::as_data_frame(igraph::mst(g, weights = 1 - igraph::E(g)$weight), what = "edges")
  clave <- function(a, b) paste(pmin(a, b), pmax(a, b))
  umbral <- quantile(el$weight, 0.90)
  el <- el %>%
    mutate(intra = memb[from] == memb[to],
           comunidad = factor(if_else(intra, NOMBRES_COM[memb[from]], NA_character_), levels = NOMBRES_COM),
           dibujar = weight >= umbral | clave(from, to) %in% clave(arbol$from, arbol$to),
           x = xy[from, 1], y = xy[from, 2], xend = xy[to, 1], yend = xy[to, 2])
  # Fuerza dentro y fuera de la comunidad (con todas las aristas phi > 0)
  fuerza <- bind_rows(el %>% transmute(nodo = from, w = weight, intra),
                      el %>% transmute(nodo = to,   w = weight, intra)) %>%
    group_by(nodo) %>% summarise(f_intra = sum(w[intra]), f_total = sum(w), grado = n(), .groups = "drop") %>%
    mutate(p_fuera = 1 - f_intra / f_total)
  nom <- nombres_L2()
  nodos <- tibble(name = rownames(xy), x = xy[, 1], y = xy[, 2]) %>%
    mutate(k = memb[name], comunidad = factor(NOMBRES_COM[k], levels = NOMBRES_COM)) %>%
    left_join(fuerza, by = c("name" = "nodo")) %>%
    group_by(k) %>% mutate(central = rank(-f_intra, ties.method = "first") <= MAPA$n_centrales) %>% ungroup() %>%
    mutate(puente = rank(-if_else(f_total >= median(f_total), p_fuera, -Inf), ties.method = "first") <= MAPA$n_puentes,
           etq = if_else(central | puente, str_trunc(unname(nom[name]), MAPA$largo_etq), NA_character_),
           etq_hull = sprintf("%s (%d)", comunidad, as.integer(table(k)[as.character(k)])))
  list(nodos = nodos, aristas = el %>% filter(dibujar), xy = xy)
}

graficar_mapa <- function(dm, extra = NULL) {
  p <- ggplot()
  if (MAPA$hulls)
    p <- p + ggforce::geom_mark_hull(data = dm$nodos, aes(x, y, group = comunidad, fill = comunidad, label = etq_hull),
                                     concavity = 3, expand = unit(2.5, "mm"), radius = unit(2.5, "mm"),
                                     alpha = 0.07, colour = NA, label.fontsize = 8.5, label.fontface = "bold",
                                     label.fill = NA, label.buffer = unit(3, "mm"), con.colour = "grey55",
                                     con.size = 0.3, show.legend = FALSE)
  p <- p +
    geom_segment(data = dm$aristas %>% filter(!intra), aes(x, y, xend = xend, yend = yend),
                 colour = "grey70", alpha = 0.55, linewidth = 0.3) +
    geom_segment(data = dm$aristas %>% filter(intra), aes(x, y, xend = xend, yend = yend, colour = comunidad),
                 alpha = 0.45, linewidth = 0.4, show.legend = FALSE) +
    geom_point(data = dm$nodos, aes(x, y, fill = comunidad, size = grado), shape = 21,
               colour = BORDE_PUNTOS, stroke = 0.3) +
    scale_size_continuous(range = MAPA$tam_nodo, guide = "none")
  if (!is.null(extra)) p <- p + extra
  n_com <- table(dm$nodos$comunidad)
  etq_com <- function(b) sprintf("%s (%d)", b, as.integer(n_com[b]))
  p + ggrepel::geom_text_repel(data = dm$nodos %>% filter(!is.na(etq)), aes(x, y, label = etq),
                               size = MAPA$tam_etq, colour = "grey20", min.segment.length = 0.1,
                               segment.colour = "grey60", segment.size = 0.2, box.padding = 0.35,
                               max.overlaps = Inf, seed = SEMILLA_LEIDEN) +
    scale_fill_manual(values = PALETA_COM, labels = etq_com, drop = FALSE) +
    scale_colour_manual(values = PALETA_COM, drop = FALSE) +
    guides(fill = guide_legend(nrow = 2, override.aes = list(size = 3.6))) +
    coord_cartesian(clip = "off") +
    theme_void(base_size = 11) +
    theme(legend.position = "bottom", legend.title = element_blank(),
          plot.margin = margin(14, 14, 6, 14))
}

hacer("art_fig_mapa_habilidades", {
  eh <- readRDS(file.path(DIR_INT, "espacio_habilidades.rds"))
  dm <- datos_mapa(eh)
  guardar_fig(graficar_mapa(dm), file.path(DIR_FIG, "art_fig_mapa_habilidades"), w = 7, h = 6.2)
})

# ---- Figura: las 27 posiciones sobre el mapa -------------------------------------
# Cada posicion del generador en el centroide de sus categorias efectivas (codigo
# de habilidades de 02). Muestra que la similitud de habilidades es una distancia
# en este espacio. Variante para comparar con la Figura 1.
hacer("art_fig_mapa_posiciones", {
  eh <- readRDS(file.path(DIR_INT, "espacio_habilidades.rds"))
  cw <- read_csv(file.path(DIR_INT, "crosswalk_gp27.csv"), col_types = cols(.default = col_character()))
  dm <- datos_mapa(eh)
  dm$nodos$etq <- NA_character_          # aqui se etiquetan las posiciones, no las categorias
  pos <- cw %>% transmute(label, k = as.character(as.integer(isco_habilidades))) %>%
    mutate(x = map_dbl(k, ~ mean(dm$xy[eh$cats[[.x]], 1])), y = map_dbl(k, ~ mean(dm$xy[eh$cats[[.x]], 2])))
  extra <- list(
    geom_point(data = pos, aes(x, y), shape = 22, size = 2.4, fill = PALETA_TESIS[["azul_oscuro"]],
               colour = "white", stroke = 0.4),
    ggrepel::geom_text_repel(data = pos, aes(x, y, label = label), size = 2.2, fontface = "bold",
                             colour = PALETA_TESIS[["azul_oscuro"]], min.segment.length = 0.1,
                             segment.colour = "grey50", segment.size = 0.2, box.padding = 0.3,
                             max.overlaps = Inf, seed = SEMILLA_LEIDEN))
  guardar_fig(graficar_mapa(dm, extra), file.path(DIR_FIG, "art_fig_mapa_posiciones"), w = 8, h = 7)
})

# ---- Figura: perfil de habilidades de las 27 posiciones --------------------------
hacer("art_fig_27_posiciones", {
  pos <- leer_tab("tabla_posiciones_gp.csv")
  largo <- pos %>%
    select(posicion, ocupacion, isei, starts_with("share_com")) %>%
    pivot_longer(starts_with("share_com"), names_to = "com", values_to = "share") %>%
    mutate(comunidad = factor(NOMBRES_COM[as.integer(str_extract(com, "\\d"))], levels = NOMBRES_COM),
           etiqueta = sprintf("%s (%d)", ocupacion, as.integer(isei)),
           etiqueta = fct_reorder(etiqueta, isei),
           share = share / 100)
  p <- ggplot(largo, aes(share, etiqueta, fill = comunidad)) +
    geom_col(width = 0.75) +
    scale_fill_manual(values = PALETA_COM) +
    guides(fill = guide_legend(nrow = 2)) +
    scale_x_continuous(labels = porc, breaks = seq(0, 1, 0.25),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(x = "Porción de las categorías de habilidades efectivas", y = NULL) +
    sin_pie
  guardar_fig(p, file.path(DIR_FIG, "art_fig_27_posiciones"), w = 7, h = 6.8)
})

# ---- Figura: quién conoce a quién -----------------------------------------------------
hacer("art_fig_quien_conoce_a_quien", {
  qq <- leer_tab("quien_conoce_a_quien.csv")
  p <- qq %>%
    mutate(ego = factor(corto[clase_ego], levels = rev(CLASES7_CORTO)),
           pos = factor(corto[class_pos], levels = CLASES7_CORTO)) %>%
    ggplot(aes(pos, ego, fill = log2(obs_esp))) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = fnum(obs_esp, 1)), size = 3) +
    scale_fill_gradient2(low = PALETA_TESIS[["azul_2"]], mid = PALETA_TESIS[["crema"]], high = UC_ROJO,
                         midpoint = 0, guide = "none") +
    labs(x = "Clase de la posición conocida", y = "Clase del trabajador") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          axis.line = element_blank(), axis.ticks = element_blank()) +
    sin_pie
  guardar_fig(p, file.path(DIR_FIG, "art_fig_quien_conoce_a_quien"), w = 6.5, h = 5)
})

# ---- Figura: tres medidas por clase ---------------------------------------------------
hacer("art_fig_tres_medidas_por_clase", {
  tres <- leer_tab("tres_medidas_por_clase.csv")
  etq <- c(hom = "Homogeneidad de clase", SH_z = "Similitud de habilidades (DE)",
           dist_isei = "Distancia de estatus (ISEI)")
  p <- tres %>%
    mutate(clase = factor(corto[clase], levels = rev(CLASES7_CORTO)),
           medida = factor(etq[medida], levels = etq)) %>%
    ggplot(aes(media, clase)) +
    geom_linerange(aes(xmin = li, xmax = ls), colour = UC_GRIS) +
    geom_point(colour = UC_AZUL, size = 2.2) +
    facet_wrap(~ medida, scales = "free_x") +
    scale_x_continuous(labels = coma, n.breaks = 4) +
    labs(x = NULL, y = NULL) +
    sin_pie
  guardar_fig(p, file.path(DIR_FIG, "art_fig_tres_medidas_por_clase"), w = 7.5, h = 3.8)
})

# ---- Figura: pendientes por clase (H2a) -----------------------------------------------
hacer("art_fig_pendientes_h2", {
  pend <- leer_tab("pendientes_h2.csv")
  manual <- CLASES7[6:7]
  p <- pend %>%
    mutate(clase_c = factor(corto[clase], levels = rev(CLASES7_CORTO)),
           tipo = if_else(clase %in% manual, "Clases manuales", "Clases de servicio y otras")) %>%
    ggplot(aes(pendiente, clase_c, colour = tipo)) +
    geom_vline(xintercept = 0, colour = PALETA_TESIS[["gris"]]) +
    geom_linerange(aes(xmin = li, xmax = ls)) +
    geom_point(size = 2.4) +
    scale_colour_manual(values = c("Clases manuales" = UC_ROJO,
                                   "Clases de servicio y otras" = UC_AZUL)) +
    scale_x_continuous(labels = coma2) +
    labs(x = "Pendiente de la similitud de habilidades (escala logit, por 1 DE)", y = NULL) +
    sin_pie
  guardar_fig(p, file.path(DIR_FIG, "art_fig_pendientes_h2"), w = 6.5, h = 3.8)
})

# ---- Figura: línea base -----------------------------------------------------------------
hacer("art_fig_linea_base_n3", {
  n3   <- leer_tab("linea_base_n3.csv")
  dist <- leer_tab("linea_base_n3_distribucion.csv")
  term <- names(dist)[1:3]
  p <- dist %>% select(all_of(term)) %>%
    pivot_longer(everything(), names_to = "termino", values_to = "valor") %>%
    mutate(termino = factor(termino, levels = term)) %>%
    ggplot(aes(valor)) +
    geom_histogram(bins = 30, fill = PALETA_TESIS[["gris"]], colour = "white") +
    geom_vline(data = n3 %>% filter(termino %in% term) %>%
                 mutate(termino = factor(termino, levels = term)),
               aes(xintercept = observado), colour = UC_AZUL, linewidth = 0.9) +
    facet_wrap(~ termino, scales = "free_y") +
    scale_x_continuous(labels = coma1) +
    labs(x = "Coeficiente (escala logit, por 1 DE)", y = "Redes al azar") +
    sin_pie
  guardar_fig(p, file.path(DIR_FIG, "art_fig_linea_base_n3"), w = 7.5, h = 3.2)
})

cat("\nListo. Volver a compilar el artículo: quarto render tesis.qmd\n")
