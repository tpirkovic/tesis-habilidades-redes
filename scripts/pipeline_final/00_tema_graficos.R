# =============================================================================
# 00_tema_graficos.R
# Estilo grafico comun de la tesis (version integrada al pipeline final de
# codigo_v2/000_tema_tesis.R). NO es una etapa del pipeline: no lee ni escribe
# datos. Lo carga 00_config.R, de modo que todo script que grafica hereda el
# tema con theme_set() y guarda con guardar_fig().
#
# Version 12-10-2026 (noche): paleta "Navy-carmesi" (Coolors) y estructura
# inspirada en BeautifulFigures (Churkin, MIT):
#   - marco completo, grilla mayor y menor muy tenue, leyenda compacta arriba
#     sin marco, tipografia sans, fondo blanco
#   - azul marino = foco del analisis, carmesi = contraste, celeste = apoyo y
#     fondo; comunidades de habilidades con PALETA_COM
#   - exportacion PNG a 300 dpi + PDF vectorial
# Respaldo de la version anterior (Kit Digital UC): _respaldo_2026-10-12b/
# La estructura base del tema viene de la skill "scientific-figure-making"
# (figures4papers, Chen Liu, CC BY-NC 4.0) y de BeautifulFigures (A. Churkin).
# Tesis Magister en Sociologia, PUC Chile | Trajan Pirkovic Palma
# =============================================================================

suppressPackageStartupMessages(library(ggplot2))

# -- Paleta: "Navy-carmesi" (coolors.co/palettes/trending) ----------------------
# #003049 azul marino | #c1121f carmesi | #669bbc celeste | #ee9b00 naranja |
# #fdf0d5 crema. Se conservan los nombres de PALETA_TESIS que usan los scripts.
PALETA_TESIS <- c(
  azul        = "#003049",  # foco del analisis
  azul_2      = "#669bbc",  # celeste: apoyo (intervalos, extremo bajo de las matrices)
  azul_oscuro = "#003049",  # ocupaciones en las redes bimodales
  rojo        = "#c1121f",  # contraste
  naranja     = "#ee9b00",  # Golden Orange (Coolors): comunidad Bio-ambiental-legal
  crema       = "#fdf0d5",  # punto medio de las matrices divergentes
  gris        = "#669bbc",  # fondo / lineas base (celeste, como en la paleta elegida)
  gris_oscuro = "#707070"   # textos secundarios
)

# Orden por defecto para variables categoricas
COLORES_TESIS <- c("#003049", "#c1121f", "#669bbc", "#ee9b00", "#707070")

UC_AZUL  <- PALETA_TESIS[["azul"]]
UC_GRIS  <- PALETA_TESIS[["azul_2"]]   # apoyo (barras de error); los textos usan gris_oscuro
UC_ROJO  <- PALETA_TESIS[["rojo"]]

# Comunidades de habilidades (Leiden): mismo color en todas las figuras.
# Celeste y azul marino se distinguen por luminosidad; el naranja (comunidad mas
# pequena) separa por tono de carmesi. El naranja tiene poco contraste sobre blanco
# (aprox. 2,2:1): en puntos usar siempre BORDE_PUNTOS; en barras se ve, pero tenue.
PALETA_COM <- c("Gestión y cuidado"   = "#669bbc",
                "Técnico-material"    = "#c1121f",
                "Analítico-simbólico" = "#003049",
                "Bio-ambiental-legal" = "#ee9b00")
BORDE_PUNTOS <- "#003049"

# -- Tema -----------------------------------------------------------------------
theme_tesis <- function(base_size = 11, base_family = "sans") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      panel.border     = element_rect(colour = "black", linewidth = 0.6, fill = NA),
      panel.grid.major = element_line(colour = scales::alpha("black", 0.16), linewidth = 0.3),
      panel.grid.minor = element_line(colour = scales::alpha("black", 0.07), linewidth = 0.15),
      axis.ticks       = element_line(colour = "black", linewidth = 0.4),
      axis.text        = element_text(colour = "black"),
      plot.title       = element_text(face = "bold", size = rel(1.1)),
      plot.subtitle    = element_text(colour = PALETA_TESIS[["gris_oscuro"]]),
      plot.caption     = element_text(colour = PALETA_TESIS[["gris_oscuro"]],
                                      size = rel(0.75), hjust = 0),
      plot.title.position = "plot",
      legend.position  = "top",
      legend.title     = element_blank(),
      legend.background = element_blank(),
      legend.key       = element_blank(),
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold"),
      plot.background  = element_rect(fill = "white", colour = NA)
    )
}

scale_colour_tesis <- function(...) scale_colour_manual(values = COLORES_TESIS, ...)
scale_fill_tesis   <- function(...) scale_fill_manual(values = COLORES_TESIS, ...)

# -- Exportacion: PNG 300 dpi + PDF vectorial (design-theory.md S2) -------------
# `ruta` va sin extension: guardar_fig(p, file.path(DIR_FIG, "fig_1")).
# PDF con pdf() base: cairo_pdf falla en este Mac (requiere XQuartz).
guardar_fig <- function(p, ruta, w = 6.5, h = 4.5, dpi = 300) {
  dir.create(dirname(ruta), showWarnings = FALSE, recursive = TRUE)
  ggsave(paste0(ruta, ".png"), p, width = w, height = h, dpi = dpi, bg = "white")
  ggsave(paste0(ruta, ".pdf"), p, width = w, height = h, bg = "white", device = pdf)
  invisible(ruta)
}

theme_set(theme_tesis())
