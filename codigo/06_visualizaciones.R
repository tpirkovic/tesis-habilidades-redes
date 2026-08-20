# =============================================================================
# 06_visualizaciones.R
# Etapa 6 del pipeline: visualización de resultados de H1/H1b, H2, H3, H4, H4b.
# Tesis Magíster en Sociología — PUC Chile | Trajan Pirkovic Palma
#
# AUTOCONTENIDO: lee intermediate/modelos.rds (etapa 05). No re-estima nada.
# SALIDA: gráficos en el panel de Plots + tablas en Viewer. Export opcional.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse); library(broom); library(broom.mixed)
  library(survey); library(glmmTMB); library(patchwork)
  library(modelsummary)
})

INTERMEDIATE_DIR <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output/intermediate"
FIG_DIR          <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output/figuras"

# EXPORTAR: FALSE por defecto (fase exploratoria, salida a pantalla). Cambiar a
# TRUE solo cuando las figuras vayan al documento final.
EXPORTAR <- TRUE
if (EXPORTAR) dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

stopifnot("Falta modelos.rds. Correr 05_modelos.R primero." =
            file.exists(file.path(INTERMEDIATE_DIR, "modelos.rds")))
mod <- readRDS(file.path(INTERMEDIATE_DIR, "modelos.rds"))

# ── Estilo institucional ──────────────────────────────────────────────────────
UC_AZUL   <- "#173F8A"
UC_GRIS   <- "#6B7280"
UC_ROJO   <- "#B03A2E"
UC_VERDE  <- "#1E6B52"
PALETA_COM <- c("Dirección-servicio"          = UC_AZUL,
                "Técnico-manual"              = UC_ROJO,
                "Analítico-digital-simbólico" = UC_VERDE,
                "Bio-ambiental-legal"         = "#B8860B")

tema_tesis <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, colour = "grey90"),
    plot.title         = element_text(face = "bold", colour = UC_AZUL, size = 12),
    plot.subtitle      = element_text(colour = UC_GRIS, size = 9),
    plot.caption       = element_text(colour = UC_GRIS, size = 8, hjust = 0),
    axis.title         = element_text(size = 9),
    legend.position    = "bottom",
    legend.title       = element_blank(),
    strip.text         = element_text(face = "bold", colour = UC_AZUL, size = 9)
  )
theme_set(tema_tesis)

guardar <- function(p, nombre, w = 8, h = 5.5) {
  if (EXPORTAR) {
    ggsave(file.path(FIG_DIR, paste0(nombre, ".png")), p,
           width = w, height = h, dpi = 300, bg = "white")
    cat("  Exportado:", nombre, "\n")
  }
  print(p)
}

# Diccionario de etiquetas legibles para los coeficientes
ETIQ <- c(
  SP_ip_sc = "Similitud de prestigio (ego-posición)",
  SH_ip_sc = "Similitud de habilidades (ego-posición)",
  logTam_sc = "Tamaño del grupo ocupacional (log)",
  educ_sc = "Educación", edad_sc = "Edad", sexo_fMujer = "Mujer",
  `SH_ip_sc:educ_sc` = "Similitud habilidades × Educación",
  ISEI_orig_hat = "ISEI de origen", educ = "Educación", edad = "Edad",
  sexoMujer = "Mujer", Ext = "Extensión de la red",
  Rango_P = "Rango de prestigio", Estatus_Max = "Estatus máximo",
  SP_red_ego = "Homofilia de prestigio ego-red",
  ISEI_orig_c = "ISEI de origen (centrado)",
  ISEI_orig_c2 = "ISEI de origen² (centrado)",
  share_com4_ego = "Peso bio-ambiental-legal (ego)",
  `ISEI_orig_hat:share_com4_ego` = "ISEI origen × Peso bio-ambiental-legal"
)
etiquetar <- function(x) ifelse(x %in% names(ETIQ), ETIQ[x], x)

# =============================================================================
# FIGURA 1 — H1/H1b: coeficientes de los modelos multinivel
# =============================================================================

tidy_mm <- function(m, etiqueta) {
  broom.mixed::tidy(m, effects = "fixed", component = "cond", conf.int = TRUE) |>
    filter(term != "(Intercept)") |>
    mutate(modelo = etiqueta, term_lab = etiquetar(term))
}

coef_h1 <- bind_rows(
  tidy_mm(mod$M0, "M0: línea base"),
  tidy_mm(mod$M1, "M1: + habilidades (H1)"),
  tidy_mm(mod$M2, "M2: + interacción (H1b)")
) |>
  mutate(modelo = factor(modelo, levels = c("M0: línea base",
                                             "M1: + habilidades (H1)",
                                             "M2: + interacción (H1b)")),
         clave = term %in% c("SH_ip_sc", "SH_ip_sc:educ_sc"))

fig1 <- ggplot(coef_h1, aes(x = estimate, y = fct_rev(factor(term_lab)),
                             colour = clave)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = UC_GRIS, linewidth = 0.4) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  linewidth = 0.6, size = 0.4) +
  facet_wrap(~ modelo, ncol = 3) +
  scale_colour_manual(values = c(`TRUE` = UC_AZUL, `FALSE` = UC_GRIS), guide = "none") +
  labs(title = "H1 y H1b: homofilia de habilidades en la red de conocidos",
       subtitle = "Modelos multinivel binomiales negativos. En azul, los términos de interés.",
       x = "Coeficiente (log-conteo), IC 95%", y = NULL,
       caption = paste0("N = ", nrow(mod$bl_m), " observaciones ego × posición, ",
                        nlevels(droplevels(mod$bl_m$ID_f)), " egos."))
guardar(fig1, "fig1_h1_coeficientes", w = 10, h = 4.5)

# =============================================================================
# FIGURA 2 — H1b: efecto marginal de la similitud de habilidades por educación
# =============================================================================
# Visualiza la interacción: la pendiente de SH_ip según nivel educativo.

b_sh   <- fixef(mod$M2)$cond["SH_ip_sc"]
b_int  <- fixef(mod$M2)$cond["SH_ip_sc:educ_sc"]
vc     <- vcov(mod$M2)$cond
educ_grid <- seq(min(mod$bl_m$educ_sc, na.rm = TRUE),
                 max(mod$bl_m$educ_sc, na.rm = TRUE), length.out = 100)

pend <- b_sh + b_int * educ_grid
var_pend <- vc["SH_ip_sc", "SH_ip_sc"] +
  educ_grid^2 * vc["SH_ip_sc:educ_sc", "SH_ip_sc:educ_sc"] +
  2 * educ_grid * vc["SH_ip_sc", "SH_ip_sc:educ_sc"]

df_marg <- tibble(educ_sc = educ_grid, pendiente = pend,
                  lo = pend - 1.96 * sqrt(var_pend),
                  hi = pend + 1.96 * sqrt(var_pend))

fig2 <- ggplot(df_marg, aes(educ_sc, pendiente)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = UC_GRIS, linewidth = 0.4) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = UC_AZUL, alpha = 0.15) +
  geom_line(colour = UC_AZUL, linewidth = 0.9) +
  geom_rug(data = tibble(educ_sc = unique(mod$bl_m$educ_sc)),
           aes(x = educ_sc), inherit.aes = FALSE, colour = UC_GRIS, alpha = 0.5) +
  labs(title = "H1b: la homofilia de habilidades se atenúa a mayor educación",
       subtitle = "Efecto marginal de la similitud de habilidades sobre el n.º de conocidos, según educación",
       x = "Nivel educativo (estandarizado)",
       y = "Pendiente de la similitud de habilidades",
       caption = "Interacción significativa al 5% (p = 0,024). Resultado frágil, no robusto: interpretar con cautela.\nLas marcas inferiores indican los niveles educativos observados.")
guardar(fig2, "fig2_h1b_efecto_marginal", w = 7.5, h = 5)

# =============================================================================
# FIGURA 3 — H3: LA FIGURA CENTRAL
# =============================================================================
# Contrasta, para cada comunidad, el coeficiente del share de comunidad de la
# red (correspondencia de CONTENIDO) contra el de SP_red_ego (correspondencia
# de PRESTIGIO). El punto de la tesis: el primero es significativo, el segundo
# no lo es en ninguna comunidad.

extraer_h3 <- function(m, etiqueta, k) {
  broom::tidy(m, conf.int = TRUE) |>
    filter(term %in% c(paste0("share_com", k, "_red"), "SP_red_ego")) |>
    mutate(comunidad = etiqueta,
           tipo = if_else(term == "SP_red_ego",
                          "Prestigio de la red\n(SP ego-red)",
                          "Composición de habilidades\n(share de comunidad)"))
}

coef_h3 <- imap(mod$H3_modelos, ~ extraer_h3(.x, .y, which(names(mod$H3_modelos) == .y))) |>
  bind_rows() |>
  mutate(comunidad = factor(comunidad, levels = names(mod$H3_modelos)),
         sig = p.value < 0.05)

fig3 <- ggplot(coef_h3, aes(x = estimate, y = fct_rev(comunidad), colour = tipo,
                             shape = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = UC_GRIS, linewidth = 0.4) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = 0.55),
                  linewidth = 0.7, size = 0.5) +
  scale_colour_manual(values = c(UC_AZUL, UC_GRIS)) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
  labs(title = "H3: la correspondencia de habilidades no se reduce al prestigio",
       subtitle = "Coeficientes en modelos paralelos por comunidad. Puntos huecos: no significativos al 5%.",
       x = "Coeficiente, IC 95%", y = NULL,
       caption = paste0("N = ", nrow(mod$df_H3), ". Modelos svyglm ponderados. Cada modelo controla además por\n",
                        "extensión, rango de prestigio, estatus máximo, educación, ISEI de origen, sexo y edad."))
guardar(fig3, "fig3_h3_central", w = 8.5, h = 5)

# =============================================================================
# FIGURA 4 — H3: relación bivariada por comunidad (respaldo visual)
# =============================================================================

df_scatter <- mod$df_H3 |>
  select(weight, starts_with("share_com")) |>
  pivot_longer(-weight, names_to = "var", values_to = "valor") |>
  separate(var, into = c("pre", "com", "lado"), sep = "_") |>
  select(-pre) |>
  pivot_wider(names_from = lado, values_from = valor, values_fn = list) |>
  unnest(c(ego, red)) |>
  mutate(comunidad = factor(recode(com,
                                    com1 = "Dirección-servicio",
                                    com2 = "Técnico-manual",
                                    com3 = "Analítico-digital-simbólico",
                                    com4 = "Bio-ambiental-legal"),
                            levels = names(PALETA_COM)))

fig4 <- ggplot(df_scatter, aes(red, ego, colour = comunidad)) +
  geom_point(aes(size = weight), alpha = 0.18, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9, formula = y ~ x) +
  facet_wrap(~ comunidad, scales = "free", ncol = 2) +
  scale_colour_manual(values = PALETA_COM, guide = "none") +
  scale_size_continuous(range = c(0.3, 2)) +
  labs(title = "H3: composición de la red y composición de la ocupación de ego",
       subtitle = "Cada punto es un encuestado; el tamaño refleja su ponderador muestral",
       x = "Peso de la comunidad en la red de ego",
       y = "Peso de la comunidad en la ocupación de ego",
       caption = "Escalas libres por panel: los rangos de variación difieren fuertemente entre comunidades,\nporque la composición del generador de posiciones acota el peso alcanzable en cada una.")
guardar(fig4, "fig4_h3_dispersion", w = 8.5, h = 7)

# =============================================================================
# FIGURA 5 — H2 y H4: las dos hipótesis que no se sostienen
# =============================================================================

coef_h2 <- broom::tidy(mod$H2_div, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(modelo = "H2: diversidad de la red (Margalef)")
coef_h4 <- broom::tidy(mod$H4_lineal, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(modelo = "H4: cierre estructural")

coef_nulo <- bind_rows(coef_h2, coef_h4) |>
  mutate(term_lab = etiquetar(term),
         clave = term %in% c("ISEI_orig_hat"),
         sig = p.value < 0.05)

fig5 <- ggplot(coef_nulo, aes(x = estimate, y = fct_rev(factor(term_lab)),
                               colour = clave, shape = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = UC_GRIS, linewidth = 0.4) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), linewidth = 0.6, size = 0.4) +
  facet_wrap(~ modelo, scales = "free_x") +
  scale_colour_manual(values = c(`TRUE` = UC_ROJO, `FALSE` = UC_GRIS), guide = "none") +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
  labs(title = "H2 y H4: la clase de origen no predice la composición de la red",
       subtitle = "En rojo, el término hipotetizado (ISEI de origen). Puntos huecos: no significativos al 5%.",
       x = "Coeficiente, IC 95%", y = NULL,
       caption = paste0("H2: N = ", nrow(mod$df_ego), ". H4: N = ", nrow(mod$df_H4),
                        ". En ambos casos el ISEI de origen es no significativo;\n",
                        "lo que sí predice ambas variables es la educación de ego."))
guardar(fig5, "fig5_h2_h4_nulos", w = 9.5, h = 5)

# =============================================================================
# FIGURA 6 — H4: ausencia de forma de U
# =============================================================================
# Valores predichos de cierre estructural a lo largo del ISEI de origen, según
# la especificación cuadrática, para mostrar visualmente que no hay curvatura.

rango_isei <- range(mod$df_H4$ISEI_orig_hat, na.rm = TRUE)
media_isei <- mean(mod$df_H4$ISEI_orig_hat, na.rm = TRUE)

nd <- tibble(
  ISEI_orig_hat = seq(rango_isei[1], rango_isei[2], length.out = 100),
  educ = median(mod$df_H4$educ, na.rm = TRUE),
  sexo = factor("Hombre", levels = levels(mod$df_H4$sexo)),
  edad = mean(mod$df_H4$edad, na.rm = TRUE),
  Ext = mean(mod$df_H4$Ext, na.rm = TRUE),
  Rango_P = mean(mod$df_H4$Rango_P, na.rm = TRUE),
  Estatus_Max = mean(mod$df_H4$Estatus_Max, na.rm = TRUE)
) |>
  mutate(ISEI_orig_c = ISEI_orig_hat - media_isei, ISEI_orig_c2 = ISEI_orig_c^2)

pred <- predict(mod$H4_cuadratico, newdata = nd, se.fit = TRUE)
nd$fit <- as.numeric(pred); nd$se <- sqrt(attr(pred, "var"))

fig6 <- ggplot(nd, aes(ISEI_orig_hat, fit)) +
  geom_ribbon(aes(ymin = fit - 1.96 * se, ymax = fit + 1.96 * se),
              fill = UC_AZUL, alpha = 0.15) +
  geom_line(colour = UC_AZUL, linewidth = 0.9) +
  geom_rug(data = mod$df_H4, aes(x = ISEI_orig_hat), inherit.aes = FALSE,
           colour = UC_GRIS, alpha = 0.25) +
  labs(title = "H4: el cierre estructural no sigue una forma de U por clase de origen",
       subtitle = "Valores predichos con especificación cuadrática, resto de covariables en sus valores típicos",
       x = "ISEI de origen", y = "Cierre estructural predicho",
       caption = paste0("Término cuadrático no significativo (p = 0,89). A diferencia de la forma de U documentada\n",
                        "por Otero, Völker y Rözer (2021) para la segregación de redes, aquí la relación es plana."))
guardar(fig6, "fig6_h4_sin_u", w = 7.5, h = 5)

# =============================================================================
# FIGURA 7 — H4b: el efecto principal, no la interacción
# =============================================================================
# La interacción no es significativa, pero el efecto principal de
# share_com4_ego sí y es fuerte. La figura muestra ambas cosas y advierte
# sobre la probable naturaleza artefactual del efecto principal.

df_h4b <- mod$df_H4 |> filter(!is.na(share_com4_ego), !is.na(cierre_blando))

fig7 <- ggplot(df_h4b, aes(share_com4_ego, cierre_blando)) +
  geom_point(aes(size = weight), alpha = 0.2, colour = "#B8860B", show.legend = FALSE) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "#B8860B",
              fill = "#B8860B", alpha = 0.15, linewidth = 0.9) +
  scale_size_continuous(range = c(0.4, 2.5)) +
  labs(title = "H4b: peso bio-ambiental-legal y cierre estructural",
       subtitle = "Efecto principal fuerte (b = −0,61, p < 0,001); la interacción con clase de origen no es significativa (p = 0,20)",
       x = "Peso de la comunidad bio-ambiental-legal en la ocupación de ego",
       y = "Cierre estructural (versión blanda)",
       caption = paste0("ADVERTENCIA: el efecto principal es probablemente artefactual. El generador de posiciones contiene\n",
                        "muy pocas posiciones de esta comunidad, por lo que quien trabaja en ella necesariamente tiene poco\n",
                        "solapamiento con su red. No interpretar sustantivamente sin resolver esa limitación del instrumento."))
guardar(fig7, "fig7_h4b_advertencia", w = 8, h = 5.5)

# =============================================================================
# FIGURA 8 — Matriz de correlaciones entre indicadores de red
# =============================================================================

cor_long <- as.data.frame(as.table(mod$mat_cor)) |>
  rename(v1 = Var1, v2 = Var2, r = Freq) |>
  mutate(v1_lab = etiquetar(as.character(v1)), v2_lab = etiquetar(as.character(v2)))

orden <- unique(cor_long$v1_lab)

fig8 <- ggplot(cor_long, aes(factor(v1_lab, orden), factor(v2_lab, rev(orden)), fill = r)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", r)), size = 2.8,
            colour = if_else(abs(cor_long$r) > 0.6, "white", "grey20")) +
  scale_fill_gradient2(low = UC_ROJO, mid = "white", high = UC_AZUL,
                       midpoint = 0, limits = c(-1, 1), name = "r") +
  labs(title = "Correlaciones entre indicadores de red",
       subtitle = "Chequeo previo a la interpretación conjunta de coeficientes",
       x = NULL, y = NULL,
       caption = paste0("Cierre estructural y diversidad de la red: r = ",
                        sprintf("%.2f", mod$mat_cor["cierre_blando", "Div_Red"]),
                        ", por debajo del umbral 0,7. Pueden coexistir en un mismo modelo.\n",
                        "Rango de prestigio y estatus máximo: r = ",
                        sprintf("%.2f", mod$mat_cor["Rango_P", "Estatus_Max"]),
                        ", muy alta: no interpretar sus coeficientes por separado.")) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        panel.grid = element_blank())
guardar(fig8, "fig8_correlaciones", w = 8, h = 7)

# =============================================================================
# TABLAS DE RESULTADOS (Viewer)
# =============================================================================

cm_h1 <- c("SP_ip_sc" = "Similitud de prestigio", "SH_ip_sc" = "Similitud de habilidades",
           "SH_ip_sc:educ_sc" = "Similitud habilidades × Educación",
           "logTam_sc" = "Tamaño grupo ocupacional (log)", "educ_sc" = "Educación",
           "edad_sc" = "Edad", "sexo_fMujer" = "Mujer")

modelsummary(list("M0" = mod$M0, "M1 (H1)" = mod$M1, "M2 (H1b)" = mod$M2),
             output = "gt", coef_map = cm_h1, stars = TRUE,
             gof_map = c("nobs", "aic", "bic"),
             title = "H1 y H1b: modelos multinivel binomiales negativos")

cm_h3 <- c("share_com1_red" = "Comunidad en la red", "share_com2_red" = "Comunidad en la red",
           "share_com3_red" = "Comunidad en la red", "share_com4_red" = "Comunidad en la red",
           "SP_red_ego" = "Homofilia de prestigio ego-red",
           "Ext" = "Extensión", "Rango_P" = "Rango de prestigio",
           "Estatus_Max" = "Estatus máximo", "educ" = "Educación",
           "ISEI_orig_hat" = "ISEI de origen", "sexoMujer" = "Mujer", "edad" = "Edad")

modelsummary(mod$H3_modelos, output = "gt", coef_map = cm_h3, stars = TRUE,
             gof_map = c("nobs", "r.squared"),
             title = "H3: correspondencia de comunidades entre red y ocupación de ego")

cm_ego <- c("ISEI_orig_hat" = "ISEI de origen", "ISEI_orig_c" = "ISEI de origen (centrado)",
            "ISEI_orig_c2" = "ISEI de origen² ", "share_com4_ego" = "Peso bio-ambiental-legal",
            "ISEI_orig_hat:share_com4_ego" = "ISEI origen × Peso bio-amb-legal",
            "educ" = "Educación", "sexoMujer" = "Mujer", "edad" = "Edad",
            "Ext" = "Extensión", "Rango_P" = "Rango de prestigio",
            "Estatus_Max" = "Estatus máximo")

modelsummary(list("H2: diversidad red" = mod$H2_div,
                  "H4: cierre (lineal)" = mod$H4_lineal,
                  "H4: cierre (cuadrático)" = mod$H4_cuadratico,
                  "H4b: interacción" = mod$H4b),
             output = "gt", coef_map = cm_ego, stars = TRUE,
             gof_map = c("nobs", "r.squared"),
             title = "H2, H4 y H4b: modelos a nivel ego")

cat("\n=== FIN ETAPA 06: 8 figuras + 3 tablas ===\n")
if (!EXPORTAR) cat("EXPORTAR = TRUE. Cambiar a TRUE para guardar PNG en output/figuras/\n")

# =============================================================================
# DECISIONES DE VISUALIZACIÓN
# =============================================================================
# V1. EXPORTAR = FALSE por defecto: la fase actual es exploratoria y la salida
#     va al panel de Plots. Cambiar a TRUE solo para las figuras que entren al
#     documento final.
# V2. La Figura 3 es la figura central de la tesis: contrasta, comunidad por
#     comunidad, el coeficiente de composición de habilidades contra el de
#     homofilia de prestigio. La forma (punto lleno/hueco) codifica
#     significancia, de modo que el patrón (contenido significativo, prestigio
#     no) se lee sin necesidad de consultar la tabla.
# V3. La Figura 4 usa escalas libres por panel deliberadamente. Con escala
#     común, las comunidades 3 y 4 quedarían comprimidas contra el eje y su
#     relación sería ilegible, porque el generador de posiciones acota el peso
#     alcanzable de cada comunidad de forma muy distinta. La nota al pie lo
#     declara para evitar una lectura errónea de las pendientes entre paneles.
# V4. Las hipótesis que NO se sostienen (H2, H4) reciben figura propia y no se
#     omiten. Un resultado nulo bien estimado es un resultado, y la Figura 5
#     muestra además qué sí predice cada variable dependiente (educación en
#     ambos casos), que es información sustantiva.
# V5. La Figura 7 lleva la advertencia sobre el carácter probablemente
#     artefactual del efecto principal de share_com4_ego incorporada en la
#     propia figura, no solo en el texto, porque una figura circula sola y
#     puede leerse fuera de su contexto.
# V6. La Figura 8 señala explícitamente la correlación de 0,95 entre rango de
#     prestigio y estatus máximo. Ambos están en todos los modelos a nivel ego
#     como controles; sus coeficientes individuales no son interpretables por
#     separado, aunque su inclusión conjunta como control sea correcta.
# =============================================================================
