# =============================================================================
# 06_visualizaciones.R
# Etapa 6 del pipeline: visualización de resultados de H1/H1b, H2, H3, H4.
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
  educ_sc = "Educación (continua, H1/H1b)", edad_sc = "Edad", sexo_fMujer = "Mujer",
  `SH_ip_sc:educ_sc` = "Similitud habilidades × Educación",
  ISEI_orig_hat = "ISEI de origen", edad = "Edad",
  # educ_f5 (H2/H4, DECISIÓN 12 de 05_modelos.R): factor de 5 categorías
  # colapsadas (reemplaza a educ_f de 10 niveles, cuya referencia tenía
  # n=3). svyglm genera un término por nivel no-referencia, con el nombre
  # del nivel concatenado (p.ej. "educ_f52. Media"). `etiquetar_educ_f5()`
  # abajo los traduce dinámicamente.
  sexoMujer = "Mujer", Ext = "Extensión de la red",
  Rango_P = "Rango de prestigio", Estatus_Max = "Estatus máximo",
  SP_red_ego = "Homofilia de prestigio ego-red",
  ISEI_orig_c = "ISEI de origen (centrado)",
  ISEI_orig_c2 = "ISEI de origen² (centrado)"
)
# Q40 colapsada a 5 categorías (DECISIÓN 12 de 05_modelos.R, 20-ago-2026):
# la categoría de referencia de educ_f (10 niveles) tenía n=3 ("Sin
# estudios"), lo que inflaba artificialmente los errores estándar de todas
# las comparaciones. educ_f5 agrupa: Básica o menos (ref.), Media, Técnica
# superior, Universitaria incompleta, Universitaria completa o posgrado.
ROTULOS_EDUC_F5 <- c(
  "2. Media"                    = "Media",
  "3. Tecnica superior"         = "Técnica superior",
  "4. Universitaria incompleta" = "Universitaria incompleta",
  "5. Universitaria completa+"  = "Universitaria completa o posgrado"
)
etiquetar_educ_f5 <- function(x) {
  nivel <- sub("^educ_f5", "", x)
  ifelse(nivel %in% names(ROTULOS_EDUC_F5),
         paste0("Educación: ", ROTULOS_EDUC_F5[nivel]), x)
}
etiquetar <- function(x) {
  x <- ifelse(x %in% names(ETIQ), ETIQ[x], x)
  ifelse(grepl("^educ_f5", x), etiquetar_educ_f5(x), x)
}

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
  # ACTUALIZADO (20-ago-2026, ver DECISIÓN 11): el título/caption anteriores
  # decían "no predice", pero el escalonamiento (tabla_h2_escalon/
  # tabla_h4_escalon) muestra que ISEI_orig_hat SÍ predice ambas DV en su
  # forma bivariada (H2: b=0.0086***, H4: b=-0.00076***) y pierde
  # significancia justo al entrar educ_f5 -- consistente con un efecto
  # mediado por educación (Blau-Duncan: origen -> educación -> destino), no
  # con ausencia de efecto. Este gráfico muestra el EFECTO DIRECTO
  # (controlando por educación); ver Fig5b para el efecto total/mediado.
  labs(title = "H2 y H4: sin efecto DIRECTO de la clase de origen, controlando por educación",
       subtitle = "En rojo, el término hipotetizado (ISEI de origen). Puntos huecos: no significativos al 5%.",
       x = "Coeficiente, IC 95%", y = NULL,
       caption = paste0("H2: N = ", nrow(mod$df_ego), ". H4: N = ", nrow(mod$df_H4),
                        ". El efecto directo de ISEI de origen es no significativo controlando\n",
                        "por educación de ego -- pero el efecto TOTAL sí lo es (ver Fig5b): el origen\n",
                        "predice la red principalmente a través de cuánta educación logra ego."))
guardar(fig5, "fig5_h2_h4_directo", w = 9.5, h = 5)

# =============================================================================
# FIGURA 5b — H2 y H4: el coeficiente de ISEI de origen se diluye al entrar
# educación (evidencia de mediación)
# =============================================================================
# NUEVO (20-ago-2026, DECISIÓN 11). Visualiza tabla_h2_escalon/tabla_h4_escalon
# de 05_modelos.R: el coeficiente de ISEI_orig_hat en cada paso del
# escalonamiento de controles. La caída ocurre en el paso 2 (+ educación) y
# se mantiene plana después -- Ext/Rango_P/Estatus_Max casi no mueven nada
# adicional. Esto NO es un test formal de mediación (habría que usar
# `mediation` o Baron-Kenny con bootstrap); es evidencia descriptiva de
# dónde se concentra la caída del coeficiente.

tabla_escalon_fig <- bind_rows(
  mod$tabla_h2_escalon |> mutate(modelo = "H2: diversidad de la red (Div_Red)"),
  mod$tabla_h4_escalon |> mutate(modelo = "H4: cierre estructural (cierre_blando)")
) |>
  mutate(
    paso_n = as.integer(str_extract(paso, "^\\d+")),
    paso_lab = str_remove(paso, "^\\d+\\.\\s*"),
    sig = p < 0.05
  )

fig5b <- ggplot(tabla_escalon_fig, aes(paso_n, b)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = UC_GRIS, linewidth = 0.4) +
  geom_vline(xintercept = 1.5, linetype = "dotted", colour = UC_ROJO, linewidth = 0.5) +
  geom_ribbon(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se), fill = UC_AZUL, alpha = 0.15) +
  geom_line(colour = UC_AZUL, linewidth = 0.8) +
  geom_point(aes(shape = sig), colour = UC_AZUL, size = 2.5) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
  scale_x_continuous(breaks = 1:6, labels = function(x) x) +
  facet_wrap(~ modelo, scales = "free_y") +
  labs(title = "El efecto del origen de clase se diluye justo al entrar educación",
       subtitle = "Coeficiente de ISEI de origen por paso del escalonamiento (punto lleno = p<0.05)",
       x = "Paso (1=solo ISEI origen; 2=+educ; 3=+sexo/edad; 4=+Ext; 5=+Rango_P; 6=+Estatus_Max)",
       y = "Coeficiente de ISEI de origen, IC 95%",
       caption = "Línea roja punteada: punto donde entra educación (paso 2). Caída ~88% (H2) y ~64% (H4)\nen ese paso; pasos 3-6 casi no mueven el coeficiente adicionalmente.")
guardar(fig5b, "fig5b_h2_h4_escalonamiento", w = 9.5, h = 5)

# =============================================================================
# FIGURA 6 — H4: ausencia de forma de U
# =============================================================================
# Valores predichos de cierre estructural a lo largo del ISEI de origen, según
# la especificación cuadrática, para mostrar visualmente que no hay curvatura.

rango_isei <- range(mod$df_H4$ISEI_orig_hat, na.rm = TRUE)
media_isei <- mean(mod$df_H4$ISEI_orig_hat, na.rm = TRUE)

nd <- tibble(
  ISEI_orig_hat = seq(rango_isei[1], rango_isei[2], length.out = 100),
  # CORREGIDO (20-ago-2026): H4_cuadratico ahora usa educ_f5 (Decisión 12 de
  # 05_modelos.R), no educ_f -- esta línea seguía usando educ_f, lo que
  # habría hecho fallar predict() (columna inexistente en el modelo) o, si
  # mod$df_H4$educ_f seguía presente como columna sin usarse, habría sido
  # silenciosamente ignorada por predict() sin error, dejando la curva mal
  # ajustada. Se fija en el nivel de referencia (Básica o menos).
  educ_f5 = factor(levels(mod$df_H4$educ_f5)[1], levels = levels(mod$df_H4$educ_f5)),
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
  labs(title = "H4: el efecto DIRECTO del origen no sigue una forma de U",
       subtitle = "Valores predichos con especificación cuadrática, controlando por educación (nivel de referencia)",
       x = "ISEI de origen", y = "Cierre estructural predicho",
       caption = paste0("Término cuadrático no significativo (p = 0,81). A diferencia de la forma de U documentada\n",
                        "por Otero, Völker y Rözer (2021), aquí el efecto directo es plano -- el efecto total\n",
                        "(sin controlar por educación) sí es significativo y lineal; ver Fig5b."))
guardar(fig6, "fig6_h4_sin_u", w = 7.5, h = 5)

# NOTA (20-ago-2026): Figura 7 (H4b: efecto principal de share_com4_ego)
# se retira -- H4b se retiró del pipeline en 05_modelos.R (Decisión 14, a
# pedido del investigador). La antigua Figura 8 (correlaciones) pasa a ser
# Figura 7.

# =============================================================================
# FIGURA 7 — Matriz de correlaciones entre indicadores de red
# =============================================================================

cor_long <- as.data.frame(as.table(mod$mat_cor)) |>
  rename(v1 = Var1, v2 = Var2, r = Freq) |>
  mutate(v1_lab = etiquetar(as.character(v1)), v2_lab = etiquetar(as.character(v2)))

orden <- unique(cor_long$v1_lab)

fig7 <- ggplot(cor_long, aes(factor(v1_lab, orden), factor(v2_lab, rev(orden)), fill = r)) +
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
guardar(fig7, "fig7_correlaciones", w = 8, h = 7)

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

# DECISIÓN 12 (05_modelos.R): H2 y H4 usan educ_f5 (5 categorías colapsadas),
# no educ_f (10 niveles, referencia n=3) ni educ continua -- coef_map
# necesita una entrada por nivel no-referencia, o modelsummary elimina esas
# filas en vez de mostrarlas sin etiqueta.
ROTULOS_EDUC_F5 <- c(
  "2. Media"                     = "Media",
  "3. Tecnica superior"          = "Técnica superior",
  "4. Universitaria incompleta"  = "Universitaria incompleta",
  "5. Universitaria completa+"   = "Universitaria completa o posgrado"
)
niveles_educ_f5 <- levels(mod$df_ego$educ_f5)[-1]  # sin el nivel de referencia
cm_educ_f5      <- setNames(paste0("Educación: ", ROTULOS_EDUC_F5[niveles_educ_f5]),
                             paste0("educ_f5", niveles_educ_f5))

cm_ego <- c("ISEI_orig_hat" = "ISEI de origen", "ISEI_orig_c" = "ISEI de origen (centrado)",
            "ISEI_orig_c2" = "ISEI de origen² ",
            cm_educ_f5, "sexoMujer" = "Mujer", "edad" = "Edad",
            "Ext" = "Extensión", "Rango_P" = "Rango de prestigio",
            "Estatus_Max" = "Estatus máximo")

# NOTA (20-ago-2026): H4b se retira de esta tabla -- ver Decisión 14 de
# 05_modelos.R (ya no está en mod$H4b, se eliminó del pipeline).
modelsummary(list("H2: diversidad red" = mod$H2_div,
                  "H4: cierre (lineal)" = mod$H4_lineal,
                  "H4: cierre (cuadrático)" = mod$H4_cuadratico),
             output = "gt", coef_map = cm_ego, stars = TRUE,
             gof_map = c("nobs", "r.squared"),
             title = "H2 y H4: modelos a nivel ego")

cat("\n=== FIN ETAPA 06: 7 figuras + 3 tablas ===\n")
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
# V5. RETIRADA (20-ago-2026, ver V8). Describía la advertencia incorporada en
#     la antigua Figura 7 (H4b), retirada del pipeline.
# V6. La Figura 7 (antes Figura 8, renumerada tras retirar H4b -- ver V8)
#     señala explícitamente la correlación de 0,95 entre rango de prestigio
#     y estatus máximo. Ambos están en todos los modelos a nivel ego como
#     controles; sus coeficientes individuales no son interpretables por
#     separado, aunque su inclusión conjunta como control sea correcta.
# V7. NUEVO (20-ago-2026). Fig5 se retitula: "no predice" -> "sin efecto
#     directo controlando por educación". El escalonamiento de controles
#     (05_modelos.R) muestra que ISEI_orig_hat SÍ predice ambas DV en forma
#     bivariada y pierde significancia justo al entrar educ_f5 (caída ~88% en
#     H2, ~64% en H4) -- consistente con mediación vía educación (Blau-Duncan:
#     origen -> educación -> destino), no con ausencia de efecto. Se agrega
#     Fig5b, que visualiza la trayectoria del coeficiente paso a paso y hace
#     visible dónde ocurre la caída. Fig6 se recaptiona en el mismo sentido:
#     la curva sin forma de U es el efecto DIRECTO, no el total. NO se corrió
#     un test formal de mediación (Baron-Kenny/bootstrap) -- Fig5b es
#     evidencia descriptiva, no una prueba formal; declarar esa distinción en
#     el texto de la tesis antes de afirmar mediación como resultado firme.
#     De paso, se corrige un error preexistente en el caption de Fig6: decía
#     "p = 0,89" para el término cuadrático; el valor real impreso por
#     H4_cuadratico es p = 0,8144 (0,81).
# V8. NUEVO (20-ago-2026). H4b se retira del pipeline a pedido del
#     investigador (ver Decisión 14 de 05_modelos.R): se elimina la antigua
#     Figura 7 (efecto principal de share_com4_ego) y la columna "H4b" de la
#     tabla modelsummary de H2/H4. La antigua Figura 8 (correlaciones) pasa a
#     numerarse como Figura 7. Además, H2/H4 pasan de educ_f (10 niveles,
#     referencia n=3) a educ_f5 (5 categorías colapsadas -- ver Decisión 12
#     de 05_modelos.R); se corrige de paso un bug real en la Figura 6: el
#     newdata para la curva cuadrática de H4 seguía construyendo la columna
#     `educ_f` cuando H4_cuadratico ya usaba `educ_f5`, lo que habría hecho
#     fallar predict() o producido una curva mal ajustada sin aviso.
# =============================================================================
