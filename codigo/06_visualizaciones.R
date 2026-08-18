# =============================================================================
# 06_visualizaciones.R
# Etapa 6 del pipeline: visualizaciones y tablas de apoyo (informe ejecutivo).
# Tesis Magíster en Sociología — PUC Chile | Trajan Pirkovic Palma
#
# AUTOCONTENIDO: lee intermediate/modelos.rds (etapa 05).
# NOTA: H4 (ingreso) y el Modelo B2 (Oesch) están fuera del alcance actual del
# proyecto (decisión 20-jul-2026); este script y 05_modelos.R ya no los incluyen.
#
# ACCIONES QUE EJECUTA ESTE SCRIPT, EN ORDEN:
#   1. Carga modelos.rds y expone sus elementos al entorno.
#   2. Arma y exporta el glosario hipótesis→indicador→definición→modelo.
#   3. Extrae los coeficientes fijos de M0/M1/M2 y arma el forest plot.
#   4. Exporta el forest plot como PNG (sin PDF).
#   5. Arma la tabla comparativa H2/H3 (gt, mostrada en el Viewer).
#   6. Arma el panel de distribuciones de indicadores clave y lo exporta PNG.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse); library(broom.mixed); library(broom); library(modelsummary); library(gt)
})

INTERMEDIATE_DIR <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output/intermediate"
OUT_DIR          <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output"

stopifnot(file.exists(file.path(INTERMEDIATE_DIR, "modelos.rds")))

# ── ACCIÓN 1: cargar modelos y exponerlos al entorno ────────────────────────
# list2env() evita tener que escribir m$M0, m$M1, ... en todo lo que sigue.
m <- readRDS(file.path(INTERMEDIATE_DIR, "modelos.rds"))
list2env(m, envir = environment())

# ── ACCIÓN 2: glosario hipótesis → indicador → definición → modelo ─────────
# Tabla de referencia rápida para no tener que recordar qué sigla es cada cosa.
glosario <- tribble(
  ~Hipotesis, ~Indicador,      ~Definicion,                                                    ~Modelo,
  "H1",       "SH_ip",         "Similitud de habilidades ego-posición (dist. euclidiana CA invertida)", "Modelo A (M1)",
  "H1b",      "SH_ip x educ",  "Interacción: ¿la homofilia de habilidades varía con educación?",        "Modelo A (M2)",
  "H1/H1b",   "SP_ip",         "Similitud de prestigio ego-posición (control, dist. ISEI)",             "Modelo A (M0-M2)",
  "H1/H1b",   "logTam_sc",     "log(TamGrupo_p) estandarizado: control de prevalencia poblacional",      "Modelo A (M0-M2)",
  "H2",       "Div_Red",       "Diversidad de habilidades de la red (categorías L2 distintas, pond.)",  "svyglm",
  "H2",       "Comp_Red",      "Complementariedad/dispersión de la red en espacio CA",                  "svyglm",
  "H2/H3",    "ISEI_orig_hat", "ISEI parental (join oficial Ganzeboom; respaldo por regresión)",        "svyglm",
  "H3",       "Div_ego",       "Diversidad de habilidades de la ocupación propia de ego",               "svyglm",
  "H3",       "Comp_ego",      "Complejidad de la ocupación de ego (coordenada Dim2 CA)",               "svyglm"
)
write_csv(glosario, file.path(OUT_DIR, "glosario_indicadores.csv"))

# ── ACCIÓN 3: extraer coeficientes fijos de M0/M1/M2 ────────────────────────
# broom.mixed::tidy(effects="fixed") saca solo los efectos fijos (no la
# varianza aleatoria); conf.int=TRUE agrega los límites del intervalo de
# confianza, necesarios para el forest plot. Se descarta el intercepto
# (no es comparable en la misma escala que los demás coeficientes).
coef_modelo_a <- bind_rows(
  broom.mixed::tidy(M0, effects = "fixed", conf.int = TRUE) |> mutate(modelo = "M0"),
  broom.mixed::tidy(M1, effects = "fixed", conf.int = TRUE) |> mutate(modelo = "M1"),
  broom.mixed::tidy(M2, effects = "fixed", conf.int = TRUE) |> mutate(modelo = "M2")
) |> filter(term != "(Intercept)")

p_coef_a <- ggplot(coef_modelo_a, aes(x = estimate, y = term, color = modelo)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.5)) +
  labs(title = "Modelo A: coeficientes multinivel (con control TamGrupo_p)",
       x = "Coeficiente (escala log, nbinom2)", y = NULL, color = "Modelo") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")

# ── ACCIÓN 4: exportar el forest plot (solo PNG, sin PDF) ──────────────────
ggsave(file.path(OUT_DIR, "fig_coef_modelo_A.png"), p_coef_a, width = 9, height = 5, dpi = 200)

# ── ACCIÓN 5: tabla comparativa H2/H3, mostrada en el Viewer (gt) ──────────
# output="gt" -> objeto gt que se auto-muestra en el panel Viewer al hacer
# print(); no se guarda como archivo HTML.
modelos_svy <- list("H2: Div_Red" = H2_div, "H2: Comp_Red" = H2_comp,
                     "H3: Div_ego" = H3_div, "H3: Comp_ego" = H3_comp)

tabla_comparativa <- modelsummary(
  modelos_svy, stars = c('*' = .05, '**' = .01, '***' = .001),
  gof_map = c("nobs", "r.squared", "aic", "bic"),
  output = "gt",
  title = "Tabla comparativa H2 / H3"
) |> gt::tab_options(table.font.size = gt::px(13))
print(tabla_comparativa)

# ── ACCIÓN 6: panel de distribuciones de indicadores clave ─────────────────
# Se seleccionan 7 indicadores a nivel ego, se pivotean a formato largo, y se
# grafica un histograma por indicador con escalas libres (cada uno tiene su
# propio rango, imposible compararlos en el mismo eje).
vars_panel <- c("Div_Red", "Comp_Red", "Ext", "Rango_P", "Estatus_Max", "Div_ego", "Comp_ego")
df_panel <- df_ego |> select(all_of(vars_panel)) |>
  pivot_longer(everything(), names_to = "indicador", values_to = "valor") |> filter(!is.na(valor))

p_panel <- ggplot(df_panel, aes(x = valor)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.8) +
  facet_wrap(~ indicador, scales = "free", ncol = 4) +
  labs(title = "Distribución de indicadores clave (nivel ego)", x = NULL, y = "Frecuencia") +
  theme_minimal(base_size = 11)
ggsave(file.path(OUT_DIR, "fig_panel_distribuciones.png"), p_panel, width = 12, height = 6, dpi = 200)

cat("Guardado: glosario_indicadores.csv, fig_coef_modelo_A.png, fig_panel_distribuciones.png\n",
    "Tabla comparativa H2/H3 mostrada en el Viewer (gt), sin archivo HTML.\n")
cat("=== FIN ETAPA 06 ===\n")
