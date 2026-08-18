# =============================================================================
# 07_lectura_resultados.R
# Lectura esencial de resultados, hipótesis por hipótesis.
# Tesis Magíster en Sociología — PUC Chile | Trajan Pirkovic Palma
#
# AUTOCONTENIDO: lee intermediate/ca_coords.rds y modelos.rds (etapas 03, 05).
# Solo LEE: no reestima nada. Seguro correrlo cuantas veces se quiera.
#
# ACCIONES QUE EJECUTA ESTE SCRIPT, EN ORDEN:
#   1.  Carga ca_coords.rds y modelos.rds.
#   2.  Define el diccionario `etiquetas` (sigla -> nombre completo) y la
#       función tabla_modelos() (gt/modelsummary, se muestra en el Viewer).
#   3.  Construye el mapa del espacio CA con etiquetas y lo exporta a PNG.
#   4.  Arma una tabla gt con las 27 ocupaciones ordenadas por Dim1.
#   5.  Muestra la tabla de H1/H1b (Modelo A: M0, M1, M2) + LRT.
#   6.  Muestra la tabla de H2 (Div_Red, Comp_Red).
#   7.  Muestra la tabla de H3 (Div_ego, Comp_ego).
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(broom.mixed)   # tidy()/glance() para glmmTMB (multinivel)
  library(broom)         # tidy()/glance() para svyglm (regresiones ponderadas)
  library(modelsummary)  # tablas de regresión con calidad académica (gt)
  library(gt)            # estilo fino de las tablas (títulos, notas, fuente)
})

INTERMEDIATE_DIR <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output/intermediate"
OUT_DIR          <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output"

stopifnot(file.exists(file.path(INTERMEDIATE_DIR, "ca_coords.rds")),
          file.exists(file.path(INTERMEDIATE_DIR, "modelos.rds")))

# ── ACCIÓN 1: cargar insumos ──────────────────────────────────────────────────
ca_obj <- readRDS(file.path(INTERMEDIATE_DIR, "ca_coords.rds"))
m      <- readRDS(file.path(INTERMEDIATE_DIR, "modelos.rds"))
gp_coords <- ca_obj$gp_coords

# ── ACCIÓN 2a: diccionario sigla -> nombre completo ────────────────────────
# modelsummary() acepta este vector directo con coef_map: nombres=término tal
# como aparece en el modelo, valores=etiqueta a mostrar en la tabla.
etiquetas <- c(
  "(Intercept)"        = "Intercepto",
  "SP_ip_sc"           = "Similitud de PRESTIGIO ego-posición (ISEI, estandarizada)",
  "SH_ip_sc"           = "Similitud de HABILIDADES ego-posición (espacio CA, estandarizada)",
  "educ_sc"            = "Nivel educativo (estandarizado)",
  "SH_ip_sc:educ_sc"   = "Interacción: similitud de habilidades × nivel educativo",
  "logTam_sc"          = "Tamaño del grupo ocupacional en la RM, log (estandarizado)",
  "ISEI_orig_hat"      = "Clase de origen (ISEI de la ocupación del padre/sostenedor)",
  "educ"               = "Nivel educativo del ego",
  "sexoMujer"          = "Sexo: mujer (ref. hombre)",
  "edad"               = "Edad",
  "Ext"                = "Extensión de la red (total de conocidos en el GP)",
  "Rango_P"            = "Rango de prestigio de la red (ISEI máx. - mín.)",
  "Estatus_Max"        = "Estatus máximo de la red (ISEI más alto alcanzado)",
  "Div_Red"            = "Diversidad de habilidades de la RED (categorías L2, ponderada)",
  "Comp_Red"           = "Complementariedad de la RED (dispersión en el espacio CA)"
)
estrellas <- c('*' = 0.05, '**' = 0.01, '***' = 0.001)

# ── ACCIÓN 2b: función que arma y muestra cada tabla en el Viewer ─────────
# modelsummary(..., output="gt") devuelve un objeto gt; se le agregan notas
# de pie (subtítulo + guía de lectura) y se fuerza print() para que se abra
# en el Viewer aunque el script se corra completo con source().
tabla_modelos <- function(lista_modelos, titulo, subtitulo = NULL, nota = NULL,
                           gof = c("nobs", "aic", "bic")) {
  tbl <- modelsummary(
    lista_modelos, coef_map = etiquetas, stars = estrellas,
    gof_map = gof, output = "gt", title = titulo
  )
  if (!is.null(subtitulo)) tbl <- tbl |> gt::tab_source_note(gt::md(paste0("*", subtitulo, "*")))
  if (!is.null(nota))      tbl <- tbl |> gt::tab_source_note(gt::md(nota))
  tbl <- tbl |> gt::tab_options(table.font.size = gt::px(14), heading.title.font.weight = "bold")
  print(tbl)
  invisible(tbl)
}

# ── ACCIÓN 3: mapa del espacio de habilidades con etiquetas (PNG) ─────────
# geom_point(aes(size=isei)) codifica el prestigio en el TAMAÑO del punto;
# geom_text(aes(label=label)) pone el nombre de la ocupación (no el código)
# arriba de cada punto. Permite ver de un vistazo si ocupaciones cercanas en
# el plano (similares en habilidades) tienen o no tamaño similar (prestigio).
cat("=== PASO 3: Mapa del espacio de habilidades (CA) ===\n")

p_espacio <- gp_coords |>
  filter(!is.na(Dim1_p)) |>
  ggplot(aes(x = Dim1_p, y = Dim2_p, label = label)) +
  geom_hline(yintercept = 0, color = "grey85", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "grey85", linewidth = 0.3) +
  geom_point(aes(size = isei), color = "#173F8A", alpha = 0.65) +
  geom_text(size = 3, vjust = -0.9, hjust = 0.5, color = "grey20") +
  scale_size_continuous(range = c(2, 10), name = "ISEI\n(prestigio)") +
  labs(
    title    = "Espacio de habilidades (CA): 27 ocupaciones del generador de posiciones",
    subtitle = "Cercanía en el plano = perfiles de habilidades similares · Tamaño = prestigio (ISEI)",
    x = "Dim1  (socio-cognitivo  ←→  manual)",
    y = "Dim2  (orientación sectorial)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13))

ggsave(file.path(OUT_DIR, "fig_CA_espacio_etiquetado.png"), p_espacio, width = 11, height = 8, dpi = 200)
cat("Guardado: fig_CA_espacio_etiquetado.png (única exportación a archivo; sin PDF)\n")

# ── ACCIÓN 4: tabla gt con las 27 ocupaciones ordenadas por Dim1 ──────────
tabla_ca <- gp_coords |>
  filter(!is.na(Dim1_p)) |>
  transmute(Ocupacion = label, Dim1 = round(Dim1_p, 2), Dim2 = round(Dim2_p, 2), ISEI = isei) |>
  arrange(Dim1) |>
  gt() |>
  tab_header(title = "P1 · Ocupaciones del GP en el espacio de habilidades (CA)",
             subtitle = "Ordenadas por Dim1: socio-cognitivo (negativo) → manual (positivo)") |>
  fmt_number(columns = c(Dim1, Dim2), decimals = 2) |>
  tab_options(table.font.size = px(13))
print(tabla_ca)

# ── ACCIÓN 5: H1/H1b — tabla del Modelo A (M0, M1, M2) + LRT ──────────────
cat("\n=== PASO 5: H1/H1b — Modelo A (multinivel) ===\n")

tabla_modelos(
  list("M0: base"                     = m$M0,
       "M1: + habilidades (H1)"       = m$M1,
       "M2: + interacción educ (H1b)" = m$M2),
  titulo    = "H1 / H1b · Modelo A — Homofilia multinivel (nbinom2)",
  subtitulo = "VD: n° de conocidos en cada una de las 27 posiciones del GP, por ego",
  nota      = "**Lectura:** si 'Similitud de HABILIDADES' se mantiene significativa junto a 'Similitud de PRESTIGIO' en M1, H1 se sostiene: la homofilia de habilidades no se reduce a la de prestigio. En M2, la interacción prueba H1b (¿varía con educación?).",
  gof       = c("nobs", "aic", "bic", "logLik")
)

cat("\n--- Comparación de modelos (LRT): ¿mejora el ajuste al agregar cada bloque? ---\n")
print(anova(m$M0, m$M1, m$M2))
cat("(Nota: M2 agrega a la vez la interacción y una pendiente aleatoria de\n",
    " habilidades por ego, así que sus 4 g.l. no se deben solo a la interacción.)\n", sep = "")

# ── ACCIÓN 6: H2 — clase de origen → composición de habilidades de la red ──
cat("\n=== PASO 6: H2 — Clase de origen → composición de habilidades de la red ===\n")

tabla_modelos(
  list("H2a: Diversidad de la red (Div_Red)"         = m$H2_div,
       "H2b: Complementariedad de la red (Comp_Red)" = m$H2_comp),
  titulo    = "H2 · Clase de origen → composición de habilidades de la red",
  subtitulo = "svyglm ponderado; VD indicada en cada columna",
  nota      = "**Lectura:** 'Clase de origen' positivo y significativo en H2a = el origen social estratifica el acceso a redes con habilidades diversas."
)

# ── ACCIÓN 7: H3 — composición de la red → perfil de habilidades del ego ──
cat("\n=== PASO 7: H3 — Composición de la red → perfil de habilidades del ego ===\n")

tabla_modelos(
  list("H3a: Diversidad del ego (Div_ego)"       = m$H3_div,
       "H3b: Complejidad ocupacional (Comp_ego)" = m$H3_comp),
  titulo    = "H3 · Composición de habilidades de la red → perfil del ego",
  subtitulo = "svyglm ponderado; controla por Ext, Rango_P y Estatus_Max (prestigio)",
  nota      = "**OJO con el signo:** en la corrida actual, Div_Red y Comp_Red salieron NEGATIVOS sobre Div_ego — a discutir (¿especialización del ego en redes diversas?) antes de fijar la redacción."
)

cat("\n", strrep("=", 78), "\n", sep = "")
cat("FIN — Tablas mostradas en el Viewer (gt/modelsummary).\n")
cat("Única figura exportada a archivo: fig_CA_espacio_etiquetado.png (sin PDF).\n")
cat(strrep("=", 78), "\n", sep = "")
