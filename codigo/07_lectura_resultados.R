# =============================================================================
# 07_lectura_resultados.R
# Lectura esencial de resultados, hipótesis por hipótesis.
# Tesis Magíster en Sociología — PUC Chile | Trajan Pirkovic Palma
#
# QUÉ HACE ESTE SCRIPT (y qué NO):
#   - NO reestima nada pesado ni cambia decisiones metodológicas.
#   - Toma los objetos ya calculados por el pipeline (etapas 03 y 05) y los
#     presenta de forma legible: el mapa del CA con etiquetas de ocupaciones,
#     el modelo multinivel resumido en consola, y las regresiones H2/H3 con
#     los NOMBRES COMPLETOS de cada indicador (no las abreviaturas SH_ip,
#     Div_Red, etc., que resultan confusas al leerlas de corrido).
#
# CÓMO SE USA:
#   Correr DESPUÉS de 03_ca_habilidades_isei.R y 05_modelos.R (que dejan sus
#   salidas en intermediate/). Este script solo LEE esos .rds; es seguro
#   correrlo cuantas veces se quiera.
#
# ESTRUCTURA:
#   PASO 0  → Cargar objetos y definir el diccionario de nombres legibles
#   PASO 1  → P1 · CA: mapa del espacio de habilidades con etiquetas
#   PASO 2  → P2 · H1/H1b: modelo multinivel (Modelo A) en consola
#   PASO 3  → P2 · H2: clase de origen → composición de habilidades de la red
#   PASO 4  → P2 · H3: composición de la red → perfil de habilidades del ego
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(broom.mixed)   # tidy() para glmmTMB (multinivel)
  library(broom)         # tidy() para svyglm (regresiones ponderadas)
})

INTERMEDIATE_DIR <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output/intermediate"
OUT_DIR          <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output"

# =============================================================================
# PASO 0 — Cargar objetos y diccionario de nombres legibles
# =============================================================================
# El pipeline guarda todo con nombres cortos (SH_ip, Div_Red, ISEI_orig_hat...).
# Este diccionario traduce cada nombre corto a una etiqueta larga y clara, para
# que las tablas de salida se lean sin tener que recordar qué es cada sigla.

stopifnot(file.exists(file.path(INTERMEDIATE_DIR, "ca_coords.rds")),
          file.exists(file.path(INTERMEDIATE_DIR, "modelos.rds")))

ca_obj <- readRDS(file.path(INTERMEDIATE_DIR, "ca_coords.rds"))
m      <- readRDS(file.path(INTERMEDIATE_DIR, "modelos.rds"))

gp_coords <- ca_obj$gp_coords

# ── Diccionario: nombre corto (como aparece en los modelos) -> etiqueta larga ─
etiquetas <- c(
  # --- Modelo A (multinivel, nivel ego × posición) ---
  "(Intercept)"        = "Intercepto",
  "SP_ip_sc"           = "Similitud de PRESTIGIO ego-posición (ISEI, estandarizada)",
  "SH_ip_sc"           = "Similitud de HABILIDADES ego-posición (espacio CA, estandarizada)",
  "educ_sc"            = "Nivel educativo (estandarizado)",
  "SH_ip_sc:educ_sc"   = "Interacción: similitud de habilidades × nivel educativo",
  "logTam_sc"          = "Tamaño del grupo ocupacional en la RM, log (estandarizado)",
  # --- Modelos a nivel ego (H2, H3) ---
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

legibiliza <- function(term) ifelse(term %in% names(etiquetas), etiquetas[term], term)

sig_estrellas <- function(p) case_when(
  is.na(p)    ~ "",
  p < 0.001   ~ "***",
  p < 0.01    ~ "**",
  p < 0.05    ~ "*",
  p < 0.1     ~ ".",
  TRUE        ~ ""
)

# Impresión legible de una tabla de coeficientes ya "tidy"
imprimir_coefs <- function(tidy_df, titulo) {
  tab <- tidy_df |>
    transmute(
      Indicador  = legibiliza(term),
      b          = round(estimate, 4),
      EE         = round(std.error, 4),
      p          = round(p.value, 4),
      Sig        = sig_estrellas(p.value)
    )
  cat("\n", strrep("=", 78), "\n", titulo, "\n", strrep("=", 78), "\n", sep = "")
  print(tab, n = nrow(tab), width = Inf)
  cat("\nSignificancia: *** p<0.001  ** p<0.01  * p<0.05  . p<0.1\n")
  invisible(tab)
}


# =============================================================================
# PASO 1 — P1 · CA: mapa del espacio de habilidades con etiquetas
# =============================================================================
# El CA ubica cada una de las 27 ocupaciones del generador de posiciones en un
# plano de 2 dimensiones según su perfil de habilidades. Este gráfico las
# muestra ETIQUETADAS con su nombre (no el código), y el tamaño del punto es
# el ISEI: permite VER de un vistazo si ocupaciones cercanas en habilidades
# (cercanas en el plano) tienen o no prestigio parecido (tamaño parecido).
#
#   Dim1 (eje X) ≈ jerarquía socio-cognitiva vs. manual
#   Dim2 (eje Y) ≈ orientación sectorial (cuidado / gestión / técnica ...)

cat("\n=== PASO 1: Mapa del espacio de habilidades (CA) ===\n")
cat("Varianza aprox. — usar ca_obj para el detalle; aquí solo el mapa.\n")

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

ggsave(file.path(OUT_DIR, "fig_CA_espacio_etiquetado.pdf"), p_espacio, width = 11, height = 8)
ggsave(file.path(OUT_DIR, "fig_CA_espacio_etiquetado.png"), p_espacio, width = 11, height = 8, dpi = 200)
cat("Guardado: fig_CA_espacio_etiquetado.pdf / .png\n")

# Tabla de apoyo: coordenadas + ISEI de cada ocupación, ordenada por Dim1
cat("\n--- Ocupaciones del GP ordenadas por Dim1 (con su ISEI) ---\n")
gp_coords |>
  filter(!is.na(Dim1_p)) |>
  transmute(Ocupacion = label, Dim1 = round(Dim1_p, 2), Dim2 = round(Dim2_p, 2), ISEI = isei) |>
  arrange(Dim1) |>
  print(n = 27)


# =============================================================================
# PASO 2 — P2 · H1 / H1b: Modelo A (multinivel) en consola
# =============================================================================
# PREGUNTA (H1): ¿las personas tienden a conocer más gente en ocupaciones cuyo
#   perfil de HABILIDADES se parece al suyo (homofilia de habilidades), incluso
#   controlando por la similitud de PRESTIGIO?
# PREGUNTA (H1b): ¿esa homofilia de habilidades cambia según el nivel educativo?
#
# Estructura: cada fila es un par (ego × una de las 27 posiciones del GP); el
# resultado es el número de conocidos en esa posición. Modelo binomial negativo
# con intercepto (y en M2, pendiente) aleatorio por ego.
#
#   M0: solo similitud de prestigio (+ tamaño de grupo)      → línea base
#   M1: agrega similitud de habilidades                       → prueba H1
#   M2: agrega interacción habilidades × educación            → prueba H1b
#
# CÓMO LEERLO: en M1, si "Similitud de HABILIDADES" es positivo y significativo
#   MANTENIÉNDOSE junto a "Similitud de PRESTIGIO", entonces la homofilia de
#   habilidades NO se reduce a homofilia de prestigio: es una dimensión propia.

cat("\n\n=== PASO 2: H1/H1b — Modelo A (multinivel) ===\n")

coef_M0 <- broom.mixed::tidy(m$M0, effects = "fixed") |> mutate(modelo = "M0 (base: prestigio)")
coef_M1 <- broom.mixed::tidy(m$M1, effects = "fixed") |> mutate(modelo = "M1 (+ habilidades, H1)")
coef_M2 <- broom.mixed::tidy(m$M2, effects = "fixed") |> mutate(modelo = "M2 (+ interacción educ, H1b)")

imprimir_coefs(coef_M0, "H1 · M0 — Solo similitud de prestigio (línea base)")
imprimir_coefs(coef_M1, "H1 · M1 — Agrega similitud de habilidades  [PRUEBA DE H1]")
imprimir_coefs(coef_M2, "H1b · M2 — Agrega interacción habilidades × educación  [PRUEBA DE H1b]")

# Comparación de ajuste entre los tres modelos (¿mejora al agregar habilidades?)
cat("\n--- Comparación de modelos (LRT): ¿mejora el ajuste al agregar cada bloque? ---\n")
print(anova(m$M0, m$M1, m$M2))

cat("\nGUÍA DE LECTURA H1:\n")
cat("  · Fíjate en 'Similitud de HABILIDADES' en M1: signo, magnitud y estrellas.\n")
cat("  · Si sigue significativo junto a 'Similitud de PRESTIGIO', H1 se sostiene.\n")
cat("  · El control 'Tamaño del grupo ocupacional' descarta que el efecto sea solo\n")
cat("    por conocer gente en ocupaciones más comunes.\n")
cat("GUÍA DE LECTURA H1b:\n")
cat("  · Fíjate en la 'Interacción: similitud de habilidades × nivel educativo' en M2.\n")
cat("  · Significativa = la homofilia de habilidades es más (o menos) fuerte según educación.\n")


# =============================================================================
# PASO 3 — P2 · H2: clase de origen → composición de habilidades de la red
# =============================================================================
# PREGUNTA (H2): ¿las personas de origen social más alto tienen redes con
#   habilidades más DIVERSAS y/o más COMPLEMENTARIAS, controlando por educación,
#   sexo y edad?
#
# Dos modelos, uno por cada variable dependiente:
#   H2a: Diversidad de habilidades de la red  (Div_Red)
#   H2b: Complementariedad de la red          (Comp_Red)
# Predictor de interés: clase de origen (ISEI del padre).
#
# CÓMO LEERLO: si "Clase de origen" es positivo y significativo en H2a, el
#   acceso relacional a habilidades diversas está estratificado por origen.

cat("\n\n=== PASO 3: H2 — Clase de origen → composición de habilidades de la red ===\n")

imprimir_coefs(
  broom::tidy(m$H2_div),
  "H2a · Variable dependiente: DIVERSIDAD de habilidades de la red (Div_Red)"
)
imprimir_coefs(
  broom::tidy(m$H2_comp),
  "H2b · Variable dependiente: COMPLEMENTARIEDAD de la red (Comp_Red)"
)

cat("\nGUÍA DE LECTURA H2:\n")
cat("  · Mira 'Clase de origen (ISEI del padre)' en cada modelo.\n")
cat("  · Positivo y significativo = a mayor origen social, red con habilidades\n")
cat("    más diversas / más complementarias.\n")


# =============================================================================
# PASO 4 — P2 · H3: composición de la red → perfil de habilidades del ego
# =============================================================================
# PREGUNTA (H3): ¿la composición de HABILIDADES de la red (diversidad y
#   complementariedad) se asocia con el perfil de habilidades del propio ego
#   (su diversidad y su complejidad ocupacional), MÁS ALLÁ de los indicadores
#   clásicos de la red basados en prestigio (extensión, rango, estatus máximo)?
#
# Dos modelos, uno por cada variable dependiente del ego:
#   H3a: Diversidad de habilidades de la ocupación del ego  (Div_ego)
#   H3b: Complejidad de la ocupación del ego                (Comp_ego, Dim2 CA)
# En ambos se controla por los indicadores clásicos del GP (prestigio/tamaño),
# de modo que el coeficiente de Div_Red / Comp_Red capta el aporte ESPECÍFICO
# del contenido de habilidades de la red.
#
# CÓMO LEERLO: si 'Diversidad de la red' o 'Complementariedad de la red' son
#   significativos mientras extensión/rango/estatus NO lo son, entonces el
#   contenido de habilidades de la red aporta algo que el prestigio de la red
#   no capta.

cat("\n\n=== PASO 4: H3 — Composición de la red → perfil de habilidades del ego ===\n")

imprimir_coefs(
  broom::tidy(m$H3_div),
  "H3a · Variable dependiente: DIVERSIDAD de habilidades del EGO (Div_ego)"
)
imprimir_coefs(
  broom::tidy(m$H3_comp),
  "H3b · Variable dependiente: COMPLEJIDAD ocupacional del EGO (Comp_ego)"
)

cat("\nGUÍA DE LECTURA H3:\n")
cat("  · Compara los indicadores de HABILIDADES de la red (Diversidad,\n")
cat("    Complementariedad) contra los de PRESTIGIO (Extensión, Rango, Estatus máx.).\n")
cat("  · Si los de habilidades son significativos y los de prestigio no,\n")
cat("    el contenido de habilidades de la red aporta información propia.\n")
cat("  · OJO con el SIGNO: en la corrida actual Div_Red y Comp_Red salieron\n")
cat("    NEGATIVOS sobre Div_ego. Discutir interpretación (¿especialización\n")
cat("    del ego en redes diversas?) antes de fijar la redacción.\n")

cat("\n\n", strrep("=", 78), "\n", sep = "")
cat("FIN — Figuras nuevas: fig_CA_espacio_etiquetado.(pdf/png)\n")
cat("Todo lo demás se imprimió en esta consola, hipótesis por hipótesis.\n")
cat(strrep("=", 78), "\n", sep = "")
