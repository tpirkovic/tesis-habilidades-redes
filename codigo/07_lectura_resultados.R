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
#   PASO 3  → P2 · H2: clase de origen → diversidad de habilidades de la red
#   PASO 4  → P2 · H3: correspondencia de comunidades entre red y ego (4 modelos)
#   PASO 5  → P2 · H4: clase de origen → cierre estructural
#
# ACTUALIZADO (20-ago-2026): PASO 3/4 reescritos -- referenciaban m$H2_comp,
# m$H3_div, m$H3_comp, objetos de una version anterior de 05_modelos.R que ya
# no existen (H2 se redujo a Div_Red + Orient_ego descriptivo; H3 pasó de
# Div_ego/Comp_ego a 4 modelos paralelos de correspondencia de comunidades).
# El script fallaba en PASO 3 (m$H2_comp es NULL) sin haber corrido nunca
# PASO 4/5 desde ese cambio. Se agregó entonces PASO 5/6 (H4, H4b).
#
# ACTUALIZADO (20-ago-2026, 2da vez): PASO 3 y PASO 5 ahora imprimen también
# tabla_h2_escalon/tabla_h4_escalon (05_modelos.R, DECISIÓN 11) y la guía de
# lectura se corrige: el efecto DIRECTO de ISEI de origen no es significativo
# controlando por educación, pero el efecto en su forma bivariada (sin
# controles) sí lo es y cae ~88% (H2) / ~64% (H4) justo al entrar educ_f5 --
# evidencia descriptiva de mediación vía educación (origen -> educación ->
# destino), no de ausencia de efecto. "H2/H4 no se sostienen" ya no es la
# lectura correcta sin esa matización.
#
# ACTUALIZADO (20-ago-2026, 3ra vez): educ_f (10 niveles, referencia n=3) se
# reemplaza por educ_f5 (5 categorías colapsadas, Decisión 12 de
# 05_modelos.R). H4b se retira del pipeline a pedido del investigador
# (Decisión 14 de 05_modelos.R): se elimina el antiguo PASO 6 completo. La
# estructura queda en 5 pasos (0 a 5), no 6.
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
  "SH_ip_sc"           = "Similitud de HABILIDADES ego-posición (red phi, distancia geodésica, estandarizada)",
  "educ_sc"            = "Nivel educativo (estandarizado)",
  "SH_ip_sc:educ_sc"   = "Interacción: similitud de habilidades × nivel educativo",
  "logTam_sc"          = "Tamaño del grupo ocupacional en la RM, log (estandarizado)",
  # --- Modelos a nivel ego (H2, H3, H4) ---
  "ISEI_orig_hat"      = "Clase de origen (ISEI de la ocupación del padre/sostenedor)",
  "ISEI_orig_c"        = "Clase de origen, centrada (ISEI del padre - media)",
  "ISEI_orig_c2"       = "Clase de origen, centrada al cuadrado (forma de U)",
  "educ"               = "Nivel educativo del ego",
  "sexoMujer"          = "Sexo: mujer (ref. hombre)",
  "edad"               = "Edad",
  "Ext"                = "Extensión de la red (total de conocidos en el GP)",
  "Rango_P"            = "Rango de prestigio de la red (ISEI máx. - mín.)",
  "Estatus_Max"        = "Estatus máximo de la red (ISEI más alto alcanzado)",
  "Div_Red"            = "Diversidad de habilidades de la RED (categorías L2, ponderada)",
  "Orient_ego"         = "Orientación sectorial de la RED (Dim2 CA, NO es hipótesis)",
  "SP_red_ego"         = "Homofilia de PRESTIGIO red-ego (control crítico de H3)",
  "share_com1_red"     = "Peso Dirección-servicio en la RED",
  "share_com2_red"     = "Peso Técnico-manual en la RED",
  "share_com3_red"     = "Peso Analítico-digital-simbólico en la RED",
  "share_com4_red"     = "Peso Bio-ambiental-legal en la RED"
)

# DECISIÓN 12 (05_modelos.R): H2 y H4 usan educ_f5 (5 categorías colapsadas),
# no educ_f (10 niveles, referencia n=3) ni educ continua. Reemplaza la
# recodificación anterior -- ver Decisión 12 para el detalle de por qué se
# colapsó (referencia con n=3 inflaba artificialmente los errores estándar).
ROTULOS_EDUC_F5 <- c(
  "2. Media"                    = "Media",
  "3. Tecnica superior"         = "Técnica superior",
  "4. Universitaria incompleta" = "Universitaria incompleta",
  "5. Universitaria completa+"  = "Universitaria completa o posgrado"
)
legibiliza <- function(term) {
  base <- ifelse(term %in% names(etiquetas), etiquetas[term], term)
  es_educ_f5 <- grepl("^educ_f5", term)
  nivel <- sub("^educ_f5", "", term)
  ifelse(es_educ_f5 & nivel %in% names(ROTULOS_EDUC_F5),
         paste0("Educación (ego, H2/H4): ", ROTULOS_EDUC_F5[nivel]), base)
}

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
# PASO 3 — P2 · H2: clase de origen → diversidad de habilidades de la red
# =============================================================================
# PREGUNTA (H2): ¿las personas de origen social más alto tienen redes con
#   habilidades más DIVERSAS, controlando por educación, sexo, edad e
#   indicadores clásicos de prestigio de la red?
#
# UN SOLO modelo de hipótesis (Div_Red). Orient_ego se reporta aparte como
# DESCRIPTIVO, no como hipótesis -- ver Decisión 2 de 05_modelos.R: Dim2 mide
# orientación sectorial, un eje bipolar sin jerarquía normativa, que no admite
# una hipótesis direccional del tipo "a menor origen, menor orientación".
#
# CÓMO LEERLO: si "Clase de origen" es significativo en H2, el acceso
#   relacional a habilidades diversas está estratificado por origen social.

cat("\n\n=== PASO 3: H2 — Clase de origen -> diversidad de habilidades de la red ===\n")

imprimir_coefs(
  broom::tidy(m$H2_div),
  "H2 · Variable dependiente: DIVERSIDAD de habilidades de la red (Div_Red)  [HIPÓTESIS]"
)
imprimir_coefs(
  broom::tidy(m$H2_orient_desc),
  "Descriptivo (NO hipótesis) · Orientación sectorial de la red (Orient_ego)"
)

# NUEVO (20-ago-2026, ver DECISIÓN 12 de 05_modelos.R): escalonamiento de
# controles. El coeficiente de ISEI_orig_hat en cada paso, agregando
# controles de a uno, para ver dónde exactamente pierde significancia.
cat("\n--- Escalonamiento de controles: coeficiente de ISEI de origen por paso ---\n")
print(m$tabla_h2_escalon, n = nrow(m$tabla_h2_escalon))

cat("\nGUÍA DE LECTURA H2:\n")
cat("  · Mira 'Clase de origen' en el primer modelo (Div_Red, con TODOS los\n")
cat("    controles): no es significativo. Eso es el efecto DIRECTO, neto de\n")
cat("    educación de ego.\n")
cat("  · Mira la tabla de escalonamiento: en el paso 1 (solo ISEI de origen,\n")
cat("    sin ningún control) el coeficiente SÍ es significativo (p<0,001) y\n")
cat("    cae ~88% justo al entrar educ_f5 en el paso 2. Esto es evidencia\n")
cat("    (descriptiva, no un test formal de mediación) de que el origen de\n")
cat("    clase predice la diversidad de la red principalmente A TRAVÉS de\n")
cat("    cuánta educación logra ego (origen -> educación -> destino), no de\n")
cat("    forma directa. NO es un resultado nulo sin más -- es un efecto\n")
cat("    mediado. Declarar esta distinción en el texto, no solo 'H2 no se\n")
cat("    sostiene'.\n")
cat("  · El segundo modelo (Orient_ego) es descriptivo, no prueba de hipótesis:\n")
cat("    se reporta para contexto, no se interpreta como confirmación/refutación.\n")


# =============================================================================
# PASO 4 — P2 · H3: correspondencia de comunidades entre red y ego
# =============================================================================
# PREGUNTA (H3): ¿el peso de cada comunidad de habilidades en la RED
#   corresponde al peso de esa misma comunidad en la OCUPACIÓN DE EGO, más
#   allá de la homofilia de PRESTIGIO red-ego (SP_red_ego)?
#
# CUATRO modelos paralelos, uno por comunidad (share_comK_ego ~ share_comK_red
# + SP_red_ego + controles). El contraste crítico en cada uno es SP_red_ego:
# si sigue siendo no significativo mientras share_comK_red sí lo es, la
# correspondencia de CONTENIDO no se reduce a proximidad de ESTATUS.
#
# PENDIENTE (ver README.md): los 4 shares son datos composicionales (suman 1).
# Válido para exploración; antes de reportar como resultado final requiere
# regresión Dirichlet o transformación log-ratio (ILR/ALR).

cat("\n\n=== PASO 4: H3 — Correspondencia de comunidades (red -> ego) ===\n")
cat("[PENDIENTE: los 4 modelos de abajo son 4 svyglm independientes sobre datos\n")
cat(" composicionales -- válido para exploración, no como resultado final. Ver\n")
cat(" README.md / Decisión 3 de 05_modelos.R.]\n")

for (nombre_com in names(m$H3_modelos)) {
  imprimir_coefs(
    broom::tidy(m$H3_modelos[[nombre_com]]),
    sprintf("H3 · Comunidad: %s", nombre_com)
  )
}

cat("\nGUÍA DE LECTURA H3:\n")
cat("  · En cada comunidad, mira 'Peso [comunidad] en la RED' (predictor) y\n")
cat("    'Homofilia de PRESTIGIO red-ego' (control crítico).\n")
cat("  · Si el peso de la red es significativo y SP_red_ego NO lo es, la\n")
cat("    correspondencia de contenido no se reduce a proximidad de estatus.\n")


# =============================================================================
# PASO 5 — P2 · H4: clase de origen → cierre estructural
# =============================================================================
# PREGUNTA (H4): ¿las personas de origen social más alto tienen redes con
#   MENOR cierre estructural (menos solapamiento entre el perfil de
#   comunidades de ego y el de su red)?
#
# Dos especificaciones: lineal (principal) y cuadrática (robustez, forma de U
# -- Otero, Völker y Rözer 2021).

cat("\n\n=== PASO 5: H4 — Clase de origen -> cierre estructural ===\n")

imprimir_coefs(
  broom::tidy(m$H4_lineal),
  "H4 · Especificación lineal  [PRINCIPAL]"
)
imprimir_coefs(
  broom::tidy(m$H4_cuadratico),
  "H4 · Especificación cuadrática (forma de U)  [ROBUSTEZ]"
)

# NUEVO (20-ago-2026, ver DECISIÓN 12 de 05_modelos.R): mismo escalonamiento
# que en H2.
cat("\n--- Escalonamiento de controles: coeficiente de ISEI de origen por paso ---\n")
print(m$tabla_h4_escalon, n = nrow(m$tabla_h4_escalon))

cat("\nGUÍA DE LECTURA H4:\n")
cat("  · Mira 'Clase de origen' en la especificación lineal CON TODOS los\n")
cat("    controles: no es significativo (p=0,23). Ese es el efecto DIRECTO,\n")
cat("    neto de educación de ego.\n")
cat("  · Mira la tabla de escalonamiento: en el paso 1 (solo ISEI de origen)\n")
cat("    el coeficiente SÍ es significativo (p<0,001, negativo -- mayor\n")
cat("    origen, menor cierre) y cae ~64% al entrar educ_f5 en el paso 2, tras\n")
cat("    lo cual queda marginal (p~0,08-0,09) y luego no significativo.\n")
cat("    Mismo patrón que H2: evidencia (descriptiva) de mediación vía\n")
cat("    educación, no de ausencia de efecto. Declarar esta distinción en el\n")
cat("    texto en vez de reportar 'H4 no se sostiene' sin más.\n")
cat("  · En la cuadrática, mira el término al cuadrado: significativo y\n")
cat("    positivo sugeriría forma de U (alto cierre en ambos extremos). Aquí\n")
cat("    no lo es (p=0,81) -- pero esa curva es también el efecto DIRECTO.\n")

# NOTA (20-ago-2026): PASO 6 (H4b: moderación por peso bio-ambiental-legal de
# ego) se retira -- H4b se eliminó del pipeline en 05_modelos.R (Decisión
# 14, a pedido del investigador). m$H4b ya no existe en modelos.rds.

cat("\n\n", strrep("=", 78), "\n", sep = "")
cat("FIN — Figuras nuevas: fig_CA_espacio_etiquetado.(pdf/png)\n")
cat("Todo lo demás se imprimió en esta consola, hipótesis por hipótesis (H1 a H4).\n")
cat(strrep("=", 78), "\n", sep = "")
