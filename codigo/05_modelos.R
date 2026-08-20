# =============================================================================
# 05_modelos.R
# Etapa 5 del pipeline: modelos estadísticos H1/H1b, H2, H3 y H4.
# Tesis Magíster en Sociología — PUC Chile | Trajan Pirkovic Palma
#
# VERSIÓN ACTUALIZADA (agosto 2026). Cambios respecto a la versión anterior:
#   - M0/M1/M2 incorporan sexo, edad y educación como controles de nivel 2,
#     siguiendo la especificación de Espinosa-Rada, Bargsted y Ortiz Ruiz
#     (2026), que condiciona los coeficientes aleatorios por atributos del
#     encuestado.
#   - H2 queda solo con Div_Red como VD (Orient_ego sale de la hipótesis).
#   - H3 se reformula: 4 modelos paralelos de share de comunidad, con
#     SP_red_ego (homofilia de prestigio ego-red) como contraste crítico.
#   - H4 (nueva): clase de origen -> cierre estructural blando.
#   - Se elimina el modelo de ingreso (H4 original, descartada del diseño).
#   - Se agrega un chequeo de colinealidad previo a la interpretación de H4.
#
# CAMBIO (19-ago-2026): SH_ip (CA) se reemplaza por SH_ip_red (distancia
# geodésica en la red de complementariedad phi) como especificación
# principal de H1/H1b -- ver Decisión 8. Requiere base_larga.rds regenerado
# con 04_indicadores_red.R actualizado (Acción 6b).
#
# CAMBIO (20-ago-2026): H4b (moderación por peso bio-ambiental-legal de ego)
# se retira del pipeline a pedido del investigador -- ver nota en la sección
# de H4. H2 y H4 pasan a usar ISEI_orig_hat (no educación) como IV focal,
# con el mismo bloque de controles, y educ_f5 (5 categorías colapsadas, ver
# Decisión 12) en vez de la versión continua o la de 10 niveles.
#
# ESTADO ACTUAL (20-ago-2026) sobre base_larga.rds/df_ego.rds regenerados:
# H1 se sostiene (SH_ip_sc = 0.199*** en M2, controlando SP_ip_sc); H1b se
# sostiene (interacción SH_ip_sc:educ_sc = -0.061***, n=26.325 díadas/975
# egos). H2 y H4: el efecto DIRECTO de ISEI_orig_hat (controlando por
# educ_f5) no es significativo, pero el efecto en su forma BIVARIADA sí lo es
# y cae ~88% (H2) / ~64% (H4) justo al entrar educ_f5 -- evidencia
# descriptiva de mediación vía educación (origen -> educación -> destino),
# no de ausencia de efecto (ver tabla_h2_escalon/tabla_h4_escalon y Decisión
# 11). H3 se sostiene en las 4 comunidades, SP_red_ego n.s. en las cuatro
# (n=931). Ver README.md para la lectura completa.
#
# PENDIENTE (ver README.md): H3 corre como 4 svyglm independientes sobre
# datos composicionales (los 4 shares suman 1) -- válido para exploración,
# pero antes de reportar como resultado final requiere regresión Dirichlet o
# transformación log-ratio (ILR/ALR).
#
# AUTOCONTENIDO: lee intermediate/base_larga.rds y df_ego.rds (etapa 04).
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse); library(glmmTMB); library(survey)
})

INTERMEDIATE_DIR <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output/intermediate"

stopifnot(file.exists(file.path(INTERMEDIATE_DIR, "base_larga.rds")),
          file.exists(file.path(INTERMEDIATE_DIR, "df_ego.rds")))

base_larga <- readRDS(file.path(INTERMEDIATE_DIR, "base_larga.rds"))
df_ego_raw <- readRDS(file.path(INTERMEDIATE_DIR, "df_ego.rds"))

n_com      <- 4
share_cols <- paste0("share_com", seq_len(n_com))
ETIQUETAS  <- c("Direccion-servicio", "Tecnico-manual",
                "Analitico-digital-simbolico", "Bio-ambiental-legal")

# =============================================================================
# H1 y H1b — MODELOS MULTINIVEL DE BINOMIAL NEGATIVA
# =============================================================================
# CAMBIO: se agregan sexo, edad y educación como controles fijos de nivel 2 ya
# desde M0. La versión anterior solo llevaba SP_ip y TamGrupo_p, dejando el
# intercepto sin condicionar por atributos del encuestado. Espinosa-Rada et
# al. (2026) condicionan explícitamente los coeficientes aleatorios por sexo,
# edad y nivel educativo (ecuaciones 2 y 3 de ese paper). Ver DECISIÓN 1.

base_larga <- base_larga |>
  mutate(
    SH_ip_sc  = scale(SH_ip_red)[, 1],  # DECISIÓN 8: SH_ip_red, no SH_ip (CA)
    SP_ip_sc  = scale(SP_ip)[, 1],
    educ_sc   = scale(educ)[, 1],
    edad_sc   = scale(edad)[, 1],
    sexo_f    = factor(sexo),
    logTam_sc = if_else(!is.na(TamGrupo_p), scale(log(TamGrupo_p))[, 1], NA_real_)
  )

tiene_tamgrupo <- sum(!is.na(base_larga$logTam_sc)) > 0
if (!tiene_tamgrupo) {
  warning("TamGrupo_p no disponible — M0-M2 corren SIN el control de ",
          "prevalencia poblacional. Completar data/tam_grupo_p_casen2024_RM.csv ",
          "y volver a correr la etapa 04 antes de tomar estos resultados como finales.")
}

bl_m <- base_larga |>
  filter(!is.na(n_conocidos), !is.na(SP_ip_sc), !is.na(SH_ip_sc),
         !is.na(educ_sc), !is.na(edad_sc), !is.na(sexo_f), !is.na(ID_f))
if (tiene_tamgrupo) bl_m <- bl_m |> filter(!is.na(logTam_sc))

cat("Filas modelo A:", nrow(bl_m), "| egos:", nlevels(droplevels(bl_m$ID_f)), "\n")

form_tam  <- if (tiene_tamgrupo) " + logTam_sc" else ""
CONTROLES <- " + sexo_f + edad_sc + educ_sc"

M0 <- glmmTMB(as.formula(paste0("n_conocidos ~ SP_ip_sc", form_tam, CONTROLES,
                                 " + (1 | ID_f)")),
              family = nbinom2(), data = bl_m)
M1 <- glmmTMB(as.formula(paste0("n_conocidos ~ SP_ip_sc + SH_ip_sc", form_tam, CONTROLES,
                                 " + (1 | ID_f)")),
              family = nbinom2(), data = bl_m)
M2 <- glmmTMB(as.formula(paste0("n_conocidos ~ SP_ip_sc + SH_ip_sc * educ_sc", form_tam,
                                 " + sexo_f + edad_sc + (1 + SH_ip_sc | ID_f)")),
              family = nbinom2(), data = bl_m)

cat("\n=== H1 / H1b: Modelos Multinivel (control TamGrupo_p:", tiene_tamgrupo, ") ===\n")
print(summary(M0)); print(summary(M1)); print(summary(M2))
print(anova(M0, M1, M2))

# =============================================================================
# PREPARACIÓN DE LA BASE EGO Y CHEQUEO DE COLINEALIDAD
# =============================================================================

df_ego <- df_ego_raw |>
  filter(!is.na(Div_Red), !is.na(ISEI_orig_hat), !is.na(educ)) |>
  mutate(
    sexo = factor(sexo),
    # DECISIÓN 9 (superada, ver DECISIÓN 13): educ pasaba de continua a
    # factor no ordenado de 10 niveles (educ_f). Se mantiene la columna por
    # si se necesita de respaldo, pero YA NO se usa en H2/H4 -- ver abajo.
    educ_f = factor(educ, levels = sort(unique(educ)))
  )
df_ego$educ_f <- relevel(df_ego$educ_f, ref = as.character(min(df_ego$educ, na.rm = TRUE)))

# DECISIÓN 13 (20-ago-2026). educ_f (10 niveles) tiene una categoría de
# referencia con n=3 ("Sin estudios") y dos categorías más con n<30 (técnica
# superior incompleta, n=27; posgrado, n=18) -- table(df_ego$educ_f):
# 3/33/51/93/380/27/130/37/206/18. Eso infla artificialmente los errores
# estándar de TODAS las comparaciones contra la referencia, produciendo la
# apariencia de "sin diferencias por educación" en H4 y comparaciones débiles
# en H2 que no reflejan necesariamente ausencia de efecto real (ver
# conversación 20-ago-2026). Se colapsa a 5 categorías sustantivamente
# sensatas, ninguna con n<37, con "Básica o menos" como referencia (n=87):
#   Básica o menos            (niveles 1+2+3,  n=87)
#   Media                     (niveles 4+5,    n=473)
#   Técnica superior          (niveles 6+7,    n=157)
#   Universitaria incompleta  (nivel 8,        n=37)
#   Universitaria completa+   (niveles 9+10,   n=224)
# # Reemplaza a educ_f en H2, H4 y el escalonamiento de controles. H1/H1b
# NO se tocan (mantienen educ_sc continua). H3 tampoco se toca en este
# cambio (sigue con educ continua -- inconsistencia declarada, no resuelta).
df_ego <- df_ego |>
  mutate(
    educ_f5 = case_when(
      educ %in% c(1, 2, 3) ~ "1. Basica o menos",
      educ %in% c(4, 5)    ~ "2. Media",
      educ %in% c(6, 7)    ~ "3. Tecnica superior",
      educ == 8            ~ "4. Universitaria incompleta",
      educ %in% c(9, 10)   ~ "5. Universitaria completa+"
    ),
    educ_f5 = factor(educ_f5)
  )
cat("\nDistribución de educ_f5 (colapsada):\n")
print(table(df_ego$educ_f5))

cat("\nn base ego:", nrow(df_ego), "\n")

# CHEQUEO PREVIO OBLIGATORIO: cierre_blando es pariente conceptual de la
# homofilia agregada, y puede correlacionar fuerte con Div_Red (una red muy
# cerrada tiende a ser poco diversa). Si |r| > 0.7 entre cierre_blando y
# Div_Red, NO deben interpretarse coeficientes de ambos en un mismo modelo.
# Ver DECISIÓN 4.
vars_chequeo <- c("cierre_blando", "Div_Red", "Orient_ego",
                  "Ext", "Rango_P", "Estatus_Max", "SP_red_ego", "ISEI_orig_hat")
mat_cor <- df_ego |> select(any_of(vars_chequeo)) |>
  cor(use = "pairwise.complete.obs")

cat("\n=== CHEQUEO DE COLINEALIDAD (previo a H4) ===\n")
print(round(mat_cor, 3))

r_cierre_div <- mat_cor["cierre_blando", "Div_Red"]
cat(sprintf("\n  Correlacion cierre_blando vs. Div_Red: %.3f\n", r_cierre_div))
if (abs(r_cierre_div) > 0.7) {
  warning("cierre_blando y Div_Red superan |r|=0.7. Reportar en modelos ",
          "SEPARADOS, no conjuntos, y declararlo en el texto.")
} else {
  cat("  Por debajo del umbral 0.7: pueden coexistir en el mismo modelo.\n")
}

svy_ego <- svydesign(ids = ~1, weights = ~weight, data = df_ego)

# =============================================================================
# H2 — CLASE DE ORIGEN -> DIVERSIDAD DE HABILIDADES DE LA RED
# =============================================================================
# CAMBIO: Orient_ego (ex Comp_Red) sale de la hipótesis. Dim2 mide orientación
# sectorial, un eje bipolar sin jerarquía normativa, por lo que no admite una
# hipótesis direccional del tipo "a menor origen, menor orientación".
# Ver DECISIÓN 2.

H2_div <- svyglm(Div_Red ~ ISEI_orig_hat + educ_f5 + sexo + edad +
                   Ext + Rango_P + Estatus_Max,
                 design = svy_ego, family = gaussian())

cat("\n=== H2: Clase de origen -> Diversidad de habilidades de la red ===\n")
print(summary(H2_div))

# NUEVO (20-ago-2026): especificación reducida, SIN Ext/Rango_P/Estatus_Max.
# Motivo: esas tres son variables de la RED de ego, del mismo nivel que la DV
# (Div_Red), no atributos previos como educ/sexo/edad -- si el origen de
# clase opera vía cuánto se extiende o cuán alto llega en prestigio la red,
# controlar por ellas resta exactamente ese canal (sesgo de "bad control").
# Rango_P y Estatus_Max además correlacionan 0.685/0.637 con Div_Red (ver
# mat_cor) -- son parientes cercanos de la propia DV. Se reportan ambas
# versiones, no se reemplaza la original, para comparar si el resultado de
# ISEI_orig_hat depende de esta decisión. Ver DECISIÓN 10.
H2_div_red <- svyglm(Div_Red ~ ISEI_orig_hat + educ_f5 + sexo + edad,
                      design = svy_ego, family = gaussian())
cat("\n--- H2, forma reducida (sin Ext/Rango_P/Estatus_Max) ---\n")
print(summary(H2_div_red))

# descriptivo de Orient_ego (ya no es hipótesis, se reporta como descripción)
H2_orient_desc <- svyglm(Orient_ego ~ ISEI_orig_hat + educ_f5 + sexo + edad,
                          design = svy_ego, family = gaussian())
cat("\n--- Descriptivo (NO hipótesis): origen -> Orient_ego ---\n")
print(summary(H2_orient_desc))

# =============================================================================
# H3 — CORRESPONDENCIA DE COMUNIDADES ENTRE RED Y OCUPACIÓN DE EGO
# =============================================================================
# Cuatro modelos paralelos, uno por comunidad. El contraste crítico es
# SP_red_ego: si el share de comunidad de la red sigue siendo significativo
# controlando por cuán cerca en PRESTIGIO está la red de ego, entonces la
# correspondencia de contenido no se reduce a correspondencia de estatus.
# Ver DECISIÓN 3.

df_H3 <- df_ego |>
  filter(if_all(all_of(c(paste0(share_cols, "_ego"), paste0(share_cols, "_red"),
                          "SP_red_ego")), ~ !is.na(.x)))
svy_H3 <- svydesign(ids = ~1, weights = ~weight, data = df_H3)

cat(sprintf("\n=== H3: Correspondencia de comunidades (n = %d) ===\n", nrow(df_H3)))

H3_modelos <- setNames(vector("list", n_com), ETIQUETAS)
for (k in seq_len(n_com)) {
  dv <- paste0(share_cols[k], "_ego")
  iv <- paste0(share_cols[k], "_red")
  f  <- as.formula(paste0(dv, " ~ ", iv,
                           " + SP_red_ego + Ext + Rango_P + Estatus_Max",
                           " + educ + ISEI_orig_hat + sexo + edad"))
  H3_modelos[[k]] <- svyglm(f, design = svy_H3, family = gaussian())
  cat(sprintf("\n--- H3, comunidad %d: %s ---\n", k, ETIQUETAS[k]))
  print(summary(H3_modelos[[k]]))
}

# =============================================================================
# H4 — CLASE DE ORIGEN -> CIERRE ESTRUCTURAL
# =============================================================================

df_H4 <- df_ego |> filter(!is.na(cierre_blando))
svy_H4 <- svydesign(ids = ~1, weights = ~weight, data = df_H4)

cat(sprintf("\n=== H4: Clase de origen -> Cierre estructural (n = %d) ===\n", nrow(df_H4)))

H4_lineal <- svyglm(cierre_blando ~ ISEI_orig_hat + educ_f5 + sexo + edad +
                      Ext + Rango_P + Estatus_Max,
                    design = svy_H4, family = gaussian())
print(summary(H4_lineal))

# NUEVO (20-ago-2026): forma reducida, sin Ext/Rango_P/Estatus_Max -- mismo
# argumento que en H2 (ver DECISIÓN 10): son variables de la red de ego, del
# mismo nivel que cierre_blando, no controles previos.
H4_lineal_red <- svyglm(cierre_blando ~ ISEI_orig_hat + educ_f5 + sexo + edad,
                         design = svy_H4, family = gaussian())
cat("\n--- H4, forma reducida (sin Ext/Rango_P/Estatus_Max) ---\n")
print(summary(H4_lineal_red))

# ROBUSTEZ: especificación cuadrática. Otero, Völker y Rözer (2021) documentan
# que la segregación de redes en Chile sigue una forma de U a lo largo de la
# distribución de clases (alta en ambos extremos, baja en el centro). Si el
# cierre estructural sigue ese patrón, el término cuadrático debería ser
# significativo y positivo. Ver DECISIÓN 5.
df_H4$ISEI_orig_c  <- df_H4$ISEI_orig_hat - mean(df_H4$ISEI_orig_hat, na.rm = TRUE)
df_H4$ISEI_orig_c2 <- df_H4$ISEI_orig_c^2
svy_H4 <- svydesign(ids = ~1, weights = ~weight, data = df_H4)

H4_cuadratico <- svyglm(cierre_blando ~ ISEI_orig_c + ISEI_orig_c2 + educ_f5 + sexo + edad +
                          Ext + Rango_P + Estatus_Max,
                        design = svy_H4, family = gaussian())
cat("\n--- H4, robustez: especificación cuadrática (forma de U) ---\n")
print(summary(H4_cuadratico))

# NOTA (20-ago-2026): H4b (moderación por peso bio-ambiental-legal de ego)
# se retira del pipeline a pedido del investigador. Ya no se ajusta ni se
# guarda en modelos.rds. El código y las decisiones metodológicas asociadas
# (antigua Decisión 6) se archivan en el historial de versiones de git, no
# aquí, para no dejar código muerto en el script activo.

# =============================================================================
# ESCALONAMIENTO DE CONTROLES — H2 y H4
# =============================================================================
# NUEVO (20-ago-2026). En vez de comparar solo "con todos los controles" vs.
# "sin ninguno" (H2_div/H4_lineal vs. H2_div_red/H4_lineal_red), se agregan
# los controles UNO A UNO para ver en qué paso exacto se mueve el coeficiente
# de ISEI_orig_hat. Rango_P y Estatus_Max correlacionan 0.945 entre sí (ver
# mat_cor) -- entran en pasos separados para poder atribuir el movimiento a
# uno u otro, no a ambos a la vez. La muestra se fija (complete cases sobre
# TODAS las variables del modelo completo) para que n no cambie entre pasos
# y las comparaciones sean limpias.

extraer_isei <- function(formula_str, design, iv = "ISEI_orig_hat") {
  m  <- svyglm(as.formula(formula_str), design = design, family = gaussian())
  co <- summary(m)$coefficients
  tibble(b = co[iv, "Estimate"], se = co[iv, "Std. Error"], p = co[iv, "Pr(>|t|)"])
}

correr_escalonamiento <- function(dv, df, pasos, titulo) {
  vars_todas  <- c("ISEI_orig_hat", "educ_f5", "sexo", "edad",
                    "Ext", "Rango_P", "Estatus_Max", dv, "weight")
  df_esc  <- df |> filter(if_all(all_of(vars_todas), ~ !is.na(.x)))
  svy_esc <- svydesign(ids = ~1, weights = ~weight, data = df_esc)
  cat(sprintf("\n=== ESCALONAMIENTO %s (n = %d, fijo en todos los pasos) ===\n",
              titulo, nrow(df_esc)))
  tabla <- purrr::imap_dfr(pasos, ~ bind_cols(paso = .y, extraer_isei(.x, svy_esc)))
  print(tabla, n = nrow(tabla))
  tabla
}

pasos_h2 <- list(
  "1. Solo ISEI origen"                   = "Div_Red ~ ISEI_orig_hat",
  "2. + educacion"                        = "Div_Red ~ ISEI_orig_hat + educ_f5",
  "3. + sexo y edad"                      = "Div_Red ~ ISEI_orig_hat + educ_f5 + sexo + edad",
  "4. + extension de red (Ext)"           = "Div_Red ~ ISEI_orig_hat + educ_f5 + sexo + edad + Ext",
  "5. + rango de prestigio (Rango_P)"     = "Div_Red ~ ISEI_orig_hat + educ_f5 + sexo + edad + Ext + Rango_P",
  "6. + estatus maximo (modelo completo)" = "Div_Red ~ ISEI_orig_hat + educ_f5 + sexo + edad + Ext + Rango_P + Estatus_Max"
)
tabla_h2_escalon <- correr_escalonamiento("Div_Red", df_ego, pasos_h2, "H2 (Div_Red)")

pasos_h4 <- list(
  "1. Solo ISEI origen"                   = "cierre_blando ~ ISEI_orig_hat",
  "2. + educacion"                        = "cierre_blando ~ ISEI_orig_hat + educ_f5",
  "3. + sexo y edad"                      = "cierre_blando ~ ISEI_orig_hat + educ_f5 + sexo + edad",
  "4. + extension de red (Ext)"           = "cierre_blando ~ ISEI_orig_hat + educ_f5 + sexo + edad + Ext",
  "5. + rango de prestigio (Rango_P)"     = "cierre_blando ~ ISEI_orig_hat + educ_f5 + sexo + edad + Ext + Rango_P",
  "6. + estatus maximo (modelo completo)" = "cierre_blando ~ ISEI_orig_hat + educ_f5 + sexo + edad + Ext + Rango_P + Estatus_Max"
)
tabla_h4_escalon <- correr_escalonamiento("cierre_blando", df_H4, pasos_h4, "H4 (cierre_blando)")

# =============================================================================
# GUARDAR
# =============================================================================

saveRDS(
  list(M0 = M0, M1 = M1, M2 = M2,
       H2_div = H2_div, H2_div_red = H2_div_red, H2_orient_desc = H2_orient_desc,
       H3_modelos = H3_modelos,
       H4_lineal = H4_lineal, H4_lineal_red = H4_lineal_red,
       H4_cuadratico = H4_cuadratico,
       tabla_h2_escalon = tabla_h2_escalon, tabla_h4_escalon = tabla_h4_escalon,
       mat_cor = mat_cor,
       bl_m = bl_m, df_ego = df_ego, df_H3 = df_H3, df_H4 = df_H4,
       tiene_tamgrupo = tiene_tamgrupo),
  file.path(INTERMEDIATE_DIR, "modelos.rds")
)
cat("\nGuardado: intermediate/modelos.rds\n")
cat("=== FIN ETAPA 05 ===\n")

# =============================================================================
# DECISIONES METODOLÓGICAS
# =============================================================================
# 1. M0/M1/M2 incorporan sexo, edad y educación como controles fijos de nivel
#    2 desde el modelo base. Espinosa-Rada, Bargsted y Ortiz Ruiz (2026), cuyo
#    diseño esta tesis sigue para H1, condicionan explícitamente los
#    coeficientes aleatorios (intercepto y pendiente de homogeneidad) por
#    atributos del encuestado: sexo, edad y nivel educativo, entre otros
#    (ecuaciones 2 y 3 de ese artículo). La versión anterior de M0/M1 solo
#    llevaba SP_ip y TamGrupo_p, dejando el intercepto sin condicionar. En M2
#    educ_sc entra vía la interacción con SH_ip_sc, por lo que no se duplica
#    como término separado.
# 2. H2 queda solo con Div_Red. Orient_ego (ex Comp_Red) sale de la hipótesis
#    porque Dim2 mide orientación sectorial (técnico-productivo vs.
#    administrativo-cuidado), un eje bipolar sin jerarquía normativa: no
#    admite una hipótesis direccional del tipo "a menor origen, menor
#    orientación". Se conserva como modelo descriptivo, explícitamente
#    etiquetado como NO hipótesis.
# 3. H3 se estima como cuatro modelos paralelos (uno por comunidad) en vez de
#    un modelo único, porque los shares suman 1 y meterlos juntos como
#    predictores genera colinealidad mecánica. El contraste crítico es
#    SP_red_ego (homofilia de prestigio ego-red): sin ese control, H3 quedaría
#    parcialmente acoplada a H1, ya que si las redes son homófilas en
#    habilidades el perfil de comunidades de la red se parecerá al de ego de
#    forma casi mecánica. LIMITACIÓN A DECLARAR: el share de comunidad de la
#    red está acotado por la composición de las 27 posiciones del generador,
#    lo que comprime el rango de variación de forma distinta para cada
#    comunidad; no tiene solución con el instrumento disponible.
#    PENDIENTE (auditoría 18-ago-2026): los 4 shares son datos
#    composicionales (suman 1). Cuatro svyglm independientes no respetan esa
#    restricción; los errores estándar y p-valores actuales son válidos para
#    exploración, pero antes de reportar como resultado final de H3 hay que
#    pasar por regresión Dirichlet o transformación log-ratio (ILR/ALR).
# 4. Se ejecuta un chequeo de colinealidad ANTES de H4, con umbral |r|=0.7
#    entre cierre_blando y Div_Red. Si se supera, ambos deben reportarse en
#    modelos separados. El chequeo emite warning explícito, no falla en
#    silencio.
# 5. H4 se reporta con especificación lineal como principal y cuadrática como
#    robustez. Otero, Völker y Rözer (2021) documentan una forma de U en la
#    segregación de redes chilenas a lo largo de la distribución de clases; si
#    el cierre estructural sigue ese patrón, el término cuadrático debería ser
#    positivo y significativo. ISEI de origen se centra antes de elevar al
#    cuadrado, para reducir colinealidad entre el término lineal y el
#    cuadrático.
# 6. RETIRADA (20-ago-2026, ver Decisión 14). Especificaba que H4b usaba
#    share_com4_ego continua como moderadora en vez de la comunidad dominante
#    categórica, por baja prevalencia de esa comunidad como dominante (1 de
#    978 egos). Ya no aplica: H4b se retiró del pipeline. Se mantiene el
#    número de decisión para no romper referencias cruzadas de otros
#    documentos (README.md, informes previos).
# 7. Se elimina el modelo de ingreso (H4 original del Informe 3). Decisión del
#    investigador: esa pregunta pasa a otra investigación. El Informe 3 ya la
#    trataba como exploratoria por las advertencias de Mouw (2003) sobre
#    selección vs. efecto de red y de Franzen y Hangartner (2006) sobre que
#    las redes operan principalmente vía calidad del empleo y no vía retornos
#    salariales directos.
# 8. NUEVO (19-ago-2026). SH_ip_sc ahora se calcula desde SH_ip_red (distancia
#    geodésica en la red de complementariedad phi), no desde SH_ip
#    (coordenadas del CA). Se mantiene el nombre de columna SH_ip_sc sin
#    cambios para no romper 06_visualizaciones.R ni 07_lectura_resultados.R,
#    que ya leen esa columna desde modelos.rds -- solo cambia la fuente del
#    dato, no la interfaz aguas abajo. SH_ip (CA) queda disponible en
#    base_larga por si se necesita de respaldo, pero ya no entra a M0/M1/M2.
#    Motivo del cambio: con SH_ip la interacción de H1b con educación no era
#    significativa (p=0.120); con SH_ip_red sí lo es -- CONFIRMADO 19-ago-2026,
#    corrida completa de M0-M2: SH_ip_sc:educ_sc = -0.061*** (no -0.227, que
#    era una reconstrucción aproximada de robustez/R5, con variables sin
#    escalar y sin pendiente aleatoria -- ver Decisión 6 de R5 y Decisión 10
#    de 04_indicadores_red.R).
# 9. NUEVO (20-ago-2026). H2 y H4 pasan a compartir exactamente el mismo
#    bloque de controles individuales (educ_f + sexo + edad), con ISEI_orig_hat
#    como IV focal en ambas -- ya era el caso antes de este cambio; lo único
#    que se modifica es la forma funcional de educ, que pasa de continua a
#    factor no ordenado (educ_f), con el nivel más bajo como referencia. Esto
#    responde a que la relación entre origen de clase y composición de red
#    puede no ser lineal en los niveles educativos (mismo criterio ya usado
#    en tesis.pdf para justificación de violencia con datos ELSOC). H1/H1b
#    NO se tocan: mantienen educ_sc continua estandarizada, porque ahí educ
#    entra como moderador en una interacción, no como control aditivo, y el
#    diseño de Espinosa-Rada et al. (2026) que se sigue para H1 especifica
#    la forma continua. H3 tampoco se toca en este cambio: sigue con educ
#    continua -- queda como inconsistencia declarada pendiente de decisión,
#    no un descuido (ver README.md / conversación 20-ago-2026).
# 10. NUEVO (20-ago-2026). Se agregan H2_div_red y H4_lineal_red: mismas
#     hipótesis, sin Ext/Rango_P/Estatus_Max como controles. Motivo: esas
#     tres son variables de la RED de ego, del mismo nivel que las DV
#     (Div_Red, cierre_blando), no atributos previos como educ/sexo/edad. Si
#     el origen de clase opera sobre la composición de la red PRECISAMENTE a
#     través de cuánto se extiende o cuán alto llega en prestigio, controlar
#     por ellas resta ese canal (sesgo de "bad control" / control por
#     mediador). Rango_P y Estatus_Max además correlacionan 0.685 y 0.637 con
#     Div_Red (ver mat_cor) -- son parientes cercanos de la propia DV, no
#     confusores externos a ella. No se reemplazan los modelos originales:
#     se reportan ambas especificaciones para poder comparar si el resultado
#     de ISEI_orig_hat depende de esta decisión de control. PENDIENTE: decidir
#     cuál va al cuerpo principal de la tesis y cuál a anexo de robustez.
# 11. NUEVO (20-ago-2026). Escalonamiento de controles para H2 y H4: se agregan
#     de a uno (educ_f -> sexo/edad -> Ext -> Rango_P -> Estatus_Max) sobre una
#     muestra fija (complete cases del modelo completo), para ver en qué paso
#     exacto se mueve el coeficiente y el p-valor de ISEI_orig_hat, en vez de
#     solo comparar el modelo completo contra el reducido de la Decisión 10.
#     Rango_P y Estatus_Max entran en pasos separados porque correlacionan
#     0.945 entre sí -- juntarlos en un mismo paso no permite saber a cuál
#     de los dos atribuir el cambio.
# 12. NUEVO (20-ago-2026). educ_f (10 niveles) se reemplaza por educ_f5 (5
#     niveles colapsados) en H2, H4 y el escalonamiento de controles.
#     Motivo: table(df_ego$educ_f) mostró que la categoría de referencia
#     ("Sin estudios") tenía n=3, y dos categorías más n<30 (técnica superior
#     incompleta, n=27; posgrado, n=18). Toda comparación contra una
#     referencia de n=3 tiene un error estándar artificialmente inflado, lo
#     que puede producir apariencia de "sin efecto de educación" (como se vio
#     en H4) sin que eso refleje ausencia de efecto real -- es un problema de
#     potencia estadística, no de especificación. educ_f5 colapsa a: Básica o
#     menos (n=87, referencia), Media (n=473), Técnica superior (n=157),
#     Universitaria incompleta (n=37), Universitaria completa o posgrado
#     (n=224) -- ningún grupo con n<37. Los cortes siguen las categorías
#     naturales de Q40 (básica/media/técnica/universitaria) y el quiebre
#     sustantivo observado en la corrida anterior con 10 niveles (la
#     significancia en H2 aparecía justo al completar estudios
#     post-secundarios). H1/H1b y H3 NO se tocan (ver Decisión 9).
# 13. NUEVO (20-ago-2026). Con educ_f5, el escalonamiento (tabla_h2_escalon/
#     tabla_h4_escalon) confirma el patrón visto con educ_f de 10 niveles:
#     ISEI_orig_hat cae de b=0.00855*** (paso 1, sin controles) a b=0.00104
#     n.s. (paso 2, +educ_f5) en H2 -- caída ~88% -- y de b=-0.00076*** a
#     b=-0.000277 (p=0.069) en H4 -- caída ~64%. Prácticamente idéntico a las
#     caídas de ~89%/~66% con 10 niveles. Esto descarta que la mediación
#     observada fuera un artefacto de la categorización anterior: con la
#     medición de educación corregida (categorías con n suficiente), el
#     patrón de mediación vía educación se mantiene.
# 14. NUEVO (20-ago-2026). H4b (interacción ISEI_orig_hat × share_com4_ego)
#     se retira del pipeline a pedido del investigador. Ya no se ajusta, no
#     se imprime, y no se guarda en modelos.rds. Motivo declarado: no es una
#     decisión metodológica (el modelo no tenía errores) sino de alcance --
#     H4b no forma parte del cuerpo de hipótesis que se reporta. Ver
#     Decisión 6 (retirada) para el detalle de la especificación que tenía.
#     06_visualizaciones.R y 07_lectura_resultados.R deben actualizarse en
#     consecuencia: quitar Figura 7, la columna H4b de la tabla modelsummary,
#     y el PASO 6 completo.
# =============================================================================
