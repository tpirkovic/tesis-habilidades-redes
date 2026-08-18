# =============================================================================
# 05_modelos.R
# Etapa 5 del pipeline: modelos estadísticos H1/H1b, H2, H3, H4 y H4b.
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
#   - H4b (nueva): interacción con el peso bio-ambiental-legal de ego.
#   - Se elimina el modelo de ingreso (H4 original, descartada del diseño).
#   - Se agrega un chequeo de colinealidad previo a la interpretación de H4.
#
# VALIDADO: corrido el 18-ago-2026 sobre base_larga.rds/df_ego.rds generados
# con el CA ponderado por RM y los fixes C2/C3/C4 ya aplicados. Resultados:
# H1 se sostiene (SH_ip significativo controlando SP_ip); H1b no (interacción
# no significativa); H2 e ISEI_orig_hat no significativo sobre Div_Red; H3 se
# sostiene en las 4 comunidades controlando SP_red_ego; H4 lineal y
# cuadrático no significativos. Ver README.md para la lectura completa.
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
    SH_ip_sc  = scale(SH_ip)[, 1],
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
  mutate(sexo = factor(sexo))

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

H2_div <- svyglm(Div_Red ~ ISEI_orig_hat + educ + sexo + edad +
                   Ext + Rango_P + Estatus_Max,
                 design = svy_ego, family = gaussian())

cat("\n=== H2: Clase de origen -> Diversidad de habilidades de la red ===\n")
print(summary(H2_div))

# descriptivo de Orient_ego (ya no es hipótesis, se reporta como descripción)
H2_orient_desc <- svyglm(Orient_ego ~ ISEI_orig_hat + educ + sexo + edad,
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

H4_lineal <- svyglm(cierre_blando ~ ISEI_orig_hat + educ + sexo + edad +
                      Ext + Rango_P + Estatus_Max,
                    design = svy_H4, family = gaussian())
print(summary(H4_lineal))

# ROBUSTEZ: especificación cuadrática. Otero, Völker y Rözer (2021) documentan
# que la segregación de redes en Chile sigue una forma de U a lo largo de la
# distribución de clases (alta en ambos extremos, baja en el centro). Si el
# cierre estructural sigue ese patrón, el término cuadrático debería ser
# significativo y positivo. Ver DECISIÓN 5.
df_H4$ISEI_orig_c  <- df_H4$ISEI_orig_hat - mean(df_H4$ISEI_orig_hat, na.rm = TRUE)
df_H4$ISEI_orig_c2 <- df_H4$ISEI_orig_c^2
svy_H4 <- svydesign(ids = ~1, weights = ~weight, data = df_H4)

H4_cuadratico <- svyglm(cierre_blando ~ ISEI_orig_c + ISEI_orig_c2 + educ + sexo + edad +
                          Ext + Rango_P + Estatus_Max,
                        design = svy_H4, family = gaussian())
cat("\n--- H4, robustez: especificación cuadrática (forma de U) ---\n")
print(summary(H4_cuadratico))

# =============================================================================
# H4b — MODERACIÓN POR PESO BIO-AMBIENTAL-LEGAL DE LA OCUPACIÓN DE EGO
# =============================================================================
# ADVERTENCIA A DECLARAR EN EL TEXTO: share_com4_ego está fuertemente
# concentrada cerca de cero (solo 1 de 978 egos tiene esa comunidad como
# dominante). La interacción se estima sobre la variable CONTINUA, no sobre
# grupos, precisamente para evitar el problema de tamaño de grupo, pero la
# potencia sigue siendo limitada. Ver DECISIÓN 6.

cat("\n=== H4b: Moderación por peso bio-ambiental-legal ===\n")
cat("\n  Distribución de share_com4_ego:\n")
print(summary(df_H4$share_com4_ego))

H4b <- svyglm(cierre_blando ~ ISEI_orig_hat * share_com4_ego + educ + sexo + edad +
                Ext + Rango_P + Estatus_Max,
              design = svy_H4, family = gaussian())
print(summary(H4b))

# =============================================================================
# GUARDAR
# =============================================================================

saveRDS(
  list(M0 = M0, M1 = M1, M2 = M2,
       H2_div = H2_div, H2_orient_desc = H2_orient_desc,
       H3_modelos = H3_modelos,
       H4_lineal = H4_lineal, H4_cuadratico = H4_cuadratico, H4b = H4b,
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
# 6. H4b usa share_com4_ego CONTINUA como moderadora, no la comunidad
#    dominante categórica. Razón: solo 1 de 978 egos tiene la comunidad
#    bio-ambiental-legal como dominante, lo que hace inviable un análisis por
#    grupos. La variable continua evita ese problema, pero la distribución
#    sigue concentrada cerca de cero y la potencia es limitada: debe
#    declararse en el texto antes de interpretar el resultado, no después.
# 7. Se elimina el modelo de ingreso (H4 original del Informe 3). Decisión del
#    investigador: esa pregunta pasa a otra investigación. El Informe 3 ya la
#    trataba como exploratoria por las advertencias de Mouw (2003) sobre
#    selección vs. efecto de red y de Franzen y Hangartner (2006) sobre que
#    las redes operan principalmente vía calidad del empleo y no vía retornos
#    salariales directos.
# =============================================================================
