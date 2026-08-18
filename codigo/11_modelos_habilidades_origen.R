# =============================================================================
# 18_modelos_habilidades_origen.R
#
# Testea H2/H3 extendido (clase de origen -> composicion de habilidades) en
# dos planos, usando los indicadores YA construidos en 04_indicadores_red.R
# (df_ego.rds), sin recalcular nada que ya exista en el pipeline:
#   A. Ocupacion propia de ego   (share_com*_ego, Div_ego)
#   B. Red personal de ego       (share_com*_red, Div_Red, Rango_P)
#
# Para cada plano se corre el contraste de mediacion: modelo TOTAL
# (ISEI_orig_hat solo) vs. modelo DIRECTO (controlando por el/los mediadores
# de estatus correspondientes), siguiendo la discusion de sesion 2026-08-17.
#
# Version 2: PONDERADO (svyglm con weight) y con Rango_P como UNICO control
# de prestigio de red en el modelo directo (se descarta SP_red_ego para esta
# tabla especifica; ver Decision D5).
#
# NOTA: reemplaza 16_imputar_comunidades_ego.R y 17_modelos_habilidades_
# origen.R (reconstruian desde cero objetos que ya viven en
# imputacion_comunidades.rds / df_ego.rds) y la version 1 de este mismo
# script (sin ponderar, con dos versiones de control de red).
#
# Script AUTOCONTENIDO salvo por su dependencia declarada de df_ego.rds
# (salida de 04_indicadores_red.R, que a su vez requiere 01, 02/03, 13, 15
# ya corridos).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr); library(broom)
  library(survey)
})

# =============================================================================
# 0. RUTAS Y PARAMETROS
# =============================================================================

RUTA_SCRIPTS_BASE <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts"
INTERMEDIATE_DIR  <- file.path(RUTA_SCRIPTS_BASE, "output/intermediate")
po <- function(f) file.path(RUTA_SCRIPTS_BASE, "output/modelos_habilidades_origen", f)
dir.create(po(""), showWarnings = FALSE, recursive = TRUE)

RUTA_DF_EGO <- file.path(INTERMEDIATE_DIR, "df_ego.rds")

stopifnot(
  "Falta df_ego.rds. Correr 04_indicadores_red.R primero." =
    file.exists(RUTA_DF_EGO)
)

# Nombres sustantivos de las 4 comunidades (mismo orden y particion que en
# 13_imputar_comunidades_ocupacion.R: comunidad 1=Direccion_servicio n=34,
# 2=Tecnico_manual n=35, 3=Analitico_digital_simbolico n=31,
# 4=Bio_ambiental_legal n=10). Si el n de comunidades detectadas cambiara,
# el stopifnot de la seccion 1 corta la ejecucion antes de asignar mal los
# nombres.
NOMBRES_COMUNIDAD <- c("Direccion_servicio", "Tecnico_manual",
                        "Analitico_digital_simbolico", "Bio_ambiental_legal")

shannon_entropy <- function(...) {
  p <- c(...)
  p <- p[p > 0]
  -sum(p * log(p))
}

# =============================================================================
# 1. CARGA Y VERIFICACION
# =============================================================================

df_ego <- readRDS(RUTA_DF_EGO)

share_cols_ego <- paste0("share_com", 1:4, "_ego")
share_cols_red <- paste0("share_com", 1:4, "_red")

stopifnot(
  "df_ego.rds no tiene exactamente 4 comunidades (share_com1..4_ego). Revisar imputacion_comunidades.rds." =
    all(share_cols_ego %in% names(df_ego)),
  "df_ego.rds debe tener share_com1..4_red" =
    all(share_cols_red %in% names(df_ego)),
  "df_ego.rds debe tener ISEI_orig_hat, isei_ego_hat, Rango_P, educ, sexo, edad, weight" =
    all(c("ISEI_orig_hat", "isei_ego_hat", "Rango_P",
          "educ", "sexo", "edad", "weight") %in% names(df_ego))
)

df_ego <- df_ego %>%
  rename(!!!setNames(share_cols_ego, paste0(NOMBRES_COMUNIDAD, "_ego"))) %>%
  rename(!!!setNames(share_cols_red, paste0(NOMBRES_COMUNIDAD, "_red")))

cols_ego <- paste0(NOMBRES_COMUNIDAD, "_ego")
cols_red <- paste0(NOMBRES_COMUNIDAD, "_red")

df_ego <- df_ego %>%
  rowwise() %>%
  mutate(
    entropia_ego = shannon_entropy(c_across(all_of(cols_ego))),
    entropia_red = shannon_entropy(c_across(all_of(cols_red)))
  ) %>%
  ungroup()

cat(sprintf("n total df_ego: %d\n", nrow(df_ego)))
cat(sprintf("cor(ISEI_orig_hat, isei_ego_hat) = %.3f\n",
            cor(df_ego$ISEI_orig_hat, df_ego$isei_ego_hat, use = "complete.obs")))

# Helper: corre TOTAL vs. DIRECTO con svyglm sobre un diseno muestral ya
# construido, y devuelve el coeficiente de ISEI_orig_hat de cada uno.
correr_mediacion_svy <- function(vars_dv, controles_directo, disenio) {
  map_dfr(vars_dv, function(v) {
    f1 <- as.formula(paste(v, "~ ISEI_orig_hat + educ + sexo + edad"))
    f2 <- as.formula(paste(v, "~ ISEI_orig_hat +", controles_directo, "+ educ + sexo + edad"))
    bind_rows(
      tidy(svyglm(f1, design = disenio)) %>% filter(term == "ISEI_orig_hat") %>%
        mutate(modelo = "total (sin control)", variable = v),
      tidy(svyglm(f2, design = disenio)) %>% filter(term == "ISEI_orig_hat") %>%
        mutate(modelo = paste0("directo (", controles_directo, ")"), variable = v)
    )
  })
}

# =============================================================================
# 2. PLANO A: OCUPACION PROPIA DE EGO
# =============================================================================
# TOTAL: ISEI_orig_hat solo (+ controles de nivel 2 estandar).
# DIRECTO: se agrega isei_ego_hat como mediador de movilidad de estatus.

cat("\n=== A. PLANO OCUPACION PROPIA DE EGO ===\n")

base_ocup <- df_ego %>%
  filter(!is.na(ISEI_orig_hat), !is.na(isei_ego_hat), !is.na(educ), !is.na(sexo),
         !is.na(edad), !is.na(weight))
cat(sprintf("  n: %d\n", nrow(base_ocup)))

disenio_ocup <- svydesign(ids = ~1, weights = ~weight, data = base_ocup)

vars_dv_ocup <- c(cols_ego, "entropia_ego", "Div_ego")
modelos_ocup <- correr_mediacion_svy(vars_dv_ocup, "isei_ego_hat", disenio_ocup)

cat("\n  Coeficiente de ISEI_orig_hat, plano ocupacion propia (svyglm, ponderado):\n")
print(modelos_ocup %>% select(variable, modelo, estimate, std.error, p.value), n = Inf)
write_csv(modelos_ocup, po("modelos_ocupacion_propia.csv"))

# =============================================================================
# 3. PLANO B: RED PERSONAL DE EGO
# =============================================================================
# TOTAL: ISEI_orig_hat solo (+ controles de nivel 2).
# DIRECTO: se agregan isei_ego_hat (estatus propio) Y Rango_P (prestigio
# clasico de la red, indicador establecido en la literatura de generador de
# posiciones) como mediadores. Se descarta SP_red_ego para esta tabla
# especifica porque su formula ya incorpora isei_ego_hat, lo que generaria
# solapamiento por diseno (no colinealidad accidental) si ambos entran juntos
# al mismo modelo (ver Decision D5 y discusion de sesion 2026-08-17).

cat("\n=== B. PLANO RED PERSONAL DE EGO ===\n")

base_red <- df_ego %>%
  filter(!is.na(ISEI_orig_hat), !is.na(isei_ego_hat), !is.na(Rango_P),
         !is.na(educ), !is.na(sexo), !is.na(edad), !is.na(weight), Ext > 0)
cat(sprintf("  n (Ext > 0, mediadores no-NA): %d\n", nrow(base_red)))

disenio_red <- svydesign(ids = ~1, weights = ~weight, data = base_red)

vars_dv_red <- c(cols_red, "entropia_red", "Div_Red")
modelos_red <- correr_mediacion_svy(vars_dv_red, "isei_ego_hat + Rango_P", disenio_red)

cat("\n  Coeficiente de ISEI_orig_hat, plano red personal (svyglm, ponderado):\n")
print(modelos_red %>% select(variable, modelo, estimate, std.error, p.value), n = Inf)
write_csv(modelos_red, po("modelos_red_personal.csv"))

cat("\n=== FIN: MODELOS HABILIDADES-ORIGEN (OCUPACION PROPIA Y RED) ===\n")

# =============================================================================
# DECISIONES METODOLOGICAS
# =============================================================================
# D1. Este script NO recalcula shares_rm, gp_shares, ni el ISEI de ego u
#     origen: todo se lee de df_ego.rds (salida de 04_indicadores_red.R). La
#     version anterior (16, 17) reconstruia esos objetos desde cero de forma
#     redundante; quedan archivadas.
# D2. Bio_ambiental_legal se mantiene dentro del set principal de DV (no se
#     separa en indicador binario), por decision explicita de Trajan en
#     sesion 2026-08-17, con el matiz ya documentado sobre su limitada
#     variacion en el GP.
# D3. share_com*_red y Rango_P en df_ego.rds estan ponderados por gp_r_vars
#     (conteos recodificados en tramos), no por conteos crudos, siguiendo la
#     convencion ya fijada en 04_indicadores_red.R (Decision 6 de ese
#     script).
# D4. Los controles de nivel 2 (educ, sexo, edad) se incluyen en todos los
#     modelos desde el TOTAL, siguiendo la convencion ya fijada para M0/M1/M2
#     (Espinosa-Rada, Bargsted y Ortiz-Ruiz, 2026).
# D5. Se usa Rango_P y NO SP_red_ego como control de prestigio de red en este
#     script, por decision de Trajan en sesion 2026-08-17: Rango_P es el
#     indicador clasico mas conocido en la literatura de generador de
#     posiciones, y ademas evita el solapamiento por construccion que se da
#     al meter SP_red_ego junto con isei_ego_hat en el mismo modelo (SP_red_ego
#     ya incorpora isei_ego_hat en su propia formula: promedio ponderado de
#     -|ISEI_ego - ISEI_p|). SP_red_ego sigue siendo el control establecido
#     para los modelos de H3 propiamente dichos (correspondencia de
#     comunidades red-ego), donde no se usa junto a isei_ego_hat en el mismo
#     modelo; no se reemplaza ahi, solo se evita aqui.
# D6. Los modelos pasan de lm() a svyglm() con svydesign(ids=~1,
#     weights=~weight), siguiendo la convencion de ponderacion muestral ya
#     usada en el resto de la tesis (Modelos A/B1/B2). Notar que
#     svydesign(ids=~1, ...) asume muestreo aleatorio simple ponderado, sin
#     estratos ni conglomerados: si el diseno muestral de Fondecyt tiene
#     estratificacion o conglomerados declarados en otra parte del pipeline,
#     hay que replicar esa misma especificacion aqui en vez de ids=~1, para
#     que los errores estandar sean consistentes con el resto de la tesis.
# =============================================================================
