################################################################################
# 00_config.R   PIPELINE FINAL DE LA TESIS
#
# Rutas, paquetes, parametros y funciones comunes. Lo cargan con source() todos
# los scripts del pipeline (01 a 08). No lee ni escribe datos.
#
# Tesis Magister en Sociologia, PUC Chile | Trajan Pirkovic Palma
################################################################################

suppressPackageStartupMessages({
  library(tidyverse); library(readxl); library(haven); library(survey); library(igraph)
})
select <- dplyr::select   # survey carga MASS, que enmascara select()
options(readr.show_progress = FALSE)   # sin barras de progreso de read_csv/write_csv en el log

# ---- Rutas (relativas: el directorio de trabajo debe ser la carpeta pipeline_final) ----
if (!file.exists("00_config.R")) stop("Fijar el directorio de trabajo en la carpeta pipeline_final (lo hace run_pipeline.R; para un script suelto: setwd() a esa carpeta)")
DIR_PIPE  <- normalizePath(getwd())
RUTA_SCRIPTS_BASE <- dirname(DIR_PIPE)
DIR_DATA  <- file.path(RUTA_SCRIPTS_BASE, "data")          # insumos externos (ver README)
DIR_OUT   <- file.path(RUTA_SCRIPTS_BASE, "output_final")
DIR_INT   <- file.path(DIR_OUT, "intermedios")     # bases que pasan de un script a otro
DIR_TAB   <- file.path(DIR_OUT, "tablas")
DIR_FIG   <- file.path(DIR_OUT, "figuras")
for (d in c(DIR_OUT, DIR_INT, DIR_TAB, DIR_FIG)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

ARCH <- list(
  fondecyt     = file.path(DIR_DATA, "fondecyt", "data_fondecyt.xlsx"),
  casen        = file.path(DIR_DATA, "casen", "casen_2024.RData"),
  isei         = file.path(DIR_DATA, "isei", "isei08_ganzeboom2010.csv"),        # enteros DIGCLASS
  esco_dir     = file.path(DIR_DATA, "esco"),
  mapha_dir    = file.path(DIR_DATA, "mapha", "v206"),
  # Salidas del pipeline anterior: SOLO para validar (VALIDAR_CONTRA_ANTERIOR);
  # el pipeline no las necesita para correr
  v3_ego       = file.path(RUTA_SCRIPTS_BASE, "output_paralelo_v3", "datos", "base_ego_v3.rds"),
  v3_diadas    = file.path(RUTA_SCRIPTS_BASE, "output_paralelo_v3", "datos", "base_diadas_v3.rds"),
  v2_comunidad = file.path(RUTA_SCRIPTS_BASE, "output_mapha", "comunidades_por_ocupacion_mapha", "imputacion_comunidades_mapha.rds")
)

# ---- Parametros y decisiones metodologicas ----------------------------------------
VALIDAR_CONTRA_ANTERIOR <- TRUE   # FALSE: no lee nada fuera de data/ y output_final/

# Correcciones de codigo ISCO (aplicadas al universo CASEN y a la ocupacion de ego).
# Las de CIUO-08.CL corrigen la desagregacion chilena; 3221 -> 5321 es una
# SUSTITUCION de perfil de habilidades: 3221 (tecnicos en enfermeria, TENS) no
# tiene habilidades registradas en ESCO ni en MAPHA, y se le imputa el perfil de
# 5321 (auxiliares de enfermeria). La clase y el ISEI siguen siendo los de 3221.
CORRECCIONES_ISCO <- tribble(
  ~isco_orig, ~isco_corr, ~motivo,
  2241L, 2261L, "CIUO-08.CL desagrega 224x; ISCO-08 usa 226x (dentistas)",
  2242L, 2262L, "CIUO-08.CL desagrega 224x; ISCO-08 usa 226x (farmaceuticos)",
  2243L, 2263L, "CIUO-08.CL desagrega 224x; ISCO-08 usa 226x (salud e higiene laboral)",
  2244L, 2264L, "CIUO-08.CL desagrega 224x; ISCO-08 usa 226x (kinesiologos)",
  2245L, 2265L, "CIUO-08.CL desagrega 224x; ISCO-08 usa 226x (nutricionistas)",
  2246L, 2266L, "CIUO-08.CL desagrega 224x; ISCO-08 usa 226x (fonoaudiologos)",
  2249L, 2269L, "CIUO-08.CL desagrega 224x; ISCO-08 usa 226x (otros de salud)",
  3215L, 3214L, "Laboratoristas dentales: grupo ISCO-08 3214",
  3231L, 3230L, "Terapias complementarias: grupo ISCO-08 3230",
  3221L, 5321L, "Sustitucion de perfil: 3221 sin habilidades en ESCO/MAPHA")
SEMILLA_LEIDEN  <- 2025      # se fija inmediatamente antes de cluster_leiden()
SEMILLA_NULO    <- 20261009  # se fija inmediatamente antes de r2dtable()
R_NULO          <- 300       # tablas aleatorias de la linea base N3
RCA_UMBRAL      <- 1         # categoria efectiva de una ocupacion si RCA > 1
PHI_MIN_ARISTA  <- 0         # se conservan las aristas con phi > 0
MIN_CONOCIDOS_SENS <- 3      # robustez: egos con 3 o mas conocidos

# ISEI y clase de ego con el codigo ISCO del equipo Fondecyt. El codigo
# corregido (3221 -> 5321, sin ocupaciones en ESCO) se usa SOLO para las
# habilidades, igual que en la posicion Q0116. FALSE reproduce las cifras de la
# presentacion del 7-oct-2026 (que usaban el ISEI de 5321 para 17 egos TENS).
ISEI_CON_CODIGO_ORIGINAL <- TRUE

# Tramos del generador de posiciones (conteo -> punto del tramo)
TRAMOS_GP <- function(x) {
  x <- suppressWarnings(as.numeric(as.character(x)))
  case_when(x == 0 ~ 0L, x == 1 ~ 1L, x >= 2 & x <= 4 ~ 3L, x >= 5 & x <= 7 ~ 6L,
            x >= 8 & x <= 10 ~ 9L, x >= 11 & x <= 15 ~ 13L, x >= 16 ~ 16L, TRUE ~ NA_integer_)
}
GP_VARS <- sprintf("Q0%03d", 101:127)

# Oesch16 -> siete clases (script del equipo Fondecyt, class_oesch7)
CLASES7 <- c("1. Higher-grade service class", "2. Lower-grade service class",
             "3. Socio-cultural (semi) professionals", "4. Clerks", "5. Small business owners",
             "6. Skilled workers", "7. Unskilled workers")
CLASES7_CORTO <- c("Higher-grade", "Lower-grade*", "Socio-cultural", "Clerks",
                   "Small business*", "Skilled workers", "Unskilled workers")
colapsar_oesch7 <- function(c16) case_when(
  c16 %in% c(1, 2, 5, 9) ~ CLASES7[1], c16 %in% c(6, 10) ~ CLASES7[2],
  c16 %in% c(13, 14)     ~ CLASES7[3], c16 %in% c(11, 12) ~ CLASES7[4],
  c16 %in% c(3, 4)       ~ CLASES7[5], c16 %in% c(7, 15)  ~ CLASES7[6],
  c16 %in% c(8, 16)      ~ CLASES7[7], TRUE ~ NA_character_)
CLASE_REF <- CLASES7[7]

OESCH16_LAB <- c("Large employers", "Self-employed professionals", "Small business owners with employees",
                 "Small business owners without employees", "Technical experts", "Technicians",
                 "Skilled manual", "Low-skilled manual", "Higher-grade managers and administrators",
                 "Lower-grade managers and administrators", "Skilled clerks", "Unskilled clerks",
                 "Socio-cultural professionals", "Socio-cultural semi-professionals", "Skilled service",
                 "Low-skilled service")

# Tramos de estatus del equipo Fondecyt (script "networks and class awareness",
# Gabriel Otero): 24 posiciones; se usan solo en la robustez R6.
TRAMOS_EQUIPO <- list(
  alto  = c("Q0101", "Q0105", "Q0107", "Q0109", "Q0114", "Q0120", "Q0123", "Q0127"),
  medio = c("Q0102", "Q0104", "Q0111", "Q0115", "Q0116", "Q0124", "Q0125", "Q0126"),
  bajo  = c("Q0106", "Q0108", "Q0112", "Q0113", "Q0117", "Q0118", "Q0121", "Q0122"))
TRAMO_DE_CLASE <- c(setNames(rep("alto", 1), CLASES7[1]), setNames(rep("medio", 4), CLASES7[2:5]),
                    setNames(rep("bajo", 2), CLASES7[6:7]))

# Comunidades de habilidades: anclas por contenido y nombres vigentes
ANCLAS_COM <- c("S4.9", "053", "S2.3", "081")   # tomar decisiones | ciencias fisicas | gestionar informacion | agricultura
NOMBRES_COM <- c("Gestión y cuidado", "Técnico-material", "Analítico-simbólico", "Bio-ambiental-legal")

# ---- Utilidades -----------------------------------------------------------------
zs <- function(v) as.numeric(scale(v))
estrellas <- function(p) case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", p < 0.1 ~ "+", TRUE ~ "")
fnum <- function(x, d = 3) formatC(x, format = "f", digits = d, decimal.mark = ",")
titulo <- function(t) cat("\n", strrep("=", 78), "\n", t, "\n", strrep("=", 78), "\n", sep = "")
wmean <- function(x, w) { ok <- !is.na(x) & !is.na(w); sum(w[ok] * x[ok]) / sum(w[ok]) }
wsd   <- function(x, w) { ok <- !is.na(x) & !is.na(w); m <- wmean(x, w); sqrt(sum(w[ok] * (x[ok] - m)^2) / sum(w[ok])) }
tidy_svy <- function(m, modelo, muestra = "principal") {
  co <- coef(summary(m))
  tibble(modelo, muestra, termino = rownames(co), est = co[, 1], se = co[, 2], p = co[, 4], n = nrow(m$model)) %>%
    mutate(texto = sprintf("%.3f (%.3f)%s", est, se, estrellas(p)))
}
# Cambio medio en la proporcion predicha (pp) al sumar 1 DE a la variable
pp_1de <- function(m, var = "SH_z") {
  b <- coef(m)[[var]]; eta <- m$linear.predictors; w <- m$prior.weights
  100 * sum(w * (plogis(eta + b) - plogis(eta))) / sum(w)
}
chequeo <- function(etiqueta, obtenido, esperado, tol = 1e-3) {
  ok <- isTRUE(all.equal(as.numeric(obtenido), as.numeric(esperado), tolerance = tol))
  cat(sprintf("  [%s] %s: obtenido %s | esperado %s\n", if (ok) "OK" else "DIFIERE", etiqueta,
              paste(round(obtenido, 4), collapse = " "), paste(round(esperado, 4), collapse = " ")))
  invisible(ok)
}

# Coordenadas del mapa de habilidades (Figura 1). Las usan 01, 07 y 08, de modo
# que las categorias quedan en el mismo lugar en todas las figuras: Fruchterman-
# Reingold con pesos phi, 2000 iteraciones, semilla fija inmediatamente antes.
coords_mapa <- function(grafo) {
  set.seed(SEMILLA_LEIDEN)
  lay <- ggraph::create_layout(tidygraph::as_tbl_graph(grafo), layout = "fr",
                               weights = weight, niter = 2000)
  xy <- as.matrix(lay[, c("x", "y")]); rownames(xy) <- lay$name
  xy
}

# Nombres de las categorias L2 de ESCO (para etiquetas)
nombres_L2 <- function() {
  sh <- suppressWarnings(read_csv(file.path(ARCH$esco_dir, "skillsHierarchy_es.csv"),
                                  col_types = cols(.default = "c"), show_col_types = FALSE)) %>%
    filter(!is.na(`Level 2 code`), !is.na(`Level 2 preferred term`)) %>% distinct(`Level 2 code`, .keep_all = TRUE)
  n <- sh$`Level 2 preferred term`
  setNames(paste0(toupper(substr(n, 1, 1)), substr(n, 2, nchar(n))), sh$`Level 2 code`)
}

# ---- Estilo grafico ------------------------------------------------------------------
source(file.path(DIR_PIPE, "00_tema_graficos.R"), encoding = "UTF-8")
