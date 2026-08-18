# =============================================================================
# 01_preprocesar_encuesta.R
# Etapa 1 del pipeline: preprocesar la encuesta Fondecyt.
# Tesis Magíster en Sociología — PUC Chile | Trajan Pirkovic Palma
#
# AUTOCONTENIDO: no depende de ningún objeto en memoria. Lee únicamente
# data_fondecyt.xlsx y escribe su salida a disco (intermediate/encuesta_prep.rds).
#
# ACCIONES QUE EJECUTA ESTE SCRIPT, EN ORDEN:
#   1.  Lee la hoja "datos" de la encuesta.
#   2.  Recodifica Q40 (texto libre) a educ (entero ordinal 1-10).
#   3.  Crea edad y weight desde Q70 y weight_m.
#   4.  Convierte a factor todas las columnas etiquetadas (labelled).
#   5.  Construye isco_ego4 (ISCO-08 del ego, con respaldo) e isco_padre4
#       (ISCO-08 del padre/sostenedor).
#   6.  Construye ingreso_imp y log_ingreso desde Q4510/Q4511.
#   7.  Copia class16_r / class_oesch8_r2 a oesch16 / oesch8 SIN recalcular.
#   8.  Recodifica las 27 columnas del GP a 7 tramos fijos.
#   9.  Filtra la muestra a quienes tienen isco_ego4 no-NA.
#   10. Guarda el resultado en intermediate/encuesta_prep.rds.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(haven)
  library(labelled)
})

# ── CONFIGURACIÓN DE RUTAS ────────────────────────────────────────────────────
FONDECYT_DIR     <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data/fondecyt"
INTERMEDIATE_DIR <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output/intermediate"
dir.create(INTERMEDIATE_DIR, showWarnings = FALSE, recursive = TRUE)

# ── ACCIÓN 2 (insumo): tabla de conversión tramo de ingreso -> punto medio ───
# Q4511 viene en 16 tramos (1 a 16); se necesita un monto en pesos para poder
# tomar log() más adelante. Este vector fijo mapea cada tramo a su punto medio.
rangos_ingreso <- c(
  `1`=100000,  `2`=300000,  `3`=500000,  `4`=700000,  `5`=900000,
  `6`=1100000, `7`=1300000, `8`=1500000, `9`=1700000, `10`=1900000,
  `11`=2100000,`12`=2300000,`13`=2500000,`14`=2700000,`15`=2900000,
  `16`=3200000
)

gp_vars <- paste0("Q0", sprintf("%03d", 101:127))  # Q0101, Q0102, ..., Q0127

# ── ACCIÓN 8 (función): recodificación del GP a 7 tramos fijos ─────────────
# El número de conocidos crudo (0, 1, 2, 3, ..., 30+) se agrupa en tramos
# porque la distribución cruda es muy sesgada (unos pocos "súper-conectores"
# distorsionarían los modelos). Tramos: 0 -> 0 | 1 -> 1 | 2-4 -> 3 |
# 5-7 -> 6 | 8-10 -> 9 | 11-15 -> 13 | 16+ -> 16.
recode_gp <- function(x) {
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  case_when(
    x_num == 0                 ~ 0L,
    x_num == 1                 ~ 1L,
    x_num >= 2  & x_num <= 4   ~ 3L,
    x_num >= 5  & x_num <= 7   ~ 6L,
    x_num >= 8  & x_num <= 10  ~ 9L,
    x_num >= 11 & x_num <= 15  ~ 13L,
    x_num >= 16                ~ 16L,
    TRUE                       ~ NA_integer_
  )
}

# ── ACCIÓN 1: leer la encuesta ────────────────────────────────────────────────
raw <- read_excel(file.path(FONDECYT_DIR, "data_fondecyt.xlsx"), sheet = "datos") |>

  # ── ACCIÓN 2: recodificar Q40 (texto libre de nivel educativo) a `educ` ────
  # Se busca con grepl() la presencia de cada combinación de palabras clave
  # (nivel + estado incompleta/completa) y se asigna un entero fijo creciente.
  mutate(
    educ = case_when(
      grepl("Sin estudios", Q40, fixed = TRUE)               ~ 1L,
      grepl("sica", Q40)  & grepl("incompleta", Q40)          ~ 2L,
      grepl("sica", Q40)  & grepl("completa",   Q40)          ~ 3L,
      grepl("Media", Q40) & grepl("incompleta", Q40)          ~ 4L,
      grepl("Media", Q40) & grepl("completa",   Q40)          ~ 5L,
      grepl("cnica", Q40) & grepl("incompleta", Q40)          ~ 6L,
      grepl("cnica", Q40) & grepl("completa",   Q40)          ~ 7L,
      grepl("Universitaria", Q40) & grepl("incompleta", Q40)  ~ 8L,
      grepl("Universitaria", Q40) & grepl("completa",   Q40)  ~ 9L,
      grepl("posgrado", Q40, ignore.case = TRUE)              ~ 10L,
      TRUE                                                     ~ NA_integer_
    ),
    # ── ACCIÓN 3: edad y ponderador muestral ──────────────────────────────
    edad   = as.numeric(Q70),
    weight = as.numeric(weight_m),
    # ── ACCIÓN 4: pasar todas las columnas labelled (SPSS/Stata) a factor ──
    # Esto permite leer sus categorías como texto (ej. Q68 = "Hombre"/"Mujer")
    # en vez de un código numérico interno.
    across(where(is.labelled), as_factor)
  ) |>
  mutate(
    sexo = as.character(Q68),

    # ── ACCIÓN 5a: ISCO-08 del ego, con respaldo ───────────────────────────
    # Se prioriza isco_mainjob (ya codificado por el equipo Fondecyt); si esa
    # celda es NA, se reemplaza por Q4402 (ocupación auto-reportada) con
    # if_else(). Se recorta a 4 dígitos con substr() para calzar con ESCO e ISEI.
    isco_ego  = as.integer(isco_mainjob),
    isco_ego  = if_else(is.na(isco_ego), as.integer(Q4402), isco_ego),
    isco_ego4 = as.integer(substr(as.character(isco_ego), 1, 4)),

    # ── ACCIÓN 5b: ISCO-08 del padre/sostenedor (Q6403) ────────────────────
    # Es la base de la clase de origen, variable central de H2.
    isco_padre4 = as.integer(substr(as.character(Q6403), 1, 4)),

    # ── ACCIÓN 6: ingreso imputado y su logaritmo ─────────────────────────
    # Se prioriza Q4510 (monto exacto, si es > 0); si falta, se usa Q4511
    # (tramo 1-16) convertido al punto medio vía la tabla rangos_ingreso.
    Q4511_num   = suppressWarnings(as.numeric(as.character(Q4511))),
    ingreso_imp = case_when(
      !is.na(Q4510) & Q4510 > 0 ~ as.numeric(Q4510),
      !is.na(Q4511_num)          ~ rangos_ingreso[as.character(Q4511_num)],
      TRUE                       ~ NA_real_
    ),
    log_ingreso = if_else(ingreso_imp > 0, log(ingreso_imp), NA_real_),

    # ── ACCIÓN 7: clase Oesch — copia directa, SIN recalcular ─────────────
    # Se toma tal cual desde las columnas ya categorizadas por el equipo del
    # Fondecyt. No se llama a la librería 'oesch' ni se deriva de ISCO+
    # situación en el empleo localmente.
    oesch16 = as.character(class16_r),
    oesch8  = as.character(class_oesch8_r2)
  ) |>
  # ── ACCIÓN 8: aplicar recode_gp() a las 27 columnas del GP ──────────────
  # Genera 27 columnas nuevas con sufijo "_r" (ej. Q0101 -> Q0101_r).
  mutate(across(all_of(gp_vars), recode_gp, .names = "{.col}_r"))

gp_r_vars <- paste0(gp_vars, "_r")

# ── ACCIÓN 9: filtrar la muestra a quienes tienen ISCO de ego válido ─────────
# Sin código ISCO no hay forma de ubicar la ocupación en el espacio de
# habilidades (etapa 03). Este filtro es el que reduce la muestra de 1.251 a
# aprox. 1.028 (los descartados son inactivos, sin dato, o código no mapeable).
df_analitica <- raw |> filter(!is.na(isco_ego4))
cat("n analítica:", nrow(df_analitica), "/ total:", nrow(raw), "\n")
cat("Cobertura oesch8 (no-NA):", sum(!is.na(df_analitica$oesch8)),
    "de", nrow(df_analitica), "\n")
cat("Tabla oesch8 (categorías del equipo Fondecyt):\n")
print(table(df_analitica$oesch8, useNA = "ifany"))

# ── ACCIÓN 10: guardar salida ─────────────────────────────────────────────────
saveRDS(
  df_analitica |> select(ID, educ, edad, weight, sexo, isco_ego4, isco_padre4,
                          ingreso_imp, log_ingreso, oesch16, oesch8,
                          all_of(gp_vars), all_of(gp_r_vars)),
  file.path(INTERMEDIATE_DIR, "encuesta_prep.rds")
)
cat("\nGuardado: intermediate/encuesta_prep.rds (", nrow(df_analitica), "filas )\n")
cat("=== FIN ETAPA 01 ===\n")
