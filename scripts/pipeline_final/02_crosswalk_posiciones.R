################################################################################
# 02_crosswalk_posiciones.R   PIPELINE FINAL
#
# Crosswalk unico de las 27 posiciones del generador (GP). Reemplaza a
# paralelo_v3/111.
#   - Codigo ISCO del equipo Fondecyt para clase e ISEI (crosswalk_gp27_FINAL,
#     salvo Q0116, que el equipo codifica 3221).
#   - Codigo para habilidades: el mismo, salvo codigos agregados (grupo unitario
#     mas frecuente en CASEN 2024 RM entre los que lista el equipo) y Q0116
#     (3221 no tiene ocupaciones en ESCO: se usa 5321).
#   - ISEI: tabla de Ganzeboom (2010) en enteros (DIGCLASS).
#   - Clase: Oesch16 del ISCO como asalariado (DIGCLASS), salvo comerciantes
#     (Q0111, cuenta propia con empleados); colapso a siete clases del equipo.
#   - TamGrupo_p: proporcion de ocupados RM en la posicion (CASEN 2024).
#
# Lee:     intermedios/frecuencias_casen_rm.csv, intermedios/espacio_habilidades.rds
# Salidas: intermedios/crosswalk_gp27.csv | tablas/tabla_posiciones_gp.csv
################################################################################

source("00_config.R", encoding = "UTF-8")

# Las 27 posiciones: codigo ISCO-08 del equipo Fondecyt y Oesch16 esperado
# (asalariado; Q0111 cuenta propia con empleados). El Oesch16 se recalcula con
# DIGCLASS y se verifica contra esta columna.
POSICIONES <- tribble(
  ~posicion, ~label,                                         ~isco_equipo, ~self_employed, ~oesch16,
  "Q0101", "Gerentes/directores de grandes empresas",        "1120", 0L,  9L,
  "Q0102", "Profesores de educación media o básica",         "2300", 0L, 13L,
  "Q0103", "Mecánicos de autos",                             "7231", 0L,  7L,
  "Q0104", "Vendedores de tiendas o almacenes",              "5223", 0L, 15L,
  "Q0105", "Ingenieros civiles/industriales/mecánicos",      "2140", 0L,  5L,
  "Q0106", "Cajeros de supermercado/comercio",               "5230", 0L, 12L,
  "Q0107", "Altos funcionarios del gobierno",                "1112", 0L,  9L,
  "Q0108", "Guardias de seguridad",                          "5414", 0L, 16L,
  "Q0109", "Profesores universitarios",                      "2310", 0L, 13L,
  "Q0110", "Trabajadores agrícolas (cosecha/cultivo)",       "9211", 0L,  8L,
  "Q0111", "Comerciantes y dueños de locales/tiendas",       "5221", 1L,  3L,
  "Q0112", "Repartidores de apps (Rappi/Uber/PedidosYa)",    "9621", 0L, 12L,
  "Q0113", "Conductores de camiones/pesados",                "8332", 0L,  7L,
  "Q0114", "Médicos o doctores",                             "2210", 0L, 13L,
  "Q0115", "Secretarios/as",                                 "4120", 0L, 11L,
  "Q0116", "Enfermeros/asistentes médicos TENS",             "3221", 0L, 14L,
  "Q0117", "Cocineros/as",                                   "5120", 0L, 15L,
  "Q0118", "Vendedores ambulantes",                          "9520", 0L, 16L,
  "Q0119", "Electricistas",                                  "7410", 0L,  7L,
  "Q0120", "Abogados",                                       "2611", 0L,  9L,
  "Q0121", "Obreros de construcción o fábricas",             "9300", 0L,  8L,
  "Q0122", "Aseadores de oficina/auxiliares de aseo",        "9112", 0L, 16L,
  "Q0123", "Arquitectos/as",                                 "2161", 0L,  5L,
  "Q0124", "Parvularios/as",                                 "2342", 0L, 14L,
  "Q0125", "Carabineros y policías",                         "5412", 0L, 15L,
  "Q0126", "Trabajadores sociales",                          "2635", 0L, 14L,
  "Q0127", "Contadores/as",                                  "3313", 0L, 10L)
TAM_COMP <- list(Q0114 = "221", Q0105 = "214", Q0102 = c("2330", "2341"), Q0119 = "741", Q0121 = c("931", "932"))
CANDIDATOS_HAB <- list(Q0114 = c("2211", "2212"), Q0105 = c("2141", "2142", "2144"), Q0102 = c("2330", "2341"),
                       Q0119 = c("7411", "7412", "7413"), Q0121 = c("9311", "9312", "9313", "9321", "9329"))
HAB_FIJO <- c(Q0116 = "5321")

isei <- read_csv(ARCH$isei, col_types = cols(isco08 = col_character(), isei08 = col_double())) %>%
  mutate(isco08 = sprintf("%04d", as.integer(isco08)))
stopifnot("La tabla ISEI debe ser la de DIGCLASS (enteros)" = all(abs(isei$isei08 - round(isei$isei08)) < 1e-9, na.rm = TRUE))
frec <- read_csv(file.path(DIR_INT, "frecuencias_casen_rm.csv"), col_types = cols(isco = col_character()))
tot_rm <- sum(frec$pond)
esp <- readRDS(file.path(DIR_INT, "espacio_habilidades.rds"))
en_B <- function(code) as.character(as.integer(code)) %in% rownames(esp$B)

cw <- POSICIONES
if (requireNamespace("DIGCLASS", quietly = TRUE)) {
  suppressPackageStartupMessages(library(DIGCLASS)); select <- dplyr::select
  o16 <- suppressWarnings(as.integer(as.character(DIGCLASS::isco08_to_oesch(cw$isco_equipo,
           self_employed = cw$self_employed, n_employees = cw$self_employed, n_classes = 16))))
  chequeo("Oesch16 de las 27 posiciones igual a DIGCLASS (posiciones que coinciden)", sum(o16 == cw$oesch16, na.rm = TRUE), 27)
  if (exists("isco08_to_isei", envir = asNamespace("DIGCLASS"))) {   # la tabla ISEI de data/ debe ser la de DIGCLASS
    i_dig <- suppressWarnings(as.numeric(as.character(DIGCLASS::isco08_to_isei(isei$isco08))))
    chequeo("Tabla ISEI de data/ igual a DIGCLASS (codigos distintos)", sum(abs(i_dig - isei$isei08) > 1e-9, na.rm = TRUE), 0)
  }
}
stopifnot(nrow(cw) == 27, setequal(cw$posicion, GP_VARS))

cand <- imap_dfr(CANDIDATOS_HAB, ~ tibble(posicion = .y, isco = .x)) %>% left_join(frec, by = "isco") %>%
  mutate(across(c(n_casen, pond), ~ replace_na(.x, 0)), en_B = en_B(isco))
elegidos <- cand %>% filter(en_B) %>% group_by(posicion) %>% slice_max(pond, n = 1, with_ties = FALSE) %>%
  ungroup() %>% select(posicion, isco_elegido = isco)

pond_de <- function(pos, cd) {
  if (pos %in% names(TAM_COMP)) sum(frec$pond[Reduce(`|`, lapply(TAM_COMP[[pos]], function(p) startsWith(frec$isco, p)))])
  else sum(frec$pond[frec$isco == cd])
}
cw <- cw %>% left_join(elegidos, by = "posicion") %>%
  mutate(isco_habilidades = case_when(posicion %in% names(HAB_FIJO) ~ unname(HAB_FIJO[posicion]),
                                      !is.na(isco_elegido) ~ isco_elegido, TRUE ~ isco_equipo)) %>%
  select(-isco_elegido) %>%
  left_join(isei %>% rename(isco_equipo = isco08, isei_pos = isei08), by = "isco_equipo") %>%
  mutate(TamGrupo_p = map2_dbl(posicion, isco_equipo, pond_de) / tot_rm,
         class_pos = if_else(self_employed == 1, CLASES7[5], colapsar_oesch7(oesch16)),
         tramo_equipo = case_when(posicion %in% TRAMOS_EQUIPO$alto ~ "alto", posicion %in% TRAMOS_EQUIPO$medio ~ "medio",
                                  posicion %in% TRAMOS_EQUIPO$bajo ~ "bajo", TRUE ~ NA_character_)) %>%
  left_join(esp$shares %>% rename(isco_habilidades = isco4), by = "isco_habilidades")

stopifnot("Codigos de habilidades fuera de B" = all(en_B(cw$isco_habilidades)),
          "Posiciones sin ISEI" = !any(is.na(cw$isei_pos)),
          "Posiciones sin clase" = !any(is.na(cw$class_pos)))
cat("Posiciones por clase:\n"); print(table(cw$class_pos))
chequeo("Posiciones por clase (5, 1, 6, 3, 1, 6, 5)", as.integer(table(factor(cw$class_pos, levels = CLASES7))), c(5, 1, 6, 3, 1, 6, 5))

write_csv(cw, file.path(DIR_INT, "crosswalk_gp27.csv"))
write_csv(cw %>% transmute(posicion, ocupacion = label, isco = isco_equipo, isco_habilidades, isei = isei_pos,
                           clase = class_pos, n_categorias, across(starts_with("share_com"), ~ round(100 * .x, 1)),
                           pct_ocupados_rm = round(100 * TamGrupo_p, 2)),
          file.path(DIR_TAB, "tabla_posiciones_gp.csv"))
cat("Guardado: intermedios/crosswalk_gp27.csv y tablas/tabla_posiciones_gp.csv\n")

################################################################################
# DECISIONES METODOLOGICAS
# 1. Codigos del equipo Fondecyt para clase e ISEI (Q0116 = 3221, igual que en
#    el script del equipo, donde 3221 es clase 14). La tabla POSICIONES reemplaza
#    a data/crosswalk/crosswalk_gp27_FINAL.csv (que traia Q0116 = 5321).
# 2. Habilidades: en codigos agregados (2140, 2210, 2300, 7410, 9300), grupo
#    unitario mas frecuente en CASEN 2024 RM entre los que lista el equipo;
#    Q0116 usa el perfil de 5321 (auxiliares de enfermeria) por imputacion,
#    porque 3221 no tiene habilidades en ESCO ni MAPHA (igual que los egos TENS).
# 3. Clase de la posicion: Oesch16 como asalariado (verificado contra la tabla
#    de DIGCLASS para las 27 posiciones); Q0111 comerciantes = Small business
#    owners. Siete clases del equipo; Lower-grade y Small business tienen una
#    sola posicion en el GP: sus coeficientes de clase no se interpretan.
# 4. tramo_equipo: tramos de estatus del script del equipo (24 posiciones), solo
#    para la robustez R6.
################################################################################
