################################################################################
# 03_base_analitica.R   PIPELINE FINAL
#
# Base analitica a nivel diada (trabajador x posicion del GP) y a nivel
# trabajador. Reemplaza a codigo_v2/01, la parte pertinente de codigo_v2/04,
# el script 107 (clase de ego, que no esta en la carpeta) y paralelo_v3/112.
#
#   1. Encuesta Fondecyt: ISCO de ego (isco_mainjob; si falta, ultima ocupacion
#      Q4402), controles y ponderador.
#   2. Clase de ego: ocupados con la clase del equipo (class16_r, script de
#      Gabriel Otero); no ocupados con ultima ocupacion, Oesch16 de DIGCLASS con
#      el tipo de empleo del ultimo trabajo (Q4404). Siete clases.
#   3. ISEI de ego (DIGCLASS; respaldo a 3 y 2 digitos).
#   4. Diadas: conocidos por posicion en tramos, similitud de habilidades,
#      similitud de estatus y misma clase.
#   5. Nivel trabajador: similitud de habilidades y de estatus de la red,
#      homogeneidad de clase (bruta y descontada), composicion de la red por
#      comunidad, homogeneidad del equipo (R6), muestra analitica.
#
# Lee:     data_fondecyt.xlsx, intermedios/espacio_habilidades.rds, intermedios/crosswalk_gp27.csv
# Salidas: intermedios/base_ego.rds, intermedios/base_diadas.rds, tablas/flujo_muestral.csv
################################################################################

source("00_config.R", encoding = "UTF-8")
if (!requireNamespace("DIGCLASS", quietly = TRUE)) stop("Falta DIGCLASS: remotes::install_github('cimentadaj/DIGCLASS')")
suppressPackageStartupMessages(library(DIGCLASS))   # sus tablas internas requieren adjuntar el paquete
select <- dplyr::select

esp <- readRDS(file.path(DIR_INT, "espacio_habilidades.rds"))
# Similitud de habilidades entre dos ocupaciones (ver 01, seccion 6)
sim_hab <- function(a, b) {
  ca <- esp$cats[[a]]; cb <- esp$cats[[b]]
  if (is.null(ca) || is.null(cb) || !length(ca) || !length(cb)) return(NA_real_)
  -mean(esp$D_geo[ca, cb])
}
sim_hab_centrada <- function(a, b) {
  s <- sim_hab(a, b); if (is.na(s)) return(NA_real_)
  -(2 * (-s) - esp$d_auto[[a]] - esp$d_auto[[b]]) / 2
}
cw  <- read_csv(file.path(DIR_INT, "crosswalk_gp27.csv"), col_types = cols(.default = col_character(),
               isei_pos = col_double(), TamGrupo_p = col_double()))
isei_tab <- read_csv(ARCH$isei, col_types = cols(isco08 = col_character(), isei08 = col_double())) %>%
  mutate(isco08 = sprintf("%04d", as.integer(isco08)))
look_isei <- setNames(isei_tab$isei08, isei_tab$isco08)
corr_map <- CORRECCIONES_ISCO %>% select(isco_orig, isco_corr)

NIVELES_EDUC <- c(
  "Sin estudios" = 1, "Educación Básica o Preparatoria incompleta" = 2,
  "Educación Básica o Preparatoria completa" = 3, "Educación Media o Humanidades incompleta" = 4,
  "Educación Media o Humanidades completa" = 5, "Técnica Superior incompleta" = 6,
  "Técnica Superior completa" = 7, "Universitaria incompleta" = 8,
  "Universitaria completa" = 9, "Estudios de posgrado (magíster o doctorado)" = 10)

# ==== 1. Encuesta ==================================================================
titulo("1. Encuesta Fondecyt")
fond <- read_excel(ARCH$fondecyt, sheet = "datos")
ego <- fond %>%
  transmute(ID = as.numeric(ID), weight_m = as.numeric(weight_m), Zona, folio_m,
            isco_mainjob = suppressWarnings(as.integer(isco_mainjob)), Q4402 = suppressWarnings(as.integer(Q4402)),
            class16_lab = as.character(class16_r), Q43 = as.character(Q43), Q4404 = as.character(Q4404),
            sexo = as.character(Q68), edad = as.numeric(Q70), educ_lab = as.character(Q40),
            ytrabajo_total = as.numeric(ytrabajo_total)) %>%
  mutate(isco_ego = coalesce(isco_mainjob, Q4402),
         isco_ego = suppressWarnings(as.integer(substr(as.character(isco_ego), 1, 4))))
n_encuesta <- nrow(ego)
ego <- ego %>% filter(!is.na(isco_ego))        # sin ocupacion actual ni ultima: fuera
cat(sprintf("Encuestados: %d | con ocupacion actual o ultima: %d\n", n_encuesta, nrow(ego)))

# Codigo para habilidades (con correcciones CIUO-08.CL -> ISCO-08 y 3221 -> 5321)
ego <- ego %>% left_join(corr_map, by = c("isco_ego" = "isco_orig")) %>%
  mutate(isco_hab = coalesce(isco_corr, isco_ego)) %>% select(-isco_corr)
# Codigo para ISEI y clase
ego <- ego %>% mutate(isco_status = if (ISEI_CON_CODIGO_ORIGINAL) isco_ego else isco_hab)
cat(sprintf("Egos con codigo corregido para habilidades: %d | ISEI con codigo %s\n",
            sum(ego$isco_hab != ego$isco_ego), if (ISEI_CON_CODIGO_ORIGINAL) "original" else "corregido"))

# ==== 2. Clase de ego ===============================================================
titulo("2. Clase de ego (siete clases)")
oesch16_dig <- function(isco, se, ne) suppressWarnings(as.integer(as.character(
  DIGCLASS::isco08_to_oesch(sprintf("%04d", as.integer(isco)), self_employed = se, n_employees = ne, n_classes = 16))))
ego <- ego %>%
  mutate(class16_equipo = match(class16_lab, OESCH16_LAB),
         actual = !is.na(class16_equipo),
         tipo_empleo_ult = case_when(str_detect(coalesce(Q4404, ""), "^Trabaja solo") ~ "cuenta propia",
                                     str_detect(coalesce(Q4404, ""), "^Patr") ~ "empleador", TRUE ~ "asalariado"))
stopifnot("Etiquetas de class16_r sin codigo" = sum(!is.na(ego$class16_lab)) == sum(ego$actual))
imp <- ego %>% filter(!actual, isco_status < 9990) %>%
  mutate(se = as.integer(tipo_empleo_ult != "asalariado"), ne = if_else(tipo_empleo_ult == "empleador", 1, 0),
         class16_imp = oesch16_dig(isco_status, se, ne)) %>% select(ID, class16_imp)
ego <- ego %>% left_join(imp, by = "ID") %>%
  mutate(class16 = if_else(actual, class16_equipo, class16_imp),
         class_oesch7 = colapsar_oesch7(class16),
         clase_imputada = as.integer(!actual & !is.na(class_oesch7)),
         fuente_clase = case_when(actual ~ "actual (equipo Fondecyt)",
                                  !is.na(class_oesch7) ~ paste("imputada DIGCLASS,", tipo_empleo_ult),
                                  TRUE ~ "sin clase"))
print(count(ego, fuente_clase))

# ==== 3. ISEI de ego ==================================================================
isei_de <- function(code) {
  c4 <- ifelse(is.na(code) | code >= 9990, NA_character_, sprintf("%04d", code))
  coalesce(unname(look_isei[c4]), unname(look_isei[paste0(substr(c4, 1, 3), "0")]),
           unname(look_isei[paste0(substr(c4, 1, 2), "00")]))
}
ego <- ego %>% mutate(isei_ego = isei_de(isco_status))
cat(sprintf("Egos sin ISEI: %d (codigos de no ocupacion 9990, 9998, 9999)\n", sum(is.na(ego$isei_ego))))

# ==== 4. Diadas ========================================================================
titulo("4. Diadas trabajador x posicion")
diad <- fond %>% transmute(ID = as.numeric(ID), across(all_of(GP_VARS))) %>%
  filter(ID %in% ego$ID) %>%
  pivot_longer(all_of(GP_VARS), names_to = "posicion", values_to = "n_bruto") %>%
  mutate(n_conocidos = TRAMOS_GP(n_bruto)) %>%
  left_join(cw %>% select(posicion, isco_hab_pos = isco_habilidades, isei_pos, class_pos), by = "posicion") %>%
  left_join(ego %>% select(ID, isco_hab, isei_ego, class_oesch7), by = "ID") %>%
  mutate(k_ego = as.character(isco_hab), k_pos = as.character(as.integer(isco_hab_pos)))
pares <- diad %>% distinct(k_ego, k_pos) %>%
  mutate(SH_ip = map2_dbl(k_ego, k_pos, sim_hab), SH_ip_centrada = map2_dbl(k_ego, k_pos, sim_hab_centrada))
diad <- diad %>% left_join(pares, by = c("k_ego", "k_pos")) %>%
  mutate(SP_ip = -abs(isei_ego - isei_pos), misma_clase_ip = as.integer(class_oesch7 == class_pos)) %>%
  arrange(ID, posicion)
cat(sprintf("Diadas: %d | con similitud de habilidades: %d\n", nrow(diad), sum(!is.na(diad$SH_ip))))

# ==== 5. Nivel trabajador ==============================================================
titulo("5. Indicadores de red por trabajador")
disp <- cw %>% group_by(class_pos) %>% summarise(B_c = sum(TamGrupo_p), .groups = "drop") %>% mutate(B_c = B_c / sum(B_c))
share_pos <- cw %>% select(posicion, starts_with("share_com")) %>% mutate(across(-posicion, as.numeric))
red <- diad %>% left_join(share_pos, by = "posicion") %>% group_by(ID) %>%
  summarise(ext = sum(n_conocidos),
            SH = if (ext > 0) sum(n_conocidos * SH_ip) / ext else NA_real_,
            SH_centrada = if (ext > 0) sum(n_conocidos * SH_ip_centrada) / ext else NA_real_,
            SP = if (ext > 0) sum(n_conocidos * SP_ip) / ext else NA_real_,
            hom = if (ext > 0) sum(n_conocidos * misma_clase_ip) / ext else NA_real_,
            across(starts_with("share_com"), ~ if (ext > 0) sum(n_conocidos * .x) / ext else NA_real_, .names = "red_{.col}"),
            .groups = "drop")
# Homogeneidad del equipo (robustez R6): conteos brutos, 24 posiciones, tramo de estatus de la clase de ego
heq <- diad %>% left_join(cw %>% select(posicion, tramo_equipo), by = "posicion") %>%
  filter(!is.na(tramo_equipo), !is.na(class_oesch7)) %>%
  group_by(ID) %>% summarise(n24 = sum(n_bruto, na.rm = TRUE),
                             n_tramo = sum(n_bruto[tramo_equipo == TRAMO_DE_CLASE[first(class_oesch7)]], na.rm = TRUE),
                             .groups = "drop") %>%
  mutate(hom_equipo = if_else(n24 > 0, n_tramo / n24, NA_real_))
shares_ego <- esp$shares %>% transmute(k_ego = isco4, across(starts_with("share_com"), .names = "ego_{.col}"))

ego <- ego %>% left_join(red, by = "ID") %>% left_join(heq %>% select(ID, hom_equipo), by = "ID") %>%
  left_join(disp %>% rename(class_oesch7 = class_pos), by = "class_oesch7") %>%
  mutate(hom_descontada = hom - B_c, k_ego = as.character(isco_hab)) %>%
  left_join(shares_ego, by = "k_ego")

media_ln <- mean(log(ego$ytrabajo_total[ego$ytrabajo_total > 0]), na.rm = TRUE)
ego <- ego %>%
  mutate(mujer = as.integer(sexo == "Mujer"),
         educ = unname(NIVELES_EDUC[educ_lab]),
         educ5 = factor(case_when(educ <= 3 ~ "Basica o menos", educ == 4 ~ "Media incompleta", educ == 5 ~ "Media completa",
                                  educ %in% 6:7 ~ "Tecnica superior", educ >= 8 ~ "Universitaria"),
                        levels = c("Basica o menos", "Media incompleta", "Media completa", "Tecnica superior", "Universitaria")),
         sin_ingreso = as.integer(is.na(ytrabajo_total) | ytrabajo_total <= 0),
         ln_ing_c = if_else(sin_ingreso == 1, 0, log(pmax(ytrabajo_total, 1)) - media_ln),
         en_muestra = (!is.na(class_oesch7) & !is.na(ext) & ext > 0 & !is.na(SH) & !is.na(SP) &
                       !is.na(ego_share_com1) & !is.na(mujer) & !is.na(edad) & !is.na(educ)) %in% TRUE)

mu <- ego$en_muestra
std <- function(x) { out <- rep(NA_real_, length(x)); out[mu] <- zs(x[mu]); out }
ego <- ego %>% mutate(SH_z = std(SH), SP_z = std(SP), SHc_z = std(SH_centrada), edad_sc = std(edad),
                      log_ext_sc = std(if_else(ext > 0, log(ext), NA_real_)),
                      en_muestra_sens = en_muestra & ext >= MIN_CONOCIDOS_SENS,
                      clase = relevel(factor(class_oesch7, levels = CLASES7), ref = CLASE_REF))

flujo <- tibble(paso = c("Encuestados", "Con ocupacion actual o ultima", "Con clase Oesch7",
                         "Con perfil de habilidades y conocidos en el GP", "Con controles completos (muestra analitica)"),
                n = c(n_encuesta, nrow(ego), sum(!is.na(ego$class_oesch7)),
                      sum(!is.na(ego$class_oesch7) & !is.na(ego$SH) & !is.na(ego$SP) & !is.na(ego$ego_share_com1)), sum(mu)))
print(flujo)
cat(sprintf("Clase imputada en la muestra: %d\n", sum(mu & ego$clase_imputada == 1)))
write_csv(flujo, file.path(DIR_TAB, "flujo_muestral.csv"))

# ==== Validacion contra el pipeline v3 =====================================================
titulo("Validacion contra paralelo_v3 (112)")
if (VALIDAR_CONTRA_ANTERIOR && file.exists(ARCH$v3_ego)) {
  v3 <- readRDS(ARCH$v3_ego) %>% mutate(ID = as.numeric(ID)) %>% select(ID, SH_red_ego, SP_red_ego, cierre_bruto, en_muestra)
  cmp <- ego %>% select(ID, SH, SP, hom, en_muestra) %>% inner_join(v3, by = "ID", suffix = c("", "_v3"))
  chequeo("Muestra analitica (v3: 971)", sum(cmp$en_muestra), sum(cmp$en_muestra_v3))
  chequeo("Max |SH - SH v3|", max(abs(cmp$SH - cmp$SH_red_ego), na.rm = TRUE), 0, tol = 1e-8)
  chequeo("Max |homogeneidad - cierre_bruto v3|", max(abs(cmp$hom - cmp$cierre_bruto), na.rm = TRUE), 0, tol = 1e-8)
  n_sp <- sum(abs(cmp$SP - cmp$SP_red_ego) > 1e-8, na.rm = TRUE)
  cat(sprintf("  Egos con similitud de estatus distinta a v3: %d (esperado: %d)\n", n_sp, if (ISEI_CON_CODIGO_ORIGINAL) 17 else 0))
}

saveRDS(ego, file.path(DIR_INT, "base_ego.rds"))
saveRDS(diad %>% select(ID, posicion, n_bruto, n_conocidos, SH_ip, SH_ip_centrada, SP_ip, misma_clase_ip, class_pos, isei_pos),
        file.path(DIR_INT, "base_diadas.rds"))
cat("\nGuardado: intermedios/base_ego.rds y intermedios/base_diadas.rds\n")

################################################################################
# DECISIONES METODOLOGICAS
# 1. Muestra de partida: encuestados con ocupacion actual (isco_mainjob) o ultima
#    (Q4402). Los codigos 9990, 9998 y 9999 no son ocupaciones: sin clase.
# 2. Clase: ocupados con class16_r del equipo (sin recalcular); no ocupados con
#    DIGCLASS y Q4404 (cuenta propia: sin empleados; empleador: 1 empleado, todo
#    empleador se trata como pequeno). clase_imputada = 1 marca la imputacion.
#    Nota: la clase "final" del script del equipo (class16) toma la clase del
#    sostenedor del hogar cuando existe; aqui se usa la clase propia (class16_r).
# 3. ISEI y clase con el codigo del equipo; el codigo corregido (CIUO-08.CL ->
#    ISCO-08 y la sustitucion 3221 -> 5321) se usa solo para habilidades. Los
#    TENS (3221) reciben por imputacion el perfil de habilidades de 5321
#    (auxiliares de enfermeria), porque 3221 no tiene habilidades en ESCO ni
#    MAPHA; su clase y su ISEI son los de 3221. Declararlo en metodos.
# 4. Conocidos en tramos (0, 1, 3, 6, 9, 13, 16). Todas las medidas de red son
#    promedios ponderados por conocidos; la unidad es el trabajador.
# 5. Homogeneidad descontada = homogeneidad menos la disponibilidad de la clase
#    en CASEN 2024 RM (robustez). hom_equipo: definicion del equipo (R6).
# 6. sin_ingreso agrupa a quienes no tienen ingreso del trabajo (no ocupados) y
#    a 15 ocupados sin dato de ingreso.
# 7. Estandarizacion dentro de la muestra analitica.
################################################################################
