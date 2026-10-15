################################################################################
# 05_modelos.R   PIPELINE FINAL
#
# Modelos del articulo, especificacion vigente (7-oct-2026, comentarios de
# Gabriel Otero). Reemplaza a paralelo_v3/113, 114, 123, 128 (secciones 3-5).
#
#   OE1  SH_z ~ SP_z + X   y   SP_z ~ SH_z + X                    (lineales)
#   M1   hom ~ X                                                  (base)
#   M2   hom ~ SH_z + X                                           (H1)
#   M3   hom ~ SH_z + SP_z + X
#   M4   hom ~ SH_z * SP_z + X                                    (H3)
#   M5   hom ~ SH_z * clase + X                                   (H2)
#   X = clase, mujer, edad, nivel educativo, log ingreso + sin ingreso.
#   M1 a M5: logit fraccional (svyglm quasibinomial), weight_m, ids = ~1.
#
# Robustez de M2 y M3: R1 3 o mas conocidos | R2 sin clase imputada |
#   R3 homogeneidad descontada por disponibilidad (lineal) | R4 con numero de
#   conocidos | R5 similitud de habilidades centrada | R6 homogeneidad del equipo
#   Fondecyt (tres tramos de estatus) | R7 diadas conocidas, multinivel (glmmTMB)
#
# Lee:     intermedios/base_ego.rds, base_diadas.rds
# Salidas: tablas/ (coeficientes, tabla 3, tabla 4, pendientes) | figuras/fig_pendientes_h2
#          intermedios/modelos.rds
################################################################################

source("00_config.R", encoding = "UTF-8")

ego <- readRDS(file.path(DIR_INT, "base_ego.rds"))
x <- ego %>% filter(en_muestra)
dis <- svydesign(ids = ~1, weights = ~weight_m, data = x)
qb <- quasibinomial()
CTRL       <- c("clase", "mujer", "edad_sc", "educ5", "ln_ing_c", "sin_ingreso")
CTRL_CONOC <- c(CTRL, "log_ext_sc")
fm <- function(lhs, rhs = NULL, ctrl = CTRL) as.formula(paste(lhs, "~", paste(c(rhs, ctrl), collapse = " + ")))

# ==== Modelos principales (Tabla 3) ======================================================
titulo("Modelos principales")
mods <- list(
  "OE1 SH"        = svyglm(fm("SH_z", "SP_z"), design = dis),
  "OE1 SP"        = svyglm(fm("SP_z", "SH_z"), design = dis),
  "M1 base"       = svyglm(fm("hom"), design = dis, family = qb),
  "M2 H1"         = svyglm(fm("hom", "SH_z"), design = dis, family = qb),
  "M3 + estatus"  = svyglm(fm("hom", c("SH_z", "SP_z")), design = dis, family = qb),
  "M4 H3"         = svyglm(fm("hom", "SH_z * SP_z"), design = dis, family = qb),
  "M5 H2"         = svyglm(fm("hom", "SH_z * clase"), design = dis, family = qb))
res <- imap_dfr(mods, ~ tidy_svy(.x, .y))
ajuste <- tibble(modelo = names(mods), r2_o_devianza = map_dbl(mods, ~ 1 - .x$deviance / .x$null.deviance),
                 n = map_int(mods, ~ nrow(.x$model)),
                 pp_SH = map_dbl(names(mods), ~ if (.x %in% c("M2 H1", "M3 + estatus")) pp_1de(mods[[.x]]) else NA_real_),
                 pp_SP = map_dbl(names(mods), ~ if (.x == "M3 + estatus") pp_1de(mods[[.x]], "SP_z") else NA_real_))
tabla3 <- res %>% filter(str_starts(modelo, "M")) %>% select(modelo, termino, texto) %>%
  pivot_wider(names_from = modelo, values_from = texto)
print(tabla3, n = Inf, width = Inf)
print(ajuste %>% mutate(across(where(is.double), ~ round(.x, 3))))

cat("\nChequeos contra las estimaciones del 7-oct-2026 (128):\n")
chequeo("N", nrow(x), 971)
chequeo("H1: similitud de habilidades", coef(mods[["M2 H1"]])[["SH_z"]], 0.402)
chequeo("H1: pp por 1 DE", pp_1de(mods[["M2 H1"]]), 7.28, tol = 1e-2)
if (!ISEI_CON_CODIGO_ORIGINAL) {
  chequeo("M3: similitud de habilidades", coef(mods[["M3 + estatus"]])[["SH_z"]], 0.286)
  chequeo("M4: interaccion", coef(mods[["M4 H3"]])[["SH_z:SP_z"]], 0.049, tol = 1e-2)
} else cat("  (ISEI con codigo original: M3, M4 y OE1 cambian respecto del 7-oct; ver AUDITORIA)\n")

# ==== Pendientes por clase (H2) ===========================================================
titulo("Pendiente de la similitud de habilidades en cada clase (M5)")
cls <- levels(x$clase)
pend <- map_dfr(cls, function(cl) {
  v <- c(SH_z = 1); if (cl != cls[1]) v[paste0("SH_z:clase", cl)] <- 1
  s <- svycontrast(mods[["M5 H2"]], v)
  tibble(clase = cl, pendiente = as.numeric(coef(s)), ee = as.numeric(SE(s)), egos = sum(x$clase == cl))
}) %>% mutate(li = pendiente - 1.96 * ee, ls = pendiente + 1.96 * ee)
p_int <- regTermTest(mods[["M5 H2"]], ~ SH_z:clase)$p
print(pend %>% mutate(across(where(is.double), ~ round(.x, 3))))
cat(sprintf("Test conjunto SH x clase: p = %.4f\n", p_int))

corto <- setNames(CLASES7_CORTO, CLASES7)
manual <- c(CLASES7[6], CLASES7[7])
fig_h2 <- pend %>% mutate(clase_c = factor(corto[clase], levels = rev(CLASES7_CORTO)),
                          tipo = if_else(clase %in% manual, "Clases manuales", "Clases de servicio y otras")) %>%
  ggplot(aes(pendiente, clase_c, colour = tipo)) +
  geom_vline(xintercept = 0, colour = PALETA_TESIS[["gris"]]) +
  geom_linerange(aes(xmin = li, xmax = ls)) + geom_point(size = 2.4) +
  scale_colour_manual(values = c("Clases manuales" = UC_ROJO, "Clases de servicio y otras" = UC_AZUL)) +
  labs(x = "Pendiente de la similitud de habilidades (log-odds por 1 DE)", y = NULL,
       caption = sprintf("Logit fraccional ponderado con interaccion; IC 95%%. Test conjunto p %s. * Una sola posicion en el GP.",
                         if (p_int < 0.001) "< 0,001" else paste("=", fnum(p_int))))
guardar_fig(fig_h2, file.path(DIR_FIG, "fig_pendientes_h2"), w = 6.5, h = 3.8)

# ==== Robustez (Tabla 4) ===================================================================
titulo("Robustez de M2 y M3")
dis_sens <- subset(dis, en_muestra_sens)
dis_act  <- subset(dis, clase_imputada == 0)
dis_c    <- update(dis, SH_z = SHc_z)                       # R5: version centrada
dis_eq   <- subset(dis, !is.na(hom_equipo))
par_M <- function(d, lhs = "hom", fam = qb, ctrl = CTRL, etq) bind_rows(
  tidy_svy(svyglm(fm(lhs, "SH_z", ctrl), design = d, family = fam), "M2 H1", etq),
  tidy_svy(svyglm(fm(lhs, c("SH_z", "SP_z"), ctrl), design = d, family = fam), "M3 + estatus", etq))
rob <- bind_rows(
  res %>% filter(modelo %in% c("M2 H1", "M3 + estatus")),
  par_M(dis_sens, etq = "R1 3 o mas conocidos"),
  par_M(dis_act, etq = "R2 sin clase imputada"),
  par_M(dis, lhs = "hom_descontada", fam = gaussian(), etq = "R3 descontada por disponibilidad (lineal)"),
  par_M(dis, ctrl = CTRL_CONOC, etq = "R4 con numero de conocidos"),
  par_M(dis_c, etq = "R5 similitud de habilidades centrada"),
  par_M(dis_eq, lhs = "hom_equipo", etq = "R6 homogeneidad del equipo (3 tramos de estatus)"))

if (requireNamespace("glmmTMB", quietly = TRUE)) {     # R7 diadas conocidas, intercepto por trabajador
  diad <- readRDS(file.path(DIR_INT, "base_diadas.rds"))
  d <- diad %>% filter(n_conocidos > 0) %>%
    inner_join(x %>% select(ID, all_of(CTRL)), by = "ID") %>% filter(!is.na(SH_ip), !is.na(SP_ip)) %>%
    mutate(SH_z = zs(SH_ip), SP_z = zs(SP_ip), ID = factor(ID))
  fd <- function(rhs) as.formula(paste("misma_clase_ip ~", paste(c(rhs, CTRL, "(1 | ID)"), collapse = " + ")))
  tidy_tmb <- function(m, modelo) { co <- summary(m)$coefficients$cond
    tibble(modelo, muestra = "R7 diadas conocidas (multinivel)", termino = rownames(co), est = co[, 1], se = co[, 2],
           p = co[, 4], n = nobs(m)) %>% mutate(texto = sprintf("%.3f (%.3f)%s", est, se, estrellas(p))) }
  rob <- bind_rows(rob,
    tidy_tmb(glmmTMB::glmmTMB(fd("SH_z"), data = d, weights = n_conocidos, family = binomial()), "M2 H1"),
    tidy_tmb(glmmTMB::glmmTMB(fd(c("SH_z", "SP_z")), data = d, weights = n_conocidos, family = binomial()), "M3 + estatus"))
} else cat("  [AVISO] glmmTMB no instalado: se omite R7\n")

tabla4 <- rob %>% filter(termino %in% c("SH_z", "SP_z")) %>%
  transmute(muestra, columna = paste(modelo, termino), texto = paste0(texto, " [n=", n, "]")) %>%
  pivot_wider(names_from = columna, values_from = texto)
print(tabla4, width = Inf)

write_csv(res, file.path(DIR_TAB, "coef_modelos.csv"))
write_csv(ajuste, file.path(DIR_TAB, "ajuste_modelos.csv"))
write_csv(tabla3, file.path(DIR_TAB, "tabla3_modelos.csv"))
write_csv(pend, file.path(DIR_TAB, "pendientes_h2.csv"))
write_csv(rob, file.path(DIR_TAB, "coef_robustez.csv"))
write_csv(tabla4, file.path(DIR_TAB, "tabla4_robustez.csv"))
saveRDS(mods, file.path(DIR_INT, "modelos.rds"))
cat("\nGuardado: tablas de modelos y robustez\n")

################################################################################
# DECISIONES METODOLOGICAS
# 1. Una sola dependiente (homogeneidad de clase); ingreso como control; sin
#    numero de conocidos (queda como robustez R4).
# 2. Referencias: Unskilled workers y basica o menos. Los coeficientes de clase
#    no se interpretan (dependen de cuantas posiciones tiene cada clase en el GP).
# 3. Diseno svydesign(ids = ~1, weights = ~weight_m), provisional: la base trae
#    Zona y folio_m, pero el diseno muestral no esta confirmado.
# 4. Efectos en pp: cambio medio en la homogeneidad predicha al sumar 1 DE.
# 5. H2 y H3 se estiman por separado, una interaccion cada una.
################################################################################
