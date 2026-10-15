################################################################################
# 04_descriptivos.R   PIPELINE FINAL
#
# Descriptivos del articulo (OE1 y variable dependiente). Reemplaza a
# paralelo_v3/120, 121, 122 y la seccion 2 de 128.
#   Tabla 2  descriptivos ponderados de la muestra analitica
#   Tabla    quien conoce a quien: porcentaje y observado/esperado por clase
#   Tabla    tres medidas de red por clase (medias ponderadas e IC 95%)
#   Figuras  27 posiciones (estatus, clase y habilidades) | quien conoce a quien |
#            tres medidas por clase | composicion de habilidades: trabajo y red
#
# Lee:     intermedios/base_ego.rds, base_diadas.rds, crosswalk_gp27.csv
# Salidas: tablas/ y figuras/
################################################################################

source("00_config.R", encoding = "UTF-8")

ego  <- readRDS(file.path(DIR_INT, "base_ego.rds"))
diad <- readRDS(file.path(DIR_INT, "base_diadas.rds"))
cw   <- read_csv(file.path(DIR_INT, "crosswalk_gp27.csv"), show_col_types = FALSE)
x <- ego %>% filter(en_muestra)
w <- x$weight_m
corto <- setNames(CLASES7_CORTO, CLASES7)

# ==== Tabla 2 ==========================================================================
titulo("Tabla 2. Descriptivos ponderados")
cont <- list("Homogeneidad de clase (0 a 1)" = x$hom, "Similitud de habilidades (bruta)" = x$SH,
             "Distancia de estatus (puntos ISEI)" = -x$SP, "Conocidos en el generador" = x$ext,
             "Mujer" = x$mujer, "Edad" = x$edad, "Sin ingreso del trabajo" = x$sin_ingreso,
             "Ingreso del trabajo (con ingreso, $)" = if_else(x$sin_ingreso == 1, NA_real_, x$ytrabajo_total),
             "Clase imputada" = x$clase_imputada)
t2_cont <- imap_dfr(cont, ~ tibble(variable = .y, media = wmean(.x, w), de = wsd(.x, w),
                                   min = min(.x, na.rm = TRUE), max = max(.x, na.rm = TRUE)))
t2_fact <- bind_rows(
  x %>% count(categoria = as.character(clase), wt = weight_m) %>% mutate(variable = "Clase Oesch (siete clases)"),
  x %>% count(categoria = as.character(educ5), wt = weight_m) %>% mutate(variable = "Nivel educativo")) %>%
  group_by(variable) %>% mutate(pct = 100 * n / sum(n)) %>% ungroup() %>% select(variable, categoria, pct) %>%
  arrange(variable, match(categoria, c(CLASES7, levels(x$educ5))))
print(t2_cont %>% mutate(across(where(is.double), ~ round(.x, 3))), width = Inf)
print(t2_fact %>% mutate(pct = round(pct, 1)), n = Inf)
cat(sprintf("N = %d | conocidos mencionados = %d\n", nrow(x), sum(x$ext)))
write_csv(t2_cont, file.path(DIR_TAB, "tabla2_continuas.csv"))
write_csv(t2_fact, file.path(DIR_TAB, "tabla2_categoricas.csv"))
cor_w <- cov.wt(as.matrix(x %>% select(hom, SH, SP)), wt = w / sum(w), cor = TRUE)$cor
print(round(cor_w, 3)); write_csv(as_tibble(cor_w, rownames = "variable"), file.path(DIR_TAB, "correlaciones.csv"))
# Correlaciones ampliadas para el articulo (12-10-2026): las tres medidas de red
# mas el numero de conocidos (log), el ISEI del trabajador y el nivel educativo
# (cinco categorias ordenadas, 1 = basica o menos). Ponderadas, como cor_w.
vars_amp <- x %>% transmute(hom, SH, SP, log_conocidos = log(ext), isei = isei_ego,
                            educ = as.integer(educ5))
cor_amp <- cov.wt(as.matrix(vars_amp), wt = w / sum(w), cor = TRUE)$cor
print(round(cor_amp, 3)); write_csv(as_tibble(cor_amp, rownames = "variable"), file.path(DIR_TAB, "correlaciones_ampliadas.csv"))

# ==== Quien conoce a quien ===============================================================
titulo("Quien conoce a quien: clase de ego x clase de la posicion")
qq <- diad %>% inner_join(x %>% select(ID, clase_ego = class_oesch7, weight_m), by = "ID") %>%
  group_by(clase_ego, class_pos) %>% summarise(n = sum(weight_m * n_conocidos), .groups = "drop") %>%
  group_by(clase_ego) %>% mutate(pct = 100 * n / sum(n)) %>% ungroup()
# Esperado: distribucion de todos los conocidos de la muestra entre las clases de posicion
marg <- qq %>% group_by(class_pos) %>% summarise(pct_esperado = sum(n), .groups = "drop") %>%
  mutate(pct_esperado = 100 * pct_esperado / sum(pct_esperado))
qq <- qq %>% left_join(marg, by = "class_pos") %>% mutate(obs_esp = pct / pct_esperado)
write_csv(qq, file.path(DIR_TAB, "quien_conoce_a_quien.csv"))
print(qq %>% select(clase_ego, class_pos, obs_esp) %>% mutate(obs_esp = round(obs_esp, 2)) %>%
        pivot_wider(names_from = class_pos, values_from = obs_esp), width = Inf)

fig_qq <- qq %>% mutate(ego = factor(corto[clase_ego], levels = rev(CLASES7_CORTO)),
                        pos = factor(corto[class_pos], levels = CLASES7_CORTO)) %>%
  ggplot(aes(pos, ego, fill = log2(obs_esp))) + geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = fnum(obs_esp, 1)), size = 3) +
  scale_fill_gradient2(low = PALETA_TESIS[["azul_2"]], mid = PALETA_TESIS[["crema"]], high = UC_ROJO, midpoint = 0, guide = "none") +
  labs(x = "Clase de la posicion conocida", y = "Clase del trabajador",
       caption = "Observado / esperado si cada clase repartiera sus conocidos como el total de la muestra.\n* Una sola posicion en el GP.") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), axis.line = element_blank(), axis.ticks = element_blank())
guardar_fig(fig_qq, file.path(DIR_FIG, "fig_quien_conoce_a_quien"), w = 6.5, h = 5)

# ==== Tres medidas por clase =================================================================
titulo("Tres medidas de red por clase")
dis <- svydesign(ids = ~1, weights = ~weight_m, data = x)
dis <- update(dis, dist_isei = -SP)
tres <- map_dfr(c("hom", "SH_z", "dist_isei"), function(v) {
    m <- svyby(as.formula(paste0("~", v)), ~class_oesch7, dis, svymean)
    tibble(medida = v, clase = as.character(m$class_oesch7), media = as.numeric(coef(m)), ee = as.numeric(SE(m)))
  }) %>% mutate(li = media - 1.96 * ee, ls = media + 1.96 * ee)
write_csv(tres, file.path(DIR_TAB, "tres_medidas_por_clase.csv"))
print(tres %>% mutate(across(where(is.double), ~ round(.x, 3))), n = Inf)
etq_med <- c(hom = "Homogeneidad de clase", SH_z = "Similitud de habilidades (DE)", dist_isei = "Distancia de estatus (ISEI)")
fig_tres <- tres %>% mutate(clase = factor(corto[clase], levels = rev(CLASES7_CORTO)), medida = factor(etq_med[medida], levels = etq_med)) %>%
  ggplot(aes(media, clase)) + geom_linerange(aes(xmin = li, xmax = ls), colour = UC_GRIS) +
  geom_point(colour = UC_AZUL, size = 2.2) + facet_wrap(~ medida, scales = "free_x") +
  labs(x = NULL, y = NULL, caption = "Medias ponderadas con IC 95%. * Una sola posicion en el GP.")
guardar_fig(fig_tres, file.path(DIR_FIG, "fig_tres_medidas_por_clase"), w = 7.5, h = 3.8)

# ==== 27 posiciones ===========================================================================
pos_largo <- cw %>% select(posicion, label, isei_pos, class_pos, starts_with("share_com")) %>%
  pivot_longer(starts_with("share_com"), names_to = "com", values_to = "share") %>%
  mutate(comunidad = factor(NOMBRES_COM[as.integer(str_extract(com, "\\d"))], levels = NOMBRES_COM),
         etiqueta = sprintf("%s (%d)", label, as.integer(isei_pos)),
         etiqueta = fct_reorder(etiqueta, isei_pos))
fig_pos <- ggplot(pos_largo, aes(share, etiqueta, fill = comunidad)) + geom_col(width = 0.75) +
  scale_fill_manual(values = PALETA_COM) + guides(fill = guide_legend(nrow = 2)) +
  scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.03))) +
  labs(x = "Porcion de las categorias de habilidades del perfil", y = NULL,
       caption = "Posiciones ordenadas por ISEI (entre parentesis).")
guardar_fig(fig_pos, file.path(DIR_FIG, "fig_27_posiciones"), w = 7, h = 6.5)

# ==== Composicion de habilidades: trabajo y red, por clase ====================================
comp <- x %>% select(class_oesch7, weight_m, starts_with("ego_share_com"), starts_with("red_share_com")) %>%
  pivot_longer(-c(class_oesch7, weight_m), names_to = c("lugar", "com"), names_pattern = "(ego|red)_share_com(\\d)") %>%
  group_by(class_oesch7, lugar, com) %>% summarise(share = wmean(value, weight_m), .groups = "drop") %>%
  mutate(comunidad = factor(NOMBRES_COM[as.integer(com)], levels = NOMBRES_COM),
         lugar = factor(if_else(lugar == "ego", "Ocupacion del trabajador", "Ocupaciones de sus conocidos"),
                        levels = c("Ocupacion del trabajador", "Ocupaciones de sus conocidos")),
         clase = factor(corto[class_oesch7], levels = rev(CLASES7_CORTO)))
write_csv(comp, file.path(DIR_TAB, "composicion_trabajo_red.csv"))
fig_comp <- ggplot(comp, aes(share, clase, fill = comunidad)) + geom_col(width = 0.75) + facet_wrap(~ lugar) +
  scale_fill_manual(values = PALETA_COM) + guides(fill = guide_legend(nrow = 2)) +
  scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.04))) +
  labs(x = NULL, y = NULL)
guardar_fig(fig_comp, file.path(DIR_FIG, "fig_trabajo_red_por_clase"), w = 7.5, h = 4)
cat("\nListo: tablas y figuras descriptivas\n")
