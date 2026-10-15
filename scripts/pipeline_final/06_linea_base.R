################################################################################
# 06_linea_base.R   PIPELINE FINAL
#
# Linea base estructural N3: cuanto de cada coeficiente reproduce la estructura
# ocupacional y cuanto es relacional. Reemplaza a paralelo_v3/116, 117, 119 y la
# seccion 6 de 128.
#
# Tablas aleatorias con ambos margenes fijos (r2dtable, Patefield): se conserva
# cuantos conocidos tiene cada trabajador y cuan conocida es cada posicion; solo
# cambia quien conoce a quien. En cada tabla se recalculan homogeneidad, SH y SP
# (SH y SP se reestandarizan) y se reestiman M2, M3 y M4; controles fijos.
#
# Lee:     intermedios/base_ego.rds, base_diadas.rds
# Salidas: tablas/linea_base_n3.csv, tablas/linea_base_n3_distribucion.csv,
#          figuras/fig_linea_base_n3
################################################################################

source("00_config.R", encoding = "UTF-8")

ego  <- readRDS(file.path(DIR_INT, "base_ego.rds"))
diad <- readRDS(file.path(DIR_INT, "base_diadas.rds"))
x <- ego %>% filter(en_muestra) %>% arrange(ID)
d <- diad %>% filter(ID %in% x$ID) %>% arrange(ID, posicion)
K <- n_distinct(d$posicion)
stopifnot("Matriz completa trabajador x posicion" = nrow(d) == nrow(x) * K, all(unique(d$ID) == x$ID))
a_mat <- function(v) matrix(v, nrow = nrow(x), ncol = K, byrow = TRUE)
N <- a_mat(d$n_conocidos); S <- a_mat(d$SH_ip); P <- a_mat(d$SP_ip); C <- a_mat(d$misma_clase_ip)

CTRL <- c("clase", "mujer", "edad_sc", "educ5", "ln_ing_c", "sin_ingreso")
fm <- function(rhs) as.formula(paste("hom ~", paste(c(rhs, CTRL), collapse = " + ")))
TERM <- c("M2: similitud de habilidades", "M3: similitud de habilidades", "M3: similitud de estatus", "M4: interaccion")
ajustar <- function(M) {
  e <- rowSums(M); xx <- x
  xx$hom <- rowSums(M * C) / e; xx$SH_z <- zs(rowSums(M * S) / e); xx$SP_z <- zs(rowSums(M * P) / e)
  g <- function(rhs) suppressWarnings(glm(fm(rhs), data = xx, weights = weight_m, family = quasibinomial()))
  m2 <- g("SH_z"); m3 <- g(c("SH_z", "SP_z")); m4 <- g("SH_z * SP_z")
  setNames(c(coef(m2)[["SH_z"]], coef(m3)[["SH_z"]], coef(m3)[["SP_z"]], coef(m4)[["SH_z:SP_z"]]), TERM)
}

titulo("Linea base N3")
obs <- ajustar(N)
print(round(obs, 3))
set.seed(SEMILLA_NULO)
tabs <- r2dtable(R_NULO, as.integer(round(rowSums(N))), as.integer(round(colSums(N))))
nulo <- t(sapply(tabs, ajustar))
n3 <- tibble(termino = TERM, observado = obs, media_nula = colMeans(nulo),
             p2.5 = apply(nulo, 2, quantile, .025), p97.5 = apply(nulo, 2, quantile, .975),
             p_empirico = map2_dbl(TERM, obs, ~ mean(nulo[, .x] >= .y))) %>%
  mutate(parte_estructural_pct = if_else(sign(observado) == sign(media_nula), 100 * media_nula / observado, NA_real_),
         parte_relacional_pct = 100 - parte_estructural_pct)
print(n3 %>% mutate(across(where(is.double), ~ round(.x, 3))), width = Inf)
rel <- function(o, v) 100 * (o - v) / o
dif <- rel(obs[[TERM[3]]], nulo[, TERM[3]]) - rel(obs[[TERM[2]]], nulo[, TERM[2]])
cat(sprintf("Parte relacional, estatus menos habilidades (M3): %.1f pp [%.1f; %.1f] en %d tablas\n",
            mean(dif), quantile(dif, .025), quantile(dif, .975), R_NULO))
write_csv(n3, file.path(DIR_TAB, "linea_base_n3.csv"))
write_csv(as_tibble(nulo), file.path(DIR_TAB, "linea_base_n3_distribucion.csv"))

fig_n3 <- as_tibble(nulo) %>% pivot_longer(everything(), names_to = "termino", values_to = "valor") %>%
  filter(termino != TERM[4]) %>% mutate(termino = factor(termino, levels = TERM[1:3])) %>%
  ggplot(aes(valor)) + geom_histogram(bins = 30, fill = PALETA_TESIS[["gris"]], colour = "white") +
  geom_vline(data = n3 %>% filter(termino != TERM[4]) %>% mutate(termino = factor(termino, levels = TERM[1:3])),
             aes(xintercept = observado), colour = UC_AZUL, linewidth = 0.9) +
  facet_wrap(~ termino, scales = "free_y") +
  labs(x = "Coeficiente (log-odds por 1 DE)", y = "Redes al azar",
       caption = sprintf("Gris: %d redes al azar con los conocidos de cada trabajador y la popularidad de cada posicion fijos. Azul: redes observadas.", R_NULO))
guardar_fig(fig_n3, file.path(DIR_FIG, "fig_linea_base_n3"), w = 7.5, h = 3.2)
cat("Listo\n")

################################################################################
# DECISIONES METODOLOGICAS
# 1. N3 es la linea base de referencia: fija la actividad de cada trabajador y la
#    popularidad de cada posicion (margenes sin ponderar, conteos en tramos).
# 2. glm ponderado: mismo punto que svyglm; aqui solo interesan los coeficientes.
# 3. Semilla fija inmediatamente antes de r2dtable().
################################################################################
