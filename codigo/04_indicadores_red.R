# =============================================================================
# 04_indicadores_red.R
# Etapa 4 del pipeline: indicadores de red a nivel ego y a nivel ego×posición.
# Tesis Magíster en Sociología — PUC Chile | Trajan Pirkovic Palma
#
# VERSIÓN 2026-08-18. Cambios respecto a la versión anterior:
#   - FIX C3: Rango_P y Estatus_Max ahora indexan por match() explícito
#     (isei_gp_ordered), no por posición cruda de columna. La versión
#     anterior asumía que gp_crosswalk.csv está guardado en el mismo orden
#     que gp_r_vars (Q0101 a Q0127); si no lo está, Rango_P/Estatus_Max
#     quedaban mal calculados en silencio. isei_gp_ordered se movió de la
#     Acción 8 (SP_red_ego) hacia arriba, antes de la Acción 7, porque ahora
#     ambos indicadores la comparten.
#   - FIX C4: se agrega diagnóstico de completitud de shares_com*_red
#     (Acción 11). Si una sola posición del GP queda sin match en
#     gp_shares, 0*NA propaga NA a las cuatro columnas para TODOS los egos
#     que reportaron esa posición, sin ningún aviso previo.
#   - (Sin cambios de fondo respecto a la versión de agosto: Div_ego/Div_Red
#     en Margalef, Comp_Red -> Orient_ego, shares de comunidad, SP_red_ego,
#     cierre_blando.)
# Ver bloque de DECISIONES al final.
#
# AUTOCONTENIDO: lee sus insumos desde intermediate/ y data/.
# =============================================================================

suppressPackageStartupMessages({ library(tidyverse); library(igraph) })

DATA_DIR         <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/data"
INTERMEDIATE_DIR <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output/intermediate"
COMUNIDADES_DIR  <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/output/comunidades_por_ocupacion"

stopifnot(
  "Falta encuesta_prep.rds (etapa 01)" = file.exists(file.path(INTERMEDIATE_DIR, "encuesta_prep.rds")),
  "Falta ca_coords.rds (etapa 02/03)"  = file.exists(file.path(INTERMEDIATE_DIR, "ca_coords.rds")),
  "Falta ego_ca_isei.rds (etapa 03)"   = file.exists(file.path(INTERMEDIATE_DIR, "ego_ca_isei.rds")),
  "Falta osr_L2.rds (etapa 02)"        = file.exists(file.path(INTERMEDIATE_DIR, "osr_L2.rds")),
  "Falta osr_essential.rds (etapa 02)" = file.exists(file.path(INTERMEDIATE_DIR, "osr_essential.rds")),
  "Falta imputacion_comunidades.rds. Correr 13_imputar_comunidades_ocupacion.R" =
    file.exists(file.path(COMUNIDADES_DIR, "imputacion_comunidades.rds")),
  "Falta indicador_cierre_estructural.csv. Correr 15_indicador_cierre_estructural.R" =
    file.exists(file.path(COMUNIDADES_DIR, "indicador_cierre_estructural.csv"))
)

# ── ACCIÓN 1: cargar insumos ──────────────────────────────────────────────────
df_analitica <- readRDS(file.path(INTERMEDIATE_DIR, "encuesta_prep.rds"))
ca_obj       <- readRDS(file.path(INTERMEDIATE_DIR, "ca_coords.rds"))
ego_ca_isei  <- readRDS(file.path(INTERMEDIATE_DIR, "ego_ca_isei.rds"))
osr_L2       <- readRDS(file.path(INTERMEDIATE_DIR, "osr_L2.rds"))
osr_essential<- readRDS(file.path(INTERMEDIATE_DIR, "osr_essential.rds"))
gp_coords    <- ca_obj$gp_coords
gp_crosswalk <- read_csv(file.path(DATA_DIR, "gp_crosswalk.csv"), show_col_types = FALSE)

# ── GUARD NUEVO (20-ago-2026): ca_coords.rds debe ser la versión CA ponderada
# RM (388 filas de universo de ocupaciones), NUNCA la versión global sin
# ponderar (426 filas) que sobrescribió en silencio a la versión correcta en
# agosto 2026 (ver 00_reconciliar_ca_ponderado.R y bug log del proyecto).
# No se conoce con certeza el nombre exacto del campo de ca_obj que guarda el
# universo completo de ocupaciones (gp_coords son solo las 27 posiciones del
# generador) -- se imprime un diagnóstico con el nrow() de cada elemento de
# ca_obj para identificarlo. Una vez confirmado, fijar CAMPO_CA_UNIVERSO abajo
# para que el stopifnot() se active automáticamente en cada corrida futura.
CAMPO_CA_UNIVERSO <- "ca_coords"  # confirmado 20-ago-2026: diagnóstico mostró ca_coords=388, gp_coords=27
N_ESPERADO_RM <- 388L
if (!is.null(CAMPO_CA_UNIVERSO)) {
  n_ca_universo <- nrow(ca_obj[[CAMPO_CA_UNIVERSO]])
  if (n_ca_universo != N_ESPERADO_RM) {
    stop(sprintf(
      paste0("ca_coords.rds no tiene %d filas de universo de ocupaciones ",
             "(tiene %d) -- podria ser la version global (426, INCORRECTA) ",
             "en vez de la ponderada RM. Correr 00_reconciliar_ca_ponderado.R antes de continuar."),
      N_ESPERADO_RM, n_ca_universo
    ))
  }
} else {
  obtener_nrow <- function(x) {
    n <- tryCatch(nrow(x), error = function(e) NULL)
    if (is.null(n)) NA_integer_ else n
  }
  tams_ca <- vapply(ca_obj, obtener_nrow, FUN.VALUE = integer(1))
  tams_ca <- tams_ca[!is.na(tams_ca)]
  cat("\nDIAGNOSTICO ca_coords.rds -- nrow() de cada elemento de la lista (para ubicar",
      "el universo de ocupaciones; se espera 388 si es la version RM ponderada,",
      "426 seria sospechoso de ser la version global):\n")
  print(tams_ca)
  warning("CAMPO_CA_UNIVERSO no esta definido en 04_indicadores_red.R -- revisa el ",
          "diagnostico impreso arriba, identifica que campo de ca_obj trae el universo ",
          "de ocupaciones, y completa CAMPO_CA_UNIVERSO para activar la verificacion ",
          "automatica en la proxima corrida. Mientras tanto, el pipeline sigue SIN ",
          "garantia automatica de que ca_coords.rds sea la version RM ponderada.")
}

comunidades  <- readRDS(file.path(COMUNIDADES_DIR, "imputacion_comunidades.rds"))
cierre_tbl   <- read_csv(file.path(COMUNIDADES_DIR, "indicador_cierre_estructural.csv"),
                          show_col_types = FALSE)

shares_rm  <- comunidades$shares_rm
gp_shares  <- comunidades$gp_shares
n_com      <- comunidades$n_comunidades
share_cols <- paste0("share_com", seq_len(n_com))

stopifnot(
  "imputacion_comunidades.rds no trae 'grafo'/'B' -- correr 08 actualizado (fix D8, 19-ago-2026) antes de 04" =
    all(c("grafo", "B") %in% names(comunidades))
)
red_grafo <- comunidades$grafo
red_B     <- comunidades$B

gp_vars   <- paste0("Q0", sprintf("%03d", 101:127))
gp_r_vars <- paste0(gp_vars, "_r")

# ── ACCIÓN 1b: isei_gp_ordered — vector de ISEI del GP, alineado por match() ──
# MOVIDO desde la antigua Acción 8. Se calcula temprano porque ahora lo usan
# tanto Rango_P/Estatus_Max (Acción 7, fix C3) como SP_red_ego (Acción 8).
# NUNCA indexar gp_crosswalk$isei por posición cruda de fila/columna: el CSV
# no está garantizado a estar en el mismo orden que gp_vars/gp_r_vars.
stopifnot(
  "gp_crosswalk debe tener las 27 posiciones del generador" = nrow(gp_crosswalk) == length(gp_vars),
  "gp_crosswalk$var debe cubrir exactamente gp_vars, sin faltantes" =
    all(gp_vars %in% gp_crosswalk$var)
)
isei_gp_ordered <- gp_crosswalk |> arrange(match(var, gp_vars)) |> pull(isei)

# ── ACCIÓN 2: unir ISEI de ego/origen (etapa 03) a la encuesta ──────────────
df_analitica <- df_analitica |> left_join(ego_ca_isei, by = "ID")

# ── ACCIÓN 3: TamGrupo_p (CASEN 2024, RM) — opcional con respaldo ───────────
tam_path <- file.path(DATA_DIR, "tam_grupo_p_casen2024_RM.csv")
tiene_tamgrupo <- file.exists(tam_path)
if (tiene_tamgrupo) {
  tam_grupo_p <- read_csv(tam_path, show_col_types = FALSE)
  gp_coords <- gp_coords |> left_join(tam_grupo_p |> select(isco4, TamGrupo_p), by = "isco4")
  cat("TamGrupo_p disponible — cobertura en GP:", sum(!is.na(gp_coords$TamGrupo_p)), "de 27\n")
} else {
  gp_coords$TamGrupo_p <- NA_real_
  warning("tam_grupo_p_casen2024_RM.csv no encontrado — TamGrupo_p quedará NA.")
}

# ── ACCIÓN 4: Div_ego — diversidad de habilidades de la ocupación (MARGALEF) ──
#   Div_ego = (n_cat_distinct - 1) / ln(n_skills_total)
# Se conserva n_cat_distinct como Div_ego_cont (conteo bruto) para robustez.
ego_div_tbl <- osr_L2 |>
  select(isco4, L2_code) |> distinct() |> count(isco4, name = "n_cat_distinct") |>
  left_join(
    osr_essential |>
      left_join(osr_L2 |> select(skillUri, L2_code) |> distinct(), by = "skillUri") |>
      filter(!is.na(L2_code)) |>
      count(isco4, name = "n_skills_total"),
    by = "isco4"
  ) |>
  mutate(
    Div_ego      = if_else(n_skills_total > 1,
                            (n_cat_distinct - 1) / log(n_skills_total), NA_real_),
    Div_ego_cont = n_cat_distinct
  )

df_analitica <- df_analitica |>
  left_join(ego_div_tbl |> rename(isco_ego4 = isco4) |>
              select(isco_ego4, Div_ego, Div_ego_cont, n_cat_distinct, n_skills_total),
            by = "isco_ego4")

# ── ACCIÓN 5: shares de comunidad de la ocupación de ego (H3, H4b) ──────────
corr <- read_csv(file.path(DATA_DIR, "correcciones_isco_casen.csv"),
                  col_types = cols(.default = "c"), show_col_types = FALSE)
corr_map <- corr |>
  transmute(isco4_orig = suppressWarnings(as.integer(isco4_casen)),
            isco4_corr = suppressWarnings(as.integer(isco4_corregido))) |>
  filter(!is.na(isco4_orig))

df_analitica <- df_analitica |>
  left_join(corr_map, by = c("isco_ego4" = "isco4_orig")) |>
  mutate(isco4_ego_corr = as.character(coalesce(isco4_corr, as.integer(isco_ego4)))) |>
  select(-isco4_corr) |>
  left_join(
    shares_rm |> select(isco4, all_of(share_cols)) |>
      rename_with(~ paste0(.x, "_ego"), all_of(share_cols)),
    by = c("isco4_ego_corr" = "isco4")
  )

cat("Egos con shares de comunidad:",
    sum(!is.na(df_analitica[[paste0(share_cols[1], "_ego")]])), "de", nrow(df_analitica), "\n")

# ── ACCIÓN 6: construir base_larga (una fila por ego × posición del GP) ────
base_larga <- df_analitica |>
  select(ID, isco_ego4, isco4_ego_corr, Dim1_ego, Comp_ego, weight, educ, sexo, edad,
         oesch8, oesch16, ISEI_orig_hat, all_of(gp_r_vars)) |>
  pivot_longer(cols = all_of(gp_r_vars), names_to = "gp_var", values_to = "n_conocidos") |>
  mutate(var_orig = str_remove(gp_var, "_r")) |>
  left_join(gp_coords |> select(var, isco4, isei, Dim1_p, Dim2_p, TamGrupo_p),
            by = c("var_orig" = "var")) |>
  left_join(df_analitica |> select(ID, isei_ego_hat), by = "ID") |>
  mutate(
    SH_ip = -sqrt((Dim1_ego - Dim1_p)^2 + (Comp_ego - Dim2_p)^2),
    SP_ip = -abs(isei_ego_hat - isei),
    ID_f  = factor(ID)
  )
# SH_ip (CA) se conserva calculada por si se necesita de respaldo, pero deja
# de ser la especificación principal de H1/H1b desde SH_ip_red (Acción 6b) --
# ver Decisión 10.

# ── ACCIÓN 6b: SH_ip_red — similitud de habilidades ego-posición, en red ───
# NUEVO (19-ago-2026). Reemplaza a SH_ip (CA) como especificación principal
# de H1/H1b: distancia geodésica ponderada por phi (peso = 1 - phi) entre
# las categorías esenciales (RCA>1) de la ocupación de ego y las de cada
# posición del GP, sobre la MISMA red que produce las 4 comunidades Leiden
# (script 08). Lógica idéntica a la ya validada en
# robustez/R5_similitud_habilidades_red_vs_ca.R; aquí se aplica sobre
# red_grafo/red_B ya cargados desde imputacion_comunidades.rds (fix D8 de
# 08), sin reconstruir la red. Ver Decisión 10.
E(red_grafo)$dist_w <- 1 - E(red_grafo)$weight
D_geo <- distances(red_grafo, weights = E(red_grafo)$dist_w)
cats_por_isco <- apply(red_B, 1, function(fila) names(which(fila == 1)))

distancia_media_par <- function(isco_ego, isco_pos) {
  ce <- cats_por_isco[[as.character(isco_ego)]]
  cp <- cats_por_isco[[as.character(isco_pos)]]
  if (is.null(ce) || is.null(cp) || length(ce) == 0 || length(cp) == 0) return(NA_real_)
  sub <- D_geo[ce, cp, drop = FALSE]
  sub <- sub[is.finite(sub)]
  if (length(sub) == 0) return(NA_real_)
  mean(sub)
}

pares_unicos_red <- base_larga |>
  mutate(isco4_chr = as.character(as.integer(isco4))) |>
  distinct(isco4_ego_corr, isco4_chr) |>
  filter(!is.na(isco4_ego_corr), !is.na(isco4_chr)) |>
  rowwise() |>
  mutate(SH_ip_red = -distancia_media_par(isco4_ego_corr, isco4_chr)) |>
  ungroup()

base_larga <- base_larga |>
  mutate(isco4_chr = as.character(as.integer(isco4))) |>
  left_join(pares_unicos_red, by = c("isco4_ego_corr", "isco4_chr")) |>
  select(-isco4_chr)

cat("SH_ip_red calculado:", sum(!is.na(base_larga$SH_ip_red)), "de", nrow(base_larga),
    "díadas (resto NA por cobertura de red) —", sprintf("%.1f%%", 100 * mean(!is.na(base_larga$SH_ip_red))), "\n")

# ── ACCIÓN 7: indicadores clásicos del GP a nivel ego ──────────────────────
gp_matrix_r <- df_analitica |> select(all_of(gp_r_vars)) |> as.matrix()
df_analitica$Ext <- rowSums(gp_matrix_r, na.rm = TRUE)

# FIX C3: antes indexaba gp_crosswalk$isei[conocidos] por POSICIÓN de columna
# de gp_matrix_r, asumiendo (sin verificarlo) que gp_crosswalk.csv está en el
# mismo orden que gp_r_vars. Ahora usa isei_gp_ordered, ya alineado por
# match() en la Acción 1b — el mismo vector que ya usaba correctamente
# SP_red_ego (Acción 8) y Div_Red (Acción 9).
calc_isei_stats <- function(row_counts) {
  conocidos <- row_counts > 0 & !is.na(row_counts)
  if (sum(conocidos) == 0) return(c(rango = NA_real_, max_isei = NA_real_))
  iseis <- isei_gp_ordered[conocidos]
  c(rango = max(iseis) - min(iseis), max_isei = max(iseis))
}
isei_stats               <- t(apply(gp_matrix_r, 1, calc_isei_stats))
df_analitica$Rango_P     <- isei_stats[, "rango"]
df_analitica$Estatus_Max <- isei_stats[, "max_isei"]

# ── ACCIÓN 8: SP_red_ego — homofilia de prestigio ego-red (control de H3) ──
calc_sp_red <- function(counts, isei_ego) {
  w <- as.numeric(counts); w[is.na(w)] <- 0
  if (is.na(isei_ego) || sum(w) == 0) return(NA_real_)
  sum(w * (-abs(isei_ego - isei_gp_ordered))) / sum(w)
}
df_analitica$SP_red_ego <- mapply(calc_sp_red,
                                   split(gp_matrix_r, row(gp_matrix_r)),
                                   df_analitica$isei_ego_hat)

# ── ACCIÓN 9: Div_Red — diversidad del repertorio de la red (MARGALEF) ─────
cats_por_posicion <- osr_L2 |>
  select(isco4, L2_code) |> distinct() |>
  right_join(gp_crosswalk |> select(var, isco4), by = "isco4") |>
  filter(!is.na(L2_code))

skills_por_posicion <- osr_essential |>
  left_join(osr_L2 |> select(skillUri, L2_code) |> distinct(), by = "skillUri") |>
  filter(!is.na(L2_code)) |>
  count(isco4, name = "n_skills_pos") |>
  right_join(gp_crosswalk |> select(var, isco4), by = "isco4") |>
  arrange(match(var, gp_vars)) |>
  pull(n_skills_pos)

lista_cats_pos <- split(cats_por_posicion$L2_code, cats_por_posicion$var)[gp_vars]

calc_div_red_margalef <- function(counts) {
  w <- as.numeric(counts); w[is.na(w)] <- 0
  idx <- which(w > 0)
  if (length(idx) == 0) return(NA_real_)
  cats_union <- unique(unlist(lista_cats_pos[idx]))
  n_cat  <- length(cats_union)
  n_skl  <- sum(w[idx] * skills_por_posicion[idx], na.rm = TRUE)
  if (is.na(n_skl) || n_skl <= 1 || n_cat == 0) return(NA_real_)
  (n_cat - 1) / log(n_skl)
}
df_analitica$Div_Red <- apply(gp_matrix_r, 1, calc_div_red_margalef)

# ── ACCIÓN 10: Orient_ego (ex Comp_Red) — orientación sectorial de la red ──
gp_ca_mat <- gp_coords |> arrange(match(var, gp_vars)) |> select(Dim1_p, Dim2_p) |> as.matrix()
d_mat <- as.matrix(dist(gp_ca_mat))

calc_orient_ego <- function(counts) {
  w <- as.numeric(counts); w[is.na(w)] <- 0
  P <- length(w); num <- 0; den <- 0
  for (j in seq_len(P - 1)) for (k in (j + 1):P) {
    if (is.na(d_mat[j, k])) next
    ww <- w[j] * w[k]; num <- num + ww * d_mat[j, k]; den <- den + ww
  }
  if (den == 0) NA_real_ else num / den
}
df_analitica$Orient_ego <- apply(gp_matrix_r, 1, calc_orient_ego)

# ── ACCIÓN 11: shares de comunidad de la RED, ponderados (H3) ──────────────
gp_mat_shares <- gp_shares |> arrange(match(var, gp_vars)) |>
  select(all_of(share_cols)) |> as.matrix()

# FIX C4 (diagnóstico previo): 0 * NA = NA en R. Si UNA sola de las 27
# posiciones del GP queda sin match en gp_shares, las cuatro columnas
# share_com*_red quedan en NA para TODOS los egos que reportaron conocer esa
# posición, sin ningún aviso. Se verifica ANTES de calcular.
posiciones_sin_share <- gp_vars[apply(gp_mat_shares, 1, function(x) any(is.na(x)))]
if (length(posiciones_sin_share) > 0) {
  stop(
    "shares_red no se puede calcular con seguridad: las siguientes posiciones ",
    "del GP no tienen share de comunidad asignado en gp_shares: ",
    paste(posiciones_sin_share, collapse = ", "),
    ". Revisar 13_imputar_comunidades_ocupacion.R antes de continuar."
  )
}

calc_shares_red <- function(counts) {
  w <- as.numeric(counts); w[is.na(w)] <- 0
  if (sum(w) == 0) return(rep(NA_real_, ncol(gp_mat_shares)))
  colSums(w * gp_mat_shares) / sum(w)
}
shares_red_mat <- t(apply(gp_matrix_r, 1, calc_shares_red))
colnames(shares_red_mat) <- paste0(share_cols, "_red")
df_analitica <- bind_cols(df_analitica, as_tibble(shares_red_mat))

# Diagnóstico C4 (posterior al cálculo): cuántos egos quedan con las cuatro
# columnas completas vs. NA por no tener ninguna posición conocida.
cat("Egos con shares_red completo:",
    sum(complete.cases(shares_red_mat)), "de", nrow(shares_red_mat), "\n")

# ── ACCIÓN 12: unir cierre estructural blando (H4, H4b) ────────────────────
df_analitica <- df_analitica |>
  left_join(cierre_tbl |> select(ID, cierre_blando), by = "ID")

cat("Egos con cierre_blando:", sum(!is.na(df_analitica$cierre_blando)),
    "de", nrow(df_analitica), "\n")

# ── ACCIÓN 13: unir indicadores de nivel ego a base_larga ──────────────────
base_larga <- base_larga |>
  left_join(df_analitica |> select(ID, Ext, Rango_P, Estatus_Max, SP_red_ego,
                                    Div_Red, Orient_ego, Div_ego, cierre_blando),
            by = "ID")

# ── ACCIÓN 14: guardar salidas ────────────────────────────────────────────────
saveRDS(base_larga, file.path(INTERMEDIATE_DIR, "base_larga.rds"))

df_ego <- df_analitica |>
  select(ID, educ, edad, sexo, weight, oesch8, oesch16,
         isei_ego_hat, ISEI_orig_hat,
         Ext, Rango_P, Estatus_Max, SP_red_ego,
         Div_Red, Orient_ego, Div_ego, Div_ego_cont, Comp_ego,
         cierre_blando,
         all_of(paste0(share_cols, "_ego")),
         all_of(paste0(share_cols, "_red")))
saveRDS(df_ego, file.path(INTERMEDIATE_DIR, "df_ego.rds"))

cat("\nFilas base_larga:", nrow(base_larga), "| n df_ego:", nrow(df_ego), "\n")
cat("Guardado: intermediate/base_larga.rds, df_ego.rds\n")
cat("=== FIN ETAPA 04 ===\n")

# =============================================================================
# DECISIONES METODOLÓGICAS
# =============================================================================
# 1. Div_ego pasa de razón a índice de Margalef: (n_cat - 1) / ln(n_skills).
#    [sin cambios respecto a la versión anterior — ver changelog previo]
# 2. Div_Red pasa a Margalef sobre el REPERTORIO AGREGADO de la red.
#    [sin cambios]
# 3. Comp_Red se renombra a Orient_ego. [sin cambios]
# 4. SP_red_ego es nueva: promedio ponderado de -|ISEI_ego - ISEI_p|.
#    [sin cambios]
# 5. Los shares de comunidad de ego se unen aplicando la misma corrección de
#    código ISCO que usa 15_indicador_cierre_estructural.R. [sin cambios]
# 6. Los shares de comunidad de la RED se ponderan por conteos recodificados
#    en tramos (gp_r_vars). PENDIENTE DE REVISIÓN (C11 de la auditoría): si
#    se prefiere consistencia total con el cierre estructural (script 15,
#    que usa conteos crudos), unificar el criterio antes de reportar.
# 7. log_ingreso sale de base_larga y df_ego (H4 de ingreso descartada).
# 8. NUEVO — Rango_P y Estatus_Max ahora se calculan sobre isei_gp_ordered,
#    alineado por match(var, gp_vars) contra gp_crosswalk, en vez de indexar
#    por posición cruda de columna de gp_matrix_r. La versión anterior no
#    verificaba que gp_crosswalk.csv estuviera guardado en orden Q0101 a
#    Q0127; si no lo estaba, ambos indicadores quedaban mal calculados sin
#    ningún error ni aviso. Se agregó un stopifnot() que verifica que
#    gp_crosswalk cubre exactamente las 27 posiciones antes de continuar.
# 9. NUEVO — shares_com*_red ahora falla explícitamente (stop()) si alguna
#    de las 27 posiciones del GP no tiene share de comunidad asignado en
#    gp_shares, en vez de propagar NA en silencio a todos los egos que
#    conocen esa posición.
# 10. NUEVO (19-ago-2026). SH_ip_red reemplaza a SH_ip (CA) como
#     especificación principal de H1/H1b en 05_modelos.R. Motivo: con SH_ip
#     (coordenadas del CA) la interacción de H1b con educación no era
#     significativa (p=0.120); con SH_ip_red (distancia geodésica en la red
#     de complementariedad phi, la misma que sustenta las 4 comunidades) sí
#     lo es -- CONFIRMADO en la corrida completa de 05_modelos.R (19-ago-2026):
#     SH_ip_sc:educ_sc = -0.061*** (n=26.325 díadas, 975 egos). El -0.227***
#     citado inicialmente venía de robustez/R5_similitud_habilidades_red_vs_ca.R,
#     una reconstrucción aproximada (variables sin escalar, sin pendiente
#     aleatoria) que el propio R5 marcaba como pendiente de verificar contra
#     05 -- no usar ese número. SH_ip (CA) se mantiene calculada en
#     base_larga por si se necesita de respaldo, pero 05_modelos.R deja de
#     reportarla. Requiere que 08 haya corrido con el fix D8 (exporta
#     grafo/B en imputacion_comunidades.rds) antes que 04.
# 11. NUEVO (20-ago-2026). Se agrega guard de verificación de ca_coords.rds
#     (RM ponderada, 388 filas, vs. global sin ponderar, 426 filas -- ver
#     Acción 1). Queda con CAMPO_CA_UNIVERSO = NULL porque no se conoce el
#     nombre exacto del campo de la lista ca_obj que trae el universo
#     completo de ocupaciones; imprime un diagnóstico y emite warning() en
#     vez de fallar en silencio como antes. COMPLETAR CAMPO_CA_UNIVERSO tras
#     revisar el diagnóstico de la primera corrida, para que el stopifnot()
#     quede activo en las corridas siguientes.
# =============================================================================
