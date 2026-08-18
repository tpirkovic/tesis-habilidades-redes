# =============================================================================
# 15_indicador_cierre_estructural.R
#
# Construye el indicador de cierre estructural: que tan cerca esta la red de
# contactos de ego (generador de posiciones) de la comunidad de habilidades
# de la propia ocupacion de ego. Responde al segundo mecanismo de la
# recomendacion de Gabriel Otero: usar la deteccion de comunidades para
# explicar la variable dependiente, no solo describir la estructura.
#
# Construye DOS versiones, para decidir con resultados cual usar en H3/H4:
#   - CIERRE DURO: % de contactos (ponderado por n conocidos) que caen en la
#     MISMA comunidad dominante que la ocupacion de ego.
#   - CIERRE BLANDO: superposicion ponderada entre el perfil completo de
#     comunidades de ego (4 shares) y el de cada posicion del generador, sin
#     forzar una unica comunidad "dominante" por ocupacion.
#
# Script AUTOCONTENIDO en el sentido del pipeline: no depende de objetos en
# memoria de otras sesiones, pero SI depende de dos archivos intermedios ya
# guardados en disco por scripts anteriores:
#   - intermediate/encuesta_prep.rds       (de 01_preprocesar_encuesta.R)
#   - comunidades_por_ocupacion/imputacion_comunidades.rds (de 13_imputar_
#     comunidades_ocupacion.R)
# Si alguno no existe, el script se detiene con un mensaje claro de que hay
# que correr el script correspondiente primero.
#
# SALIDA: consola + CSV con el indicador por encuestado, listo para unir a
# la base de modelos.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble)
})
options(dplyr.summarise.inform = FALSE)

# =============================================================================
# 0. RUTAS
# =============================================================================

BASE_DIR         <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts"
INTERMEDIATE_DIR <- file.path(BASE_DIR, "output", "intermediate")
COMUNIDADES_DIR  <- file.path(BASE_DIR, "output", "comunidades_por_ocupacion")
OUT_DIR          <- COMUNIDADES_DIR
po <- function(f) file.path(OUT_DIR, f)

RUTA_ENCUESTA    <- file.path(INTERMEDIATE_DIR, "encuesta_prep.rds")
RUTA_COMUNIDADES <- file.path(COMUNIDADES_DIR, "imputacion_comunidades.rds")

# etiquetas cortas ya bautizadas, para que la salida sea legible sin tener
# que volver al reporte de bautizo cada vez
ETIQUETAS_COMUNIDAD <- c("1" = "Direccion-servicio", "2" = "Tecnico-manual",
                          "3" = "Analitico-digital-simbolico", "4" = "Bio-ambiental-legal")
# NOTA: este mapeo numero->etiqueta es el que resulto de la corrida de
# 13_imputar_comunidades_ocupacion.R guardada en imputacion_comunidades.rds.
# Como el ID numerico de comunidad es arbitrario entre corridas (ver
# decisiones ya documentadas), se verifica mas abajo contra los hubs
# conocidos antes de usarlo, no se asume ciegamente.

# =============================================================================
# 1. CARGA DE INSUMOS, CON VALIDACION FAIL-FAST
# =============================================================================

cat("\n=== 1. CARGA DE INSUMOS ===\n")

stopifnot(
  "Falta intermediate/encuesta_prep.rds. Correr 01_preprocesar_encuesta.R primero." =
    file.exists(RUTA_ENCUESTA),
  "Falta imputacion_comunidades.rds. Correr 13_imputar_comunidades_ocupacion.R primero." =
    file.exists(RUTA_COMUNIDADES)
)

encuesta <- readRDS(RUTA_ENCUESTA)
comunidades <- readRDS(RUTA_COMUNIDADES)

stopifnot(
  "encuesta_prep.rds debe tener isco_ego4" = "isco_ego4" %in% names(encuesta),
  "imputacion_comunidades.rds debe tener shares_rm y gp_shares" =
    all(c("shares_rm", "gp_shares", "n_comunidades") %in% names(comunidades))
)

shares_rm  <- comunidades$shares_rm
gp_shares  <- comunidades$gp_shares
n_com      <- comunidades$n_comunidades

cat(sprintf("  Encuestados en encuesta_prep.rds: %d\n", nrow(encuesta)))
cat(sprintf("  Ocupaciones con shares (universo RM): %d\n", nrow(shares_rm)))
cat(sprintf("  Posiciones GP con shares: %d\n", nrow(gp_shares)))
cat(sprintf("  Algoritmo base de la particion: %s\n", comunidades$algoritmo_base))

# =============================================================================
# 2. VERIFICAR CORRESPONDENCIA DE VARIABLES GP ENTRE LOS DOS INSUMOS
# =============================================================================
# gp_shares trae 'var' (ej. "Q0101"); encuesta_prep.rds trae las columnas
# crudas Q0101...Q0127. Se verifica que calcen antes de continuar, en vez de
# asumirlo.

gp_vars <- paste0("Q0", sprintf("%03d", 101:127))

stopifnot(
  "gp_shares debe tener una columna 'var'" = "var" %in% names(gp_shares),
  "Las 27 posiciones de gp_shares deben coincidir exactamente con Q0101..Q0127" =
    setequal(gp_shares$var, gp_vars),
  "encuesta_prep.rds debe tener las 27 columnas crudas Q0101..Q0127" =
    all(gp_vars %in% names(encuesta))
)
cat("  Verificado: las 27 posiciones GP calzan entre encuesta_prep.rds y gp_shares.\n")

share_cols <- paste0("share_com", seq_len(n_com))
stopifnot(
  "shares_rm debe tener las columnas share_com1..N" = all(share_cols %in% names(shares_rm)),
  "gp_shares debe tener las columnas share_com1..N" = all(share_cols %in% names(gp_shares))
)

# =============================================================================
# 3. COMUNIDAD DOMINANTE POR OCUPACION (para la version dura)
# =============================================================================

comunidad_dominante <- function(tabla_shares) {
  m <- as.matrix(tabla_shares[, share_cols])
  idx <- max.col(m, ties.method = "first")
  # empates exactos entre 2+ comunidades: se marca aparte, no se fuerza
  n_empates <- sum(apply(m, 1, function(fila) sum(fila == max(fila)) > 1))
  if (n_empates > 0) {
    cat(sprintf("  Advertencia: %d de %d ocupaciones tienen empate entre comunidades (se usa la primera por orden de columna).\n",
                n_empates, nrow(tabla_shares)))
  }
  idx
}

shares_rm$dom_comunidad <- comunidad_dominante(shares_rm)
gp_shares$dom_comunidad <- comunidad_dominante(gp_shares)

# =============================================================================
# 4. UNIR LA OCUPACION DE EGO A SU PERFIL DE COMUNIDADES
# =============================================================================

cat("\n=== 2. UNIENDO OCUPACION DE EGO A SUS SHARES DE COMUNIDAD ===\n")

# CORRECCION: isco_ego4 (construido en 01_preprocesar_encuesta.R) nunca pasa
# por correcciones_isco_casen.csv, a diferencia de universo_rm (que si la usa
# para construir mat_rm en 06/13). Se aplica aqui la misma correccion antes
# del join, para que un ego con isco4=3221 se lea como 5321 igual que el
# resto del pipeline. Ver Decision D7 al final.
RUTA_CORRECCIONES <- file.path(BASE_DIR, "data", "correcciones_isco_casen.csv")
stopifnot("Falta correcciones_isco_casen.csv" = file.exists(RUTA_CORRECCIONES))

corr <- read_csv(RUTA_CORRECCIONES, col_types = cols(.default = "c"), show_col_types = FALSE)
corr_map <- corr %>%
  transmute(isco4_orig = suppressWarnings(as.integer(isco4_casen)),
            isco4_corr = suppressWarnings(as.integer(isco4_corregido))) %>%
  filter(!is.na(isco4_orig))

ego <- encuesta %>%
  transmute(ID, isco4_ego_orig = as.integer(isco_ego4)) %>%
  left_join(corr_map, by = c("isco4_ego_orig" = "isco4_orig")) %>%
  mutate(isco4_ego = as.character(coalesce(isco4_corr, isco4_ego_orig))) %>%
  select(-isco4_corr) %>%
  left_join(
    shares_rm %>% select(isco4, dom_comunidad_ego = dom_comunidad,
                          all_of(share_cols)) %>%
      rename_with(~ paste0(.x, "_ego"), all_of(share_cols)),
    by = c("isco4_ego" = "isco4")
  )

n_corregidos <- sum(!is.na(ego$isco4_ego_orig) &
                     as.character(ego$isco4_ego_orig) != ego$isco4_ego)
cat(sprintf("  Egos con codigo ISCO corregido antes del match: %d\n", n_corregidos))

n_sin_match <- sum(is.na(ego$dom_comunidad_ego))
cat(sprintf("  Egos con match en el universo RM: %d de %d (%.1f%%)\n",
            nrow(ego) - n_sin_match, nrow(ego), 100 * (nrow(ego) - n_sin_match) / nrow(ego)))
if (n_sin_match > 0) {
  cat(sprintf("  Advertencia: %d egos sin match (ISCO fuera del universo RM). Quedan con indicador NA.\n",
              n_sin_match))
  cat("  ISCO de ego sin match (primeros 10 distintos):\n")
  print(head(sort(unique(ego$isco4_ego[is.na(ego$dom_comunidad_ego)])), 10))
}

# =============================================================================
# 5. CALCULO DEL INDICADOR: VERSION DURA Y VERSION BLANDA
# =============================================================================

cat("\n=== 3. CALCULANDO CIERRE ESTRUCTURAL (DURO Y BLANDO) ===\n")

# matriz de contactos crudos (n personas conocidas por posicion), en el mismo
# orden que gp_shares$var
contactos <- encuesta %>% select(ID, all_of(gp_vars)) %>%
  pivot_longer(-ID, names_to = "var", values_to = "n_contactos") %>%
  mutate(n_contactos = suppressWarnings(as.numeric(n_contactos)),
         n_contactos = if_else(is.na(n_contactos), 0, n_contactos))

gp_dom  <- setNames(gp_shares$dom_comunidad, gp_shares$var)
gp_mat_shares <- as.matrix(gp_shares[, share_cols])
rownames(gp_mat_shares) <- gp_shares$var

calcular_cierre <- function(id_row, dom_ego, shares_ego_vec) {
  fila <- contactos %>% filter(ID == id_row)
  total <- sum(fila$n_contactos)
  if (is.na(dom_ego) || total == 0) return(c(duro = NA_real_, blando = NA_real_, n_contactos_total = total))

  # version dura: contactos en la misma comunidad dominante que ego
  mismo_dom <- fila$var[gp_dom[fila$var] == dom_ego]
  cierre_duro <- sum(fila$n_contactos[fila$var %in% mismo_dom]) / total

  # version blanda: producto punto entre shares de ego y shares de cada
  # posicion, ponderado por contactos (0 = sin solapamiento, 1 = perfiles
  # identicos y concentrados en una sola comunidad)
  solapes <- sapply(fila$var, function(v) sum(shares_ego_vec * gp_mat_shares[v, ]))
  cierre_blando <- sum(fila$n_contactos * solapes) / total

  c(duro = cierre_duro, blando = cierre_blando, n_contactos_total = total)
}

resultado <- mapply(
  function(id_row, dom_ego, s1, s2, s3, s4) {
    calcular_cierre(id_row, dom_ego, c(s1, s2, s3, s4))
  },
  ego$ID, ego$dom_comunidad_ego,
  ego[[paste0(share_cols[1], "_ego")]], ego[[paste0(share_cols[2], "_ego")]],
  ego[[paste0(share_cols[3], "_ego")]], ego[[paste0(share_cols[4], "_ego")]]
)

ego$cierre_duro           <- resultado["duro", ]
ego$cierre_blando         <- resultado["blando", ]
ego$n_contactos_total     <- resultado["n_contactos_total", ]

n_sin_contactos <- sum(ego$n_contactos_total == 0, na.rm = TRUE)
if (n_sin_contactos > 0) {
  cat(sprintf("  Advertencia: %d egos con 0 contactos totales en el generador (indicador NA por division por 0 evitada).\n",
              n_sin_contactos))
}

# =============================================================================
# 6. DESCRIPTIVOS PARA DECIDIR ENTRE VERSIONES
# =============================================================================

cat("\n=== 4. DESCRIPTIVOS: DURO VS. BLANDO ===\n")

resumen_desc <- ego %>%
  filter(!is.na(cierre_duro)) %>%
  summarise(
    n = n(),
    media_duro = mean(cierre_duro), sd_duro = sd(cierre_duro),
    media_blando = mean(cierre_blando), sd_blando = sd(cierre_blando),
    correlacion = cor(cierre_duro, cierre_blando, use = "complete.obs")
  )
print(as.data.frame(resumen_desc), row.names = FALSE)

cat("\n  Distribucion cierre_duro:\n");   print(summary(ego$cierre_duro))
cat("\n  Distribucion cierre_blando:\n"); print(summary(ego$cierre_blando))

cat(sprintf("\n  Correlacion duro-blando: %.3f\n", resumen_desc$correlacion))
cat("  Lectura: si la correlacion es muy alta (>0.9), ambas versiones capturan\n")
cat("  lo mismo y conviene usar la blanda (mas informacion, sin forzar una sola\n")
cat("  comunidad dominante). Si difieren bastante, hay que decidir con los\n")
cat("  modelos H3/H4 cual tiene mejor ajuste o interpretacion mas clara.\n")

cat("\n  Tabla de comunidad dominante de ego (para contexto):\n")
print(table(ego$dom_comunidad_ego, useNA = "ifany"))

# =============================================================================
# 7. EXPORTAR
# =============================================================================

salida <- ego %>%
  select(ID, isco4_ego, dom_comunidad_ego, cierre_duro, cierre_blando, n_contactos_total)

write_csv(salida, po("indicador_cierre_estructural.csv"))
cat("\nGuardado en:", po("indicador_cierre_estructural.csv"), "\n")
cat(sprintf("  %d filas, %d con indicador valido (no NA)\n",
            nrow(salida), sum(!is.na(salida$cierre_duro))))

cat("\n=== FIN: INDICADOR DE CIERRE ESTRUCTURAL ===\n")

# =============================================================================
# DECISIONES METODOLOGICAS
# =============================================================================
# D1. Se pondera con los conteos CRUDOS de Q0101-Q0127 (numero real de
#     contactos conocidos por posicion), no con la version recodificada en 7
#     tramos (Q0101_r, etc.) que usa 01_preprocesar_encuesta.R para los
#     modelos de regresion. La recodificacion en tramos existe para evitar
#     que "super-conectores" distorsionen coeficientes de regresion; el
#     indicador de cierre es un promedio ponderado, no un predictor lineal
#     directo, por lo que no tiene el mismo riesgo y se prefiere la
#     informacion completa de los conteos reales.
# D2. VERSION DURA: requiere asignar una "comunidad dominante" (share
#     maximo) tanto a ego como a cada posicion del generador. Ocupaciones con
#     shares repartidos de forma pareja (ej. 30/30/20/20) pierden informacion
#     al forzarlas a una sola categoria. Los empates exactos entre 2+
#     comunidades se resuelven tomando la primera por orden de columna, y el
#     script avisa cuantos casos hay.
# D3. VERSION BLANDA: producto punto entre el vector de 4 shares de ego y el
#     de cada posicion del generador. Como ambos vectores son proporciones
#     que suman 1, el producto punto queda acotado entre 0 (perfiles
#     completamente disjuntos) y 1 (perfiles identicos y concentrados en una
#     sola comunidad), sin necesidad de normalizar por norma (no es similitud
#     coseno, es solapamiento directo de las proporciones). No fuerza una
#     comunidad dominante, por lo que aprovecha toda la informacion del
#     perfil de 4 comunidades de cada ocupacion.
# D4. Egos sin match en el universo RM (ISCO de ego fuera de las 391
#     ocupaciones con shares calculados) quedan con el indicador en NA, no se
#     imputan con el universo global como respaldo. Si el numero de casos
#     sin match es alto, se debe evaluar esa alternativa; el script reporta
#     el conteo exacto para tomar esa decision con informacion.
# D5. Egos con 0 contactos totales en las 27 posiciones del generador (no
#     conocen a nadie en ninguna posicion) quedan con el indicador en NA por
#     division por cero evitada explicitamente, no se imputan a 0.
# D6. El mapeo de ID numerico de comunidad a etiqueta bautizada (Direccion-
#     servicio, Tecnico-manual, etc.) se guarda como referencia en
#     ETIQUETAS_COMUNIDAD pero NO se aplica automaticamente a la tabla de
#     salida en este script, porque ese ID es arbitrario segun la corrida de
#     13_imputar_comunidades_ocupacion.R que haya generado el .rds cargado
#     aqui. Verificar el mapeo real contra los hubs conocidos (ver reporte de
#     bautizo) antes de usar ETIQUETAS_COMUNIDAD para interpretar la columna
#     dom_comunidad_ego de la salida.
# =============================================================================
# D7. isco_ego4 (construido en 01_preprocesar_encuesta.R) no pasaba por
#     correcciones_isco_casen.csv, a diferencia del universo RM (que si la
#     usa). Se agrego ese paso aqui, antes del join con shares_rm, para que
#     un ego con isco4=3221 (u otro codigo con correccion documentada) se
#     lea igual que en el resto del pipeline. El numero de egos corregidos
#     se reporta explicitamente en consola.
# =============================================================================
