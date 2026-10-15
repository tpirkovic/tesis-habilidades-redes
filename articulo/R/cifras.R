# =============================================================================
# cifras.R: lee las salidas de pipeline_final y deja las cifras listas para el
# texto, con coma decimal. Se carga desde tesis.qmd con source("R/cifras.R").
#
# Regla: ninguna cifra del artículo se escribe a mano. En el texto se usa
# `r cf$h1` y no "0,402". Si el pipeline cambia, basta volver a compilar.
#
# Solo usa R base (más knitr para las tablas), para no sumar dependencias.
# =============================================================================

# --- Rutas --------------------------------------------------------------------
# Por defecto, este proyecto vive en Tesis/articulo y el pipeline en
# Tesis/scripts. Para otra ubicación, definir la variable de entorno
# TESIS_OUTPUT_FINAL con la ruta a output_final.
ruta_output <- Sys.getenv(
  "TESIS_OUTPUT_FINAL",
  unset = file.path("..", "scripts", "output_final")
)
ruta_tablas  <- file.path(ruta_output, "tablas")
ruta_figuras <- file.path(ruta_output, "figuras")

if (!dir.exists(ruta_tablas)) {
  stop("No encuentro output_final en: ", normalizePath(ruta_output, mustWork = FALSE),
       ". Corre pipeline_final o define TESIS_OUTPUT_FINAL.")
}

leer <- function(archivo) {
  read.csv(file.path(ruta_tablas, archivo), stringsAsFactors = FALSE,
           check.names = FALSE, encoding = "UTF-8")
}

# --- Formato ------------------------------------------------------------------
# num(0.4022, 3) -> "0,402"; num(1251, 0) -> "1.251"
num <- function(x, d = 3) {
  formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = ".")
}
pct <- function(x, d = 0) paste0(num(x, d), "%")
valor_p <- function(p) {
  if (p < 0.001) "p < 0,001" else if (p < 0.01) "p < 0,01" else
    if (p < 0.05) "p < 0,05" else paste0("p = ", num(p, 2))
}
# Convierte textos del pipeline ("0.402 (0.045)***") a coma decimal
coma <- function(x) gsub("(\\d)\\.(\\d)", "\\1,\\2", x)

# --- Tablas del pipeline --------------------------------------------------------
coef_m   <- leer("coef_modelos.csv")
ajuste   <- leer("ajuste_modelos.csv")
corr     <- leer("correlaciones.csv")
flujo    <- leer("flujo_muestral.csv")
n3       <- leer("linea_base_n3.csv")
pend_h2  <- leer("pendientes_h2.csv")
tabla3   <- leer("tabla3_modelos.csv")
tabla4   <- leer("tabla4_robustez.csv")
comun    <- leer("comunidades_caracterizacion.csv")
quien    <- leer("quien_conoce_a_quien.csv")

# Coeficiente de un modelo de la muestra principal
coef_de <- function(modelo, termino, campo = "est") {
  fila <- coef_m[coef_m$modelo == modelo & coef_m$muestra == "principal" &
                   coef_m$termino == termino, ]
  if (nrow(fila) != 1) stop("No encuentro ", modelo, " / ", termino, " en coef_modelos.csv")
  fila[[campo]]
}

# Línea base N3
n3_de <- function(termino, campo) n3[n3$termino == termino, campo]

# Modularidad: se lee del registro del pipeline
log_pipeline <- readLines(file.path(ruta_output, "log_pipeline.txt"), warn = FALSE)
linea_q <- grep("^Modularidad:", log_pipeline, value = TRUE)[1]
modularidad <- as.numeric(sub("^Modularidad: ([0-9.]+).*", "\\1", linea_q))

# n de la robustez sin clase imputada (R2), para obtener los imputados
n_r2 <- as.numeric(sub(".*\\[n=([0-9]+)\\].*", "\\1",
                       tabla4[grepl("^R2", tabla4$muestra), 2]))

# Observado sobre esperado de una clase consigo misma
obs_esp <- function(clase) {
  quien$obs_esp[quien$clase_ego == clase & quien$class_pos == clase]
}

# --- Cifras para el texto -------------------------------------------------------
n_analitica <- flujo$n[nrow(flujo)]

cf <- list(
  # Muestra
  n_encuestados = num(flujo$n[1], 0),
  n             = num(n_analitica, 0),
  n_imputados   = num(n_analitica - n_r2, 0),

  # Espacio de habilidades
  n_comunidades = length(comun$comunidad),
  n_categorias  = sum(comun$n_categorias),
  modularidad   = num(modularidad, 3),

  # Correlación entre ambas similitudes
  corr_sh_sp = num(corr$SP[corr$variable == "SH"], 2),

  # H1 (M2)
  h1    = num(coef_de("M2 H1", "SH_z"), 3),
  h1_ee = num(coef_de("M2 H1", "SH_z", "se"), 3),
  h1_pp = num(ajuste$pp_SH[ajuste$modelo == "M2 H1"], 1),

  # M3
  m3_sh    = num(coef_de("M3 + estatus", "SH_z"), 3),
  m3_sh_ee = num(coef_de("M3 + estatus", "SH_z", "se"), 3),
  m3_sp    = num(coef_de("M3 + estatus", "SP_z"), 3),
  m3_sp_ee = num(coef_de("M3 + estatus", "SP_z", "se"), 3),
  m3_pp_sh = num(ajuste$pp_SH[ajuste$modelo == "M3 + estatus"], 1),
  m3_pp_sp = num(ajuste$pp_SP[ajuste$modelo == "M3 + estatus"], 1),

  # H3 (M4)
  h3    = num(coef_de("M4 H3", "SH_z:SP_z"), 3),
  h3_ee = num(coef_de("M4 H3", "SH_z:SP_z", "se"), 3),
  h3_p  = valor_p(coef_de("M4 H3", "SH_z:SP_z", "p")),

  # Línea base N3 (parte relacional, %)
  n3_h1_rel = pct(n3_de("M2: similitud de habilidades", "parte_relacional_pct")),
  n3_sh_rel = pct(n3_de("M3: similitud de habilidades", "parte_relacional_pct")),
  n3_sp_rel = pct(n3_de("M3: similitud de estatus", "parte_relacional_pct")),

  # Descriptivos
  higher_obs_esp = num(obs_esp("1. Higher-grade service class"), 1)
)

# Pendientes de H2 por clase, en el orden del esquema de Oesch
pend_h2 <- pend_h2[order(pend_h2$clase), ]
cf$pendiente <- setNames(num(pend_h2$pendiente, 2), pend_h2$clase)

# --- Tablas para el texto -------------------------------------------------------
etiquetas_terminos <- c(
  "(Intercept)" = "Constante",
  "clase1. Higher-grade service class" = "Clase de servicio superior",
  "clase2. Lower-grade service class" = "Clase de servicio inferior",
  "clase3. Socio-cultural (semi) professionals" = "Profesionales socioculturales",
  "clase4. Clerks" = "Empleados administrativos",
  "clase5. Small business owners" = "Pequeños empresarios",
  "clase6. Skilled workers" = "Trabajadores calificados",
  "mujer" = "Mujer",
  "edad_sc" = "Edad (estandarizada)",
  "educ5Media incompleta" = "Media incompleta",
  "educ5Media completa" = "Media completa",
  "educ5Tecnica superior" = "Técnica superior",
  "educ5Universitaria" = "Universitaria",
  "ln_ing_c" = "Ingreso del trabajo (logaritmo)",
  "sin_ingreso" = "Sin ingreso del trabajo",
  "SH_z" = "Similitud de habilidades",
  "SP_z" = "Similitud de estatus",
  "SH_z:SP_z" = "Habilidades × estatus"
)

# Tabla 3: modelos anidados M1 a M4 (M5 va en la figura de pendientes)
tabla3_texto <- function() {
  t3 <- tabla3[!grepl("^SH_z:clase", tabla3$termino), c("termino", "M1 base", "M2 H1",
                                                        "M3 + estatus", "M4 H3")]
  t3$termino <- ifelse(t3$termino %in% names(etiquetas_terminos),
                       etiquetas_terminos[t3$termino], t3$termino)
  t3[] <- lapply(t3, function(x) coma(ifelse(is.na(x) | x == "NA", "", x)))
  names(t3) <- c("", "M1", "M2 (H1)", "M3", "M4 (H3)")
  t3
}

# Tabla 3 abreviada para el texto: similitudes, interacción y ajuste.
# La tabla completa (con clase y controles) va al apéndice con tabla3_texto().
tabla3_corta <- function() {
  t3 <- tabla3_texto()
  nombres <- names(t3)
  m <- as.matrix(t3)
  claves <- c("Similitud de habilidades", "Similitud de estatus", "Habilidades × estatus")
  m <- m[match(claves, m[, 1]), , drop = FALSE]
  r2 <- vapply(c("M1 base", "M2 H1", "M3 + estatus", "M4 H3"),
               function(x) num(ajuste$r2_o_devianza[ajuste$modelo == x], 3), character(1))
  m <- rbind(m, c("Clase y controles", rep("Sí", 4)), c("Devianza explicada", unname(r2)))
  out <- as.data.frame(m, stringsAsFactors = FALSE)
  names(out) <- nombres
  rownames(out) <- NULL
  out
}

# Tabla 4: robustez de H1. El n va en su propia columna para ganar ancho.
etiquetas_robustez <- c(
  "principal" = "Especificación principal",
  "R1" = "R1. Tres o más conocidos",
  "R2" = "R2. Sin clase imputada",
  "R3" = "R3. Descontada por disponibilidad (lineal)",
  "R4" = "R4. Control por número de conocidos",
  "R5" = "R5. Similitud de habilidades centrada",
  "R6" = "R6. Medida del equipo (tramos de estatus)",
  "R7" = "R7. Díadas con conocidos (multinivel)"
)
# kable en LaTeX con ancho máximo: envuelve el tabular en \ajustarancho{}
# (latex/preambulo.tex), que solo reduce la tabla si excede la caja de texto.
kable_ajustada <- function(...) {
  x <- knitr::kable(...)
  y <- sub("\\begin{tabular}", "\\ajustarancho{\\begin{tabular}", x, fixed = TRUE)
  y <- sub("\\end{tabular}", "\\end{tabular}}", y, fixed = TRUE)
  attributes(y) <- attributes(x)
  y
}

tabla4_texto <- function() {
  t4 <- tabla4
  clave <- ifelse(t4$muestra == "principal", "principal", sub("^(R[0-9]+).*", "\\1", t4$muestra))
  n <- sub(".*\\[n=([0-9]+)\\].*", "\\1", t4[[2]])
  sin_n <- function(x) coma(trimws(sub("\\[n=[0-9]+\\]", "", x)))
  out <- data.frame(unname(etiquetas_robustez[clave]), num(as.numeric(n), 0),
                    sin_n(t4[[2]]), sin_n(t4[[3]]), sin_n(t4[[4]]),
                    stringsAsFactors = FALSE, check.names = FALSE)
  names(out) <- c("Especificación", "n", "M2: habilidades", "M3: habilidades", "M3: estatus")
  rownames(out) <- NULL
  out
}

# Figura del pipeline (PDF vectorial)
# Si existe la versión para el artículo (art_*, generada por
# pipeline_final/07_figuras_articulo.R, con etiquetas en español y coma
# decimal), se usa esa; si no, la original del pipeline.
figura <- function(nombre) {
  art <- file.path(ruta_figuras, paste0("art_", nombre, ".pdf"))
  if (file.exists(art)) art else file.path(ruta_figuras, paste0(nombre, ".pdf"))
}

# Cifras adicionales para Datos y métodos y Resultados
source("R/cifras_extra.R", encoding = "UTF-8")

# Tabla 3 completa para el texto: similitudes, interacción, clase, controles y
# devianza explicada (las similitudes primero).
tabla3_completa <- function() {
  t3 <- tabla3_texto()
  m <- as.matrix(t3)
  prim <- c("Similitud de habilidades", "Similitud de estatus", "Habilidades × estatus")
  orden <- c(match(prim, m[, 1]), setdiff(seq_len(nrow(m)), match(prim, m[, 1])))
  m <- m[orden, , drop = FALSE]
  r2 <- vapply(c("M1 base", "M2 H1", "M3 + estatus", "M4 H3"),
               function(x) num(ajuste$r2_o_devianza[ajuste$modelo == x], 3), character(1))
  m <- rbind(m, c("Devianza explicada", unname(r2)))
  out <- as.data.frame(m, stringsAsFactors = FALSE)
  names(out) <- names(t3); rownames(out) <- NULL
  out
}

# Correlaciones ponderadas (triángulo inferior). Usa correlaciones_ampliadas.csv
# (pipeline_final/04_descriptivos.R, 12-10-2026) si existe; si no, solo las tres
# medidas de red de correlaciones.csv.
tabla_correlaciones <- function() {
  arch <- file.path(ruta_tablas, "correlaciones_ampliadas.csv")
  cm <- if (file.exists(arch)) leer("correlaciones_ampliadas.csv") else corr
  etq_todas <- c(hom = "Homogeneidad de clase", SH = "Similitud de habilidades",
                 SP = "Similitud de estatus", log_conocidos = "Cantidad de conocidos (log)",
                 isei = "ISEI del trabajador", educ = "Nivel educativo")
  v <- intersect(names(etq_todas), cm$variable)
  m <- as.matrix(cm[match(v, cm$variable), v]); k <- length(v)
  out <- matrix("", k, k)
  for (i in 1:k) for (j in 1:i) out[i, j] <- if (i == j) "1" else num(m[i, j], 2)
  out <- data.frame(paste0("(", 1:k, ") ", etq_todas[v]), out, stringsAsFactors = FALSE)
  names(out) <- c("", paste0("(", 1:k, ")"))
  out
}
