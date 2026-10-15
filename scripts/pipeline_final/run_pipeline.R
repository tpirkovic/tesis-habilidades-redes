################################################################################
# run_pipeline.R   PIPELINE FINAL DE LA TESIS
#
# Corre los scripts en orden y guarda todo lo que imprimen en
# output_final/log_pipeline.txt. Cada script lee y escribe sus insumos en disco
# (output_final/intermedios/), asi que tambien se pueden correr por separado.
#
#   01_espacio_habilidades.R   universo RM, matriz MAPHA, red phi, Leiden, similitud
#   02_crosswalk_posiciones.R  27 posiciones: ISCO, ISEI, clase, habilidades, tamano
#   03_base_analitica.R        clase e ISEI de ego, diadas, indicadores de red, muestra
#   04_descriptivos.R          Tabla 2, quien conoce a quien, medidas por clase, figuras
#   05_modelos.R               OE1, M1 a M5 (H1 a H3), pendientes, robustez R1 a R7
#   06_linea_base.R            linea base estructural N3
#   07_figuras_articulo.R      figuras del articulo (prefijo art_), desde las salidas guardadas
#   08_grafo_bimodal.R         redes bimodales ocupacion - categoria (prefijo art_fig_bimodal_)
#
# Con SOLO_FIGURAS <- TRUE corre solo 07 y 08 (requiere una corrida previa de
# 01 a 06): sirve para regenerar las figuras tras cambiar el tema grafico.
#
# No modifica nada de data/, output_mapha/ ni output_paralelo_v3/.
# Si el log quedo abierto por una corrida interrumpida: sink(); closeAllConnections()
################################################################################

# Ubicacion de la carpeta pipeline_final: se detecta sola si el archivo se corre
# con source(); si se corre linea por linea, se usa RUTA_POR_DEFECTO (cambiarla
# en otro computador). Despues se fija el directorio de trabajo ahi.
RUTA_POR_DEFECTO <- "/Users/trajanpirkovic/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Tesis/scripts/pipeline_final"
DIR_PIPE <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NA_character_)
if (is.na(DIR_PIPE) || !file.exists(file.path(DIR_PIPE, "00_config.R"))) {
  DIR_PIPE <- if (file.exists("00_config.R")) normalizePath(getwd()) else RUTA_POR_DEFECTO
}
if (!file.exists(file.path(DIR_PIPE, "00_config.R"))) stop("No se encuentra pipeline_final en: ", DIR_PIPE)
setwd(DIR_PIPE)
DIR_OUT  <- file.path(dirname(DIR_PIPE), "output_final")
dir.create(DIR_OUT, recursive = TRUE, showWarnings = FALSE)
SOLO_FIGURAS <- FALSE
SCRIPTS <- c("01_espacio_habilidades.R", "02_crosswalk_posiciones.R", "03_base_analitica.R",
             "04_descriptivos.R", "05_modelos.R", "06_linea_base.R",
             "07_figuras_articulo.R", "08_grafo_bimodal.R")
if (SOLO_FIGURAS) SCRIPTS <- c("07_figuras_articulo.R", "08_grafo_bimodal.R")

options(width = 180, dplyr.summarise.inform = FALSE, pillar.width = 180)
con <- file(file.path(DIR_OUT, "log_pipeline.txt"), open = "wt", encoding = "UTF-8")
sink(con, split = TRUE)
cat("PIPELINE FINAL |", format(Sys.time(), "%Y-%m-%d %H:%M"), "| R", R.version$major, ".", R.version$minor, "\n")
ok <- TRUE
for (s in SCRIPTS) {
  cat("\n\n", strrep("#", 80), "\n# ", s, "\n", strrep("#", 80), "\n", sep = "")
  t0 <- Sys.time()
  res <- tryCatch({
    withCallingHandlers(source(file.path(DIR_PIPE, s), local = new.env(), echo = FALSE, encoding = "UTF-8"),
                        warning = function(w) { cat("[WARNING]", conditionMessage(w), "\n"); invokeRestart("muffleWarning") })
    TRUE
  }, error = function(e) { cat("\n[ERROR en ", s, "] ", conditionMessage(e), "\n", sep = ""); FALSE })
  cat(sprintf("\n[%s] %s en %.1f s\n", s, if (res) "OK" else "FALLO", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  if (!res) { ok <- FALSE; break }
}
cat("\n", if (ok) "PIPELINE COMPLETO" else "PIPELINE DETENIDO: revisar el error de arriba", "\n")
cat("\nVersiones de los paquetes:\n")
for (p in c("tidyverse", "dplyr", "readxl", "haven", "survey", "igraph", "ggraph", "tidygraph", "ggrepel", "ragg",
            "DIGCLASS", "glmmTMB"))
  cat(sprintf("  %-10s %s\n", p, if (requireNamespace(p, quietly = TRUE)) as.character(packageVersion(p)) else "no instalado"))
print(sessionInfo())
sink(); close(con)
