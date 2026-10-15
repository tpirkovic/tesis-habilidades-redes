# =============================================================================
# cifras_extra.R: cifras adicionales para Datos y métodos y Resultados.
# Se carga al final de cifras.R (usa sus objetos: leer, num, pct, valor_p,
# coef_de, n3_de, log_pipeline, coef_m, ajuste, corr, quien, pend_h2, cf).
# Solo R base.
# =============================================================================

# Extrae un número del registro del pipeline con una expresión regular
del_log <- function(patron, grupo = 1) {
  linea <- grep(patron, log_pipeline, value = TRUE)[1]
  if (is.na(linea)) stop("No encuentro en log_pipeline.txt: ", patron)
  as.numeric(regmatches(linea, regexec(patron, linea))[[1]][grupo + 1])
}

# --- Espacio de habilidades y generador ---------------------------------------
cf$n_ocup_rm  <- num(del_log("Matriz RM: ([0-9]+) ocupaciones"), 0)
cf$n_aristas  <- num(del_log("red: [0-9]+ nodos, ([0-9]+) aristas"), 0)
cf$conocidos  <- num(del_log("conocidos mencionados = ([0-9]+)"), 0)
cf$n_con_ocup <- num(flujo$n[2], 0)

# --- Descriptivos -----------------------------------------------------------------
t2c <- leer("tabla2_continuas.csv")
t2f <- leer("tabla2_categoricas.csv")
media_t2 <- function(v) t2c$media[t2c$variable == v]
cf$hom_media  <- pct(100 * media_t2("Homogeneidad de clase (0 a 1)"), 1)
cf$dist_media <- num(media_t2("Distancia de estatus (puntos ISEI)"), 1)
cf$conoc_media <- num(media_t2("Conocidos en el generador"), 1)

cf$corr_hom_sh <- num(corr$SH[corr$variable == "hom"], 2)
cf$corr_hom_sp <- num(corr$SP[corr$variable == "hom"], 2)

# Tres medidas por clase
med <- leer("tres_medidas_por_clase.csv")
por_clase <- function(m, d) setNames(num(med$media[med$medida == m], d), med$clase[med$medida == m])
cf$hom_cl  <- por_clase("hom", 2)
cf$sh_cl   <- por_clase("SH_z", 2)
cf$isei_cl <- por_clase("dist_isei", 1)

# Observado sobre esperado (clase del trabajador a, clase de la posición b)
oe <- function(a, b) num(quien$obs_esp[quien$clase_ego == a & quien$class_pos == b], 2)

# --- OE1 y ajuste -------------------------------------------------------------------
cf$oe1_sp <- num(coef_de("OE1 SH", "SP_z"), 2)
cf$oe1_sh <- num(coef_de("OE1 SP", "SH_z"), 2)
r2_de <- function(m) num(ajuste$r2_o_devianza[ajuste$modelo == m], 3)
cf$r2_m1 <- r2_de("M1 base"); cf$r2_m2 <- r2_de("M2 H1"); cf$r2_m3 <- r2_de("M3 + estatus")

# Coeficientes de controles en M2 y M3 (para la lectura de la Tabla 3)
cf$univ_m2 <- num(coef_de("M2 H1", "educ5Universitaria"), 3)
cf$univ_m3 <- num(coef_de("M3 + estatus", "educ5Universitaria"), 3)

# --- H3: pendiente de la similitud de habilidades según similitud de estatus ------
b_sh  <- coef_de("M4 H3", "SH_z"); b_int <- coef_de("M4 H3", "SH_z:SP_z")
cf$h3_sh_media <- num(b_sh, 3)
cf$h3_sh_alta  <- num(b_sh + b_int, 3)
cf$h3_sh_baja  <- num(b_sh - b_int, 3)

# --- H2 -------------------------------------------------------------------------------
cf$h2_p <- valor_p(del_log("Test conjunto SH x clase: p = ([0-9.]+)"))
cf$pend_li <- setNames(num(pend_h2$li, 2), pend_h2$clase)
cf$pend_ls <- setNames(num(pend_h2$ls, 2), pend_h2$clase)

# --- Línea base N3 ------------------------------------------------------------------
cf$n3_h1_nula <- num(n3_de("M2: similitud de habilidades", "media_nula"), 3)
cf$n3_h1_p975 <- num(n3_de("M2: similitud de habilidades", "p97.5"), 3)
cf$n3_sh_nula <- num(n3_de("M3: similitud de habilidades", "media_nula"), 3)
cf$n3_sp_nula <- num(n3_de("M3: similitud de estatus", "media_nula"), 3)
cf$n3_int_nula <- num(n3_de("M4: interaccion", "media_nula"), 3)
cf$n3_int_p    <- num(n3_de("M4: interaccion", "p_empirico"), 2)
cf$n3_dif    <- num(del_log("estatus menos habilidades \\(M3\\): (-?[0-9.]+) pp"), 1)
cf$n3_dif_li <- num(del_log("estatus menos habilidades \\(M3\\): -?[0-9.]+ pp \\[(-?[0-9.]+);"), 1)
cf$n3_dif_ls <- num(del_log("estatus menos habilidades \\(M3\\): -?[0-9.]+ pp \\[-?[0-9.]+; (-?[0-9.]+)\\]"), 1)

# --- Robustez ---------------------------------------------------------------------------
rob <- leer("coef_robustez.csv")
rob_de <- function(prefijo, modelo, termino) {
  fila <- rob[startsWith(rob$muestra, prefijo) & rob$modelo == modelo & rob$termino == termino, ]
  if (nrow(fila) != 1) stop("No encuentro robustez ", prefijo, " / ", modelo, " / ", termino)
  num(fila$est, 3)
}
cf$r5_m2 <- rob_de("R5", "M2 H1", "SH_z")
cf$r5_m3 <- rob_de("R5", "M3 + estatus", "SH_z")
cf$r6_m2 <- rob_de("R6", "M2 H1", "SH_z")
cf$r2_m2_sinimp <- rob_de("R2", "M2 H1", "SH_z")

# --- Tablas adicionales --------------------------------------------------------------------
# Tabla 2: descriptivos ponderados de la muestra analítica
tabla2_texto <- function() {
  cont <- t2c[t2c$variable %in% c("Homogeneidad de clase (0 a 1)", "Similitud de habilidades (bruta)",
                                   "Distancia de estatus (puntos ISEI)", "Conocidos en el generador",
                                   "Mujer", "Edad", "Sin ingreso del trabajo", "Clase imputada"), ]
  binaria <- cont$variable %in% c("Mujer", "Sin ingreso del trabajo", "Clase imputada")
  etq_cont <- c("Homogeneidad de clase (0 a 1)" = "Homogeneidad de clase entre el trabajador y sus conocidos (0 a 1)",
                "Similitud de habilidades (bruta)" = "Similitud de habilidades entre el trabajador y sus conocidos (escala original)",
                "Distancia de estatus (puntos ISEI)" = "Distancia de estatus entre el trabajador y sus conocidos (puntos ISEI)",
                "Conocidos en el generador" = "Cantidad de conocidos en el generador",
                "Mujer" = "Mujer", "Edad" = "Edad (años)",
                "Sin ingreso del trabajo" = "Sin ingreso del trabajo", "Clase imputada" = "Clase imputada")
  a <- data.frame(Variable = unname(etq_cont[cont$variable]),
                  Valor = ifelse(binaria, pct(100 * cont$media, 1),
                                 paste0(num(cont$media, 2), " (", num(cont$de, 2), ")")),
                  stringsAsFactors = FALSE)
  orden_educ <- c("Basica o menos", "Media incompleta", "Media completa", "Tecnica superior", "Universitaria")
  etq_educ <- c("Básica o menos", "Media incompleta", "Media completa", "Técnica superior", "Universitaria")
  cl <- t2f[t2f$variable == "Clase Oesch (siete clases)", ]
  ed <- t2f[t2f$variable == "Nivel educativo", ]
  ed <- ed[match(orden_educ, ed$categoria), ]
  b <- data.frame(Variable = c("Clase (Oesch, siete clases)", paste0("    ", unname(clase_es[cl$categoria])),
                               "Nivel educativo", paste0("    ", etq_educ)),
                  Valor = c("", pct(cl$pct, 1), "", pct(ed$pct, 1)), stringsAsFactors = FALSE)
  out <- rbind(a, b)
  out$Variable <- sub("Distancia de estatus \\(puntos ISEI\\)", "Distancia de estatus (puntos ISEI)", out$Variable)
  names(out) <- c("", "Media (DE) o %")
  out
}

# Nombres de las clases en español
clase_es <- c("1. Higher-grade service class" = "Clase de servicio superior",
              "2. Lower-grade service class" = "Clase de servicio inferior",
              "3. Socio-cultural (semi) professionals" = "Profesionales socioculturales",
              "4. Clerks" = "Empleados administrativos",
              "5. Small business owners" = "Pequeños empresarios",
              "6. Skilled workers" = "Trabajadores calificados",
              "7. Unskilled workers" = "Trabajadores no calificados")
clase_corta <- clase_es

# Anexo: las 27 posiciones del generador
tabla_posiciones_texto <- function() {
  p <- leer("tabla_posiciones_gp.csv")
  p <- p[order(p$clase, -p$isei), ]
  cl <- unname(clase_es[p$clase]); cl[duplicated(cl)] <- ""
  data.frame(Clase = cl, Ocupación = p$ocupacion, ISCO = p$isco, ISEI = p$isei,
             `Gestión` = num(p$share_com1, 0), `Técnico` = num(p$share_com2, 0),
             `Analítico` = num(p$share_com3, 0), `Bio-amb.` = num(p$share_com4, 0),
             check.names = FALSE, stringsAsFactors = FALSE)
}

# Sección 5.4: categorías efectivas de tres posiciones del generador y su comunidad.
# Fuente: bimodal_gp_vinculos.csv (una fila por posición y categoría efectiva).
# Los nombres de las categorías son los de ESCO (nivel 2); el CSV los trae
# truncados a 40 caracteres, por eso se completan aquí a partir del código.
nombre_categoria <- c(
  "061" = "Tecnologías de la información y la comunicación", "091" = "Salud",
  "031" = "Ciencias sociales y del comportamiento", "042" = "Derecho",
  "S1.0" = "Comunicación, colaboración y creatividad", "S1.1" = "Negociar",
  "S1.2" = "Actuar como enlace y crear redes de contactos",
  "S1.4" = "Presentar información", "S1.5" = "Prestar asesoramiento y consultoría",
  "S1.7" = "Obtener información verbal", "S1.8" = "Trabajar con otros",
  "S2.1" = "Realizar estudios, investigaciones y análisis",
  "S2.2" = "Documentar y registrar información", "S2.3" = "Gestionar información",
  "S2.4" = "Procesar información", "S2.7" = "Analizar y evaluar información y datos",
  "S2.8" = "Realizar seguimiento, inspección y ensayos",
  "S3.2" = "Prestar atención sanitaria o administrar tratamientos médicos",
  "S3.3" = "Proteger y velar por el cumplimiento",
  "S3.4" = "Facilitar información y apoyo al público y a los clientes",
  "S4.1" = "Desarrollar objetivos y estrategias",
  "S4.2" = "Organizar, planificar y programar trabajo y actividades",
  "S5.1" = "Programar sistemas informáticos", "S6.2" = "Mover y elevar",
  "S6.3" = "Transformar y mezclar materiales", "S6.7" = "Utilizar herramientas manuales",
  "S7.1" = "Construir y reparar estructuras",
  "S7.3" = "Rematar estructuras interiores o exteriores",
  "S8.1" = "Operar maquinaria móvil", "T2.1" = "Procesar información, ideas y conceptos"
)
tabla_ejemplo_perfiles <- function() {
  v <- leer("bimodal_gp_vinculos.csv")
  p <- leer("tabla_posiciones_gp.csv")
  ocup <- c("Médicos o doctores", "Obreros de construcción o fábricas", "Abogados")
  filas <- lapply(ocup, function(o) {
    d <- v[v$ocupacion == o, ]
    d$nombre <- ifelse(d$L2_code %in% names(nombre_categoria),
                       nombre_categoria[d$L2_code], d$categoria)
    d$nombre <- paste0(d$nombre, " (", d$L2_code, ")")
    orden <- names(sort(table(d$comunidad), decreasing = TRUE))
    out <- do.call(rbind, lapply(orden, function(cm) {
      x <- d[d$comunidad == cm, ]
      data.frame(Comunidad = cm, `Categorías efectivas` = paste(x$nombre[order(x$L2_code)], collapse = "; "),
                 stringsAsFactors = FALSE, check.names = FALSE)
    }))
    out <- cbind(Ocupación = c(paste0(o, " (ISCO ", p$isco[p$ocupacion == o], ")"),
                               rep("", nrow(out) - 1)), out)
    out
  })
  res <- do.call(rbind, filas)
  rownames(res) <- NULL
  res
}

# --- Posiciones del generador (para citar perfiles en el texto) --------------------------
pgp <- leer("tabla_posiciones_gp.csv")
pos_isei <- function(q) num(pgp$isei[pgp$posicion == q], 0)
pos_sh   <- function(q, k) pct(pgp[[paste0("share_com", k)]][pgp$posicion == q], 1)

# Alias de las clases para el texto
HG <- "1. Higher-grade service class"; LG <- "2. Lower-grade service class"
SC <- "3. Socio-cultural (semi) professionals"; CL <- "4. Clerks"
SB <- "5. Small business owners"; SK <- "6. Skilled workers"; UN <- "7. Unskilled workers"

# Probabilidad empírica de la línea base para H1 (proporción de redes al azar con
# un coeficiente igual o mayor que el observado)
cf$n3_h1_p <- num(n3_de("M2: similitud de habilidades", "p_empirico"), 2)
