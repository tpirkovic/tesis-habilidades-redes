# Presentación de avances ante la comisión · 7 de octubre de 2026

Abrir `presentacion_comision_2026-10-07.qmd` en RStudio o Positron y Render. No lee datos del pipeline: todo está en `images/`.
Notas del expositor en cada lámina (tecla "s").

## Estructura (27 láminas de contenido, unos 22 minutos) · versión con comentarios de Gabriel

Diseño vigente: una sola variable dependiente (homogeneidad de clase); ingreso como control.

- **Pregunta:** ¿Cómo interactúan el nivel socioeconómico de los trabajadores y su similitud de habilidades laborales y de estatus con la homogeneidad de clase de sus redes personales, en la Región Metropolitana en 2025?
- **OE1 (describir):** dos regresiones lineales cruzadas (similitud de habilidades ~ similitud de estatus, y al revés).
- **OE2 · H1:** a mayor similitud de habilidades, mayor homogeneidad de clase, más allá de características socioeconómicas. Sostenida (0,402; +7,3 pp).
- **OE3 · H2:** asociación mayor en clases manuales que en servicio. No sostenida (más fuerte en Socio-cultural y Clerks; casi nula en Skilled).
- **OE3 · H3:** asociación mayor cuanto mayor la similitud de estatus. No sostenida (interacción 0,049, p = 0,25): lectura horizontal.
- **Controles en todos los modelos:** clase, nivel educativo, ingreso (log y sin ingreso), sexo y edad. Sin número de conocidos.
- **Tensión 2** reformulada: "¿frontera horizontal o vertical?", respondida por H3.

Estimaciones del 7 de octubre de 2026 en consola, replicando la construcción de datos del script 123 (chequeos: 0,378 y 0,279 de la especificación anterior). No están guardadas: conviene pasarlas a un script del pipeline con su bloque de decisiones metodológicas. Efectos en pp: cambio medio en la homogeneidad predicha al sumar 1 DE (la especificación anterior reportaba 6,5 pp con pendiente instantánea; con este método daba 6,8).

## Figuras en images/ (rehechas con rótulos vigentes)

| Archivo | Origen |
|---|---|
| `fig_gp27_comunidades.png` | `d3_gp_shares.csv` (script 122) + `crosswalk_gp27_v3.csv` |
| `fig_trabajo_red_clase.png` | **Reconstruida** desde los porcentajes rotulados de `fig_d2_shares_clase.png`; bio-ambiental-legal = resto (error de redondeo de ±1 pp). Si existe el CSV de esa figura, conviene regenerarla exacta |
| `fig_clase_pct.png`, `fig_clase_oe.png` | `d1_clase_x_clase.csv` (columnas `pct` y `oe`) |
| `fig_tres_medidas_por_clase.png` | `desc_por_clase.csv` (medias e IC 95%) |
| `fig_h2_pendientes_clase.png` | Pendientes de la similitud de habilidades por clase (regresión de H2, `svycontrast`), IC 95% |
| `fig_correlaciones.png` | Valores de `fig_corr_v3.png` (correlaciones ponderadas, N = 971), sin "cierre controlado" |
| `pintana_costanera.png` | Fotografía de La Pintana con la torre Costanera al fondo (falta crédito) |
| `logo_negro.png` | tema |

Cambios de rótulos respecto de las figuras del 30-09: comunidades con nombres vigentes (técnico-material, gestión y cuidado, analítico-simbólico, bio-ambiental-legal), "trabajador" en vez de "ego", "homogeneidad de clase" en vez de "cierre", secretarios y parvularios como en el GP.

## Cambios en el tema (libs/tesis_redes.scss)

Al final: colores de comunidades para texto (`.c-tec`, `.c-ges`, `.c-ana`, `.c-bio`), tabla compacta `.tabla-gp` (ya sin uso), y para la versión aligerada `.cita`, `.cifra`, `.lista-aire` `.idea` (franja roja; `.azul-bar` la deja azul), `.hipotesis` (hipótesis grandes), `.bajada` (línea "en simple" bajo cada hipótesis) y `.ficha.dep` (variable dependiente, franja roja).

## Por verificar

- Año de levantamiento de la encuesta Fondecyt (2025)
- Otero, Plaza y Zavala-Aránguiz (2026): DOI tomado de la conversación de antecedentes
- Versión de ESCO usada (hoy citada como "s. f.")
- PNUD (2017), *Desiguales*: referencia agregada; confirmar editorial (PNUD / Uqbar Editores)
- Ajuste vertical tras renderizar: "Hipótesis: ingreso laboral", "Variables independientes", "27 posiciones", "H1" y respaldo C1 y C2
- Citas agregadas desde la conversación de antecedentes (DOI incluidos): DiMaggio y Garip (2012), Lindh et al. (2021), Tomaskovic-Devey y Avent-Holt (2019), Torche (2014)

Verificado: Casen 2022 (Gini 0,470 e índice 20/20 de 8,2 corresponden al **ingreso monetario**);
Flores et al. (2020), pp. 850–874; título de Espinosa-Rada et al. (2026); Otero et al. (2021)
miden el estatus de los contactos con ISEI; redacción de la pregunta del GP (listado de variables).

**Coeficientes de "Resultado OE1: espejo de habilidades"**: `output_paralelo_v3/modelos_espejo/espejo_coeficientes.csv` (script 124). Solo trae el coeficiente espejo por especificación; los controles entran por comunidad y no se reportan.

**Coeficientes de "Resultado OE1: similitud de habilidades"**: `output_paralelo_v3/modelos_cobertura/coef_cobertura.csv`, modelo "M0 original" (svyglm); R² de `tabla_cobertura.csv`. Con oferta del GP ("M0a + SH esperada") los coeficientes casi no cambian.

**Columna de estatus en "Resultado OE1: dos similitudes"**: estimada el 7 de octubre replicando el script 123 (mismos datos; chequeos: universitaria 0,357 en habilidades y similitud de habilidades 0,277 en estatus). No está guardada: conviene agregarla a la lista `mods` del script 123 como `SP_z ~ shares` (y, como respaldo, `SP_z ~ shares + SP_esp_z`).
