# Presentación de avances ante la comisión · 7 de octubre de 2026

Abrir `presentacion_comision_2026-10-07.qmd` en RStudio y Render. Requiere R (solo base) para
la lámina "27 ocupaciones". No lee datos del pipeline: todo está en `images/`.
Notas del expositor en cada lámina (tecla "s").

## Estructura (25 láminas de contenido, unos 25 minutos)

1. Portada y recorrido (cada bloque con la pregunta que responde)
2. Problema de investigación: desigualdad que se reproduce en redes; fronteras medidas en vertical; mismo estatus, distinto trabajo (caso de contraste con datos del GP)
3. Problema teórico en cuatro ideas: habilidades que definen posiciones; posiciones que se vuelven fronteras; fronteras visibles en redes; redes que reflejan estructura. Luego cuatro ideas en diálogo y problema en síntesis
4. Propósito, pregunta, objetivos, hipótesis OE2 y OE3
5. Datos y métodos: encuesta y GP; de ocupaciones a habilidades; 27 ocupaciones; tres medidas, cuatro personas; estrategia de análisis
6. Descriptivos: tres medidas por clase; tres medidas por nivel educativo
7. Resultados preliminares: H1 (Modelos C1 y C2); síntesis y próximos pasos
8. Referencias (2 láminas) y respaldo: tres medidas de red, quién conoce a quién, C1 y C2 completos, robustez, Modelos A, B y D, muestra

Sin redes barajadas en ninguna lámina: la descomposición de H1 figura solo como análisis "en desarrollo".

## Archivos en images/

| Archivo | Origen |
|---|---|
| `fig_quien_conoce_a_quien.png` | `d1_clase_x_clase.csv` (script 122), columna `oe` |
| `gp27.csv` | `crosswalk_gp27_v3.csv`: posición, etiqueta corta, ISEI, clase Oesch7, código de habilidades |
| `gp27_comunidades.csv` | `d3_gp_shares.csv` (script 122) con nombres vigentes de comunidades |
| `logo_negro.png` | tema |

Tablas de descriptivos: valores de `desc_por_clase.csv` y `d6_educacion.csv`, escritos en el qmd.
Coeficientes: manual técnico (scripts 113, 123, 124 y 126).

## Sobre las porciones de comunidades

Son porciones de las **categorías de habilidades** del perfil ESCO de cada ocupación, no de habilidades
sueltas. Algunos perfiles son delgados: repartidores (2 categorías), médicos (3), trabajadores agrícolas (6),
carabineros (7). Por eso los ejemplos de las láminas usan electricistas y secretarios (12 y 11 categorías)
y gerentes e ingenieros (17 y 19). Queda anotado en las notas del expositor de la lámina "27 ocupaciones".

## Cambios en el tema (libs/tesis_redes.scss)

Al final: colores de comunidades (`.c-tec`, `.c-ges`, `.c-ana`, `.c-bio`, deben coincidir con
`COM_HEX` en el qmd) y tabla compacta `.tabla-gp`.

## Por verificar

- Año de levantamiento de la encuesta Fondecyt (2025)
- Otero, Plaza y Zavala-Aránguiz (2026): DOI tomado de la conversación de antecedentes
- Versión de ESCO usada (hoy citada como "s. f.")
- Ajuste vertical tras renderizar: "Desigualdad que se reproduce en redes", "27 ocupaciones", "Estrategia de análisis" y respaldo C1 y C2
- Citas agregadas desde la conversación de antecedentes (DOI incluidos): DiMaggio y Garip (2012), Lindh et al. (2021), Tomaskovic-Devey y Avent-Holt (2019), Torche (2014)

Verificado: Casen 2022 (Gini 0,470 e índice 20/20 de 8,2 corresponden al **ingreso monetario**);
Flores et al. (2020), pp. 850–874; título de Espinosa-Rada et al. (2026); Otero et al. (2021)
miden el estatus de los contactos con ISEI; redacción de la pregunta del GP (listado de variables).
