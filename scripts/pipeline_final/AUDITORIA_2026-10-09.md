# Auditoría del pipeline: 9 de octubre de 2026

Objeto de la auditoría: la cadena que produce las bases `output_paralelo_v3/datos/base_ego_v3.rds` y `base_diadas_v3.rds`, de las que salen todos los resultados vigentes. Hoy esa cadena es:

`codigo_v2/02` y `08` (MAPHA), `codigo_v2/01` y `04`, `107`, `paralelo_v3/111`, `112` y `128`.

Método: cada eslabón se recalculó de forma independiente a partir de los archivos crudos, sin usar R, y se comparó con lo que guardó el pipeline. Los archivos crudos son la encuesta Fondecyt en xlsx, MAPHA v2.06, ESCO, la tabla ISEI de DIGCLASS y las correcciones CIUO.

## 1. Lo que se verificó y está correcto

| Eslabón | Verificación | Resultado |
|---|---|---|
| Matriz ocupación × categoría L2 y matriz B (RCA > 1) | Reconstruida desde MAPHA y ESCO crudos | Idéntica: 391 × 112, 0 celdas distintas |
| Red φ | Recalculada desde B | Mismas 2.836 aristas; diferencia máxima en φ de 1e-14 |
| Comunidades | Modularidad de la partición guardada | 0,3875 (se reporta 0,388) |
| Estabilidad de la partición | 20 corridas de Louvain | Q entre 0,377 y 0,387; ARI entre 0,66 y 0,93 respecto de Leiden |
| Similitud de habilidades por díada | Recalculada: distancias geodésicas con pesos 1 − φ | Idéntica en 26.406 díadas; mismos 1.350 faltantes |
| Conocidos por posición | Tramos recalculados desde la encuesta | 0 diferencias en 27.756 díadas |
| ISEI de las 27 posiciones | Contra la tabla DIGCLASS | 0 diferencias |
| Oesch16 de las 27 posiciones | Contra `isco08_to_oesch16` de DIGCLASS (asalariado; Q0111 cuenta propia con empleados) | 27 de 27 coinciden |
| Clase de los 787 ocupados | Contra `class16_r` del equipo en la base | 100% de coincidencia |
| Clase imputada de los 193 no ocupados | Contra la tabla DIGCLASS con el tipo de empleo de Q4404 | 193 de 193 coinciden |
| Colapso a siete clases | Contra el script del equipo (`class_oesch7`) | Idéntico |
| Misma clase y homogeneidad de clase | Recalculadas | 0 diferencias |
| Ponderador | Contra `weight_m` de la base | Idéntico |
| Coeficientes del 7 de octubre | Reestimados con un logit fraccional programado aparte | 0,402, 0,286, 0,454 y 0,049, con los mismos errores estándar |

Flujo muestral, que ahora queda completamente explicado:

| Paso | N |
|---|---|
| Encuestados | 1.251 |
| Con ocupación actual o última | 1.028 |
| Con clase | 980 (46 con códigos de no ocupación 9990/9998/9999 y 2 con códigos ISCO inexistentes, 2429 y 7415) |
| Con perfil de habilidades y conocidos | 974 (2 con el código 2340, que no es un grupo unitario y no tiene perfil; 4 sin conocidos en el generador) |
| Muestra analítica | 971 (3 sin edad) |

## 2. Hallazgos

**A. Error que cambia un resultado: el ISEI de 17 TENS.** A los egos codificados 3221 (técnicos y auxiliares paramédicos de enfermería) se les aplicó la corrección 3221 a 5321, que solo se necesita para las habilidades, porque ESCO no tiene ocupaciones en 3221. Con eso recibieron el ISEI de 5321, que es 28, en vez del ISEI de 3221, que es 48. La posición Q0116 sí usa 48, y el script del equipo deja 3221 en la clase 14 en todos los casos. El pipeline final corrige esto: ISEI y clase con el código original. Efecto, reestimado:

| Coeficiente | 7 de octubre | Corregido |
|---|---|---|
| H1, similitud de habilidades (M2) | 0,402 (0,045) | 0,402 (sin cambio) |
| M3, similitud de habilidades | 0,286 (0,042) | 0,270 (0,041) |
| M3, similitud de estatus | 0,454 (0,047) | 0,487 (0,044) |
| H3, interacción (M4) | 0,049 (0,043), n.s. | 0,085 (0,033), p ≈ 0,01 |
| Correlación entre similitud de habilidades y de estatus | 0,229 | 0,257 |

Con la corrección, H3 pasa a sostenerse: la asociación de la similitud de habilidades con la homogeneidad de clase es mayor cuando también hay similitud de estatus. Esto cambia la lectura de "frontera horizontal" que se presentó a la comisión, y conviene conversarlo con Gabriel.

**B. Propiedad de la medida de similitud de habilidades.** La medida es menos la distancia media entre todas las categorías de dos perfiles. Con esa construcción, una ocupación no siempre es la más parecida a sí misma: pasa en 8 de las 27 posiciones. Por ejemplo, para la medida un conductor de camión se parece más a un trabajador agrícola que a otro conductor. Los perfiles compactos, con pocas categorías, actúan como atractores. El pipeline agrega una versión centrada como robustez (R5, distancia de energía), con la que cada ocupación sí es siempre la más parecida a sí misma. H1 se mantiene (0,330, EE 0,044; r = 0,69 con la medida original) y el patrón de H2 se repite. Con estatus en el modelo, la similitud de habilidades baja más que con la medida original (0,170). Conviene declararlo en métodos y reportar R5.

**C. La homogeneidad de clase del equipo es otra medida.** En el script de Gabriel ("networks and class awareness"), `classhomog` usa tres tramos de estatus en 24 posiciones (deja fuera a mecánicos, trabajadores agrícolas y electricistas) y asigna cada clase a un tramo. Con esa medida H1 se mantiene: 0,408 (EE 0,043); con estatus, 0,274. Correlación con nuestra medida: 0,60. Queda como robustez R6.

**D. La clase "final" del equipo es la del sostenedor del hogar.** En ese mismo script, `class16` toma la clase del sostenedor cuando existe (`class16_p`) y, si no, la propia (`class16_r`). La tesis usa la clase propia. Es coherente con medir la similitud del trabajo de la persona, pero conviene decirlo explícitamente y confirmarlo con Gabriel.

**E. Problemas de procedencia.** El script `107`, que produce la clase de ego (`output_mapha/clase_oesch7_v2/`), no está en la carpeta. `crosswalk_gp27_FINAL.csv` se construyó en una conversación y tampoco tiene script. `codigo_v2/` y `paralelo_v3/` no están en git. El pipeline final reemplaza a `107`: lee la clase directo de la base. Las decisiones del crosswalk quedan escritas en `02`.

**F. Documentación desactualizada.** `CONTEXTO_PROYECTO.md` y el encabezado de `codigo_v2/08_..._mapha.R` dicen que la partición vigente es la de ESCO, pero todo v3 usa la de MAPHA. La regla de "un salto" vale para las habilidades ESCO; las 737 habilidades Relink se asignan subiendo 1 a 4 niveles por la jerarquía de MAPHA. `000_tema_tesis.R` usaba los nombres antiguos de las comunidades; ya está actualizado en `00_tema_graficos.R`.

**G. Menores.**
- `sin_ingreso` reúne a los 189 no ocupados y a 15 ocupados sin dato de ingreso, así que funciona casi como un indicador de no ocupación.
- Hay cuatro egos con códigos ISCO inválidos (2340, 2429, 7415) que podrían recodificarse.
- La base trae `Zona` y `folio_m`, de modo que el diseño muestral podría declararse cuando el equipo lo confirme.

## 3. Pendientes de decisión

1. Si se adopta la corrección del ISEI de los TENS (recomendado) y cómo se reescribe la lectura de H3.
2. Si R5 (similitud centrada) y R6 (medida del equipo) entran al cuerpo del texto o al anexo.
3. Clase propia o clase del sostenedor (D).
