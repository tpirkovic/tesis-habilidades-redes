# Artículo: informe de avance de tesis (13 de octubre de 2026)

Proyecto Quarto que produce el informe de avance en PDF, compilado con LaTeX (xelatex). Vive en `articulo/`, junto a `scripts/`, y lee las cifras, tablas y figuras desde `scripts/output_final/`. **Ninguna cifra del texto se escribe a mano**: si el pipeline cambia, basta volver a compilar.

## Compilar

Desde la terminal, en esta carpeta:

```bash
quarto render              # genera _salida/tesis.pdf (y conserva el .tex)
quarto preview tesis.qmd   # vista previa que se actualiza al guardar
```

En VS Code: instalar la extensión **Quarto** (quarto.quarto), abrir `tesis.qmd` y usar *Quarto: Render* o *Quarto: Preview* (Cmd+Shift+K).

Antes de compilar hay que haber corrido `scripts/pipeline_final/run_pipeline.R`, porque el artículo lee `scripts/output_final/`.

## Requisitos

- Quarto 1.4 o superior.
- R con el paquete `knitr` (y `rmarkdown`). `R/cifras.R` usa solo R base.
- Una distribución de LaTeX con xelatex: MacTeX o TinyTeX (`quarto install tinytex`). Si falta algún paquete, TinyTeX lo instala solo.
- La fuente Times New Roman, que viene con macOS. En otro equipo, cambiar `mainfont` en `_quarto.yml` por "TeX Gyre Termes".

## Estructura

```
articulo/
├── _quarto.yml          formato PDF, xelatex, idioma, bibliografía y estilo APA
├── tesis.qmd            documento maestro: título, resumen e inclusión de capítulos
├── capitulos/           un archivo por sección del artículo
├── latex/preambulo.tex  paquetes y ajustes LaTeX (tablas y títulos APA, notas)
├── R/cifras.R           lee output_final y deja las cifras en `cf$...`
├── referencias.bib      bibliografía (desde las fichas del proyecto)
└── apa.csl              estilo APA 7 (Citation Style Language)
```

## Cómo escribir

- **Citas:** `[@bol2015]`, `[@bol2015; @oesch2017]`, o `@bol2015` para una cita narrativa. Las claves están en `referencias.bib`.
- **Cifras:** `` `r cf$h1` `` escribe 0,402. La lista completa está en `R/cifras.R`.
- **Referencias cruzadas:** `@tbl-modelos`, `@fig-pendientes-h2`.
- **LaTeX directo:** se puede escribir LaTeX en cualquier capítulo (`\notatabla{...}`, ecuaciones, `\newpage`).
- **Pendientes:** el texto entre `*[...]*` es andamiaje que se reemplaza al redactar.
- **Bibliografía:** el campo `verificar` de cada entrada de `referencias.bib` dice qué falta contrastar con la fuente. Antes de entregar, buscar `verificar` y resolverlo.
