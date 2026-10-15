#!/bin/bash
# Prepara un .zip del manuscrito para subirlo a Overleaf (New project > Existing project).
# Usa el tesis.tex que deja quarto render (keep-tex) y copia las figuras y el logo.
# Uso, desde la carpeta articulo:   bash preparar_overleaf.sh
# Resultado: _salida/tesis_overleaf.zip
set -e
cd "$(dirname "$0")"
DEST="_salida/overleaf"
FIG="${FIG:-../scripts/output_final}"   # carpeta que contiene figuras/
rm -rf "$DEST" && mkdir -p "$DEST/figuras" "$DEST/img"

# Rutas de figuras: de ../scripts/output_final/figuras/ a figuras/
sed 's#\.\./scripts/output_final/figuras/#figuras/#g' tesis.tex > "$DEST/main.tex"

# Copia solo las figuras que el .tex usa
grep -o 'figuras/[^}]*' "$DEST/main.tex" | sort -u | while read -r f; do
  cp "$FIG/$f" "$DEST/$f"
done
cp img/logo-uc-escudo.png "$DEST/img/"
cp referencias.bib "$DEST/"

(cd "$DEST" && rm -f ../tesis_overleaf.zip && zip -rq ../tesis_overleaf.zip .)
echo "Listo: _salida/tesis_overleaf.zip ($(ls "$DEST/figuras" | wc -l | tr -d ' ') figuras)"
