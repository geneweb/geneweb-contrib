#!/bin/bash
# Répertoire du dépôt à parcourir
repositoryDir="~/gw"

# Répertoires à parcourir dans le dépôt: /bin et /hd/etc
directories=("${repositoryDir}/lib" "${repositoryDir}/hd/etc")

# Fichier de sortie pour la liste d'icônes
outputFile="subset-icons.txt"
outputFileCond="subset-icons-2check.txt"

# Parcours récursif des répertoires
for dir in "${directories[@]}"; do
    echo "Analyzing files in $dir..."
    find "$dir" -type f \( -name "*.txt" -o -name "*.ml" \) -exec grep -Po 'class="fa[^ ]* fa-\K([^ "%]*%?)' {} + | \
        sed 's/^source://' | awk '!/-$/ {print}' | sort -u >> "$outputFile"
done

# Extract icon names ending with "%" to secondary file and remove them from main output file
grep -Po '.*%$' "$outputFile" >> "$outputFileCond"
sed -i '/%$/d' "$outputFile"

# Add icon with condition (handmade maintained list)
cat subset-icon-cond.txt >> "$outputFile"

# Remove "path/file:" prefix and sort the unique entries in the main output file
sed 's/^.*://' "$outputFile" | sort -u -o "$outputFile"

# Reformat the icon list for subset-font
sed "s/.*/'&', /" "$outputFile" | tr -d '\n' > "$outputFile.tmp" && mv "$outputFile.tmp" "$outputFile"
sed -i '$s/, *$//' "$outputFile"

echo "Liste d’icônes générée dans cat $outputFile. Vérifiez celles avec conditions dans $outputFileCond."
