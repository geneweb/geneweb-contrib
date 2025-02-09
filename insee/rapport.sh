#!/bin/bash

MYSQL=./mysql.sh
gwserver="http://192.168.2.57:2317"

# Optional database name parameter
DB_NAME="$1"

# Define output file based on DB_NAME
if [ -n "$DB_NAME" ]; then
   OUTPUT_FILE="RESULT-${DB_NAME}.txt"
   TITLE="Comparaisons de la base ${DB_NAME} avec celle Insee"
else
   OUTPUT_FILE="RESULT.txt"
   TITLE="Comparaisons avec la base Insee"
fi

# Génération du titre et début du HTML
echo -e "TITLE=$TITLE\n<pre>\n" > "$OUTPUT_FILE"

# Requête principale
$MYSQL -N << EOF | sed 's/\\n/\n/g' >> "$OUTPUT_FILE"
SELECT CONCAT('IdInsee(', IdInsee, ') État ', Etat, ' (',
    CASE Etat 
        WHEN 2 THEN 'Évènement'
        WHEN 1 THEN 'Partiel'
        WHEN 0 THEN 'Nom'
        WHEN -1 THEN 'Date'
        ELSE 'Non trouvé'
    END,
    ') Score ', Score,
    ' Matches ', NbMatch, '\n',
    IF('$DB_NAME' != '', 
       REGEXP_REPLACE(CONCAT('<a href="$gwserver/$DB_NAME', '_w?m=S&edit=1&n=', 
              REPLACE(Cle, ' ', '%20'), 
              '" target="_blank">', Cle, 
              '</a>'), '[\r]', ''), Cle), '\n',
    concat_ws('|',
        Nom, Prenom, Sexe,
        concat('°', NaissanceD, '/', NaissanceM, '/', NaissanceY),
        NaissancePlace,
        concat('+', DecesD, '/', DecesM, '/', DecesY),
        DecesPlace), '\n',
    Msg, '\n')
FROM TODO
WHERE Msg != ''
ORDER BY 
    Etat DESC,
    Score DESC,
    NbMatch ASC,
    Nom,
    Prenom;
EOF

# Fermeture du pre
echo "</pre>" >> "$OUTPUT_FILE"