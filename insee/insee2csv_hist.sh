#!/bin/bash

MYSQL="./mysql.sh"
insee_path=~/insee

if [ -z "$1" ]; then
    echo "Usage: $0 YYYYMM (e.g., 202412)" 
    exit 1
fi

secure_path=$($MYSQL -N << EOF
SELECT @@secure_file_priv;
EOF
)

echo "Starting SQL export to deces_insee_$1.csv"
$MYSQL << EOF

SET @start = NOW(2);
SELECT
    i.Nom,
    i.Prenom,
    i.Sexe,
    CONCAT(i.NaissanceY, '-', i.NaissanceM, '-', i.NaissanceD),
    i.NaissanceCode,
    i.NaissanceLocalite,
    CONCAT(i.DecesY, '-', i.DecesM, '-', i.DecesD),
    i.DecesCode,
    i.NumeroActe,
    @birth_place := getPlaceLibHistorique(i.NaissanceCode, i.NaissanceY, i.NaissanceM, i.NaissanceD) as birth_place_hist,
    CASE
        WHEN insee_code > '89999' THEN
            (SELECT LIBCOG FROM cog_pays WHERE COG = insee_code)
    ELSE 
        WHEN getPlaceLibHistorique(i.DecesCode, i.DecesY, i.DecesM, i.DecesD) = @birth_place THEN 'id.'
        ELSE getPlaceLibHistorique(i.DecesCode, i.DecesY, i.DecesM, i.DecesD)
    END as death_place_hist
FROM insee i
INTO OUTFILE '${secure_path}deces_insee_$1.csv'
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n';

SELECT CONCAT('Temps d''exécution : ', TIMEDIFF(NOW(6), @start)) as Performance;
EOF

cygwin_path=$(echo "$secure_path" | sed 's|c:\\|/cygdrive/c/|;s|\\|/|g')
export_file="${cygwin_path}deces_insee_$1.csv"
lines=$(wc -l < "$export_file")
size=$(ls -lh "$export_file" | awk '{print $5}')
mv $export_file $insee_path
echo "CSV exported to $insee_path/ ($lines entries/$size)"