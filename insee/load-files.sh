#!/bin/bash

MYSQL="./mysql.sh"
YEAR_PARALLEL=4
MONTH_PARALLEL=4

echo "(Re)create table..."
$MYSQL << EOF

DROP TABLE IF EXISTS INSEE;

CREATE TABLE INSEE (
    Id INTEGER UNSIGNED auto_increment primary key,
    Nom VARCHAR(80) not null,
    Prenom VARCHAR(80) not null,
    Sexe CHAR(1) not null,
    NaissanceY CHAR(4) not null,
    NaissanceM CHAR(2) not null,
    NaissanceD CHAR(2) not null,
    NaissanceCode CHAR(5) not null,
    NaissanceLocalite VARCHAR(30) not null,
    NaissancePays VARCHAR(30) not null,
    DecesY CHAR(4) not null,
    DecesM CHAR(2) not null,
    DecesD CHAR(2) not null,
    DecesCode CHAR(5) not null,
    NumeroActe CHAR(9) not null,
    Source CHAR(8) not null,
    KEY idx_nom_prenom (Nom, Prenom),
    KEY idx_naissance (NaissanceY, NaissanceM, NaissanceD),
    KEY idx_deces (DecesY, DecesM, DecesD)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
ALTER TABLE insee DISABLE KEYS;
EOF

echo "Loading data in parallel (max $MAX_PARALLEL processes)..."
START_TIME=$SECONDS
for f in INSEE/deces*txt; do
    if [[ "$f" =~ "-m" ]]; then
        while (( $(jobs -r | wc -l) >= MONTH_PARALLEL )); do
            wait -n
        done
    else
        while (( $(jobs -r | wc -l) >= YEAR_PARALLEL )); do
            wait -n
        done
    fi

    (
        year=$(echo $f | grep -o '[0-9]\{4\}')
        charset=$([ "$year" -gt "2016" ] && echo "utf8" || echo "ascii")
        start=$SECONDS
        output=$(./reload-file.sh "$f" "$charset")
        duration=$(( SECONDS - start ))
        printf "%s took %d seconds\n" "$output" "$duration"
    ) &
done

wait

DURATION=$(( SECONDS - START_TIME ))
echo "Total loading time: $(( DURATION / 60 )) minutes and $(( DURATION % 60 )) seconds"

echo "Enabling indexes..."
START_TIME=$SECONDS
$MYSQL << EOF
ALTER TABLE INSEE ENABLE KEYS;
EOF
DURATION=$(( SECONDS - START_TIME ))
echo "Index enabling time: $(( DURATION / 60 )) minutes and $(( DURATION % 60 )) seconds"

echo "Analyzing table..."
$MYSQL << EOF
ANALYZE TABLE INSEE;
EOF

echo "Database loaded and indexed."