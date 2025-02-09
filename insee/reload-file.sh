#!/bin/bash

MYSQL="./mysql.sh"
pattern='^INSEE/deces-[[:digit:]]{4}(-m[[:digit:]]{2})?\.txt$'

if [[ ! "$1" =~ $pattern ]]; then
    echo "Usage: $0 INSEE/deces-<annee>[-mMM].txt [charset]"
    exit 1
fi

f="$1"
charset="${2:-utf8}"

if [ ! -f "$f" ]; then
    echo "ERROR: File $f not readable."
    exit 1
fi

source=$(basename "$f" | sed -e 's#^deces-##' -e 's/\.txt$//')

# Get the count in a variable
count=$($MYSQL -N << EOF
LOAD DATA LOCAL INFILE '$f'
INTO TABLE INSEE
CHARACTER SET $charset
(@row)
SET
    Id                = null,
    Nom               = TRIM(substr(@row, 1, locate('*', @row)-1)),
    Prenom            = TRIM(substr(@row, locate('*', @row)+1, locate('/', @row)-locate('*', @row)-1)),
    Sexe              = substr(@row, 81, 1),
    NaissanceY        = substr(@row, 82, 4),
    NaissanceM        = substr(@row, 86, 2),
    NaissanceD        = substr(@row, 88, 2),
    NaissanceCode     = substr(@row, 90, 5),
    NaissanceLocalite = TRIM(substr(@row, 95, 30)),
    NaissancePays     = TRIM(substr(@row, 125, 30)),
    DecesY            = substr(@row, 155, 4),
    DecesM            = substr(@row, 159, 2),
    DecesD            = substr(@row, 161, 2),
    DecesCode         = substr(@row, 163, 5),
    NumeroActe        = substr(@row, 168, 9),
    Source            = '$source';

SELECT COUNT(*) FROM INSEE WHERE Source = '$source';
EOF
)

printf "Loading %s: %d records, %s charset" "$f" "$count" "${charset^^}"