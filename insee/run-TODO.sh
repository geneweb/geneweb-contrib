#!/bin/bash

usage() {
    echo "Usage: $0 [-h] [-bl database] [-c cursors] [-m matches] [file]"
    echo
    echo "Options:"
    echo "  -h, --help      : Show this help"
    echo "  -bl database    : Database name for blacklist"
    echo "  -c cursors      : Number of cursors (1-4, default 1)"
    echo "  -m matches      : Max matches (default 20)"
    echo "  file           : Input TODO file (optional)"
    echo
    echo "Note: GeneWeb export is automatically used if input file is not found"
    exit 1
}

# Default values
TODO_FILE="TODO.lst"
DB_NAME=""
CURSORS=2        # Nombre de curseurs (1-4), défaut 2
MAX_MATCHES=20   # Maximum total de matches, défaut 20

# Parse options
while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--help)
            usage
            ;;
        -bl)
            shift
            if [ -z "$1" ]; then
                echo "Error: -bl requires a database name"
                usage
            fi
            DB_NAME="$1"
            shift
            ;;
        -c)
            shift
            CURSORS="$1"
            shift
            ;;
        -m)
            shift
            MAX_MATCHES="$1"
            shift
            ;;
        *)
            TODO_FILE="$1"
            shift
            ;;
    esac
done

# Temporarily modify the file existence check
if [ ! -f "$TODO_FILE" ]; then
    debug_file_checks "$TODO_FILE"
    echo "ERROR: Input file '$TODO_FILE' not found"
    exit 1
fi

if [ ! -r "$TODO_FILE" ]; then
    echo "ERROR: Input file $TODO_FILE is not readable"
    exit 1
fi

# Verify it's not empty
if [ ! -s "$TODO_FILE" ]; then
    echo "ERROR: Input file $TODO_FILE is empty"
    exit 1
fi

# TODO: add parameter for Geneweb install path to target insee.exe
EXE=$PWD/../../../_build/default/bin/contrib/insee/insee.exe
CFG=.run-TODO.cfg

MYSQL=./mysql.sh

# Try direct file first, fallback to GeneWeb if needed
# Check input file existence first
if [ ! -f "$TODO_FILE" ]; then
    # Try GeneWeb export as fallback
    if [ -x "${EXE}" ]; then
        echo "Attempting GeneWeb export..."
        if [ ! -f $CFG ]; then
            echo -n "Path to GeneWeb databases : "
            read BDIR
            if [ ! -d "$BDIR" ]; then
                echo "ERROR $BDIR not found."
                exit 1
            fi
            echo -n "GeneWeb database : "
            read BASE
            echo "BDIR=$BDIR" > $CFG
            echo "BASE=$BASE" >> $CFG
        else
            . $CFG
        fi
        cd $BDIR
        $EXE $BASE > $OLDPWD/$TODO_FILE
        cd -
    else
        echo "ERROR: GeneWeb exporter not available."
        echo "Please provide a valid input file or install GeneWeb exporter."
        exit 1
    fi
fi

echo "Comparaison des entrées de $TODO_FILE avec les données des décès Insee"
$MYSQL << EOF || { echo "ERROR: Failed to load $TODO_FILE into MySQL"; exit 1; }
drop table if exists TODO;

create table TODO (
	Id INTEGER UNSIGNED auto_increment primary key,
	Nom VARCHAR(80) not null,
	Prenom VARCHAR(80) not null,
	Sexe CHAR(1) not null,
	NaissanceY CHAR(4) not null,
	NaissanceM CHAR(2) not null,
	NaissanceD CHAR(2) not null,
	NaissancePlace VARCHAR(500),
	DecesY CHAR(4) not null,
	DecesM CHAR(2) not null,
	DecesD CHAR(2) not null,
	DecesPlace VARCHAR(500),
	Cle VARCHAR(100) not null,
	Etat INTEGER not null default 0,
	NbMatch INTEGER,
	Score INTEGER,
	IdInsee INTEGER UNSIGNED,
	Msg VARCHAR(1000),
	INDEX idx_etat (Etat),
	INDEX idx_nom_prenom (Nom, Prenom),
	INDEX idx_search (Etat, IdInsee, NbMatch)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

load data
 local infile '$TODO_FILE'
 ignore
 into table TODO
 character set utf8mb4
 fields terminated by '|'
 (Nom, Prenom, Sexe, NaissanceD, NaissanceM, NaissanceY, NaissancePlace, DecesD, DecesM, DecesY, DecesPlace, Cle)
 set
  Id = null
;
EOF

SECONDS=0
if ! $MYSQL -N << EOF
SET @database_name = $([ ! -z "$DB_NAME" ] && echo "'$DB_NAME'" || echo "NULL");
SET @max_cursors = ${CURSORS};
SET @max_total_matches = ${MAX_MATCHES};
CALL processTodo();
EOF
then
    echo "Error during processing."
fi
DURATION=$SECONDS
echo
echo "Processing completed in $(($DURATION / 60)) minutes and $(($DURATION % 60)) seconds"
echo

$MYSQL -t << EOF
select
 Etat,
 case Etat
    when 3 then 'FULL > BL'
    when 2 then 'EVENT'
    when 1 then 'PARTIAL'
    when 0 then 'NAME'
    when -1 then 'DATES'
    when -2 then 'NO MATCH'
 end as "Type",
 score as "Score",
 count(*) as "Nbr"
from TODO
group by 1,3
order by Etat desc, Score desc;
;
EOF

if [ ! -z "$DB_NAME" ]; then
    ./rapport.sh "$DB_NAME"
else
    ./rapport.sh
fi
