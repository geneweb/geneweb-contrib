#!/bin/bash

MYSQL="./mysql.sh"

usage() {
    echo "Usage: $0 database_name [input_file]"
    echo
    echo "  database_name    : Name of the database (required)"
    echo "  input_file       : File containing IdInsee entries (defaults RESULT.txt)"
    echo
    echo "Examples:"
    echo "  $0 mybase          # Uses RESULT.txt as input"
    echo "  $0 mybase list.txt # Uses list.txt as input"
    exit 1
}

if [ $# -lt 1 ] || [ $# -gt 2 ]; then
    usage
fi

DB_NAME="$1"
TABLE_NAME="blacklist_${DB_NAME}"
RESULT_FILE="${2:-RESULT.txt}"

if [ ! -f "$RESULT_FILE" ]; then
    echo "Error: Input file $RESULT_FILE not found"
    exit 1
fi

echo "Processing false positives for database ${DB_NAME} from ${RESULT_FILE}."

# Process both $/% cases into structured temp file
TMP_DATA=$(mktemp)
sed -n '
  /^[%$]IdInsee/ {
    # Capture the marker ($ or %)
    s/^\([%$]\)IdInsee(\([0-9]*\)).*/\1@\2/
    h   # Store marker@IdInsee in hold space
    
    # Read next two lines
    n
    H   # Append first additional line
    n
    H   # Append second additional line
    
    # Retrieve full pattern and reformat
    g
    s/\n/@/g  # Replace all newlines with @
    
    p  # Print the final result
  }' "$RESULT_FILE" > "$TMP_DATA"

# Count matches using the processed data
match_count=$(grep -c '^\$@' "$TMP_DATA")
todo_count=$(grep -c '^%@' "$TMP_DATA")
echo "Found $match_count complete matches and $todo_count non-matching entries to process."

# Only proceed if we found entries to process
if [ $match_count -eq 0 ] && [ $todo_count -eq 0 ]; then
    rm -f "$TMP_DATA"
    echo "No entries to process"
    exit 0
fi

$MYSQL << EOF
CREATE TABLE IF NOT EXISTS \`${TABLE_NAME}\` (
    \`Id\` INTEGER UNSIGNED auto_increment primary key,
    \`IdInsee\` INTEGER UNSIGNED NOT NULL,
    \`GwKey\` VARCHAR(100),
    \`TodoKey\` VARCHAR(255),
    \`CreatedAt\` TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE KEY uk_blacklist (IdInsee, GwKey, TodoKey),
    KEY idx_idinsee (IdInsee),
    KEY idx_created (CreatedAt)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

# Créer une table temporaire pour le chargement initial
CREATE TEMPORARY TABLE tmp_blacklist (
    mark CHAR(1),
    IdInsee INTEGER UNSIGNED NOT NULL,
    GwKey VARCHAR(100),
    TodoKey VARCHAR(255),
    KEY idx_mark_idinsee (mark, IdInsee)
) ENGINE=MEMORY;

# Charger les données brutes
LOAD DATA LOCAL INFILE '${TMP_DATA}'
INTO TABLE tmp_blacklist
FIELDS TERMINATED BY '@'
(mark, IdInsee, @gwkey, @todokey)
SET
    GwKey = CASE WHEN mark = '$' THEN NULL 
                 ELSE NULLIF(@gwkey,'') END,
    TodoKey = CASE WHEN mark = '$' THEN NULL 
                   ELSE NULLIF(@todokey,'') END;

START TRANSACTION;
# [%>$] Remove any existing entries that are now complete matches
DELETE FROM \`${TABLE_NAME}\` 
WHERE IdInsee IN (SELECT IdInsee FROM tmp_blacklist WHERE mark = '$');

# [$] Insert the new complete IdInsee entry match
INSERT INTO \`${TABLE_NAME}\` (IdInsee)
SELECT DISTINCT IdInsee 
FROM tmp_blacklist 
WHERE mark = '$';

# [%] Insert non-matching IdInsee+TodoKey entries
INSERT IGNORE INTO \`${TABLE_NAME}\` (IdInsee, GwKey, TodoKey)
SELECT IdInsee, GwKey, TodoKey
FROM tmp_blacklist t
WHERE mark = '%'
AND NOT EXISTS (
    SELECT 1 
    FROM \`${TABLE_NAME}\` b
    WHERE b.IdInsee = t.IdInsee
    AND (b.TodoKey IS NULL
        OR (b.TodoKey = t.TodoKey AND b.GwKey = t.GwKey))
);

COMMIT;

DROP TEMPORARY TABLE tmp_blacklist;

-- Show statistics
SELECT 
    CONCAT('[$] Complete matches added: ', 
           COUNT(*)) as ''
FROM \`${TABLE_NAME}\`
WHERE CreatedAt >= DATE_SUB(NOW(), INTERVAL 1 MINUTE)
AND TodoKey IS NULL
UNION ALL
SELECT 
    CONCAT('[%] Non-matches added: ',
           COUNT(*))
FROM \`${TABLE_NAME}\`
WHERE CreatedAt >= DATE_SUB(NOW(), INTERVAL 1 MINUTE)
AND TodoKey IS NOT NULL
UNION ALL
SELECT 
    CONCAT('Total blacklisted entries: ',
           COUNT(Id))
FROM \`${TABLE_NAME}\`;

-- Cleanup
DROP TEMPORARY TABLE IF EXISTS tmp_blacklist;
EOF

# Clean up temporary file
rm -f "$TMP_DATA"