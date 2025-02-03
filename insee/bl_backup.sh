#!/bin/bash

MYSQL="./mysql.sh"

if [ $# -ne 1 ]; then
    echo "Usage: $0 database_name"
    exit 1
fi

DB_NAME="$1"
RESULT_FILE="RESULT.txt"
TABLE_NAME="blacklist_${DB_NAME}"

if [ ! -f "$RESULT_FILE" ]; then
    echo "Error: $RESULT_FILE not found"
    exit 1
fi

# Create blacklist table if it doesn't exist
$MYSQL << EOF
CREATE TABLE IF NOT EXISTS \`${TABLE_NAME}\` (
    \`Id\` INTEGER UNSIGNED auto_increment primary key,
    \`TodoKey\` VARCHAR(255) COMMENT 'Combination of key fields (NULL for direct InseeId blacklist)',
    \`IdInsee\` INTEGER UNSIGNED NOT NULL,
    \`CreatedAt\` TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE KEY \`uk_todo_insee\` (\`TodoKey\`, \`IdInsee\`),
    KEY \`idx_insee\` (\`IdInsee\`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

-- Insert IdInsee-only entries (good matches marked with $)
INSERT IGNORE INTO \`${TABLE_NAME}\` (IdInsee)
SELECT DISTINCT CAST(
    SUBSTRING_INDEX(
        SUBSTRING_INDEX(line, 'IdInsee(', -1), 
        ')', 1
    ) AS UNSIGNED
) as IdInsee
FROM (
    SELECT REGEXP_SUBSTR(LOAD_FILE('${RESULT_FILE}'), '[^\n]+', 1, n) as line
    FROM (
        SELECT a.N + b.N * 10 + 1 n
        FROM 
            (SELECT 0 AS N UNION SELECT 1 UNION SELECT 2 UNION SELECT 3 UNION SELECT 4 
             UNION SELECT 5 UNION SELECT 6 UNION SELECT 7 UNION SELECT 8 UNION SELECT 9) a,
            (SELECT 0 AS N UNION SELECT 1 UNION SELECT 2 UNION SELECT 3 UNION SELECT 4 
             UNION SELECT 5 UNION SELECT 6 UNION SELECT 7 UNION SELECT 8 UNION SELECT 9) b
        ORDER BY n
    ) numbers
    WHERE REGEXP_SUBSTR(LOAD_FILE('${RESULT_FILE}'), '[^\n]+', 1, n) IS NOT NULL
) t
WHERE t.line LIKE '\$IdInsee(%';

-- Insert entries with TodoKey (non-matches marked with %)
INSERT IGNORE INTO \`${TABLE_NAME}\` (TodoKey, IdInsee)
SELECT 
    CONCAT(Nom, '|', Prenom, '|', Sexe, '|',
           NaissanceY, NaissanceM, NaissanceD, '|', NaissancePlace, '|',
           DecesY, DecesM, DecesD, '|', DecesPlace) as TodoKey,
    IdInsee
FROM TODO 
WHERE IdInsee IN (
    SELECT DISTINCT CAST(
        SUBSTRING_INDEX(
            SUBSTRING_INDEX(t.line, 'IdInsee(', -1), 
            ')', 1
        ) AS UNSIGNED
    ) as IdInsee
    FROM (
        SELECT REGEXP_SUBSTR(LOAD_FILE('${RESULT_FILE}'), '[^\n]+', 1, n) as line
        FROM (
            SELECT a.N + b.N * 10 + 1 n
            FROM 
                (SELECT 0 AS N UNION SELECT 1 UNION SELECT 2 UNION SELECT 3 UNION SELECT 4 
                 UNION SELECT 5 UNION SELECT 6 UNION SELECT 7 UNION SELECT 8 UNION SELECT 9) a,
                (SELECT 0 AS N UNION SELECT 1 UNION SELECT 2 UNION SELECT 3 UNION SELECT 4 
                 UNION SELECT 5 UNION SELECT 6 UNION SELECT 7 UNION SELECT 8 UNION SELECT 9) b
            ORDER BY n
        ) numbers
        WHERE REGEXP_SUBSTR(LOAD_FILE('${RESULT_FILE}'), '[^\n]+', 1, n) IS NOT NULL
    ) t
    WHERE t.line LIKE '%IdInsee(%'
);

-- Show statistics
SELECT 
    'Direct IdInsee entries added' as Type,
    COUNT(*) as Count
FROM \`${TABLE_NAME}\`
WHERE CreatedAt >= DATE_SUB(NOW(), INTERVAL 1 MINUTE)
AND TodoKey IS NULL
UNION ALL
SELECT
    'TodoKey entries added',
    COUNT(*)
FROM \`${TABLE_NAME}\`
WHERE CreatedAt >= DATE_SUB(NOW(), INTERVAL 1 MINUTE)
AND TodoKey IS NOT NULL
UNION ALL
SELECT
    'Total blacklisted IdInsee',
    COUNT(DISTINCT IdInsee)
FROM \`${TABLE_NAME}\`;
EOF