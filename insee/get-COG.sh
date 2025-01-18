#!/bin/bash

usage() {
    echo "Usage: $0 [2019|2024]"
    echo "Downloads and extracts COG (Code Officiel Géographique) data for the specified year"
    echo "If no year is specified, defaults to 2024"
    exit 1
}

DIR=COG
YEAR=${1:-2024}  # Default to 2024 if no argument provided

# Validate input
if [[ ! "$YEAR" =~ ^(2019|2024)$ ]]; then
    echo "Error: Year must be either 2019 or 2024"
    usage
fi

# INSEE COG root : https://www.insee.fr/fr/information/2560452
if [ "$YEAR" = "2019" ]; then
    COG_URL="https://www.insee.fr/fr/statistiques/fichier/3720946/cog_ensemble_2019_csv.zip"
    ZIP_PREFIX="cog_ensemble_2019_csv"
    PAYS_FILE="pays2019.csv"
    DEPT_FILE="departement2019.csv"
    COMM_FILE="communes-01042019.csv"
    MVT_FILE="mvtcommune-01042019.csv"
else
    COG_URL="https://www.insee.fr/fr/statistiques/fichier/7766585/cog_ensemble_2024_csv.zip"
    ZIP_PREFIX="cog_ensemble_2024_csv"
    PAYS_FILE="v_pays_territoire_2024.csv"
    DEPT_FILE="v_departement_2024.csv"
    COMM_FILE="v_commune_2024.csv"
    MVT_FILE="v_mvt_commune_2024.csv"
fi

# Autre source pour les pays car les libellés ne sont pas satisfaisants dans le lot de fichier INSEE (au moins pour 2019)
PAYS_URL="https://sql.sh/ressources/sql-pays/sql-pays.csv"

# Create directory if it doesn't exist
mkdir -p ${DIR}

# Download COG data if needed
if [ ! -f "${DIR}/${ZIP_PREFIX}.zip" ]; then
    echo "Downloading COG data for ${YEAR}..."
    wget "${COG_URL}" -O "${DIR}/${ZIP_PREFIX}.zip"
else
    echo "Using existing COG zip file for ${YEAR}"
fi

echo "Extracting COG files for ${YEAR}..."
# Extract files with year-specific names
unzip -qc "${DIR}/${ZIP_PREFIX}.zip" "${PAYS_FILE}" | sed 's/$//' > "${DIR}/COG-pays.csv"
unzip -qc "${DIR}/${ZIP_PREFIX}.zip" "${DEPT_FILE}" | sed 's/$//' > "${DIR}/COG-departement.csv"
unzip -qc "${DIR}/${ZIP_PREFIX}.zip" "${COMM_FILE}" | sed 's/$//' > "${DIR}/COG-commune.csv"
unzip -qc "${DIR}/${ZIP_PREFIX}.zip" "${MVT_FILE}" | sed 's/$//' > "${DIR}/COG-mvt.csv"

# Download supplementary country data if needed
if [ ! -f "${DIR}/sql-pays.csv" ]; then
    echo "Downloading supplementary country data..."
    wget "${PAYS_URL}" -O "${DIR}/sql-pays.csv"
fi

echo "COG data for ${YEAR} successfully processed"