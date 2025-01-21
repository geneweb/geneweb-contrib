#!/bin/bash

# Verify geneweb installation
GENEWEB_DIR=$(dirname $(which gwd))
if [ -z "$GENEWEB_DIR" ]; then
    echo "ERROR: Cannot find geneweb installation"
    exit 1
fi

# Check if we have the necessary base name argument
if [ $# -lt 1 ]; then
    echo "Usage: $0 <database_name>"
    exit 1
fi

# Convert database path to absolute path
DATABASE=$(readlink -f "$1")
if [ ! -f "$DATABASE" ]; then
    # Try adding .gwb extension
    DATABASE="${DATABASE}.gwb"
fi
if [ ! -f "$DATABASE" ]; then
    echo "ERROR: Cannot find database at $DATABASE"
    exit 1
fi

# Create temporary directory for work
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

# Copy insee.ml to temp dir
cp insee.ml "$TEMP_DIR/"

# CD to temp dir so utop finds the files
cd "$TEMP_DIR"

# Get dune directives and run
(dune ocaml top && cat insee.ml) | utop -stdin "$DATABASE"