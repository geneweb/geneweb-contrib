#!/bin/bash

# Directory containing the source files
SOURCE_DIR="INSEE"
PATTERN='^INSEE/deces-[[:digit:]]{4}(-m[[:digit:]]{2})?\.txt$'

process_file() {
    local file=$1

    # Ensure the file exists
    if [[ ! -f "$file" ]]; then
        echo "ERROR: File $file does not exist or is not readable."
        return 1
    fi

    # Check for TAB characters
    grep -P "\t" "$file" > /dev/null 2>&1
    if [[ "$?" == "0" ]]; then
        echo "WARNING: TAB characters found in $file"
        mv "$file" "${file}-tab"
        sed -e "s/\t/ /g" "${file}-tab" > "$file"
        echo "TAB characters replaced with spaces in $file."
    fi

    # Check for NUL characters
    grep -Pa '\x00' "$file" > /dev/null 2>&1
    if [[ "$?" == "0" ]]; then
        echo "WARNING: NUL characters found in $file"
        mv "$file" "${file}.NUL"
        sed -e "s/\x00/ /g" "${file}.NUL" > "$file"
        echo "NUL characters replaced with spaces in $file."
    fi
}

if [[ $# -eq 0 ]]; then
    # Process all matching files in the source directory
    echo "Processing all files in $SOURCE_DIR..."
    for file in "$SOURCE_DIR"/deces-*; do
        if [[ "$file" =~ $PATTERN ]]; then
            process_file "$file"
        fi
    done
else
    # Process a specific file provided as an argument
    if [[ "$1" =~ $PATTERN ]]; then
        process_file "$1"
    else
        echo "ERROR: Invalid file name. Ensure it matches the pattern '$PATTERN'."
        exit 1
    fi
fi
