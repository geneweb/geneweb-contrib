#!/bin/bash

SCRIPT_DIR=$(dirname "$0")
GENEWEB_DIR="/cygdrive/c/gw"

# Set minimal environment
export OCAMLPATH="$GENEWEB_DIR/_build/default/lib:$OCAMLPATH"

# Parse arguments to maintain original behavior
if [ "$1" = "-bd" ]; then
    BASE_DIR="$2"
    BASE_NAME="$3"
    FULL_PATH="$BASE_DIR/$BASE_NAME.gwb"
else
    FULL_PATH="$1"
fi

# Create our OCaml program that will execute non-interactively
cat > "$SCRIPT_DIR/tmp.ml" << EOF
#!/usr/bin/env ocaml

#use "topfind";;
#require "geneweb.gwdb";;
#require "geneweb.def";;
#require "str";;

(* The rest of insee.ml contents will be appended here *)
EOF

# Append the main code but skip the original argument parsing
# since we're handling it in the shell script
sed '/let main () =.*/,$d' "$SCRIPT_DIR/insee.ml" >> "$SCRIPT_DIR/tmp.ml"

# Add our simplified main execution
cat >> "$SCRIPT_DIR/tmp.ml" << EOF

let () =
  Secure.set_base_dir (Filename.dirname "$FULL_PATH");
  let base = Gwdb.open_base "$FULL_PATH" in
  load_strings_array base;
  check_insee base
EOF

# Execute using ocaml directly instead of utop
ocaml "$SCRIPT_DIR/tmp.ml"

# Clean up
rm "$SCRIPT_DIR/tmp.ml"