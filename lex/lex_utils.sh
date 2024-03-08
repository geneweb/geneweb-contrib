#! /bin/sh

# Assumes GW and CONTRIB are defined
# e.g.
REPO="../../geneweb"
GW="$REPO/distribution/gw"
GWREPL_NOPROMPT=1 
GWREPL_VERBOSE=1

cat ./lex_utils.ml | $GW/gwrepl -repo $REPO "$@"

