#! /bin/sh

# Assumes GW and CONTRIB are defined
# e.g.
# CONTRIB="/Users/Henri/GitHub/hgouraud/geneweb-contrib"
# GW="/Users/Henri/GitHub/hgouraud/geneweb/distribution/gw"
# OPT="GWREPL_PPF=/dev/null GWREPL_NOPROMPT=1"

cat $CONTRIB/lex/lex_utils.ml | $OPT $GW/gwrepl "$@"
