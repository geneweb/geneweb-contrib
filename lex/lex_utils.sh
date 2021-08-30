#! /bin/sh

#CONTRIB="/Users/Henri/GitHub/hgouraud/geneweb-contrib"
#GW="/Users/Henri/GitHub/hgouraud/geneweb/distribution/gw"

# assumes GW and CONTRIB are defined env variables

#OPT="GWREPL_PPF=/dev/null GWREPL_NOPROMPT=1"
cat $CONTRIB/lex/lex_utils.ml | $OPT $GW/gw/gwrepl "$@"
