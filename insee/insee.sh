#! /bin/sh

#CONTRIB="/Users/Henri/GitHub/hgouraud/geneweb-contrib"
#GW="/Users/Henri/GitHub/hgouraud/geneweb/distribution/gw"

# assumes GW and CONTRIB are defined env variables

#OPT="GWREPL_NOPROMPT=1 GWREPL_PPF=/dev/null "
cat $CONTRIB/insee/insee.ml | GWREPL_NOPROMPT=1 GWREPL_PPF=/dev/null $GW/gwrepl "$@"
