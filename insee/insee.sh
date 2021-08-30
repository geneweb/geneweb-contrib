#! /bin/sh

#CONTRIB="/Users/Henri/GitHub/hgouraud/geneweb-contrib"
#GW="/Users/Henri/GitHub/hgouraud/geneweb/distribution/gw"

# assumes GW and CONTRIB are defined env variables

cat $CONTRIB/insee/insee.ml | $GW/gw/gwrepl "$@"
