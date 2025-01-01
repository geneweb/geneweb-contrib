#!/bin/bash

CONTRIB_DIR=`dirname "$0"`

cat $CONTRIB_DIR/lex_utils.ml > $CONTRIB_DIR/tmp.ml
echo "main \"$@\";;" >> $CONTRIB_DIR/tmp.ml

echo "#use \"$CONTRIB_DIR/tmp.ml\";;" | utop -stdin
rm $CONTRIB_DIR/tmp.ml