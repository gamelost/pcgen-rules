#!/usr/bin/env bash
#
# usage: verify.sh lst_type regex
#
DATA_DIR=data/
PCGEN=./dist/build/pcgen-rules/pcgen-rules # calling `cabal run` repeatedly is slow
for file in `find $DATA_DIR -type f | grep -iE $2`
do
    $PCGEN $file $1
done
