#!/usr/bin/env bash
#
# usage: verify.sh lst_type regex
#
DATA_DIR=data/
# calling `cabal run` repeatedly is slow
PCGEN=./dist/build/pcgen-rules/pcgen-rules
# for each pcc file we have, grep it for the lst type in question, and extract the path
files=`grep -R --include="*.pcc" "$1:" $DATA_DIR | grep -v '#' | cut -d ':' -f 3 | cut -d '|' -f 1 | tr '\r' ' ' | sort | uniq`
for file in $files
do
    $PCGEN $file $1
done
