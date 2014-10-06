#!/usr/bin/env bash
# file line col
COLUMN=${3-"1"}
cat $1 | head -$2 | tail -1 | sed 's/\t/......../g' | cut -b$COLUMN-
