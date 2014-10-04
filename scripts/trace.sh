#!/usr/bin/env bash
# file line col
cat $1 | head -$2 | tail -1 | sed 's/\t/......../g' | cut -b$3-
