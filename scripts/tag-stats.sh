#!/usr/bin/env bash
#
# print out tag usage statistics.

set -x

if [ $# -ne 2 ]
then
    echo "usage: $0 tag-name directory"
    exit 1
fi

TAG=$1
DIRPATH=$2
echo "searching for $TAG in $DIRPATH"

# calculate the total number.
TOTAL=`grep -c -R --include="*.lst" $TAG $DIRPATH | grep -v '#' | cut -d ':' -f 2 | paste -sd+ | bc`

# calculate occurrences of each tag
grep -R --include="*.lst" $TAG $DIRPATH | grep -v '#' | \
    sed -n -e "s/^.*\($TAG\)/\1/p" | \
    cut -d '|' -f 1 | cut -f 1 | \
    sort | uniq -c | sort -n | \
    awk -v total=$TOTAL '{ stats = $1 / total * 100.00; printf "%.2f%% %s\n", stats, $2 }'
