#!/usr/bin/env bash
DATA_DIR=data/
for x in "LANGUAGE" "SHIELDPROF" "WEAPONPROF" "ARMORPROF" "SKILL" "COMPANIONMOD" "DEITY" "DOMAIN" "EQUIPMOD" "EQUIPMENT" "SPELL" "FEAT" "RACE" "KIT" "TEMPLATE" "CLASS" "ABILITY" "ABILITYCATEGORY"
do
    echo "${x}: `grep -R --include="*.pcc" "${x}:" ${DATA_DIR} | grep -v '#' | cut -d ':' -f 3 | cut -d '|' -f 1 | tr -d '\r' | xargs basename -a | sort | uniq | wc -l`"
done
