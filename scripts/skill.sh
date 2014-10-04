#!/usr/bin/bash
make && rm failed.list && make test-tags TYPE=skill 2>&1 | tee -a failed.list
grep "failed to parse" failed.list | cut -d "'" -f 2 | tr '\t' '*' | tr -s '*' | grep --color -E "\*\!?[A-Z]+:"
grep "failed to parse" failed.list | wc -l
