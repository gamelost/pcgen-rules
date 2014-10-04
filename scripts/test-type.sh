#!/usr/bin/bash
make && rm failed.list && make test-tags TYPE=$1 2>&1 | tee -a failed.list
echo "unexpected: `grep "unexpected" failed.list | wc -l`"
echo "unknown: `grep "unknown" failed.list | wc -l`"
