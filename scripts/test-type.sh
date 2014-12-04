#!/usr/bin/bash
rm failed.list
make && make test-tags TYPE=$1 2>&1 | tee -a failed.list
echo "unexpected: `grep "unexpected " failed.list | wc -l`"
echo "unknown: `grep "unknown tag" failed.list | wc -l`"
