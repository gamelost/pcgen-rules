#!/usr/bin/env python
import re
import sys

lines = ' '.join(sys.argv).split('pcgen-rules: ')[1:]
regex = re.compile(r'([^ ]+) \(line (\d+), column (\d+)\)')
for line in lines:
    try:
        filename, line, column = regex.findall(line)[0]
        line = int(line)
        column = int(column)
        target = open(filename, 'r+').readlines()[line-1]
        index = 0
        count = 0
        for char in target:
            if count >= column:
                break
            if char == '\t':
                # jump to the next multiple of 8.
                count += 8 - (count % 8)
            else:
                count += 1
            index += 1
        starting = target.rfind('\t', 0, index-1)
        ending = target.find('\t', index)
        if starting == -1: starting = 0
        if ending == -1: ending = len(target)
        assert(starting <= index <= ending)
        print(filename, 'error:', target[starting:ending].strip())
    except IndexError:
        print('error: unable to get line.')
