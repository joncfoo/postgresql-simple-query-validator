#!/usr/bin/env python3

# extracts SQL statements embedded in postgresql-simple's quasiquoter

import fileinput

stmts = []
stmt = ''

for line in fileinput.input():
    begin_idx = line.find('[sql|')
    if begin_idx >= 0:
        stmt = (' ' * (begin_idx+5)) + line[begin_idx+5:]
        continue

    end_idx = line.find('|]')
    if end_idx >= 0:
        stmt += (' ' * (end_idx+2)) + line[:end_idx]
        stmts.append(stmt)
        stmt = ''
        continue

    if stmt:
        if line.strip():
            stmt += line

# replace ? with $n since PostgreSQL only accepts the latter format when using
# 'describe'
def swap_qs(stmt):
    n = 1
    idx = stmt.find('?')
    copy = stmt
    while idx >= 0:
        copy = copy.replace('?', '$'+str(n), 1)
        idx = copy.find('?', idx+1)
        n += 1
    return copy

for s in stmts:
    print(swap_qs(s)+'\0')
