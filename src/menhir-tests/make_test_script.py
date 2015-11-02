tests = open('tests.sh', 'w')

dict_access = open('dict-access.txt', 'r')
member_call = open('member-call.txt', 'r')
member_var = open('member-var.txt', 'r')
simple_func = open('simple-func.txt', 'r')

tests.write('#!/bin/bash \n\n')

tests.write('echo -e Running tests in dict-access.txt\n')
tests.write('echo -e ================================\n')
for line in dict_access:
    if line[:3] == '***' or line[0] == '\n':
        continue
    tests.write('echo \'' + line.strip() + '\' | menhir --interpret --interpret-show-cst parser.mly ' + '\n')

tests.write('echo -e Running tests in member-call.txt\n')
tests.write('echo -e ================================\n')
for line in member_call:
    if line[:3] == '***' or line[0] == '\n':
        continue
    tests.write('echo \'' + line.strip() + '\' | menhir --interpret --interpret-show-cst parser.mly ' + '\n')

tests.write('echo -e Running tests in member-var.txt\n')
tests.write('echo -e ================================\n')
for line in member_var:
    if line[:3] == '***' or line[0] == '\n':
        continue
    tests.write('echo \'' + line.strip() + '\' | menhir --interpret --interpret-show-cst parser.mly ' + '\n')

tests.write('echo -e Running tests in simple-func.txt\n')
tests.write('echo -e ================================\n')
for line in simple_func:
    if line[:3] == '***' or line[0] == '\n':
        continue
    tests.write('echo \'' + line.strip() + '\' | menhir --interpret --interpret-show-cst parser.mly ' + '\n')


tests.close()
dict_access.close()
member_var.close()
member_call.close()