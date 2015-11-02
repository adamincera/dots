# Menhir test automating script
# Might use it to automate more tests
# Make sure all of the input files are .txt files!

import os, sys, subprocess
run_normal = True
suppress_stderr = True

if len(sys.argv) == 1:
    print('Running with default configurations:\n')
elif '-f' in sys.argv and '-e' in sys.argv:
    run_normal = False
    suppress_stderr = False
elif '-f' in sys.argv:
    run_normal = False
elif '-e' in sys.argv:
    suppress_stderr = False
else:
    print('usage: -f,  or no command line args')
    print('-f: prints full results of every test')
    print('-e: show stderr of menhir')
    print ('no command line args: runs all tests and only prints tests that failed, suppresses stderr of menhir.\n')
    sys.exit()

print(sys.argv)

path = r'menhir-tests'
for dir_entry in os.listdir(path):
    filepath = os.path.join(path, dir_entry)
    if os.path.isfile(filepath) and filepath[-3:] == 'txt':
        print('Running tests in ' + dir_entry)
        print('================================\n')
        with open(filepath, 'r') as test_file:
            for line in test_file:
                if line[:3] == '***' or line[0] == '\n':
                    continue
                to_pipe = line.strip().split()
                to_pipe.insert(0, 'echo')
                
                #can pipe with subprocess only by opening another process, not standard syntax
                menhir_input = subprocess.Popen(to_pipe, stdout=subprocess.PIPE) 
                menhir_cmd = ['menhir', '--interpret', '--interpret-show-cst', 'parser.mly']
                if suppress_stderr == True:
                    with open(os.devnull, 'w') as devnull:    
                        output = subprocess.check_output(menhir_cmd, stdin=menhir_input.stdout, stderr=devnull)
                else:
                    output = subprocess.check_output(menhir_cmd, stdin=menhir_input.stdout) 

                if run_normal == False:
                    print(output)
                else:
                    if 'REJECT' in output:
                        print('"' + line.strip() + '" failed.\n')

    else:
        print (dir_entry + ' is messed up.\n')
        print ('--------------------------\n\n')

print('\n Tests completed.')
