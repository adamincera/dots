# Test automation script 

import os, sys, glob
import argparse
from subprocess32 import check_output, Popen, PIPE, call

# if len(sys.argv) == 1:
#     print('Running with default configurations:\n')

# else:
#     print('usage: -f,  or no command line args')
#     print('-f: prints full results of every test')
#     print('-e: show stderr of menhir')
#     print ('no command line args: runs all tests and only prints tests that failed, suppresses stderr of menhir.\n')
#     sys.exit()

#####################
# ARGUMENT PARSING: #
#####################

parser = argparse.ArgumentParser(description='Run tests on compilation of dots files.')
parser.add_argument("-c", "--clean", action="store_true", 
    help="removes all created files after tests have finished")
args = parser.parse_args()

#########
# TESTS #
#########

path = r'dtest'
npath = r'ntests'

summary_results = {}
summary_results_n = {}

for directory in os.walk(path):
    print ('\nRunning tests in "' + directory[0] + '" folder:')
    print ('*************************************************')
    for dir_entry in os.listdir(directory[0]):
        filepath = os.path.join(path, directory[0], dir_entry)
        if os.path.isfile(filepath) and filepath[-5:] == '.dots':
            print('\nRunning tests: ' + dir_entry)
            print('================================')

            comp_success = False
            try:
                return_code = call(['./gdc', filepath, os.path.join(path, directory[0], dir_entry[:-5] + '.exec')], 
                    timeout=30)
                if return_code == 0:
                    print 'COMPILATION SUCCESSFUL'
                    comp_success = True
                else:
                    print 'COMPILATION FAILED'
                    summary_results[dir_entry[:-5]] = ('fail', directory[0])
            except:
                print 'compile executable. Stop.'
                continue;

            if (comp_success):
                out_child = Popen('./' + os.path.join(path, directory[0], dir_entry[:-5]) + '.exec', 
                    shell=True, stdout=PIPE)
                output = out_child.communicate()[0]
                
                output_filepath = os.path.join(path, directory[0], dir_entry[:-5] + '.outgdc')
                with open(output_filepath, 'w') as intermediate_output:
                    intermediate_output.write(output)

                out_filepath = os.path.join(path, directory[0], dir_entry[:-5] + '.out')
                output_filepath = os.path.join(path, directory[0], dir_entry[:-5] + '.outgdc')

                if (os.path.exists(out_filepath)):
                    diff_command = ['diff', '-bB', out_filepath, output_filepath]
                    diff_child = Popen(diff_command, stdout=PIPE)
                    diff_output = diff_child.communicate()[0]

                    if diff_output.strip() == '':
                        print 'PASSED TEST'
                        summary_results[dir_entry[:-5]] = ('pass', directory[0])
                    else: 
                        print 'FAILED TEST....writing diff files'
                        summary_results[dir_entry[:-5]] = ('fail', directory[0])
                        with open(os.path.join(path, dir_entry[:-5] + '.dif'), 'w') as output_diff:
                            output_diff.write(diff_output.strip())
                else:
                    print "FAIL: no .out file exists to check against"


for directory in os.walk(path):
    print ('\nRunning tests in "' + directory[0] + '" folder:')
    print ('*************************************************')
    for dir_entry in os.listdir(directory[0]):
        filepath = os.path.join(path, directory[0], dir_entry)
        if os.path.isfile(filepath) and filepath[-5:] == '.dots':
            print('\nRunning tests: ' + dir_entry)
            print('================================')

            comp_success = False
            try:
                return_code = call(['./gdc', filepath, os.path.join(npath, directory[0], dir_entry[:-5] + '.exec')], 
                    timeout=30)
                if return_code == 0:
                    print 'COMPILATION SUCCESSFUL'
                    comp_success = True
                else:
                    print 'COMPILATION FAILED'
                    summary_results_n[dir_entry[:-5]] = ('fail', directory[0])
            except:
                
                continue;

            if (comp_success):
                out_child = Popen('./' + os.path.join(npath, directory[0], dir_entry[:-5]) + '.exec', 
                    shell=True, stdout=PIPE)
                output = out_child.communicate()[0]
                
                output_filepath = os.path.join(npath, directory[0], dir_entry[:-5] + '.outgdc')
                with open(output_filepath, 'w') as intermediate_output:
                    intermediate_output.write(output)

                out_filepath = os.path.join(npath, directory[0], dir_entry[:-5] + '.out')
                output_filepath = os.path.join(npath, directory[0], dir_entry[:-5] + '.outgdc')

                if (os.path.exists(out_filepath)):
                    diff_command = ['diff', '-bB', out_filepath, output_filepath]
                    diff_child = Popen(diff_command, stdout=PIPE)
                    diff_output = diff_child.communicate()[0]

                    if diff_output.strip() == '':
                        print 'PASSED TEST'
                        summary_results_n[dir_entry[:-5]] = ('pass', directory[0])
                    else: 
                        print 'FAILED TEST....writing diff files'
                        summary_results_n[dir_entry[:-5]] = 'fail'
                        with open(os.path.join(path, directory[0], dir_entry[:-5] + '.dif'), 'w') as output_diff:
                            output_diff.write(diff_output.strip())
                else:
                    print "FAIL: no .out file exists to check against"

print('\n Tests completed.')
print('\n Summary below (checked boxes = performed as expected): \n')

#################
# PRINT SUMMARY #
#################
print('Tests that should pass:')
for test_name in sorted(summary_results):
    if summary_results[test_name][0] == 'pass':
        print('[X] ' + test_name + '(' + summary_results[test_name][1] + ')')
    else:
        print('[ ] ' + test_name + '(' + summary_results[test_name][1] + ')')


print('\nTests that should fail:')
for test_name in sorted(summary_results_n):
    if summary_results_n[test_name][0] == 'pass':
        print('[ ] ' + test_name + '(' + summary_results_n[test_name][1] + ')')
    else:
        print('[X] ' + test_name + '(' + summary_results_n[test_name][1] + ')')

############
# CLEAN-UP #
############

# remove all the intermediate file output if the clean flag is set
if args.clean:
    file_exts = ['*.outgdc', '*.dif', '*.c', '*.exec']
    for ext in file_exts:
        for directory in os.walk(path):
            for f in glob.glob(os.path.join(path, directory[0], ext)):
                os.remove(f)
        for directory in os.walk(npath):
            for f in glob.glob(os.path.join(path, directory[0], ext)):
                os.remove(f)

