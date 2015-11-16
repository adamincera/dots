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
for dir_entry in os.listdir(path):
    filepath = os.path.join(path, dir_entry)
    if os.path.isfile(filepath) and filepath[-5:] == '.dots':
        print('\nRunning tests in ' + dir_entry)
        print('================================')

        comp_success = False
        try:
            return_code = call(['./gdc', filepath, dir_entry[:-5]], timeout=30)
            if return_code == 0:
                print 'COMPILATION SUCCESSFUL'
                comp_success = True
            else:
                print 'COMPILATION FAILED'
        except:
            print 'compile executable. Stop.'
            continue;

        if (comp_success):
            out_child = Popen('./' + dir_entry[:-5], shell=True, stdout=PIPE)
            output = out_child.communicate()[0]
            
            output_filepath = os.path.join(path, dir_entry[:-5] + '.outgdc')
            with open(output_filepath, 'w') as intermediate_output:
                intermediate_output.write(output)

            out_filepath = os.path.join(path, dir_entry[:-5] + '.out')
            output_filepath = os.path.join(path, dir_entry[:-5] + '.outgdc')

            diff_command = ['diff', '-bB', out_filepath, output_filepath]
            diff_child = Popen(diff_command, stdout=PIPE)
            diff_output = diff_child.communicate()[0]

            if diff_output.strip() == '':
                print 'PASSED TEST'
            else: 
                print 'FAILED TEST....writing diff files'
                with open(os.path.join(path, dir_entry[:-5] + '.dif'), 'w') as output_diff:
                    output_diff.write(diff_output.strip())

print('\n Tests completed.')

############
# CLEAN-UP #
############

# remove all the intermediate file output if the clean flag is set
if args.clean:
    file_exts = ['*.outgdc', '*.dif', '*.c']
    for ext in file_exts:
        for f in glob.glob(os.path.join(path, ext)):
            os.remove(f)


