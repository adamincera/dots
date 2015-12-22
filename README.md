# D.O.T.S 
 
Create a versatile language to allow simple implementation of graph data structures and algorithms.

### Authors:

Hosanna Fuller, manager
Rachel Gordon, language guru
Adam Incera, system architect
Yumeng Liao, yl2908, tester

###  INSTALLATION INSTRUCTIONS:

Add the following line to your .bash_profile:
    export DOTS=/Users/rcgordon/code/class/coms4115-plt/dots/src/clib
    
    This adds the DOTS environment variable to your shell's path, which is a dependency for the dots compiler.
    in order to get the latest changes, run:
    make clean;
    make library
This turns the clib dependnecies into a library archive that the dots compiler uses.

In your dots compiler folder, run:
    make setup
    make

###  COMPILATON INSTRUCTIONS:

In order to compile the ./dotc compiler you must run:
``` unix 
   make  clean; make
 ```

Then to test specific files you can either run the file directly with the ./dotc compiler ie:
  ``` unix
   ./dotc dtest/example.dots
   ./exec example.dots.c
  ```
Or if you want to execute the file in one fell swoop:
   ``` unix
   ./gdc dtest/example.dots
   ```
