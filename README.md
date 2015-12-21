Authors:

Hosanna Fuller, manager
Rachel Gordon, language guru
Adam Incera, system architect
Yumeng Liao, yl2908, tester


INSTALLATION INSTRUCTIONS:


Add the following line to your .bash_profile:
    export DOTS=/Users/rcgordon/code/class/coms4115-plt/dots/src/clib

This adds the DOTS environment variable to your shell's path, which is a dependency for the dots compiler.


In your dots compiler folder, run:
    make setup

This turn the clib dependnecies into a library archive that the dots compiler uses.
