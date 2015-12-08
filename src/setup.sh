#!/bin/bash

# make the c library files
cd clib ; make ; cd ..

# turn the clib files into a library file
cd clib ; ar -cvq libdots.a *.o ; cd ..
