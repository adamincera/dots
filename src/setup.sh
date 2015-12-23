#!/bin/bash

# NOTE: run this script from within the directory it's in or
# variables won't be set up correctly

# make the C library archive
make setup

# make the dots compiler
make

# add clib to the DOTS environment variable
export DOTS=$(pwd)/clib
