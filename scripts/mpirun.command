#!/bin/sh
# see if $2 is a script in $1 dir and return the correct mpirun command with
# options if so, just unmodified $2 otherwise
if [ -e $1/$2 ]; then
echo $1/$2 $3
else
echo $2
fi
