#!/bin/sh
# see if $2 is a script in $1 dir
if [ -e $1/$2 ]; then
echo "esmfscript"
else
echo "noesmfscript"
fi
