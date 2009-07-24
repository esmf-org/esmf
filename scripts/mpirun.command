#!/bin/sh
# see if $2 is a script in $1 dir
if [ -f $1/$2 ]; then
echo "esmfscript"
else
echo "noesmfscript"
fi
