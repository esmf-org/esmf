# $Id$

import os
import ESMF

# run utests, pipe to file
utestfile=os.path.join('src/ESMF/test/','unit_test.py')
utestoutfile='run_unit_test.log'

if ESMF.constants._ESMF_OS is ESMF.constants._ESMF_OS_UNICOS:
    os.system("aprun -n 1 -a xt "+utestfile+" > "+utestoutfile+" 2>&1")
else:
    os.system("python "+utestfile+" > "+utestoutfile+" 2>&1")

# traverse output, find number of pass and fail and print report
UTEST = open(utestoutfile)

utpass = 0
utfail = 0
utskip = 0

for line in UTEST:
    if 'RESULT: PASS' in line:
        utpass=utpass+1
    if 'RESULT: FAIL' in line:
        utfail=utfail+1
    if 'RESULT: SKIP' in line:
        utskip=utskip+1

UTEST.close()

print "Unit test results: "+utestoutfile
print "PASS = "+str(utpass)
print "FAIL = "+str(utfail)
print "SKIP = "+str(utskip)

if utpass == 0 and utfail == 0 and utskip == 0: 
    print utestoutfile+":"
    os.system("tail "+utestoutfile)
