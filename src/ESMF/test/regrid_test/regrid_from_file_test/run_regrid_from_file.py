#!/usr/bin/env python
#
# $Id: utest_regrid_run.py,v 1.1.2.3 2013/04/09 01:18:01 rokuingh Exp $

import os
import ESMF

# run utests, pipe to file
REGRID_TEST_DIR = 'src/ESMF/test/regrid_test/regrid_from_file_test/'
rtestfile=os.path.join(REGRID_TEST_DIR, 'regrid_check_driver.py')
rtestoutfile='run_regrid.log'

if ESMF.constants._ESMF_OS is ESMF.constants._ESMF_OS_UNICOS:
    os.system("aprun -n 1 -a xt "+rtestfile+" > "+rtestoutfile+" 2>&1")
else:
    os.system("python "+rtestfile+" > "+rtestoutfile+" 2>&1")


# traverse output, find number of pass and fail and print report
RTEST = open(rtestoutfile)

rtpass = 0
rtfail = 0
rtskip = 0

for line in RTEST:
    if 'RESULT: PASS' in line:
        rtpass=rtpass+1
    if 'RESULT: FAIL' in line:
        rtfail=rtfail+1
    if 'RESULT: SKIP' in line:
        rtskip=rtskip+1

RTEST.close()

print "Regrid unit test results: "+rtestoutfile
print "PASS = "+str(rtpass)
print "FAIL = "+str(rtfail)
print "SKIP = "+str(rtskip)

if rtpass == 0 and rtfail == 0 and rtskip == 0: 
    print rtestoutfile+":"
    os.system("tail "+rtestoutfile)