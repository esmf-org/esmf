# $Id$

import os
import ESMF
from regrid_from_file_consts import TEST_REGRID_DIR
# run utests, pipe to file
rtestfile=os.path.join(TEST_REGRID_DIR, 'regrid_check_driver.py')
rtestoutfile='run_regrid_from_file.log'

if ESMF.constants._ESMF_OS is ESMF.constants._ESMF_OS_UNICOS:
    os.system("aprun -n 1 -a xt python "+rtestfile+" > "+rtestoutfile+" 2>&1")
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

print "Regrid from file test results: "+rtestoutfile
print "PASS = "+str(rtpass)
print "FAIL = "+str(rtfail)
print "SKIP = "+str(rtskip)

if rtpass == 0 and rtfail == 0 and rtskip == 0: 
    print rtestoutfile+":"
    os.system("tail "+rtestoutfile)
