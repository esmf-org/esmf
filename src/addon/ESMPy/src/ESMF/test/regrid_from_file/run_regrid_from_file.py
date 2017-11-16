# $Id$

import os
import sys

import ESMF.api.constants as constants
from ESMF.test.regrid_from_file.regrid_from_file_consts import TEST_REGRID_DIR


parallel = False
if len(sys.argv) > 1:
    if "--parallel" in sys.argv[1]:
        parallel = True

# run utests, pipe to file
rtestfile=os.path.join(TEST_REGRID_DIR, 'regrid_check_driver.py')
rtestoutfile='run_regrid_from_file.log'
num_proc = 1
if parallel:
    # make sure we are not in uni mode
    if constants._ESMF_COMM == constants._ESMF_COMM_MPIUNI:
        raise ValueError("Cannot run parallel tests when ESMF is built with ESMF_COMM=mpiuni")

    # setup the constants
    num_proc = 4
    rtestoutfile='run_regrid_from_file_parallel.log'

constants._ESMF_MPIRUN + " -n "
os.system(constants._ESMF_MPIRUN + " -n " + str(
    num_proc) + " python " + rtestfile + " > " + rtestoutfile + " 2>&1")

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

rtpass = rtpass/num_proc
rtfail = rtfail/num_proc
rtskip = rtskip/num_proc

print("Regrid from file test results: "+rtestoutfile)
print("PASS  = "+str(int(rtpass)))
print("FAIL  = "+str(int(rtfail)))
print("SKIP  = "+str(int(rtskip)))

if rtpass == 0 and rtfail == 0 and rtskip == 0: 
    print(rtestoutfile+":")
    os.system("tail "+rtestoutfile)
