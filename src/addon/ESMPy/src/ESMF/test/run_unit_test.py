# $Id$

import os, sys
import ESMF
import ESMF.api.constants as constants


parallel = False
if len(sys.argv) > 1:
    if "--parallel" in sys.argv[1]:
        parallel = True

# run utests, pipe to file
utestfile=os.path.join('./src/ESMF/test/','unit_test.py')
utestoutfile='run_unit_test.log'
num_proc = 1
if parallel:
    # make sure we are not in uni mode
    if constants._ESMF_COMM == constants._ESMF_COMM_MPIUNI:
        raise ValueError("Cannot run parallel tests when ESMF is built with ESMF_COMM=mpiuni")

    # setup the constants
    num_proc = 4
    utestoutfile='run_unit_tests_parallel.log'

if ESMF.constants._ESMF_OS is ESMF.constants._ESMF_OS_UNICOS:
    os.system("aprun -n "+str(num_proc)+" -a xt python "+utestfile+" > "+utestoutfile+" 2>&1")
else:
    if parallel:
        os.system("mpirun -np "+str(num_proc)+" python "+utestfile+" > "+utestoutfile+" 2>&1")
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

utpass = utpass/num_proc
utfail = utfail/num_proc
utskip = utskip/num_proc

print "Unit test results: "+utestoutfile
print "PASS = "+str(utpass)
print "FAIL = "+str(utfail)
print "SKIP = "+str(utskip)

if utpass == 0 and utfail == 0 and utskip == 0: 
    print utestoutfile+":"
    os.system("tail "+utestoutfile)
