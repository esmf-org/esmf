# $Id$

import os
import ESMF

# run regrid tests, pipe to file
REGRID_TEST_DIR = './src/ESMF/test/regrid_test'
regridtestoutfile='run_regrid.log'
regridtestfiles_temp = [ \
'grid_grid_regrid_csrv_mask_test.py',
'grid_grid_regrid_csrv_mask_3D_test.py',
'grid_mesh_regrid_test.py',
'grid_mesh_regrid_csrv_test.py',
'grid_mesh_regrid_csrv_mask_test.py',
'grid_mesh_regrid_mask_test.py',
'mesh_mesh_regrid_test.py',
'field_regridding_test.py',
]

regridtestfiles = [os.path.join(REGRID_TEST_DIR, a) for a in regridtestfiles_temp]

if ESMF.constants._ESMF_OS is ESMF.constants._ESMF_OS_UNICOS:
    os.system("echo 'regrid test output:' > "+regridtestoutfile)
else:
    os.system("echo 'regrid test output:' > "+regridtestoutfile)

for test in regridtestfiles:
    if ESMF.constants._ESMF_OS is ESMF.constants._ESMF_OS_UNICOS:
        os.system("aprun -n 1 "+test+" >> "+regridtestoutfile+" 2>&1")
    else:
        os.system("python "+test+" >> "+regridtestoutfile+" 2>&1")

# traverse output, find number of pass and fail and print report
REGRIDTEST = open(regridtestoutfile)

rtpass = 0
rtfail = 0

for line in REGRIDTEST:
    if 'PASS' in line:
        rtpass=rtpass+1
    if 'FAIL' in line:
        rtfail=rtfail+1

REGRIDTEST.close()

rtcrash = len(regridtestfiles) - rtpass - rtfail

print "Regrid test results: "+regridtestoutfile
print "PASS  = "+str(rtpass)
print "FAIL  = "+str(rtfail)
print "CRASH = "+str(rtcrash)

if rtpass == 0 and rtfail == 0: 
    print regridtestoutfile+":"
    os.system("tail "+regridtestoutfile)
