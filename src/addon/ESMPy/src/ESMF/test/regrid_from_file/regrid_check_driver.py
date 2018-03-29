# $Id$

"""
Reads each line of a control file where each line corresponds to one test
case.    Parses each line and calls a test subroutine that creates meshes from
source and destination NetCDF files, creates an analytic field across the
source mesh, regrids the source mesh to the grid of the destination mesh,
and compares the analytic field of the resulting regridded mesh to that of the
source mesh.
"""

import sys
import os
import traceback

try:
    import ESMF
except:
    raise ImportError('The ESMF library cannot be found!')
from ESMF.test.regrid_from_file.regrid_from_file_consts import DATA_SUBDIR
from ESMF.test.regrid_from_file.run_regrid_from_file_dryrun import cache_data_files_for_test_cases
from ESMF.test.regrid_from_file.regrid_check import regrid_check
from ESMF.test.regrid_from_file.read_test_cases_from_control_file import read_control_file

# Start up ESMF and run regrid test for each line of options
# read from a control file.    Retrieve data files for each test from a remote
# server if they do not exist locally.
# Start up ESMF.
mg = ESMF.Manager(debug=True)

parallel = False
if ESMF.pet_count() > 1:
    parallel = True

# Read the test case parameters from the control file.

print('Reading control file...')
test_cases = read_control_file()
if (ESMF.local_pet() == 0):
    # Retrieve the data files needed for the test cases from the remote server.
    print('Retrieving regrid_from_file data...')
    status_ok = cache_data_files_for_test_cases(test_cases)

# For each test case line from the control file parse the line and call
# the test subroutine.
for ctr, test_case in enumerate(test_cases, start=1):
    print('Running {0} of {1} regrid_from_file test cases...'.format(ctr, len(test_cases)))
    (src_fname, dst_fname, regrid_method, options, 
     itrp_mean_err, itrp_max_err, csrv_err) = test_case
    test_str = 'Regrid %s to %s as %s with %s itrp_mean_err=%f, itrp_max_err=%f, and csrv_err=%f' % (src_fname, dst_fname, regrid_method, options, itrp_mean_err, itrp_max_err, csrv_err)
    print ('\n' + test_str + ' - START\n')
    src_fname_full = os.path.join(DATA_SUBDIR, src_fname)
    dst_fname_full = os.path.join(DATA_SUBDIR, dst_fname)

    if parallel:
        # set a barrier to wait for files to be downloaded
        mg.barrier()

    # run the data file retrieval and regridding through try/except
    correct = False
    try:
        correct = regrid_check(src_fname_full, dst_fname_full, regrid_method, 
                               options, itrp_mean_err, itrp_max_err, csrv_err)
    except:
        print ("Regridding ERROR:\n")
        traceback.print_exc(file=sys.stdout)

    skip = False
    for i in range(ESMF.pet_count()):
        for line in open("PET"+str(i)+".ESMF_LogFile"):
            if "ESMF_NETCDF not defined when lib was compiled" in line or \
              "File format is not supported" in line:
              # set skip
              skip = True

    # print the file
    print ('\n***NOTE*** The log files must be deleted in this test case, they are printed below for future reference\n')
    if skip:
        for line in open("PET"+str(i)+".ESMF_LogFile"):
            print (line)
            
    # clean the log files
    for i in range(ESMF.pet_count()):
        os.system("echo ' ' > PET"+str(i)+".ESMF_LogFile")
        
    print ("\nPET: " + str(ESMF.local_pet()) + " - " + test_str + " - FINISH\n")

    if skip:
        print ('RESULT: SKIP\n\n')
    elif correct:
        print ('RESULT: PASS\n\n')
    else:
        print ('RESULT: FAIL\n\n')
