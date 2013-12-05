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
import subprocess
import urllib
import traceback
try:
    import ESMF
except:
    raise ImportError('The ESMF library cannot be found!')
import re
from regrid_check import regrid_check
from regrid_from_file_consts import DATA_SUBDIR
from read_test_cases_from_control_file import read_test_cases_from_control_file
from run_regrid_from_file_dryrun import cache_data_files_for_test_cases

# Main program: Start up ESMF and run regrid test for each line of options
# read from a control file.    Retrieve data files for each test from a remote
# server if they do not exist locally.
def main():
    # Start up ESMF.
    esmp = ESMF.Manager(logkind=ESMF.LogKind.MULTI, debug=True)
    pet_count = ESMF.pet_count()

    # Read the test case parameters from the control file.
    test_cases = read_test_cases_from_control_file()

    # Retrieve the data files needed for the test cases from the remote server.
    status_ok = cache_data_files_for_test_cases(test_cases)

    # For each test case line from the control file parse the line and call
    # the test subroutine.
    for test_case in test_cases:
        (src_fname, dst_fname, regrid_method, options, max_err, max_area_err) = test_case
        test_str = 'Regrid %s to %s as %s with %s and max_err = %f' % \
          (src_fname, dst_fname, regrid_method, options, max_err)
        print '\n' + test_str + ' - START\n'
        src_fname_full = os.path.join(DATA_SUBDIR, src_fname)
        dst_fname_full = os.path.join(DATA_SUBDIR, dst_fname)

        # run the data file retrieval and regridding through try/except
        correct = False
        try:
            correct = regrid_check(src_fname_full, dst_fname_full,
                                   regrid_method, options, max_err)
        except:
            print "Regridding ERROR:\n"
            traceback.print_exc(file=sys.stdout)

        skip = False
        for i in range(pet_count):
            for line in open("PET"+str(i)+".ESMF_LogFile"):
                if "ESMF_NETCDF not defined when lib was compiled" in line or \
                  "File format is not supported" in line:
                  # set skip
                  skip = True

        # print the file
        print '\n***NOTE*** The log files must be deleted in this test case, they are printed below for future reference\n'
        if skip:
            for line in open("PET"+str(i)+".ESMF_LogFile"):
                print line
                
        # clean the log files
        for i in range(pet_count):
            os.system("echo ' ' > PET"+str(i)+".ESMF_LogFile")
            
        print '\n' + test_str + ' - FINISH\n'

        if skip:
            print 'RESULT: SKIP\n\n'
        elif correct:
            print 'RESULT: PASS\n\n'
        else:
            print 'RESULT: FAIL\n\n'
            
    return 0

if __name__ == '__main__':
    sys.exit(main())
