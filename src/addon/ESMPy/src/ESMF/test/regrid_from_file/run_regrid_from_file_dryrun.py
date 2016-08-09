"""
Reads each line of a control file where each line corresponds to one test
case for regridding from a source file coordinate grid to a destination file
coordinate grid.    Parses each line and retrieves the source and destination
files from a remote server if they do not already exist locally.  On parallel
computers where the compute nodes do not have internet access, this script must
be run on the front-end machine before running run_regrid_from_file.py to do 
the regridding.
"""

import sys
import os

from ESMF.test.regrid_from_file.regrid_from_file_consts import DATA_SUBDIR, DATA_URL_ROOT
from ESMF.util.cache_data import cache_data_file
from ESMF.test.regrid_from_file.read_test_cases_from_control_file import read_control_file

def cache_data_files_for_test_cases(test_cases):
    # Create data subdirectory if it doesn't exist.
    if not os.path.exists(DATA_SUBDIR):
        os.mkdir(DATA_SUBDIR)

    # For each test case line from the control file parse the line and call
    # the test subroutine.
    status_ok = True
    for test_case in test_cases:
        (src_fname, dst_fname, regrid_method, options, mean_err, max_err, 
         max_area_err) = test_case
        src_fname_full = os.path.join(DATA_SUBDIR, src_fname)
        dst_fname_full = os.path.join(DATA_SUBDIR, dst_fname)

        # run the data file retrieval and regridding through try/except
        correct = False
        status_ok = cache_data_file(src_fname_full) and cache_data_file(dst_fname_full, DATA_URL_ROOT)
        if not status_ok:
            break
    return status_ok

# Main program:  Retrieve data files from a remote server if they do not exist 
# locally for each test read from a control file.
def main():
    # Read the test case parameters from the control file.
    test_cases = read_control_file()

    # Retrieve the data files needed for the test cases from the remote server.
    status_ok = cache_data_files_for_test_cases(test_cases)
    if status_ok:
        print ('RESULT: PASS - regrid_from_file_dryrun ok\n\n')
    else:
        print ('RESULT: FAIL - regrid_from_file_dryrun error\n\n')

if __name__ == '__main__':
    sys.exit(main())
