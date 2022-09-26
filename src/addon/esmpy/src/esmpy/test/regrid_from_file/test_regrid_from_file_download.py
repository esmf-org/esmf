"""
Reads each line of a control file where each line corresponds to one test
case for regridding from a source file coordinate grid to a destination file
coordinate grid.    Parses each line and retrieves the source and destination
files from a remote server if they do not already exist locally.  On parallel
computers where the compute nodes do not have internet access, this script must
be run on the front-end machine before running test_regrid_from_file.py to do 
the regridding.
"""

import sys
import os

from esmpy.util.cache_data import cache_data_file, DATA_DIR
from esmpy.test.regrid_from_file.read_test_cases_from_control_file import read_control_file

def cache_data_files_for_test_cases(test_cases):
    wget = True
    if 'ESMPY_DATA_DIR' in os.environ:
        wget = False
    else:
        print ('Data directory: {}'.format(DATA_DIR))

    # Create data subdirectory if it doesn't exist.
    if not os.path.exists(DATA_DIR):
        os.mkdir(DATA_DIR)
  
    if wget:
      # For each test case line from the control file parse the line and call
      # the test subroutine.
      for test_case in test_cases:
          (src_fname, dst_fname, regrid_method, options, mean_err, max_err, 
           max_area_err) = test_case
          src_fname_full = os.path.join(DATA_DIR, src_fname)
          dst_fname_full = os.path.join(DATA_DIR, dst_fname)
  
          # run the data file retrieval and regridding through try/except
          status_ok = cache_data_file(src_fname_full) and cache_data_file(dst_fname_full)
          if not status_ok:
              raise ESMPyException("Error downloading files")
    return

# Read the test case parameters from the control file.
test_cases = read_control_file()

# Retrieve the data files needed for the test cases from the remote server.
cache_data_files_for_test_cases(test_cases)
