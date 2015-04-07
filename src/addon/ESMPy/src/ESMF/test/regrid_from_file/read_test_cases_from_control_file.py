"""
Reads each line of a control file where each line corresponds to one test
case for regridding from a source file coordinate grid to a destination file
coordinate grid.    Parses each line and stores the test parameters in a list.
Each entry in the list is itself a list with the following information about
each test: source filename, destination filename, regrid method, options,
regrid error threshold.
"""

import os
import re

from ESMF.test.regrid_from_file.regrid_from_file_consts import TEST_REGRID_DIR, CONTROL_FNAME


def read_control_file():

    # Parse each test case line from the control file.
    test_cases = []
    for line in open(os.path.join(TEST_REGRID_DIR, CONTROL_FNAME), 'r'):
        if line[0] != '#' and re.match('(\s*.+\s*:){3}', line):
            (src_fname, dst_fname, regrid_method, options, mean_err_str, 
             max_err_str, max_area_err_str) = re.split('\s*:\s*', line)
            test_cases.append([src_fname, dst_fname, regrid_method, options,
                               float(mean_err_str), float(max_err_str), 
                               float(max_area_err_str)])
    return test_cases
