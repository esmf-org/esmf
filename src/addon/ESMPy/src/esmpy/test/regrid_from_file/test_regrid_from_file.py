# $Id$

"""
Reads each line of a control file where each line corresponds to one test
case.    Parses each line and calls a test subroutine that creates meshes from
source and destination NetCDF files, creates an analytic field across the
source mesh, regrids the source mesh to the grid of the destination mesh,
and compares the analytic field of the resulting regridded mesh to that of the
source mesh.
"""

import pytest

import sys
import os
import traceback

from esmpy import *
from esmpy.api.constants import _ESMF_NETCDF, _ESMF_PIO
from esmpy.test.regrid_from_file.regrid_from_file_consts import DATA_SUBDIR
from esmpy.test.regrid_from_file.regrid_check import regrid_check
from esmpy.test.regrid_from_file.read_test_cases_from_control_file import read_control_file


# Start up esmpy
mg = Manager(debug=True)

if mg.pet_count == 1:
    import esmpy.test.regrid_from_file.test_regrid_from_file_download

# Read the test case parameters from the control file.
print('Reading control file...')
test_cases = read_control_file()

# For each test case line from the control file parse the line and call
# the test subroutine.

@pytest.mark.skipif(_ESMF_PIO==False, reason="PIO required in ESMF build")
@pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
@pytest.mark.parametrize('test_case', test_cases)
def test_run_regrid_from_file(test_case):
    (src_fname, dst_fname, regrid_method, options, 
     itrp_mean_err, itrp_max_err, csrv_err) = test_case
    test_str = 'Regrid %s to %s as %s with %s itrp_mean_err=%f, itrp_max_err=%f, and csrv_err=%f' % (src_fname, dst_fname, regrid_method, options, itrp_mean_err, itrp_max_err, csrv_err)
    if local_pet() == 0:
        print ('\n' + test_str)
    src_fname_full = os.path.join(DATA_SUBDIR, src_fname)
    dst_fname_full = os.path.join(DATA_SUBDIR, dst_fname)
    
    # run the data file retrieval and regridding through try/except
    regrid_check(src_fname_full, dst_fname_full, regrid_method, 
                               options, itrp_mean_err, itrp_max_err, csrv_err)
