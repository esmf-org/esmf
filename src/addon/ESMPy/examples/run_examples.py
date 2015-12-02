# $Id$

import os
import sys

import ESMF
import ESMF.api.constants as constants
from ESMF.test.regrid_from_file.run_regrid_from_file_dryrun import cache_data_file

parallel = False
if len(sys.argv) > 1:
    if "--parallel" in sys.argv[1]:
        parallel = True

# data files
datafilelist = ["ll1deg_grid.nc", "ll2.5deg_grid.nc", "mpas_uniform_10242_dual_counterclockwise.nc",
                "GRIDSPEC_ACCESS1.nc", "tx0.1v2_070911.nc", "T42_grid.nc", "so_Omon_GISS-E2.nc"]

# download the test files
if not parallel:

    # Create data subdirectory if it doesn't exist.
    datadir = "examples/data"
    if not os.path.exists(datadir):
        os.mkdir(datadir)

        # Retrieve the data files needed for the test cases from the remote server.
        for fname in datafilelist:
            status_ok = cache_data_file(os.path.join(datadir,fname))
            if not status_ok:
                raise IOError

# run utests, pipe to file
num_proc = 1
if parallel:
    # make sure we are not in uni mode
    if constants._ESMF_COMM == constants._ESMF_COMM_MPIUNI:
        raise ValueError("Cannot run parallel tests when ESMF is built with ESMF_COMM=mpiuni")

    # setup the constants
    num_proc = 4

os.system(constants._ESMF_MPIRUN + " -n " + str(
    num_proc) + " nosetests -vs --with-id -a '!slow' examples/exampletest.py")