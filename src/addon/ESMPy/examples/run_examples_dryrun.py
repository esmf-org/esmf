# $Id$

import os

import ESMF
from ESMF.util.cache_data import cache_data_file

# data files
datafilelist = ["ll1deg_grid.nc", "ll2.5deg_grid.nc", 
                "mpas_uniform_10242_dual_counterclockwise.nc",
                "GRIDSPEC_ACCESS1.nc", "tx0.1v2_070911.nc", 
                "T42_grid.nc", "so_Omon_GISS-E2.nc"]

# Create data subdirectory if it doesn't exist.
datadir = "examples/data"
if not os.path.exists(datadir):
    os.mkdir(datadir)

# download the test files
for fname in datafilelist:
    # Retrieve the data files needed for the test cases from the remote server.
    status_ok = cache_data_file(os.path.join(datadir, fname))
    if not status_ok:
         raise IOError
