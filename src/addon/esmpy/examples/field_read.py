# This example demonstrates how to regrid between a GRIDSPEC grid and a tripole grid,
# both grids use masking.

import esmpy

import os

from esmpy.util.cache_data import DATA_DIR
from esmpy.util.exceptions import DataMissing

# The data files can be retrieved from the ESMF data repository by uncommenting the
# following block of code:
#
# from esmpy.util.cache_data import cache_data_file
# cache_data_file(os.path.join(DATA_DIR, "so_Omon_GISS-E2.nc"))
# cache_data_file(os.path.join(DATA_DIR, "ll1deg_grid.nc"))

# This call enables debug logging
esmpy.Manager(debug=True)

datafile = os.path.join(DATA_DIR, "so_Omon_GISS-E2.nc")
if not os.path.exists(datafile):
    raise DataMissing("Data not available, try 'make download'.")

gridfile = os.path.join(DATA_DIR, "ll1deg_grid.nc")
if not os.path.exists(gridfile):
    raise DataMissing("Data not available, try 'make download'.")

# Create a  grid from a GRIDSPEC formatted file
srcgrid = esmpy.Grid(filename=datafile, filetype=esmpy.FileFormat.GRIDSPEC)

# Create a field on the center stagger locations of the source grid with
# ungridded dimensions large enough to receive the data from file
# dimensions follow Fortran index order: lon, lat, level, time
srcfield = esmpy.Field(srcgrid, staggerloc=esmpy.StaggerLoc.CENTER, ndbounds=[33, 2])

# Read the field data into the data structure
srcfield.read(filename=datafile, variable="so", timeslice=2)

# Create a 1 degree latlon grid
dstgrid = esmpy.Grid(filename=gridfile, filetype=esmpy.FileFormat.SCRIP)

# Create a field on the center stagger locations of the latlon grid, also with
# ungridded dimensions large enough to recieve the data from the source field
dstfield = esmpy.Field(dstgrid, name='dstfield', meshloc=esmpy.StaggerLoc.CENTER,
                      ndbounds=[33, 2])
dstfield.data[...] = 1e20

# Create an object to regrid data from the source to the destination field
regrid = esmpy.Regrid(srcfield, dstfield,
                     regrid_method=esmpy.RegridMethod.BILINEAR,
                     unmapped_action=esmpy.UnmappedAction.IGNORE)

# Do the regridding from source to destination field
dstfield = regrid(srcfield, dstfield)

# Output the results from one processor only
if esmpy.local_pet() == 0: print ("ESMPy Field Data Regridding Example Finished Successfully")
