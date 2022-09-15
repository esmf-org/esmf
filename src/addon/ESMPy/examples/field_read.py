# This example demonstrates how to regrid between a GRIDSPEC grid and a tripole grid,
# both grids use masking.
# The data files can be retrieved from the ESMF data repository by uncommenting the
# following block of code:
#
# import os
# DD = os.path.join(os.getcwd(), "examples/data")
# if not os.path.isdir(DD):
#     os.makedirs(DD)
# from esmpy.util.cache_data import cache_data_file
# cache_data_file(os.path.join(DD, "so_Omon_GISS-E2.nc"))
# cache_data_file(os.path.join(DD, "ll1deg_grid.nc"))

import esmpy

# This call enables debug logging
esmpy.Manager(debug=True)

datafile = "examples/data/so_Omon_GISS-E2.nc"
gridfile = "examples/data/ll1deg_grid.nc"

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
