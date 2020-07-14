# This example demonstrates how to regrid between a GRIDSPEC grid and a tripole grid,
# both grids use masking.
# The data files can be retrieved from the ESMF data repository by uncommenting the
# following block of code:
#
# import os
# DD = os.path.join(os.getcwd(), "examples/data")
# if not os.path.isdir(DD):
#     os.makedirs(DD)
# from ESMF.util.cache_data import cache_data_file
# cache_data_file(os.path.join(DD, "so_Omon_GISS-E2.nc"))
# cache_data_file(os.path.join(DD, "ll1deg_grid.nc"))

import ESMF

# This call enables debug logging
esmpy = ESMF.Manager(debug=True)

datafile = "examples/data/so_Omon_GISS-E2.nc"
gridfile = "examples/data/ll1deg_grid.nc"

# Create a  grid from a GRIDSPEC formatted file
srcgrid = ESMF.Grid(filename=datafile, filetype=ESMF.FileFormat.GRIDSPEC)

# Create a field on the center stagger locations of the source grid with
# ungridded dimensions large enough to receive the data from file
# dimensions follow Fortran index order: lon, lat, level, time
srcfield = ESMF.Field(srcgrid, staggerloc=ESMF.StaggerLoc.CENTER, ndbounds=[33, 2])

# Read the field data into the data structure
srcfield.read(filename=datafile, variable="so", timeslice=2)

# Create a 1 degree latlon grid
dstgrid = ESMF.Grid(filename=gridfile, filetype=ESMF.FileFormat.SCRIP)

# Create a field on the center stagger locations of the latlon grid, also with
# ungridded dimensions large enough to recieve the data from the source field
dstfield = ESMF.Field(dstgrid, name='dstfield', meshloc=ESMF.StaggerLoc.CENTER,
                      ndbounds=[33, 2])
dstfield.data[...] = 1e20

# Create an object to regrid data from the source to the destination field
regrid = ESMF.Regrid(srcfield, dstfield,
                     regrid_method=ESMF.RegridMethod.BILINEAR,
                     unmapped_action=ESMF.UnmappedAction.IGNORE)

# Do the regridding from source to destination field
dstfield = regrid(srcfield, dstfield)

# Output the results from one processor only
if ESMF.local_pet() == 0: print ("ESMPy Field Data Regridding Example Finished Successfully")