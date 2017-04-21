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
# cache_data_file(os.path.join(DD, "tasmax_day_CanCM4_decadal2010_r2i1p1_20110101-20201231.nc"))
# cache_data_file(os.path.join(DD, "ll1deg_grid.nc"))

import ESMF

# This call enables debug logging
esmpy = ESMF.Manager(debug=True)

datafile = "examples/data/tasmax_day_CNRM-CM5_historical_r1i1p1_18500101-1850010.nc"
gridfile = "examples/data/ll1deg_grid.nc"

# Create a  grid from a GRIDSPEC formatted file
srcgrid = ESMF.Grid(filename=datafile, filetype=ESMF.FileFormat.GRIDSPEC)

# create a field on the center stagger locations of the source grid
srcfield = ESMF.Field(srcgrid, name='srcfield', staggerloc=ESMF.StaggerLoc.CENTER, ndbounds=[2])

srcfield.read(filename=datafile, variable="tasmax", ndbounds=2)

# create a tripole grid
dstgrid = ESMF.Grid(filename=gridfile, filetype=ESMF.FileFormat.SCRIP)

# create fields on the center stagger locations of the tripole grid
dstfield = ESMF.Field(dstgrid, name='dstfield', meshloc=ESMF.StaggerLoc.CENTER, ndbounds=[2])
dstfield.data[...] = 1e20

import ipdb; ipdb.set_trace()

# create an object to regrid data from the source to the destination field
regrid = ESMF.Regrid(srcfield, dstfield,
                     regrid_method=ESMF.RegridMethod.BILINEAR,
                     unmapped_action=ESMF.UnmappedAction.IGNORE)

# do the regridding from source to destination field
dstfield = regrid(srcfield, dstfield)

# output the results from one processor only
if ESMF.local_pet() is 0: print ("ESMPy Field Data Regridding Example Finished Successfully")