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
# cache_data_file(os.path.join(DD, "GRIDSPEC_ACCESS1.nc"))
# cache_data_file(os.path.join(DD, "tx0.1v2_070911.nc"))

import esmpy
import numpy

import os

import esmpy.util.helpers as helpers
import esmpy.api.constants as constants
from esmpy.util.cache_data import DATA_DIR
from esmpy.util.exceptions import DataMissing

# This call enables debug logging
# esmpy = esmpy.Manager(debug=True)

grid1 = os.path.join(DATA_DIR, "GRIDSPEC_ACCESS1.nc")
if not os.path.exists(grid1):
    raise DataMissing("Data not available, try 'make download'.")
grid2 = os.path.join(DATA_DIR, "tx0.1v2_070911.nc")
if not os.path.exists(grid2):
    raise DataMissing("Data not available, try 'make download'.")

# Create a  grid from a GRIDSPEC formatted file
grid = esmpy.Grid(filename=grid1, filetype=esmpy.FileFormat.GRIDSPEC,
                                add_corner_stagger=True, add_mask=True, varname="so")

# create a field on the center stagger locations of the source grid
srcfield = esmpy.Field(grid, name='srcfield', staggerloc=esmpy.StaggerLoc.CENTER)

# create a tripole grid
tripole = esmpy.Grid(filename=grid2, filetype=esmpy.FileFormat.SCRIP,
                    add_corner_stagger=True, add_mask=True, varname="grid_imask")

# create fields on the center stagger locations of the tripole grid
dstfield = esmpy.Field(tripole, name='dstfield', meshloc=esmpy.StaggerLoc.CENTER)
xctfield = esmpy.Field(tripole, name='xctfield', meshloc=esmpy.StaggerLoc.CENTER)

# create fields needed to analyze accuracy of conservative regridding
srcfracfield = esmpy.Field(grid, name='srcfracfield')
dstfracfield = esmpy.Field(tripole, name='dstfracfield')
srcareafield = esmpy.Field(grid, name='srcareafield')
dstareafield = esmpy.Field(tripole, name='dstareafield')

# get the coordinate pointers and set the coordinates
[lon,lat] = [0, 1]
gridXCoord = srcfield.grid.get_coords(lon, esmpy.StaggerLoc.CENTER)
gridYCoord = srcfield.grid.get_coords(lat, esmpy.StaggerLoc.CENTER)

deg2rad = 3.14159/180

srcfield.data[...] = 10.0 + (gridXCoord*deg2rad)**2 + (gridYCoord*deg2rad)**2

# get the coordinate pointers and set the coordinates
[lon,lat] = [0, 1]
gridXCoord = xctfield.grid.get_coords(lon, esmpy.StaggerLoc.CENTER)
gridYCoord = xctfield.grid.get_coords(lat, esmpy.StaggerLoc.CENTER)

xctfield.data[...] = 10.0 + (gridXCoord*deg2rad)**2 + (gridYCoord*deg2rad)**2

dstfield.data[...] = 1e20

# create an object to regrid data from the source to the destination field
# NOTE: this example requires the unmapped_action flag to be set to IGNORE missing values due to masking
regrid = esmpy.Regrid(srcfield, dstfield,
                     regrid_method=esmpy.RegridMethod.CONSERVE,
                     unmapped_action=esmpy.UnmappedAction.IGNORE,
                     src_mask_values=numpy.array([0]),
                     dst_mask_values=numpy.array([0]),
                     src_frac_field=srcfracfield,
                     dst_frac_field=dstfracfield)

# do the regridding from source to destination field
dstfield = regrid(srcfield, dstfield)

# calculate the area fields
srcareafield.get_area()
dstareafield.get_area()

csrverr = 0
srcmass = numpy.sum(numpy.abs(srcareafield.data * srcfracfield.data * srcfield.data))
dstmass = numpy.sum(numpy.abs(dstareafield.data * dstfield.data))
if dstmass != 0:
    csrverr = numpy.abs(srcmass - dstmass) / dstmass

# compute the mean relative interpolation and conservation error
from operator import mul
num_nodes = numpy.prod(xctfield.data.shape[:])
relerr = 0
meanrelerr = 0
if num_nodes != 0:
    ind = numpy.where((dstfield.data != 1e20) & (xctfield.data != 0) & (dstfracfield.data > .999))[0]
    relerr = numpy.sum(numpy.abs(dstfield.data[ind]/dstfracfield.data[ind] - xctfield.data[ind]) / numpy.abs(xctfield.data[ind]))
    meanrelerr = relerr / num_nodes

# handle the parallel case
if esmpy.pet_count() > 1:
    relerr = helpers.reduce_val(relerr, op=constants.Reduce.SUM)
    num_nodes = helpers.reduce_val(num_nodes, op=constants.Reduce.SUM)
    srcmass = helpers.reduce_val(srcmass, op=constants.Reduce.SUM)
    dstmass = helpers.reduce_val(dstmass, op=constants.Reduce.SUM)

# output the results from one processor only
if esmpy.local_pet() == 0:
    meanrelerr = relerr / num_nodes
    csrverr = numpy.abs(srcmass - dstmass) / dstmass

    print ("ESMPy Tripole Regridding Example")
    print ("  interpolation mean relative error = {0}".format(meanrelerr))
    print ("  mass conservation relative error  = {0}".format(csrverr))

    assert (meanrelerr < 8e-4)
