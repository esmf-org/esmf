# This example demonstrates how to regrid between a cubed sphere Grid and a 
# one degree lat-long Grid.
# The data files can be retrieved from the ESMF data repository by uncommenting
# the following block of code:
#
# import os
# DD = os.path.join(os.getcwd(), "examples/data")
# if not os.path.isdir(DD):
#     os.makedirs(DD)
# from ESMF.util.cache_data import cache_data_file
# cache_data_file(os.path.join(DD, "ll1deg_grid.nc"))

import ESMF
import numpy

import ESMF.util.helpers as helpers
import ESMF.api.constants as constants

# This call enables debug logging when debug=True
mg = ESMF.Manager(debug=False)

# if ESMF.pet_count() != 6:
#     print ("ESMPy cubed sphere regridding example requires 6 processors")
#     import sys; sys.exit(0)

grid1 = "examples/data/ll1deg_grid.nc"

# Create a cubed sphere grid with 20 elements per tile
srcgrid = ESMF.Grid(tilesize=20, name="cubed_sphere")

# create an regular lat lon grid from file
dstgrid = ESMF.Grid(filename=grid1, filetype=ESMF.FileFormat.SCRIP)

# create a field on the center stagger locations of the source grid
srcfield = ESMF.Field(srcgrid, name='srcfield', staggerloc=ESMF.StaggerLoc.CENTER)
srcfracfield = ESMF.Field(srcgrid, name='srcfracfield', staggerloc=ESMF.StaggerLoc.CENTER)

# create a field on the center stagger locations of the destination grid
dstfield = ESMF.Field(dstgrid, name='dstfield', staggerloc=ESMF.StaggerLoc.CENTER)
xctfield = ESMF.Field(dstgrid, name='xctfield', staggerloc=ESMF.StaggerLoc.CENTER)
dstfracfield = ESMF.Field(dstgrid, name='dstfracfield', staggerloc=ESMF.StaggerLoc.CENTER)

# initialize the fields
[lon,lat] = [0, 1]

deg2rad = 3.14/180.

gridLon = srcfield.grid.get_coords(lon, ESMF.StaggerLoc.CENTER)
gridLat = srcfield.grid.get_coords(lat, ESMF.StaggerLoc.CENTER)

x = numpy.cos(numpy.radians(gridLon))*numpy.sin(numpy.radians(90-gridLat))
y = numpy.sin(numpy.radians(gridLon))*numpy.sin(numpy.radians(90-gridLat))
z = numpy.cos(numpy.radians(90-gridLat))

srcfield.data[...] = 200.0 + x + y + z

gridLon = xctfield.grid.get_coords(lon, ESMF.StaggerLoc.CENTER)
gridLat = xctfield.grid.get_coords(lat, ESMF.StaggerLoc.CENTER)

x = numpy.cos(numpy.radians(gridLon))*numpy.sin(numpy.radians(90-gridLat))
y = numpy.sin(numpy.radians(gridLon))*numpy.sin(numpy.radians(90-gridLat))
z = numpy.cos(numpy.radians(90-gridLat))

xctfield.data[...] = 200.0 + x + y + z

dstfield.data[...] = 1e20

# write regridding weights to file
import os
filename = "esmpy_example_weight_file_cs.nc"
if ESMF.local_pet() == 0:
    if os.path.isfile(
        os.path.join(os.getcwd(), filename)):
        os.remove(os.path.join(os.getcwd(), filename))

mg.barrier()

regrid = ESMF.Regrid(srcfield, dstfield, filename=filename,
                     regrid_method=ESMF.RegridMethod.BILINEAR,
                     unmapped_action=ESMF.UnmappedAction.ERROR)

mg.barrier()
regrid = ESMF.RegridFromFile(srcfield, dstfield, filename=filename)

# do the regridding from source to destination field
mg.barrier()
dstfield = regrid(srcfield, dstfield)

# compute the mean relative error
from operator import mul
num_nodes = numpy.prod(xctfield.data.shape[:])
relerr = 0
maxrelerr = 0
meanrelerr = 0
if num_nodes is not 0:
    # ind = numpy.where((dstfield.data != 1e20) & (xctfield.data != 0))[0]
    relerr = numpy.sum(numpy.abs(dstfield.data - xctfield.data) / numpy.abs(xctfield.data))
    maxrelerr = numpy.max(numpy.abs(dstfield.data - xctfield.data) / numpy.abs(xctfield.data))

# handle the parallel case
if ESMF.pet_count() > 1:
    relerr = helpers.reduce_val(relerr, op=constants.Reduce.SUM)
    maxrelerr = helpers.reduce_val(maxrelerr, op=constants.Reduce.MAX)
    num_nodes = helpers.reduce_val(num_nodes, op=constants.Reduce.SUM)

# output the results from one processor only
if ESMF.local_pet() is 0:
    meanrelerr = relerr / num_nodes
    print ("ESMPy cubed sphere regridding example")
    print ("  interpolation mean relative error = {0}".format(meanrelerr))
    print ("  interpolation max relative (pointwise) error = {0}".format(maxrelerr))

    # assert (meanrelerr < 3e-3)
    # assert (maxrelerr < 4e-4)
