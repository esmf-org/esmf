# This example demonstrates how to regrid between a cubed sphere Grid and a 
# one degree lat-long Grid.

import esmpy
import numpy

import os

import esmpy.util.helpers as helpers
import esmpy.api.constants as constants
from esmpy.util.cache_data import DATA_DIR
from esmpy.util.exceptions import DataMissing

# The data files can be retrieved from the ESMF data repository by uncommenting
# the following block of code:
#
# from esmpy.util.cache_data import cache_data_file
# cache_data_file(os.path.join(DATA_DIR, "ll1deg_grid.nc"))

# This call enables debug logging when debug=True
mg = esmpy.Manager(debug=True)

# if esmpy.pet_count() != 6:
#     print ("ESMPy cubed sphere regridding example requires 6 processors")
#     import sys; sys.exit(0)

grid1 = os.path.join(DATA_DIR, "ll1deg_grid.nc")
if not os.path.exists(grid1):
    raise DataMissing("Data not available, try 'make download'.")

# Create a cubed sphere grid with 20 elements per tile
regDecompPTile = numpy.array([[2,2,1,1,1,1],[2,2,2,2,2,2]], dtype=numpy.int32)
srcgrid = esmpy.Grid(tilesize=20, regDecompPTile = regDecompPTile, name="cubed_sphere")

# create an regular lat lon grid from file
dstgrid = esmpy.Grid(filename=grid1, filetype=esmpy.FileFormat.SCRIP)

# create a field on the center stagger locations of the source grid
srcfield = esmpy.Field(srcgrid, name='srcfield', staggerloc=esmpy.StaggerLoc.CENTER)
srcfracfield = esmpy.Field(srcgrid, name='srcfracfield', staggerloc=esmpy.StaggerLoc.CENTER)

# create a field on the center stagger locations of the destination grid
dstfield = esmpy.Field(dstgrid, name='dstfield', staggerloc=esmpy.StaggerLoc.CENTER)
xctfield = esmpy.Field(dstgrid, name='xctfield', staggerloc=esmpy.StaggerLoc.CENTER)
dstfracfield = esmpy.Field(dstgrid, name='dstfracfield', staggerloc=esmpy.StaggerLoc.CENTER)

# initialize the fields
[lon,lat] = [0, 1]

deg2rad = 3.14/180.

gridLon = srcfield.grid.get_coords(lon, esmpy.StaggerLoc.CENTER)
gridLat = srcfield.grid.get_coords(lat, esmpy.StaggerLoc.CENTER)

x = numpy.cos(numpy.radians(gridLon))*numpy.sin(numpy.radians(90-gridLat))
y = numpy.sin(numpy.radians(gridLon))*numpy.sin(numpy.radians(90-gridLat))
z = numpy.cos(numpy.radians(90-gridLat))

srcfield.data[...] = 200.0 + x + y + z

gridLon = xctfield.grid.get_coords(lon, esmpy.StaggerLoc.CENTER)
gridLat = xctfield.grid.get_coords(lat, esmpy.StaggerLoc.CENTER)

x = numpy.cos(numpy.radians(gridLon))*numpy.sin(numpy.radians(90-gridLat))
y = numpy.sin(numpy.radians(gridLon))*numpy.sin(numpy.radians(90-gridLat))
z = numpy.cos(numpy.radians(90-gridLat))

xctfield.data[...] = 200.0 + x + y + z

dstfield.data[...] = 1e20

# write regridding weights to file
filename = "esmpy_example_weight_file_cs.nc"
if esmpy.local_pet() == 0:
    if os.path.isfile(os.path.join(os.getcwd(), filename)):
        os.remove(os.path.join(os.getcwd(), filename))

mg.barrier()

regrid = esmpy.Regrid(srcfield, dstfield, filename=filename,
                     regrid_method=esmpy.RegridMethod.BILINEAR,
                     unmapped_action=esmpy.UnmappedAction.ERROR)

mg.barrier()
regrid = esmpy.RegridFromFile(srcfield, dstfield, filename=filename)

# do the regridding from source to destination field
mg.barrier()
dstfield = regrid(srcfield, dstfield)

# compute the mean relative error
from operator import mul
num_nodes = numpy.prod(xctfield.data.shape[:])
relerr = 0
maxrelerr = 0
meanrelerr = 0
if num_nodes != 0:
    # ind = numpy.where((dstfield.data != 1e20) & (xctfield.data != 0))[0]
    relerr = numpy.sum(numpy.abs(dstfield.data - xctfield.data) / numpy.abs(xctfield.data))
    maxrelerr = numpy.max(numpy.abs(dstfield.data - xctfield.data) / numpy.abs(xctfield.data))

# handle the parallel case
if esmpy.pet_count() > 1:
    relerr = helpers.reduce_val(relerr, op=constants.Reduce.SUM)
    maxrelerr = helpers.reduce_val(maxrelerr, op=constants.Reduce.MAX)
    num_nodes = helpers.reduce_val(num_nodes, op=constants.Reduce.SUM)

# output the results from one processor only
if esmpy.local_pet() == 0:
    meanrelerr = relerr / num_nodes
    print ("ESMPy cubed sphere regridding example")
    print ("  interpolation mean relative error = {0}".format(meanrelerr))
    print ("  interpolation max relative (pointwise) error = {0}".format(maxrelerr))

    if os.path.isfile(os.path.join(os.getcwd(), filename)):
        os.remove(os.path.join(os.getcwd(), filename))

    # assert (meanrelerr < 3e-3)
    # assert (maxrelerr < 4e-4)
