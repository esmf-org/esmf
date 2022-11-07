# This example demonstrates how to regrid between a LocStream and a Grid.

import esmpy
import numpy

import os

import esmpy.util.helpers as helpers
import esmpy.api.constants as constants
from esmpy.util.cache_data import DATA_DIR
from esmpy.util.exceptions import DataMissing

# The data files can be retrieved from the ESMF data repository by uncommenting the
# following block of code:
#
# from esmpy.util.cache_data import cache_data_file
# cache_data_file(os.path.join(DATA_DIR, "ll1deg_grid.nc"))

# This call enables debug logging
esmpy.Manager(debug=True)

grid1 = os.path.join(DATA_DIR, "ll1deg_grid.nc")
if not os.path.exists(grid1):
    raise DataMissing("Data not available, try 'make download'.")

grid = esmpy.Grid(filename=grid1, filetype=esmpy.FileFormat.SCRIP)

from esmpy.util.locstream_utilities import create_locstream_spherical_16, create_locstream_spherical_16_parallel
coord_sys=esmpy.CoordSys.SPH_DEG
domask=True
if esmpy.pet_count() == 1:
    locstream = create_locstream_spherical_16(coord_sys=coord_sys, domask=domask)
else:
    locstream = create_locstream_spherical_16_parallel(coord_sys=coord_sys, domask=domask)

# create a field
srcfield = esmpy.Field(locstream, name='srcfield')

dstfield = esmpy.Field(grid, name='dstfield')
xctfield = esmpy.Field(grid, name='xctfield')

# initialize the fields
[x, y] = [0, 1]
deg2rad = 3.14159/180

gridXCoord = locstream["ESMF:Lon"]
gridYCoord = locstream["ESMF:Lat"]
if coord_sys == esmpy.CoordSys.SPH_DEG:
    srcfield.data[...] = 10.0 + numpy.cos(gridXCoord * deg2rad) ** 2 + numpy.cos(2 * gridYCoord * deg2rad)
elif coord_sys == esmpy.CoordSys.SPH_RAD:
    srcfield.data[...] = 10.0 + numpy.cos(gridXCoord) ** 2 + numpy.cos(2 * gridYCoord)
else:
    raise ValueError("coordsys value does not apply in this example")

gridXCoord = xctfield.grid.get_coords(x)
gridYCoord = xctfield.grid.get_coords(y)
xctfield.data[...] = 10.0 + numpy.cos(gridXCoord * deg2rad) ** 2 + numpy.cos(2 * gridYCoord * deg2rad)


dstfield.data[...] = 1e20

# create an object to regrid data from the source to the destination field
mask_values=None
if domask:
    mask_values=numpy.array([0])

regrid = esmpy.Regrid(srcfield, dstfield,
                     regrid_method=esmpy.RegridMethod.NEAREST_DTOS,
                     unmapped_action=esmpy.UnmappedAction.ERROR,
                     src_mask_values=mask_values)

# do the regridding from source to destination field
dstfield = regrid(srcfield, dstfield, zero_region=esmpy.Region.SELECT)

# compute the mean relative error
from operator import mul
num_nodes = numpy.prod(xctfield.data.shape[:])
relerr = 0
meanrelerr = 0

dstfield = numpy.ravel(dstfield.data)
xctfield = numpy.ravel(xctfield.data)

if num_nodes != 0:
    ind = numpy.where((dstfield != 1e20) & (xctfield != 0))[0]
    relerr = numpy.sum(numpy.abs(dstfield[ind] - xctfield[ind]) / numpy.abs(xctfield[ind]))
    meanrelerr = relerr / num_nodes

# handle the parallel case
if esmpy.pet_count() > 1:
    relerr = helpers.reduce_val(relerr, op=constants.Reduce.SUM)
    num_nodes = helpers.reduce_val(num_nodes, op=constants.Reduce.SUM)

# output the results from one processor only
if esmpy.local_pet() == 0:
    meanrelerr = relerr / num_nodes
    print ("ESMPy LocStream Grid Regridding Example")
    print ("  interpolation mean relative error = {0}".format(meanrelerr))

    assert (meanrelerr < 9e-5)
