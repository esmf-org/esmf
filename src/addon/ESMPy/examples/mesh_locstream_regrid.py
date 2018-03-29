# This example demonstrates how to regrid between a mesh and a locstream.

import ESMF
import numpy

import ESMF.util.helpers as helpers
import ESMF.api.constants as constants

# This call enables debug logging
# ESMF.Manager(debug=True)

from ESMF.util.mesh_utilities import mesh_create_5, mesh_create_5_parallel
from ESMF.util.locstream_utilities import create_locstream_16, create_locstream_16_parallel
if ESMF.pet_count() == 1:
    mesh, _, _, _, _, _ = mesh_create_5()
    locstream = create_locstream_16()
else:
    if ESMF.pet_count() is not 4:
        raise ValueError("processor count must be 4 or 1 for this example")
    else:
        mesh, _, _, _, _ = mesh_create_5_parallel()
        locstream = create_locstream_16_parallel()

# create a field
srcfield = ESMF.Field(mesh, name='srcfield')#, meshloc=ESMF.MeshLoc.ELEMENT)

# create a field on the locstream
dstfield = ESMF.Field(locstream, name='dstfield')
xctfield = ESMF.Field(locstream, name='xctfield')

# initialize the fields
[x, y] = [0, 1]
deg2rad = 3.14159/180

gridXCoord = srcfield.grid.get_coords(x)
gridYCoord = srcfield.grid.get_coords(y)
srcfield.data[...] = 10.0 + (gridXCoord * deg2rad) ** 2 + (gridYCoord * deg2rad) ** 2

gridXCoord = locstream["ESMF:X"]
gridYCoord = locstream["ESMF:Y"]
xctfield.data[...] = 10.0 + (gridXCoord * deg2rad) ** 2 + (gridYCoord * deg2rad) ** 2

dstfield.data[...] = 1e20

# create an object to regrid data from the source to the destination field
# TODO: this example seems to fail occasionally with UnmappedAction.ERROR, probably due to a tolerance issue - ask Bob
regrid = ESMF.Regrid(srcfield=srcfield, dstfield=dstfield, regrid_method=ESMF.RegridMethod.BILINEAR,
                     unmapped_action=ESMF.UnmappedAction.IGNORE)

# do the regridding from source to destination field
dstfield = regrid(srcfield, dstfield)

# compute the mean relative error
num_nodes = numpy.prod(xctfield.data.shape[:])
relerr = 0
meanrelerr = 0
if num_nodes is not 0:
    ind = numpy.where((dstfield.data != 1e20) & (xctfield.data != 0))[0]
    relerr = numpy.sum(numpy.abs(dstfield.data[ind] - xctfield.data[ind]) / numpy.abs(xctfield.data[ind]))
    meanrelerr = relerr / num_nodes

# handle the parallel case
if ESMF.pet_count() > 1:
    relerr = helpers.reduce_val(relerr, op=constants.Reduce.SUM)
    num_nodes = helpers.reduce_val(num_nodes, op=constants.Reduce.SUM)

# output the results from one processor only
if ESMF.local_pet() is 0:
    meanrelerr = relerr / num_nodes
    print ("ESMPy Grid Mesh Regridding Example")
    print ("  interpolation mean relative error = {0}".format(meanrelerr))

    assert (meanrelerr < 3e-5)
