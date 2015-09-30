# This example demonstrates how to regrid between a Grid and a Mesh.
# The data files can be retrieved from the ESMF data repository by uncommenting the
# following block of code:
#
# import os
# DD = os.path.join(os.getcwd(), "examples/data")
# if not os.path.isdir(DD):
#     os.makedirs(DD)
# from ESMF.util.cache_data import cache_data_file
# cache_data_file(os.path.join(DD, "ll1deg_grid.nc"))
# cache_data_file(os.path.join(DD, "mpas_uniform_10242_dual_counterclockwise.nc"))

import ESMF
import numpy

# This call enables debug logging
# esmpy = ESMF.Manager(debug=True)

grid1 = "examples/data/ll1deg_grid.nc"
grid2 = "examples/data/mpas_uniform_10242_dual_counterclockwise.nc"

# Create a uniform global latlon grid from a SCRIP formatted file
grid = ESMF.Grid(filename=grid1, filetype=ESMF.FileFormat.SCRIP)
# NOTE: corners are needed for conservative regridding
# grid = ESMF.Grid(filename=grid1, filetype=ESMF.FileFormat.SCRIP,
#                  add_corner_stagger=True)

# create a field on the center stagger locations of the source grid
srcfield = ESMF.Field(grid, name='srcfield', staggerloc=ESMF.StaggerLoc.CENTER)

# create an ESMF formatted unstructured mesh with clockwise cells removed
mesh = ESMF.Mesh(filename=grid2, filetype=ESMF.FileFormat.ESMFMESH)

# create a field on the nodes of the destination mesh
dstfield = ESMF.Field(mesh, name='dstfield', meshloc=ESMF.MeshLoc.NODE)
xctfield = ESMF.Field(mesh, name='xctfield', meshloc=ESMF.MeshLoc.NODE)
# NOTE: Field must be built on elements of Mesh for conservative regridding
# dstfield = ESMF.Field(mesh, name='dstfield', meshloc=ESMF.MeshLoc.ELEMENT)
# xctfield = ESMF.Field(mesh, name='xctfield', meshloc=ESMF.MeshLoc.ELEMENT)

# initialize the fields
[lon,lat] = [0, 1]
deg2rad = 3.14159/180

gridXCoord = srcfield.grid.get_coords(lon, ESMF.StaggerLoc.CENTER)
gridYCoord = srcfield.grid.get_coords(lat, ESMF.StaggerLoc.CENTER)
srcfield.data[...] = 10.0 + (gridXCoord * deg2rad) ** 2 + (gridYCoord * deg2rad) ** 2

gridXCoord = xctfield.grid.get_coords(lon, ESMF.StaggerLoc.CENTER)
gridYCoord = xctfield.grid.get_coords(lat, ESMF.StaggerLoc.CENTER)
xctfield.data[...] = 10.0 + (gridXCoord * deg2rad) ** 2 + (gridYCoord * deg2rad) ** 2

dstfield.data[...] = 1e20

# create an object to regrid data from the source to the destination field
regrid = ESMF.Regrid(srcfield, dstfield,
                     regrid_method=ESMF.RegridMethod.BILINEAR,
                     unmapped_action=ESMF.UnmappedAction.ERROR)

# do the regridding from source to destination field
dstfield = regrid(srcfield, dstfield)


# compute the mean relative error
from operator import mul
num_nodes = reduce(mul, xctfield.data.shape)
relerr = 0
meanrelerr = 0
if num_nodes is not 0:
    ind = numpy.where((dstfield.data != 1e20) & (xctfield.data != 0))[0]
    relerr = numpy.sum(numpy.abs(dstfield.data[ind] - xctfield.data[ind]) / numpy.abs(xctfield.data[ind]))
    meanrelerr = relerr / num_nodes

# handle the parallel case
if ESMF.pet_count() > 1:
    try:
        from mpi4py import MPI
    except:
        raise ImportError
    comm = MPI.COMM_WORLD
    relerr = comm.reduce(relerr, op=MPI.SUM)
    num_nodes = comm.reduce(num_nodes, op=MPI.SUM)

# output the results from one processor only
if ESMF.local_pet() is 0:
    meanrelerr = relerr / num_nodes
    print "ESMPy Grid Mesh Regridding Example"
    print "  interpolation mean relative error = {0}".format(meanrelerr)

    assert (meanrelerr < 2e-3)
