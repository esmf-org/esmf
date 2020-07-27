# This example demonstrates how to call ESMPy regridding as a parallel
# subprocess spawned using mpi4py from a serial Python driver.
#
# NOTE: MPI.COMM_WORLD.Spawn does not seem to work for mpi4py installations
#       installations built with mpich, however openmpi does work (July 2016).
#
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

import numpy
from mpi4py import MPI
import sys

def regrid():
    try:
        import ESMF
    except:
        raise ImportError("ESMF is not available on this machine")

    grid1 = "examples/data/ll1deg_grid.nc"
    grid2 = "examples/data/mpas_uniform_10242_dual_counterclockwise.nc"

    # Create a uniform global latlon grid from a SCRIP formatted file
    grid = ESMF.Grid(filename=grid1, filetype=ESMF.FileFormat.SCRIP)
    # NOTE: corners are needed for conservative regridding
    # grid = ESMF.Grid(filename=grid1, filetype=ESMF.FileFormat.SCRIP,
    #                  add_corner_stagger=True)

    # create a field on the center stagger locations of the source grid
    srcfield = ESMF.Field(grid, name='srcfield',
                          staggerloc=ESMF.StaggerLoc.CENTER)

    # create an ESMF formatted unstructured mesh with clockwise cells removed
    mesh = ESMF.Mesh(filename=grid2, filetype=ESMF.FileFormat.ESMFMESH)

    # create a field on the nodes of the destination mesh
    dstfield = ESMF.Field(mesh, name='dstfield', meshloc=ESMF.MeshLoc.NODE)
    xctfield = ESMF.Field(mesh, name='xctfield', meshloc=ESMF.MeshLoc.NODE)
    # NOTE: Field must be built on elements of Mesh for conservative regridding
    # dstfield = ESMF.Field(mesh, name='dstfield', meshloc=ESMF.MeshLoc.ELEMENT)
    # xctfield = ESMF.Field(mesh, name='xctfield', meshloc=ESMF.MeshLoc.ELEMENT)

    # initialize the fields
    [lon, lat] = [0, 1]
    deg2rad = 3.14159 / 180

    gridXCoord = srcfield.grid.get_coords(lon, ESMF.StaggerLoc.CENTER)
    gridYCoord = srcfield.grid.get_coords(lat, ESMF.StaggerLoc.CENTER)
    srcfield.data[...] = 10.0 + (gridXCoord * deg2rad) ** 2 + (
                                                              gridYCoord * deg2rad) ** 2

    gridXCoord = xctfield.grid.get_coords(lon, ESMF.StaggerLoc.CENTER)
    gridYCoord = xctfield.grid.get_coords(lat, ESMF.StaggerLoc.CENTER)
    xctfield.data[...] = 10.0 + (gridXCoord * deg2rad) ** 2 + (
                                                              gridYCoord * deg2rad) ** 2

    dstfield.data[...] = 1e20

    # create an object to regrid data from the source to the destination field
    regrid = ESMF.Regrid(srcfield, dstfield,
                         regrid_method=ESMF.RegridMethod.BILINEAR,
                         unmapped_action=ESMF.UnmappedAction.ERROR)

    # do the regridding from source to destination field
    dstfield = regrid(srcfield, dstfield)

    return dstfield, xctfield

def compute_error(dstfield, xctfield):
    # compute the mean relative error
    from operator import mul
    num_nodes = reduce(mul, xctfield.shape)
    relerr = 0
    meanrelerr = 0
    if num_nodes != 0:
        ind = numpy.where((dstfield != 1e20) & (xctfield != 0))[0]
        relerr = numpy.sum(
            numpy.abs(dstfield[ind] - xctfield[ind]) / numpy.abs(
                xctfield[ind]))
        meanrelerr = relerr / num_nodes

    meanrelerr = relerr / num_nodes
    print "ESMPy regridding as a spawned MPI process:"
    print "  interpolation mean relative error = {0}".format(meanrelerr)


########################################### MAIN #############################

start_worker = 'worker'
usage = 'Program should be started without arguments'
pet_count = 4

# Parent
if len(sys.argv) == 1:

    # Spawn workers
    comm = MPI.COMM_WORLD.Spawn(
        sys.executable,
        args=[sys.argv[0], start_worker],
        maxprocs=pet_count)

    # gather output fields from workers
    dstfield = None
    dstfield = comm.gather(dstfield, root=MPI.ROOT)
    dstfield = numpy.concatenate([dstfield[i] for i in range(pet_count)])

    xctfield = None
    xctfield = comm.gather(xctfield, root=MPI.ROOT)
    xctfield = numpy.concatenate([xctfield[i] for i in range(pet_count)])

    # plot results
    compute_error(dstfield, xctfield)

    # Shutdown
    comm.Disconnect()

# Worker
elif sys.argv[1] == start_worker:

    # Connect to parent
    try:
        comm = MPI.Comm.Get_parent()
        rank = comm.Get_rank()
    except:
        raise ValueError('Could not connect to parent - ' + usage)

    try:
        # call ESMPy regridding
        dstfield, xctfield = regrid()

        # send output to parent
        comm.gather(sendobj=dstfield.data, root=0)
        comm.gather(sendobj=xctfield.data, root=0)
    except:
        comm.Disconnect()

    # Shutdown
    comm.Disconnect()

# Catch
else:
    raise ValueError(usage)
