# This example demonstrates how to regrid between a Grid and a Mesh.
# The data files can be retrieved from the ESMF data repository by uncommenting the
# following block of code:
#
# import os
# DD = os.path.join(os.getcwd(), "examples/data")
# if not os.path.isdir(DD):
#     os.makedirs(DD)
# from ESMF.util.cache_data import cache_data_file
# cache_data_file(os.path.join(DD, "ll2.5deg_grid.nc"))
# cache_data_file(os.path.join(DD, "mpas_uniform_10242_dual_counterclockwise.nc"))

import ESMF
import numpy

# This call enables debug logging
# esmpy = ESMF.Manager(debug=True)

# # Create the source grid from memory with periodic dimension specified.
# lats  = numpy.arange(  0, 360, 360./2.)
# lons = numpy.arange(-90., 90.1, 180./2.)
# max_index = numpy.array([lons.size, lats.size])
# srcgrid = ESMF.Grid(max_index, coord_sys=ESMF.CoordSys.SPH_DEG, coord_typekind=ESMF.TypeKind.R4,
#                     num_peri_dims=1, periodic_dim=1, pole_dim=0)

# # Add coordinates to the source grid.
# srcgrid.add_coords(staggerloc=[ESMF.StaggerLoc.CENTER])
#
# # Get and set the source grid coordinates.
# gridCoordLat = srcgrid.get_coords(lat)
# gridCoordLon = srcgrid.get_coords(lon)
#
# lons_par = lons[srcgrid.lower_bounds[ESMF.StaggerLoc.CENTER][0]:srcgrid.upper_bounds[ESMF.StaggerLoc.CENTER][0]]
# lats_par = lats[srcgrid.lower_bounds[ESMF.StaggerLoc.CENTER][1]:srcgrid.upper_bounds[ESMF.StaggerLoc.CENTER][1]]
#
# gridCoordLat[...] = lats_par.reshape(1, lats_par.size)
# gridCoordLon[...] = lons_par.reshape(lons_par.size, 1)

pi = 3.14159

row = [-pi, -pi/2, pi/2, pi]; col = [0, pi/2, 3*pi/2, 2*pi]
rowm, colm = numpy.meshgrid(row, col)

srcgrid = ESMF.Grid(numpy.array([len(col), len(row)]), num_peri_dims=0, coord_sys=ESMF.CoordSys.CART,
                    coord_typekind=ESMF.TypeKind.R4, staggerloc=[ESMF.StaggerLoc.CENTER])

grid_row = srcgrid.get_coords(0, staggerloc=ESMF.StaggerLoc.CENTER)
grid_col = srcgrid.get_coords(1, staggerloc=ESMF.StaggerLoc.CENTER)

grid_row[:] = rowm; grid_col[:] = colm

# # Create the dest grid from memory with periodic dimension specified.
# # switch lats and lons to correspond to swapping the periodic and pole dimensions (below)
# lats  = numpy.arange(  0, 360, 360./2.)
# lons = numpy.arange(-90., 90.1, 180./2.)
# max_index = numpy.array([lons.size, lats.size])
# dstgrid = ESMF.Grid(max_index, coord_sys=ESMF.CoordSys.SPH_DEG, coord_typekind=ESMF.TypeKind.R4,
#                     num_peri_dims=1, periodic_dim=1, pole_dim=0)
#
# # Add coordinates to the source grid.
# dstgrid.add_coords(staggerloc=[ESMF.StaggerLoc.CENTER])
#
# # Get and set the source grid coordinates.
# gridCoordLat = dstgrid.get_coords(lat)
# gridCoordLon = dstgrid.get_coords(lon)
#
# lons_par = lons[dstgrid.lower_bounds[ESMF.StaggerLoc.CENTER][0]:dstgrid.upper_bounds[ESMF.StaggerLoc.CENTER][0]]
# lats_par = lats[dstgrid.lower_bounds[ESMF.StaggerLoc.CENTER][1]:dstgrid.upper_bounds[ESMF.StaggerLoc.CENTER][1]]
#
# gridCoordLat[...] = lats_par.reshape(1, lats_par.size)
# gridCoordLon[...] = lons_par.reshape(lons_par.size, 1)

row = [-pi, 0, pi]; col = [0, pi, 2*pi]
rowm, colm = numpy.meshgrid(row, col)

dstgrid = ESMF.Grid(numpy.array([len(col), len(row)]), num_peri_dims=0, coord_sys=ESMF.CoordSys.CART,
                    coord_typekind=ESMF.TypeKind.R4, staggerloc=[ESMF.StaggerLoc.CENTER])

grid_row = dstgrid.get_coords(0, staggerloc=ESMF.StaggerLoc.CENTER)
grid_col = dstgrid.get_coords(1, staggerloc=ESMF.StaggerLoc.CENTER)

grid_row[:] = rowm; grid_col[:] = colm



# Create a field on the centers of the source grid with the mask applied.
srcfield = ESMF.Field(srcgrid, name="srcfield", staggerloc=ESMF.StaggerLoc.CENTER)

srcfield.data[:] = 0

# Create a field on the centers of the source grid with the mask applied.
dstfield = ESMF.Field(dstgrid, name="dstfield", staggerloc=ESMF.StaggerLoc.CENTER)
xctfield = ESMF.Field(dstgrid, name="xctfield", staggerloc=ESMF.StaggerLoc.CENTER)

[lat,lon] = [1,0]

gridLon = srcfield.grid.get_coords(lon, ESMF.StaggerLoc.CENTER)
gridLat = srcfield.grid.get_coords(lat, ESMF.StaggerLoc.CENTER)
# srcfield.data[:] = 2.0 + numpy.cos(numpy.radians(gridLat[...]))**2 * \
#                            numpy.cos(2.0*numpy.radians(gridLon[...]))
srcfield.data[:] = numpy.arange(16).reshape(4,4)+10
srcfield.data[:] = 1


gridLon = xctfield.grid.get_coords(lon, ESMF.StaggerLoc.CENTER)
gridLat = xctfield.grid.get_coords(lat, ESMF.StaggerLoc.CENTER)
# xctfield.data[:] = 2.0 + numpy.cos(numpy.radians(gridLat[...]))**2 * \
#                            numpy.cos(2.0*numpy.radians(gridLon[...]))
xctfield.data[:] = 42.0

dstfield.data[:] = 1e20

# write regridding weights to file
import os

if os.path.isfile(
        os.path.join(os.getcwd(), "esmpy_example_weight_file.nc")):
    os.remove(os.path.join(os.getcwd(), "esmpy_example_weight_file.nc"))

regrid = ESMF.Regrid(srcfield, dstfield, filename="esmpy_example_weight_file.nc",
        regrid_method=ESMF.RegridMethod.BILINEAR,
        unmapped_action=ESMF.UnmappedAction.IGNORE)
print srcfield.data

# create a regrid object from file
regrid = ESMF.RegridFromFile(srcfield, dstfield, "esmpy_example_weight_file.nc")


# calculate the regridding from source to destination field
srcfield.data[:] = 42.
dstfield = regrid(srcfield, dstfield)

# compute the mean relative error
from operator import mul
num_nodes = numpy.prod(xctfield.data.shape[:])
relerr = 0
meanrelerr = 0
if num_nodes is not 0:
    relerr = numpy.sum(numpy.abs(dstfield.data - xctfield.data) / numpy.abs(xctfield.data))
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
    print ("ESMPy Grid Mesh Regridding Example")
    print ("  interpolation mean relative error = {0}".format(meanrelerr))
