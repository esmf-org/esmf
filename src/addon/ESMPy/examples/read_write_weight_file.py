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

import ESMF.util.helpers as helpers
import ESMF.api.constants as constants

# This call enables debug logging
mg = ESMF.Manager(debug=True)

# ESMPy uses Fortran style dimension ordering (as of November 2017)
[lat,lon] = [1,0]

# Create the source grid from memory with periodic dimension specified.
lons = numpy.arange(5, 350.1, 10)
lats  = numpy.arange(-85, 85.1, 10)
srcgrid = ESMF.Grid(numpy.array([lons.size, lats.size]),
                    coord_sys=ESMF.CoordSys.SPH_DEG,
                    staggerloc=ESMF.StaggerLoc.CENTER,
                    num_peri_dims=1, periodic_dim=0, pole_dim=1)

# Get and set the source grid coordinates.
srcGridCoordLon = srcgrid.get_coords(lon)
srcGridCoordLat = srcgrid.get_coords(lat)

slons_par = lons[srcgrid.lower_bounds[ESMF.StaggerLoc.CENTER][0]:srcgrid.upper_bounds[ESMF.StaggerLoc.CENTER][0]]
slats_par = lats[srcgrid.lower_bounds[ESMF.StaggerLoc.CENTER][1]:srcgrid.upper_bounds[ESMF.StaggerLoc.CENTER][1]]

# make sure to use indexing='ij' as ESMPy backend uses matrix indexing (not Cartesian)
lonm, latm = numpy.meshgrid(slons_par, slats_par, indexing='ij')

srcGridCoordLon[:] = lonm
srcGridCoordLat[:] = latm

# Create the dest grid from memory with periodic dimension specified.
lons = numpy.arange(2.5, 357.6, 5)
lats = numpy.arange(-87.5, 87.6, 5)
dstgrid = ESMF.Grid(numpy.array([lons.size, lats.size]),
                    coord_sys=ESMF.CoordSys.SPH_DEG,
                    staggerloc=ESMF.StaggerLoc.CENTER,
                    num_peri_dims=1, periodic_dim=1, pole_dim=0)

# Get and set the source grid coordinates.
dstGridCoordLat = dstgrid.get_coords(lat)
dstGridCoordLon = dstgrid.get_coords(lon)

dlons_par = lons[dstgrid.lower_bounds[ESMF.StaggerLoc.CENTER][0]:dstgrid.upper_bounds[ESMF.StaggerLoc.CENTER][0]]
dlats_par = lats[dstgrid.lower_bounds[ESMF.StaggerLoc.CENTER][1]:dstgrid.upper_bounds[ESMF.StaggerLoc.CENTER][1]]

# make sure to use indexing='ij' as ESMPy backend uses matrix indexing (not Cartesian)
lonm, latm = numpy.meshgrid(dlons_par, dlats_par, indexing='ij')

dstGridCoordLon[:] = lonm
dstGridCoordLat[:] = latm

# Create a field on the centers of the source grid with the mask applied.
srcfield = ESMF.Field(srcgrid, name="srcfield", staggerloc=ESMF.StaggerLoc.CENTER)

# Create a field on the centers of the source grid with the mask applied.
dstfield = ESMF.Field(dstgrid, name="dstfield", staggerloc=ESMF.StaggerLoc.CENTER)
xctfield = ESMF.Field(dstgrid, name="xctfield", staggerloc=ESMF.StaggerLoc.CENTER)

gridLon = srcfield.grid.get_coords(lon, ESMF.StaggerLoc.CENTER)
gridLat = srcfield.grid.get_coords(lat, ESMF.StaggerLoc.CENTER)

# wave = lambda x,k:  numpy.sin(x*k*numpy.pi/180.0)
# srcfield.data[...] = numpy.outer(wave(slons_par,3), wave(slats_par,3)) + 2

srcfield.data[:,:] = 2.0 + numpy.cos(numpy.radians(srcGridCoordLat)[...])**2 * \
                           numpy.cos(2.0*numpy.radians(srcGridCoordLon)[...])

# wave = lambda x,k:  numpy.sin(x*k*numpy.pi/180.0)
# xctfield.data[...] = numpy.outer(wave(dlons_par,3), wave(dlats_par,3)) + 2

xctfield.data[:,:] = 2.0 + numpy.cos(numpy.radians(dstGridCoordLat)[...])**2 * \
                           numpy.cos(2.0*numpy.radians(dstGridCoordLon)[...])

dstfield.data[:] = 1e20

# write regridding weights to file
filename = "esmpy_example_weight_file.nc"
if ESMF.local_pet() == 0:
    import os
    if os.path.isfile(
        os.path.join(os.getcwd(), filename)):
        os.remove(os.path.join(os.getcwd(), filename))

mg.barrier()
regrid = ESMF.Regrid(srcfield, dstfield, filename=filename,
                     regrid_method=ESMF.RegridMethod.BILINEAR,
                     unmapped_action=ESMF.UnmappedAction.IGNORE)


# # create a regrid object from file
regrid = ESMF.RegridFromFile(srcfield, dstfield, filename)

# calculate the regridding from source to destination field
dstfield = regrid(srcfield, dstfield)

# compute the mean relative error
num_nodes = numpy.prod(xctfield.data.shape[:])
relerr = 0
meanrelerr = 0
if num_nodes != 0:
    relerr = numpy.sum(numpy.abs(dstfield.data - xctfield.data) /
                       numpy.abs(xctfield.data))
    meanrelerr = relerr / num_nodes

# handle the parallel case
if ESMF.pet_count() > 1:
    relerr = helpers.reduce_val(relerr, op=constants.Reduce.SUM)
    num_nodes = helpers.reduce_val(num_nodes, op=constants.Reduce.SUM)

# output the results from one processor only
if ESMF.local_pet() == 0:
    meanrelerr = relerr / num_nodes
    print ("ESMPy Grid Mesh Regridding Example")
    print ("  interpolation mean relative error = {0}".format(meanrelerr))

    if os.path.isfile(os.path.join(os.getcwd(), filename)):
        os.remove(os.path.join(os.getcwd(), filename))

# set to 1 to output results
# if ESMF.pet_count() == 0:
#     import matplotlib.pyplot as plt
#     fig = plt.figure(1, (15, 6))
#     fig.suptitle('ESMPy Periodic Grids', fontsize=14, fontweight='bold')
# 
#     ax = fig.add_subplot(1, 2, 1)
#     im = ax.imshow(srcfield.data, vmin=1, vmax=3, cmap='gist_ncar', aspect='auto',
#                    extent=[min(slons_par), max(slons_par), min(slats_par), max(slats_par)])
#     ax.set_xbound(lower=min(slons_par), upper=max(slons_par))
#     ax.set_ybound(lower=min(slats_par), upper=max(slats_par))
#     ax.set_xlabel("Longitude")
#     ax.set_ylabel("Latitude")
#     ax.set_title("Source Data")
# 
#     ax = fig.add_subplot(1, 2, 2)
#     im = ax.imshow(dstfield.data, vmin=1, vmax=3, cmap='gist_ncar', aspect='auto',
#                    extent=[min(dlons_par), max(dlons_par), min(dlats_par), max(dlats_par)])
#     ax.set_xlabel("Longitude")
#     ax.set_ylabel("Latitude")
#     ax.set_title("Regrid Solution")
# 
#     fig.subplots_adjust(right=0.8)
#     cbar_ax = fig.add_axes([0.9, 0.1, 0.01, 0.8])
#     fig.colorbar(im, cax=cbar_ax)
# 
#     plt.show()