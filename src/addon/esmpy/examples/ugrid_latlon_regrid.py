# This example demonstrates how to regrid between a UGRID Mesh and a Grid.

import esmpy
import numpy

import os

from esmpy.util.cache_data import DATA_DIR
from esmpy.util.exceptions import DataMissing

# The data files can be retrieved from the ESMF data repository by uncommenting the
# following block of code:
#

# from esmpy.util.cache_data import cache_data_file
# cache_data_file(os.path.join(DATA_DIR, "ll1deg_grid.nc"))
# cache_data_file(os.path.join(DATA_DIR, "aggregAtlanticESTOFS.nc"))


def plot_error(field1, field2, uninitval):

    try:
        import matplotlib
        import matplotlib.pyplot as plt
    except:
        raise ImportError("matplotlib is not available on this machine")

    colormap = "gist_stern"
    lons = field2.grid.get_coords(0)
    lats = field2.grid.get_coords(1)

    fig = plt.figure(1, (15, 6))
    fig.suptitle('ESMPy Regridding', fontsize=14, fontweight='bold')

    ax = fig.add_subplot(1, 2, 1)
    solution = numpy.copy(field2.data)
    solution[numpy.where(field2.data == uninitval)] = 0
    im = ax.imshow(solution, vmin=numpy.min(solution),
                   vmax=numpy.max(solution), cmap=colormap, aspect='auto',
                   extent=[numpy.min(lons), numpy.max(lons),
                           numpy.min(lats), numpy.max(lats)])
    ax.set_xlabel("Longitude")
    ax.set_ylabel("Latitude")
    ax.set_title("Regrid Solution")

    relerr = numpy.abs(field2.data - field1.data)/numpy.abs(field1.data)
    relerr[numpy.where(field2.data == uninitval)] = 0
    ax = fig.add_subplot(1, 2, 2)
    im1 = ax.imshow(relerr, vmin=numpy.min(relerr),
                    vmax=numpy.max(relerr), cmap=colormap, aspect='auto',
                    extent=[numpy.min(lons), numpy.max(lons),
                            numpy.min(lats), numpy.max(lats)])
    ax.set_xlabel("Longitude")
    ax.set_ylabel("Latitude")
    ax.set_title("Relative Error")

    fig.subplots_adjust(right=0.8)
    cbar_ax = fig.add_axes([0.9, 0.1, 0.01, 0.8])
    fig.colorbar(im1, cax=cbar_ax)

    plt.show()

def plot_field(field2, uninitval):

    try:
        import matplotlib
        import matplotlib.pyplot as plt
    except:
        raise ImportError("matplotlib is not available on this machine")

    colormap = "gist_stern"
    lons = field2.grid.get_coords(0)
    lats = field2.grid.get_coords(1)

    fig = plt.figure(1, (15, 6))
    fig.suptitle('ESMPy Regridding', fontsize=14, fontweight='bold')

    solution = numpy.copy(field2.data)
    solution[numpy.where(field2.data == uninitval)] = 0
    im = plt.imshow(solution, vmin=numpy.min(solution),
                   vmax=numpy.max(solution), cmap=colormap, aspect='auto',
                   extent=[numpy.min(lons), numpy.max(lons),
                           numpy.min(lats), numpy.max(lats)])
    ax = plt.gca()
    ax.set_xlabel("Longitude")
    ax.set_ylabel("Latitude")
    ax.set_title("Regrid Solution")

    fig.subplots_adjust(right=0.8)
    cbar_ax = fig.add_axes([0.9, 0.1, 0.01, 0.8])
    fig.colorbar(im, cax=cbar_ax)

    plt.show()

# This call enables debug logging
# esmpy = esmpy.Manager(debug=True)

# these are for a new test using opendap, dataset has periodic failures..
# meshfile = "http://coastalmodeldev.data.noaa.gov/thredds/dodsC/aggregAtlanticESTOFS"
meshfile = os.path.join(DATA_DIR, "aggregAtlanticESTOFS.nc")
if not os.path.exists(meshfile):
    raise DataMissing("Data not available, try 'make download'.")
mesh = esmpy.Mesh(filename=meshfile, filetype=esmpy.FileFormat.UGRID, meshname='adcirc_mesh')

# Create a global latlon grid from a SCRIP formatted file
gridfile = os.path.join(DATA_DIR, "ll1deg_grid.nc")
if not os.path.exists(gridfile):
    raise DataMissing("Data not available, try 'make download'.")
grid = esmpy.Grid(filename=gridfile, filetype=esmpy.FileFormat.SCRIP,
                 add_corner_stagger=True, is_sphere=True)

# create a field on the element locations of the source mesh
srcfield = esmpy.Field(mesh, name='srcfield', meshloc=esmpy.MeshLoc.NODE)

# create a field on the centers of the destination grid
dstfield = esmpy.Field(grid, name='dstfield', staggerloc=esmpy.StaggerLoc.CENTER)
xctfield = esmpy.Field(grid, name='xctfield', staggerloc=esmpy.StaggerLoc.CENTER)

# initialize the fields
[lon,lat] = [0, 1]

srcgridXCoord = srcfield.grid.get_coords(lon, meshloc=esmpy.MeshLoc.NODE)
srcgridYCoord = srcfield.grid.get_coords(lat, meshloc=esmpy.MeshLoc.NODE)
srcfield.data[...] = 2.0 + numpy.cos(numpy.radians(srcgridYCoord)[...])**2 * \
                           numpy.cos(2.0*numpy.radians(srcgridXCoord)[...])

dstgridXCoord = xctfield.grid.get_coords(lon, esmpy.StaggerLoc.CENTER)
dstgridYCoord = xctfield.grid.get_coords(lat, esmpy.StaggerLoc.CENTER)
xctfield.data[...] = 2.0 + numpy.cos(numpy.radians(dstgridYCoord)[...])**2 * \
                           numpy.cos(2.0*numpy.radians(dstgridXCoord)[...])

uninitval = 1e20
dstfield.data[...] = uninitval

# create an object to regrid data from the source to the destination field
regrid = esmpy.Regrid(srcfield, dstfield,
                     regrid_method=esmpy.RegridMethod.BILINEAR,
                     unmapped_action=esmpy.UnmappedAction.IGNORE)

# do the regridding from source to destination field
dstfield = regrid(srcfield, dstfield, zero_region=esmpy.Region.SELECT)

# output the results from one processor only
if esmpy.local_pet() == 0:
    print ("ESMPy UGRID to LatLon Regridding Example")
    # plot_error(xctfield, dstfield, uninitval=uninitval)

# # now read some real data and give it a whirl
# try:
#     import netCDF4 as nc
#     # f = nc.Dataset('http://coastalmodeldev.data.noaa.gov/thredds/dodsC/aggregAtlanticESTOFS')
#     f = nc.Dataset("examples/data/aggregAtlanticESTOFS.nc")
#     zeta = f.variables["zeta"]
#     srcfield.data[...] = zeta[0]
#     f.close()
# except:
#      raise ImportError("netCDF4 is not available on this machine")

# This call fails due to ESMF limitation that time be the file's unlimited dimension
# srcfield.data[...] = srcfield.read(meshfile, "zeta")

# dstfield.data[...] = uninitval
# # create an object to regrid data from the source to the destination field
# regrid = esmpy.Regrid(srcfield, dstfield,
#                      regrid_method=esmpy.RegridMethod.BILINEAR,
#                      unmapped_action=esmpy.UnmappedAction.IGNORE)
#
# # do the regridding from source to destination field
# dstfield = regrid(srcfield, dstfield, zero_region=esmpy.Region.SELECT)
#
# plot_field(dstfield, uninitval=uninitval)
