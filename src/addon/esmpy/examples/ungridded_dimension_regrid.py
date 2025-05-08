# This example demonstrates how to regrid a field with extra dimensions,
# such as time and vertical layers. This


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
# cache_data_file(os.path.join(DATA_DIR, "ll2.5deg_grid.nc"))
# cache_data_file(os.path.join(DATA_DIR, "T42_grid.nc"))

# This call enables debug logging
# esmpy = esmpy.Manager(debug=True)

grid1 = os.path.join(DATA_DIR, "ll2.5deg_grid.nc")
if not os.path.exists(grid1):
    raise DataMissing("Data not available, try 'make download'.")
grid2 = os.path.join(DATA_DIR, "T42_grid.nc")
if not os.path.exists(grid2):
    raise DataMissing("Data not available, try 'make download'.")

# the number of elements in the extra field dimensions
levels = 2
time = 5

# Create a uniform global latlon grid from a SCRIP formatted file
srcgrid = esmpy.Grid(filename=grid1, filetype=esmpy.FileFormat.SCRIP,
                    add_corner_stagger=True)

# Create a uniform global latlon grid from a SCRIP formatted file
dstgrid = esmpy.Grid(filename=grid2, filetype=esmpy.FileFormat.SCRIP,
                    add_corner_stagger=True)

# Create a field on the center stagger locations of the source grid
srcfield = esmpy.Field(srcgrid, name='srcfield',
                      staggerloc=esmpy.StaggerLoc.CENTER,
                      ndbounds=[levels, time])

# Create a field on the center stagger locations of the destination grid
dstfield = esmpy.Field(dstgrid, name='dstfield',
                      staggerloc=esmpy.StaggerLoc.CENTER,
                      ndbounds=[levels, time])

# Create a field on the center stagger locations of the destination grid
xctfield = esmpy.Field(dstgrid, name='xctfield',
                      staggerloc=esmpy.StaggerLoc.CENTER,
                      ndbounds=[levels, time])

# Create fields needed to analyze accuracy of conservative regridding
srcfracfield = esmpy.Field(srcgrid, name='srcfracfield')
dstfracfield = esmpy.Field(dstgrid, name='dstfracfield')
srcareafield = esmpy.Field(srcgrid, name='srcareafield')
dstareafield = esmpy.Field(dstgrid, name='dstareafield')


# Get the coordinate pointers and initialize the source field
[lon,lat] = [0, 1]
gridXCoord = srcfield.grid.get_coords(lon, esmpy.StaggerLoc.CENTER)
gridYCoord = srcfield.grid.get_coords(lat, esmpy.StaggerLoc.CENTER)

deg2rad = 3.14159/180

for level in range(levels):
    for timestep in range(time):
        srcfield.data[:,:, level,timestep]=10.0*(level+timestep+1) + \
                                          (gridXCoord*deg2rad)**2 + \
                                          (gridXCoord*deg2rad)*\
                                          (gridYCoord*deg2rad) + \
                                          (gridYCoord*deg2rad)**2

# Get the coordinate pointers and initialize the exact solution
gridXCoord = xctfield.grid.get_coords(lon, esmpy.StaggerLoc.CENTER)
gridYCoord = xctfield.grid.get_coords(lat, esmpy.StaggerLoc.CENTER)

for level in range(levels):
    for timestep in range(time):
        xctfield.data[:,:,level,timestep]=10.0*(level+timestep+1) + \
                                          (gridXCoord*deg2rad)**2 + \
                                          (gridXCoord*deg2rad)*\
                                          (gridYCoord*deg2rad) + \
                                          (gridYCoord*deg2rad)**2

dstfield.data[...] = 1e20

# Create an object to regrid data from the source to the destination field
regrid = esmpy.Regrid(srcfield, dstfield,
                     regrid_method=esmpy.RegridMethod.CONSERVE,
                     unmapped_action=esmpy.UnmappedAction.ERROR,
                     src_frac_field=srcfracfield,
                     dst_frac_field=dstfracfield)

# Do the regridding from source to destination field
dstfield = regrid(srcfield, dstfield)

# Calculate the area fields
srcareafield.get_area()
dstareafield.get_area()

# Compute the pointwise relative error and the mass on each field
srcmass = 0
dstmass = 0
relerr = 0
for level in range(levels):
    for timestep in range(time):
        srcmass += numpy.sum(numpy.abs(srcareafield.data*srcfracfield.data*
                                       srcfield.data[:, :, level, timestep]))
        dstmass += numpy.sum(numpy.abs(dstareafield.data*
                                       dstfield.data[:, :, level, timestep]))
        relerr += numpy.sum(numpy.abs(dstfield.data[:, :, level, timestep] /
                                      dstfracfield.data -
                                      xctfield.data[:, :, level, timestep]) /
                                      numpy.abs(xctfield.data[:, :, level, timestep]))

# Compute the mean relative interpolation and conservation error
from operator import mul
num_nodes = numpy.prod(xctfield.data.shape[:])
meanrelerr = 0
if num_nodes != 0:
    meanrelerr = relerr / num_nodes
csrverr = 0
if dstmass != 0:
    csrverr = numpy.abs(srcmass - dstmass) / dstmass

# Handle the parallel case
if esmpy.pet_count() > 1:
    relerr = helpers.reduce_val(relerr, op=constants.Reduce.SUM)
    num_nodes = helpers.reduce_val(num_nodes, op=constants.Reduce.SUM)
    srcmass = helpers.reduce_val(srcmass, op=constants.Reduce.SUM)
    dstmass = helpers.reduce_val(dstmass, op=constants.Reduce.SUM)

# Output the results from one processor only
if esmpy.local_pet() == 0:
    meanrelerr = relerr / num_nodes
    csrverr = numpy.abs(srcmass - dstmass) / dstmass

    print ("ESMPy Ungridded Field Dimensions Example")
    print ("  interpolation mean relative error = {0}".format(meanrelerr))
    print ("  mass conservation relative error  = {0}".format(csrverr))

# try:
#     import matplotlib.pyplot as plt
#     from matplotlib import animation
#
#     lons = dstfield.grid.get_coords(0)
#     lats = dstfield.grid.get_coords(1)
#
#     fig = plt.figure()
#     ax = plt.axes(xlim=(numpy.min(lons), numpy.max(lons)),
#               ylim=(numpy.min(lats), numpy.max(lats)))
#     ax.set_xlabel("Longitude")
#     ax.set_ylabel("Latitude")
#     ax.set_title("Regrid Solution")
#
#     def animate(i):
#         z = dstfield.data[0,i,:,:]
#         cont = plt.contourf(lons, lats, z)
#         return cont
#
#     anim = animation.FuncAnimation(fig, animate, frames=time)
#
#     anim.save('ESMPyRegrid.mp4')
#
#     plt.show()
# except:
#     raise ImportError("matplotlib is not available on this machine")
