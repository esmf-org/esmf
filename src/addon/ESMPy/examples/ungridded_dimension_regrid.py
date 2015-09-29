# This example demonstrates how to regrid a field with extra dimensions,
# such as time and vertical layers.
# The data files can be retrieved from the ESMF data repository by uncommenting the
# following block of code:
#
# import os
# DD = os.path.join(os.getcwd(), "examples/data")
# if not os.path.isdir(DD):
#     os.makedirs(DD)
# from ESMF.util.cache_data import cache_data_file
# cache_data_file(os.path.join(DD, "ll2.5deg_grid.nc"))
# cache_data_file(os.path.join(DD, "T42_grid.nc"))

import ESMF
import numpy

# This call enables debug logging
# esmpy = ESMF.Manager(debug=True)

grid1 = "examples/data/ll2.5deg_grid.nc"
grid2 = "examples/data/T42_grid.nc"

# the number of elements in the extra field dimensions
levels = 2
time = 5

# Create a uniform global latlon grid from a SCRIP formatted file
srcgrid = ESMF.Grid(filename=grid1, filetype=ESMF.FileFormat.SCRIP,
                 add_corner_stagger=True)

# Create a uniform global latlon grid from a SCRIP formatted file
dstgrid = ESMF.Grid(filename=grid2, filetype=ESMF.FileFormat.SCRIP,
                 add_corner_stagger=True)

# create a field on the center stagger locations of the source grid
srcfield = ESMF.Field(srcgrid, name='srcfield',
                      staggerloc=ESMF.StaggerLoc.CENTER,
                      ndbounds=[levels, time])

# create a field on the center stagger locations of the destination grid
dstfield = ESMF.Field(dstgrid, name='dstfield',
                      staggerloc=ESMF.StaggerLoc.CENTER,
                      ndbounds=[levels, time])

# create a field on the center stagger locations of the destination grid
xctfield = ESMF.Field(dstgrid, name='xctfield',
                      staggerloc=ESMF.StaggerLoc.CENTER,
                      ndbounds=[levels, time])

# create fields needed to analyze accuracy of conservative regridding
srcfracfield = ESMF.Field(srcgrid, name='srcfracfield')
dstfracfield = ESMF.Field(dstgrid, name='dstfracfield')
srcareafield = ESMF.Field(srcgrid, name='srcareafield')
dstareafield = ESMF.Field(dstgrid, name='dstareafield')


# get the coordinate pointers and set the coordinates
[lon,lat] = [0, 1]
gridXCoord = srcfield.grid.get_coords(lon, ESMF.StaggerLoc.CENTER)
gridYCoord = srcfield.grid.get_coords(lat, ESMF.StaggerLoc.CENTER)

deg2rad = 3.14159/180

for timestep in range(time):
    for level in range(levels):
        srcfield.data[level,timestep,:,:]=10.0*(level+timestep+1) + (gridXCoord*deg2rad)**2 + \
                                 (gridXCoord*deg2rad)*(gridYCoord*deg2rad) + (gridYCoord*deg2rad)**2

# get the coordinate pointers and set the coordinates
gridXCoord = xctfield.grid.get_coords(lon, ESMF.StaggerLoc.CENTER)
gridYCoord = xctfield.grid.get_coords(lat, ESMF.StaggerLoc.CENTER)

for timestep in range(time):
    for level in range(levels):
        xctfield.data[level,timestep,:,:]=10.0*(level+timestep+1) + (gridXCoord*deg2rad)**2 + \
                                 (gridXCoord*deg2rad)*(gridYCoord*deg2rad) + (gridYCoord*deg2rad)**2

dstfield.data[...] = 1e20

# create an object to regrid data from the source to the destination field
regrid = ESMF.Regrid(srcfield, dstfield,
                     regrid_method=ESMF.RegridMethod.CONSERVE,
                     unmapped_action=ESMF.UnmappedAction.ERROR,
                     src_frac_field=srcfracfield,
                     dst_frac_field=dstfracfield)

# do the regridding from source to destination field
dstfield = regrid(srcfield, dstfield)

# ask ESMF to calculate the area fields
srcareafield.get_area()
dstareafield.get_area()

# compute the pointwise relative error and the mass on each field
srcmass = 0
dstmass = 0
relerr = 0
for timestep in range(time):
    for level in range(levels):
        srcmass += numpy.sum(numpy.abs(srcareafield.data*srcfracfield.data*srcfield.data[level, timestep, :, :]))
        dstmass += numpy.sum(numpy.abs(dstareafield.data*dstfield.data[level, timestep, :, :]))
        relerr += numpy.sum(numpy.abs(dstfield.data[level, timestep, :, :] /
                                      dstfracfield.data - xctfield.data[level, timestep, :, :]) /
                            numpy.abs(xctfield.data[level, timestep, :, :]))

# compute the mean relative interpolation and conservation error
from operator import mul
num_nodes = reduce(mul, xctfield.data.shape)
meanrelerr = 0
if num_nodes is not 0:
    meanrelerr = relerr / num_nodes
csrverr = 0
if dstmass is not 0:
    csrverr = numpy.abs(srcmass - dstmass) / dstmass

# handle the parallel case
if ESMF.pet_count() > 1:
    try:
        from mpi4py import MPI
    except:
        raise ImportError
    comm = MPI.COMM_WORLD
    relerr = comm.reduce(relerr, op=MPI.SUM)
    num_nodes = comm.reduce(num_nodes, op=MPI.SUM)
    srcmass = comm.reduce(srcmass, op=MPI.SUM)
    dstmass = comm.reduce(dstmass, op=MPI.SUM)

# output the results from one processor only
if ESMF.local_pet() is 0:
    meanrelerr = relerr / num_nodes
    csrverr = numpy.abs(srcmass - dstmass) / dstmass

    print "ESMPy Ungridded Field Dimensions Example"
    print "  interpolation mean relative error = {0}".format(meanrelerr)
    print "  mass conservation relative error  = {0}".format(csrverr)

    assert (meanrelerr < 8e-4)
