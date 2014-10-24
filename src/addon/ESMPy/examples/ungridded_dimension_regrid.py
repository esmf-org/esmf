# This example demonstrates how to regrid a field with multiple layers
# The grid and mesh files are required, they can be retrieved from the ESMF data repository:
#   wget http://www.earthsystemmodeling.org/download/data/ll2.5deg_grid.nc
#   wget http://www.earthsystemmodeling.org/download/data/T42_grid.nc

import ESMF
import numpy as numpy

ESMF.Manager(logkind=ESMF.LogKind.MULTI, debug=True)

grid1 = "ll2.5deg_grid.nc"
grid2 = "T42_grid.nc"

# extra is the number of vertical levels
levels = 1
time = 5

# Create a uniform global latlon grid from a SCRIP formatted file
srcgrid = ESMF.Grid(filename=grid1, filetype=ESMF.FileFormat.SCRIP,
                 add_corner_stagger=True)

# Create a uniform global latlon grid from a SCRIP formatted file
dstgrid = ESMF.Grid(filename=grid2, filetype=ESMF.FileFormat.SCRIP,
                 add_corner_stagger=True)

# create a field on the center stagger locations of the source grid
srcfield = ESMF.Field(srcgrid, 'srcfield',
                      staggerloc=ESMF.StaggerLoc.CENTER,
                      ndbounds=[levels, time])

# create a field on the center stagger locations of the source grid
dstfield = ESMF.Field(dstgrid, 'dstfield',
                      staggerloc=ESMF.StaggerLoc.CENTER,
                      ndbounds=[levels, time])

# create a field on the center stagger locations of the source grid
xctfield = ESMF.Field(dstgrid, 'xctfield',
                      staggerloc=ESMF.StaggerLoc.CENTER,
                      ndbounds=[levels, time])

srcfracfield = ESMF.Field(srcgrid, 'srcfracfield')
dstfracfield = ESMF.Field(dstgrid, 'dstfracfield')
srcareafield = ESMF.Field(srcgrid, 'srcareafield')
dstareafield = ESMF.Field(dstgrid, 'dstareafield')


# get the coordinate pointers and set the coordinates
[lon,lat] = [0, 1]
gridXCoord = srcfield.grid.get_coords(0, ESMF.StaggerLoc.CENTER)
gridYCoord = srcfield.grid.get_coords(1, ESMF.StaggerLoc.CENTER)

deg2rad = 3.14159/180

for timestep in range(time):
    for level in range(levels):
        srcfield.data[:,:,level,timestep]=10.0*(level+timestep+1) + (gridXCoord*deg2rad)**2 + \
                                 (gridXCoord*deg2rad)*(gridYCoord*deg2rad) + (gridYCoord*deg2rad)**2

# get the coordinate pointers and set the coordinates
[lon,lat] = [0, 1]
gridXCoord = xctfield.grid.get_coords(0, ESMF.StaggerLoc.CENTER)
gridYCoord = xctfield.grid.get_coords(1, ESMF.StaggerLoc.CENTER)

for timestep in range(time):
    for level in range(levels):
        xctfield.data[:,:,level,timestep]=10.0*(level+timestep+1) + (gridXCoord*deg2rad)**2 + \
                                 (gridXCoord*deg2rad)*(gridYCoord*deg2rad) + (gridYCoord*deg2rad)**2

dstfield[...] = 1e20

# create an object to regrid data from the source to the destination field
regrid = ESMF.Regrid(srcfield, dstfield, \
                     regrid_method=ESMF.RegridMethod.CONSERVE, \
                     unmapped_action=ESMF.UnmappedAction.IGNORE,
                     src_frac_field=srcfracfield,
                     dst_frac_field=dstfracfield)

# do the regridding from source to destination field
dstfield = regrid(srcfield, dstfield)

# ask ESMF to calculate the area fields
# TODO: have this be done automatically?
srcareafield.get_area()
dstareafield.get_area()

# check results
srcmass = 0
dstmass = 0
for timestep in range(time):
    for level in range(levels):
        srcmass += numpy.sum(srcareafield.data*srcfracfield.data*srcfield[:,:,level,timestep].data)
        dstmass += numpy.sum(dstareafield.data*dstfield[:,:,level,timestep].data)

relerr = 0
for timestep in range(time):
    for level in range(levels):
        relerr += numpy.sum(numpy.abs(dstfield[:,:,level, timestep].data / \
                                      dstfracfield.data - xctfield[:,:,level,timestep].data) / \
                                      numpy.abs(xctfield[:,:,level,timestep].data))

from operator import mul
relerr = relerr / reduce(mul, xctfield.shape)

print "interpolation mean relative error"
print relerr

print "mass conservation relative error"
print numpy.abs(srcmass - dstmass)/numpy.abs(dstmass)