# This example demonstrates how to regrid between a grid and a mesh.
# The grid and mesh files are required, they can be retrieved from the ESMF data repository:
#   wget http://www.earthsystemmodeling.org/download/data/GRIDSPEC_ACCESS1.nc
#   wget http://www.earthsystemmodeling.org/download/data/tx0.1v2_070911.nc

import ESMF
import numpy as np

ESMF.Manager(logkind=ESMF.LogKind.MULTI, debug=True)

grid1 = "GRIDSPEC_ACCESS1.nc"
grid2 = "tx0.1v2_070911.nc"

# Create a uniform global latlon grid from a SCRIP formatted file
grid = ESMF.Grid(filename=grid1, filetype=ESMF.FileFormat.GRIDSPEC,
                 add_corner_stagger=True)

# create a field on the center stagger locations of the source grid
srcfield = ESMF.Field(grid, 'srcfield', staggerloc=ESMF.StaggerLoc.CENTER)

# initialize the field to a constant value
srcfield[...] = 25

# create an ESMF formatted unstructured mesh with clockwise cells removed
tripole = ESMF.Grid(filename=grid2, filetype=ESMF.FileFormat.SCRIP,
                 add_corner_stagger=True, add_mask=True, varname="grid_imask")

# create a field on the elements of the destination mesh
dstfield = ESMF.Field(tripole, 'dstfield', meshloc=ESMF.StaggerLoc.CENTER)

dstfield[...] = 0

# create an object to regrid data from the source to the destination field
regrid = ESMF.Regrid(srcfield, dstfield, \
                     regrid_method=ESMF.RegridMethod.CONSERVE, \
                     unmapped_action=ESMF.UnmappedAction.IGNORE, \
                     dst_mask_values=np.array([0]))

# do the regridding from source to destination field
dstfield = regrid(srcfield, dstfield)

print "Successfully read a grid and a mesh and did a regridding of a constant field!"
print "The field values on PET (processor) # {0} are:".format(ESMF.local_pet())
print dstfield.data