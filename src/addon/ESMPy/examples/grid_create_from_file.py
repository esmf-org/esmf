# This example demonstrates how to create an ESMPy grid from file
# The grid file is required, it can be retrieved from the ESMF data repository:
#   wget http://www.earthsystemmodeling.org/download/data/ll2.5deg_grid.nc

import ESMF

# Start up ESMF, this call is only necessary to override the default parameters
# for logkind (ESMF.LogKind.NONE) and debug (False)
esmpy = ESMF.Manager(logkind=ESMF.LogKind.MULTI, debug=True)

# Create a uniform global latlon grid from a SCRIP formatted file
grid = ESMF.Grid(filename="examples/data/ll2.5deg_grid.nc", filetype=ESMF.FileFormat.SCRIP)

# Create a field on the centers of the grid
field = ESMF.Field(grid, "field", staggerloc=ESMF.StaggerLoc.CENTER)

if ESMF.local_pet() == 0:
    print "Successfully read a grid and created a field!"