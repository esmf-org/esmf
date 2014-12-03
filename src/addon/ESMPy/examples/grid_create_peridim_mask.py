# This example demonstrates how to create an ESMPy grid in memory with
# periodic dimension and mask.
import ESMF
import numpy as np

# Start up ESMF, this call is only necessary to override the default parameters
# for logkind (ESMF.LogKind.NONE) and debug (False)
esmpy = ESMF.Manager(logkind=ESMF.LogKind.MULTI, debug=True)

# Create the grid from memory with periodic dimension specified.
max_index = np.array([12, 20])
grid = ESMF.Grid(max_index, num_peri_dims=1, periodic_dim=1,
                 coord_sys=ESMF.CoordSys.SPH_DEG, 
                 coord_typekind=ESMF.TypeKind.R4)

# Add a mask.
mask = grid.add_item(ESMF.GridItem.MASK)

# Set the mask values.
[x, y] = [0, 1]
for i in xrange(mask.shape[x]):
    for j in xrange(mask.shape[y]):
        if (i == 2.0):
            mask[i, j] = 1
        else:
            mask[i, j] = 0
            
# Create a field on the centers of the grid with the mask applied.
field = ESMF.Field(grid, "field", staggerloc=ESMF.StaggerLoc.CENTER,
                   mask_values=mask)

if ESMF.local_pet() == 0:
    print "Successfully created a grid with periodic dimension and mask and created a field on it!"
