# This example demonstrates how to create an ESMPy grid in memory with
# periodic dimension and mask.
import ESMF
import numpy as np

# Start up ESMF, this call is only necessary to override the default parameters
# for logkind (ESMF.LogKind.NONE) and debug (False)
esmpy = ESMF.Manager(logkind=ESMF.LogKind.MULTI, debug=True)

# Create the source grid from memory with periodic dimension specified.
max_index = np.array([12, 20])
srcgrid = ESMF.Grid(max_index, num_peri_dims=1, periodic_dim=1,
                    coord_sys=ESMF.CoordSys.SPH_DEG, 
                    coord_typekind=ESMF.TypeKind.R4)

# Add a mask to the source grid
mask = srcgrid.add_item(ESMF.GridItem.MASK)

# Set the source grid mask values.
[x, y] = [0, 1]
for i in xrange(mask.shape[x]):
    for j in xrange(mask.shape[y]):
        if (i == 2.0):
            mask[i, j] = 1
        else:
            mask[i, j] = 0
            
# Add coordinates to the source grid.
srcgrid.add_coords(staggerloc=[ESMF.StaggerLoc.CENTER])

# Check the source grid coordinate bounds.
exLB = srcgrid.lower_bounds[ESMF.StaggerLoc.CENTER]
exUB = srcgrid.upper_bounds[ESMF.StaggerLoc.CENTER]
print "Grid Bounds:"
print "    exclusiveLBounds = " + str(exLB)
print type(exLB)
print "    exclusiveUBounds = " + str(exUB)
print exUB.dtype

# Get and set the source grid coordinates.
gridCoord = srcgrid.coords[ESMF.StaggerLoc.CENTER]
for i in xrange(gridCoord[x].shape[x]):
    gridCoord[x][i, :] = float(i)
for j in xrange(gridCoord[y].shape[y]):
    gridCoord[y][:, j] = float(j)

# Create a field on the centers of the source grid with the mask applied.
srcfield = ESMF.Field(srcgrid, "srcfield", staggerloc=ESMF.StaggerLoc.CENTER,
                      mask_values=mask)

# Create a destination grid from a SCRIP formatted file.
dstgrid = ESMF.Grid(filename="data/ll2.5deg_grid.nc",
                    filetype=ESMF.FileFormat.SCRIP)

# Create a field on the centers of the destination grid.
dstfield = ESMF.Field(dstgrid, "dstfield", staggerloc=ESMF.StaggerLoc.CENTER)

# Regrid from source grid to destination grid.
regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                            src_mask_values=mask,
                            regrid_method=ESMF.RegridMethod.BILINEAR,
                            unmapped_action=ESMF.UnmappedAction.IGNORE)
dstfield = regridSrc2Dst(srcfield, dstfield, zero_region=ESMF.Region.SELECT)

if ESMF.local_pet() == 0:
    print "Successfully created a grid with periodic dimension and mask and created a field on it!"
