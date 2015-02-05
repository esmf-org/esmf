# This example demonstrates how to create an ESMPy grid in memory with
# periodic dimension and mask.
import ESMF
import numpy

# Start up ESMF, this call is only necessary to override the default parameters
# for logkind (ESMF.LogKind.NONE) and debug (False)
esmpy = ESMF.Manager(logkind=ESMF.LogKind.MULTI, debug=True)

# Create the source grid from memory with periodic dimension specified.
[lat,lon] = [1,0]
lons  = numpy.arange(  0, 360, 360./70.)
lats = numpy.arange(-90.,  90.1, 180./140.)
max_index = numpy.array([lats.size, lons.size])
srcgrid = ESMF.Grid(max_index, num_peri_dims=1, periodic_dim=1,
                    coord_sys=ESMF.CoordSys.SPH_DEG,
                    coord_typekind=ESMF.TypeKind.R4)

# Add a mask to the source grid
mask = srcgrid.add_item(ESMF.GridItem.MASK)
mask[...] = 1
mask[5,:] = 0

# Add coordinates to the source grid.
srcgrid.add_coords(staggerloc=[ESMF.StaggerLoc.CENTER])

# Get and set the source grid coordinates.
gridCoordLat = srcgrid.get_coords(lat)
gridCoordLon = srcgrid.get_coords(lon)

gridCoordLat[...] = lats.reshape(lats.size, 1)
gridCoordLon[...] = lons.reshape(1, lons.size)

# Create a field on the centers of the source grid with the mask applied.
srcfield = ESMF.Field(srcgrid, "srcfield", staggerloc=ESMF.StaggerLoc.CENTER,
                      mask_values=mask)

srcfield.data[...] = 25

# Create a destination grid from a SCRIP formatted file.
dstgrid = ESMF.Grid(filename="examples/data/ll2.5deg_grid.nc",
                    filetype=ESMF.FileFormat.SCRIP)

# Create a field on the centers of the destination grid.
dstfield = ESMF.Field(dstgrid, "dstfield", staggerloc=ESMF.StaggerLoc.CENTER)

missing_val = 1e20
dstfield.data[...] = missing_val

# Regrid from source grid to destination grid.
regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                            src_mask_values=numpy.array([0], dtype=numpy.int32),
                            regrid_method=ESMF.RegridMethod.BILINEAR,
                            unmapped_action=ESMF.UnmappedAction.ERROR)
dstfield = regridSrc2Dst(srcfield, dstfield, zero_region=ESMF.Region.SELECT)

maskedlats = gridCoordLat[numpy.where(mask == 1)]
maskedlons = gridCoordLon[numpy.where(mask == 1)]

dgridCoordLat = dstgrid.get_coords(lat)
dgridCoordLon = dstgrid.get_coords(lon)
dstmaskedlats = dgridCoordLat[numpy.where(dstfield.data == missing_val)]
dstmaskedlons = dgridCoordLon[numpy.where(dstfield.data == missing_val)]

# TODO: the source masking does not seem to be causing any missing values in the destination field!!
assert ((dstmaskedlons.size and dstmaskedlats.size) != 0)

if ESMF.local_pet() == 0:
    print "Successfully created a grid with masking and switched periodic dimensions for regridding!"
