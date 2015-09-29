# This example demonstrates how to create an ESMPy grid in memory with
# periodic dimension and mask.
# The data files can be retrieved from the ESMF data repository by uncommenting the
# following block of code:
#
# import os
# DD = os.path.join(os.getcwd(), "examples/data")
# if not os.path.isdir(DD):
#     os.makedirs(DD)
# from ESMF.util.cache_data import cache_data_file
# cache_data_file(os.path.join(DD, "ll2.5deg_grid.nc"))

import ESMF
import numpy

gridfile = "examples/data/ll2.5deg_grid.nc"

# This call enables debug logging
# esmpy = ESMF.Manager(debug=True)

# Create the source grid from memory with periodic dimension specified.
[lat,lon] = [1,0]
# switch lats and lons to correspond to swapping the periodic and pole dimensions (below)
lats  = numpy.arange(  0, 360, 360./70.)
lons = numpy.arange(-90., 90.1, 180./140.)
max_index = numpy.array([lons.size, lats.size])
srcgrid = ESMF.Grid(max_index, coord_sys=ESMF.CoordSys.SPH_DEG, coord_typekind=ESMF.TypeKind.R4,
                    num_peri_dims=1, periodic_dim=1, pole_dim=0)

# Add coordinates to the source grid.
srcgrid.add_coords(staggerloc=[ESMF.StaggerLoc.CENTER])

# Get and set the source grid coordinates.
gridCoordLat = srcgrid.get_coords(lat)
gridCoordLon = srcgrid.get_coords(lon)

lons_par = lons[srcgrid.lower_bounds[ESMF.StaggerLoc.CENTER][0]:srcgrid.upper_bounds[ESMF.StaggerLoc.CENTER][0]]
lats_par = lats[srcgrid.lower_bounds[ESMF.StaggerLoc.CENTER][1]:srcgrid.upper_bounds[ESMF.StaggerLoc.CENTER][1]]

gridCoordLat[...] = lats_par.reshape(1, lats_par.size)
gridCoordLon[...] = lons_par.reshape(lons_par.size, 1)

# Add a mask to the source grid
mask = srcgrid.add_item(ESMF.GridItem.MASK)
dx=gridCoordLon-max_index[0]/2.0
dy=gridCoordLat-max_index[1]/2.0
mask[...] = 0
mask[numpy.where(numpy.sqrt(dx*dx+dy*dy) < 10.0)] = 2

# Create a field on the centers of the source grid with the mask applied.
srcfield = ESMF.Field(srcgrid, name="srcfield", staggerloc=ESMF.StaggerLoc.CENTER)

srcfield.data[...] = 0

# Create a destination grid from a SCRIP formatted file.
dstgrid = ESMF.Grid(filename=gridfile,
                    filetype=ESMF.FileFormat.SCRIP)

# Create a field on the centers of the destination grid.
dstfield = ESMF.Field(dstgrid, name="dstfield", staggerloc=ESMF.StaggerLoc.CENTER)

missing_val = 1000
dstfield.data[...] = missing_val

# Regrid from source grid to destination grid.
regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                            src_mask_values=numpy.array([2], dtype=numpy.int32),
                            regrid_method=ESMF.RegridMethod.BILINEAR,
                            unmapped_action=ESMF.UnmappedAction.IGNORE)

dstfield = regridSrc2Dst(srcfield, dstfield, zero_region=ESMF.Region.SELECT)

dgridCoordLat = dstgrid.get_coords(lat)
dstmaskedlats = dgridCoordLat[numpy.where(dstfield.data == missing_val)]

masked_values = dstmaskedlats.size
try:
    # use mpi4py to collect values
    from mpi4py import MPI
    comm = MPI.COMM_WORLD
    masked_values = comm.reduce(dstmaskedlats.size, op=MPI.SUM)
except:
    pass

if ESMF.local_pet() == 0:
    assert (masked_values > 0)
    print "Successfully created a grid with masking and switched periodic dimensions for regridding!"
