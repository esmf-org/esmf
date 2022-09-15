# This example demonstrates how to create an ESMPy grid in memory with
# periodic dimension and mask.
# The data files can be retrieved from the ESMF data repository by uncommenting the
# following block of code:
#
# import os
# DD = os.path.join(os.getcwd(), "examples/data")
# if not os.path.isdir(DD):
#     os.makedirs(DD)
# from esmpy.util.cache_data import cache_data_file
# cache_data_file(os.path.join(DD, "ll2.5deg_grid.nc"))

import esmpy
import numpy

import os

import esmpy.util.helpers as helpers
import esmpy.api.constants as constants
from esmpy.util.cache_data import DATA_DIR
from esmpy.util.exceptions import DataMissing

gridfile = os.path.join(DATA_DIR, "ll2.5deg_grid.nc")
if not os.path.exists(gridfile):
    raise DataMissing("Data not available, try 'make download'.")

# This call enables debug logging
# esmpy = esmpy.Manager(debug=True)

# Create the source grid from memory with periodic dimension specified.
[lat,lon] = [1,0]
# switch lats and lons to correspond to swapping the periodic and pole dimensions (below)
lats  = numpy.arange(  0, 360, 360./70.)
lons = numpy.arange(-90., 90.1, 180./140.)
max_index = numpy.array([lons.size, lats.size])
srcgrid = esmpy.Grid(max_index, coord_sys=esmpy.CoordSys.SPH_DEG, coord_typekind=esmpy.TypeKind.R4,
                    num_peri_dims=1, periodic_dim=1, pole_dim=0)

# Add coordinates to the source grid.
srcgrid.add_coords(staggerloc=[esmpy.StaggerLoc.CENTER])

# Get and set the source grid coordinates.
gridCoordLat = srcgrid.get_coords(lat)
gridCoordLon = srcgrid.get_coords(lon)

lons_par = lons[srcgrid.lower_bounds[esmpy.StaggerLoc.CENTER][0]:srcgrid.upper_bounds[esmpy.StaggerLoc.CENTER][0]]
lats_par = lats[srcgrid.lower_bounds[esmpy.StaggerLoc.CENTER][1]:srcgrid.upper_bounds[esmpy.StaggerLoc.CENTER][1]]

gridCoordLat[...] = lats_par.reshape(1, lats_par.size)
gridCoordLon[...] = lons_par.reshape(lons_par.size, 1)

# Add a mask to the source grid
mask = srcgrid.add_item(esmpy.GridItem.MASK)
dx=gridCoordLon-max_index[0]/2.0
dy=gridCoordLat-max_index[1]/2.0
mask[...] = 0
mask[numpy.where(numpy.sqrt(dx*dx+dy*dy) < 10.0)] = 2

# Create a field on the centers of the source grid with the mask applied.
srcfield = esmpy.Field(srcgrid, name="srcfield", staggerloc=esmpy.StaggerLoc.CENTER)

srcfield.data[...] = 0

# Create a destination grid from a SCRIP formatted file.
dstgrid = esmpy.Grid(filename=gridfile,
                    filetype=esmpy.FileFormat.SCRIP)

# Create a field on the centers of the destination grid.
dstfield = esmpy.Field(dstgrid, name="dstfield", staggerloc=esmpy.StaggerLoc.CENTER)

missing_val = 1000
dstfield.data[...] = missing_val

# Regrid from source grid to destination grid.
regridSrc2Dst = esmpy.Regrid(srcfield, dstfield,
                            src_mask_values=numpy.array([2], dtype=numpy.int32),
                            regrid_method=esmpy.RegridMethod.BILINEAR,
                            unmapped_action=esmpy.UnmappedAction.IGNORE)

dstfield = regridSrc2Dst(srcfield, dstfield, zero_region=esmpy.Region.SELECT)

dgridCoordLat = dstgrid.get_coords(lat)
dstmaskedlats = dgridCoordLat[numpy.where(dstfield.data == missing_val)]

masked_values = dstmaskedlats.size

# handle the parallel case
if esmpy.pet_count() > 1:
    masked_values = helpers.reduce_val(dstmaskedlats.size, op=constants.Reduce.SUM)

if esmpy.local_pet() == 0:
    assert (masked_values > 0)
    print ("Successfully created a grid with masking and switched periodic dimensions for regridding!")
