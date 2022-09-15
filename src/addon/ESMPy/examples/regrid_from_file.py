# This example demonstrates how to create ESMPy Grid, Mesh and Field objects 
# from file and use them for regridding.
# The data files can be retrieved from the ESMF data repository by uncommenting the
# following block of code:
#
# import os
# DD = os.path.join(os.getcwd(), "examples/data")
# if not os.path.isdir(DD):
#     os.makedirs(DD)
# from esmpy.util.cache_data import cache_data_file
# cache_data_file(os.path.join(DD, "so_Omon_GISS-E2.nc"))
# cache_data_file(os.path.join(DD, "mpas_uniform_10242_dual_counterclockwise.nc"))

import os
import esmpy

# This call enables debug logging
# esmpy.Manager(debug=True)

# Set up the DATADIR
DATADIR = os.path.join(os.getcwd(), "examples/data")

# Create a  global grid from a GRIDSPEC formatted file
grid = esmpy.Grid(filename=os.path.join(DATADIR, "so_Omon_GISS-E2.nc"),
                 filetype=esmpy.FileFormat.GRIDSPEC)

# Create a field on the centers of the grid, with extra dimensions
srcfield = esmpy.Field(grid, staggerloc=esmpy.StaggerLoc.CENTER, ndbounds=[33, 2])

# Read the field data from file
srcfield.read(filename=os.path.join(DATADIR, "so_Omon_GISS-E2.nc"),
           variable="so", timeslice=2)

# Create an ESMF formatted unstructured mesh with clockwise cells removed
mesh = esmpy.Mesh(filename=os.path.join(DATADIR, "mpas_uniform_10242_dual_counterclockwise.nc"),
                 filetype=esmpy.FileFormat.ESMFMESH)

# Create a field on the nodes of the mesh
dstfield = esmpy.Field(mesh, meshloc=esmpy.MeshLoc.NODE, ndbounds=[33, 2])

dstfield.data[:] = 1e20

# compute the weight matrix for regridding
regrid = esmpy.Regrid(srcfield, dstfield,
                     regrid_method=esmpy.RegridMethod.BILINEAR,
                     unmapped_action=esmpy.UnmappedAction.IGNORE)

# calculate the regridding from source to destination field
dstfield = regrid(srcfield, dstfield)

if esmpy.local_pet() == 0:
    print ("Fields created from file regridded successfully :)")
