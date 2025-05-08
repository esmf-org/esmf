# This example demonstrates how to create ESMPy Grid, Mesh and Field objects 
# from file and use them for regridding.


import os
import esmpy

from esmpy.util.cache_data import DATA_DIR
from esmpy.util.exceptions import DataMissing

# The data files can be retrieved from the ESMF data repository by uncommenting the
# following block of code:
#
# from esmpy.util.cache_data import cache_data_file
# cache_data_file(os.path.join(DATA_DIR, "so_Omon_GISS-E2.nc"))
# cache_data_file(os.path.join(DATA_DIR, "mpas_uniform_10242_dual_counterclockwise.nc"))

# This call enables debug logging
# esmpy.Manager(debug=True)

datafile = os.path.join(DATA_DIR, "so_Omon_GISS-E2.nc")
if not os.path.exists(datafile):
    raise DataMissing("Data not available, try 'make download'.")
meshfile = os.path.join(DATA_DIR, "mpas_uniform_10242_dual_counterclockwise.nc")
if not os.path.exists(meshfile):
    raise DataMissing("Data not available, try 'make download'.")


# Create a  global grid from a GRIDSPEC formatted file
grid = esmpy.Grid(filename=os.path.join(DATA_DIR, datafile),
                 filetype=esmpy.FileFormat.GRIDSPEC)

# Create a field on the centers of the grid, with extra dimensions
srcfield = esmpy.Field(grid, staggerloc=esmpy.StaggerLoc.CENTER, ndbounds=[33, 2])

# Read the field data from file
srcfield.read(filename=os.path.join(DATA_DIR, datafile),
           variable="so", timeslice=2)

# Create an ESMF formatted unstructured mesh with clockwise cells removed
mesh = esmpy.Mesh(filename=os.path.join(DATA_DIR, meshfile),
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
