# This example demonstrates how to create ESMPy Grid, Mesh and Field objects from file.
# The data files can be retrieved from the ESMF data repository by uncommenting the
# following block of code:
#
# import os
# DD = os.path.join(os.getcwd(), "examples/data")
# if not os.path.isdir(DD):
#     os.makedirs(DD)
# from ESMF.util.cache_data import cache_data_file
# cache_data_file(os.path.join(DD, "so_Omon_GISS-E2.nc"))
# cache_data_file(os.path.join(DD, "mpas_uniform_10242_dual_counterclockwise.nc"))

import os
import ESMF

# This call enables debug logging
# ESMF.Manager(debug=True)

# set up the DATADIR
DATADIR = os.path.join(os.getcwd(), "examples/data")

# Create a uniform global latlon grid from a SCRIP formatted file
grid = ESMF.Grid(filename=os.path.join(DATADIR, "so_Omon_GISS-E2.nc"),
                 filetype=ESMF.FileFormat.GRIDSPEC)

# Create a field on the centers of the grid
field = ESMF.Field(grid, staggerloc=ESMF.StaggerLoc.CENTER)#, ndbounds=[2])

# field.read does not work if ESMF is built with MPIUNI
if ESMF.api.constants._ESMF_COMM is not ESMF.api.constants._ESMF_COMM_MPIUNI:
    field.read(filename=os.path.join(DATADIR, "so_Omon_GISS-E2.nc"),
               variable="so")

# create an ESMF formatted unstructured mesh with clockwise cells removed
mesh = ESMF.Mesh(filename=os.path.join(DATADIR, "mpas_uniform_10242_dual_counterclockwise.nc"),
                 filetype=ESMF.FileFormat.ESMFMESH)

# create a field on the nodes of the mesh
field = ESMF.Field(mesh, meshloc=ESMF.MeshLoc.NODE)

if ESMF.local_pet() == 0:
    print "Grid, Mesh and Field all created/read from file successfully :)"
