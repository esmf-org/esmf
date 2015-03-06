# This example demonstrates how to create an ESMPy grid from file
# The grid file is required, it can be retrieved from the ESMF data repository:
#   wget http://www.earthsystemmodeling.org/download/data/ll2.5deg_grid.nc

import ESMF

# Start up ESMF, this call is only necessary to enable debug logging
#esmpy = ESMF.Manager(logkind=ESMF.LogKind.MULTI, debug=True)

# Create a uniform global latlon grid from a SCRIP formatted file
grid = ESMF.Grid(filename="examples/data/so_Omon_GISS-E2.nc", filetype=ESMF.FileFormat.GRIDSPEC)

# Create a field on the centers of the grid
field = ESMF.Field(grid, "field", staggerloc=ESMF.StaggerLoc.CENTER)#, ndbounds=[2])

field.read(filename="examples/data/so_Omon_GISS-E2.nc", variable="so")


# create an ESMF formatted unstructured mesh with clockwise cells removed
mesh = ESMF.Mesh(filename="examples/data/mpas_uniform_10242_dual_counterclockwise.nc",
                 filetype=ESMF.FileFormat.ESMFMESH)

# create a field on the nodes of the mesh
field = ESMF.Field(mesh, "field", meshloc=ESMF.MeshLoc.NODE)

if ESMF.local_pet() == 0:
    print "Grid, Mesh and Field all created/read from file successfully :)"