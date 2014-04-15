# This example demonstrates how to create an ESMPy mesh from file
# The mesh file is required, it can be retrieved from the ESMF data repository:
#   wget http://www.earthsystemmodeling.org/download/data/mpas_uniform_10242_dual_counterclockwise.nc

import ESMF

# Start up ESMF, this call is only necessary to override the default parameters
# for logkind (ESMF.LogKind.NONE) and debug (False)
esmpy = ESMF.Manager(logkind=ESMF.LogKind.MULTI, debug=True)

# create an ESMF formatted unstructured mesh with clockwise cells removed
mesh = ESMF.Mesh(filename="data/mpas_uniform_10242_dual_counterclockwise.nc",
                 filetype=ESMF.FileFormat.ESMFMESH)

# create a field on the nodes of the mesh
field = ESMF.Field(mesh, "field", meshloc=ESMF.MeshLoc.NODE)

print "Successfully read a mesh and created a field!"
print "The field values on PET (processor) # {0} are:".format(ESMF.local_pet())
print field