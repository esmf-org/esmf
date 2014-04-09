import ESMF

grid1 = "data/ll2.5deg_grid.nc"
grid2 = "data/mpas_uniform_10242_dual_counterclockwise.nc"         #SCRIP
    
# Create a uniform global latlon grid from a SCRIP formatted file
grid = ESMF.Grid(filename="data/ll2.5deg_grid.nc", filetype=ESMF.FileFormat.SCRIP,
                 add_corner_stagger=True)

# create a field on the center stagger locations of the source grid
srcfield = ESMF.Field(grid, 'srcfield', staggerloc=ESMF.StaggerLoc.CENTER)

# initialize the field to a constant value
srcfield[...] = 25

# create an ESMF formatted unstructured mesh with clockwise cells removed
mesh = ESMF.Mesh(filename="data/mpas_uniform_10242_dual_counterclockwise.nc",
                 filetype=ESMF.FileFormat.ESMFMESH)

# create a field on the elements of the destination mesh
dstfield = ESMF.Field(mesh, 'dstmesh', meshloc=ESMF.MeshLoc.ELEMENT)

# create an object to regrid data from the source to the destination field
regrid = ESMF.Regrid(srcfield, dstfield, \
                     regrid_method=ESMF.RegridMethod.CONSERVE, \
                     unmapped_action=ESMF.UnmappedAction.IGNORE)

# do the regridding from source to destination field
dstfield = regrid(srcfield, dstfield)

print "Successfully read a grid and a mesh and did a regridding of a constant field!"
print "The field values on PET (processor) # {0} are:".format(ESMF.local_pet())
print dstfield.data
