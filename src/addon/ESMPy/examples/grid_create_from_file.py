import ESMF

# Start up ESMF, this call is only necessary to override the default parameters
# for logkind (ESMF.LogKind.NONE) and debug (False)
esmpy = ESMF.Manager(logkind=ESMF.LogKind.MULTI, debug=True)

# Create a uniform global latlon grid from a SCRIP formatted file
grid = ESMF.Grid(filename="data/ll2.5deg_grid.nc", filetype=ESMF.FileFormat.SCRIP)

# Create a field on the centers of the grid
field = ESMF.Field(grid, "field", staggerloc=ESMF.StaggerLoc.CENTER)

print "Successfully read a grid and created a field!"
print "The field values on PET (processor) # {0} are:".format(ESMF.local_pet())
print field