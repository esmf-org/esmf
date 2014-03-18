import os
import ESMF
from ESMF.test.regrid_test.regrid_from_file_test.run_regrid_from_file_dryrun import cache_data_file

# Test setup
grids = [
         # global SCRIP format grid
         "ll2.5deg_grid.nc",
         # global SCRIP T42 grid
         "T42_grid.nc",
         # 1.9x2.5 CAM finite volume grid 
         "fv1.9x2.5_050503.nc"]#,
         # global SCRIP format grid
         #"GRIDSPEC_ACCESS1.nc"]

filetype = [
            ESMF.FileFormat.SCRIP,
            ESMF.FileFormat.SCRIP,
            ESMF.FileFormat.SCRIP]#,
            #ESMF.FileFormat.GRIDSPEC]

grid = 2
prefix = 'data/'
filename = prefix+grids[grid]
if ESMF.local_pet() == 0:
    if not os.path.exists(prefix):
        os.mkdir(prefix)
    cache_data_file(filename)

# Start up ESMF.
esmp = ESMF.Manager(logkind=ESMF.LogKind.SINGLE, debug=True)
pet_count = ESMF.pet_count()

# Create Grid
grid = ESMF.Grid(filename=filename, filetype=filetype[grid])
# create a field on the centers of the Grid
name = filename+'-field'
field = ESMF.Field(grid, name, staggerloc=ESMF.StaggerLoc.CENTER)

# write the mesh to vtk formatted file
#grid._write(filename.rsplit('.',1)[0])

# print the field
#print field
