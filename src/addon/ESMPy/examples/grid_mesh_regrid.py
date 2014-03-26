import os
import ESMF
from ESMF.test.regrid_test.regrid_from_file_test.run_regrid_from_file_dryrun import cache_data_file

grid1 = "T42_grid.nc"
grid2 = "ne15np4_scrip.nc"         #SCRIP

prefix = 'data/'
grid1 = prefix+grid1
grid2 = prefix+grid2
if ESMF.local_pet() == 0:
    if not os.path.exists(prefix):
        os.mkdir(prefix)
    response = cache_data_file(grid1)
    response = cache_data_file(grid2)
    
# create a logically rectangular source grid for a SCRIP format file 
grid = ESMF.Grid(filename=grid1, \
                 filetype=ESMF.FileFormat.SCRIP, \
                 add_corner_stagger=True)

# create a field on the center stagger locations of the source grid
srcfield = ESMF.Field(grid, 'srcfield', staggerloc=ESMF.StaggerLoc.CENTER)

# initialize the field to a constant value
srcfield[...] = 25

# create an unstructured cubed-sphere destination mesh from a SCRIP format file
mesh = ESMF.Mesh(filename=grid2, \
                 filetype=ESMF.FileFormat.SCRIP)

# create a field on the elements of the destination mesh
dstfield = ESMF.Field(mesh, 'dstmesh', meshloc=ESMF.MeshLoc.ELEMENT)

# create an object to regrid data from the source to the destination field
regrid = ESMF.Regrid(srcfield, dstfield, \
                     regrid_method=ESMF.RegridMethod.CONSERVE, \
                     unmapped_action=ESMF.UnmappedAction.IGNORE)

# do the regridding from source to destination field
dstfield = regrid(srcfield, dstfield)

# show that the destination field data matches the initial source data
print dstfield.data
