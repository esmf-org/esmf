import ESMF
from ESMF.test.regrid_test.regrid_from_file_test.run_regrid_from_file_dryrun import cache_data_file

if ESMF.local_pet == 0:
    # retrieve the data files from the esmf data repository
    response = cache_data_file("T42_grid.nc")
    response = cache_data_file("ne15np4_scrip.nc")
  
# create a logically rectangular T42 source grid for a SCRIP format file 
grid = ESMF.Grid(filename="T42_grid.nc", \
                 filetype=ESMF.FileFormat.SCRIP, \
                 staggerloc=ESMF.StaggerLoc.CENTER, \
                 add_corner_stagger=True)

print "PET {0}: size = {1}\n size_local = {2}".format(ESMF.local_pet(), 
                                                      grid.size, 
                                                      grid.size_local)
'''
# create a field on the center stagger locations of the source grid
srcfield = ESMF.Field(grid, 'srcfield', staggerloc=ESMF.StaggerLoc.CENTER)

# initialize the field to a constant value
srcfield[...] = 25

# create an unstructured cubed-sphere destination mesh from a SCRIP format file
mesh = ESMF.Mesh(filename="ne15np4_scrip.nc", \
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
'''