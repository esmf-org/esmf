# $Id$

"""
Two Field objects are created, one on a Grid and the other on a Mesh.  
The source Field is set to an analytic function, and a regridding 
operation is performed from the source to the destination Field.  After 
the regridding is completed, the destination Field is compared to the 
exact solution over that domain.
"""

try:
	import numpy as np
except:
	raise ImportError('The Numpy library cannot be found!')

try:
	import ESMF
except:
	raise ImportError('The ESMF library cannot be found!')

from ESMF.test.regrid_test.mesh_regridding_utilities import *
from ESMF.test.regrid_test.grid_regridding_utilities import *

esmp = ESMF.Manager(logkind=ESMF.LogKind.MULTI, debug=True)

parallel = False
if ESMF.pet_count() > 1:
		if ESMF.pet_count() != 4:
				raise NameError('MPI rank must be 4 in parallel mode!')
		parallel = True

# opening remarks
if ESMF.local_pet() == 0:
		print "\ngrid_mesh_regrid"

# create a grid
grid = grid_create([0,0,8,8], [0,0,4,4])

#grid._write("grid")

# create a Mesh
if parallel:
		mesh, nodeCoord, elemType, elemConn = \
				mesh_create_50_parallel()
else:
		mesh, nodeCoord, elemType, elemConn = \
				mesh_create_50()

# create Field objects
srcfield = ESMF.Field(mesh, 'srcfield')
dstfield = ESMF.Field(grid, 'dstfield')
#dstareafield = ESMF.Field(mesh, 'dstareafield')
exactfield = ESMF.Field(grid, 'exactfield')

# initialize the Fields to an analytic function
srcfield = initialize_field_mesh(srcfield, nodeCoord, elemType, elemConn)
exactfield = initialize_field_grid(exactfield)

#import pdb; pdb.set_trace()
# run the ESMF regridding
regridSrc2Dst = ESMF.Regrid(srcfield, dstfield, 
                            regrid_method=ESMF.RegridMethod.BILINEAR, 
                            unmapped_action=ESMF.UnmappedAction.ERROR)
dstfield = regridSrc2Dst(srcfield, dstfield)

'''
print "PET:{0} - grid.size  = {1}\n grid.size_local  = {2}\n \
	  grid.lower_bounds = {3}\n grid.upper_bounds = {4}\n \
	  grid.coords = {5}".format(
	  ESMF.local_pet(), grid.size, grid.size_local, grid.lower_bounds, 
	  grid.upper_bounds, grid.coords)
'''
print "PET:{0} - dstfield.shape = {1}".format(ESMF.local_pet(), dstfield.shape)
print "PET:{0} - dstfield = \n{1}".format(ESMF.local_pet(), dstfield.data)
print "PET:{0} - exactfield = \n{1}".format(ESMF.local_pet(), exactfield.data)

print "PET:{0} - mesh.size  = {1}\n mesh.size_local  = {2}".format(ESMF.local_pet(), mesh.size, mesh.size_local)
print "PET:{0} - srcfield.shape = {1} - srcfield = \n{2}".format(ESMF.local_pet(), srcfield.shape, srcfield.data)

#print compute_mass_mesh(dstfield, dstareafield)

# compare results and output PASS or FAIL
compare_fields_grid(dstfield, exactfield, 40E-2, 10E-16, parallel=parallel)
