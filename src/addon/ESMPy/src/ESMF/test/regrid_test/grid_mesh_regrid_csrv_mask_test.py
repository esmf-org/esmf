# $Id$

"""
Two Field objects are created, one on a Grid and the other on a Mesh.  
A mask is set on the mesh which is used later in the regridding.  The
source Field is set to an analytic function, and a conservative 
regridding operation is performed from the source to the destination 
Field.  After the regridding is completed, the destination Field is 
compared to the exact solution over that domain.
"""

import sys

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
    print "\ngrid_mesh_regrid_csrv_mask"

# create a Mesh
if parallel:
    mesh, nodeCoord, elemType, elemConn = \
        mesh_create_10_parallel(ESMF.local_pet())
else:
    mesh, nodeCoord, elemType, elemConn, elemMask, elemArea = \
        mesh_create_10(domask=True, doarea=True)

# create a grid
grid = grid_create([0,0,4,4], [0,0,4,4], domask=True, doarea=True)

# create Field objects on the Meshes
srcfield = ESMF.Field(mesh, 'srcfield', meshloc=ESMF.MeshLoc.ELEMENT)
srcfracfield = ESMF.Field(mesh, 'srcfracfield', meshloc=ESMF.MeshLoc.ELEMENT)
srcareafield = ESMF.Field(mesh, 'srcareafield', meshloc=ESMF.MeshLoc.ELEMENT)

# make gridded fields
dstfield = ESMF.Field(grid, 'dstfield')
exactfield = ESMF.Field(grid, 'exactfield')
dstfracfield = ESMF.Field(grid, 'dstfracfield')
dstareafield = ESMF.Field(grid, 'dstareafield')

# initialize the Fields to an analytic function
srcfield = initialize_field_mesh(srcfield, nodeCoord, elemType, elemConn,
                                        domask=True, elemMask=elemMask)
exactfield = initialize_field_grid(exactfield, domask=True, doarea=True)

# run the ESMF regridding
regridSrc2Dst = ESMF.Regrid(srcfield, dstfield, \
                            src_mask_values=np.array([1]), \
                            regrid_method=ESMF.RegridMethod.CONSERVE, \
                            unmapped_action=ESMF.UnmappedAction.ERROR, \
                            src_frac_field=srcfracfield, \
                            dst_frac_field=dstfracfield)
dstfield = regridSrc2Dst(srcfield, dstfield)

# compute the mass
srcmass = compute_mass_mesh(srcfield, srcareafield, dofrac=True, 
                            fracfield=srcfracfield)
dstmass = compute_mass_grid(dstfield, dstareafield)

# compare results and output PASS or FAIL
compare_fields_grid(dstfield, exactfield, 80E-1, 10E-15, parallel=parallel, 
                    dstfracfield=dstfracfield, mass1=srcmass, mass2=dstmass)
