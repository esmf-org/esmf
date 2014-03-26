# $Id$

"""
Two Field objects are created, both on a Mesh.  The source Field is set 
to an analytic function, and a regridding operation is performed from 
the source to the destination Field.  After the regridding is 
completed, the destination Field is compared to the exact solution over
that domain.
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

esmp = ESMF.Manager(logkind=ESMF.LogKind.MULTI, debug=True)

parallel = False
if ESMF.pet_count() > 1:
    if ESMF.pet_count() > 4:
        raise NameError('MPI rank must be 4 in parallel mode!')
    parallel = True

# opening remarks
if ESMF.local_pet() == 0:
    print "\nmesh_mesh_regrid"

# create two unique Mesh objects
if parallel:
    srcmesh, nodeCoordSrc, elemTypeSrc, elemConnSrc = \
        mesh_create_5_parallel(ESMF.local_pet())
    dstmesh, nodeCoordDst, elemTypeDst, elemConnDst = \
        mesh_create_10_parallel(ESMF.local_pet())
else:
    srcmesh, nodeCoordSrc, elemTypeSrc, elemConnSrc = \
        mesh_create_5()
    dstmesh, nodeCoordDst, elemTypeDst, elemConnDst = \
        mesh_create_10()

# create ESMP_Field objects on the Meshes
srcfield = ESMF.Field(srcmesh, 'srcfield', meshloc=ESMF.MeshLoc.ELEMENT)
srcareafield = ESMF.Field(srcmesh, 'srcareafield', meshloc=ESMF.MeshLoc.ELEMENT)
srcfracfield = ESMF.Field(srcmesh, 'srcfracfield', meshloc=ESMF.MeshLoc.ELEMENT)
dstfield = ESMF.Field(dstmesh, 'dstfield', meshloc=ESMF.MeshLoc.ELEMENT)
dstareafield = ESMF.Field(dstmesh, 'dstareafield', meshloc=ESMF.MeshLoc.ELEMENT)
dstfracfield = ESMF.Field(dstmesh, 'dstfracfield', meshloc=ESMF.MeshLoc.ELEMENT)
exactfield = ESMF.Field(dstmesh, 'exactfield', meshloc=ESMF.MeshLoc.ELEMENT)

# initialize the Fields to an analytic function
srcfield = initialize_field_mesh(srcfield, nodeCoordSrc, \
                                        elemTypeSrc, elemConnSrc)
exactfield = initialize_field_mesh(exactfield, nodeCoordDst, \
                                         elemTypeDst, elemConnDst)

# run the ESMF regridding
regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                            regrid_method=ESMF.RegridMethod.CONSERVE,
                            unmapped_action=ESMF.UnmappedAction.ERROR, \
                            src_frac_field=srcfracfield, \
                            dst_frac_field=dstfracfield)
dstfield = regridSrc2Dst(srcfield, dstfield)

# compute the mass
srcmass = compute_mass_mesh(srcfield, srcareafield, 
                            dofrac=True, fracfield=srcfracfield)
dstmass = compute_mass_mesh(dstfield, dstareafield)

# compare results and output PASS or FAIL
compare_fields_mesh(dstfield, exactfield, 50E-2, 10E-16, parallel=parallel, 
                    dstfracfield=dstfracfield, mass1=srcmass, mass2=dstmass)
