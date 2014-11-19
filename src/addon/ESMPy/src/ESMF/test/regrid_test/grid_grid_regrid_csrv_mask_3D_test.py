# $Id$

"""
This test demonstrates conservative regridding between 3d grids 
with destination masking.

Two Field objects are created on 3d Grids.    The
source Field is set to an analytic function, and a conservative 
regridding operation is performed from the source to the destination 
grid with masking.  After the regridding is completed, the destination 
Field is compared to the exact solution over that domain.
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

from ESMF.test.regrid_test.grid_regridding_utilities import *

esmp = ESMF.Manager(logkind=ESMF.LogKind.MULTI, debug=True)

parallel = False
if ESMF.pet_count() > 1:
    if ESMF.pet_count() != 4:
        raise NameError('MPI rank must be 4 in parallel mode!')
    parallel = True

# opening remarks
if ESMF.local_pet() == 0:
    print "\ngrid_grid_regrid_csrv_mask_3D"

# create a grid
srcgrid = grid_create_3d([0,0,0,21,21,21], [0,0,0,21,21,21])
dstgrid = grid_create_3d([0.5,0.5,0.5,19.5,19.5,19.5], \
                         [0.5,0.5,0.5,19.5,19.5,19.5])

# create Field objects on the Meshes
srcfield = ESMF.NewField(srcgrid, 'srcfield')
srcareafield = ESMF.NewField(srcgrid, 'srcareafield')
srcfracfield = ESMF.NewField(srcgrid, 'srcfracfield')
dstfield = ESMF.NewField(dstgrid, 'dstfield')
dstareafield = ESMF.NewField(dstgrid, 'dstareafield')
dstfracfield = ESMF.NewField(dstgrid, 'dstfracfield')
exactfield = ESMF.NewField(dstgrid, 'exactfield')

# initialize the Fields to an analytic function
srcfield = initialize_field_grid_3d(srcfield)
exactfield = initialize_field_grid_3d(exactfield)

# run the ESMF regridding
regridSrc2Dst = ESMF.Regrid(srcfield, dstfield, \
                            regrid_method=ESMF.RegridMethod.CONSERVE, \
                            unmapped_action=ESMF.UnmappedAction.ERROR, \
                            src_frac_field=srcfracfield, \
                            dst_frac_field=dstfracfield)
dstfield = regridSrc2Dst(srcfield, dstfield)

# compute the mass
srcmass = compute_mass_grid(srcfield, srcareafield, 
                            dofrac=True, fracfield=srcfracfield)
dstmass = compute_mass_grid(dstfield, dstareafield)

# compare results and output PASS or FAIL
compare_fields_grid(dstfield, exactfield, 10E-03, 10E-16, parallel=parallel, 
                    dstfracfield=dstfracfield, mass1=srcmass, mass2=dstmass)
