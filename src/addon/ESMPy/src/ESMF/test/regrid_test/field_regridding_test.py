# $Id$

"""
This is a demonstration of the regridding capabilities available 
through the ESMF Python interface.  This can be run as a stand-alone 
Python program, it will as long as the ESMP library has been installed.  
See the ESMP README for more information on the build and installation.

The program flow of this tutorial goes as follows: Two Field objects 
are created on top of two independent and different Grid objects.  The
Grids are created as periodic grids defined on the surface of the 
sphere, and there is masking on the source Grid.  The source Field is 
set to an analytic function, and a conservative regridding operation is 
performed from the source to the destination Field.  After the 
regridding is completed, the destination Field is compared to the exact 
solution over that domain and the conservation of mass from the source 
to destination is validated.
"""

try:
    import numpy as np
except:
    raise ImportError('The Numpy library cannot be found!')

try:
    import ESMF
except:
    raise ImportError('The ESMP library cannot be found!')

from ESMF.test.regrid_test.grid_regridding_utilities import *

esmp = ESMF.Manager(logkind=ESMF.LogKind.MULTI, debug=True)

parallel = False
if ESMF.pet_count() > 1:
    if ESMF.pet_count() != 4:
        raise NameError('MPI rank must be 4 in parallel mode!')
    parallel = True

# opening remarks
if ESMF.local_pet() == 0:
    print "\nfield_regridding"

# create a grid
srcgrid = grid_create_periodic([60,30], domask=True)
dstgrid = grid_create_periodic([55,28])

# create the Fields
srcfield = ESMF.Field(srcgrid, 'srcfield', mask_values=[0])
dstfield = ESMF.Field(dstgrid, 'dstfield')
exactfield = ESMF.Field(dstgrid, 'exactfield')

# create the area fields
srcareafield = ESMF.Field(srcgrid, 'srcareafield')
dstareafield = ESMF.Field(dstgrid, 'dstareafield')

# create the fraction fields
srcfracfield = ESMF.Field(srcgrid, 'srcfracfield')
dstfracfield = ESMF.Field(dstgrid, 'dstfracfield')

# initialize the Fields to an analytic function
srcfield = initialize_field_grid_periodic(srcfield)
exact_field = initialize_field_grid_periodic(exactfield)

# run the ESMF regridding
regridSrc2Dst = ESMF.Regrid(srcfield, dstfield, \
                            src_mask_values=np.array([0]), \
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
compare_fields_grid(dstfield, exact_field, 10E-2, 10e-15, parallel=parallel, 
                    dstfracfield=dstfracfield, mass1=srcmass, mass2=dstmass)
