"""
Utilities for Fields
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

def compare_fields(field1, field2, itrp_mean_tol, itrp_max_tol, csrv_tol, 
                   parallel=False, dstfracfield=None, mass1=None, mass2=None, 
                   regrid_method=ESMF.RegridMethod.CONSERVE, 
                   uninitval=422397696., mask_values=[0]):
    '''
    PRECONDITIONS: Two Fields have been created and a comparison of the
                   the values is desired between 'field1' and 
                   'field2'.  The fields should be the same size on have
                   rank=2 or 3.
    POSTCONDITIONS: The values on 'field1' and 'field2' are 
                    compared against the each other.
    '''
    import numpy.ma as ma

    correct = False
    # verify that the fields are the same size
    assert field1.data.shape == field2.data.shape, 'compare_fields: Fields must be the same size!'
    
    # deal with default values for fracfield
    if dstfracfield is None:
        dstfracfield = ma.ones(field1.data.shape)

    # compute pointwise error measures
    totalErr = 0.0
    max_error = 0.0
    min_error = 1000000.0
    num_nodes = 0

    # allow fields of all dimensions
    field1_flat = np.ravel(field1.data)
    field2_flat = np.ravel(field2.data)
    dstfracfield_flat = np.ravel(dstfracfield.data)
    # setup mask, no Mask on a Mesh (yet) so need to look at the type first
    if ((type(field2.grid) is ESMF.Grid) and
        (field2.grid.mask[field2.staggerloc] is not None)):
        if (field2.grid.mask[field2.staggerloc] is not None):
            field2mask_flat = [True if x in mask_values else False for x in field2.grid.mask[field2.staggerloc].flatten().tolist()]
    else:
        field2mask_flat = np.ravel(np.zeros_like(field2.data))

    for i in range(field2_flat.size):     
        if ((not field2mask_flat[i]) and 
            (field1_flat[i] != uninitval) and 
            (dstfracfield_flat[i] >= 0.999)):
            if (field2_flat.data[i] != 0.0):
                err = abs(field1_flat[i]/dstfracfield_flat[i] - \
                            field2_flat[i])/abs(field2_flat[i])
            else:
                err = abs(field1_flat[i]/dstfracfield_flat[i] - \
                            field2_flat[i])
            num_nodes += 1
            totalErr += err
            if (err > max_error):
                max_error = err
            if (err < min_error):
                min_error = err


    # gather error on processor 0 or set global variables in serial case
    mass1_global = 0.
    mass2_global = 0.
    csrv_error_global = 0
    if parallel:
        # use mpi4py to collect values
        from mpi4py import MPI
        comm = MPI.COMM_WORLD
        total_error_global = comm.reduce(totalErr, op=MPI.SUM)
        num_nodes_global = comm.reduce(num_nodes, op=MPI.SUM)
        max_error_global = comm.reduce(max_error, op=MPI.MAX)
        min_error_global = comm.reduce(min_error, op=MPI.MIN)
        if (mass1 is not None) and (mass2 is not None):
            mass1_global = comm.reduce(mass1, op=MPI.SUM)
            mass2_global = comm.reduce(mass2, op=MPI.SUM)
    else:
        total_error_global = totalErr
        num_nodes_global = num_nodes
        max_error_global = max_error
        min_error_global = min_error
        if (mass1 is not None) and (mass2 is not None):
            mass1_global = mass1
            mass2_global = mass2

    # compute relative error measures and compare against tolerance values
    itrp_mean = False
    itrp_max = False
    csrv = False
    if ESMF.local_pet() == 0:
        if mass1_global == 0.:
            csrv_error_global = abs(mass2_global - mass1_global)
        else:
            csrv_error_global = abs(mass2_global - mass1_global)/abs(mass1_global)
        # compute mean relative error
        if num_nodes_global != 0:
            total_error_global = total_error_global/num_nodes_global

        # determine if interpolation and conservation are up to spec
        if (total_error_global < itrp_mean_tol):
            itrp_mean = True
        if (max_error_global < itrp_max_tol):
            itrp_max = True
        if (csrv_error_global < csrv_tol):
            csrv = True

        # print out diagnostic information
        print "  Mean relative error = "+str(total_error_global)
        print "  Max  relative error = "+str(max_error_global)
        print "  Conservation  error = "+str(csrv_error_global)
        #print "  Min error   = "+str(min_error_global)
        #print "  srcmass     = "+str(mass1_global)
        #print "  dstmass     = "+str(mass2_global)

    # broadcast in parallel case
    if parallel:
        itrp_mean, itrp_max, csrv = \
            MPI.COMM_WORLD.bcast([itrp_mean, itrp_max, csrv],0)
        total_error_global, csrv_error_global = \
            MPI.COMM_WORLD.bcast([total_error_global, csrv_error_global], 0)

    # print pass or fail
    if (itrp_mean and itrp_max  and csrv):
        print "PET{0} - PASS".format(ESMF.local_pet())
        correct = True
    else:
        print "PET{0} - FAIL".format(ESMF.local_pet())

    return total_error_global, csrv_error_global, correct
