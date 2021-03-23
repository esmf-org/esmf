"""
Utilities for Fields
"""

import numpy as np
import ESMF

import ESMF.util.helpers as helpers
import ESMF.api.constants as constants

def compare_fields(field1, field2, itrp_mean_tol, itrp_max_tol, csrv_tol, 
                   dstfracfield=None, mass1=None, mass2=None,
                   regrid_method=ESMF.RegridMethod.CONSERVE, 
                   uninitval=422397696., mask_values=[0]):
    """
    Compare the values of two fields to verify the accuracy of a Regrid.  The
    Fields should be the same size and have rank = 2 or 3.
    :param field1: The Field that received the interpolation values.
    :param field2: The Field holding the values of the exact solution.
    :param itrp_mean_tol: The mean relative error tolerance.
    :param itrp_max_tol: The maximum relative error tolerance.
    :param csrv_tol: The conservation relative error tolerance.
    :param parallel: True or False value to tell whether this is a parallel run
    :param dstfracfield:
    :param mass1: The mass of Field 1.
    :param mass2: The mass of Field 2.
    :param regrid_method: The regrid method that was used.
    :param uninitval: The uninitialized value for Field1.
    :param mask_values: Any masked values to skip when comparing the Fields.
    :return:
    """
    import numpy.ma as ma

    parallel = False
    if ESMF.pet_count() > 1:
        parallel = True

    correct = False
    # verify that the fields are the same size
    assert field1.data.shape == field2.data.shape, 'compare_fields: Fields must be the same size!'
    
    # deal with default values for fracfield
    if isinstance(dstfracfield, type(None)):
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
    if (isinstance(field2.grid, ESMF.Grid)) and \
        (not isinstance(field2.grid.mask[field2.staggerloc], type(None))):
        if not isinstance(field2.grid.mask[field2.staggerloc], type(None)):
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
        total_error_global = helpers.reduce_val(totalErr)
        num_nodes_global = helpers.reduce_val(num_nodes)
        max_error_global = helpers.reduce_val(max_error, op=constants.Reduce.MAX)
        min_error_global = helpers.reduce_val(min_error, op=constants.Reduce.MIN)
        if not isinstance(mass1, type(None)) and not isinstance(mass2, type(None)):
            mass1_global = helpers.reduce_val(mass1)
            mass2_global = helpers.reduce_val(mass2)

    else:
        total_error_global = totalErr
        num_nodes_global = num_nodes
        max_error_global = max_error
        min_error_global = min_error
        if not isinstance(mass1, type(None)) and not isinstance(mass2, type(None)):
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
        print ("\n  Mean relative error = "+str(total_error_global))
        print ("  Max  relative error = "+str(max_error_global))
        print ("  Conservation  error = "+str(csrv_error_global))
        #print ("  Min error   = "+str(min_error_global))
        #print ("  srcmass     = "+str(mass1_global))
        #print ("  dstmass     = "+str(mass2_global))

    # broadcast in parallel case
    if parallel:
        itrp_mean = helpers.broadcast_val(itrp_mean)
        itrp_max = helpers.broadcast_val(itrp_max)
        csrv = helpers.broadcast_val(csrv)
        total_error_global = helpers.broadcast_val(total_error_global)
        csrv_error_global = helpers.broadcast_val(csrv_error_global)

    # print pass or fail
    if (itrp_mean and itrp_max  and csrv):
        print ("PET{0} - PASS".format(ESMF.local_pet()))
        correct = True
    else:
        print ("PET{0} - FAIL".format(ESMF.local_pet()))

    return total_error_global, csrv_error_global, correct