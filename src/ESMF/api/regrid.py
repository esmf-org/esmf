# $Id$

"""
The Regrid API
"""

#### IMPORT LIBRARIES #########################################################
import numpy as np
import numpy.ma as ma

from constants import *
from cbindings import *
from decorators import initialize

from manager import *
from grid import *
from mesh import *
from field import *

#### Regrid class ##############################################################

class Regrid(object):

    # call RegridStore
    @initialize
    def __init__(self, *args, **kwargs):
        """
        Create a handle to a Regridding operation between two Fields. \n
        Required Arguments: \n
            srcfield: source Field associated with an underlying Grid 
                      or Mesh. \n
            dstfield: destination Field associated with an underlying 
                      Grid or Mesh. \n
        Optional Arguments: \n
            src_mask_values: a numpy array (internally cast to 
                             dtype=numpy.int32)of values that can be 
                             used to specify a masked value on the 
                             source Field. \n
                type: numpy.array
                shape: (n, 1) where n is the number of values
            dst_mask_values: a numpy array (internally cast to 
                             dtype=numpy.int32)of values that can be 
                             used to specify a masked value on the 
                             destination Field. \n
                type: numpy.array
                shape: (n, 1) where n is the number of values
            regrid_method: specifies which regridding method to use. \n
                Argument values are: \n
                    (default) RegridMethod.BILINEAR\n
                    RegridMethod.PATCH\n
                    RegridMethod.CONSERVE\n
            unmapped_action: specifies which action to take if a 
                             destination point is found which does not 
                             map to any source point.\n
                Argument values are : \n
                    (default) UnmappedAction.ERROR\n
                    UnmappedAction.IGNORE\n
            src_frac_field: return a numpy array of values containing 
                            weights corresponding to the amount of 
                            each Field value which contributes to the 
                            total mass of the Field. \n
            dst_frac_field: return a numpy array of values containing 
                            weights corresponding to the amount of each 
                            Field value which contributes to the total 
                            mass of the Field. \n
        Returns: \n
            Regrid \n
        """
        # routehandle storage
        self.routehandle = 0

        # args
        try:
            srcfield = args[0]
            dstfield = args[1]
        except:
            raise RequiredArgs(Regrid.__init__.__doc__)

        # kwargs
        coord_dim = None
        if 'coord_dim' in kwargs:
            coord_dim = kwargs.get('coord_dim')

        src_mask_values = None                                                        
        if 'src_mask_values' in kwargs:
            src_mask_values = kwargs.get('src_mask_values')

        dst_mask_values = None
        if 'dst_mask_values' in kwargs:
            dst_mask_values = kwargs.get('dst_mask_values')

        regrid_method = None
        if 'regrid_method' in kwargs:
            regrid_method = kwargs.get('regrid_method')

        unmapped_action = None
        if 'unmapped_action' in kwargs:
            unmapped_action = kwargs.get('unmapped_action')

        src_frac_field = None
        if 'src_frac_field' in kwargs:
            src_frac_field = kwargs.get('src_frac_field')

        dst_frac_field = None
        if 'dst_frac_field' in kwargs:
            dst_frac_field = kwargs.get('dst_frac_field')

        # type checking
        local_src_mask_values = None
        if src_mask_values is not None:
            if src_mask_values.dtype is not np.int32:
                local_src_mask_values = np.array(src_mask_values, 
                                                 dtype=np.int32)
            else:
                local_src_mask_values = src_mask_values
        # else case handled by initialization to None
        local_dst_mask_values = None
        if dst_mask_values is not None:
            if dst_mask_values.dtype is not np.int32:
                local_dst_mask_values = np.array(dst_mask_values, 
                                                 dtype=np.int32)
            else:
                local_dst_mask_values = dst_mask_values
        # else case handled by initialization to None

        # call into the ctypes layer
        self.routehandle = ESMP_FieldRegridStore(srcfield,
                                            dstfield,
                                            srcMaskValues=local_src_mask_values,
                                            dstMaskValues=local_dst_mask_values,
                                            regridmethod=regrid_method,
                                            unmappedaction=unmapped_action,
                                            srcFracField=src_frac_field,
                                            dstFracField=dst_frac_field)

    def __call__(self, *args, **kwargs):
        """
        Call a regridding operation from srcfield to dstfield. \n
        Required Arguments: \n
            srcfield: the Field of source data to regrid. \n
            dstfield: the Field to hold the regridded data. \n
        Optional Arguments: \n
            zero_region: specify which region of the field indices will
                         be zeroed out before adding the values resulting 
                         from the interpolation. \n
                Argument values are: \n
                    (default)TOTAL = the entire Field \n
                    SELECT = only the Field indices which participate 
                             in regridding \n
                    EMPTY = none of the Field \n
        Returns: \n
            dstfield
        """
        # args
        try:
            srcfield = args[0]
            dstfield = args[1]
        except:
            raise RequiredArgs(Regrid.__call__.__doc__)

        # kwargs values
        zero_region = None
        if 'zero_region' in kwargs:
            zero_region = kwargs.get('zero_region')

        # call into the ctypes layer
        ESMP_FieldRegrid(srcfield, dstfield,
                         self.routehandle, zeroregion=zero_region)
        return dstfield

    def __del__(self):
        """
        Release the memory associated with a Regrid operation. \n
        Required Arguments: \n
            None \n
        Optional Arguments: \n
            None \n
        Returns: \n
            None \n
        """
        # call into the ctypes layer
        ESMP_FieldRegridRelease(self.routehandle)
