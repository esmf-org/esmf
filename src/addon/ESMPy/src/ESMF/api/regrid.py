# $Id$

"""
The Regrid API
"""

#### IMPORT LIBRARIES #########################################################
from ESMF.api.constants import *
from ESMF.interface.cbindings import *
from ESMF.util.decorators import initialize

from ESMF.api.esmpymanager import *
from ESMF.api.grid import *
from ESMF.api.mesh import *
from ESMF.api.field import *

#### Regrid class ##############################################################

class Regrid(object):

    # call RegridStore
    @initialize
    def __init__(self, srcfield, dstfield,
                 src_mask_values=None,
                 dst_mask_values=None,
                 regrid_method=None,
                 pole_method=None,
                 regrid_pole_npoints=None,
                 unmapped_action=None,
                 src_frac_field=None,
                 dst_frac_field=None):
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
            pole_method: specifies which type of artificial pole
                         to construct on the source Grid for regridding.\n
                Argument values are:\n
                    (default for regridmethod == RegridMethod.CONSERVE) PoleMethod.NONE\n
                    (default for regridmethod != RegridMethod.CONSERVE) PoleMethod.ALLAVG\n
                    PoleMethod.NPNTAVG\n
                    PoleMethod.TEETH\n
            regrid_pole_npoints: specifies how many points to average over 
                             if polemethod == PoleMethod.NPNTAVG\n
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
                                            polemethod=pole_method,
                                            regridPoleNPnts=regrid_pole_npoints,
                                            unmappedaction=unmapped_action,
                                            srcFracField=src_frac_field,
                                            dstFracField=dst_frac_field)
        
        self.srcfield = srcfield
        self.dstfield = dstfield
        self.src_mask_values = src_mask_values
        self.dst_mask_values = dst_mask_values
        self.regrid_method = regrid_method
        self.pole_method = pole_method
        self.regrid_pole_npoints = regrid_pole_npoints
        self.unmapped_action = unmapped_action
        self.src_frac_field = src_frac_field
        self.dst_frac_field = dst_frac_field

        # regist with atexit
        import atexit; atexit.register(self.__del__)
        self.__finalized = False

    def __call__(self, srcfield, dstfield,
                 zero_region=None):
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
        if not self.__finalized:
            ESMP_FieldRegridRelease(self.routehandle)
            self.__finalized = True


    def __repr__(self):
        """
        Return a string containing a printable representation of the object
        """
        string = ("Regrid:\n"
                  "    routehandle = %r\n"
                  "    src_mask_values = %r\n"
                  "    dst_mask_values = %r\n"
                  "    regrid_method = %r\n"
                  "    unmapped_action = %r\n"
                  "    src_frac_field = %r\n"
                  "    dst_frac_field = %r\n"
                  "    srcfield = %r\n"
                  "    dstfield = %r\n"
                  %
                  (self.routehandle,
                   self.src_mask_values,
                   self.dst_mask_values,
                   self.regrid_method,
                   self.unmapped_action,
                   self.src_frac_field,
                   self.dst_frac_field,
                   self.srcfield,
                   self.dstfield))

        return string
    
