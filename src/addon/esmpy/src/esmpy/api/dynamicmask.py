# $Id$

"""
The DynamicMask API
"""
from esmpy.api import constants
from esmpy.api.esmpymanager import *

class DynamicMask(object):

    @initialize
    def __init__(self, mask_type=None, handle_all_elements=None, src_mask_value=None, dst_mask_value=None):

        self._struct = ESMP_DynamicMaskPredefinedSetR8R8R8(masktype=mask_type, 
                       handleAllElements=handle_all_elements, 
                       srcMaskValue=src_mask_value,
                       dstMaskValue=dst_mask_value)

        import atexit; atexit.register(self.__del__)
        self._finalized = False

    @property
    def struct(self):
        return self._struct

    def __del__(self):
        self.destroy()

    def destroy(self):
        self._finalized = True
