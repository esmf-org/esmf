"""
Utilities for regridding with LocStreams
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

def create_locstream_16():
    if ESMF.pet_count() is not 1:
        raise ValueError("processor count must be 1 to use this function")

    locstream = ESMF.LocStream(16)

    locstream["ESMF:X"] = [0.0, 1.5, 2.5, 4.0, 0.0, 1.5, 2.5, 4.0, 0.0, 1.5, 2.5, 4.0, 0.0, 1.5, 2.5, 4.0]
    locstream["ESMF:Y"] = [0.0, 0.0, 0.0, 0.0, 1.5, 1.5, 1.5, 1.5, 2.5, 2.5, 2.5, 2.5, 4.0, 4.0, 4.0, 4.0]

    return locstream

def create_locstream_16_parallel():
    if ESMF.pet_count() is not 4:
        raise ValueError("processor count must be 4 to use this function")

    if ESMF.local_pet() is 0:
        locstream = ESMF.LocStream(4)
        locstream["ESMF:X"] = [0.0, 1.5, 0.0, 1.5]
        locstream["ESMF:Y"] = [0.0, 0.0, 1.5, 1.5]
    elif ESMF.local_pet() is 1:
        locstream = ESMF.LocStream(4)
        locstream["ESMF:X"] = [2.5, 4.0, 2.5, 4.0]
        locstream["ESMF:Y"] = [0.0, 0.0, 1.5, 1.5]
    elif ESMF.local_pet() is 2:
        locstream = ESMF.LocStream(4)
        locstream["ESMF:X"] = [0.0, 1.5, 0.0, 1.5]
        locstream["ESMF:Y"] = [2.5, 2.5, 4.0, 4.0]
    elif ESMF.local_pet() is 3:
        locstream = ESMF.LocStream(4)
        locstream["ESMF:X"] = [2.5, 4.0, 2.5, 4.0]
        locstream["ESMF:Y"] = [2.5, 2.5, 4.0, 4.0]

    return locstream
