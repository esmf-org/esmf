"""
Utilities for regridding with LocStreams
"""

import sys

try:
    import numpy as np
except:
    raise ImportError('The Numpy library cannot be found!')

try:
    import esmpy
except:
    raise ImportError('The ESMF library cannot be found!')

pi = 3.14159
deg_rad = pi

def create_locstream_16(domask=False):
    """
    :param domask: a boolean to tell whether or not to add a mask
    :return: LocStream
    """
    if esmpy.pet_count() != 1:
        raise ValueError("processor count must be 1 to use this function")

    locstream = esmpy.LocStream(16)

    locstream["ESMF:X"] = [0.0, 1.5, 2.5, 4.0, 0.0, 1.5, 2.5, 4.0, 0.0, 1.5, 2.5, 4.0, 0.0, 1.5, 2.5, 4.0]
    locstream["ESMF:Y"] = [0.0, 0.0, 0.0, 0.0, 1.5, 1.5, 1.5, 1.5, 2.5, 2.5, 2.5, 2.5, 4.0, 4.0, 4.0, 4.0]
    if domask:
        locstream["ESMF:Mask"] = [1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]

    return locstream

def create_locstream_16_parallel(domask=False):
    """
    :param domask: a boolean to tell whether or not to add a mask
    :return: LocStream
    """
    if esmpy.pet_count() != 4:
        raise ValueError("processor count must be 4 to use this function")

    locstream = None
    if esmpy.local_pet() == 0:
        locstream = esmpy.LocStream(4)
        locstream["ESMF:X"] = [0.0, 1.5, 0.0, 1.5]
        locstream["ESMF:Y"] = [0.0, 0.0, 1.5, 1.5]
        if domask:
            locstream["ESMF:Mask"] = [1, 0, 0, 1]
    elif esmpy.local_pet() == 1:
        locstream = esmpy.LocStream(4)
        locstream["ESMF:X"] = [2.5, 4.0, 2.5, 4.0]
        locstream["ESMF:Y"] = [0.0, 0.0, 1.5, 1.5]
        if domask:
            locstream["ESMF:Mask"] = [1, 1, 1, 1]
    elif esmpy.local_pet() == 2:
        locstream = esmpy.LocStream(4)
        locstream["ESMF:X"] = [0.0, 1.5, 0.0, 1.5]
        locstream["ESMF:Y"] = [2.5, 2.5, 4.0, 4.0]
        if domask:
            locstream["ESMF:Mask"] = [1, 1, 1, 1]
    elif esmpy.local_pet() == 3:
        locstream = esmpy.LocStream(4)
        locstream["ESMF:X"] = [2.5, 4.0, 2.5, 4.0]
        locstream["ESMF:Y"] = [2.5, 2.5, 4.0, 4.0]
        if domask:
            locstream["ESMF:Mask"] = [1, 1, 1, 1]

    return locstream

def create_locstream_spherical_16(coord_sys=esmpy.CoordSys.SPH_DEG, domask=False):
    """
    :param coord_sys: the coordinate system of the LocStream
    :param domask: a boolean to tell whether or not to add a mask
    :return: LocStream
    """
    if esmpy.pet_count() != 1:
        raise ValueError("processor count must be 1 to use this function")

    locstream = esmpy.LocStream(16, coord_sys=coord_sys)

    deg_rad = pi
    if coord_sys == esmpy.CoordSys.SPH_DEG:
        deg_rad = 180

    locstream["ESMF:Lon"] = [0.2*deg_rad, 0.5*deg_rad, 1.5*deg_rad, 1.8*deg_rad, 0.2*deg_rad, 0.5*deg_rad, 1.5*deg_rad, 1.8*deg_rad, 0.2*deg_rad, 0.5*deg_rad, 1.5*deg_rad, 1.8*deg_rad, 0.2*deg_rad, 0.5*deg_rad, 1.5*deg_rad, 1.8*deg_rad]
    locstream["ESMF:Lat"] = [-0.4*deg_rad, -0.4*deg_rad, -0.4*deg_rad, -0.4*deg_rad, -0.25*deg_rad, -0.25*deg_rad, -0.25*deg_rad, -0.25*deg_rad, 0.25*deg_rad, 0.25*deg_rad, 0.25*deg_rad, 0.25*deg_rad, 0.4*deg_rad, 0.4*deg_rad, 0.4*deg_rad, 0.4*deg_rad]
    if domask:
        locstream["ESMF:Mask"] = np.array([1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], dtype=np.int32)

    return locstream

def create_locstream_spherical_16_parallel(coord_sys=esmpy.CoordSys.SPH_DEG, domask=False):
    """
    :param coord_sys: the coordinate system of the LocStream
    :param domask: a boolean to tell whether or not to add a mask
    :return: LocStream
    """
    if esmpy.pet_count() != 4:
        raise ValueError("processor count must be 4 to use this function")

    deg_rad = pi
    if coord_sys == esmpy.CoordSys.SPH_DEG:
        deg_rad = 180.0

    locstream = None
    if esmpy.local_pet() == 0:
        locstream = esmpy.LocStream(4, coord_sys=coord_sys)
        locstream["ESMF:Lon"] = [0.2*deg_rad, 0.5*deg_rad, 0.2*deg_rad, 0.5*deg_rad]
        locstream["ESMF:Lat"] = [-0.4*deg_rad, -0.4*deg_rad, -0.25*deg_rad, -0.25*deg_rad]
        if domask:
            locstream["ESMF:Mask"] = np.array([1, 0, 1, 1], dtype=np.int32)
    elif esmpy.local_pet() == 1:
        locstream = esmpy.LocStream(4, coord_sys=coord_sys)
        locstream["ESMF:Lon"] = [1.5*deg_rad, 1.8*deg_rad, 1.5*deg_rad, 1.8*deg_rad]
        locstream["ESMF:Lat"] = [-0.4*deg_rad, -0.4*deg_rad, -0.25*deg_rad, -0.25*deg_rad]
        if domask:
            locstream["ESMF:Mask"] = np.array([0, 1, 1, 1], dtype=np.int32)
    elif esmpy.local_pet() == 2:
        locstream = esmpy.LocStream(4, coord_sys=coord_sys)
        locstream["ESMF:Lon"] = [0.2*deg_rad, 0.5*deg_rad, 0.2*deg_rad, 0.5*deg_rad]
        locstream["ESMF:Lat"] = [0.25*deg_rad, 0.25*deg_rad, 0.4*deg_rad, 0.4*deg_rad]
        if domask:
            locstream["ESMF:Mask"] = np.array([1, 1, 1, 1], dtype=np.int32)
    elif esmpy.local_pet() == 3:
        locstream = esmpy.LocStream(4, coord_sys=coord_sys)
        locstream["ESMF:Lon"] = [1.5*deg_rad, 1.8*deg_rad, 1.5*deg_rad, 1.8*deg_rad]
        locstream["ESMF:Lat"] = [0.25*deg_rad, 0.25*deg_rad, 0.4*deg_rad, 0.4*deg_rad]
        if domask:
            locstream["ESMF:Mask"] = np.array([1, 1, 1, 1], dtype=np.int32)

    return locstream
