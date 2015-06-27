# $Id$

"""
The Array utility class
"""

#### IMPORT LIBRARIES #########################################################
import ESMF.api.constants as constants
from operator import mul
import numpy as np
import numpy.ma as ma
import ctypes as ct

def esmf_array(data, dtype, shape):
    '''
    :param data: buffer of fortran allocated ESMF array
    :type data: ctypes void_p
    :param dtype: the type of the esmf buffer
    :type dtype: ESMF.TypeKind
    :param shape: N-D Python shape corresponding to 1D ESMF allocation
    :type shape: list or tuple
    :return: numpy array representing the data with dtype and shape
    '''
    # find the size of the local coordinates
    size = reduce(mul, shape)

    # create a numpy array to point to the ESMF data allocation
    buffer = np.core.multiarray.int_asbuffer(
        ct.addressof(data.contents),
        np.dtype(constants._ESMF2PythonType[dtype]).itemsize * size)
    esmfarray = np.frombuffer(buffer, constants._ESMF2PythonType[dtype])

    esmfarray = esmfarray.reshape(shape, order='F')

    return esmfarray

def esmf_array1d(data, dtype, size):
    '''
    :param data: buffer of fortran allocated ESMF array
    :type data: ctypes void_p
    :param dtype: the type of the esmf buffer
    :type dtype: ESMF.TypeKind
    :param size: size of the the 1D ESMF allocation
    :type size: integer
    :return: numpy array representing the data with dtype and shape
    '''
    # create a numpy array to point to the ESMF data allocation
    buffer = np.core.multiarray.int_asbuffer(
        ct.addressof(data.contents),
        np.dtype(constants._ESMF2PythonType[dtype]).itemsize * size)
    esmfarray = np.frombuffer(buffer, constants._ESMF2PythonType[dtype])

    return esmfarray

class MaskedArray(ma.MaskedArray):

    def __new__(cls, data, mask, dtype, shape):
        '''
        :param cls: MaskedArray class type
        :param data: buffer of fortran allocated ESMF array
        :type data: ctypes void_p
        :param mask: mask corresponding to the fortran allocated ESMF array
        :type mask: list
        :param dtype: the type of the esmf buffer
        :type dtype: ESMF.TypeKind
        :param shape: N-D Python shape corresponding to 1D ESMF allocation
        :type shape: list or tuple
        :return: MaskedArray object

        :attribute contents: esmf array pointer
        '''
        # find the size of the local coordinates
        size = reduce(mul, shape)

        # create a numpy array to point to the ESMF data allocation
        buffer = np.core.multiarray.int_asbuffer(
            ct.addressof(data.contents),
            np.dtype(constants._ESMF2PythonType[dtype]).itemsize * size)
        npdata = np.frombuffer(buffer, constants._ESMF2PythonType[dtype])

        # TODO: the fortran order here will re-reindex data that has already been reindexed, causing the new and ownership tests to fail
        npdata = npdata.reshape(shape, order='F')

        if mask is None: mamask = [False]*size
        else: mamask = mask

        # create the new Field instance
        obj = super(MaskedArray, cls).__new__(cls, data=npdata, mask=mamask,
                                              dtype=constants._ESMF2PythonType[dtype])

        # save objectwide metadata
        obj.contents = data.contents

        return obj

    def __array_finalize__(self, obj):
        if obj is None: return
        # set instance metadata
        self.contents = getattr(obj, 'contents', None)
        super(MaskedArray, self).__array_finalize__(obj)

class Array1D(np.ndarray):

    def __new__(cls, data, dtype, size):
        '''
        :param cls: Array class type
        :param data: buffer of fortran allocated ESMF array
        :type data: ctypes void_p
        :param dtype: the type of the esmf buffer
        :type dtype: ESMF.TypeKind
        :param size: size of the the 1D ESMF allocation
        :type size: integer
        :return: Array object

        :attribute contents: esmf array pointer
        '''

        # create a numpy array to point to the ESMF data allocation
        buffer = np.core.multiarray.int_asbuffer(
            ct.addressof(data.contents),
            np.dtype(constants._ESMF2PythonType[dtype]).itemsize * size)
        npdata = np.frombuffer(buffer, constants._ESMF2PythonType[dtype])

        # create the new Field instance
        obj = super(Array1D, cls).__new__(cls, size,
                                         dtype=constants._ESMF2PythonType[dtype],
                                         buffer=npdata)
        # save objectwide metadata
        obj.contents = data.contents

        return obj

    def __array_finalize__(self, obj):
        if obj is None: return
        # set instance metadata
        self.contents = getattr(obj, 'contents', None)
        super(Array1D, self).__array_finalize__(obj)

# class Array(np.ndarray):
#
#     def __new__(cls, data, dtype, shape):
#         '''
#         :param cls: Array class type
#         :param data: buffer of fortran allocated ESMF array
#         :type data: ctypes void_p
#         :param dtype: the type of the esmf buffer
#         :type dtype: ESMF.TypeKind
#         :param shape: N-D Python shape corresponding to 1D ESMF allocation
#         :type shape: list or tuple
#         :return: Array object
#
#         :attribute contents: esmf array pointer
#         '''
#         # find the size of the local coordinates
#         size = reduce(mul, shape)
#
#         # create a numpy array to point to the ESMF data allocation
#         buffer = np.core.multiarray.int_asbuffer(
#             ct.addressof(data.contents),
#             np.dtype(constants._ESMF2PythonType[dtype]).itemsize * size)
#         npdata = np.frombuffer(buffer, constants._ESMF2PythonType[dtype])
#
#         npdata = npdata.reshape(shape, order='F')
#
#         # create the new Field instance
#         obj = super(Array, cls).__new__(cls, tuple(shape),
#                                          dtype=constants._ESMF2PythonType[dtype],
#                                          buffer=npdata)
#
#         # save objectwide metadata
#         obj.contents = data.contents
#
#         return obj
#
#     def __array_finalize__(self, obj):
#         if obj is None: return
#         # set instance metadata
#         self.contents = getattr(obj, 'contents', None)
#
#     def __array_wrap__(self, out_arr):
#         return np.ndarray.__array_wrap__(self, out_arr)
#
