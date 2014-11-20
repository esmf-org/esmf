
"""
unit test file
"""

from ESMF import *
from ESMF.interface.cbindings import *
from ESMF.test.base import TestBase

import numpy as np

# TODO:
#   1) MaskedArray is transposed when built on esmf allocation of Grid coord buffer using Array
#      ** cannot replicate this with generic esmf allocated buffers
#   2) regridding fails when using Array for Grid coords

# TODO: fix the regrid testing
# TODO: test view casting
# TODO: understand the implications of Fortran ordering in reshape call

class TestMaskedArray(TestBase):
    class ctypesgrid(object):
        def __init__(self, maxindex):
            '''
            :param self: testgrid object
            :param maxindex: maxindex of the grid
            :type maxindex: np array with dtype = int32
            :return:
            '''
            self.struct = ESMP_GridStruct()
            self.maxindex = maxindex
            self.rank = len(maxindex)

    def get_maskedarray_info(self, dim=0):
        typekind = TypeKind.R8
        grid = Grid(np.array([100, 100]), coord_sys=CoordSys.CART,
                    coord_typekind=typekind, staggerloc=[StaggerLoc.CENTER])

        grid_row = grid.get_coords(0, staggerloc=StaggerLoc.CENTER)
        grid_col = grid.get_coords(1, staggerloc=StaggerLoc.CENTER)

        row = np.random.rand(100, 100)
        col = np.random.rand(100, 100)

        grid_row[:] = row
        grid_col[:] = col

        data = ESMP_GridGetCoordPtr(grid, dim)
        lbounds, ubounds = ESMP_GridGetCoordBounds(grid)

        mask = [False]*reduce(mul, ubounds-lbounds)

        return data, mask, typekind, lbounds, ubounds, grid

    def get_array(self):
        data, mask, tk, lb, ub, grid = self.get_maskedarray_info()
        esmpyarray = MaskedArray(data, mask, tk, ub-lb)
        return esmpyarray

    def make_maskedarray(self, array, type=TypeKind.R8):
        '''
        :param self: TestMaskedArray class type
        :param array: maxindices of a 2- or 3d array
        :type array: np.array of dtype=np.int32
        :param type: the type of the esmf buffer
        :type type: ESMF.TypeKind
        '''
        # turn on debuggin and initialize the cbindings calls
        Manager(logkind=LogKind.MULTI, debug=True)

        # create a ctypes grid object to hold pointer and other info for ctypes layer
        if array.dtype is not np.int32:
            array = np.array(array, dtype=np.int32)
        esmfalloc = self.ctypesgrid(array)

        # create an esmf data allocation to test numpy array with
        esmfalloc.struct = ESMP_GridCreateNoPeriDim(esmfalloc.maxindex,
                                                    coordSys=CoordSys.CART,
                                                    coordTypeKind=type)
        ESMP_GridAddCoord(esmfalloc)
        dataptr = ESMP_GridGetCoordPtr(esmfalloc, 0)
        lb, ub = ESMP_GridGetCoordBounds(esmfalloc)

        return dataptr, lb, ub

    def test_del(self):
        self.esmpyarray = self.get_array()
        del(self.esmpyarray)
        assert(not hasattr(self, 'esmpyarray'))

    def test_copy(self):
        esmpyarray = self.get_array()
        esmpyarray2 = esmpyarray
        self.assertNumpyAll(esmpyarray, esmpyarray2)
        assert(np.may_share_memory(esmpyarray, esmpyarray2))

    def test_reshape(self):
        data, mask, tk, lb, ub, grid = self.get_maskedarray_info()
        esmpyarray = MaskedArray(data, mask, tk, ub-lb)

        # test reshape
        self.assertNumpyAll(np.array(esmpyarray.shape, dtype=np.int32),
                            np.array(ub - lb, dtype=np.int32))

    def test_slice(self):
        data, mask, tk, lb, ub, grid = self.get_maskedarray_info()
        esmpyarray = MaskedArray(data, mask, tk, ub-lb)

        # slice
        esmpyarrayslice = esmpyarray[:, 0]

        # test slice
        assert (esmpyarrayslice.shape == (ub[0] - lb[0]))

    def test_slice2(self):

        dataptr, lb, ub = self.make_maskedarray(np.array([10, 10], dtype=np.int32))

        array0 = MaskedArray(dataptr, None, TypeKind.R8, ub-lb)
        vals = np.random.rand(10, 10)
        array0.data[:] = vals

        self.assertEqual(array0.shape, (10,10))
        self.assertEqual(array0[1,1], vals[1,1])

        res = array0 * 2
        self.assertTrue(np.all(vals * 2 == res))




    def test_mul(self):
        esmpyarray1 = self.get_array()
        esmpyarray2 = self.get_array()
        esmpyarray3 = self.get_array()

        self.assertNumpyAll(esmpyarray1*5, 5*esmpyarray1)
        self.assertNumpyAll(esmpyarray1*esmpyarray2, esmpyarray2*esmpyarray1)
        # todo: associate and distributive properties have floating point errors?
        self.assertNumpyAllClose((esmpyarray1*(esmpyarray2*esmpyarray3)),
                                 ((esmpyarray1*esmpyarray2) * esmpyarray3))
        self.assertNumpyAllClose((esmpyarray1*(esmpyarray2+esmpyarray3)),
                                 ((esmpyarray1*esmpyarray2)+
                                  (esmpyarray1*esmpyarray3)))

    def test_stress(self):
        for _ in range(100):
            grid = Grid(np.array([100, 100]), coord_sys=CoordSys.CART,
                        coord_typekind=TypeKind.R8, staggerloc=[StaggerLoc.CENTER])

            # get the coordinate pointers and set the coordinates
            grid_row = grid.get_coords(0, staggerloc=StaggerLoc.CENTER)
            grid_col = grid.get_coords(1, staggerloc=StaggerLoc.CENTER)

            row = np.random.rand(100, 100)
            col = np.random.rand(100, 100)

            grid_row[:] = row
            grid_col[:] = col

            data0 = ESMP_GridGetCoordPtr(grid, 0)
            data1 = ESMP_GridGetCoordPtr(grid, 1)
            lb, ub = ESMP_GridGetCoordBounds(grid)

            mask = [False] * reduce(mul, ub - lb)

            esmpy_row = MaskedArray(data0, mask, TypeKind.R8, ub - lb)
            esmpy_col = MaskedArray(data1, mask, TypeKind.R8, ub - lb)

            self.assertNumpyAll(row, np.array(esmpy_row))
            self.assertNumpyAll(col, np.array(esmpy_col))

    def test_doublebuffer(self):

        typekind = TypeKind.R8
        dataptr, lb, ub = self.make_maskedarray(np.array([100,100], dtype=np.int32))

        # create ESMPy Arrays using ESMF data allocations
        array0 = MaskedArray(dataptr, None, typekind, ub-lb)

        vals = np.random.rand(100, 100)
        array0.data[:] = vals

        array1 = MaskedArray(dataptr, None, typekind, ub-lb)
        array2 = MaskedArray(dataptr, None, typekind, ub-lb)

        # assert that these numpy mangled esmf allocations are transposed
        self.assertNumpyAll(array0, array1)
        self.assertNumpyAll(array0, array2)
        self.assertNumpyAll(array1, array2)


    def test_new(self):
        data0, mask, tk, lb1, ub1, grid = self.get_maskedarray_info()
        esmpyarray0 = MaskedArray(data0, mask, tk, ub1 - lb1)
        data1 = ESMP_GridGetCoordPtr(grid, 1)
        lb1, ub1 = ESMP_GridGetCoordBounds(grid)
        esmpyarray1 = MaskedArray(data1, mask, tk, ub1 - lb1)
        self.assertNumpyAllClose(np.array(esmpyarray0),
                                 np.array(grid.coords[StaggerLoc.CENTER][0]))
        self.assertNumpyAllClose(np.array(esmpyarray1),
                                 np.array(grid.coords[StaggerLoc.CENTER][1]))

    def test_ownership(self):
        data0, mask, tk, lb0, ub0, grid = self.get_maskedarray_info(dim=0)

        esmpyarray0 = MaskedArray(data0, mask, tk, ub0 - lb0)

        # don't call get_array_info again or it will reset the grid!!
        data1 = ESMP_GridGetCoordPtr(grid, 1)
        lb1, ub1 = ESMP_GridGetCoordBounds(grid)

        esmpyarray1 = MaskedArray(data1, mask, tk, ub1 - lb1)

        coords0 = grid.get_coords(0, staggerloc=StaggerLoc.CENTER)
        coords1 = grid.get_coords(1, staggerloc=StaggerLoc.CENTER)

        # test that the data has the same values and sizes for each grid dimension
        self.assertNumpyAll(np.array(esmpyarray0), np.array(coords0))
        self.assertNumpyAll(np.array(esmpyarray1), np.array(coords1))

        # test ownership
        assert (np.may_share_memory(esmpyarray0, coords0))
        assert (np.may_share_memory(esmpyarray1, coords1))

# class TestArray(TestBase):
#
#     class ctypesgrid(object):
#         def __init__(self, maxindex):
#             '''
#             :param self: testgrid object
#             :param maxindex: maxindex of the grid
#             :type maxindex: np array with dtype = int32
#             :return:
#             '''
#             self.struct = ESMP_GridStruct()
#             self.maxindex = maxindex
#             self.rank = len(maxindex)
#
#     def get_array_info(self, dim=0):
#         typekind = TypeKind.R8
#         grid = Grid(np.array([100, 100]), coord_sys=CoordSys.CART,
#                     coord_typekind=typekind, staggerloc=[StaggerLoc.CENTER])
#
#         grid_row = grid.get_coords(0, staggerloc=StaggerLoc.CENTER)
#         grid_col = grid.get_coords(1, staggerloc=StaggerLoc.CENTER)
#
#         row = np.random.rand(100, 100)
#         col = np.random.rand(100, 100)
#
#         grid_row[:] = row
#         grid_col[:] = col
#
#         data = ESMP_GridGetCoordPtr(grid, dim)
#         lbounds, ubounds = ESMP_GridGetCoordBounds(grid)
#
#         return data, typekind, lbounds, ubounds, grid
#
#     def get_array(self):
#         data, tk, lb, ub, grid = self.get_array_info()
#         esmpyarray = Array(data, tk, ub - lb)
#         return esmpyarray
#
#     def test_new(self):
#         data0, tk, lb0, ub0, grid = self.get_array_info()
#         esmpyarray0 = Array(data0, tk, ub0 - lb0)
#         # ###esmpyarray1 = Array(data0, tk, ub0-lb0)
#         data1 = ESMP_GridGetCoordPtr(grid, 1)
#         lb1, ub1 = ESMP_GridGetCoordBounds(grid)
#         esmpyarray1 = Array(data1, tk, ub1 - lb1)
#         self.assertNumpyAll(np.array(esmpyarray0),
#                             np.array(grid.coords[StaggerLoc.CENTER][0]))
#         self.assertNumpyAll(np.array(esmpyarray1),
#                             np.array(grid.coords[StaggerLoc.CENTER][1]))
#
#         self.assertNumpyAll(np.array(esmpyarray0),
#                             np.array(grid.coords[StaggerLoc.CENTER][0]))
#         self.assertNumpyAll(np.array(esmpyarray1),
#                             np.array(grid.coords[StaggerLoc.CENTER][1]))
#
#
#     def test_ownership(self):
#         data0, tk, lb0, ub0, grid = self.get_array_info(dim=0)
#
#         esmpyarray0 = Array(data0, tk, ub0 - lb0)
#
#         # don't call get_array_info again or it will reset the grid!!
#         data1 = ESMP_GridGetCoordPtr(grid, 1)
#         lb1, ub1 = ESMP_GridGetCoordBounds(grid)
#
#         esmpyarray1 = Array(data1, tk, ub1 - lb1)
#
#         coords0 = grid.get_coords(0, staggerloc=StaggerLoc.CENTER)
#         coords1 = grid.get_coords(1, staggerloc=StaggerLoc.CENTER)
#
#         # test that the data has the same values and sizes for each grid dimension
#         self.assertNumpyAll(np.array(esmpyarray0), np.array(coords0))
#         self.assertNumpyAll(np.array(esmpyarray1), np.array(coords1))
#
#         # test ownership
#         assert (np.may_share_memory(esmpyarray0, coords0))
#         assert (np.may_share_memory(esmpyarray1, coords1))
#         assert (ct.addressof(esmpyarray0.contents) == ct.addressof(coords0.contents))
#         assert (ct.addressof(esmpyarray1.contents) == ct.addressof(coords1.contents))
#
#
#     def test_del(self):
#         self.esmpyarray = self.get_array()
#         del(self.esmpyarray)
#         assert(not hasattr(self, 'esmpyarray'))
#
#     def test_copy(self):
#         esmpyarray = self.get_array()
#         esmpyarray2 = esmpyarray
#         self.assertNumpyAll(esmpyarray, esmpyarray2)
#         assert(np.may_share_memory(esmpyarray, esmpyarray2))
#
#     def test_reshape(self):
#         data, tk, lb, ub, grid = self.get_array_info()
#         esmpyarray = Array(data, tk, ub-lb)
#
#         # test reshape
#         self.assertNumpyAll(np.array(esmpyarray.shape, dtype=np.int32), np.array(ub - lb, dtype=np.int32))
#
#     def test_slice(self):
#         data, tk, lb, ub, grid = self.get_array_info()
#         esmpyarray = Array(data, tk, ub-lb)
#
#         # slice
#         esmpyarrayslice = esmpyarray[:, 0]
#
#         # test slice
#         assert (esmpyarrayslice.shape == (ub[0] - lb[0]))
#
#         #TODO: can I test types here?
#
#     def test_mul(self):
#         esmpyarray1 = self.get_array()
#         esmpyarray2 = self.get_array()
#         esmpyarray3 = self.get_array()
#
#         self.assertNumpyAll(esmpyarray1*5, 5*esmpyarray1)
#         self.assertNumpyAll(esmpyarray1*esmpyarray2, esmpyarray2*esmpyarray1)
#         # todo: associate and distributive properties have floating point errors?
#         self.assertNumpyAllClose((esmpyarray1*(esmpyarray2*esmpyarray3)), ((esmpyarray1*esmpyarray2) * esmpyarray3))
#         self.assertNumpyAllClose((esmpyarray1*(esmpyarray2+esmpyarray3)), ((esmpyarray1*esmpyarray2) + (esmpyarray1*esmpyarray3)))
#
#     def test_stress(self):
#         for _ in range(100):
#             grid = Grid(np.array([100, 100]), coord_sys=CoordSys.CART,
#                                  coord_typekind=TypeKind.R8, staggerloc=[StaggerLoc.CENTER])
#
#             # get the coordinate pointers and set the coordinates
#             grid_row = grid.get_coords(0, staggerloc=StaggerLoc.CENTER)
#             grid_col = grid.get_coords(1, staggerloc=StaggerLoc.CENTER)
#
#             row = np.random.rand(100, 100)
#             col = np.random.rand(100, 100)
#
#             grid_row[:] = row
#             grid_col[:] = col
#
#             data0 = ESMP_GridGetCoordPtr(grid, 0)
#             data1 = ESMP_GridGetCoordPtr(grid, 1)
#             lb, ub = ESMP_GridGetCoordBounds(grid)
#
#             esmpy_row = Array(data0, TypeKind.R8, ub-lb)
#             esmpy_col = Array(data1, TypeKind.R8, ub-lb)
#
#             self.assertNumpyAll(row, np.array(esmpy_row))
#             self.assertNumpyAll(col, np.array(esmpy_col))
#
#     def test_doublebuffer(self):
#         # turn on debuggin and initialize the cbindings calls
#         Manager(logkind=LogKind.MULTI, debug=True)
#         typekind = TypeKind.R8
#
#         # create a ctypes grid object to hold pointer and other info for ctypes layer
#         esmfalloc = TestArray.ctypesgrid(np.array([100, 100], dtype=np.int32))
#
#         # create an esmf data allocation to test numpy array with
#         esmfalloc.struct = ESMP_GridCreateNoPeriDim(esmfalloc.maxindex,
#                                                     coordSys=CoordSys.CART,
#                                                     coordTypeKind=typekind)
#         ESMP_GridAddCoord(esmfalloc)
#         dataptr0 = ESMP_GridGetCoordPtr(esmfalloc, 0)
#         dataptr1 = ESMP_GridGetCoordPtr(esmfalloc, 1)
#         lb, ub = ESMP_GridGetCoordBounds(esmfalloc)
#
#         # create ESMPy Arrays using ESMF data allocations
#         array0a = Array(dataptr0, typekind, ub - lb)
#         # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         # # find the size of the local coordinates
#         # shape = ub - lb
#         # size = reduce(mul, shape)
#         #
#         # # create a numpy array to point to the ESMF data allocation
#         # buffer0 = np.core.multiarray.int_asbuffer(
#         #     ct.addressof(dataptr0.contents),
#         #     np.dtype(constants._ESMF2PythonType[typekind]).itemsize * size)
#         # npdata0 = np.frombuffer(buffer0, constants._ESMF2PythonType[typekind])
#         #
#         # npdata0 = npdata0.reshape(shape, order='F')
#         #
#         # # overwrite the gridCoordP variable to work around bug for now
#         # array0a = npdata0.view()
#         # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#         array1a = MaskedArray(dataptr1, None, typekind, ub - lb)
#         # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         # # find the size of the local coordinates
#         # shape = ub - lb
#         # size = reduce(mul, shape)
#         #
#         # # create a numpy array to point to the ESMF data allocation
#         # buffer1 = np.core.multiarray.int_asbuffer(
#         #     ct.addressof(dataptr1.contents),
#         #     np.dtype(constants._ESMF2PythonType[typekind]).itemsize * size)
#         # npdata1 = np.frombuffer(buffer1, constants._ESMF2PythonType[typekind])
#         #
#         # npdata1 = npdata1.reshape(shape, order='F')
#         #
#         # # overwrite the gridCoordP variable to work around bug for now
#         # array1a = npdata1.view()
#         # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#         row = np.random.rand(100, 100)
#         col = np.random.rand(100, 100)
#         array0a[:] = row
#         array1a.data[:] = col
#
#         array0b = Array(dataptr0, typekind, ub - lb)
#         array0c = MaskedArray(dataptr0, None, typekind, ub - lb)
#
#         array1b = MaskedArray(dataptr1, None, typekind, ub - lb)
#         array1c = Array(dataptr1,typekind, ub - lb)
#
#         # assert that these numpy mangled esmf allocations are transposed
#         self.assertNumpyAll(np.array(array0a), np.array(array0b))                     # pass
#         self.assertNumpyAll(np.array(array0a), np.array(array0c).transpose()) # fail
#         self.assertNumpyAll(np.array(array0b), np.array(array0c).transpose()) # fail
#         self.assertNumpyAll(np.array(array1a), np.array(array1b))                     # pass
#         self.assertNumpyAll(np.array(array1a), np.array(array1c).transpose()) # fail
#         self.assertNumpyAll(np.array(array1b), np.array(array1c).transpose()) # fail