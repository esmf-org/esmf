"""
field unit test file
"""

from ESMF import *
from ESMF.interface.cbindings import *
from ESMF.test.base import TestBase

import numpy as np

class TestField(TestBase):
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

    def make_maskedfield(self, array):
        '''
        :param self: TestMaskedArray class type
        :param array: maxindices of a 2- or 3d array
        :type array: np.array of dtype=np.int32
        '''

        grid = Grid(array, coord_sys=CoordSys.CART)

        mask = grid.add_item(GridItem.MASK)
        mask[:] = 1
        mask[0,1] = 0

        field = Field(grid, "name", ndbounds=[2,5], mask_values=[0])

        assert(np.all(field.mask[:,:,0,1] == True))

        return field

    def test_del(self):
        self.maskedfield = self.make_maskedfield(np.array([10,10], dtype=np.int32))
        del(self.maskedfield)
        assert(not hasattr(self, 'maskedfield'))
