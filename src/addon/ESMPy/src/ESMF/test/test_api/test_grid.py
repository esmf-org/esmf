
"""
grid unit test file
"""

import ESMF
from ESMF import *
from ESMF.interface.cbindings import *
from ESMF.test.base import TestBase

import numpy as np
import os
import inspect

class TestGrid(TestBase):

    class ctypesgrid(object):
        def __init__(self, maxindex):
            '''
            :param self: testgrid object
            :param maxindex: maxindex of the grid
            :type maxindex: np array with dtype = int32
            :return:
            '''
            self.struct = ESMP_GridStruct(self)
            self.maxindex = maxindex
            self.rank = len(maxindex)

    def make_grid(self, dim=0):
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


    def test_grid_create(self):

        max_index = np.array([12, 20])

        grid = Grid(max_index)

        grid2 = Grid(max_index, num_peri_dims=0, coord_sys=CoordSys.SPH_RAD,
                     coord_typekind=TypeKind.R4)
        grid3 = Grid(max_index, num_peri_dims=1)

        grid4 = Grid(max_index, num_peri_dims=1, coord_sys=CoordSys.SPH_DEG,
                     coord_typekind=TypeKind.R4)
        grid5 = Grid(max_index, num_peri_dims=1, periodic_dim=1,
                     coord_sys=CoordSys.SPH_DEG, coord_typekind=TypeKind.R4)
        grid6 = Grid(max_index, num_peri_dims=1, periodic_dim=2, pole_dim=1,
                     coord_sys=CoordSys.SPH_DEG, coord_typekind=TypeKind.R4)

        del grid
        del grid2
        del grid3
        del grid4
        del grid5
        del grid6

    def test_grid_create_3D(self):

        max_index = np.array([12, 20, 37])

        grid = Grid(max_index)

        grid2 = Grid(max_index, num_peri_dims=0, coord_sys=CoordSys.SPH_RAD,
                     coord_typekind=TypeKind.R4)
        grid3 = Grid(max_index, num_peri_dims=1)
        grid4 = Grid(max_index, num_peri_dims=1, coord_sys=CoordSys.SPH_DEG,
                     coord_typekind=TypeKind.R4)

        del grid
        del grid2
        del grid3
        del grid4

    def test_grid_coords(self):

        max_index = np.array([12, 20])

        grid = Grid(max_index)

        # Add coordinates
        grid.add_coords(staggerloc=[StaggerLoc.CENTER])

        exLB = grid.lower_bounds[StaggerLoc.CENTER]
        exUB = grid.upper_bounds[StaggerLoc.CENTER]

        print "Grid Bounds:"
        print "    exclusiveLBounds = " + str(exLB)
        print type(exLB)
        print "    exclusiveUBounds = " + str(exUB)
        print exUB.dtype

        # get and set the coordinates
        [x, y] = [0, 1]
        gridCoord = grid.coords[StaggerLoc.CENTER]

        for i in xrange(gridCoord[x].shape[x]):
            gridCoord[x][i, :] = float(i)

        for j in xrange(gridCoord[y].shape[y]):
            gridCoord[y][:, j] = float(j)

        # validate the coordinates using an easily understood double loop
        gridXCoord_check = grid.get_coords(x)
        gridYCoord_check = grid.get_coords(y)

        correct = True
        for i in xrange(gridXCoord_check.shape[x]):
            for j in xrange(gridXCoord_check.shape[y]):
                assert(gridXCoord_check[i, j] == float(i))

        for i in xrange(gridYCoord_check.shape[x]):
            for j in xrange(gridYCoord_check.shape[y]):
                assert(gridYCoord_check[i, j] == float(j))

    def test_grid_coords_3D(self):

        max_index = np.array([10, 20, 30])

        grid = Grid(max_index, coord_sys=CoordSys.CART)

        # Add coordinates
        grid.add_coords(staggerloc=[StaggerLoc.CENTER])

        exLB = grid.lower_bounds[StaggerLoc.CENTER]
        exUB = grid.upper_bounds[StaggerLoc.CENTER]

        print "Grid Bounds:"
        print "    exclusiveLBounds = " + str(exLB)
        print type(exLB)
        print "    exclusiveUBounds = " + str(exUB)
        print exUB.dtype

        # get and set the coordinates
        [x, y, z] = [0, 1, 2]
        gridCoord = grid.coords[StaggerLoc.CENTER]

        for i in xrange(gridCoord[x].shape[x]):
            gridCoord[x][i, :, :] = float(i)

        for j in xrange(gridCoord[y].shape[y]):
            gridCoord[y][:, j, :] = float(j)

        for k in xrange(gridCoord[z].shape[z]):
            gridCoord[z][:, :, k] = float(k)

        # get the coordinate pointers and validate the coordinates
        gridXCoord_check = grid.get_coords(x)
        gridYCoord_check = grid.get_coords(y)
        gridZCoord_check = grid.get_coords(z)

        for i in xrange(gridXCoord_check.shape[x]):
            for j in xrange(gridXCoord_check.shape[y]):
                for k in xrange(gridXCoord_check.shape[z]):
                    assert(gridXCoord_check[i, j, k] == float(i))

        for i in xrange(gridYCoord_check.shape[x]):
            for j in xrange(gridYCoord_check.shape[y]):
                for k in xrange(gridYCoord_check.shape[z]):
                    assert(gridYCoord_check[i, j, k] == float(j))

        for i in xrange(gridZCoord_check.shape[x]):
            for j in xrange(gridZCoord_check.shape[y]):
                for k in xrange(gridZCoord_check.shape[z]):
                    assert (gridZCoord_check[i, j, k] == float(k))

    def test_grid_mask(self):

        max_index = np.array([12, 20])

        grid = Grid(max_index)

        # Add coordinates
        grid.add_coords(staggerloc=[StaggerLoc.CENTER])

        # Add Mask
        mask = grid.add_item(GridItem.MASK)

        [x, y] = [0, 1]
        for i in xrange(mask.shape[x]):
            for j in xrange(mask.shape[y]):
                if (i == 2.0):
                    mask[i, j] = 1
                else:
                    mask[i, j] = 0

        assert(all(mask[2, :] == [1]*mask.shape[y]))

    def test_grid_mask_3D(self):

        max_index = np.array([10, 20, 30])

        grid = Grid(max_index, coord_sys=CoordSys.CART)

        # Add coordinates
        grid.add_coords(staggerloc=[StaggerLoc.CENTER])

        # Add Mask
        mask = grid.add_item(GridItem.MASK)
        mask[...] = 1
        mask[:, 2, 4] = 0

        assert (np.all(mask[:, 2, 4] == [0]*mask.shape[0]))

    def test_grid_area(self):

        max_index = np.array([12, 20])

        grid = Grid(max_index)

        # Add coordinates
        grid.add_coords(staggerloc=[StaggerLoc.CENTER])
        exLB = grid.lower_bounds[StaggerLoc.CENTER]
        exUB = grid.upper_bounds[StaggerLoc.CENTER]

        # Add Areas
        area = grid.add_item(GridItem.AREA)

        area[...] = 1.0

        if pet_count() == 0:
            assert(np.all(area[...] == np.ones([12,20])))

    def test_grid_area_3D(self):

        max_index = np.array([10, 20, 30])

        grid = Grid(max_index, coord_sys=CoordSys.CART)

        # Add coordinates
        grid.add_coords(staggerloc=[StaggerLoc.CENTER])
        exLB = grid.lower_bounds[StaggerLoc.CENTER]
        exUB = grid.upper_bounds[StaggerLoc.CENTER]

        # Add Areas
        area = grid.add_item(GridItem.AREA)

        if pet_count() == 0:
            areavals = np.ones([10, 20, 30])*12
            area[:] = areavals
            assert(np.all(area[...] == 12*np.ones([10, 20, 30])))


    def test_grid_create_from_file_scrip(self):
        reg_decomp = [pet_count(), 1]
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            grid_from_file = Grid(filename=os.path.join(esmfdir, "test/data/T42_grid.nc"),
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp)
        except:
            raise NameError('grid_create_from_file_scrip failed!')

    def test_grid_create_from_file_scrip_decomp_balanced_balanced(self):
        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.BALANCED, DecompFlag.BALANCED],
                              dtype=np.int32)
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            grid_from_file = Grid(filename=os.path.join(esmfdir, "test/data/T42_grid.nc"),
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_balanced_balanced failed!')

    def test_grid_create_from_file_scrip_decomp_balanced_restfirst(self):
        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.BALANCED, DecompFlag.RESTFIRST],
                              dtype=np.int32)
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            grid_from_file = Grid(filename=os.path.join(esmfdir, "test/data/T42_grid.nc"),
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_balanced_restfirst failed!')

    def test_grid_create_from_file_scrip_decomp_balanced_restlast(self):
        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.BALANCED, DecompFlag.RESTLAST],
                              dtype=np.int32)
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            grid_from_file = Grid(filename=os.path.join(esmfdir, "test/data/T42_grid.nc"),
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_balanced_restlast failed!')

    @expected_failure
    def test_grid_create_from_file_scrip_decomp_balanced_cyclic(self):
        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.BALANCED, DecompFlag.CYCLIC],
                              dtype=np.int32)
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            grid_from_file = Grid(filename=os.path.join(esmfdir, "test/data/T42_grid.nc"),
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_balanced_cyclic failed!')

    def test_grid_create_from_file_scrip_decomp_restfirst_balanced(self):
        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.RESTFIRST, DecompFlag.BALANCED],
                              dtype=np.int32)
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            grid_from_file = Grid(filename=os.path.join(esmfdir, "test/data/T42_grid.nc"),
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_restfirst_balanced failed!')

    def test_grid_create_from_file_scrip_decomp_restfirst_restfirst(self):
        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.RESTFIRST, DecompFlag.RESTFIRST],
                              dtype=np.int32)
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            grid_from_file = Grid(filename=os.path.join(esmfdir, "test/data/T42_grid.nc"),
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_restfirst_restfirst failed!')

    def test_grid_create_from_file_scrip_decomp_restfirst_restlast(self):
        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.RESTFIRST, DecompFlag.RESTLAST],
                              dtype=np.int32)
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            grid_from_file = Grid(filename=os.path.join(esmfdir, "test/data/T42_grid.nc"),
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_restfirst_restlast failed!')

    @expected_failure
    def test_grid_create_from_file_scrip_decomp_restfirst_cyclic(self):
        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.RESTFIRST, DecompFlag.CYCLIC],
                              dtype=np.int32)
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            grid_from_file = Grid(filename=os.path.join(esmfdir, "test/data/T42_grid.nc"),
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_restfirst_cyclic failed!')

    def test_grid_create_from_file_scrip_decomp_restlast_balanced(self):
        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.RESTLAST, DecompFlag.BALANCED],
                              dtype=np.int32)
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            grid_from_file = Grid(filename=os.path.join(esmfdir, "test/data/T42_grid.nc"),
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_restlast_balanced failed!')

    def test_grid_create_from_file_scrip_decomp_restlast_restfirst(self):
        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.RESTLAST, DecompFlag.RESTFIRST],
                              dtype=np.int32)
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            grid_from_file = Grid(filename=os.path.join(esmfdir, "test/data/T42_grid.nc"),
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_restlast_restfirst failed!')

    def test_grid_create_from_file_scrip_decomp_restlast_restlast(self):
        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.RESTLAST, DecompFlag.RESTLAST],
                              dtype=np.int32)
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            grid_from_file = Grid(filename=os.path.join(esmfdir, "test/data/T42_grid.nc"),
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_restlast_restlast failed!')

    @expected_failure
    def test_grid_create_from_file_scrip_decomp_restlast_cyclic(self):
        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.RESTLAST, DecompFlag.CYCLIC],
                              dtype=np.int32)
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            grid_from_file = Grid(filename=os.path.join(esmfdir, "test/data/T42_grid.nc"),
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_restlast_cyclic failed!')

    @expected_failure
    def test_grid_create_from_file_scrip_decomp_cyclic_balanced(self):
        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.CYCLIC, DecompFlag.BALANCED],
                              dtype=np.int32)
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            grid_from_file = Grid(filename=os.path.join(esmfdir, "test/data/T42_grid.nc"),
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_cyclic_balanced failed!')

    @expected_failure
    def test_grid_create_from_file_scrip_decomp_cyclic_restfirst(self):
        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.CYCLIC, DecompFlag.RESTFIRST],
                              dtype=np.int32)
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            grid_from_file = Grid(filename=os.path.join(esmfdir, "test/data/T42_grid.nc"),
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_cyclic_restfirst failed!')

    @expected_failure
    def test_grid_create_from_file_scrip_decomp_cyclic_restlast(self):
        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.CYCLIC, DecompFlag.RESTLAST],
                              dtype=np.int32)
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            grid_from_file = Grid(filename=os.path.join(esmfdir, "test/data/T42_grid.nc"),
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_cyclic_restlast failed!')

    @expected_failure
    def test_grid_create_from_file_scrip_decomp_cyclic_cyclic(self):
        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.CYCLIC, DecompFlag.CYCLIC],
                              dtype=np.int32)
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            grid_from_file = Grid(filename=os.path.join(esmfdir, "test/data/T42_grid.nc"),
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_cyclic_cyclic failed!')

