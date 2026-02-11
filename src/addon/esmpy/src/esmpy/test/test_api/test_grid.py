
"""
grid unit test file
"""

import pytest
import numpy as np

import os

from esmpy import *
from esmpy.interface.cbindings import *
from esmpy.test.base import TestBase
from esmpy.api.constants import _ESMF_NETCDF
from esmpy.util.cache_data import DATA_DIR

class TestGrid(TestBase):
    
    # prefer TestBase.mg, but in this case required for test_exhaustive in pytest markers
    mg = Manager(debug=True)
    mg.test_exhaustive = False

    def examine_grid_attributes(self, grid):
        for de in range(grid.local_de_count):
            self.examine_grid_attributes_one_de(grid, de)

    def examine_grid_attributes_one_de(self, grid, localde):
        # ~~~~~~~~~~~~~~~~~~~~~~  STAGGER LOCATIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # grid.staggerloc returns a boolean list of the activated stagger locations
        assert (type(grid.staggerloc) is list)
        assert (len(grid.staggerloc) is 2 ** grid.rank)

        # ~~~~~~~~~~~~~~~~~~~~~~  SIZE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # grid.all_sizes[localde] returns a list of the local (processor specific) sizes
        # of grid coordinates in each stagger location
        assert (type(grid.all_sizes[localde]) is list)
        assert (len(grid.all_sizes[localde]) is 2 ** grid.rank)

        # ~~~~~~~~~~~~~~~~~~~~~~  BOUNDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # grid.all_lower_bounds[localde] or grid.all_upper_bounds[localde] returns a list
        # of numpy arrays containing the bounds of all coordinate dimensions in the grid
        assert (type(grid.all_lower_bounds[localde]) is list)
        assert (len(grid.all_lower_bounds[localde]) is 2 ** grid.rank)
        assert (type(grid.all_upper_bounds[localde]) is list)
        assert (len(grid.all_upper_bounds[localde]) is 2 ** grid.rank)

        # ~~~~~~~~~~~~~~~~~~~~~~  COORDINATES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # grid.all_coords[localde] returns a list of lists of numpy arrays for coordinates
        # of all stagger locations and coordinate dimensions
        assert (type(grid.all_coords[localde]) is list)
        assert (len(grid.all_coords[localde]) is 2 ** grid.rank)

        # ~~~~~~~~~~~~~~~~~~~~~~  MASK ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # grid.all_masks[localde] returns a list of numpy arrays containing the grid mask
        # at each stagger location
        assert (type(grid.all_masks[localde]) is list)
        assert (len(grid.all_masks[localde]) is 2 ** grid.rank)

        # ~~~~~~~~~~~~~~~~~~~~~~  AREA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # grid.all_areas[localde] returns a list of numpy arrays containing the grid cell
        # areas at each stagger location
        assert (type(grid.all_areas[localde]) is list)
        assert (len(grid.all_areas[localde]) is 2 ** grid.rank)

        # stagger specific attributes
        for i in range(len(grid.staggerloc)):
            if grid.staggerloc[i]:

                # ~~~~~~~~~~~~~~~~~~~~~~  SIZE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # grid.all_sizes[localde][i] returns a numpy array of the local (processor
                # specific) size of the grid coordinates at the i'th stagger location
                assert (type(grid.all_sizes[localde][i]) is np.ndarray)
                assert (grid.all_sizes[localde][i].shape == tuple([grid.rank]))
                assert np.array_equal(grid.all_sizes[localde][i],
                                      grid.all_upper_bounds[localde][i] - grid.all_lower_bounds[localde][i])

                # ~~~~~~~~~~~~~~~~~~~~~~  BOUNDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # grid.all_lower_bounds[localde][i] or grid.all_upper_bounds[localde][i]
                # returns a numpy array containing the bounds of all coordinate dimensions
                # at the i'th stagger location
                assert (type(grid.all_lower_bounds[localde][i] is np.ndarray))
                assert (grid.all_lower_bounds[localde][i].shape == tuple([grid.rank]))
                assert (type(grid.all_upper_bounds[localde][i] is np.ndarray))
                assert (grid.all_upper_bounds[localde][i].shape == tuple([grid.rank]))

                # ~~~~~~~~~~~~~~~~~~~~~~  COORDINATES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # grid.all_coords[localde][i] returns a list of length=rank containing
                # numpy arrays for each coordinate dimension
                assert (type(grid.all_coords[localde][i]) is list)
                assert (len(grid.all_coords[localde][i]) is grid.rank)

                # grid.all_coords[localde][i][j] returns a numpy array containing
                # coordinates of i'th stagger location and j'th coordinate dimension
                for j in range(grid.rank):
                    assert (type(grid.all_coords[localde][i][j] is np.ndarray))
                    assert (
                        grid.all_coords[localde][i][j].shape == tuple(np.array(grid.all_upper_bounds[localde][i]) - np.array(grid.all_lower_bounds[localde][i])))

                # ~~~~~~~~~~~~~~~~~~~~~~  MASK ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # grid.all_masks[localde][i] returns a numpy array containing the grid
                # mask at the i'th stagger location
                if not isinstance(grid.all_masks[localde][i], type(None)):
                    assert (type(grid.all_masks[localde][i]) is np.ndarray)
                    assert (
                        grid.all_masks[localde][i].shape == tuple(np.array(grid.all_upper_bounds[localde][i]) - np.array(grid.all_lower_bounds[localde][i])))

                    # Confirm that the Python data matches the internal ESMF data; if not,
                    # the linkage between the Python data and the internal data may not
                    # have been set up correctly
                    masks_from_esmf = ESMP_GridGetItem(grid, GridItem.MASK, staggerloc=i, localde=localde)
                    assert(np.array_equal(grid.all_masks[localde][i],
                                          ndarray_from_esmf(masks_from_esmf, TypeKind.I4, grid.all_sizes[localde][i])))

                # ~~~~~~~~~~~~~~~~~~~~~~  AREA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # grid.all_areas[localde][i] returns a numpy array containing the grid
                # cell areas at the i'th stagger location
                if not isinstance(grid.all_areas[localde][i], type(None)):
                    assert (type(grid.all_areas[localde][i]) is np.ndarray)
                    assert (
                        grid.all_areas[localde][i].shape == tuple(np.array(grid.all_upper_bounds[localde][i]) - np.array(grid.all_lower_bounds[localde][i])))

                    # Confirm that the Python data matches the internal ESMF data; if not,
                    # the linkage between the Python data and the internal data may not
                    # have been set up correctly
                    areas_from_esmf = ESMP_GridGetItem(grid, GridItem.AREA, staggerloc=i, localde=localde)
                    assert(np.array_equal(grid.all_areas[localde][i],
                                          ndarray_from_esmf(areas_from_esmf, TypeKind.R8, grid.all_sizes[localde][i])))

    def assert_expected_grid_shape(self, grid, staggerloc, expected_shape):
        """
        Assert that various properties of the given :class:`~esmpy.api.grid.Grid` at the
        given stagger location match the given expected shape.

        :param Grid grid: The Grid to check
        :param StaggerLoc staggerloc: The stagger location to check
        :param tuple expected_shape: The expected shape of the Grid
        """
        assert grid.coords[staggerloc][0].shape == expected_shape
        assert grid.upper_bounds[staggerloc].tolist() == list(expected_shape)
        assert grid.size[staggerloc].tolist() == list(expected_shape)

    def make_grid_2d(self):
        typekind = TypeKind.R8
        grid = Grid(np.array([100, 100]), coord_sys=CoordSys.CART,
                    coord_typekind=typekind, staggerloc=[StaggerLoc.CENTER])

        grid_row = grid.get_coords(0, staggerloc=StaggerLoc.CENTER)
        grid_col = grid.get_coords(1, staggerloc=StaggerLoc.CENTER)

        row = np.random.rand(*tuple(grid.upper_bounds[StaggerLoc.CENTER]-grid.lower_bounds[StaggerLoc.CENTER]))
        col = np.random.rand(*tuple(grid.upper_bounds[StaggerLoc.CENTER]-grid.lower_bounds[StaggerLoc.CENTER]))

        grid_row[:] = row
        grid_col[:] = col

        grid.add_item(GridItem.MASK)
        grid.add_item(GridItem.AREA)

        return grid

    def make_grid_3d(self):
        typekind = TypeKind.R8
        grid = Grid(np.array([100, 100, 100]), coord_sys=CoordSys.CART,
                    coord_typekind=typekind, staggerloc=[StaggerLoc.CENTER])

        grid_row = grid.get_coords(0, staggerloc=StaggerLoc.CENTER)
        grid_col = grid.get_coords(1, staggerloc=StaggerLoc.CENTER)
        grid_vrt = grid.get_coords(2, staggerloc=StaggerLoc.CENTER)

        row = np.random.rand(*tuple(grid.upper_bounds[StaggerLoc.CENTER]-grid.lower_bounds[StaggerLoc.CENTER]))
        col = np.random.rand(*tuple(grid.upper_bounds[StaggerLoc.CENTER]-grid.lower_bounds[StaggerLoc.CENTER]))
        vrt = np.random.rand(*tuple(grid.upper_bounds[StaggerLoc.CENTER]-grid.lower_bounds[StaggerLoc.CENTER]))

        grid_row[:] = row
        grid_col[:] = col
        grid_vrt[:] = vrt

        grid.add_item(GridItem.MASK)
        grid.add_item(GridItem.AREA)

        return grid

    def make_grid_periodic(self):
        lon = np.arange(0, 360, 360 / 100.)
        lat = np.linspace(90., -90., 100)

        assert lon.size == 100
        assert lat.size == 100

        grid = Grid(np.array([lon.size, lat.size], 'int32'),
                         num_peri_dims=1, staggerloc=[StaggerLoc.CENTER, StaggerLoc.CORNER])

        gridXCorner = grid.get_coords(0, staggerloc=StaggerLoc.CORNER)
        lon_par = lon[grid.lower_bounds[StaggerLoc.CORNER][0]:grid.upper_bounds[StaggerLoc.CORNER][0]]
        gridXCorner[...] = lon_par.reshape((lon_par.size, 1))

        gridYCorner = grid.get_coords(1, staggerloc=StaggerLoc.CORNER)
        lat_corner = np.linspace(90, -90, lat.size + 1)
        lat_par = lat_corner[grid.lower_bounds[StaggerLoc.CORNER][1]:grid.upper_bounds[StaggerLoc.CORNER][1]]
        gridYCorner[...] = lat_par.reshape((1, lat_par.size))

        offset_lon = 360. / lon.size / 2.
        lon -= offset_lon
        gridXCenter = grid.get_coords(0)
        lon_par = lon[grid.lower_bounds[StaggerLoc.CENTER][0]:grid.upper_bounds[StaggerLoc.CENTER][0]]
        gridXCenter[...] = lon_par.reshape((lon_par.size, 1))

        offset_lat = 180. / (lat.size) / 2.
        lat = np.linspace(90 - offset_lat, -90 + offset_lat, lat.size)
        gridYCenter = grid.get_coords(1)
        lat_par = lat[grid.lower_bounds[StaggerLoc.CENTER][1]:grid.upper_bounds[StaggerLoc.CENTER][1]]
        gridYCenter[...] = lat_par.reshape((1, lat_par.size))


        return grid, lon.size, lat.size

    def test_grid_attributes_2d(self):
        grid = self.make_grid_2d()
        self.examine_grid_attributes(grid)

    def test_grid_attributes_3d(self):
        grid = self.make_grid_3d()
        self.examine_grid_attributes(grid)

    def test_grid_periodic(self):
        grid,_,_ = self.make_grid_periodic()
        self.examine_grid_attributes(grid)

    @pytest.mark.skipif(mg.test_exhaustive==False, reason="only run in exhaustive mode")
    @pytest.mark.skipif(pet_count()!=1, reason="test must be run in serial")
    def test_grid_create_2d(self):
        keywords = dict(
            # periodic specifies all valid combos of [pole_kind, num_peri_dims, periodic_dim, pole_dim]
            pole_kind=[[PoleKind.NONE, PoleKind.NONE],
                      [PoleKind.NONE, PoleKind.MONOPOLE],
                      [PoleKind.NONE, PoleKind.BIPOLE],
                      [PoleKind.MONOPOLE, PoleKind.NONE],
                      [PoleKind.BIPOLE, PoleKind.NONE],
                      [PoleKind.MONOPOLE, PoleKind.BIPOLE],
                      [PoleKind.BIPOLE, PoleKind.MONOPOLE]],
            periodic=[[None, None, None], [None, None, 0], [None, None, 1],
                      [0, None, None], [0, None, 0], [0, None, 1],
                      [1, None, None], [1, 0, 1], [1, 1, 0]],
            staggerloc=[None, StaggerLoc.CENTER, StaggerLoc.EDGE1, StaggerLoc.EDGE2, StaggerLoc.CORNER],
            coord_sys=[None, CoordSys.CART, CoordSys.SPH_DEG, CoordSys.SPH_RAD],
            typekind=[None, TypeKind.I4, TypeKind.I8, TypeKind.R4, TypeKind.R8],
        )

        testcases = self.iter_product_keywords(keywords)
        fail = []
        for a in testcases:
            try:
                grid = Grid(np.array([12, 12]),
                            pole_kind=np.array(a.pole_kind),
                            num_peri_dims=a.periodic[0],
                            periodic_dim=a.periodic[1],
                            pole_dim=a.periodic[2],
                            coord_sys=a.coord_sys,
                            coord_typekind=a.typekind,
                            staggerloc=a.staggerloc)
                grid.add_item(GridItem.MASK)
                grid.add_item(GridItem.AREA)
                grid2 = grid[2:10, 4:7]
                self.examine_grid_attributes(grid)
                self.examine_grid_attributes(grid2)
                grid.destroy()
                grid2.destroy()
            except:
                fail += a

        if len(fail) > 0:
            raise ValueError(
                "The following combinations of Grid parameters failed to create a proper Grid: " + str(fail))

    @pytest.mark.skipif(mg.test_exhaustive==False, reason="only run in exhaustive mode")
    @pytest.mark.skipif(pet_count()!=1, reason="test must be run in serial")
    def test_grid_create_3d(self):
        keywords = dict(
            # periodic specifies all valid combos of [num_peri_dims, periodic_dim, pole_dim]
            periodic=[[None, None, None], [None, None, 0], [None, None, 1], [None, None, 2],
                      [0, None, None], [0, None, 0], [0, None, 1], [0, None, 2],
                      [1, None, None], [1, 0, 1], [1, 0, 2], [1, 1, 0], [1, 1, 2], [1, 2, 0], [1, 2, 1]],
            staggerloc=[None, StaggerLoc.CENTER_VCENTER, StaggerLoc.EDGE1_VCENTER, StaggerLoc.EDGE2_VCENTER,
                        StaggerLoc.CORNER_VCENTER, StaggerLoc.CENTER_VFACE, StaggerLoc.EDGE1_VFACE,
                        StaggerLoc.EDGE2_VFACE, StaggerLoc.CORNER_VFACE],
            coord_sys=[None, CoordSys.CART, CoordSys.SPH_DEG, CoordSys.SPH_RAD],
            typekind=[None, TypeKind.I4, TypeKind.I8, TypeKind.R4, TypeKind.R8])

        testcases = self.iter_product_keywords(keywords)
        fail = []
        for a in testcases:
            try:
                grid = Grid(np.array([12, 12, 12]),
                            num_peri_dims=a.periodic[0], periodic_dim=a.periodic[1], pole_dim=a.periodic[2],
                            coord_sys=a.coord_sys, coord_typekind=a.typekind, staggerloc=a.staggerloc)
                grid.add_item(GridItem.MASK)
                grid.add_item(GridItem.AREA)
                grid2 = grid[2:10, 4:7, 1:9]
                self.examine_grid_attributes(grid)
                self.examine_grid_attributes(grid2)
                grid.destroy()
                grid2.destroy()
            except:
                fail += a

        if len(fail) > 0:
            raise ValueError(
                "The following combinations of Grid parameters failed to create a proper Grid: " + str(fail))

    def test_grid_create_cubed_sphere(self):
        """
        Test creation of a cubed-sphere grid with the default decomposition.
        """
        # keywords = dict(
        # periodic specifies all valid combos of [num_peri_dims, periodic_dim, pole_dim]
        # periodic=[[None, None, None], [None, None, 0], [None, None, 1], [None, None, 2],
        #           [0, None, None], [0, None, 0], [0, None, 1], [0, None, 2],
        #           [1, None, None], [1, 0, 1], [1, 0, 2], [1, 1, 0], [1, 1, 2], [1, 2, 0], [1, 2, 1]],
        # staggerloc=[None, StaggerLoc.CENTER_VCENTER, StaggerLoc.EDGE1_VCENTER, StaggerLoc.EDGE2_VCENTER,
        #             StaggerLoc.CORNER_VCENTER, StaggerLoc.CENTER_VFACE, StaggerLoc.EDGE1_VFACE,
        #             StaggerLoc.EDGE2_VFACE, StaggerLoc.CORNER_VFACE],
        # coord_sys=[None, CoordSys.CART, CoordSys.SPH_DEG, CoordSys.SPH_RAD],
        # typekind=[None, TypeKind.I4, TypeKind.I8, TypeKind.R4, TypeKind.R8])

        # decompFlagPTile = np.array([[DecompFlag.DEFAULT, 1],
        #                           [DecompFlag.BALANCED, 2],
        #                           [DecompFlag.RESTFIRST, 3],
        #                           [DecompFlag.RESTLAST, 4],
        #                           [DecompFlag.CYCLIC, 5],
        #                           [DecompFlag.DEFAULT, 6]], dtype=np.int32)
        # deLabelList = np.array([11,12,13,14,15,16], dtype=np.int32)

        grid = Grid(tilesize = 45,
                    # decompFlagPTile = decompFlagPTile,
                    # deLabelList = deLabelList,
                    name = "cubed_sphere")
        grid.add_item(GridItem.MASK)
        grid.add_item(GridItem.AREA)
        # # slicing just the first de (slicing doesn't work for multiple des)
        # grid2 = grid[2:10, 4:7]
        self.examine_grid_attributes(grid)
        # self.examine_grid_attributes(grid2)
        grid.destroy()
        # grid2.destroy()

    def test_grid_create_cubed_sphere_reg_decomp_p_tile(self):
        """
        Test the creation of cubed-sphere grids when specifying regDecompPTile
        """
        regDecompPTile = np.array([[2,2,1,1,1,1],[2,2,2,2,2,2]], dtype=np.int32)

        grid = Grid(tilesize = 45, regDecompPTile = regDecompPTile,
                    name = "cubed_sphere")
        grid.add_item(GridItem.MASK)
        grid.add_item(GridItem.AREA)
        self.examine_grid_attributes(grid)
        grid.destroy()

    @pytest.mark.skipif(pet_count()!=1, reason="test must be run in serial")
    def test_grid_slice_2d(self):
        grid = self.make_grid_2d()

        grid2 = grid[1:21, 3:17]
        grid3 = grid2[4:6, 5:6]

        self.assert_expected_grid_shape(grid, StaggerLoc.CENTER, (100, 100))
        del grid

        self.assert_expected_grid_shape(grid2, StaggerLoc.CENTER, (20, 14))
        del grid2

        self.assert_expected_grid_shape(grid3, StaggerLoc.CENTER, (2, 1))

    @pytest.mark.skipif(pet_count()!=1, reason="test must be run in serial")
    def test_grid_slice_2d_corners(self):
        grid = self.make_grid_2d()

        grid.add_coords(staggerloc=StaggerLoc.CORNER)
        grid_row = grid.get_coords(0, staggerloc=StaggerLoc.CORNER)
        grid_col = grid.get_coords(1, staggerloc=StaggerLoc.CORNER)

        row = np.random.rand(*tuple(grid.upper_bounds[StaggerLoc.CORNER] - grid.lower_bounds[StaggerLoc.CORNER]))
        col = np.random.rand(*tuple(grid.upper_bounds[StaggerLoc.CORNER] - grid.lower_bounds[StaggerLoc.CORNER]))

        grid_row[:] = row
        grid_col[:] = col

        grid2 = grid[1:21, 3:17]
        grid3 = grid2[4:6, 5:6]

        self.assert_expected_grid_shape(grid, StaggerLoc.CORNER, (101, 101))
        del grid

        self.assert_expected_grid_shape(grid2, StaggerLoc.CORNER, (21, 15))
        del grid2

        self.assert_expected_grid_shape(grid3, StaggerLoc.CORNER, (3, 2))

    @pytest.mark.skipif(pet_count()!=1, reason="test must be run in serial")
    def test_grid_slice_3d(self):
        grid = self.make_grid_3d()

        grid2 = grid[1:21, 3:17, 7:47]
        grid3 = grid[4:6, 5:6, 0:2]

        self.assert_expected_grid_shape(grid, StaggerLoc.CENTER, (100, 100, 100))
        del grid

        self.assert_expected_grid_shape(grid2, StaggerLoc.CENTER, (20, 14, 40))
        del grid2

        self.assert_expected_grid_shape(grid3, StaggerLoc.CENTER, (2, 1, 2))

    @pytest.mark.skipif(pet_count()!=1, reason="test must be run in serial")
    def test_grid_slice_3d_corners(self):
        grid = self.make_grid_3d()

        cvf = StaggerLoc.CORNER_VFACE
        grid.add_coords(staggerloc=cvf)

        grid_row = grid.get_coords(0, staggerloc=cvf)
        grid_col = grid.get_coords(1, staggerloc=cvf)
        grid_vrt = grid.get_coords(2, staggerloc=cvf)

        row = np.random.rand(*tuple(grid.upper_bounds[cvf] - grid.lower_bounds[cvf]))
        col = np.random.rand(*tuple(grid.upper_bounds[cvf] - grid.lower_bounds[cvf]))
        vrt = np.random.rand(*tuple(grid.upper_bounds[cvf] - grid.lower_bounds[cvf]))

        grid_row[:] = row
        grid_col[:] = col
        grid_vrt[:] = vrt

        grid2 = grid[1:21, 3:17, 7:47]
        grid3 = grid[4:6, 5:6, 0:2]

        self.assert_expected_grid_shape(grid, cvf, (101, 101, 101))
        del grid

        self.assert_expected_grid_shape(grid2, cvf, (21, 15, 41))
        del grid2

        self.assert_expected_grid_shape(grid3, cvf, (3, 2, 3))

    @pytest.mark.skipif(pet_count()!=1, reason="test must be run in serial")
    def test_grid_slice_periodic(self):
        grid, x, y = self.make_grid_periodic()

        grid2 = grid[1:21, 3:17]
        grid3 = grid2[4:6, 5:6]

        self.assert_expected_grid_shape(grid, StaggerLoc.CENTER, (x, y))
        self.assert_expected_grid_shape(grid, StaggerLoc.CORNER, (x, y + 1))
        del grid

        self.assert_expected_grid_shape(grid2, StaggerLoc.CENTER, (20, 14))
        self.assert_expected_grid_shape(grid2, StaggerLoc.CORNER, (21, 15))
        del grid2

        self.assert_expected_grid_shape(grid3, StaggerLoc.CENTER, (2, 1))
        self.assert_expected_grid_shape(grid3, StaggerLoc.CORNER, (3, 2))

    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    @pytest.mark.skipif(pet_count()!=1, reason="test must be run in serial")
    def test_slice_grid_created_from_file_scrip(self):
        datafile = os.path.join(DATA_DIR, "T42_grid.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        reg_decomp = [pet_count(), 1]
        try:
            grid = Grid(filename=datafile,
                        filetype=FileFormat.SCRIP,
                        pole_kind=[PoleKind.MONOPOLE, PoleKind.BIPOLE],
                        reg_decomp=reg_decomp)
        except:
            raise NameError('grid_create_from_file_scrip failed!')

        grid2 = grid[0:5, 0:5]

        self.assert_expected_grid_shape(grid, StaggerLoc.CENTER, (128, 64))
        del grid

        self.assert_expected_grid_shape(grid2, StaggerLoc.CENTER, (5, 5))

    def test_grid_copy(self):
        grid = self.make_grid_2d()
        self.examine_grid_attributes(grid)

        grid2 = grid.copy()
        self.examine_grid_attributes(grid2)

        assert np.all(grid.coords == grid2.coords)

    def test_grid_coords(self):

        max_index = np.array([12, 20])

        grid = Grid(max_index)

        # Add coordinates
        grid.add_coords(staggerloc=[StaggerLoc.CENTER])

        exLB = grid.lower_bounds[StaggerLoc.CENTER]
        exUB = grid.upper_bounds[StaggerLoc.CENTER]

        print ("Grid Bounds:")
        print ("    exclusiveLBounds = " + str(exLB))
        print (type(exLB))
        print ("    exclusiveUBounds = " + str(exUB))
        print (exUB.dtype)

        # get and set the coordinates
        [x, y] = [0, 1]
        gridCoord = grid.coords[StaggerLoc.CENTER]

        for i in range(gridCoord[x].shape[x]):
            gridCoord[x][i, :] = float(i)

        for j in range(gridCoord[y].shape[y]):
            gridCoord[y][:, j] = float(j)

        # validate the coordinates using an easily understood double loop
        gridXCoord_check = grid.get_coords(x)
        gridYCoord_check = grid.get_coords(y)

        correct = True
        for i in range(gridXCoord_check.shape[x]):
            for j in range(gridXCoord_check.shape[y]):
                assert(gridXCoord_check[i, j] == float(i))

        for i in range(gridYCoord_check.shape[x]):
            for j in range(gridYCoord_check.shape[y]):
                assert(gridYCoord_check[i, j] == float(j))

    @pytest.mark.skipif(pet_count()!=1, reason="test must be run in serial")
    def test_grid_with_multiple_des_per_pet(self):
        """
        Test a grid's coordinates, areas and masks with multiple DEs per PET
        """

        def set_areas_and_masks(grid):
            grid.add_item(GridItem.AREA)
            grid.add_item(GridItem.MASK)
            for de in range(grid.local_de_count):
                # Do an initial assertion that area and mask have been set up properly for
                # all DEs
                assert type(grid.all_areas[de][StaggerLoc.CENTER]) is np.ndarray
                assert grid.all_areas[de][StaggerLoc.CENTER].shape == tuple(grid.all_sizes[de][StaggerLoc.CENTER])
                assert type(grid.all_masks[de][StaggerLoc.CENTER]) is np.ndarray
                assert grid.all_masks[de][StaggerLoc.CENTER].shape == tuple(grid.all_sizes[de][StaggerLoc.CENTER])

                # Create arbitrary, spatially-varying areas and masks:
                grid.all_areas[de][StaggerLoc.CENTER][...] = np.cos(np.radians(
                    grid.all_coords[de][StaggerLoc.CENTER][1]))
                grid.all_masks[de][StaggerLoc.CENTER][...] = (
                    (grid.all_coords[de][StaggerLoc.CENTER][0] > 90) &
                    (grid.all_coords[de][StaggerLoc.CENTER][1] > 0)).astype(int)

        def assert_areas_and_masks(grid):
            for de in range(grid.local_de_count):
                lons = grid.get_coords(0, localde=de)
                lats = grid.get_coords(1, localde=de)
                areas = grid.get_item(GridItem.AREA, localde=de)
                masks = grid.get_item(GridItem.MASK, localde=de)
                assert(np.array_equal(areas, np.cos(np.radians(lats))))
                assert(np.array_equal(masks, ((lons > 90) & (lats > 0)).astype(int)))

            mask_sum = np.sum(np.concatenate([np.ravel(grid.all_masks[de][StaggerLoc.CENTER])
                                              for de in range(grid.local_de_count)]))
            # 6 tiles, each 8x8; masks are 1 in 3/8 of the points (1/2 of the latitudes
            # and 3/4 of the longitudes)
            assert mask_sum == 6*8*8*(3/8)

        # Set up two grids with the same coordinates but different decompositions:
        regDecompPTile1 = np.array([[1,1,1,1,1,1],[1,1,1,1,1,1]], dtype=np.int32)
        grid1 = Grid(tilesize = 8, regDecompPTile=regDecompPTile1)
        set_areas_and_masks(grid1)
        regDecompPTile2 = np.array([[2,2,1,1,1,1],[2,2,2,2,2,2]], dtype=np.int32)
        grid2 = Grid(tilesize = 8, regDecompPTile=regDecompPTile2)
        set_areas_and_masks(grid2)

        # Do some basic checks
        self.examine_grid_attributes(grid1)
        self.examine_grid_attributes(grid2)

        # Make 1-d arrays of x and y coordinates, containing coordinates for all DEs. Note
        # that we use get_coords rather than all_coords in order to test the get_coords
        # function.
        grid1_x_coords = np.concatenate([np.ravel(grid1.get_coords(0, localde=de))
                                        for de in range(grid1.local_de_count)])
        grid1_y_coords = np.concatenate([np.ravel(grid1.get_coords(1, localde=de))
                                        for de in range(grid1.local_de_count)])
        grid2_x_coords = np.concatenate([np.ravel(grid2.get_coords(0, localde=de))
                                        for de in range(grid2.local_de_count)])
        grid2_y_coords = np.concatenate([np.ravel(grid2.get_coords(1, localde=de))
                                        for de in range(grid2.local_de_count)])

        # Check the coordinates by comparing the two grids. The following assertions don't
        # definitively confirm that the coordinates are correct with multiple DEs per PET,
        # but they catch some potential errors with multiple DEs per PET.
        #
        # Note that this only works with a single PET: For this to work with multiple
        # PETs, we would need to merge the arrays across PETs.
        assert(np.array_equal(np.sort(grid1_x_coords), np.sort(grid2_x_coords)))
        assert(np.array_equal(np.sort(grid1_y_coords), np.sort(grid2_y_coords)))

        # Check the areas and masks of both grids
        assert_areas_and_masks(grid1)
        assert_areas_and_masks(grid2)

    def test_grid_coords_3D(self):

        max_index = np.array([10, 20, 30])

        grid = Grid(max_index, coord_sys=CoordSys.CART)

        # Add coordinates
        grid.add_coords(staggerloc=[StaggerLoc.CENTER])

        exLB = grid.lower_bounds[StaggerLoc.CENTER]
        exUB = grid.upper_bounds[StaggerLoc.CENTER]

        print ("Grid Bounds:")
        print ("    exclusiveLBounds = " + str(exLB))
        print (type(exLB))
        print ("    exclusiveUBounds = " + str(exUB))
        print (exUB.dtype)

        # get and set the coordinates
        [x, y, z] = [0, 1, 2]
        gridCoord = grid.coords[StaggerLoc.CENTER]

        for i in range(gridCoord[x].shape[x]):
            gridCoord[x][i, :, :] = float(i)

        for j in range(gridCoord[y].shape[y]):
            gridCoord[y][:, j, :] = float(j)

        for k in range(gridCoord[z].shape[z]):
            gridCoord[z][:, :, k] = float(k)

        # get the coordinate pointers and validate the coordinates
        gridXCoord_check = grid.get_coords(x)
        gridYCoord_check = grid.get_coords(y)
        gridZCoord_check = grid.get_coords(z)

        for i in range(gridXCoord_check.shape[x]):
            for j in range(gridXCoord_check.shape[y]):
                for k in range(gridXCoord_check.shape[z]):
                    assert(gridXCoord_check[i, j, k] == float(i))

        for i in range(gridYCoord_check.shape[x]):
            for j in range(gridYCoord_check.shape[y]):
                for k in range(gridYCoord_check.shape[z]):
                    assert(gridYCoord_check[i, j, k] == float(j))

        for i in range(gridZCoord_check.shape[x]):
            for j in range(gridZCoord_check.shape[y]):
                for k in range(gridZCoord_check.shape[z]):
                    assert (gridZCoord_check[i, j, k] == float(k))

    def test_grid_mask(self):

        max_index = np.array([120, 200])

        grid = Grid(max_index)

        # Add coordinates
        grid.add_coords(staggerloc=[StaggerLoc.CENTER])

        # Add Mask
        mask = grid.add_item(GridItem.MASK)

        [x, y] = [0, 1]
        for i in range(mask.shape[x]):
            for j in range(mask.shape[y]):
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

    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_create_from_file_gridspec1D(self):
        datafile = os.path.join(DATA_DIR, "gridspec1Dcoords.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        grid = Grid(filename=datafile,
                    filetype=FileFormat.GRIDSPEC, add_corner_stagger=True,
                    coord_names=["longitude", "latitude"])

        self.examine_grid_attributes(grid)

    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_create_from_file_scrip(self):
        datafile = os.path.join(DATA_DIR, "T42_grid.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        reg_decomp = [pet_count(), 1]
        try:
            grid_from_file = Grid(filename=datafile,
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp)
        except:
            raise NameError('grid_create_from_file_scrip failed!')

    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_create_from_file_scrip_decomp_balanced_balanced(self):
        datafile = os.path.join(DATA_DIR, "T42_grid.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.BALANCED, DecompFlag.BALANCED],
                              dtype=np.int32)
        try:
            grid_from_file = Grid(filename=datafile,
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_balanced_balanced failed!')

    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_create_from_file_scrip_decomp_balanced_restfirst(self):
        datafile = os.path.join(DATA_DIR, "T42_grid.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.BALANCED, DecompFlag.RESTFIRST],
                              dtype=np.int32)
        try:
            grid_from_file = Grid(filename=datafile,
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_balanced_restfirst failed!')

    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_create_from_file_scrip_decomp_balanced_restlast(self):
        datafile = os.path.join(DATA_DIR, "T42_grid.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.BALANCED, DecompFlag.RESTLAST],
                              dtype=np.int32)
        try:
            grid_from_file = Grid(filename=datafile,
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_balanced_restlast failed!')

    @pytest.mark.xfail
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_create_from_file_scrip_decomp_balanced_cyclic(self):
        datafile = os.path.join(DATA_DIR, "T42_grid.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.BALANCED, DecompFlag.CYCLIC],
                              dtype=np.int32)
        try:
            grid_from_file = Grid(filename=datafile,
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_balanced_cyclic failed!')

    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_create_from_file_scrip_decomp_restfirst_balanced(self):
        datafile = os.path.join(DATA_DIR, "T42_grid.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.RESTFIRST, DecompFlag.BALANCED],
                              dtype=np.int32)
        try:
            grid_from_file = Grid(filename=datafile,
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_restfirst_balanced failed!')

    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_create_from_file_scrip_decomp_restfirst_restfirst(self):
        datafile = os.path.join(DATA_DIR, "T42_grid.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.RESTFIRST, DecompFlag.RESTFIRST],
                              dtype=np.int32)
        try:
            grid_from_file = Grid(filename=datafile,
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_restfirst_restfirst failed!')

    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_create_from_file_scrip_decomp_restfirst_restlast(self):
        datafile = os.path.join(DATA_DIR, "T42_grid.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.RESTFIRST, DecompFlag.RESTLAST],
                              dtype=np.int32)
        try:
            grid_from_file = Grid(filename=datafile,
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_restfirst_restlast failed!')

    @pytest.mark.xfail
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_create_from_file_scrip_decomp_restfirst_cyclic(self):
        datafile = os.path.join(DATA_DIR, "T42_grid.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.RESTFIRST, DecompFlag.CYCLIC],
                              dtype=np.int32)
        try:
            grid_from_file = Grid(filename=datafile,
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_restfirst_cyclic failed!')

    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_create_from_file_scrip_decomp_restlast_balanced(self):
        datafile = os.path.join(DATA_DIR, "T42_grid.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.RESTLAST, DecompFlag.BALANCED],
                              dtype=np.int32)
        try:
            grid_from_file = Grid(filename=datafile,
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_restlast_balanced failed!')

    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_create_from_file_scrip_decomp_restlast_restfirst(self):
        datafile = os.path.join(DATA_DIR, "T42_grid.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.RESTLAST, DecompFlag.RESTFIRST],
                              dtype=np.int32)
        try:
            grid_from_file = Grid(filename=datafile,
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_restlast_restfirst failed!')

    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_create_from_file_scrip_decomp_restlast_restlast(self):
        datafile = os.path.join(DATA_DIR, "T42_grid.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.RESTLAST, DecompFlag.RESTLAST],
                              dtype=np.int32)
        try:
            grid_from_file = Grid(filename=datafile,
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_restlast_restlast failed!')

    @pytest.mark.xfail
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_create_from_file_scrip_decomp_restlast_cyclic(self):
        datafile = os.path.join(DATA_DIR, "T42_grid.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.RESTLAST, DecompFlag.CYCLIC],
                              dtype=np.int32)
        try:
            grid_from_file = Grid(filename=datafile,
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_restlast_cyclic failed!')

    @pytest.mark.xfail
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_create_from_file_scrip_decomp_cyclic_balanced(self):
        datafile = os.path.join(DATA_DIR, "T42_grid.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.CYCLIC, DecompFlag.BALANCED],
                              dtype=np.int32)
        try:
            grid_from_file = Grid(filename=datafile,
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_cyclic_balanced failed!')

    @pytest.mark.xfail
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_create_from_file_scrip_decomp_cyclic_restfirst(self):
        datafile = os.path.join(DATA_DIR, "T42_grid.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.CYCLIC, DecompFlag.RESTFIRST],
                              dtype=np.int32)
        try:
            grid_from_file = Grid(filename=datafile,
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_cyclic_restfirst failed!')

    @pytest.mark.xfail
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_create_from_file_scrip_decomp_cyclic_restlast(self):
        datafile = os.path.join(DATA_DIR, "T42_grid.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.CYCLIC, DecompFlag.RESTLAST],
                              dtype=np.int32)
        try:
            grid_from_file = Grid(filename=datafile,
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_cyclic_restlast failed!')

    @pytest.mark.xfail
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_create_from_file_scrip_decomp_cyclic_cyclic(self):
        datafile = os.path.join(DATA_DIR, "T42_grid.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        reg_decomp = [pet_count(), 1]
        decompflag = np.array([DecompFlag.CYCLIC, DecompFlag.CYCLIC],
                              dtype=np.int32)
        try:
            grid_from_file = Grid(filename=datafile,
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp, decompflag=decompflag)
        except:
            raise NameError('grid_create_from_file_scrip_cyclic_cyclic failed!')
