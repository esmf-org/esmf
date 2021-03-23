
"""
grid unit test file
"""

import ESMF
from ESMF import *
from ESMF.interface.cbindings import *
from ESMF.test.base import TestBase, attr

import numpy as np
import os
import inspect

class TestGrid(TestBase):
    
    Manager(debug=True)

    def examine_grid_attributes(self, grid):
        # ~~~~~~~~~~~~~~~~~~~~~~  STAGGER LOCATIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # grid.staggerloc returns a boolean list of the activated stagger locations
        assert (type(grid.staggerloc) is list)
        assert (len(grid.staggerloc) is 2 ** grid.rank)

        # ~~~~~~~~~~~~~~~~~~~~~~  SIZE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # grid.size returns a list of the local (processor specific) sizes of grid coordinates in each stagger location
        assert (type(grid.size) is list)
        assert (len(grid.size) is 2 ** grid.rank)

        # ~~~~~~~~~~~~~~~~~~~~~~  BOUNDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # grid.lower_bounds or grid.upper_bounds returns a list of numpy arrays containing the bounds of all coordinate dimensions in the grid
        assert (type(grid.lower_bounds) is list)
        assert (len(grid.lower_bounds) is 2 ** grid.rank)
        assert (type(grid.upper_bounds) is list)
        assert (len(grid.upper_bounds) is 2 ** grid.rank)

        # ~~~~~~~~~~~~~~~~~~~~~~  COORDINATES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # grid.coords returns a list of lists of numpy arrays for coordinates of all stagger locations and coordinate dimensions
        assert (type(grid.coords) is list)
        assert (len(grid.coords) is 2 ** grid.rank)

        # ~~~~~~~~~~~~~~~~~~~~~~  MASK ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # grid.mask returns a list of numpy arrays containing the grid mask at each stagger location
        assert (type(grid.mask) is list)
        assert (len(grid.mask) is 2 ** grid.rank)

        # ~~~~~~~~~~~~~~~~~~~~~~  AREA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # grid.area returns a list of numpy arrays containing the grid cell areas at each stagger location
        assert (type(grid.mask) is list)
        assert (len(grid.mask) is 2 ** grid.rank)

        # stagger specific attributes
        for i in range(len(grid.staggerloc)):
            if grid.staggerloc[i]:

                # ~~~~~~~~~~~~~~~~~~~~~~  SIZE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # grid.size[i] returns a numpy array of the local (processor specific) size of the grid coordinates at the i'th stagger location
                assert (type(grid.size[i]) is np.ndarray)
                assert (grid.size[i].shape == tuple([grid.rank]))

                # ~~~~~~~~~~~~~~~~~~~~~~  BOUNDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # grid.lower_bounds[i] or grid.upper_bounds[i] returns a numpy array containing the bounds of all coordinate dimensions at the i'th stagger location
                assert (type(grid.lower_bounds[i] is np.ndarray))
                assert (grid.lower_bounds[i].shape == tuple([grid.rank]))
                assert (type(grid.upper_bounds[i] is np.ndarray))
                assert (grid.upper_bounds[i].shape == tuple([grid.rank]))

                # ~~~~~~~~~~~~~~~~~~~~~~  COORDINATES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # grid.coords[i] returns a list of length=rank containing numpy arrays for each coordinate dimension
                assert (type(grid.coords[i]) is list)
                assert (len(grid.coords[i]) is grid.rank)

                # grid.coords[i][j] returns a numpy array containing coordinates of i'th stagger location and j'th coordinate dimension
                for j in range(grid.rank):
                    assert (type(grid.coords[i][j] is np.ndarray))
                    assert (
                        grid.coords[i][j].shape == tuple(np.array(grid.upper_bounds[i]) - np.array(grid.lower_bounds[i])))

                # ~~~~~~~~~~~~~~~~~~~~~~  MASK ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # grid.mask[i] returns a numpy array containing the grid mask at the i'th stagger location
                if not isinstance(grid.mask[i], type(None)):
                    assert (type(grid.mask[i]) is np.ndarray)
                    assert (
                        grid.mask[i].shape == tuple(np.array(grid.upper_bounds[i]) - np.array(grid.lower_bounds[i])))

                # ~~~~~~~~~~~~~~~~~~~~~~  AREA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # grid.area[i] returns a numpy array containing the grid cell areas at the i'th stagger location
                if not isinstance(grid.area[i], type(None)):
                    assert (type(grid.area[i]) is np.ndarray)
                    assert (
                        grid.area[i].shape == tuple(np.array(grid.upper_bounds[i]) - np.array(grid.lower_bounds[i])))

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

        grid = ESMF.Grid(np.array([lon.size, lat.size], 'int32'),
                         num_peri_dims=1, staggerloc=[StaggerLoc.CENTER, StaggerLoc.CORNER])

        gridXCorner = grid.get_coords(0, staggerloc=ESMF.StaggerLoc.CORNER)
        lon_par = lon[grid.lower_bounds[ESMF.StaggerLoc.CORNER][0]:grid.upper_bounds[ESMF.StaggerLoc.CORNER][0]]
        gridXCorner[...] = lon_par.reshape((lon_par.size, 1))

        gridYCorner = grid.get_coords(1, staggerloc=ESMF.StaggerLoc.CORNER)
        lat_corner = np.linspace(90, -90, lat.size + 1)
        lat_par = lat_corner[grid.lower_bounds[ESMF.StaggerLoc.CORNER][1]:grid.upper_bounds[ESMF.StaggerLoc.CORNER][1]]
        gridYCorner[...] = lat_par.reshape((1, lat_par.size))

        offset_lon = 360. / lon.size / 2.
        lon -= offset_lon
        gridXCenter = grid.get_coords(0)
        lon_par = lon[grid.lower_bounds[ESMF.StaggerLoc.CENTER][0]:grid.upper_bounds[ESMF.StaggerLoc.CENTER][0]]
        gridXCenter[...] = lon_par.reshape((lon_par.size, 1))

        offset_lat = 180. / (lat.size) / 2.
        lat = np.linspace(90 - offset_lat, -90 + offset_lat, lat.size)
        gridYCenter = grid.get_coords(1)
        lat_par = lat[grid.lower_bounds[ESMF.StaggerLoc.CENTER][1]:grid.upper_bounds[ESMF.StaggerLoc.CENTER][1]]
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

    @attr('serial')
    @attr('slow')
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

    @attr('serial')
    @attr('slow')
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

    @attr('serial')
    def test_grid_create_cubed_sphere(self):
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

            regDecompPTile = np.array([[2,2],[2,2],[1,2],[1,2],[1,2],[1,2]], dtype=np.int32)
            regDecompPTile = np.array([[2,2,1,1,1,1],[2,2,2,2,2,2]], dtype=np.int32)
            # decompFlagPTile = np.array([[DecompFlag.DEFAULT, 1],
            #                           [DecompFlag.BALANCED, 2],
            #                           [DecompFlag.RESTFIRST, 3],
            #                           [DecompFlag.RESTLAST, 4],
            #                           [DecompFlag.CYCLIC, 5],
            #                           [DecompFlag.DEFAULT, 6]], dtype=np.int32)
            # deLabelList = np.array([11,12,13,14,15,16], dtype=np.int32)

            ESMF.Manager(debug=True)

            grid = Grid(tilesize = 45, regDecompPTile = regDecompPTile,
                                     #decompFlagPTile = decompFlagPTile,
                                     #deLabelList = deLabelList,
                                     name = "cubed_sphere")
            grid.add_item(GridItem.MASK)
            grid.add_item(GridItem.AREA)
            # # slicing just the first de (slicing doesn't work for multiple des)
            # grid2 = grid[2:10, 4:7]
            # self.examine_grid_attributes(grid)
            # self.examine_grid_attributes(grid2)
            grid.destroy()
            # grid2.destroy()

    @attr('serial')
    def test_grid_slice_2d(self):
        grid = self.make_grid_2d()

        grid2 = grid[1:21, 3:17]
        grid3 = grid2[4:6, 5:6]

        assert grid.coords[StaggerLoc.CENTER][0].shape == (100, 100)
        assert grid.upper_bounds[StaggerLoc.CENTER].tolist() == [100, 100]

        del grid

        assert grid2.coords[StaggerLoc.CENTER][0].shape == (20, 14)
        assert grid2.upper_bounds[StaggerLoc.CENTER].tolist() == [20, 14]

        del grid2

        assert grid3.coords[StaggerLoc.CENTER][0].shape == (2, 1)
        assert grid3.upper_bounds[StaggerLoc.CENTER].tolist() == [2, 1]

    @attr('serial')
    def test_grid_slice_2d_corners(self):
        grid = self.make_grid_2d()

        grid.add_coords(staggerloc=ESMF.StaggerLoc.CORNER)
        grid_row = grid.get_coords(0, staggerloc=StaggerLoc.CORNER)
        grid_col = grid.get_coords(1, staggerloc=StaggerLoc.CORNER)

        row = np.random.rand(*tuple(grid.upper_bounds[StaggerLoc.CORNER] - grid.lower_bounds[StaggerLoc.CORNER]))
        col = np.random.rand(*tuple(grid.upper_bounds[StaggerLoc.CORNER] - grid.lower_bounds[StaggerLoc.CORNER]))

        grid_row[:] = row
        grid_col[:] = col

        grid2 = grid[1:21, 3:17]
        grid3 = grid2[4:6, 5:6]

        assert grid.coords[StaggerLoc.CORNER][0].shape == (101, 101)
        assert grid.upper_bounds[StaggerLoc.CORNER].tolist() == [101, 101]

        del grid

        assert grid2.coords[StaggerLoc.CORNER][0].shape == (21, 15)
        assert grid2.upper_bounds[StaggerLoc.CORNER].tolist() == [21, 15]

        del grid2

        assert grid3.coords[StaggerLoc.CORNER][0].shape == (3, 2)
        assert grid3.upper_bounds[StaggerLoc.CORNER].tolist() == [3, 2]


    @attr('serial')
    def test_grid_slice_3d(self):
        grid = self.make_grid_3d()

        grid2 = grid[1:21, 3:17, 7:47]
        grid3 = grid[4:6, 5:6, 0:2]

        assert grid.coords[StaggerLoc.CENTER][0].shape == (100, 100, 100)
        assert grid.upper_bounds[StaggerLoc.CENTER].tolist() == [100, 100, 100]

        del grid

        assert grid2.coords[StaggerLoc.CENTER][0].shape == (20, 14, 40)
        assert grid2.upper_bounds[StaggerLoc.CENTER].tolist() == [20, 14, 40]

        del grid2

        assert grid3.coords[StaggerLoc.CENTER][0].shape == (2, 1, 2)
        assert grid3.upper_bounds[StaggerLoc.CENTER].tolist() == [2, 1, 2]

    @attr('serial')
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

        assert grid.coords[cvf][0].shape == (101, 101, 101)
        assert grid.upper_bounds[cvf].tolist() == [101, 101, 101]

        del grid

        assert grid2.coords[cvf][0].shape == (21, 15, 41)
        assert grid2.upper_bounds[cvf].tolist() == [21, 15, 41]

        del grid2

        assert grid3.coords[cvf][0].shape == (3, 2, 3)
        assert grid3.upper_bounds[cvf].tolist() == [3, 2, 3]

    @attr('serial')
    def test_grid_slice_periodic(self):
        grid, x, y = self.make_grid_periodic()

        grid2 = grid[1:21, 3:17]
        grid3 = grid2[4:6, 5:6]

        assert grid.coords[StaggerLoc.CENTER][0].shape == (x, y)
        assert grid.upper_bounds[StaggerLoc.CENTER].tolist() == [x, y]
        assert grid.coords[StaggerLoc.CORNER][0].shape == (x, y + 1)
        assert grid.upper_bounds[StaggerLoc.CORNER].tolist() == [x, y + 1]

        del grid

        assert grid2.coords[StaggerLoc.CENTER][0].shape == (20, 14)
        assert grid2.upper_bounds[StaggerLoc.CENTER].tolist() == [20, 14]
        assert grid2.coords[StaggerLoc.CORNER][0].shape == (21, 15)
        assert grid2.upper_bounds[StaggerLoc.CORNER].tolist() == [21, 15]

        del grid2

        assert grid3.coords[StaggerLoc.CENTER][0].shape == (2, 1)
        assert grid3.upper_bounds[StaggerLoc.CENTER].tolist() == [2, 1]
        assert grid3.coords[StaggerLoc.CORNER][0].shape == (3, 2)
        assert grid3.upper_bounds[StaggerLoc.CORNER].tolist() == [3, 2]

    @attr('data')
    @attr('serial')
    def test_slice_grid_created_from_file_scrip(self):
        reg_decomp = [pet_count(), 1]
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            grid = Grid(filename=os.path.join(esmfdir, "test/data/T42_grid.nc"),
                        filetype=FileFormat.SCRIP,
                        pole_kind=[PoleKind.MONOPOLE, PoleKind.BIPOLE],
                        reg_decomp=reg_decomp)
        except:
            raise NameError('grid_create_from_file_scrip failed!')

        grid2 = grid[0:5, 0:5]

        assert grid.coords[0][0].shape == (128, 64)
        assert grid.upper_bounds[0].tolist() == [128, 64]

        del grid

        assert grid2.coords[0][0].shape == (5, 5)
        assert grid2.upper_bounds[0].tolist() == [5, 5]

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

    @attr('data')
    def test_grid_create_from_file_gridspec1D(self):
        esmfdir = os.path.dirname(inspect.getfile(ESMF))
        grid = Grid(filename=os.path.join(esmfdir, "test/data/gridspec1Dcoords.nc"),
                    filetype=FileFormat.GRIDSPEC, add_corner_stagger=True,
                    coord_names=["longitude", "latitude"])

        self.examine_grid_attributes(grid)

    @attr('data')
    def test_grid_create_from_file_scrip(self):
        reg_decomp = [pet_count(), 1]
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            grid_from_file = Grid(filename=os.path.join(esmfdir, "test/data/T42_grid.nc"),
                                  filetype=FileFormat.SCRIP,
                                  reg_decomp=reg_decomp)
        except:
            raise NameError('grid_create_from_file_scrip failed!')

    @attr('data')
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

    @attr('data')
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

    @attr('data')
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

    @attr('data')
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

    @attr('data')
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

    @attr('data')
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

    @attr('data')
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

    @attr('data')
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

    @attr('data')
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

    @attr('data')
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

    @attr('data')
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

    @attr('data')
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

    @attr('data')
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

    @attr('data')
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

    @attr('data')
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

    @attr('data')
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

