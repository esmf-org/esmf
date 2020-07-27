"""
field unit test file
"""

try:
    from unittest import SkipTest
except ImportError:
    from nose import SkipTest

from ESMF import *
from ESMF.interface.cbindings import *
from ESMF.test.base import TestBase, attr
from ESMF.util.mesh_utilities import mesh_create_50, mesh_create_50_parallel


class TestField(TestBase):
    # this is for the documentation, do not modify
    def create_field(grid_or_mesh, name):
        '''
        PRECONDITIONS: An Grid or Mesh has been created, and 'name' is a string that
                       will be used to initialize the name of a new Field.\n
        POSTCONDITIONS: A Field has been created.\n
        RETURN VALUES: \n Field :: field \n
        '''
        field = Field(grid_or_mesh, name=name)

        return field

    def examine_field_attributes(self, field):
        # ~~~~~~~~~~~~~~~~~~~~~~  STAGGER LOCATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        assert (type(field.staggerloc) in [MeshLoc, StaggerLoc, int])

        # ~~~~~~~~~~~~~~~~~~~~~~  BOUNDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        assert (type(field.lower_bounds is np.ndarray))
        assert (field.lower_bounds.shape == tuple([field.rank]))
        assert (type(field.upper_bounds is np.ndarray))
        assert (field.upper_bounds.shape == tuple([field.rank]))
        if field.ndbounds:
            assert (type(field.ndbounds) is list)
            assert (len(field.ndbounds) == field.xd)

        # ~~~~~~~~~~~~~~~~~~~~~~  DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        assert (type(field.data) is np.ndarray)

    def make_field(self, array, ndbounds=True):
        '''
        :param self: TestMaskedArray class type
        :param array: maxindices of a 2- or 3d array
        :type array: np.array of dtype=np.int32
        '''

        grid = Grid(array, coord_sys=CoordSys.CART, staggerloc=StaggerLoc.CENTER)

        mask = grid.add_item(GridItem.MASK)
        mask[:] = 1
        mask[0, 1] = 0

        if ndbounds:
            field = Field(grid, ndbounds=[5, 2])
        else:
            field = Field(grid)

        field.data[...] = np.random.rand(*tuple(field.upper_bounds - field.lower_bounds))

        return field

    def test_meta_del(self):
        self.field = self.make_field(np.array([10, 10], dtype=np.int32))
        self.field.meta["test"] = "testmetaobject"
        assert (self.field.meta["test"] ==  "testmetaobject")
        del (self.field)
        assert (not hasattr(self, 'field'))

    @attr('serial')
    def test_numpy_funcs(self):
        field = self.make_field(np.array([10, 10], dtype=np.int32))

        field.data[...] = 4

        assert ((field.data.T == 4).all())
        assert ((field.data.flatten() == 4).all())

        # TODO: np.empty_like gives a segfault, waiting for support request to implement empty_like functionality
        #       for Field that actually allocates a new ESMF Field underneath
        # assert (type(nfp.empty_like(field)) == Field)

        # extended slices
        for i in range(10):
            field.data[i, :, :, :] = i
        assert field.data[::2, 0, 0, 0].all() == np.array([0, 2, 4, 6, 8]).all()

        # extended slices
        for i in range(10):
            field.data[i,:,:,:] = i
        assert field.data[::2, 0, 0, 0].all() == np.array([0, 2, 4, 6, 8]).all()




    @attr('serial')
    @attr('slow')
    #nosetests src/ESMF/test/test_api/test_field.py:TestField.test_field_create_2d_grid
    def test_field_create_2d_grid(self):
        keywords = dict(
            # periodic specifies all valid combos of [num_peri_dims, periodic_dim, pole_dim]
            periodic=[[None, None, None], [None, None, 0], [None, None, 1],
                      [0, None, None], [0, None, 0], [0, None, 1],
                      [1, None, None], [1, 0, 1], [1, 1, 0]],
            staggerloc=[None, StaggerLoc.CENTER, StaggerLoc.EDGE1, StaggerLoc.EDGE2, StaggerLoc.CORNER],
            coord_sys=[None, CoordSys.CART, CoordSys.SPH_DEG, CoordSys.SPH_RAD],
            typekind_grid=[None, TypeKind.I4, TypeKind.I8, TypeKind.R4, TypeKind.R8],
            typekind_field=[None, TypeKind.I4, TypeKind.I8, TypeKind.R4, TypeKind.R8],
            ndbounds=[None, [2], [5, 2]]
            )

        testcases = self.iter_product_keywords(keywords)
        fail = 0
        for a in testcases:
            try:
                grid = Grid(np.array([12, 12]),
                            num_peri_dims=a.periodic[0], periodic_dim=a.periodic[1], pole_dim=a.periodic[2],
                            coord_sys=a.coord_sys, coord_typekind=a.typekind_grid, staggerloc=a.staggerloc)
                # if not isinstance(a.mask_values, type(None)) and not isinstance(a.staggerloc, None):
                #     grid.add_item(GridItem.MASK, staggerloc=a.staggerloc)
                #     for b in a.mask_values:
                #         grid.mask[a.staggerloc][:, b] = b

                field = Field(grid, name="test_field_grid_2d", typekind=a.typekind_field,
                              staggerloc=a.staggerloc, ndbounds=a.ndbounds)

                field2 = None
                if not isinstance(a.ndbounds, type(None)):
                    if len(a.ndbounds) == 1:
                        field2 = field[7:9, 2:10, 1]
                    elif len(a.ndbounds) == 2:
                        field2 = field[7:9, 2:10, 2:4, 1]
                else:
                    field2 = field[7:9, 2:10]
                self.examine_field_attributes(field)
                self.examine_field_attributes(field2)
                field2.destroy()
                field.destroy()
                grid.destroy()
            except:
                fail += 1

        if fail > 0:
            raise ValueError(
                "The following combinations of parameters failed to create a proper Field: " + str(fail))

    @attr('serial')
    @attr('slow')
    def test_field_create_3d_grid(self):
        keywords = dict(
            # periodic specifies all valid combos of [num_peri_dims, periodic_dim, pole_dim]
            periodic=[[None, None, None], [None, None, 0], [None, None, 1], [None, None, 2],
                      [0, None, None], [0, None, 0], [0, None, 1], [0, None, 2],
                      [1, None, None], [1, 0, 1], [1, 0, 2], [1, 1, 0], [1, 1, 2], [1, 2, 0], [1, 2, 1]],
            staggerloc=[None, StaggerLoc.CENTER_VCENTER, StaggerLoc.EDGE1_VCENTER, StaggerLoc.EDGE2_VCENTER,
                        StaggerLoc.CORNER_VCENTER, StaggerLoc.CENTER_VFACE, StaggerLoc.EDGE1_VFACE,
                        StaggerLoc.EDGE2_VFACE, StaggerLoc.CORNER_VFACE],
            coord_sys=[None, CoordSys.CART, CoordSys.SPH_DEG, CoordSys.SPH_RAD],
            mask_values=[None, [2], [2, 3, 4]],
            typekind_grid=[None, TypeKind.I4, TypeKind.I8, TypeKind.R4, TypeKind.R8],
            typekind_field=[None, TypeKind.I4, TypeKind.I8, TypeKind.R4, TypeKind.R8],
            ndbounds=[None, [2], [5, 2]]
            )

        testcases = self.iter_product_keywords(keywords)
        fail = []
        for a in testcases:
            try:
                grid = Grid(np.array([12, 12, 12]),
                            num_peri_dims=a.periodic[0], periodic_dim=a.periodic[1], pole_dim=a.periodic[2],
                            coord_sys=a.coord_sys, coord_typekind=a.typekind_grid, staggerloc=a.staggerloc)
                if not isinstance(a.mask_values, type(None)) and not isinstance(a.staggerloc, type(None)):
                    grid.add_item(GridItem.MASK, staggerloc=a.staggerloc)
                    for b in a.mask_values:
                        grid.mask[a.staggerloc][:, :, b] = b

                field = Field(grid, name="test_field_grid_2d", typekind=a.typekind_field,
                              staggerloc=a.staggerloc, ndbounds=a.ndbounds)
                self.examine_field_attributes(field)
                field2 = None
                if not isinstance(a.ndbounds, type(None)):
                    if len(a.ndbounds) == 1:
                        field2 = field[4:11, 7:9, 2:10, 1]
                    elif len(a.ndbounds) == 2:
                        field2 = field[4:11, 7:9, 2:10, 2:4, 1]
                else:
                    field2 = field[4:11, 7:9, 2:10]
                self.examine_field_attributes(field2)
                field2.destroy()
                field.destroy()
                grid.destroy()
            except:
                fail += a

        if len(fail) > 0:
            raise ValueError(
                "The following combinations of parameters failed to create a proper Field: " + str(len(fail)))

    @attr('slow')
    def test_field_create_2d_mesh(self):
        parallel = False
        if pet_count() > 1:
            parallel = True

        if parallel:
            if constants._ESMF_MPIRUN_NP != 4:
                raise SkipTest('This test must be run with 4 processors.')

        keywords = dict(
            meshloc=[MeshLoc.NODE, MeshLoc.ELEMENT],
            typekind_field=[None, TypeKind.I4, TypeKind.I8, TypeKind.R4, TypeKind.R8],
        )
        # TODO: Mesh masking, periodicity?
        # TODO: extra dimensions on a mesh?

        testcases = self.iter_product_keywords(keywords)
        fail = []
        for a in testcases:
            try:
                # create mesh
                mesh = None
                if parallel:
                    mesh, nodeCoord, nodeOwner, elemType, elemConn, elemCoord = \
                        mesh_create_50_parallel()
                else:
                    mesh, nodeCoord, nodeOwner, elemType, elemConn, elemCoord = \
                        mesh_create_50()

                field = Field(mesh, name="test_field_mesh_2d",
                              typekind=a.typekind_field, meshloc=a.meshloc)
                self.examine_field_attributes(field)
                field2 = field[2:10]
                self.examine_field_attributes(field2)
                field2.destroy()
                field.destroy()
                mesh.destroy()
            except:
                fail += a

        if len(fail) > 0:
            raise ValueError(
                "The following combinations of parameters failed to create a proper Field: " + str(len(fail)))

    # TODO: 3d Field mesh?

    # copy constructorish
    def test_field_create_from_gridfield(self):
        field = self.make_field(np.array([10, 10], dtype=np.int32))
        self.examine_field_attributes(field)
        data = np.random.rand(*tuple(field.upper_bounds - field.lower_bounds))
        field.data[...] = data

        field2 = Field(field.grid)
        self.examine_field_attributes(field2)
        # field1 was created with ungridded dimensions, but field2 was not
        field2.data[...] = data[:,:,0,0]

        assert np.all(field.grid.coords == field2.grid.coords)
        assert np.all(field.data[:,:,0,0] == field2.data)

    def test_field_grid_copy(self):
        field = self.make_field(np.array([10, 10], dtype=np.int32))
        self.examine_field_attributes(field)

        field2 = field.copy()
        self.examine_field_attributes(field2)

        assert np.all(field.grid.coords == field2.grid.coords)
        assert np.all(field.data == field2.data)

    # don't change this function, it's used in the documentation
    def create_field(gml, name):
        '''
        PRECONDITIONS: An Grid, Mesh or LocStream has been created, and 'name' is a string that
                       will be used to initialize the name of a new Field.\n
        POSTCONDITIONS: A Field has been created.\n
        RETURN VALUES: \n Field :: field \n
        '''
        field = Field(gml, name=name)

        return field

    def test_field_uniqueness(self):
        parallel = False
        if pet_count() > 1:
            parallel = True

        if parallel:
            if constants._ESMF_MPIRUN_NP != 4:
                raise SkipTest('This test must be run with 4 processors.')

        mesh = None
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, _ = \
                mesh_create_50()

        field = Field(mesh, typekind=TypeKind.I4, meshloc=MeshLoc.NODE)
        self.examine_field_attributes(field)

        field2 = Field(mesh, typekind=TypeKind.I4, meshloc=MeshLoc.ELEMENT)
        self.examine_field_attributes(field2)

        for i in range(field.data.shape[0]):
            field.data[i] = 10

        for i in range(field2.data.shape[0]):
            field2.data[i] = 10

        assert (field.struct.ptr != field2.struct.ptr)

    @attr('serial')
    def test_field_area(self):
        grid = Grid(np.array([3, 4]), staggerloc=[StaggerLoc.CENTER, StaggerLoc.CORNER],
                    coord_sys=CoordSys.SPH_DEG, num_peri_dims=1,
                    periodic_dim=0, pole_dim=1)

        gridLon = grid.get_coords(0)
        gridLat = grid.get_coords(1)
        gridLonCorner = grid.get_coords(0, staggerloc=StaggerLoc.CORNER)
        gridLatCorner = grid.get_coords(1, staggerloc=StaggerLoc.CORNER)

        lon = np.linspace(-120, 120, 3)
        lat = np.linspace(-67.5, 67.5, 4)
        lon_corner = np.arange(-180, 180, 120)
        lat_corner = np.linspace(-90, 90, 5)

        lonm, latm = np.meshgrid(lon, lat, indexing='ij')
        lonm_corner, latm_corner = np.meshgrid(lon_corner, lat_corner,
                                               indexing='ij')

        gridLon[:] = lonm
        gridLat[:] = latm
        gridLonCorner[:] = lonm_corner
        gridLatCorner[:] = latm_corner

        field = Field(grid)
        field.get_area()

        # this will allocate space for user areas, reinitializing esmf areas
        # grid.add_item(GridItem.AREA)

        field2 = Field(grid)
        field2.get_area()

        assert(np.all(field.data == field2.data))


    def test_field_locstream_mask(self):
        # LocStream creation and simple validation
        locstream = LocStream(5, name="Test LocStream")
        assert locstream.size == 5

        locstream["ESMF:X"] = [1, 2, 3, 4, 5]
        locstream["ESMF:Y"] = [7, 7, 7, 7, 7]
        locstream["ESMF:Mask"] = np.array([0, 0, 1, 1, 1])

        field = Field(locstream)
        field.data[...] = 7
        self.examine_field_attributes(field)

        print (field)

    def test_field_switchedindices_grid(self):
        # create grid
        max_index = np.array([12, 20])
        grid = Grid(max_index, num_peri_dims=1,
                    coord_sys=CoordSys.SPH_RAD,
                    staggerloc=[StaggerLoc.CENTER])

        gridtofieldmap = np.array([2, 1])

        field = Field(grid, typekind=TypeKind.R8, ndbounds=gridtofieldmap)

        field2 = Field(grid, ndbounds=np.array([2, 1]))

        field.data[...] = 10
        self.examine_field_attributes(field)

        field2.data[...] = 10
        self.examine_field_attributes(field2)


    def test_field_extradims_grid(self):
        max_index = np.array([12, 20])
        grid = Grid(max_index, num_peri_dims=1,
                    coord_sys=CoordSys.SPH_RAD,
                    staggerloc=[StaggerLoc.CENTER])

        gridtofieldmap = np.array([2, 5])
        field = Field(grid, typekind=TypeKind.R8, ndbounds=gridtofieldmap)

        field2 = Field(grid, ndbounds=np.array([5, 2]))

        field.data[...] = 10
        self.examine_field_attributes(field)

        field2.data[...] = 10
        self.examine_field_attributes(field2)


    def test_field_extradims_mesh(self):
        parallel = False
        if pet_count() > 1:
            parallel = True

        if parallel:
            if constants._ESMF_MPIRUN_NP != 4:
                raise SkipTest('This test must be run with 4 processors.')

        mesh = None
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, _ = \
                mesh_create_50()

        field = Field(mesh, typekind=TypeKind.R8, meshloc=MeshLoc.NODE)
        field2 = Field(mesh, meshloc=MeshLoc.ELEMENT, ndbounds=np.array([5, 2]))

        field.data[...] = 10
        self.examine_field_attributes(field)

        field2.data[...] = 10
        self.examine_field_attributes(field2)

    @attr('serial')
    def test_field_slice_grid(self):
        typekind = TypeKind.R8
        grid = Grid(np.array([100, 100]), coord_sys=CoordSys.CART,
                    coord_typekind=typekind, staggerloc=[StaggerLoc.CENTER])

        grid_row = grid.get_coords(0, staggerloc=StaggerLoc.CENTER)
        grid_col = grid.get_coords(1, staggerloc=StaggerLoc.CENTER)

        row = np.random.rand(100, 100)
        col = np.random.rand(100, 100)

        grid_row[:] = row
        grid_col[:] = col

        grid.add_item(GridItem.MASK)
        grid.add_item(GridItem.AREA)

        field = Field(grid, staggerloc=StaggerLoc.CENTER)
        self.examine_field_attributes(field)

        field2 = field[0:5, 0:5]
        self.examine_field_attributes(field2)

        field3 = field2[2:4, 2:4]
        self.examine_field_attributes(field3)

        assert (field.data.shape == (100, 100))
        assert (field2.data.shape == (5, 5))
        assert (field3.data.shape == (2, 2))

        assert (field.upper_bounds.tolist() == [100, 100])
        assert (field2.upper_bounds.tolist() == [5, 5])
        assert (field3.upper_bounds.tolist() == [2, 2])

        assert (field.grid.upper_bounds[0].tolist() == [100, 100])
        assert (field2.grid.upper_bounds[0].tolist() == [5, 5])
        assert (field3.grid.upper_bounds[0].tolist() == [2, 2])

    # slicing is disabled in parallel
    @attr('serial')
    def test_field_slice_mesh(self):
        parallel = False
        if pet_count() > 1:
            parallel = True

        if parallel:
            if constants._ESMF_MPIRUN_NP != 4:
                raise SkipTest('This test must be run with 4 processors.')

        mesh = None
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, _ = \
                mesh_create_50()

        field = Field(mesh, typekind=TypeKind.R8, meshloc=MeshLoc.NODE)
        self.examine_field_attributes(field)

        field2 = field[0:5]
        self.examine_field_attributes(field2)

        field3 = field2[2:4]
        self.examine_field_attributes(field3)

        assert (field.data.shape == (64,))
        assert (field2.data.shape == (5,))
        assert (field3.data.shape == (2,))

        assert (field.upper_bounds.tolist() == [64])
        assert (field2.upper_bounds.tolist() == [5])
        assert (field3.upper_bounds.tolist() == [2])

        assert (field.grid.size[0] == 64)
        assert (field2.grid.size[0] == 5)
        assert (field3.grid.size[0] == 2)

    @attr('serial')
    def test_field_slice_grid_extraindices(self):
        n = 10
        grid = Grid(np.array([n,n]), coord_sys=CoordSys.CART, staggerloc=StaggerLoc.CENTER)

        grid_row = grid.get_coords(0, staggerloc=StaggerLoc.CENTER)
        grid_col = grid.get_coords(1, staggerloc=StaggerLoc.CENTER)

        row = np.arange(0, n, 1)

        grid_row[...] = row.reshape((row.size,1))
        grid_col[...] = row.reshape((1,row.size))

        field = Field(grid, ndbounds=[5, 2])
        self.examine_field_attributes(field)

        for i in range(5):
            for j in range(2):
                field.data[:, :, i, j] = i+j

        field2 = field[0:5, 0:5, 0:2, 0:1]
        self.examine_field_attributes(field2)

        field3 = field2[2:4, 2:4, 0:1, 0:1]
        self.examine_field_attributes(field3)

        assert field.data.shape == (10, 10, 5, 2)
        assert field2.data.shape == (5, 5, 2, 1)
        assert field3.data.shape == (2, 2, 1, 1)

        assert (field.upper_bounds.tolist() == [10, 10, 5, 2])
        assert (field2.upper_bounds.tolist() == [5, 5, 2 ,1])
        assert (field3.upper_bounds.tolist() == [2, 2, 1, 1])

        assert (field.grid.upper_bounds[0].tolist() == [10, 10])
        assert (field2.grid.upper_bounds[0].tolist() == [5, 5])
        assert (field3.grid.upper_bounds[0].tolist() == [2, 2])

    @attr('serial')
    def disable_est_field_slice_mesh_extraindices(self):
        parallel = False
        if pet_count() > 1:
            parallel = True

        if parallel:
            if constants._ESMF_MPIRUN_NP != 4:
                raise SkipTest('This test must be run with 4 processors.')

        mesh = None
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50()

        field = Field(mesh, typekind=TypeKind.R8,
                      meshloc=MeshLoc.NODE, ndbounds=[5, 2])
        self.examine_field_attributes(field)

        for i in range(5):
            for j in range(2):
                field[:, :, i, j] = i + j

        field2 = field[0:5, 0:2, 0:1]
        self.examine_field_attributes(field2)

        field3 = field2[2:4, 1:2, 0:1]
        self.examine_field_attributes(field3)

        assert field.data.shape == (10, 5, 2)
        assert field2.data.shape == (5, 2, 1)
        assert field3.data.shape == (2, 1, 1)

    @attr('serial')
    def test_field_reshape(self):
        field = self.make_field(np.array([10, 10], dtype=np.int32),
                                ndbounds=False)

        field.data[...] = 4

        field2 = field.data.reshape(5, 20)

        assert type(field2) == np.ndarray
        assert field2.shape == (5,20)
        # self.examine_field_attributes(field2)
