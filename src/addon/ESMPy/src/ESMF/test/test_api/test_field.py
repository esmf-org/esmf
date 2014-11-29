"""
field unit test file
"""

from ESMF import *
from ESMF.interface.cbindings import *
from ESMF.test.base import TestBase
from ESMF.test.test_api.mesh_utilities import mesh_create_50, mesh_create_50_parallel


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

    def test_ocgismeta(self):
        self.maskedfield = self.make_maskedfield(np.array([10, 10], dtype=np.int32))
        self.maskedfield._ocgis["test"] = "testmetaobject"
        assert (self.maskedfield._ocgis["test"] ==  "testmetaobject")

    def test_del(self):
        self.maskedfield = self.make_maskedfield(np.array([10,10], dtype=np.int32))
        del(self.maskedfield)
        assert(not hasattr(self, 'maskedfield'))

    def test_field_mask(self):

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
                    mask[i, j] = 2
                elif (i == 3.0):
                    mask[i, j] = 3
                else:
                    mask[i, j] = 0;

        # create a Field on the Grid, should inherit the mask
        field = NewField(grid, "FIELD!", mask_values=[2])

        if (not field.mask[2][0]):
            raise ValueError("field mask is incorrect")

    def test_field_mask_with_xd(self):

        grid = Grid(np.array([10, 10], dtype=np.int32), coord_sys=CoordSys.CART)

        mask = grid.add_item(GridItem.MASK)
        mask[:] = 1
        mask[0, 1] = 0

        field = Field(grid, "name", ndbounds=[2, 5], mask_values=[0])

        assert (np.all(field.mask[:, :, 0, 1]))

    def test_field_mask_3D(self):

        max_index = np.array([10, 20, 30])

        grid = Grid(max_index)

        # Add coordinates
        grid.add_coords(staggerloc=[StaggerLoc.CENTER])

        # Add Mask
        mask = grid.add_item(GridItem.MASK)

        [x, y, z] = [0, 1, 2]
        for i in xrange(mask.shape[x]):
            for j in xrange(mask.shape[y]):
                for k in xrange(mask.shape[z]):
                    if (i == 1.0):
                        mask[i, j, k] = 2
                    elif (j == 2.0):
                        mask[i, j, k] = 3
                    else:
                        mask[i, j, k] = 0

        # create a Field on the Grid, should inherit the mask
        field = Field(grid, "FIELD!", mask_values=[2, 3])

        assert (np.all(field.mask[1, :, :]))
        assert (np.all(field.mask[:, 2, :]))

    def test_field_mask_3D_with_xd(self):

        grid = Grid(np.array([10, 10, 10], dtype=np.int32), coord_sys=CoordSys.CART)

        mask = grid.add_item(GridItem.MASK)
        mask[:] = 1
        mask[0, 1, 0] = 0

        field = Field(grid, "name", ndbounds=[2, 5], mask_values=[0])

        assert (np.all(field.mask[:, :, 0, 1, 0]))

    def test_field(self):

        # create a Grid with center staggers
        max_index = np.array([12, 20])
        grid = Grid(max_index, staggerloc=[StaggerLoc.CENTER])

        # create a Field on the Grid
        field = NewField(grid, "FIELD!")

        for i in range(field.shape[0]):
            for j in range(field.shape[1]):
                field.data[i, j] = 10

    def test_field_edge1(self):

        # create a Grid with center staggers
        max_index = np.array([12, 20])
        grid = Grid(max_index, staggerloc=[StaggerLoc.EDGE1])

        # create a Field on the Grid
        field = NewField(grid, "GRIDFIELD!", staggerloc=StaggerLoc.EDGE1)

        for i in range(field.shape[0]):
            for j in range(field.shape[1]):
                field.data[i, j] = 10

    def test_field_edge2(self):

        # create a Grid with center staggers
        max_index = np.array([12, 20])
        grid = Grid(max_index, staggerloc=[StaggerLoc.EDGE2])

        # create a Field on the Grid
        field = NewField(grid, "GRIDFIELD!", staggerloc=StaggerLoc.EDGE2)

        for i in range(field.shape[0]):
            for j in range(field.shape[1]):
                field.data[i, j] = 10

    def test_field_corner(self):

        # create a Grid with center staggers
        max_index = np.array([12, 20])
        grid = Grid(max_index, staggerloc=[StaggerLoc.CORNER])

        # create a Field on the Grid
        field = NewField(grid, "GRIDFIELD!", staggerloc=StaggerLoc.CORNER)

        for i in range(field.shape[0]):
            for j in range(field.shape[1]):
                field.data[i, j] = 10

    def test_field_3D(self):

        # create a Grid with center staggers
        max_index = np.array([10, 20, 30])
        grid = Grid(max_index, coord_sys=CoordSys.CART, \
                    staggerloc=[StaggerLoc.CENTER])

        # create a Field on the Grid
        field = NewField(grid, "GRIDFIELD!")

        for i in range(field.shape[0]):
            for j in range(field.shape[1]):
                field.data[i, j] = 10

    def test_field_3D_edge1vcenter(self):

        # create a Grid with center staggers
        max_index = np.array([10, 20, 30])
        grid = Grid(max_index, coord_sys=CoordSys.CART, \
                    staggerloc=[StaggerLoc.EDGE1_VCENTER])

        # grid does not need coordinates to be ready for a field

        # create a Field on the Grid
        field = NewField(grid, "GRIDFIELD!", staggerloc=StaggerLoc.EDGE1_VCENTER)

        for i in range(field.shape[0]):
            for j in range(field.shape[1]):
                field.data[i, j] = 10

    def test_field_3D_edge2vcenter(self):

        # create a Grid with center staggers
        max_index = np.array([10, 20, 30])
        grid = Grid(max_index, coord_sys=CoordSys.CART, \
                    staggerloc=[StaggerLoc.EDGE2_VCENTER])

        # grid does not need coordinates to be ready for a field

        field = NewField(grid, "GRIDFIELD!", staggerloc=StaggerLoc.EDGE2_VCENTER)

        for i in range(field.shape[0]):
            for j in range(field.shape[1]):
                field.data[i, j] = 10

    def test_field_3D_cornervcenter(self):

        # create a Grid with center staggers
        max_index = np.array([10, 20, 30])
        grid = Grid(max_index, coord_sys=CoordSys.CART, \
                    staggerloc=[StaggerLoc.CORNER_VCENTER])

        # grid does not need coordinates to be ready for a field

        # create a Field on the Grid
        field = NewField(grid, "GRIDFIELD!", staggerloc=StaggerLoc.CORNER_VCENTER)

        for i in range(field.shape[0]):
            for j in range(field.shape[1]):
                field.data[i, j] = 10

    def test_field_3D_centervface(self):

        # create a Grid with center staggers
        max_index = np.array([10, 20, 30])
        grid = Grid(max_index, coord_sys=CoordSys.CART, \
                    staggerloc=[StaggerLoc.CENTER_VFACE])

        # create a Field on the Grid
        field = NewField(grid, "GRIDFIELD!", staggerloc=StaggerLoc.CENTER_VFACE)

        for i in range(field.shape[0]):
            for j in range(field.shape[1]):
                field.data[i, j] = 10

    def test_field_3D_edge1vface(self):

        # create a Grid with center staggers
        max_index = np.array([10, 20, 30])
        grid = Grid(max_index, coord_sys=CoordSys.CART, \
                    staggerloc=[StaggerLoc.EDGE1_VFACE])

        # create a Field on the Grid
        field = NewField(grid, "GRIDFIELD!", staggerloc=StaggerLoc.EDGE1_VFACE)

        for i in range(field.shape[0]):
            for j in range(field.shape[1]):
                field.data[i, j] = 10

    def test_field_3D_edge2vface(self):

        # create a Grid with center staggers
        max_index = np.array([10, 20, 30])
        grid = Grid(max_index, coord_sys=CoordSys.CART, \
                    staggerloc=[StaggerLoc.EDGE2_VFACE])

        # create a Field on the Grid
        field = NewField(grid, "GRIDFIELD!", staggerloc=StaggerLoc.EDGE2_VFACE)

        for i in range(field.shape[0]):
            for j in range(field.shape[1]):
                field.data[i, j] = 10

    def test_field_3D_cornervface(self):

        # create a Grid with center staggers
        max_index = np.array([10, 20, 30])
        grid = Grid(max_index, coord_sys=CoordSys.CART, \
                    staggerloc=[StaggerLoc.CORNER_VFACE])

        # create a Field on the Grid
        field = NewField(grid, "GRIDFIELD!", staggerloc=StaggerLoc.CORNER_VFACE)

        for i in range(field.shape[0]):
            for j in range(field.shape[1]):
                field.data[i, j] = 10

    def test_field_r8_grid(self):
        # create grid
        max_index = np.array([12, 20])
        grid = Grid(max_index, num_peri_dims=1, coord_sys=CoordSys.SPH_RAD, \
                    staggerloc=[StaggerLoc.CENTER])

        field = NewField(grid, 'Field!', TypeKind.R8)
        field2 = NewField(grid, 'Field!')

        for i in range(field.shape[0]):
            for j in range(field.shape[1]):
                field.data[i, j] = 10

        for i in range(field2.shape[0]):
            for j in range(field2.shape[1]):
                field2.data[i, j] = 10

    def test_field_r4_grid(self):
        # create grid
        max_index = np.array([12, 20])
        grid = Grid(max_index, num_peri_dims=1, \
                    coord_sys=CoordSys.SPH_RAD, \
                    coord_typekind=TypeKind.R4, \
                    staggerloc=[StaggerLoc.CENTER])

        field = NewField(grid, 'Field!', typekind=TypeKind.R4)

        field2 = NewField(grid, 'Field!', typekind=TypeKind.R4)

        for i in range(field.shape[0]):
            for j in range(field.shape[1]):
                field.data[i, j] = 10

        for i in range(field2.shape[0]):
            for j in range(field2.shape[1]):
                field2.data[i, j] = 10

    def test_field_i8_grid(self):
        # create grid
        max_index = np.array([12, 20])
        grid = Grid(max_index, num_peri_dims=1, \
                    coord_sys=CoordSys.SPH_RAD, \
                    coord_typekind=TypeKind.R4, \
                    staggerloc=[StaggerLoc.CENTER])

        field = NewField(grid, 'Field!', TypeKind.I8)

        field2 = NewField(grid, 'Field!', typekind=TypeKind.I8)

        for i in range(field.shape[0]):
            for j in range(field.shape[1]):
                field.data[i, j] = 10

        for i in range(field2.shape[0]):
            for j in range(field2.shape[1]):
                field2.data[i, j] = 10

    def test_field_i4_grid(self):
        # create grid
        max_index = np.array([12, 20])
        grid = Grid(max_index, num_peri_dims=1, \
                    coord_sys=CoordSys.SPH_RAD, \
                    coord_typekind=TypeKind.R4, \
                    staggerloc=[StaggerLoc.CENTER])

        field = NewField(grid, 'Field!', TypeKind.I4)

        field2 = NewField(grid, 'Field!', typekind=TypeKind.I4)

        for i in range(field.shape[0]):
            for j in range(field.shape[1]):
                field.data[i, j] = 10

        for i in range(field2.shape[0]):
            for j in range(field2.shape[1]):
                field2.data[i, j] = 10

    def test_field_r8_mesh(self):
        # create mesh
        parallel = False
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        mesh = None
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50()

        field = NewField(mesh, 'Field!', \
                         TypeKind.R8, \
                         MeshLoc.NODE)
        print "field1 created"
        field2 = NewField(mesh, 'Field!',
                          meshloc=MeshLoc.ELEMENT)
        print "field2 created"

        for i in range(field.shape[0]):
            field.data[i] = 10

        for i in range(field2.shape[0]):
            field2.data[i] = 10

    def test_field_r4_mesh(self):
        # create mesh
        parallel = False
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        mesh = None
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50()

        field = NewField(mesh, 'Field!',
                         typekind=TypeKind.R4)

        field2 = NewField(mesh, 'Field!',
                          typekind=TypeKind.R4,
                          meshloc=MeshLoc.ELEMENT)

        for i in range(field.shape[0]):
            field.data[i] = 10

        for i in range(field2.shape[0]):
            field2.data[i] = 10

    def test_field_i8_mesh(self):
        # create mesh
        parallel = False
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        mesh = None
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50()

        field = NewField(mesh, 'Field!',
                         TypeKind.I8,
                         MeshLoc.NODE)

        field2 = NewField(mesh, 'Field!',
                          typekind=TypeKind.I8,
                          meshloc=MeshLoc.ELEMENT)

        for i in range(field.shape[0]):
            field.data[i] = 10

        for i in range(field2.shape[0]):
            field2.data[i] = 10

    def test_field_i4_mesh(self):
        # create mesh
        parallel = False
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        mesh = None
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50()

        field = NewField(mesh, 'Field!',
                         TypeKind.I4,
                         MeshLoc.NODE)

        field2 = NewField(mesh, 'Field!',
                          typekind=TypeKind.I4,
                          meshloc=MeshLoc.ELEMENT)

        for i in range(field.shape[0]):
            field.data[i] = 10

        for i in range(field2.shape[0]):
            field2.data[i] = 10

    def test_field_uniqueness(self):
        # create mesh
        parallel = False
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        mesh = None
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50()

        field = NewField(mesh, 'Field!',
                         TypeKind.I4,
                         MeshLoc.NODE)

        field2 = NewField(mesh, 'Field!',
                          typekind=TypeKind.I4,
                          meshloc=MeshLoc.ELEMENT)

        for i in range(field.shape[0]):
            field.data[i] = 10

        for i in range(field2.shape[0]):
            field2.data[i] = 10

        assert (field.struct.ptr != field2.struct.ptr)

    def test_field_switchedindices_grid(self):
        # create grid
        max_index = np.array([12, 20])
        grid = Grid(max_index, num_peri_dims=1, \
                    coord_sys=CoordSys.SPH_RAD, \
                    staggerloc=[StaggerLoc.CENTER])

        gridtofieldmap = np.array([2, 1])

        field = NewField(grid, 'Field!', TypeKind.R8,
                         ndbounds=gridtofieldmap)
        field2 = NewField(grid, 'Field!',
                          ndbounds=np.array([2, 1]))

        field.data[...] = 10

        field2.data[...] = 10

    def test_field_switchedindices_mesh(self):
        # create mesh
        parallel = False
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        mesh = None
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50()

        field = NewField(mesh, 'Field!',
                         TypeKind.R8,
                         MeshLoc.NODE)
        field2 = NewField(mesh, 'Field!',
                          meshloc=MeshLoc.ELEMENT,
                          ndbounds=np.array([2, 5]))

        field.data[...] = 10

        field2.data[...] = 10

    def test_field_extraindices_mesh(self):
        # create mesh
        parallel = False
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        mesh = None
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50()

        field = NewField(mesh, 'Field!',
                         TypeKind.R8,
                         MeshLoc.NODE)
        field2 = NewField(mesh, 'Field!',
                          meshloc=MeshLoc.ELEMENT,
                          ndbounds=np.array([2, 5]))

        field.data[...] = 10

        field2.data[...] = 10