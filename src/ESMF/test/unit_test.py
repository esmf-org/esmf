#!/usr/bin/env python
#
#    Exp $

"""
unit test file
"""

import sys
import traceback
import logging

try:
    from ESMF import *
except:
    traceback.print_exc(file=sys.stdout)
    raise ImportError('The ESMF library cannot be found!')

try:
    import numpy as np
except:
    raise ImportError('The Numpy library cannot be found!')

def make_index( nx, ny ):
    '''
    Return an array, of size (ny, nx) that is
    an index, ie runs from 0 to (nx*ny)-1
    '''
    import numpy as np
    
    return np.arange(constants. nx*ny ).reshape( ny, nx )

def make_index_3D( nx, ny, nz ):
    '''
    Return an array, of size (nz, ny, nx) that is
    an index, ie runs from 0 to (nx*ny*nz)-1
    '''
    import numpy as np
    
    return np.arange( nx*ny*nz ).reshape( nz, ny, nx )

def test_vm():
    # inquire for rank and proc from ESMF Virtual Machine
    localPet = get_localPet()
    petCount = get_petCount()

    print '\nlocalPet = {0}\n'.format(localPet)
    print '\npetCount = {0}\n'.format(petCount)

    # return True from unit test
    return True

def test_grid_create():

    max_index = np.array([12,20])

    print "GridCreate 1"
    grid = Grid(max_index)

    print "GridCreate 2"
    grid2 = Grid(max_index, num_peri_dims=0, coord_sys=CoordSys.SPH_RAD,
                 coord_typekind=TypeKind.R4)
    print "GridCreate 3"
    grid3 = Grid(max_index, num_peri_dims=1)

    print "GridCreate 4"
    grid4 = Grid(max_index, num_peri_dims=1, coord_sys=CoordSys.SPH_DEG,
                 coord_typekind=TypeKind.R4)

    del grid
    del grid2
    del grid3
    del grid4

    # return True from unit test
    return True

def test_grid_create_3D():

    max_index = np.array([12,20,37])

    print "GridCreate 1"
    grid = Grid(max_index)

    print "GridCreate 2"
    grid2 = Grid(max_index, num_peri_dims=0, coord_sys=CoordSys.SPH_RAD,
                 coord_typekind=TypeKind.R4)

    print "GridCreate 3"
    grid3 = Grid(max_index, num_peri_dims=1)

    print "GridCreate 4"
    grid4 = Grid(max_index, num_peri_dims=1, coord_sys=CoordSys.SPH_DEG,
                 coord_typekind=TypeKind.R4)

    del grid
    del grid2
    del grid3
    del grid4

    # return True from unit test
    return True

def test_grid_coords():

    max_index = np.array([12,20])

    grid = Grid(max_index)

    # Add coordinates
    grid.add_coords(staggerloc=[StaggerLoc.CENTER])

    exLB = grid.lower_bounds[StaggerLoc.CENTER]
    exUB = grid.upper_bounds[StaggerLoc.CENTER]


    print "Grid Bounds:"
    print "    exclusiveLBounds = "+str(exLB)
    print type(exLB)
    print "    exclusiveUBounds = "+str(exUB)
    print exUB.dtype

    # get and set the coordinates
    [x,y] = [0, 1]
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
            if gridXCoord_check[i,j] != float(i):
                correct = False
                print "FAIL - gridXCoord[,"+str(i)+","+str(j)+"] = "+\
                      str(gridXCoord_check[i,j])

    for i in xrange(gridYCoord_check.shape[x]):
        for j in xrange(gridYCoord_check.shape[y]):
            if gridYCoord_check[i,j] != float(j):
                correct = False
                print "FAIL - gridYCoord[,"+str(i)+","+str(j)+"] = "+\
                      str(gridYCoord_check[i,j])

    grid.write("gridcoords2DCart")

    # return correct from unit test
    return correct

def test_grid_coords_3D():

    max_index = np.array([10,20,30])

    grid = Grid(max_index, coord_sys=CoordSys.CART)

    # Add coordinates
    grid.add_coords(staggerloc=[StaggerLoc.CENTER])

    exLB = grid.lower_bounds[StaggerLoc.CENTER]
    exUB = grid.upper_bounds[StaggerLoc.CENTER]

    print "Grid Bounds:"
    print "    exclusiveLBounds = "+str(exLB)
    print type(exLB)
    print "    exclusiveUBounds = "+str(exUB)
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

    correct = True
    for i in xrange(gridXCoord_check.shape[x]):
        for j in xrange(gridXCoord_check.shape[y]):
            for k in xrange(gridXCoord_check.shape[z]):
                if gridXCoord_check[i, j, k] != float(i):
                    correct = False
                    print "FAIL - gridXCoord[,"+\
                          str(i)+","+str(j)+","+str(k)+"] = "+\
                          str(gridXCoord_check[i, j, k])

    for i in xrange(gridYCoord_check.shape[x]):
        for j in xrange(gridYCoord_check.shape[y]):
            for k in xrange(gridYCoord_check.shape[z]):
                if gridYCoord_check[i, j, k] != float(j):
                    correct = False
                    print "FAIL - gridYCoord[,"+str(i)+","+\
                          str(j)+","+str(k)+"] = "+\
                          str(gridYCoord_check[i, j, k])

    for i in xrange(gridZCoord_check.shape[x]):
        for j in xrange(gridZCoord_check.shape[y]):
            for k in xrange(gridZCoord_check.shape[z]):
                if gridZCoord_check[i, j, k] != float(k):
                    correct = False
                    print "FAIL - gridZCoord[,"+str(i)+","+\
                          str(j)+","+str(k)+"] = "+\
                          str(gridZCoord_check[i, j, k])

    grid.write("gridcoord3DCart")

    # return correct from unit test
    return correct

def test_grid_mask():

    correct = True
    
    max_index = np.array([12,20])

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
                mask[i, j] = 0;

    # create a Field on the Grid, should inherit the mask
    field = Field(grid, "FIELD!")
    assert(field.mask.any() == 1)

    field.mask[0,3] = True
     
    # the nature of MaskedArray is that the Field will inherit a copy
    # of the Grid's mask, so changing the Field mask will not change the
    # original Grid mask.
     
    #if(mask[0,3] != 1):
    #    correct = False

    #import pdb; pdb.set_trace()

    # return True from unit test
    return correct

def test_grid_mask_3D():

    max_index = np.array([10,20,30])

    grid = Grid(max_index, coord_sys=CoordSys.CART)

    # Add coordinates
    grid.add_coords(staggerloc=[StaggerLoc.CENTER])

    # Add Mask
    mask = grid.add_item(GridItem.MASK)

    [x, y, z] = [0, 1, 2]
    for i in range(mask.shape[x]):
        if (i == 2.0):
            mask[i, :, :] = 1
        else:
            mask[i, :, :] = 0;

    # return True from unit test
    return True

def test_grid_area():

    max_index = np.array([12,20])

    grid = Grid(max_index)

    # Add coordinates
    grid.add_coords(staggerloc=[StaggerLoc.CENTER])
    exLB = grid.lower_bounds[StaggerLoc.CENTER]
    exUB = grid.upper_bounds[StaggerLoc.CENTER]

    # Add Areas
    area = grid.add_item(GridItem.AREA)

    area[:] = 1.0

    # return True from unit test
    return True

def test_grid_area_3D():

    max_index = np.array([10,20,30])

    grid = Grid(max_index, coord_sys=CoordSys.CART)

    # Add coordinates
    grid.add_coords(staggerloc=[StaggerLoc.CENTER])
    exLB = grid.lower_bounds[StaggerLoc.CENTER]
    exUB = grid.upper_bounds[StaggerLoc.CENTER]

    # Add Areas
    area = grid.add_item(GridItem.AREA)

    area[:] = 1.0

    # return True from unit test
    return True

@expected_failure
def test_grid_write_empty():

    # this test should fail
    try:
        max_index = np.array([12,20])

        grid = Grid(max_index)

        grid.write("writebeforeempty")

    except:
        raise TestGridWriteBeforeCoords("FAIL")
        
    # return True from unit test
    return True

def test_grid_field():

    # create a Grid with center staggers
    max_index = np.array([12,20])
    grid = Grid(max_index, staggerloc=[StaggerLoc.CENTER])

    # create a Field on the Grid
    field = Field(grid, "FIELD!")

    for i in range(field.shape[0]):
        for j in range(field.shape[1]):
            field[i, j] = 10

    # return True from unit test
    return True

def test_grid_field_edge1():

    # create a Grid with center staggers
    max_index = np.array([12,20])
    grid = Grid(max_index, staggerloc=[StaggerLoc.EDGE1])

    # create a Field on the Grid
    field = Field(grid, "GRIDFIELD!", staggerloc=StaggerLoc.EDGE1)

    for i in range(field.shape[0]):
        for j in range(field.shape[1]):
            field[i, j] = 10

    # return True from unit test
    return True

def test_grid_field_edge2():

    # create a Grid with center staggers
    max_index = np.array([12,20])
    grid = Grid(max_index, staggerloc=[StaggerLoc.EDGE2])

    # create a Field on the Grid
    field = Field(grid, "GRIDFIELD!", staggerloc=StaggerLoc.EDGE2)

    for i in range(field.shape[0]):
        for j in range(field.shape[1]):
            field[i, j] = 10

    # return True from unit test
    return True

def test_grid_field_corner():

    # create a Grid with center staggers
    max_index = np.array([12,20])
    grid = Grid(max_index, staggerloc=[StaggerLoc.CORNER])

    # create a Field on the Grid
    field = Field(grid, "GRIDFIELD!", staggerloc=StaggerLoc.CORNER)

    for i in range(field.shape[0]):
        for j in range(field.shape[1]):
            field[i, j] = 10

    # return True from unit test
    return True

def test_grid_field_3D():

    # create a Grid with center staggers
    max_index = np.array([10,20,30])
    grid = Grid(max_index, coord_sys=CoordSys.CART, \
                            staggerloc=[StaggerLoc.CENTER])

    # create a Field on the Grid
    field = Field(grid, "GRIDFIELD!")

    for i in range(field.shape[0]):
        for j in range(field.shape[1]):
            field[i, j] = 10

    # return True from unit test
    return True

def test_grid_field_3D_edge1vcenter():

    # create a Grid with center staggers
    max_index = np.array([10,20,30])
    grid = Grid(max_index, coord_sys=CoordSys.CART, \
                            staggerloc=[StaggerLoc.EDGE1_VCENTER])

    # grid does not need coordinates to be ready for a field

    # create a Field on the Grid
    field = Field(grid, "GRIDFIELD!", staggerloc=StaggerLoc.EDGE1_VCENTER)

    for i in range(field.shape[0]):
        for j in range(field.shape[1]):
            field[i, j] = 10

    # return True from unit test
    return True

def test_grid_field_3D_edge2vcenter():

    # create a Grid with center staggers
    max_index = np.array([10,20,30])
    grid = Grid(max_index, coord_sys=CoordSys.CART, \
                            staggerloc=[StaggerLoc.EDGE2_VCENTER])

    # grid does not need coordinates to be ready for a field

    field = Field(grid, "GRIDFIELD!", staggerloc=StaggerLoc.EDGE2_VCENTER)

    for i in range(field.shape[0]):
        for j in range(field.shape[1]):
            field[i, j] = 10

    # return True from unit test
    return True

def test_grid_field_3D_cornervcenter():

    # create a Grid with center staggers
    max_index = np.array([10,20,30])
    grid = Grid(max_index, coord_sys=CoordSys.CART, \
                staggerloc=[StaggerLoc.CORNER_VCENTER])

    # grid does not need coordinates to be ready for a field

    # create a Field on the Grid
    field = Field(grid, "GRIDFIELD!", staggerloc=StaggerLoc.CORNER_VCENTER)

    for i in range(field.shape[0]):
        for j in range(field.shape[1]):
            field[i, j] = 10

    # return True from unit test
    return True

def test_grid_field_3D_centervface():

    # create a Grid with center staggers
    max_index = np.array([10,20,30])
    grid = Grid(max_index, coord_sys=CoordSys.CART, \
                staggerloc=[StaggerLoc.CENTER_VFACE])

    # create a Field on the Grid
    field = Field(grid, "GRIDFIELD!", staggerloc=StaggerLoc.CENTER_VFACE)

    for i in range(field.shape[0]):
        for j in range(field.shape[1]):
            field[i, j] = 10

    # return True from unit test
    return True

def test_grid_field_3D_edge1vface():

    # create a Grid with center staggers
    max_index = np.array([10,20,30])
    grid = Grid(max_index, coord_sys=CoordSys.CART, \
                staggerloc=[StaggerLoc.EDGE1_VFACE])

    # create a Field on the Grid
    field = Field(grid, "GRIDFIELD!", staggerloc=StaggerLoc.EDGE1_VFACE)

    for i in range(field.shape[0]):
        for j in range(field.shape[1]):
            field[i, j] = 10

    # return True from unit test
    return True

def test_grid_field_3D_edge2vface():

    # create a Grid with center staggers
    max_index = np.array([10,20,30])
    grid = Grid(max_index, coord_sys=CoordSys.CART, \
                staggerloc=[StaggerLoc.EDGE2_VFACE])

    # create a Field on the Grid
    field = Field(grid, "GRIDFIELD!", staggerloc=StaggerLoc.EDGE2_VFACE)

    for i in range(field.shape[0]):
        for j in range(field.shape[1]):
            field[i, j] = 10

    # return True from unit test
    return True

def test_grid_field_3D_cornervface():

    # create a Grid with center staggers
    max_index = np.array([10,20,30])
    grid = Grid(max_index, coord_sys=CoordSys.CART, \
                staggerloc=[StaggerLoc.CORNER_VFACE])

    # create a Field on the Grid
    field = Field(grid, "GRIDFIELD!", staggerloc=StaggerLoc.CORNER_VFACE)

    for i in range(field.shape[0]):
        for j in range(field.shape[1]):
            field[i, j] = 10

    # return True from unit test
    return True

def mesh_create_2x2():
    '''
    PRECONDITIONS: A Mesh has been declared.
    POSTCONDITIONS: A 2x2 Mesh has been created.
    
                   2x2 Mesh
    
    
      2.0   7 ------- 8 -------- 9
            |         |          |
            |    3    |    4     |
            |         |          |
      1.0   4 ------- 5 -------- 6
            |         |          |
            |    1    |    2     |
            |         |          |
      0.0   1 ------- 2 -------- 3
    
           0.0       1.0        2.0
    
          Node Ids at corners
          Element Ids in centers
    
          (Everything owned by PET 0)
    '''
    # set up a simple mesh
    num_node = 9
    num_elem = 4
    nodeId = np.array([1,2,3,4,5,6,7,8,9])
    # change to 0 based indexing
    nodeCoord = np.array([0.0,0.0,
                          1.0,0.0,
                          2.0,0.0,
                          0.0,1.0,
                          1.0,1.0,
                          2.0,1.0,
                          0.0,2.0,
                          1.0,2.0,
                          2.0,2.0])
    nodeOwner = np.zeros(num_node)

    elemId = np.array([1,2,3,4])
    elemType = np.ones(num_elem)
    elemType*=MeshElemType.QUAD
    elemConn = np.array([0,1,4,3,
                         1,2,5,4,
                         3,4,7,6,
                         4,5,8,7])

    mesh = Mesh(parametric_dim=2, spatial_dim=2)

    mesh.add_nodes(num_node, nodeId, nodeCoord, nodeOwner)

    mesh.add_elements(num_elem, elemId, elemType, elemConn)

    return mesh, nodeCoord


def test_mesh():
    status = True
    mesh, nodeCoord = mesh_create_2x2()
    
    element_count = mesh.size_local[element]
    print 'local element_count = '+str(element_count)+'\n'

    node_count = mesh.size_local[node]
    print 'local node_count = '+str(node_count)+'\n'

    element_count = mesh.size[element]
    print 'owned element_count = '+str(element_count)+'\n'

    node_count = mesh.size[node]
    print 'owned node_count = '+str(node_count)+'\n'
    
    coords, num_nodes = ESMP_MeshGetCoordPtr(mesh.struct.ptr)
    print 'ESMP_MeshGetCoordPtr() - num_nodes = '+str(num_nodes)+'\n'
    status = all([coords[i] == nodeCoord[i] for i in range(num_nodes*2)])

    # this call fails if nodes and elements have not been added first
    #mesh.free_memory()

    # return status from unit test (True if everything passed)
    return status

def test_meshvtk():
    mesh, _ = mesh_create_2x2()

    mesh.write("mesh")

    # return True from unit test
    return True

def test_interfaceint():
    Narray = np.array([4,5,6], dtype=np.int32)
    interfaceint = ESMP_InterfaceIntCreate(Narray, len(Narray))

    ESMP_InterfaceIntDestroy(interfaceint)
    # return True from unit test
    return True

@expected_failure
def test_interfaceint2():
    # This test should fail
    try:
        a = (ct.c_int*3)()
        a = [1,2,3]
        print a
        interfaceint2 = ESMP_InterfaceIntCreate(a, 3)
        ESMP_InterfaceIntDestroy(interfaceint2)
    except:
        raise TypeError('FAIL: tuples cannot be used in place of numpy.array')
    # return True from unit test
    return True

@expected_failure
def test_interfaceint3():
    # This test should fail
    try:
        interfaceint2 = ESMP_InterfaceIntCreate(array([1,2,3]), 3)
        ESMP_InterfaceIntDestroy(interfaceint2)
    except:
        raise TypeError('FAIL: tuples cannot be used in place of numpy.array')

    # return True from unit test
    return True

def test_field_r8_grid():
    # create grid
    max_index = np.array([12,20])
    grid = Grid(max_index, num_peri_dims=1, coord_sys=CoordSys.SPH_RAD, \
                staggerloc=[StaggerLoc.CENTER])

    field = Field(grid, 'Field!', TypeKind.R8)
    field2 = Field(grid, 'Field!')

    for i in range(field.shape[0]):
        for j in range(field.shape[1]):
            field[i, j] = 10


    for i in range(field2.shape[0]):
        for j in range(field2.shape[1]):
            field2[i, j] = 10

    # return True from unit test
    return True

def test_field_r4_grid():
    # create grid
    max_index = np.array([12,20])
    grid = Grid(max_index, num_peri_dims=1, \
                coord_sys=CoordSys.SPH_RAD, \
                coord_typekind=TypeKind.R4, \
                staggerloc=[StaggerLoc.CENTER])

    field = Field(grid, 'Field!', typekind=TypeKind.R4)

    field2 = Field(grid, 'Field!', typekind=TypeKind.R4)

    for i in range(field.shape[0]):
        for j in range(field.shape[1]):
            field[i, j] = 10

    for i in range(field2.shape[0]):
        for j in range(field2.shape[1]):
            field2[i, j] = 10

    # return True from unit test
    return True

def test_field_i8_grid():
    # create grid
    max_index = np.array([12,20])
    grid = Grid(max_index, num_peri_dims=1, \
                coord_sys=CoordSys.SPH_RAD,\
                coord_typekind=TypeKind.R4, \
                staggerloc=[StaggerLoc.CENTER])

    field = Field(grid, 'Field!', TypeKind.I8)

    field2 = Field(grid, 'Field!', typekind=TypeKind.I8)

    for i in range(field.shape[0]):
        for j in range(field.shape[1]):
            field[i, j] = 10

    for i in range(field2.shape[0]):
        for j in range(field2.shape[1]):
            field2[i, j] = 10

    # return True from unit test
    return True

def test_field_i4_grid():
    # create grid
    max_index = np.array([12,20])
    grid = Grid(max_index, num_peri_dims=1, \
                coord_sys=CoordSys.SPH_RAD, \
                coord_typekind=TypeKind.R4, \
                staggerloc=[StaggerLoc.CENTER])

    field = Field(grid, 'Field!', TypeKind.I4)

    field2 = Field(grid, 'Field!', typekind=TypeKind.I4)

    for i in range(field.shape[0]):
        for j in range(field.shape[1]):
            field[i, j] = 10

    for i in range(field2.shape[0]):
        for j in range(field2.shape[1]):
            field2[i, j] = 10

    # return True from unit test
    return True

def test_field_r8_mesh():
    # create mesh
    mesh = mesh_create_2x2()[0]

    field = Field(mesh, 'Field!', \
                  TypeKind.R8, \
                  MeshLoc.NODE)
    print "field1 created"
    field2 = Field(mesh, 'Field!',
                   meshloc=MeshLoc.ELEMENT)
    print "field2 created"

    for i in range(field.shape[0]):
        field[i] = 10

    for i in range(field2.shape[0]):
        field2[i] = 10

    # return True from unit test
    return True

def test_field_r4_mesh():
    # create mesh
    mesh = mesh_create_2x2()[0]

    field = Field(mesh, 'Field!',
                  typekind=TypeKind.R4)

    field2 = Field(mesh, 'Field!',
                   typekind=TypeKind.R4,
                   meshloc=MeshLoc.ELEMENT)

    for i in range(field.shape[0]):
        field[i] = 10

    for i in range(field2.shape[0]):
        field2[i] = 10

    # return True from unit test
    return True

def test_field_i8_mesh():
    # create mesh
    mesh = mesh_create_2x2()[0]

    field = Field(mesh, 'Field!',
                  TypeKind.I8,
                  MeshLoc.NODE)

    field2 = Field(mesh, 'Field!',
                   typekind=TypeKind.I8,
                   meshloc=MeshLoc.ELEMENT)

    for i in range(field.shape[0]):
        field[i] = 10

    for i in range(field2.shape[0]):
        field2[i] = 10

    # return True from unit test
    return True

def test_field_i4_mesh():
    # create mesh
    mesh = mesh_create_2x2()[0]

    field = Field(mesh, 'Field!',
                  TypeKind.I4,
                  MeshLoc.NODE)

    field2 = Field(mesh, 'Field!',
                   typekind=TypeKind.I4,
                   meshloc=MeshLoc.ELEMENT)

    for i in range(field.shape[0]):
        field[i] = 10

    for i in range(field2.shape[0]):
        field2[i] = 10

    # return True from unit test
    return True

def test_field_uniqueness():
    # create mesh
    mesh = mesh_create_2x2()[0]

    field = Field(mesh, 'Field!',
                  TypeKind.I4,
                  MeshLoc.NODE)

    field2 = Field(mesh, 'Field!',
                   typekind=TypeKind.I4,
                   meshloc=MeshLoc.ELEMENT)

    for i in range(field.shape[0]):
        field[i] = 10

    for i in range(field2.shape[0]):
        field2[i] = 10

    assert(field.struct.ptr != field2.struct.ptr)

    # return True from unit test
    return True

def test_field_switchedindices_grid():
    # create grid
    max_index = np.array([12,20])
    grid = Grid(max_index, num_peri_dims=1, \
                coord_sys=CoordSys.SPH_RAD, \
                staggerloc=[StaggerLoc.CENTER])

    gridtofieldmap = np.array([2,1])

    field = Field(grid, 'Field!', TypeKind.R8,
                  grid_to_field_map=gridtofieldmap)
    field2 = Field(grid, 'Field!',
                   grid_to_field_map=np.array([2,1]))

    for i in range(field.shape[0]):
        for j in range(field.shape[1]):
            field[i, j] = 10

    for i in range(field2.shape[0]):
        for j in range(field2.shape[1]):
            field2[i, j] = 10

    # return True from unit test
    return True

def test_field_switchedindices_mesh():
    # create mesh
    mesh = mesh_create_2x2()[0]

    gridtofieldmap = np.array([1])

    field = Field(mesh, 'Field!',
                  TypeKind.R8,
                  MeshLoc.NODE,
                  grid_to_field_map=gridtofieldmap)
    field2 = Field(mesh, 'Field!',
                   meshloc=MeshLoc.ELEMENT,
                   grid_to_field_map=np.array([1]))

    for i in range(field.shape[0]):
        field[i] = 10

    for i in range(field2.shape[0]):
        field2[i] = 10

    # return True from unit test
    return True

def test_field_extraindices_mesh():
    # create mesh
    mesh = mesh_create_2x2()[0]

    gridtofieldmap = np.array([1])

    field = Field(mesh, 'Field!',
                  TypeKind.R8,
                  MeshLoc.NODE,
                  grid_to_field_map=gridtofieldmap)
    field2 = Field(mesh, 'Field!',
                   meshloc=MeshLoc.ELEMENT,
                   grid_to_field_map=np.array([2]),
                   ungridded_lower_bound=np.array([0,0]),
                   ungridded_upper_bound=np.array([1,1]))

    for i in range(field.shape[0]):
        field[i] = 10

    for i in range(field2.shape[0]):
        field2[i] = 10

    # return True from unit test
    return True

def test_field_regrid():
    # create mesh
    mesh = mesh_create_2x2()[0]
    dstfield = Field(mesh, 'MESHFIELD!', meshloc=MeshLoc.ELEMENT)

    # create grid
    max_index = np.array([4,4])
    grid = Grid(max_index, coord_sys=CoordSys.CART)

    # Add coordinates
    grid.add_coords(staggerloc=[StaggerLoc.CENTER, StaggerLoc.CORNER])

    [x,y] = [0, 1]
    gridXCorner = grid.get_coords(x, staggerloc=StaggerLoc.CORNER)
    gridYCorner = grid.get_coords(y, staggerloc=StaggerLoc.CORNER)

    for i in xrange(gridXCorner.shape[x]):
        gridXCorner[i, :] = float(i)

    for j in xrange(gridYCorner.shape[y]):
        gridYCorner[:, j] = float(j)

    # create a Field on the Grid
    srcfield = Field(grid, "GRIDFIELD!")

    srcfield[:, :] = 10.

    # regridding
    rh = Regrid(srcfield, dstfield, regrid_method=RegridMethod.CONSERVE)
    dstfield = rh(srcfield, dstfield)

    # return True from unit test
    return True

def test_field_regrid_zeroregion():
    # create mesh
    mesh = mesh_create_2x2()[0]

    # create a field on the mesh
    srcfield = Field(mesh, 'MESHFIELD!', meshloc=MeshLoc.ELEMENT)

    # initialize the source field
    for i in range(srcfield.shape[0]):
        srcfield[i] = 20.0

    # create grid
    lb_x = float(0)
    lb_y = float(0)
    ub_x = float(2)
    ub_y = float(2)

    cellwidth_x = 1.0
    cellwidth_y = 1.0
    cellcenter_x = cellwidth_x/2.0
    cellcenter_y = cellwidth_y/2.0

    max_index = np.array([ub_x,ub_y])
    grid = Grid(max_index, coord_sys=CoordSys.CART)

    # Add coordinates
    grid.add_coords(staggerloc=[StaggerLoc.CENTER, StaggerLoc.CORNER])

    [x,y] = [0, 1]

    gridXCorner = grid.get_coords(x, staggerloc=StaggerLoc.CORNER)
    gridYCorner = grid.get_coords(y, staggerloc=StaggerLoc.CORNER)

    for i in xrange(gridXCorner.shape[x]):
        gridXCorner[i, :] = float(i)

    for j in xrange(gridYCorner.shape[y]):
        gridYCorner[:, j] = float(j)

    # set up the grid mask
    grid.add_item(GridItem.MASK)

    mask = grid.get_item(GridItem.MASK)

    for i in range(mask.shape[x]):
        if (i == 2.0):
            mask[i, :] = 1
        else:
            mask[i, :] = 0;

    # create a Field on the Grid
    dstfield = Field(grid, "GRIDFIELD!")

    # initialize the destination field according to the mask
    dstfield[:, :] = -1

    # regridding
    rh = Regrid(srcfield, dstfield, regrid_method=RegridMethod.CONSERVE,
                dst_mask_values=np.array([1]))
    dstfield = rh(srcfield, dstfield, zero_region=Region.SELECT)

    # validate that the masked values were not zeroed out
    for i in range(mask.shape[x]):
        for j in range(mask.shape[y]):
            if mask[i, j] == 1:
                if dstfieldPtr[i, j] >= 0:
                    print "DING: {0}".format(dstfield[i, j])
                    return False

    # return True from unit test
    return True

def test_field_regrid_area():
    # create mesh
    mesh = mesh_create_2x2()[0]

    # create grid
    max_index = np.array([4,4])
    grid = Grid(max_index, coord_sys=CoordSys.CART)

    # Add coordinates
    grid.add_coords(staggerloc=[StaggerLoc.CENTER, StaggerLoc.CORNER])

    [x,y] = [0, 1]

    gridXCorner = grid.get_coords(x, staggerloc=StaggerLoc.CORNER)
    gridYCorner = grid.get_coords(y, staggerloc=StaggerLoc.CORNER)

    for i in xrange(gridXCorner.shape[x]):
        gridXCorner[i, :] = float(i)

    for j in xrange(gridYCorner.shape[y]):
        gridYCorner[:, j] = float(j)

    # create area field
    dstarea = Field(mesh, 'DESTINATION AREAS!',
                    meshloc=MeshLoc.ELEMENT)
    dstarea.get_area()

    # create a Field on the Grid
    srcarea = Field(grid, "SOURCE AREAS!")
    srcarea.get_area()

    correct = True

    for i in range(srcarea.shape[x]):
        for j in range(srcarea.shape[y]):
            if (srcarea[i, j] != 1):
                correct = False

    for i in range(dstarea.shape[0]):
        if (dstarea[i] != 1):
            correct = False

    # return correct from unit test
    return correct

def test_log():

    flush = True
    ESMP_LogSet(flush)

    # return True from unit test
    return True

def test_version_compare():
    correct = True
    # should be -1
    if version_compare("ESMF_5_3_0_ESMP_02","ESMF_5_3_0_ESMP_01") != 1:
        correct = False
    if version_compare("ESMF_5_3_0_ESMP_01",
                       "ESMF_5_3_1_beta_snapshot_02_ESMP_01") != -1:
        correct = False
    if version_compare("ESMF_5_3_0_ESMP_01",
                       "ESMF_5_3_0_beta_snapshot_42_ESMP_01") != 1:
        correct = False
    if version_compare("ESMF_5_3_0_ESMP_01",
                       "ESMF_5_3_0_beta_snapshot_37_ESMP_02") != 1:
        correct = False
    if version_compare("ESMF_5_3_0_ESMP_01",
                       "ESMF_5_3_1_beta_snapshot_02_ESMP_01") != -1:
        correct = False
    if version_compare("ESMF_5_3_0_ESMP_01",
                       "ESMF_6_1_0_beta_snapshot_00_ESMP_01") != -1:
        correct = False
    if version_compare("ESMF_6_1_0_beta_snapshot_00_ESMP_01",
                       "ESMF_5_3_1_beta_snapshot_02_ESMP_01") != 1:
        correct = False
    if version_compare("ESMF_6_1_0_beta_snapshot_00_ESMP_01",
                       "ESMF_6_1_0_beta_snapshot_00_ESMP_01") != 0:
        correct = False
    if version_compare("ESMPy_620b10_04",
                       "ESMF_6_1_0_beta_snapshot_00_ESMP_01") != 1:
        correct = False

    # return correct from unit test
    return correct

def main():
    # make an esmp object that won't go out of scope
    esmp = Manager(logkind=LogKind.SINGLE, debug=True)

    try:
        vm = ESMP_VMGetGlobal()
    except:
        return

    # set up a dictionary with the names of the tests to be run
    dispatch = {
    (1,'LogSet flush immediately') : test_log,
    (2,'VMGetGlobal, VMPrint, and VMGet') : test_vm,
    (3,'Mesh create and destroy') : test_mesh,
    (4,'Grid create and destroy') : test_grid_create,
    (4.1,'Grid 3D create and destroy') : test_grid_create_3D,
    (4.2,'Grid coordinates') : test_grid_coords,
    (4.3,'Grid 3D coordinates') : test_grid_coords_3D,
    (4.4,'Grid masking') : test_grid_mask,
    (4.5,'Grid 3D masking') : test_grid_mask_3D,
    (4.6,'Grid area') : test_grid_area,
    (4.7,'Grid 3D area') : test_grid_area_3D,
    (4.8,'Grid write empty') : test_grid_write_empty,
    (5.0,'Grid field creation') : test_grid_field,
    (5.1,'Grid field creation edge1 stagger') : test_grid_field_edge1,
    (5.2,'Grid field creation edge2 stagger') : test_grid_field_edge2,
    (5.3,'Grid field creation corner stagger') : test_grid_field_corner,
    (6.0,'Grid 3D field creation') : test_grid_field_3D,
    (6.1,'Grid 3D field creation edge1_vcenter stagger') : test_grid_field_3D_edge1vcenter,
    (6.2,'Grid 3D field creation edge2_vcenter stagger') : test_grid_field_3D_edge2vcenter,
    (6.3,'Grid 3D field creation corner_vcenter stagger') : test_grid_field_3D_cornervcenter,
    (6.4,'Grid 3D field creation center_vface stagger') : test_grid_field_3D_centervface,
    (6.5,'Grid 3D field creation edge1_vface stagger') : test_grid_field_3D_edge1vface,
    (6.6,'Grid 3D field creation edge2_vface stagger') : test_grid_field_3D_edge2vface,
    (6.7,'Grid 3D field creation_corner_vface stagger') : test_grid_field_3D_cornervface,
    (7,'InterfaceInt') : test_interfaceint,
    (7.1,'InterfaceInt2') : test_interfaceint2,
    (7.2,'InterfaceInt3') : test_interfaceint3,
    (8,'Field create and destroy R8 from grid') : test_field_r8_grid,
    (8.1,'Field create and destroy R4 from grid') : test_field_r4_grid,
    (8.2,'Field create and destroy I8 from grid') : test_field_i8_grid,
    (8.3,'Field create and destroy I4 from grid') : test_field_i4_grid,
    (8.4,'Field create and destroy R8 from mesh') : test_field_r8_mesh,
    (8.5,'Field create and destroy R4 from mesh') : test_field_r4_mesh,
    (8.6,'Field create and destroy I8 from mesh') : test_field_i8_mesh,
    (8.7,'Field create and destroy I4 from mesh') : test_field_i4_mesh,
    (8.71,'Field uniqueness') : test_field_uniqueness,
    (8.8,'Field create and destroy with switched indices from grid') : test_field_switchedindices_grid,
    (8.9,'Field create and destroy with switched indices from mesh') : test_field_switchedindices_mesh,
    (12,'Field regridding') : test_field_regrid,
    (12.1,'Field regridding with zeroregion') : test_field_regrid_zeroregion,
    (13.1,'Field regrid areas') : test_field_regrid_area,
    (14,'version comparison') : test_version_compare}

    # here we run the tests that are in the dispatch dictionary
    for test in sorted(dispatch.keys()):
        try:
            print '\n'+test[1]+' - START\n'
            correct = dispatch[test]()
            print test[1]+' - FINISH\n\n'
            if correct:
                print 'RESULT: PASS\n\n'
            else:
                print 'RESULT: FAIL\n\n'
        except Exception, err:
            traceback.print_exc(file=sys.stdout)
            tb = traceback.format_exc()
            if ('SkipTest' in tb):
                print test[1]+' - FINISH\n\n'\
                        'RESULT: SKIP\n\n'
            else:
                print test[1]+' - FINISH\n\n'\
                        'RESULT: FAIL\n\n'

    return 0

if __name__ == '__main__':
    sys.exit(main())
