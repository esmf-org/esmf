# $Id$

"""
Two Field objects are created, one on a Grid and the other on a Mesh.  
The source Field is set to an analytic function, and a conservative 
regridding operation is performed from the source to the destination 
Field.  After the regridding is completed, the destination Field is 
compared to the exact solution over that domain.
"""

import sys

try:
    import numpy as np
except:
    raise ImportError('The Numpy library cannot be found!')

try:
    import ESMF
except:
    raise ImportError('The ESMF library cannot be found!')

def grid_create():
    '''
    PRECONDITIONS: ESMPy has been initialized.
    POSTCONDITIONS: A Grid has been created.
    '''
    ub_x = float(4)
    ub_y = float(4)

    lb_x = float(0)
    lb_y = float(0)
    
    max_x = float(4)
    max_y = float(4)

    min_x = float(0)
    min_y = float(0)

    cellwidth_x = (max_x-min_x)/(ub_x-lb_x)
    cellwidth_y = (max_y-min_y)/(ub_y-lb_y)
    
    cellcenter_x = cellwidth_x/2
    cellcenter_y = cellwidth_y/2
    
    max_index = np.array([ub_x,ub_y])

    grid = ESMF.Grid(max_index, coord_sys=ESMF.CoordSys.CART)

    ##     CORNERS
    grid.add_coords(staggerloc=[ESMF.StaggerLoc.CORNER])

    # get the coordinate pointers and set the coordinates
    [x,y] = [0,1]
    gridCorner = grid.coords[ESMF.StaggerLoc.CORNER]
    
    for i in xrange(gridCorner[x].shape[x]):
        gridCorner[x][i, :] = float(i)*cellwidth_x + lb_x    
 
    for j in xrange(gridCorner[y].shape[y]):
        gridCorner[y][:, j] = float(j)*cellwidth_y + lb_y

    ##     CENTERS
    grid.add_coords(staggerloc=[ESMF.StaggerLoc.CENTER])

    # get the coordinate pointers and set the coordinates
    [x,y] = [0,1]
    gridCenter = grid.coords[ESMF.StaggerLoc.CENTER]
    
    for i in xrange(gridCenter[x].shape[x]):
        gridCenter[x][i, :] = float(i)*cellwidth_x + lb_x + cellwidth_x/2.0    
 
    for j in xrange(gridCenter[y].shape[y]):
        gridCenter[y][:, j] = float(j)*cellwidth_y + lb_y + cellwidth_y/2.0

    return grid

def create_ESMPmesh_3x3():    
    """
    PRECONDITIONS: ESMPy has been initialized.
    POSTCONDITIONS: A Mesh (3x3) has been created and returned as 
                    'mesh'.

                 3x3 Mesh
   
  
        4.0   41 ------ 42 ------- 43 ------ 44 
              |         |          |  331 /  |  
              |    31   |    32    |    /    |  
              |         |          |  /  332 |  
        2.5   31 ------ 32 ------- 33 ------ 34 
              |         |          |         |  
              |    21   |    22    |   23    |  
              |         |          |         |  
        1.5   21 ------ 22 ------- 23 ------ 24 
              |         |          |         |  
              |    11   |    12    |   13    |  
              |         |          |         |  
        0.0   11 ------ 12 ------- 13 ------ 14 
       
             0.0       1.5        2.5       4.0
    
        Node Ids at corners
        Element Ids in centers
  
        (Everything owned by PET 0) 
    """
    # set up a simple mesh
    num_node = 16
    num_elem = 10
    nodeId = np.array([11,12,13,14,21,22,23,24,31,32,33,34,41,42,43,44])
    nodeCoord = np.array([0.0,0.0, 1.5,0.0, 2.5,0.0, 4.0,0.0,
                          0.0,1.5, 1.5,1.5, 2.5,1.5, 4.0,1.5,
                          0.0,2.5, 1.5,2.5, 2.5,2.5, 4.0,2.5,
                          0.0,4.0, 1.5,4.0, 2.5,4.0, 4.0,4.0])
    nodeOwner = np.zeros(num_node)
    elemId = np.array([11,12,13,21,22,23,31,32,331,332])
    elemType=np.array([ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.TRI,
                       ESMF.MeshElemType.TRI])
    elemConn = np.array([0,1,5,4,
                         1,2,6,5,
                         2,3,7,6,
                         4,5,9,8,
                         5,6,10,9,
                         6,7,11,10,
                         8,9,13,12,
                         9,10,14,13,
                         10,15,14,
                         10,11,15])

    mesh = ESMF.Mesh(parametric_dim=2, spatial_dim=2)

    mesh.add_nodes(num_node, nodeId, nodeCoord, nodeOwner)
    
    mesh.add_elements(num_elem, elemId, elemType, elemConn)
    
    mesh._write("mesh")
    
    return mesh, nodeCoord, elemType, elemConn

def create_fieldgrid(grid, name):
    '''
    PRECONDITIONS: A Grid has been created, and 'name' is a string that 
                   will be used to initialize the name of a new Field.
    POSTCONDITIONS: A Field has been created.
    '''
    # defaults to center staggerloc
    field = ESMF.Field(grid, name)

    return field

def build_analyticfieldgrid(field, grid):
    '''
    PRECONDITIONS: A Field has been created.
    POSTCONDITIONS: The 'field' has been initialized to an analytic 
                    field.
    '''
    # get the coordinate pointers and set the coordinates
    [x,y] = [0,1]
    gridXCoord = grid.get_coords(x, ESMF.StaggerLoc.CENTER)
    gridYCoord = grid.get_coords(y, ESMF.StaggerLoc.CENTER)

    for i in range(gridXCoord.shape[x]):
        for j in range(gridYCoord.shape[y]):
            field[i, j] = 20.0 + gridXCoord[i, j] + gridYCoord[i, j]

    #fieldPtr[:] = 20.0+gridXCoord.flat[:]+gridYCoord.flat[:]

    return field

def create_field(mesh, name):
    '''
    PRECONDITIONS: A Mesh has been created, and 'name' is a string that 
                   will be used to initialize the name of a new Field.
    POSTCONDITIONS: A Field has been created.
    '''
    field = ESMF.Field(mesh, name, meshloc=ESMF.MeshLoc.ELEMENT)

    return field

def build_analyticfield(field, nodeCoord, elemType, elemConn):
    '''
    PRECONDITIONS: A Field has been created.
    POSTCONDITIONS: The 'field' has been initialized to an analytic 
                    field.
    '''
    # set the field to a vanilla initial field for now
    #print "Mesh center coordinates"
    offset = 0
    for i in range(field.shape[0]):    # this routine assumes element fields
        if (elemType[i] == ESMF.MeshElemType.TRI):
            x1 = nodeCoord[(elemConn[offset])*2]
            x2 = nodeCoord[(elemConn[offset+1])*2]
            x3 = nodeCoord[(elemConn[offset+2])*2]
            y1 = nodeCoord[(elemConn[offset])*2+1]
            y2 = nodeCoord[(elemConn[offset+1])*2+1]
            y3 = nodeCoord[(elemConn[offset+2])*2+1]
            x = (x1 + x2 + x3) / 3.0
            y = (y1 + y2 + y3) / 3.0
            offset = offset + 3
        elif (elemType[i] == ESMF.MeshElemType.QUAD):
            x1 = nodeCoord[(elemConn[offset])*2]
            x2 = nodeCoord[(elemConn[offset+1])*2]
            y1 = nodeCoord[(elemConn[offset+1])*2+1]
            y2 = nodeCoord[(elemConn[offset+3])*2+1]
            x = (x1 + x2) / 2.0
            y = (y1 + y2) / 2.0
            offset = offset + 4
        else:
            raise NameError("Elem type is not supported.")

        #print '[{0},{1}] = {2}'.format(x,y,field.data[i])
        field[i] = 20.0+x+y
 
    return field

def run_regridding(srcfield, dstfield):
    '''
    PRECONDITIONS: Two Fields have been created and a regridding 
                   operation is desired from 'srcfield' to 'dstfield'.
    POSTCONDITIONS: A regridding operation has set the data on 
                    'dstfield'.
    '''
    # call the regridding functions
    regridSrc2Dst = ESMF.Regrid(srcfield, dstfield, 
                                regrid_method=ESMF.RegridMethod.CONSERVE, 
                                unmapped_action=ESMF.UnmappedAction.ERROR)
    dstfield = regridSrc2Dst(srcfield, dstfield)
 
    return dstfield

def compare_fields(field1, field2):
    '''
    PRECONDITIONS: Two Fields have been created and a comparison of the 
                   the values is desired between 'srcfield' and 
                   'dstfield'.
    POSTCONDITIONS: The values on 'srcfield' and 'dstfield' are 
                    compared.
    '''
    
    # compare point values of field1 to field2
    # first verify they are the same size
    if (field1.shape != field2.shape): 
        raise NameError('compare_fields: Fields must be the same size!')
    
    # initialize to True, and check for False point values
    correct = True
    totalErr = 0.0
    [x, y] = [0, 1]
    for i in range(field1.shape[x]):
        for j in range(field2.shape[y]):
            err = abs(field1[i, j] - field2[i, j])/abs(field2[i, j])
            if err > .06:
                correct = False
                print "ACCURACY ERROR - "+str(err)
                print "field1 = {0} : field2 = {1}\n".format(field1[i, j], \
                             field2[i, j])
            totalErr += err
    
    if correct:
        print " - PASS - Total Error = "+str(totalErr)
    else:
        print " - FAIL - Total Error = "+str(totalErr)
    
    return

def test_main():
    print "\ngrid_mesh_regrid_csrv"
    
    # start up ESMF
    # this call is not necessary unless you want to to override the
    # default options:
    #  LogKind = NONE
    #  debug = False
    #manager = ESMF.Manager(logkind=ESMF.LogKind.SINGLE, debug=True)    

    # create two unique Mesh objects
    grid = grid_create()
    mesh, nodeCoord, elemType, elemConn = create_ESMPmesh_3x3()
    
    # this is for mesh to grid
    # create Field objects on the Meshes
    srcfield = create_field(mesh, 'srcfield')        
    dstfield = create_fieldgrid(grid, 'dstfield')
    dstfield2 = create_fieldgrid(grid, 'dstfield_exact')
    
    # initialize the Fields to an analytic function
    srcfield = build_analyticfield(srcfield, nodeCoord, elemType, elemConn)
    dstfield2 = build_analyticfieldgrid(dstfield2, grid)
    
    # run the ESMF regridding
    dstfield = run_regridding(srcfield, dstfield)
        
    # compare results and output PASS or FAIL
    compare_fields(dstfield, dstfield2)
    
    print "\n"
    return 0

if __name__ == '__main__':
    sys.exit(test_main())
