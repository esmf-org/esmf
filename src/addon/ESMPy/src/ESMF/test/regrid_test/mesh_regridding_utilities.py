"""
Utilities for regridding with Meshes
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

def mesh_create_5():
    '''
    PRECONDITIONS: None
    POSTCONDITIONS: A 5 element Mesh has been created.    
    
      4.0   31 ------ 32 ------ 33
            |         |  22  /   |
            |    21   |     /    |
            |         |   /  23  |
      2.0   21 ------ 22 ------ 23
            |         |          |
            |    11   |    12    |
            |         |          |
      0.0   11 ------ 12 ------ 13
    
           0.0       2.0        4.0
    
          Node Ids at corners
          Element Ids in centers
    
    Note: This mesh is not parallel, it can only be used in serial
    '''
    # Two parametric dimensions, and two spatial dimensions
    mesh = ESMF.Mesh(parametric_dim=2, spatial_dim=2)
    
    num_node = 9
    num_elem = 5
    nodeId = np.array([11,12,13,21,22,23,31,32,33])
    nodeCoord = np.array([0.0,0.0,  # node 11
                          2.0,0.0,  # node 12
                          4.0,0.0,  # node 13
                          0.0,2.0,  # node 21
                          2.0,2.0,  # node 22
                          4.0,2.0,  # node 23
                          0.0,4.0,  # node 31
                          2.0,4.0,  # node 32
                          4.0,4.0]) # node 33
    nodeOwner = np.zeros(num_node)

    elemId = np.array([11,12,21,22,23])
    elemType=np.array([ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.TRI,
                       ESMF.MeshElemType.TRI])
    elemConn=np.array([0,1,4,3, # element 11
                       1,2,5,4, # element 12
                       3,4,7,6, # element 21
                       4,8,7,   # element 22
                       4,5,8])  # element 23

    mesh.add_nodes(num_node,nodeId,nodeCoord,nodeOwner)

    mesh.add_elements(num_elem,elemId,elemType,elemConn)

    return mesh, nodeCoord, elemType, elemConn

def mesh_create_10(domask=False, doarea=False):
    '''
    PRECONDITIONS: None
    POSTCONDITIONS: A 10 element Mesh has been created.    
    
      4.0   41 ------ 42 ------- 43 ------ 44
            |         |          |  33 /   |
            |    31   |    32    |    /    |
            |         |          |  /  34  |
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
    
    Note: This mesh is not parallel, it can only be used in serial
    '''
    # Two parametric dimensions, and two spatial dimensions
    mesh = ESMF.Mesh(parametric_dim=2, spatial_dim=2)
    
    num_node = 16
    num_elem = 10
    nodeId = np.array([11,12,13,14,21,22,23,24,31,32,33,34,41,42,43,44])
    nodeCoord = np.array([0.0,0.0, 1.5,0.0, 2.5,0.0, 4.0,0.0,
                          0.0,1.5, 1.5,1.5, 2.5,1.5, 4.0,1.5,
                          0.0,2.5, 1.5,2.5, 2.5,2.5, 4.0,2.5,
                          0.0,4.0, 1.5,4.0, 2.5,4.0, 4.0,4.0])
    nodeOwner = np.zeros(num_node)
    elemId = np.array([11,12,13,21,22,23,31,32,33,34])
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
    elemMask = None
    if domask:
        elemMask = np.array([0,0,0,0,1,0,0,0,0,0])
    elemArea = None
    if doarea:
        elemArea = np.array([5,5,5,5,5,5,5,5,2.5,2.5], dtype=np.float64)

    mesh.add_nodes(num_node,nodeId,nodeCoord,nodeOwner)

    mesh.add_elements(num_elem,elemId,elemType,elemConn, 
        element_mask=elemMask, element_area=elemArea)

    if domask and doarea:
        return mesh, nodeCoord, elemType, elemConn, elemMask, elemArea
    elif domask and not doarea:
        return mesh, nodeCoord, elemType, elemConn, elemMask
    elif not domask and doarea:
        return mesh, nodeCoord, elemType, elemConn, elemArea
    else:
        return mesh, nodeCoord, elemType, elemConn

def mesh_create_5_parallel (localPet):
    '''
    PRECONDITIONS: None
    POSTCONDITIONS: A 5 element Mesh has been created in parallel.
    
    #  4.0   31 ------ 32       [32] ----- 33
    #        |         |         | 22  /   |
    #        |    21   |         |    /    |
    #        |         |         |  /  23  |
    #  2.0  [21] ---- [22]      [22] ---- [23]
    #
    #       0.0       2.0       2.0       4.0
    #
    #           PET 2               PET 3
    #
    #
    #  2.0   21 ------ 22       [22] ----- 23
    #        |         |         |         |
    #        |    11   |         |    12   |
    #        |         |         |         |
    #  0.0   11 ------ 12       [12] ----- 13
    #
    #       0.0       2.0       2.0      4.0
    #
    #           PET 0               PET 1
    #
    #               Node Id labels at corners
    #              Element Id labels in centers
    '''
    # Two parametric dimensions, and two spatial dimensions
    mesh = ESMF.Mesh(parametric_dim=2, spatial_dim=2)
    
    if (localPet == 0):
        num_node=4
        num_elem=1

        nodeId=np.array([11,12,21,22])
        nodeCoord=np.array([0.0,0.0,
                            2.0,0.0,
                            0.0,2.0,
                            2.0,2.0 ])
        nodeOwner=np.zeros(num_node)
        elemId=np.array([11])
        elemType=np.array([ESMF.MeshElemType.QUAD])
        elemConn=np.array([0,1,3,2])

    elif (localPet == 1):
        num_node=4
        num_elem=1

        nodeId=np.array([12,13,22,23])
        nodeCoord=np.array([2.0,0.0,
                            4.0,0.0,
                            2.0,2.0,
                            4.0,2.0 ])
        nodeOwner=np.array([0,
                            1,
                            0,
                            1])
        elemId=np.array([12])
        elemType=np.array([ESMF.MeshElemType.QUAD])
        elemConn=np.array([0,1,3,2])

    elif (localPet == 2):
        num_node=4
        num_elem=1

        nodeId=np.array([21,22,31,32])
        nodeCoord=np.array([0.0,2.0,
                            2.0,2.0,
                            0.0,4.0,
                            2.0,4.0 ])
        nodeOwner=np.array([0,
                            0,
                            2,
                            2])
        elemId=np.array([21])
        elemType=np.array([ESMF.MeshElemType.QUAD])
        elemConn=np.array([0,1,3,2])

    elif (localPet == 3):
        num_node=4
        num_elem=2

        nodeId=np.array([22,23,32,33])
        nodeCoord=np.array([2.0,2.0,
                            4.0,2.0,
                            2.0,4.0,
                            4.0,4.0 ])
        nodeOwner=np.array([0,
                            1,
                            2,
                            3])
        elemId=np.array([22,23])
        elemType=np.array([ESMF.MeshElemType.TRI,
                           ESMF.MeshElemType.TRI])
        elemConn=np.array([0,3,2,
                           0,1,3])

    # Add nodes and elements to the Mesh
    mesh.add_nodes(num_node,nodeId,nodeCoord,nodeOwner)
    mesh.add_elements(num_elem,elemId,elemType,elemConn)

    return mesh, nodeCoord, elemType, elemConn

def mesh_create_10_parallel (localPet):
    '''
    PRECONDITIONS: None
    POSTCONDITIONS: A 10 element Mesh has been created in parallel.
    
    #  4.0   41 ------ 42 ------ 43      [43] ---------- 44
    #        |         |         |       |          /    |
    #        |         |         |       |  33   /       |
    #        |    31   |    32   |       |      /        |
    #        |         |         |       |    /    34    |
    #        |         |         |       |  /            |
    #  2.5  [31] ----- [32] ---- [33]    [33] ---------- [34]
    #
    #       0.0       1.5       2.5     2.5             4.0
    #
    #                PET 2                      PET 3
    #
    #
    #  2.5   31 ------ 32 ------ 33     [33] ----------- 34
    #        |         |         |       |               |
    #        |    21   |    22   |       |       23      |
    #        |         |         |       |               |
    #  1.5   21 ------ 22 ------ 23     [23] ----------  24
    #        |         |         |       |               |
    #        |    11   |    12   |       |       13      |
    #        |         |         |       |               |
    #  0.0   11 ------ 12 ------ 13     [13] ----------- 14
    #
    #       0.0       1.5       2.5     2.5             4.0
    #
    #                PET 0                      PET 1
    #
    #               Node Id labels at corners
    #              Element Id labels in centers
    '''
    # Two parametric dimensions, and two spatial dimensions
    mesh = ESMF.Mesh(parametric_dim=2, spatial_dim=2)
    
    if (localPet == 0):
        num_node=9
        num_elem=4

        nodeId=np.array([11,12,13,21,22,23,31,32,33])
        nodeCoord=np.array([0.0,0.0,
                            1.5,0.0,
                            2.5,0.0,
                            0.0,1.5,
                            1.5,1.5,
                            2.5,1.5,
                            0.0,2.5,
                            1.5,2.5,
                            2.5,2.5])
        nodeOwner=np.zeros(num_node)
        elemId=np.array([11,12,21,22])
        elemType=np.array([ESMF.MeshElemType.QUAD,
                           ESMF.MeshElemType.QUAD,
                           ESMF.MeshElemType.QUAD,
                           ESMF.MeshElemType.QUAD])
        elemConn=np.array([0,1,4,3,
                           1,2,5,4,
                           3,4,7,6,
                           4,5,8,7])

    elif (localPet == 1):
        num_node=6
        num_elem=2

        nodeId=np.array([13,14,23,24,33,34])
        nodeCoord=np.array([2.5,0.0,
                            4.0,0.0,
                            2.5,1.5,
                            4.0,1.5,
                            2.5,2.5,
                            4.0,2.5 ])
        nodeOwner=np.array([0,1,0,1,0,1])
        elemId=np.array([13,23])
        elemType=np.array([ESMF.MeshElemType.QUAD,
                           ESMF.MeshElemType.QUAD])
        elemConn=np.array([0,1,3,2,
                           2,3,5,4])

    elif (localPet == 2):
        num_node=6
        num_elem=2

        nodeId=np.array([31,32,33,41,42,43])
        nodeCoord=np.array([0.0,2.5,
                            1.5,2.5,
                            2.5,2.5,
                            0.0,4.0,
                            1.5,4.0,
                            2.5,4.0 ])
        nodeOwner=np.array([0,0,0,2,2,2])
        elemId=np.array([31,32])
        elemType=np.array([ESMF.MeshElemType.QUAD,
                           ESMF.MeshElemType.QUAD])
        elemConn=np.array([0,1,4,3,
                           1,2,5,4])

    elif (localPet == 3):
        num_node=4
        num_elem=2

        nodeId=np.array([33,34,43,44])
        nodeCoord=np.array([2.5,2.5,
                            4.0,2.5,
                            2.5,4.0,
                            4.0,4.0 ])
        nodeOwner=np.array([0,1,2,3])
        elemId=np.array([33,34])
        elemType=np.array([ESMF.MeshElemType.TRI,
                           ESMF.MeshElemType.TRI])
        elemConn=np.array([0,3,2,
                           0,1,3])

    # Add nodes and elements to the Mesh
    mesh.add_nodes(num_node,nodeId,nodeCoord,nodeOwner)
    mesh.add_elements(num_elem,elemId,elemType,elemConn)

    return mesh, nodeCoord, elemType, elemConn

def initialize_field_mesh(field, nodeCoord, elemType, elemConn, 
                          domask=False, elemMask=None):
    '''
    PRECONDITIONS: A Field has been created on the elements of a Mesh.
    POSTCONDITIONS: The Field has been initialized to the analytic 
                    field f(x,y) = 20.0 + x + y
    '''

    [node, element] = [0,1]

    if field.staggerloc == element:
        offset = 0
        for i in range(field.grid.size[element]):    # this loop should go through elements
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
                raise ValueError("Elem type is not supported.")

            #print '[{0},{1}] = {2}'.format(x,y,field.data[i])
            field[i] = 20.0 + x**2 +x*y

            if domask:
                # calculate field
                if (elemMask[i] == 1):
                    field[i] = 1000000000.0

    
    elif field.staggerloc == node:
        for i in range(field.grid.size[node]):    # this loop should go through elements
            x = nodeCoord[i*2]
            y = nodeCoord[i*2+1]

            #print '[{0},{1}] = {2}'.format(x,y,field.data[i])
            field[i] = 20.0 + x**2 +x*y

            if domask:
                # calculate field
                if (elemMask[i] == 1):
                    field[i] = 1000000000.0

    else:
        raise ValueError("Field staggerloc is not supported")

    return field

def compute_mass_mesh(valuefield, areafield, dofrac=False, fracfield=None):
    '''
    PRECONDITIONS: Two Fields have been created and initialized.  
                   'valuefield' contains data values of a field built 
                   on the cells of a mesh, 'areafield' contains the 
                   areas associated with the mesh cells, and 
                   'fracfield' contains the fractions of each cell
                   which contributed to a regridding operation involving
                   'valuefield.  'dofrac' is a boolean value that gives 
                   the option to not use the 'fracfield'.\n
    POSTCONDITIONS: The mass of the data field is computed.\n
    RETURN VALUES: integer :: mass \n
    '''
    mass = 0.0
    areafield.get_area()
    frac = 0
    for i in range(valuefield.shape[0]):
        if dofrac:
            mass += areafield[i] * valuefield[i] * \
                    fracfield[i]
        else:
            mass += areafield[i] * valuefield[i]

    return mass

def compare_fields_mesh(field1, field2, itrp_tol, csrv_tol, parallel=False, 
                        dstfracfield=None, mass1=None, mass2=None):
    '''
    PRECONDITIONS: Two Fields have been created and a comparison of the
                   the values is desired between 'field1' and 
                   'field2'.  The fields should be the same size on have
                   rank=1.
    POSTCONDITIONS: The values on 'field1' and 'field2' are 
                    compared against the analytic function
                    f(x,y) = 20.0 + x + y.
    '''
    # compare point values of field1 to field2
    # first verify they are the same size
    if (field1.shape != field2.shape):
        raise NameError('compare_fields: Fields must be the same size!')

    if dstfracfield is None:
        dstfracfield = np.ones(field1.shape)

    # initialize to True, and check for False point values
    correct = True
    totalErr = 0.0
    max_error = 0.0
    min_error = 1000000.0
    for i in range(field1.shape[0]):
        if (field2[i] != 0.0):
            err = abs(field1[i]/dstfracfield[i] - field2[i])/abs(field2[i])
        else:
            err = abs(field1[i]/dstfracfield[i]) - field2[i]
        totalErr += err
        if (err > max_error):
            max_error = err
        if (err < min_error):
            min_error = err

    if parallel:
        # use mpi4py to collect values
        from mpi4py import MPI
        comm = MPI.COMM_WORLD
        total_error_global = comm.reduce(totalErr, op=MPI.SUM)
        max_error_global = comm.reduce(max_error, op=MPI.MAX)
        min_error_global = comm.reduce(min_error, op=MPI.MIN)
        if (mass1 and mass2):
            mass1_global = comm.reduce(mass1, op=MPI.SUM)
            mass2_global = comm.reduce(mass2, op=MPI.SUM)
    else:
        total_error_global = totalErr
        max_error_global = max_error
        min_error_global = min_error
        mass1_global = 0
        mass2_global = 0
        if (mass1 and mass2):
            mass1_global = mass1
            mass2_global = mass2

    if ESMF.local_pet() == 0:
        if mass1_global == 0:
            csrv_error_global = abs(mass2_global - mass1_global)
        else:
            csrv_error_global = abs(mass2_global - mass1_global)/abs(mass1_global)

        itrp = False
        csrv = False
        if (total_error_global < itrp_tol):
            itrp = True
        if (csrv_error_global < csrv_tol):
            csrv = True

        if (itrp and csrv):
            print " PASS"
        else:
            print " FAIL"
        print "  Total error = "+str(total_error_global)
        print "  Max error   = "+str(max_error_global)
        print "  Min error   = "+str(min_error_global)
        print "  Csrv error  = "+str(csrv_error_global)
        print "  srcmass     = "+str(mass1_global)
        print "  dstmass     = "+str(mass2_global)
