# $Id$

"""
Two Field objects are created, both on a Mesh.  The source Field is set 
to an analytic function, and a regridding operation is performed from 
the source to the destination Field.  After the regridding is 
completed, the destination Field is compared to the exact solution over
that domain.
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
    
      2.0   31 ------ 32 ------ 33
            |         |  22  /   |
            |    21   |     /    |
            |         |   /  23  |
      1.0   21 ------ 22 ------ 23
            |         |          |
            |    11   |    12    |
            |         |          |
      0.0   11 ------ 12 ------ 13
    
           0.0       1.0        2.0
    
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
                          1.0,0.0,  # node 12
                          2.0,0.0,  # node 13
                          0.0,1.0,  # node 21
                          1.0,1.0,  # node 22
                          2.0,1.0,  # node 23
                          0.0,2.0,  # node 31
                          1.0,2.0,  # node 32
                          2.0,2.0]) # node 33
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

def mesh_create_10():
    '''
    PRECONDITIONS: None
    POSTCONDITIONS: A 10 element Mesh has been created.    
    
      2.0   41 ------ 42 ------- 43 ------ 44
            |         |          |  33 /   |
            |    31   |    32    |    /    |
            |         |          |  /  34  |
      1.0   31 ------ 32 ------- 33 ------ 34
            |         |          |         |
            |    21   |    22    |   23    |
            |         |          |         |
      0.5   21 ------ 22 ------- 23 ------ 24
            |         |          |         |
            |    11   |    12    |   13    |
            |         |          |         |
      0.0   11 ------ 12 ------- 13 ------ 14
    
           0.0       0.5        1.0       2.0
    
          Node Ids at corners
          Element Ids in centers
    
    Note: This mesh is not parallel, it can only be used in serial
    '''
    # Two parametric dimensions, and two spatial dimensions
    mesh = ESMF.Mesh(parametric_dim=2, spatial_dim=2)
    
    num_node = 16
    num_elem = 10
    nodeId = np.array([11,12,13,14,21,22,23,24,31,32,33,34,41,42,43,44])
    nodeCoord = np.array([0.0,0.0, 0.5,0.0, 1.0,0.0, 2.0,0.0,
                          0.0,0.5, 0.5,0.5, 1.0,0.5, 2.0,0.5,
                          0.0,1.0, 0.5,1.0, 1.0,1.0, 2.0,1.0,
                          0.0,2.0, 0.5,2.0, 1.0,2.0, 2.0,2.0])
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

    mesh.add_nodes(num_node,nodeId,nodeCoord,nodeOwner)

    mesh.add_elements(num_elem,elemId,elemType,elemConn)

    return mesh, nodeCoord, elemType, elemConn

def mesh_create_5_parallel (localPet):
    '''
    PRECONDITIONS: None
    POSTCONDITIONS: A 5 element Mesh has been created.
    
    #  2.0   31 ------ 32       [32] ----- 33
    #        |         |         | 22  /   |
    #        |    21   |         |    /    |
    #        |         |         |  /  23  |
    #  1.0  [21] ---- [22]      [22] ---- [23]
    #
    #       0.0       1.0       1.0       2.0
    #
    #           PET 2               PET 3
    #
    #
    #  1.0   21 ------ 22       [22] ----- 23
    #        |         |         |         |
    #        |    11   |         |    12   |
    #        |         |         |         |
    #  0.0   11 ------ 12       [12] ----- 13
    #
    #       0.0       1.0       1.0      2.0
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
                            1.0,0.0,
                            0.0,1.0,
                            1.0,1.0 ])
        nodeOwner=np.zeros(num_node)
        elemId=np.array([11])
        elemType=np.array([ESMF.MeshElemType.QUAD])
        elemConn=np.array([0,1,3,2])

    elif (localPet == 1):
        num_node=4
        num_elem=1

        nodeId=np.array([12,13,22,23])
        nodeCoord=np.array([1.0,0.0,
                            2.0,0.0,
                            1.0,1.0,
                            2.0,1.0 ])
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
        nodeCoord=np.array([0.0,1.0,
                            1.0,1.0,
                            0.0,2.0,
                            1.0,2.0 ])
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
        nodeCoord=np.array([1.0,1.0,
                            2.0,1.0,
                            1.0,2.0,
                            2.0,2.0 ])
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
    POSTCONDITIONS: A 10 element Mesh has been created.
    
    #  2.0   41 ------ 42 ------ 43      [43] ---------- 44
    #        |         |         |       |          /    |
    #        |         |         |       |  33   /       |
    #        |    31   |    32   |       |      /        |
    #        |         |         |       |    /    34    |
    #        |         |         |       |  /            |
    #  1.0  [31] ----- [32] ---- [33]    [33] ---------- [34]
    #
    #       0.0       0.5       1.0     1.0             2.0
    #
    #                PET 2                      PET 3
    #
    #
    #  1.0   31 ------ 32 ------ 33     [33] ----------- 34
    #        |         |         |       |               |
    #        |    21   |    22   |       |       23      |
    #        |         |         |       |               |
    #  0.5   21 ------ 22 ------ 23     [23] ----------  24
    #        |         |         |       |               |
    #        |    11   |    12   |       |       13      |
    #        |         |         |       |               |
    #  0.0   11 ------ 12 ------ 13     [13] ----------- 14
    #
    #       0.0       0.5       1.0     1.0             2.0
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
                            0.5,0.0,
                            1.0,0.0,
                            0.0,0.5,
                            0.5,0.5,
                            1.0,0.5,
                            0.0,1.0,
                            0.5,1.0,
                            1.0,1.0])
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
        nodeCoord=np.array([1.0,0.0,
                            2.0,0.0,
                            1.0,0.5,
                            2.0,0.5,
                            1.0,1.0,
                            2.0,1.0 ])
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
        nodeCoord=np.array([0.0,1.0,
                            0.5,1.0,
                            1.0,1.0,
                            0.0,2.0,
                            0.5,2.0,
                            1.0,2.0 ])
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
        nodeCoord=np.array([1.0,1.0,
                            2.0,1.0,
                            1.0,2.0,
                            2.0,2.0 ])
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

def create_field(mesh, name):
    '''
    PRECONDITIONS: A Mesh has been created, and 'name' is a string that
                   will be used to initialize the name of a new Field.
    POSTCONDITIONS: An Field has been created.
    '''
    field = ESMF.Field(mesh, name, meshloc=ESMF.MeshLoc.ELEMENT)

    return field

def build_analyticfield_meshcsrv(field, nodeCoord, elemType, elemConn):
    '''
    PRECONDITIONS: A Field has been created on the elements of a Mesh.
    POSTCONDITIONS: The 'field' has been initialized to an analytic 
                    field.
    '''
    # set the field to a vanilla initial field for now
    #print "Mesh center coordinates"
    offset = 0
    for i in range(field.shape[0]):    # this routine assumes element field
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
        field[i] = 20.0+x*y+y**2

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

def compare_fields(field1, field2, parallel=False, localPet=0, petCount=1):
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
    for i in range(field1.shape[0]):
        err = abs(field1[i] - field2[i])/abs(field2[i])
        if err > .06:
            correct = False
            print "ACCURACY ERROR - "+str(err)
        totalErr += err

    # this is for parallel
    if parallel:
        # use mpi4py to collect values
        from mpi4py import MPI

        comm = MPI.COMM_WORLD
        rank = comm.Get_rank()

        total_error_global = comm.reduce(totalErr, op=MPI.SUM)

        if rank == 0:
            itrp = False
            if (total_error_global < 20E-2):
                itrp = True

            if (itrp and correct):
                print " - PASS - Total error = "+str(total_error_global)+"\n"
            else:
                print " - FAIL - Total error = "+str(total_error_global)+"\n"

    # this is for serial
    else:
        if correct:
            print " - PASS - Total Error = "+str(totalErr)+"\n"
        else:
            print " - FAIL - Total Error = "+str(totalErr)+"\n"

    return

def test_main():
    from sys import getrefcount

    # start up ESMF
    # this call is not necessary unless you want to to override the
    # default options:
    #  LogKind = NONE
    #  debug = False
    #manager = ESMF.Manager(logkind=ESMF.LogKind.SINGLE, debug=True)

    # inquire for rank and proc from ESMF Virtual Machine
    localPet = ESMF.local_pet()
    petCount = ESMF.pet_count()

    parallel = False
    if petCount > 1:
        if petCount > 4:
            raise NameError('MPI rank must be 4 in parallel mode!')
        parallel = True

    # opening remarks
    if localPet == 0:
        print "\nmesh_mesh_regrid"

    # create two unique Mesh objects
    if parallel:
        srcmesh, nodeCoordSrc, elemTypeSrc, elemConnSrc = \
            mesh_create_5_parallel(localPet)
        dstmesh, nodeCoordDst, elemTypeDst, elemConnDst = \
            mesh_create_10_parallel(localPet)
    else:
        srcmesh, nodeCoordSrc, elemTypeSrc, elemConnSrc = \
            mesh_create_5()
        dstmesh, nodeCoordDst, elemTypeDst, elemConnDst = \
            mesh_create_10()

    # create ESMP_Field objects on the Meshes
    srcfield = create_field(srcmesh, 'srcfield')
    dstfield = create_field(dstmesh, 'dstfield')
    dstfield2 = create_field(dstmesh, 'dstfield_exact')

    # initialize the Fields to an analytic function
    srcfield = build_analyticfield_meshcsrv(srcfield, nodeCoordSrc, \
                                            elemTypeSrc, elemConnSrc)
    dstfield2 = build_analyticfield_meshcsrv(dstfield2, nodeCoordDst, \
                                             elemTypeDst, elemConnDst)

    # run the ESMF regridding
    dstfield = run_regridding(srcfield, dstfield)

    # compare results and output PASS or FAIL
    compare_fields(dstfield, dstfield2, parallel)

    return 0

if __name__ == '__main__':
    sys.exit(test_main())
