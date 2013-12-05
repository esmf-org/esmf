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

def mesh_create_2x2(mesh, localPet):
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
    elemType=np.array([ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD])
    elemConn=np.array([0,1,4,3,
                       1,2,5,4,
                       3,4,7,6,
                       4,5,8,7])

    mesh.add_nodes(num_node,nodeId,nodeCoord,nodeOwner)

    mesh.add_elements(num_elem,elemId,elemType,elemConn)

    return mesh, nodeCoord, elemType, elemConn

def mesh_create_3x3(mesh, localPet):
    '''
    PRECONDITIONS: A Mesh has been declared.
    POSTCONDITIONS: A 3x3 Mesh has been created.
    
                   3x3 Mesh
    
    
      2.0   13 -------14 --------15--------16
            |         |          |         |
            |    7    |    8     |   9     |
            |         |          |         |
      1.0   9 ------- 10 --------11--------12
            |         |          |         |
            |    4    |    5     |   6     |
            |         |          |         |
      0.5   5 ------- 6 -------- 7-------- 8
            |         |          |         |
            |    1    |    2     |   3     |
            |         |          |         |
      0.0   1 ------- 2 -------- 3-------- 4
    
           0.0       0.5        1.0       2.0
    
          Node Ids at corners
          Element Ids in centers
    
          (Everything owned by PET 0)
    '''
    # set up a simple mesh
    num_node = 16
    num_elem = 9
    nodeId = np.array([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16])
    nodeCoord = np.array([0.0,0.0, 0.5,0.0, 1.0,0.0, 2.0,0.0,
                          0.0,0.5, 0.5,0.5, 1.0,0.5, 2.0,0.5,
                          0.0,1.0, 0.5,1.0, 1.0,1.0, 2.0,1.0,
                          0.0,2.0, 0.5,2.0, 1.0,2.0, 2.0,2.0])
    nodeOwner = np.zeros(num_node)
    elemId = np.array([1,2,3,4,5,6,7,8,9])
    elemType=np.array([ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.QUAD])
    elemConn = np.array([0,1,5,4,
                         1,2,6,5,
                         2,3,7,6,
                         4,5,9,8,
                         5,6,10,9,
                         6,7,11,10,
                         8,9,13,12,
                         9,10,14,13,
                         10,11,15,14])

    mesh.add_nodes(num_node,nodeId,nodeCoord,nodeOwner)

    mesh.add_elements(num_elem,elemId,elemType,elemConn)

    return mesh, nodeCoord, elemType, elemConn

def mesh_create_2x2_parallel (mesh, localPet):
    '''
    PRECONDITIONS: A Mesh has been declared.
    POSTCONDITIONS: A 2x2 Mesh has been created.
    #  2.0   7 ------- 8        [8] ------ 9
    #        |         |         |         |
    #        |    3    |         |    4    |
    #        |         |         |         |
    #  1.0  [4] ----- [5]       [5] ----- [6]
    #
    #       0.0       1.0       1.0       2.0
    #
    #           PET 2               PET 3
    #
    #
    #  1.0   4 ------- 5        [5] ------ 6
    #        |         |         |         |
    #        |    1    |         |    2    |
    #        |         |         |         |
    #  0.0   1 ------- 2        [2] ------ 3
    #
    #       0.0       1.0       1.0      2.0
    #
    #           PET 0               PET 1
    #
    #               Node Id labels at corners
    #              Element Id labels in centers
    '''

    # Break up what's being set by PET
    if (localPet == 0): # This part only for PET 0
        # Set number of nodes
        num_node=4
        num_elem=1

        nodeId=np.array([1,2,4,5])
        nodeCoord=np.array([0.0,0.0,     # node id 1
                            1.0,0.0,     # node id 2
                            0.0,1.0,     # node id 4
                            1.0,1.0 ])   # node id 5
        nodeOwner=np.zeros(num_node)     # node id 5
        elemId=np.array([1])
        elemType=np.array([ESMF.MeshElemType.QUAD]) # elem id 1
        elemConn=np.array([0,1,3,2]) # elem id 1

    elif (localPet == 1): # This part only for PET 1
        # Set number of nodes
        num_node=4
        num_elem=1

        nodeId=np.array([2,3,5,6])
        nodeCoord=np.array([1.0,0.0,     # node id 2
                            2.0,0.0,     # node id 3
                            1.0,1.0,     # node id 5
                            2.0,1.0 ]) # node id 6
        nodeOwner=np.array([0,     # node id 2
                            1,     # node id 3
                            0,     # node id 5
                            1])    # node id 6
        elemId=np.array([2])
        elemType=np.array([ESMF.MeshElemType.QUAD]) # elem id 2
        elemConn=np.array([0,1,3,2]) # elem id 2

    elif (localPet == 2): # This part only for PET 2
        # Set number of nodes
        num_node=4
        num_elem=1

        nodeId=np.array([4,5,7,8])
        nodeCoord=np.array([0.0,1.0,     # node id 4
                            1.0,1.0,     # node id 5
                            0.0,2.0,     # node id 7
                            1.0,2.0 ]) # node id 8
        nodeOwner=np.array([0,     # node id 4
                            0,     # node id 5
                            2,     # node id 7
                            2])    # node id 8
        elemId=np.array([3])
        elemType=np.array([ESMF.MeshElemType.QUAD]) # elem id 4
        elemConn=np.array([0,1,3,2]) # elem id 4

    elif (localPet == 3): # This part only for PET 3
        # Set number of nodes
        num_node=4
        num_elem=1

        nodeId=np.array([5,6,8,9])
        nodeCoord=np.array([1.0,1.0,     # node id 5
                            2.0,1.0,     # node id 6
                            1.0,2.0,     # node id 8
                            2.0,2.0 ]) # node id 9
        nodeOwner=np.array([0,     # node id 5
                            1,     # node id 6
                            2,     # node id 8
                            3])    # node id 9
        elemId=np.array([4])
        elemType=np.array([ESMF.MeshElemType.QUAD]) # elem id 5
        elemConn=np.array([0,1,3,2]) # elem id 5

    # Add nodes and elements to the Mesh
    mesh.add_nodes(num_node,nodeId,nodeCoord,nodeOwner)
    mesh.add_elements(num_elem,elemId,elemType,elemConn)

    return mesh, nodeCoord, elemType, elemConn

def mesh_create_3x3_parallel (mesh, localPet):
    '''
    PRECONDITIONS: A Mesh has been declared.
    POSTCONDITIONS: A 3x3 Mesh has been created.
    #  2.0   13 ------ 14 ------ 15     [15] ----------- 16
    #        |         |         |       |               |
    #        |         |         |       |               |
    #        |    7    |    8    |       |       9       |
    #        |         |         |       |               |
    #        |         |         |       |               |
    #  1.0  [9] ----- [10] ---- [11]    [11] ---------- [12]
    #
    #       0.0       0.5       1.0     1.0             2.0
    #
    #                PET 2                      PET 3
    #
    #
    #  1.0   9 ------- 10 ------ 11     [11] ----------- 12
    #        |         |         |       |               |
    #        |    4    |    5    |       |       6       |
    #        |         |         |       |               |
    #  0.5   5 ------- 6 ------- 7      [7] -----------  8
    #        |         |         |       |               |
    #        |    1    |    2    |       |       3       |
    #        |         |         |       |               |
    #  0.0   1 ------- 2 ------- 3      [3] ------------ 4
    #
    #       0.0       0.5       1.0     1.0             2.0
    #
    #                PET 0                      PET 1
    #
    #               Node Id labels at corners
    #              Element Id labels in centers
    '''

    # Break up what's being set by PET
    if (localPet == 0): # This part only for PET 0
        # Set number of nodes
        num_node=9
        num_elem=4

        nodeId=np.array([1,2,3,5,6,7,9,10,11])
        nodeCoord=np.array([0.0,0.0,     # node id 1
                            0.5,0.0,     # node id 2
                            1.0,0.0,     # node id 3
                            0.0,0.5,     # node id 5
                            0.5,0.5,     # node id 6
                            1.0,0.5,     # node id 7
                            0.0,1.0,     # node id 9
                            0.5,1.0,     # node id 10
                            1.0,1.0]) # node id 11
        nodeOwner=np.zeros(num_node)
        elemId=np.array([1,2,4,5])
        elemType=np.array([ESMF.MeshElemType.QUAD,
                           ESMF.MeshElemType.QUAD,
                           ESMF.MeshElemType.QUAD,
                           ESMF.MeshElemType.QUAD])
        elemConn=np.array([0,1,4,3,
                           1,2,5,4,
                           3,4,7,6,
                           4,5,8,7]) # elem id 1

    elif (localPet == 1): # This part only for PET 1
        # Set number of nodes
        num_node=6
        num_elem=2

        nodeId=np.array([3,4,7,8,11,12])
        nodeCoord=np.array([1.0,0.0, # node id 3
                            2.0,0.0,     # node id 4
                            1.0,0.5,     # node id 7
                            2.0,0.5,     # node id 8
                            1.0,1.0,     # node id 11
                            2.0,1.0 ]) # node id 12
        nodeOwner=np.array([0,1,0,1,0,1])
        elemId=np.array([3,6])
        elemType=np.array([ESMF.MeshElemType.QUAD,
                           ESMF.MeshElemType.QUAD])
        elemConn=np.array([0,1,3,2,
                           2,3,5,4])

    elif (localPet == 2): # This part only for PET 2
        # Set number of nodes
        num_node=6
        num_elem=2

        nodeId=np.array([9,10,11,13,14,15])
        nodeCoord=np.array([0.0,1.0, # node id 9
                            0.5,1.0,     # node id 10
                            1.0,1.0,     # node id 11
                            0.0,2.0,     # node id 13
                            0.5,2.0,     # node id 14
                            1.0,2.0 ]) # node id 15
        nodeOwner=np.array([0,0,0,2,2,2])    # node id 8
        elemId=np.array([7,8])
        elemType=np.array([ESMF.MeshElemType.QUAD,
                           ESMF.MeshElemType.QUAD]) # elem id 4
        elemConn=np.array([0,1,4,3,
                           1,2,5,4]) # elem id 4

    elif (localPet == 3): # This part only for PET 3
        # Set number of nodes
        num_node=4
        num_elem=1

        nodeId=np.array([11,12,15,16])
        nodeCoord=np.array([1.0,1.0, # node id 11
                            2.0,1.0,     # node id 12
                            1.0,2.0,     # node id 15
                            2.0,2.0 ]) # node id 16
        nodeOwner=np.array([0,1,2,3])
        elemId=np.array([9])
        elemType=np.array([ESMF.MeshElemType.QUAD]) # elem id 5
        elemConn=np.array([0,1,3,2]) # elem id 5

    # Add nodes and elements to the Mesh
    mesh.add_nodes(num_node,nodeId,nodeCoord,nodeOwner)
    mesh.add_elements(num_elem,elemId,elemType,elemConn)

    return mesh, nodeCoord, elemType, elemConn

def create_mesh_2x2(localPet, parallel=False):
    '''
    PRECONDITIONS: ESMPy has been initialized.
    POSTCONDITIONS: A Mesh (2x2) has been created and returned as 
                    'mesh'.
    '''
    # Two parametric dimensions, and three spatial dimensions
    mesh = ESMF.Mesh(parametric_dim=2, spatial_dim=2)

    # this is for parallel
    if parallel:
        mesh, nodeCoord, elemType, elemConn = mesh_create_2x2_parallel(mesh, 
                                                                       localPet)
    # this is for serial
    else:
        mesh, nodeCoord, elemType, elemConn = mesh_create_2x2(mesh, localPet)

    return mesh, nodeCoord, elemType, elemConn

def create_mesh_3x3(localPet, parallel=False, othermesh=None):
    '''
    PRECONDITIONS: ESMPy has been initialized.
    POSTCONDITIONS: A Mesh (3x3) has been created and returned as 'mesh'.
    '''
    # Two parametric dimensions, and three spatial dimensions
    mesh = ESMF.Mesh(parametric_dim=2, spatial_dim=2)

    # this is for parallel
    if parallel:
        mesh, nodeCoord, elemType, elemConn = mesh_create_3x3_parallel(mesh, 
                                                                       localPet)
    # this is for serial
    else:
        mesh, nodeCoord, elemType, elemConn = mesh_create_3x3(mesh, localPet)

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
            raise NameError("Cannot compute a non-constant analytic field \
                             for a mesh with triangular elements!")
        x1 = nodeCoord[(elemConn[offset])*2]
        x2 = nodeCoord[(elemConn[offset+1])*2]
        y1 = nodeCoord[(elemConn[offset+1])*2+1]
        y2 = nodeCoord[(elemConn[offset+3])*2+1]
        x = (x1+x2)/2.0
        y = (y1+y2)/2.0
        field[i] = 20.0+x*y+y**2
        #print '[{0},{1}] = {2}'.format(x,y,fieldPtr[i])
        offset = offset + 4
    #print "\n"

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
    srcmesh, nodeCoordSrc, elemTypeSrc, elemConnSrc = \
        create_mesh_2x2(localPet, parallel)
    dstmesh, nodeCoordDst, elemTypeDst, elemConnDst = \
        create_mesh_3x3(localPet, parallel, srcmesh)

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
