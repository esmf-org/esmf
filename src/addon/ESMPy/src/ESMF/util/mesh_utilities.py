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





def mesh_create_5_pentahexa(coord_sys=None):
    '''
    PRECONDITIONS: None
    POSTCONDITIONS: A 5 element Mesh has been created.
    RETURN VALUES: \n Mesh :: mesh \n


  2.5        8        10 --------11
          /     \   /            |
  2.1   7         9              12
        |         |      5       /
        |    4    |            /
        |         |          /
  1.0   4 ------- 5 ------- 6
        |         |  \   3  |
        |    1    |    \    |
        |         |  2   \  |
 -0.1   1 ------- 2 ------- 3

      -0.1       1.0       2.1   2.5

          Node Ids at corners
          Element Ids in centers

    Note: This mesh is not parallel, it can only be used in serial
    '''
    # Two parametric dimensions, and two spatial dimensions
    mesh = ESMF.Mesh(parametric_dim=2, spatial_dim=2, coord_sys=coord_sys)

    num_node = 12
    num_elem = 5
    nodeId = np.array([1,2,3,4,5,6,7,8,9,10,11,12])
    nodeCoord = np.array([-0.1,-0.1,  #node id 1
                          1.0,-0.1,  #node id 2
                          2.1,-0.1,  #node id 3
                          0.1, 1.0,  #node id 4
                          1.0, 1.0,  #node id 5
                          2.1, 1.0,  #node id 6
                          0.1, 2.1,  #node id 7
                          0.5, 2.5,  #node id 8
                          1.0, 2.1,  #node id 9
                          1.5, 2.5,  #node id 10
                          2.5, 2.5,  #node id 11
                          2.5, 2.1]) #node id 12
 

    nodeOwner = np.zeros(num_node)

    elemId = np.array([1,2,3,4,5])
    elemType=np.array([ESMF.MeshElemType.QUAD,
                       ESMF.MeshElemType.TRI,
                       ESMF.MeshElemType.TRI, 5, 6])
 

# I believe python connections are 0-based
#    elemConn=np.array([1,2,5,4,         # elem id 1
#                       2,3,5,           # elem id 2
#                       3,6,5,           # elem id 3
#                       4,5,9,8,7,       # elem id 4
#                       5,6,12,11,10,9]) # elem id 5
    elemConn=np.array([0,1,4,3,         # elem id 1
                       1,2,4,           # elem id 2
                       2,5,4,           # elem id 3
                       3,4,8,7,6,       # elem id 4
                       4,5,11,10,9,8]) # elem id 5

    mesh.add_nodes(num_node,nodeId,nodeCoord,nodeOwner)

    mesh.add_elements(num_elem,elemId,elemType,elemConn)

    return mesh, nodeCoord, nodeOwner, elemType, elemConn

def mesh_create_4_ngons():
    '''
    PRECONDITIONS: None
    POSTCONDITIONS: A 4 element Mesh has been created.
    RETURN VALUES: \n Mesh :: mesh \n

      2.25  6 ------ 7 -----   8 ------  9
            |         \        /         |
            |          \  4   /          |
            |           \    /           |
            |            \  /            |
      1.00  |             5              |
            |            /  \            |
            |    1     /  2  \     3     |
            |         /       \          |
      0.25  1 ------ 2 -----   3 ------  4

           0.25      0.75 1.0 1.25      1.75

          Node Ids at corners
          Element Ids in centers

    Note: This mesh is not parallel, it can only be used in serial
    '''
    # Two parametric dimensions, and two spatial dimensions
    mesh = ESMF.Mesh(parametric_dim=2, spatial_dim=2)

    num_node = 9
    num_elem = 4
    nodeId = np.array([1,2,3,4,5,6,7,8,9])
    nodeCoord = np.array([0.25, 0.25,
                          0.25, 0.75,
                          0.25, 1.25,
                          0.25, 1.75,
                          1.0, 1.0,
                          2.25, 0.25,
                          2.25, 0.75,
                          2.25, 1.25,
                          2.25, 1.75])


    nodeOwner = np.zeros(num_node)

    elemId = np.array([1,2,3,4])
    elemType=np.array([5,3,5,3])

    elemConn=np.array([0,1,4,6,5,
                       1,2,4,
                       2,5,4,
                       2,3,8,7,4,
                       4,7,6])

    mesh.add_nodes(num_node,nodeId,nodeCoord,nodeOwner)

    mesh.add_elements(num_elem,elemId,elemType,elemConn)

    return mesh, nodeCoord, nodeOwner, elemType, elemConn

def mesh_create_5():
    '''
    PRECONDITIONS: None
    POSTCONDITIONS: A 5 element Mesh has been created.    
    RETURN VALUES: \n Mesh :: mesh \n
    
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
    elemCoord = np.array([1.0, 1.0,
                          3.0, 1.0,
                          1.0, 3.0,
                          2.5, 3.5,
                          3.5, 2.5])

    mesh.add_nodes(num_node,nodeId,nodeCoord,nodeOwner)

    mesh.add_elements(num_elem,elemId,elemType,elemConn, element_coords=elemCoord)

    return mesh, nodeCoord, nodeOwner, elemType, elemConn, elemCoord

def mesh_create_10():
    '''
    PRECONDITIONS: None
    POSTCONDITIONS: A 10 element Mesh has been created.    
    RETURN VALUES: \n Mesh :: mesh \n
    
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
    elemCoord = np.array([0.75, 0.75, 2.0, 0.75, 3.25, 0.75,
                          0.75, 2.0, 2.0, 2.0, 3.25, 2.0,
                          0.75, 3.25, 2.0, 3.25, 3.0, 3.5, 3.5, 3.0])

    mesh.add_nodes(num_node,nodeId,nodeCoord,nodeOwner)

    mesh.add_elements(num_elem,elemId,elemType,elemConn, element_coords=elemCoord)

    return mesh, nodeCoord, nodeOwner, elemType, elemConn, elemCoord

def mesh_create_50(domask=False, doarea=False):
    '''
    PRECONDITIONS: None
    POSTCONDITIONS: A 50 element Mesh has been created.    
    RETURN VALUES: \n Mesh :: mesh \n
    
      3.75  81 ------ 82 ----- 83 ------ 84 ------ 85 ------ 86 ------ 87 ------ 88
            |         |        |         |         |         |         |  77 /   |
            |    71   |   72   |    73   |    74   |   75    |    76   |    /    |
            |         |        |         |         |         |         |  /  78  |
      3.25  71 ------ 72 ----- 73 ------ 74 ------ 75 ------ 76 ------ 77 ------ 78
            |         |        |         |         |         |         |         |
            |    61   |   62   |    63   |    64   |    65   |    66   |   67    |
            |         |        |         |         |         |         |         |
      2.75  61 ------ 62 ----- 63 ------ 64 ------ 65 ------ 66 ------ 67 ------ 68
            |         |        |         |         |         |         |         |
            |    51   |   52   |    53   |    54   |    55   |    56   |   57    |
            |         |        |         |         |         |         |         |
      2.25  51 ------ 52 ----- 53 ------ 54 ------ 55 ------ 56 ------ 57 ------ 58
            |         |        |         |         |         |         |         |
            |    41   |   42   |    43   |    44   |    45   |    46   |   47    |
            |         |        |         |         |         |         |         |
      1.75  41 ------ 42 ----- 43 ------ 44 ------ 45 ------ 46 ------ 47 ------ 48
            |         |        |         |         |         |         |         |
            |    31   |   32   |    33   |    34   |    35   |    36   |   37    |
            |         |        |         |         |         |         |         |
      1.25  31 ------ 32 ----- 33 ------ 34 ------ 35 ------ 36 ------ 37 ------ 38
            |         |        |         |         |         |         |         |
            |    21   |   22   |    23   |    24   |    25   |    26   |   27    |
            |         |        |         |         |         |         |         |
      0.75  21 ------ 22 ----- 23 ------ 24 ------ 25 ------ 26 ------ 27 ------ 28
            |         |        |         |         |         |         |         |
            |    11   |   12   |    13   |    14   |    15   |    16   |   17    |
            |         |        |         |         |         |         |         |
      0.25  11 ------ 12 ----- 13 ------ 14 ------ 15 ------ 16 ------ 17 ------ 18
    
           0.25      0.75     1.25      1.75      2.25      2.75      3.25      3.75
    
          Node Ids at corners
          Element Ids in centers
    
    Note: This mesh is not parallel, it can only be used in serial
    '''
    # Two parametric dimensions, and two spatial dimensions
    mesh = ESMF.Mesh(parametric_dim=2, spatial_dim=2)
    
    num_node = 64
    num_elem = 50
    nodeId = np.array([11,12,13,14,15,16,17,18,
                       21,22,23,24,25,26,27,28,
                       31,32,33,34,35,36,37,38,
                       41,42,43,44,45,46,47,48,
                       51,52,53,54,55,56,57,58,
                       61,62,63,64,65,66,67,68,
                       71,72,73,74,75,76,77,78,
                       81,82,83,84,85,86,87,88])
    nodeCoord = np.array([0.25,0.25, 0.25,0.75, 0.25,1.25, 0.25,1.75, 0.25,2.25, 0.25,2.75, 0.25,3.25, 0.25,3.75,
                          0.75,0.25, 0.75,0.75, 0.75,1.25, 0.75,1.75, 0.75,2.25, 0.75,2.75, 0.75,3.25, 0.75,3.75,
                          1.25,0.25, 1.25,0.75, 1.25,1.25, 1.25,1.75, 1.25,2.25, 1.25,2.75, 1.25,3.25, 1.25,3.75,
                          1.75,0.25, 1.75,0.75, 1.75,1.25, 1.75,1.75, 1.75,2.25, 1.75,2.75, 1.75,3.25, 1.75,3.75,
                          2.25,0.25, 2.25,0.75, 2.25,1.25, 2.25,1.75, 2.25,2.25, 2.25,2.75, 2.25,3.25, 2.25,3.75,
                          2.75,0.25, 2.75,0.75, 2.75,1.25, 2.75,1.75, 2.75,2.25, 2.75,2.75, 2.75,3.25, 2.75,3.75,
                          3.25,0.25, 3.25,0.75, 3.25,1.25, 3.25,1.75, 3.25,2.25, 3.25,2.75, 3.25,3.25, 3.25,3.75,
                          3.75,0.25, 3.75,0.75, 3.75,1.25, 3.75,1.75, 3.75,2.25, 3.75,2.75, 3.75,3.25, 3.75,3.75])
    nodeOwner = np.zeros(num_node)
    elemId = np.array([11,12,13,14,15,16,17,
                       21,22,23,24,25,26,27,
                       31,32,33,34,35,36,37,
                       41,42,43,44,45,46,47,
                       51,52,53,54,55,56,57,
                       61,62,63,64,65,66,67,
                       71,72,73,74,75,76,77,78])
    elemType = np.ones(num_elem-2)*ESMF.MeshElemType.QUAD
    elemType = np.append(elemType, [ESMF.MeshElemType.TRI, ESMF.MeshElemType.TRI])
    elemConn = np.array([11,12,22,21,12,13,23,22,13,14,24,23,14,15,25,24,15,16,26,25,16,17,27,26,17,18,28,27,
                         21,22,32,31,22,23,33,32,23,24,34,33,24,25,35,34,25,26,36,35,26,27,37,36,27,28,38,37,
                         31,32,42,41,32,33,43,42,33,34,44,43,34,35,45,44,35,36,46,45,36,37,47,46,37,38,48,47,
                         41,42,52,51,42,43,53,52,43,44,54,53,44,45,55,54,45,46,56,55,46,47,57,56,47,48,58,57,
                         51,52,62,61,52,53,63,62,53,54,64,63,54,55,65,64,55,56,66,65,56,57,67,66,57,58,68,67,
                         61,62,72,71,62,63,73,72,63,64,74,73,64,65,75,74,65,66,76,75,66,67,77,76,67,68,78,77,
                         71,72,82,81,72,73,83,82,73,74,84,83,74,75,85,84,75,76,86,85,76,77,87,86, 
                         77,88,87,
                         77,78,88])
    elemConn = np.array([np.where(a==nodeId) for a in elemConn]).flatten()
    elemCoord = np.array(
        [0.5, 0.5, 1.0, 0.5, 1.5, 0.5, 2.0, 0.5, 2.5, 0.5, 3.0, 0.5, 3.5, 0.5,
         0.5, 1.0, 1.0, 1.0, 1.5, 1.0, 2.0, 1.0, 2.5, 1.0, 3.0, 1.0, 3.5, 1.0,
         0.5, 1.5, 1.0, 1.5, 1.5, 1.5, 2.0, 1.5, 2.5, 1.5, 3.0, 1.5, 3.5, 1.5,
         0.5, 2.0, 1.0, 2.0, 1.5, 2.0, 2.0, 2.0, 2.5, 2.0, 3.0, 2.0, 3.5, 2.0,
         0.5, 2.5, 1.0, 2.5, 1.5, 2.5, 2.0, 2.5, 2.5, 2.5, 3.0, 2.5, 3.5, 2.5,
         0.5, 3.0, 1.0, 3.0, 1.5, 3.0, 2.0, 3.0, 2.5, 3.0, 3.0, 3.0, 3.5, 3.0,
         0.5, 3.5, 1.0, 3.5, 1.5, 3.5, 2.0, 3.5, 2.5, 3.5, 3.0, 3.5, 3.375, 3.625, 3.625, 3.375])
    elemMask = None
    if domask:
        elemMask = np.ones(50)
        elemMask[1] = 0
    elemArea = None
    if doarea:
        elemArea = np.ones(48)*5
        elemArea = np.append(elemArea, [2.5, 2.5])

    mesh.add_nodes(num_node,nodeId,nodeCoord,nodeOwner)

    mesh.add_elements(num_elem,elemId,elemType,elemConn, 
        element_mask=elemMask, element_area=elemArea, element_coords=elemCoord)

    # TODO: clean this up!
    if domask and doarea:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn, elemMask, elemArea
    elif domask and not doarea:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn, elemMask
    elif not domask and doarea:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn, elemArea
    else:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn, elemCoord

def mesh_create_50_ngons(domask=False, doarea=False):
    '''
    PRECONDITIONS: None
    POSTCONDITIONS: A 50 element Mesh has been created.
    RETURN VALUES: \n Mesh :: mesh \n

      3.75  81 ------ 82 ----- 83 ------ 84 ------ 85 ------ 86 ------ 87 ------ 88
            |         |        |         |         |         |         |         |
            |    71   |   72   |    73   |    74   |   75    |    76   |   77    |
            |         |        |         |         |         |         |         |
      3.25  71 ------ 72 ----- 73 ------ 74 ------ 75 ------ 76 ------ 77 ------ 78
            |         \        /         |         |         |         |         |
            |          \  64  /          |         |         |         |         |
            |           \    /           |         |         |         |         |
            |            \  /            |         |         |         |         |
      3.00  |             69             |         |         |         |         |
            |            /  \            |         |         |         |         |
            |    61    /  62 \     63    |    65   |    66   |    67   |   68    |
            |         /       \          |         |         |         |         |
      2.75  61 ------ 62 ----- 63 ------ 64 ------ 65 ------ 66 ------ 67 ------ 68
            |         |        |         |         |         |         |         |
            |    51   |   52   |    53   |    54   |    55   |    56   |   57    |
            |         |        |         |         |         |         |         |
      2.25  51 ------ 52 ----- 53 ------ 54 ------ 55 ------ 56 ------ 57 ------ 58
            |         |        |         |         |         |         |         |
            |    41   |   42   |    43   |    44   |    45   |    46   |   47    |
            |         |        |         |         |         |         |         |
      1.75  41 ------ 42 ----- 43 ------ 44 ------ 45 ------ 46 ------ 47 ------ 48
            |         |        |         |         |         |         |         |
            |    31   |   32   |    33   |    34   |    35   |    36   |   37    |
            |         |        |         |         |         |         |         |
      1.25  31 ------ 32 ----- 33 ------ 34 ------ 35 ------ 36 ------ 37 ------ 38
            |         |        |         |         |         |         |         |
            |    21   |   22   |    23   |    24   |    25   |    26   |   27    |
            |         |        |         |         |         |         |         |
      0.75  21 ------ 22 ----- 23 ------ 24 ------ 25 ------ 26 ------ 27 ------ 28
            |         |        |         |         |         |         |         |
            |    11   |   12   |    13   |    14   |    15   |    16   |   17    |
            |         |        |         |         |         |         |         |
      0.25  11 ------ 12 ----- 13 ------ 14 ------ 15 ------ 16 ------ 17 ------ 18

           0.25      0.75 1.0 1.25      1.75      2.25      2.75      3.25      3.75

          Node Ids at corners
          Element Ids in centers

    Note: This mesh is not parallel, it can only be used in serial
    '''

    # Two parametric dimensions, and two spatial dimensions
    mesh = ESMF.Mesh(parametric_dim=2, spatial_dim=2)

    num_node = 65
    num_elem = 50
    nodeId = np.array([11,12,13,14,15,16,17,18,
                       21,22,23,24,25,26,27,28,
                       31,32,33,34,35,36,37,38,
                       41,42,43,44,45,46,47,48,
                       51,52,53,54,55,56,57,58,
                       61,62,63,64,65,66,67,68,69,
                       71,72,73,74,75,76,77,78,
                       81,82,83,84,85,86,87,88])
    nodeCoord = np.array([0.25,0.25, 0.75,0.25, 1.25,0.25, 1.75,0.25, 2.25,0.25, 2.75,0.25, 3.25,0.25, 3.75,0.25,
                          0.25,0.75, 0.75,0.75, 1.25,0.75, 1.75,0.75, 2.25,0.75, 2.75,0.75, 3.25,0.75, 3.75,0.75,
                          0.25,1.25, 0.75,1.25, 1.25,1.25, 1.75,1.25, 2.25,1.25, 2.75,1.25, 3.25,1.25, 3.75,1.25,
                          0.25,1.75, 0.75,1.75, 1.25,1.75, 1.75,1.75, 2.25,1.75, 2.75,1.75, 3.25,1.75, 3.75,1.75,
                          0.25,2.25, 0.75,2.25, 1.25,2.25, 1.75,2.25, 2.25,2.25, 2.75,2.25, 3.25,2.25, 3.75,2.25,
                          0.25,2.75, 0.75,2.75, 1.25,2.75, 1.75,2.75, 2.25,2.75, 2.75,2.75, 3.25,2.75, 3.75,2.75, 1.0,3.0,
                          0.25,3.25, 0.75,3.25, 1.25,3.25, 1.75,3.25, 2.25,3.25, 2.75,3.25, 3.25,3.25, 3.75,3.25,
                          0.25,3.75, 0.75,3.75, 1.25,3.75, 1.75,3.75, 2.25,3.75, 2.75,3.75, 3.25,3.75, 3.75,3.75,])
    nodeOwner = np.zeros(num_node)
    elemId = np.array([11,12,13,14,15,16,17,
                       21,22,23,24,25,26,27,
                       31,32,33,34,35,36,37,
                       41,42,43,44,45,46,47,
                       51,52,53,54,55,56,57,
                       61,62,63,64,65,66,67,68,
                       71,72,73,74,75,76,77])
    elemType = np.ones(num_elem)*ESMF.MeshElemType.QUAD
    elemType[35] = 5
    elemType[36] = ESMF.MeshElemType.TRI
    elemType[37] = 5
    elemType[38] = ESMF.MeshElemType.TRI
    elemConn = np.array([11,12,22,21,12,13,23,22,13,14,24,23,14,15,25,24,15,16,26,25,16,17,27,26,17,18,28,27,
                         21,22,32,31,22,23,33,32,23,24,34,33,24,25,35,34,25,26,36,35,26,27,37,36,27,28,38,37,
                         31,32,42,41,32,33,43,42,33,34,44,43,34,35,45,44,35,36,46,45,36,37,47,46,37,38,48,47,
                         41,42,52,51,42,43,53,52,43,44,54,53,44,45,55,54,45,46,56,55,46,47,57,56,47,48,58,57,
                         51,52,62,61,52,53,63,62,53,54,64,63,54,55,65,64,55,56,66,65,56,57,67,66,57,58,68,67,
                         61, 62, 69, 72, 71, 62, 63, 69, 63, 64, 74, 73, 69, 69, 73, 72, 64, 65, 75, 74, 65, 66, 76, 75,
                         66, 67, 77, 76, 67, 68, 78, 77,
                         71,72,82,81,72,73,83,82,73,74,84,83,74,75,85,84,75,76,86,85,76,77,87,86,77,78,88,87])
    elemConn = np.array([np.where(a==nodeId) for a in elemConn]).flatten()
    # TODO: element coordinates is not supported for meshes containing ngons
    elemCoord = np.array(
        [0.5, 0.5, 1.0, 0.5, 1.5, 0.5, 2.0, 0.5, 2.5, 0.5, 3.0, 0.5, 3.5, 0.5,
         0.5, 1.0, 1.0, 1.0, 1.5, 1.0, 2.0, 1.0, 2.5, 1.0, 3.0, 1.0, 3.5, 1.0,
         0.5, 1.5, 1.0, 1.5, 1.5, 1.5, 2.0, 1.5, 2.5, 1.5, 3.0, 1.5, 3.5, 1.5,
         0.5, 2.0, 1.0, 2.0, 1.5, 2.0, 2.0, 2.0, 2.5, 2.0, 3.0, 2.0, 3.5, 2.0,
         0.5, 2.5, 1.0, 2.5, 1.5, 2.5, 2.0, 2.5, 2.5, 2.5, 3.0, 2.5, 3.5, 2.5,
         0.5, 3.0, 1.0, 2.875, 1.5, 3.0, 1.0, 3.12, 2.0, 3.0, 2.5, 3.0, 3.0, 3.0, 3.5, 3.0,
         0.5, 3.5, 1.0, 3.5, 1.5, 3.5, 2.0, 3.5, 2.5, 3.5, 3.0, 3.5, 3.5, 3.5])

    elemMask = None
    if domask:
        elemMask = np.ones(num_elem)
        elemMask[1] = 0
    elemArea = None
    if doarea:
        elemArea = np.ones(num_elem)*5
        elemArea[35] = 6.25
        elemArea[36] = 1.25
        elemArea[37] = 6.25
        elemArea[38] = 1.25

    mesh.add_nodes(num_node,nodeId,nodeCoord,nodeOwner)

    mesh.add_elements(num_elem,elemId,elemType,elemConn, element_area=elemArea, element_mask=elemMask)

    if domask and doarea:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn, elemMask, elemArea
    elif domask and not doarea:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn, elemMask
    elif not domask and doarea:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn, elemArea
    else:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn

def mesh_create_4_ngons(domask=False, doarea=False):
    '''
    PRECONDITIONS: None
    POSTCONDITIONS: A 4 element Mesh has been created.
    RETURN VALUES: \n Mesh :: mesh \n

      3.25  71 ------ 72 ----- 73 ------ 74
            |         \        /         |
            |          \  64  /          |
            |           \    /           |
            |            \  /            |
      3.00  |             69             |
            |            /  \            |
            |    61    /  62 \     63    |
            |         /       \          |
      2.75  61 ------ 62 ----- 63 ------ 64

           0.25      0.75 1.0 1.25      1.75

          Node Ids at corners
          Element Ids in centers

    Note: This mesh is not parallel, it can only be used in serial
    '''
    # Two parametric dimensions, and two spatial dimensions
    mesh = ESMF.Mesh(parametric_dim=2, spatial_dim=2)

    num_node = 9
    num_elem = 4
    nodeId = np.array([61,62,63,64,69,71,72,73,74,])
    nodeCoord = np.array([0.25,2.75, 0.75,2.75, 1.25,2.75, 1.75,2.75,
                          1.,3.,
                          0.25, 3.25, 0.75, 3.25, 1.25, 3.25, 1.75, 3.25])
    nodeOwner = np.zeros(num_node)
    elemId = np.array([61,62,63,64])
    elemType = np.ones(num_elem)
    elemType[0] = 5
    elemType[1] = ESMF.MeshElemType.TRI
    elemType[2] = 5
    elemType[3] = ESMF.MeshElemType.TRI
    elemConn = np.array([61, 62, 69, 72, 71, 62, 63, 69, 63, 64, 74, 73, 69, 69, 73, 72])
    elemConn = np.array([np.where(a==nodeId) for a in elemConn]).flatten()
    elemMask = None
    if domask:
        elemMask = np.ones(num_elem)
        elemMask[1] = 0
    elemArea = None
    if doarea:
        elemArea = np.ones(num_elem)*5
        elemArea[35] = 6.25
        elemArea[36] = 1.25
        elemArea[37] = 6.25
        elemArea[42] = 1.25

    mesh.add_nodes(num_node,nodeId,nodeCoord,nodeOwner)

    mesh.add_elements(num_elem,elemId,elemType,elemConn,
        element_mask=elemMask, element_area=elemArea)

    if domask and doarea:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn, elemMask, elemArea
    elif domask and not doarea:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn, elemMask
    elif not domask and doarea:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn, elemArea
    else:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn

def mesh_create_5_parallel ():
    '''
    PRECONDITIONS: None
    POSTCONDITIONS: A 5 element Mesh has been created in parallel.
    RETURN VALUES: \n Mesh :: mesh \n
    
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
    
    if ESMF.pet_count() > 1:
        if ESMF.pet_count() != 4:
                raise NameError('MPI rank must be 4 to build this mesh!')

    if (ESMF.local_pet() == 0):
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

    elif (ESMF.local_pet() == 1):
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

    elif (ESMF.local_pet() == 2):
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

    elif (ESMF.local_pet() == 3):
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

    return mesh, nodeCoord, nodeOwner, elemType, elemConn

def mesh_create_5_pentahexa_parallel ():
    '''
    PRECONDITIONS: None
    POSTCONDITIONS: A 5 element Mesh has been created in parallel.
    RETURN VALUES: \n Mesh :: mesh \n


    #  2.1        8               10 --------11
    #          /     \          /            |
    #        7         9     [9]             12
    #        |         |      |      5       /
    #        |    4    |      |            /
    #        |         |      |          /
    #  1.0  [4] ----- [5]    [5] ----- [6]
    #
    #       -0.1      1.0     1.0        2.1  2.5
    #
    #           PET 2               PET 3
    #
    #
    #  1.0   4 ------- 5      [5] ------- 6
    #        |         |       |  \   3  |
    #        |    1    |       |    \    |
    #        |         |       |  2   \  |
    #  -0.1  1 ------- 2      [2] ------- 3
    #
    #       -0.1      1.0     1.0        2.1  2.5
    #
    #           PET 0               PET 1
    #
    #               Node Id labels at corners
    #              Element Id labels in centers
    '''

    # Two parametric dimensions, and two spatial dimensions
    mesh = ESMF.Mesh(parametric_dim=2, spatial_dim=2)

    if ESMF.pet_count() > 1:
        if ESMF.pet_count() != 4:
                raise NameError('MPI rank must be 4 to build this mesh!')

    if (ESMF.local_pet() == 0):
        num_node=4
        num_elem=1

        nodeId=np.array([1, 2, 4, 5])
        nodeCoord=np.array([-0.1, -0.1,
                            1.0, -0.1,
                            - 0.1, 1.0,
                            1.0, 1.0 ])
        nodeOwner=np.zeros(num_node)
        elemId=np.array([1])
        elemType=np.array([ESMF.MeshElemType.QUAD])
        elemConn=np.array([0, 1, 3, 2 ])

    elif (ESMF.local_pet() == 1):
        num_node=4
        num_elem=2

        nodeId=np.array([2, 3, 5, 6])
        nodeCoord=np.array([1.0, -0.1,
                            2.1, -0.1,
                            1.0, 1.0,
                            2.1, 1.0 ])
        nodeOwner=np.array([0,
                            1,
                            0,
                            1])
        elemId=np.array([2, 3])
        elemType=np.array([ESMF.MeshElemType.TRI, ESMF.MeshElemType.TRI])
        elemConn=np.array([0, 1, 2,
                           1, 3, 2])

    elif (ESMF.local_pet() == 2):
        num_node=5
        num_elem=1

        nodeId=np.array([4, 5, 7, 8, 9])
        nodeCoord=np.array([-0.1, 1.0,
                            1.0, 1.0,
                            -0.1, 2.1,
                            0.5, 2.5,
                            1.0, 2.1 ])
        nodeOwner=np.array([0,
                            0,
                            2,
                            2,
                            2])
        elemId=np.array([4])
        elemType=np.array([5])
        elemConn=np.array([0, 1, 4, 3, 2])

    elif (ESMF.local_pet() == 3):
        num_node=6
        num_elem=1

        nodeId=np.array([5, 6, 9, 10, 11, 12])
        nodeCoord=np.array([1.0, 1.0,
                            2.1, 1.0,
                            1.0, 2.1,
                            1.5, 2.5,
                            2.5, 2.5,
                            2.5, 2.1 ])
        nodeOwner=np.array([0,
                            1,
                            2,
                            3,
                            3,
                            3])
        elemId=np.array([5])
        elemType=np.array([6])
        elemConn=np.array([0, 1, 5, 4, 3, 2])

    # Add nodes and elements to the Mesh
    mesh.add_nodes(num_node,nodeId,nodeCoord,nodeOwner)
    mesh.add_elements(num_elem,elemId,elemType,elemConn)

    return mesh, nodeCoord, nodeOwner, elemType, elemConn

def mesh_create_10_parallel ():
    '''
    PRECONDITIONS: None
    POSTCONDITIONS: A 10 element Mesh has been created in parallel.
    RETURN VALUES: \n Mesh :: mesh \n
    
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
    if ESMF.pet_count() > 1:
        if ESMF.pet_count() != 4:
                raise NameError('MPI rank must be 4 to build this mesh!')

    # Two parametric dimensions, and two spatial dimensions
    mesh = ESMF.Mesh(parametric_dim=2, spatial_dim=2)
    
    if (ESMF.local_pet() == 0):
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

    elif (ESMF.local_pet() == 1):
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

    elif (ESMF.local_pet() == 2):
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

    elif (ESMF.local_pet() == 3):
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

    return mesh, nodeCoord, nodeOwner, elemType, elemConn

def mesh_create_50_parallel(domask=False, doarea=False):
    '''
    PRECONDITIONS: None
    POSTCONDITIONS: A 50 element Mesh has been created in parallel.    
    RETURN VALUES: \n Mesh :: mesh \n
    
      3.75  81 ------ 82 ----- 83 ------ 84   [84] ----- 85 ------ 86 ------ 87 ------ 88
            |         |        |         |     |         |         |         |  77 /   |
            |    71   |   72   |    73   |     |    74   |   75    |    76   |    /    |
            |         |        |         |     |         |         |         |  /  78  |
      3.25  71 ------ 72 ----- 73 ------ 74   [74] ----- 75 ------ 76 ------ 77 ------ 78
            |         |        |         |     |         |         |         |         |
            |    61   |   62   |    63   |     |    64   |    65   |    66   |   67    |
            |         |        |         |     |         |         |         |         |
      2.75  61 ------ 62 ----- 63 ------ 64   [64] ----- 65 ------ 66 ------ 67 ------ 68
            |         |        |         |     |         |         |         |         |
            |    51   |   52   |    53   |     |    54   |    55   |    56   |   57    |
            |         |        |         |     |         |         |         |         |
      2.25  51 ------ 52 ----- 53 ------ 54   [54] ----- 55 ------ 56 ------ 57 ------ 58
            |         |        |         |     |         |         |         |         |
            |    41   |   42   |    43   |     |    44   |    45   |    46   |   47    |
            |         |        |         |     |         |         |         |         |
      1.75 [41] ---- [42] --- [43] ---- [44]  [44] ---- [45] ---- [46] ---- [47] ---- [48]

                        PET 2                                     PET 3


      1.75  41 ------ 42 ----- 43 ------ 44   [44] ----- 45 ------ 46 ------ 47 ------ 48
            |         |        |         |     |         |         |         |         |
            |    31   |   32   |    33   |     |    34   |    35   |    36   |   37    |
            |         |        |         |     |         |         |         |         |
      1.25  31 ------ 32 ----- 33 ------ 34   [34] ----- 35 ------ 36 ------ 37 ------ 38
            |         |        |         |     |         |         |         |         |
            |    21   |   22   |    23   |     |    24   |    25   |    26   |   27    |
            |         |        |         |     |         |         |         |         |
      0.75  21 ------ 22 ----- 23 ------ 24   [24] ----- 25 ------ 26 ------ 27 ------ 28
            |         |        |         |     |         |         |         |         |
            |    11   |   12   |    13   |     |    14   |    15   |    16   |   17    |
            |         |        |         |     |         |         |         |         |
      0.25  11 ------ 12 ----- 13 ------ 14   [14] ----- 15 ------ 16 ------ 17 ------ 18
    
           0.25      0.75     1.25      1.75  1.75      2.25      2.75      3.25      3.75
    
                        PET 0                                     PET 1

          Node Ids at corners
          Element Ids in centers
    '''
    if ESMF.pet_count() > 1:
        if ESMF.pet_count() != 4:
                raise NameError('MPI rank must be 4 to build this mesh!')

    # Two parametric dimensions, and two spatial dimensions
    mesh = ESMF.Mesh(parametric_dim=2, spatial_dim=2)
    
    if ESMF.local_pet() == 0:
        num_node = 16
        num_elem = 9
        nodeId = np.array([11,12,13,14,
                           21,22,23,24,
                           31,32,33,34,
                           41,42,43,44])
        nodeCoord = np.array([0.25,0.25, 0.25,0.75, 0.25,1.25, 0.25,1.75, 
                              0.75,0.25, 0.75,0.75, 0.75,1.25, 0.75,1.75, 
                              1.25,0.25, 1.25,0.75, 1.25,1.25, 1.25,1.75, 
                              1.75,0.25, 1.75,0.75, 1.75,1.25, 1.75,1.75])
        nodeOwner = np.zeros(num_node)
        elemId = np.array([11,12,13,
                           21,22,23,
                           31,32,33])
        elemType = np.ones(num_elem)*ESMF.MeshElemType.QUAD
        elemConn = np.array([11,12,22,21,12,13,23,22,13,14,24,23,
                             21,22,32,31,22,23,33,32,23,24,34,33,
                             31,32,42,41,32,33,43,42,33,34,44,43])
        elemConn = np.array([np.where(a==nodeId) for a in elemConn]).flatten()
        elemMask = None
        if domask:
            elemMask = np.ones(num_elem)
            elemMask[1] = 0
        elemArea = None
        if doarea:
            elemArea = np.ones(num_elem)*5

    elif ESMF.local_pet() == 1:
        num_node = 20
        num_elem = 12
        nodeId = np.array([14,15,16,17,18,
                           24,25,26,27,28,
                           34,35,36,37,38,
                           44,45,46,47,48])
        nodeCoord = np.array([0.25,1.75, 0.25,2.25, 0.25,2.75, 0.25,3.25, 0.25,3.75,
                              0.75,1.75, 0.75,2.25, 0.75,2.75, 0.75,3.25, 0.75,3.75,
                              1.25,1.75, 1.25,2.25, 1.25,2.75, 1.25,3.25, 1.25,3.75,
                              1.75,1.75, 1.75,2.25, 1.75,2.75, 1.75,3.25, 1.75,3.75])
        nodeOwner = np.array([0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1])
        elemId = np.array([14,15,16,17,
                           24,25,26,27,
                           34,35,36,37])
        elemType = np.ones(num_elem)*ESMF.MeshElemType.QUAD
        elemConn = np.array([14,15,25,24,15,16,26,25,16,17,27,26,17,18,28,27,
                             24,25,35,34,25,26,36,35,26,27,37,36,27,28,38,37,
                             34,35,45,44,35,36,46,45,36,37,47,46,37,38,48,47])
        elemConn = np.array([np.where(a==nodeId) for a in elemConn]).flatten()
        elemMask = None
        if domask:
            elemMask = np.ones(num_elem)
        elemArea = None
        if doarea:
            elemArea = np.ones(num_elem)*5

    elif ESMF.local_pet() == 2:
        num_node = 20
        num_elem = 12
        nodeId = np.array([41,42,43,44,
                           51,52,53,54,
                           61,62,63,64,
                           71,72,73,74,
                           81,82,83,84])
        nodeCoord = np.array([1.75,0.25, 1.75,0.75, 1.75,1.25, 1.75,1.75,
                              2.25,0.25, 2.25,0.75, 2.25,1.25, 2.25,1.75,
                              2.75,0.25, 2.75,0.75, 2.75,1.25, 2.75,1.75,
                              3.25,0.25, 3.25,0.75, 3.25,1.25, 3.25,1.75,
                              3.75,0.25, 3.75,0.75, 3.75,1.25, 3.75,1.75])
        nodeOwner = np.array([0,0,0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2])
        elemId = np.array([41,42,43,
                           51,52,53,
                           61,62,63,
                           71,72,73])
        elemType = np.ones(num_elem)*ESMF.MeshElemType.QUAD
        elemConn = np.array([41,42,52,51,42,43,53,52,43,44,54,53,
                             51,52,62,61,52,53,63,62,53,54,64,63,
                             61,62,72,71,62,63,73,72,63,64,74,73,
                             71,72,82,81,72,73,83,82,73,74,84,83])
        elemConn = np.array([np.where(a==nodeId) for a in elemConn]).flatten()
        elemMask = None
        if domask:
            elemMask = np.ones(num_elem)
        elemArea = None
        if doarea:
            elemArea = np.ones(num_elem)*5

    elif ESMF.local_pet() == 3:
        num_node = 25
        num_elem = 17
        nodeId = np.array([44,45,46,47,48,
                           54,55,56,57,58,
                           64,65,66,67,68,
                           74,75,76,77,78,
                           84,85,86,87,88])
        nodeCoord = np.array([1.75,1.75, 1.75,2.25, 1.75,2.75, 1.75,3.25, 1.75,3.75,
                              2.25,1.75, 2.25,2.25, 2.25,2.75, 2.25,3.25, 2.25,3.75,
                              2.75,1.75, 2.75,2.25, 2.75,2.75, 2.75,3.25, 2.75,3.75,
                              3.25,1.75, 3.25,2.25, 3.25,2.75, 3.25,3.25, 3.25,3.75,
                              3.75,1.75, 3.75,2.25, 3.75,2.75, 3.75,3.25, 3.75,3.75])
        nodeOwner = np.array([0,1,1,1,1,2,3,3,3,3,2,3,3,3,3,2,3,3,3,3,2,3,3,3,3])
        elemId = np.array([44,45,46,47,
                           54,55,56,57,
                           64,65,66,67,
                           74,75,76,77,78])
        elemType = np.ones(num_elem-2)*ESMF.MeshElemType.QUAD
        elemType = np.append(elemType, [ESMF.MeshElemType.TRI, ESMF.MeshElemType.TRI])
        elemConn = np.array([44,45,55,54,45,46,56,55,46,47,57,56,47,48,58,57,
                             54,55,65,64,55,56,66,65,56,57,67,66,57,58,68,67,
                             64,65,75,74,65,66,76,75,66,67,77,76,67,68,78,77,
                             74,75,85,84,75,76,86,85,76,77,87,86, 
                             77,88,87,
                             77,78,88])
        elemConn = np.array([np.where(a==nodeId) for a in elemConn]).flatten()
        elemMask = None
        if domask:
            elemMask = np.ones(num_elem)
        elemArea = None
        if doarea:
            elemArea = np.ones(num_elem-2)*5
            elemArea = np.append(elemArea, [2.5, 2.5])

    mesh.add_nodes(num_node,nodeId,nodeCoord,nodeOwner)

    mesh.add_elements(num_elem,elemId,elemType,elemConn, 
        element_mask=elemMask, element_area=elemArea)

    if domask and doarea:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn, elemMask, elemArea
    elif domask and not doarea:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn, elemMask
    elif not domask and doarea:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn, elemArea
    else:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn

def mesh_create_50_ngons_parallel(domask=False, doarea=False):
    '''
    PRECONDITIONS: None
    POSTCONDITIONS: A 50 element Mesh has been created in parallel.
    RETURN VALUES: \n Mesh :: mesh \n

      3.75  81 ------ 82 ----- 83 ------ 84   [84] ----- 85 ------ 86 ------ 87 ------ 88
            |         |        |         |     |         |         |         |         |
            |    71   |   72   |    73   |     |    74   |   75    |    76   |   77    |
            |         |        |         |     |         |         |         |         |
      3.25  71 ------ 72 ----- 73 ------ 74   [74] ----- 75 ------ 76 ------ 77 ------ 78
            |         \        /         |     |         |         |         |         |
            |          \  68  /          |     |         |         |         |         |
            |           \    /           |     |         |         |         |         |
            |            \  /            |     |         |         |         |         |
      3.00  |             69             |     |         |         |         |         |
            |            /  \            |     |         |         |         |         |
            |    61    /  62 \     63    |     |    64   |    65   |    66   |   67    |
            |         /       \          |     |         |         |         |         |
      2.75  61 ------ 62 ----- 63 ------ 64   [64] ----- 65 ------ 66 ------ 67 ------ 68
            |         |        |         |     |         |         |         |         |
            |    51   |   52   |    53   |     |    54   |    55   |    56   |   57    |
            |         |        |         |     |         |         |         |         |
      2.25  51 ------ 52 ----- 53 ------ 54   [54] ----- 55 ------ 56 ------ 57 ------ 58
            |         |        |         |     |         |         |         |         |
            |    41   |   42   |    43   |     |    44   |    45   |    46   |   47    |
            |         |        |         |     |         |         |         |         |
      1.75 [41] ---- [42] --- [43] ---- [44]  [44] ---- [45] ---- [46] ---- [47] ---- [48]

                        PET 2                                     PET 3


      1.75  41 ------ 42 ----- 43 ------ 44   [44] ----- 45 ------ 46 ------ 47 ------ 48
            |         |        |         |     |         |         |         |         |
            |    31   |   32   |    33   |     |    34   |    35   |    36   |   37    |
            |         |        |         |     |         |         |         |         |
      1.25  31 ------ 32 ----- 33 ------ 34   [34] ----- 35 ------ 36 ------ 37 ------ 38
            |         |        |         |     |         |         |         |         |
            |    21   |   22   |    23   |     |    24   |    25   |    26   |   27    |
            |         |        |         |     |         |         |         |         |
      0.75  21 ------ 22 ----- 23 ------ 24   [24] ----- 25 ------ 26 ------ 27 ------ 28
            |         |        |         |     |         |         |         |         |
            |    11   |   12   |    13   |     |    14   |    15   |    16   |   17    |
            |         |        |         |     |         |         |         |         |
      0.25  11 ------ 12 ----- 13 ------ 14   [14] ----- 15 ------ 16 ------ 17 ------ 18

           0.25      0.75 1.0 1.25      1.75  1.75      2.25      2.75      3.25      3.75

                        PET 0                                     PET 1

          Node Ids at corners
          Element Ids in centers
    '''
    if ESMF.pet_count() > 1:
        if ESMF.pet_count() != 4:
                raise NameError('MPI rank must be 4 to build this mesh!')

    # Two parametric dimensions, and two spatial dimensions
    mesh = ESMF.Mesh(parametric_dim=2, spatial_dim=2)

    if ESMF.local_pet() == 0:
        num_node = 16
        num_elem = 9
        nodeId = np.array([11,12,13,14,
                           21,22,23,24,
                           31,32,33,34,
                           41,42,43,44])
        nodeCoord = np.array([0.25,0.25, 0.75,0.25, 1.25,0.25, 1.75,0.25,
                              0.25,0.75, 0.75,0.75, 1.25,0.75, 1.75,0.75,
                              0.25,1.25, 0.75,1.25, 1.25,1.25, 1.75,1.25,
                              0.25,1.75, 0.75,1.75, 1.25,1.75, 1.75,1.75])
        nodeOwner = np.zeros(num_node)
        elemId = np.array([11,12,13,
                           21,22,23,
                           31,32,33])
        elemType = np.ones(num_elem)*ESMF.MeshElemType.QUAD
        elemConn = np.array([11,12,22,21,12,13,23,22,13,14,24,23,
                             21,22,32,31,22,23,33,32,23,24,34,33,
                             31,32,42,41,32,33,43,42,33,34,44,43])
        elemConn = np.array([np.where(a==nodeId) for a in elemConn]).flatten()
        elemMask = None
        if domask:
            elemMask = np.ones(num_elem)
            elemMask[1] = 0
        elemArea = None
        if doarea:
            elemArea = np.ones(num_elem)*5

    elif ESMF.local_pet() == 1:
        num_node = 20
        num_elem = 12
        nodeId = np.array([14,15,16,17,18,
                           24,25,26,27,28,
                           34,35,36,37,38,
                           44,45,46,47,48])
        nodeCoord = np.array([1.75,0.25, 2.25,0.25, 2.75,0.25, 3.25,0.25, 3.75,0.25,
                              1.75,0.75, 2.25,0.75, 2.75,0.75, 3.25,0.75, 3.75,0.75,
                              1.75,1.25, 2.25,1.25, 2.75,1.25, 3.25,1.25, 3.75,1.25,
                              1.75,1.75, 2.25,1.75, 2.75,1.75, 3.25,1.75, 3.75,1.75])
        nodeOwner = np.array([0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1])
        elemId = np.array([14,15,16,17,
                           24,25,26,27,
                           34,35,36,37])
        elemType = np.ones(num_elem)*ESMF.MeshElemType.QUAD
        elemConn = np.array([14,15,25,24,15,16,26,25,16,17,27,26,17,18,28,27,
                             24,25,35,34,25,26,36,35,26,27,37,36,27,28,38,37,
                             34,35,45,44,35,36,46,45,36,37,47,46,37,38,48,47])
        elemConn = np.array([np.where(a==nodeId) for a in elemConn]).flatten()
        elemMask = None
        if domask:
            elemMask = np.ones(num_elem)
        elemArea = None
        if doarea:
            elemArea = np.ones(num_elem)*5

    elif ESMF.local_pet() == 2:
        num_node = 21
        num_elem = 13
        nodeId = np.array([41,42,43,44,
                           51,52,53,54,
                           61,62,63,64,69,
                           71,72,73,74,
                           81,82,83,84])
        nodeCoord = np.array([0.25,1.75, 0.75,1.75, 1.25,1.75, 1.75,1.75,
                              0.25,2.25, 0.75,2.25, 1.25,2.25, 1.75,2.25,
                              0.25,2.75, 0.75,2.75, 1.25,2.75, 1.75,2.75, 1.0,3.0,
                              0.25,3.25, 0.75,3.25, 1.25,3.25, 1.75,3.25,
                              0.25,3.75, 0.75,3.75, 1.25,3.75, 1.75,3.75])
        nodeOwner = np.array([0,0,0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2])
        elemId = np.array([41,42,43,
                           51,52,53,
                           61,62,63,68,
                           71,72,73])
        elemType = np.ones(num_elem)*ESMF.MeshElemType.QUAD
        elemType[6] = 5
        elemType[7] = ESMF.MeshElemType.TRI
        elemType[8] = 5
        elemType[9] = ESMF.MeshElemType.TRI
        elemConn = np.array([41,42,52,51,42,43,53,52,43,44,54,53,
                             51,52,62,61,52,53,63,62,53,54,64,63,
                             61,62,69,72,71,62,63,69,63,64,74,73,69,69,73,72,
                             71,72,82,81,72,73,83,82,73,74,84,83])
        elemConn = np.array([np.where(a==nodeId) for a in elemConn]).flatten()
        elemMask = None
        if domask:
            elemMask = np.ones(num_elem)
        elemArea = None
        if doarea:
            elemArea = np.ones(num_elem)*5
            elemArea[6] = 6.25
            elemArea[7] = 1.25
            elemArea[8] = 6.25
            elemArea[9] = 1.25

    elif ESMF.local_pet() == 3:
        num_node = 25
        num_elem = 16
        nodeId = np.array([44,45,46,47,48,
                           54,55,56,57,58,
                           64,65,66,67,68,
                           74,75,76,77,78,
                           84,85,86,87,88])
        nodeCoord = np.array([1.75,1.75, 2.25,1.75, 2.75,1.75, 3.25,1.75, 3.75,1.75,
                              1.75,2.25, 2.25,2.25, 2.75,2.25, 3.25,2.25, 3.75,2.25,
                              1.75,2.75, 2.25,2.75, 2.75,2.75, 3.25,2.75, 3.75,2.75,
                              1.75,3.25, 2.25,3.25, 2.75,3.25, 3.25,3.25, 3.75,3.25,
                              1.75,3.75, 2.25,3.75, 2.75,3.75, 3.25,3.75, 3.75,3.75])
        nodeOwner = np.array([0,1,1,1,1,2,3,3,3,3,2,3,3,3,3,2,3,3,3,3,2,3,3,3,3])
        elemId = np.array([44,45,46,47,
                           54,55,56,57,
                           64,65,66,67,
                           74,75,76,77])
        elemType = np.ones(num_elem)*ESMF.MeshElemType.QUAD
        elemConn = np.array([44,45,55,54,45,46,56,55,46,47,57,56,47,48,58,57,
                             54,55,65,64,55,56,66,65,56,57,67,66,57,58,68,67,
                             64,65,75,74,65,66,76,75,66,67,77,76,67,68,78,77,
                             74,75,85,84,75,76,86,85,76,77,87,86,77,78,88,87])
        elemConn = np.array([np.where(a==nodeId) for a in elemConn]).flatten()
        elemMask = None
        if domask:
            elemMask = np.ones(num_elem)
        elemArea = None
        if doarea:
            elemArea = np.ones(num_elem)*5

    mesh.add_nodes(num_node,nodeId,nodeCoord,nodeOwner)

    mesh.add_elements(num_elem,elemId,elemType,elemConn,
        element_mask=elemMask, element_area=elemArea)

    if domask and doarea:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn, elemMask, elemArea
    elif domask and not doarea:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn, elemMask
    elif not domask and doarea:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn, elemArea
    else:
        return mesh, nodeCoord, nodeOwner, elemType, elemConn

def initialize_field_mesh(field, nodeCoord, nodeOwner, elemType, elemConn,
                          domask=False, elemMask=None):
    '''
    PRECONDITIONS: A Field has been created on the elements of a Mesh.
    POSTCONDITIONS: The Field has been initialized to an analytic 
                    field.
    RETURN VALUES: \n Field :: field \n
    '''

    [node, element] = [0,1]

    if field.staggerloc == element:
        offset = 0
        for i in range(field.grid.size_owned[element]):
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

            #print ('[{0},{1}] = {2}'.format(x,y,field.data[i]))
            field.data[i] = 20.0 + x**2 +x*y + y**2

            if domask:
                # calculate field
                if (elemMask[i] == 0):
                    field.data[i] = 0

    
    elif field.staggerloc == node:
        ind = 0
        for i in range(field.grid.size[node]):
            x = nodeCoord[i*2]
            y = nodeCoord[i*2+1]

            if (nodeOwner[i] == ESMF.local_pet()):
                if ind > field.grid.size_owned[node]:
                    raise ValueError("Overstepped the mesh bounds!")
                field.data[ind] = 20.0 + x**2 +x*y + y**2
                #print ('[{0},{1}] = {2}'.format(x,y,field.data[ind]))
                ind += 1

            if domask:
                # calculate field
                if (elemMask[i] == 0):
                    field.data[i] = 0

    else:
        raise ValueError("Field staggerloc is not supported")

    return field

def compute_mass_mesh(valuefield, dofrac=False, fracfield=None,
                      uninitval=422397696.):
    '''
    PRECONDITIONS: 'fracfield' contains the fractions of each cell
                   which contributed to a regridding operation involving
                   'valuefield.  'dofrac' is a boolean value that gives 
                   the option to not use the 'fracfield'.\n
    POSTCONDITIONS: The mass of the data field is computed.\n
    RETURN VALUES: float :: mass \n
    '''
    mass = 0.0
    # mesh area field must be built on elements
    areafield = ESMF.Field(valuefield.grid, name='areafield',
                           meshloc=ESMF.MeshLoc.ELEMENT)
    areafield.get_area()

    ind = np.where(valuefield.data != uninitval)
    if dofrac:
        mass = np.sum(areafield.data[ind[0]] * valuefield.data[ind[0]] * fracfield.data[ind[0]])
    else:
        mass = np.sum(areafield.data[ind[0]] * valuefield.data[ind[0]])

    return mass
