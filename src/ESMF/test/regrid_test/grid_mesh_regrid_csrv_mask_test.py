# $Id$

"""
Two Field objects are created, one on a Grid and the other on a Mesh.  
A mask is set on the mesh which is used later in the regridding.  The
source Field is set to an analytic function, and a conservative 
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
    gridXCorner = grid.get_coords(x, staggerloc=ESMF.StaggerLoc.CORNER)
    gridYCorner = grid.get_coords(y, staggerloc=ESMF.StaggerLoc.CORNER)

    for i in xrange(gridXCorner.shape[x]):
        gridXCorner[i, :] = float(i)*cellwidth_x + lb_x

    for j in xrange(gridYCorner.shape[y]):
        gridYCorner[:, j] = float(j)*cellwidth_y + lb_y

    ##     CENTERS
    grid.add_coords(staggerloc=[ESMF.StaggerLoc.CENTER])

    # get the coordinate pointers and set the coordinates
    [x,y] = [0,1]
    gridXCenter = grid.get_coords(x, staggerloc=ESMF.StaggerLoc.CENTER)
    gridYCenter = grid.get_coords(y, staggerloc=ESMF.StaggerLoc.CENTER)

    for i in xrange(gridXCenter.shape[x]):
        gridXCenter[i, :] = float(j)*cellwidth_x + lb_x + cellwidth_x/2.0

    for j in xrange(gridXCenter.shape[y]):
        gridYCenter[:, j] = float(j)*cellwidth_y + lb_y + cellwidth_y/2.0

    ## Mask

    mask = grid.add_item(ESMF.GridItem.MASK)

    maskregionX = [(ub_x-lb_x)/2.-0.5, (ub_x-lb_x)/2.+0.5]
    maskregionY = [(ub_y-lb_y)/2.-0.5, (ub_y-lb_y)/2.+0.5]

    for i in range(mask.shape[x]):
        for j in range(mask.shape[y]):
            if (maskregionX[0] < gridXCenter[i, j] < maskregionX[1] and
                    maskregionY[0] < gridYCenter[i, j] < maskregionY[1]):
                mask[i, j] = 1
            else:
                mask[i, j] = 0

    ## Area

    grid.add_item(ESMF.GridItem.AREA)

    area = grid.get_item(ESMF.GridItem.AREA)

    area[:] = 5.0

    return grid

def mesh_create_3x3():
    '''
    PRECONDITIONS: ESMPy has been initialized.
    POSTCONDITIONS: A Mesh (3x3) has been created and returned as 
                    'mesh'.
    
                   3x3 Mesh
    
    
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
    
          (Everything owned by PET 0)
    '''
    # set up a simple mesh
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
    elemMask = np.array([0,0,0,0,1,0,0,0,0,0])
    elemArea = np.array([5,5,5,5,5,5,5,5,2.5,2.5], dtype=np.float64)

    mesh = ESMF.Mesh(parametric_dim=2, spatial_dim=2)

    mesh.add_nodes(num_node, nodeId, nodeCoord, nodeOwner)

    mesh.add_elements(num_elem, elemId, elemType, elemConn, 
                      element_mask=elemMask, element_area=elemArea)

    mesh._write("mesh")

    return mesh, nodeCoord, elemType, elemConn, elemMask, elemArea

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
    # get the mask
    mask = grid.get_item(ESMF.GridItem.MASK)

    # get the coordinate pointers and set the coordinates
    [x,y] = [0,1]
    gridXCoord = grid.get_coords(x, ESMF.StaggerLoc.CENTER)
    gridYCoord = grid.get_coords(y, ESMF.StaggerLoc.CENTER)

    area = grid.get_item(ESMF.GridItem.AREA)

    # TODO: temporary fix: manually access the field in fortran order
    mass = 0
    for i in range(gridXCoord.shape[x]):
        for j in range(gridYCoord.shape[y]):
            field[i, j] = 20.0 + gridXCoord[i, j] + gridYCoord[i, j]
            if mask[i, j] == 1:
                field[i, j] = 10000000000000
            mass = mass + area[i, j] * field[i, j]

    #fieldPtr[:] = 20.0+gridXCoord.flat[:]+gridYCoord.flat[:]
    #fieldPtr [ [i for i in range(len(mask.flat)) 
    #            if mask.flat[i] == 1] ] = 10000000000000
    #mass = area.flat[:]*fieldPtr[:]

    return field, mass

def create_field(mesh, name):
    '''
    PRECONDITIONS: A Mesh has been created, and 'name' is a string that
                   will be used to initialize the name of a new Field.
    POSTCONDITIONS: A Field has been created.
    '''
    field = ESMF.Field(mesh, name, meshloc=ESMF.MeshLoc.ELEMENT)

    return field

def build_analyticfield(field, nodeCoord, elemType, elemConn, 
                        elemMask, elemArea):
    '''
    PRECONDITIONS: A Field has been created.
    POSTCONDITIONS: The 'field' has been initialized to an analytic 
                    field.
    '''

    # set the field to a vanilla initial field for now
    offset = 0
    mass = 0
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

            # calculate field
            if (elemMask[i] == 0):
                field[i] = 20.0 + x + y
            elif (elemMask[i] == 1):
                field[i] = 1000000000.0
 
            # calculate the mass
            mass = mass + elemArea[i]*field[i]
            
        elif (elemType[i] == ESMF.MeshElemType.QUAD):
            x1 = nodeCoord[(elemConn[offset])*2]
            x2 = nodeCoord[(elemConn[offset+1])*2]
            y1 = nodeCoord[(elemConn[offset+1])*2+1]
            y2 = nodeCoord[(elemConn[offset+3])*2+1]
            x = (x1 + x2) / 2.0
            y = (y1 + y2) / 2.0
            offset = offset + 4

            # calculate field
            if (elemMask[i] == 0):
                field[i] = 20.0 + x + y
            elif (elemMask[i] == 1):
                field[i] = 1000000000.0
 
            # calculate the mass
            mass = mass + elemArea[i]*field[i]
            
        else:
            raise NameError("Elem type is not supported.")


    return field, mass

def run_regridding(srcfield, dstfield, srcfracfield, dstfracfield):
    '''
    PRECONDITIONS: Two Fields have been created and a regridding 
                   operation is desired from 'srcfield' to 'dstfield'.
    POSTCONDITIONS: A regridding operation has set the data on 
                    'dstfield'.
    '''
    # call the regridding functions
    regridSrc2Dst = ESMF.Regrid(srcfield, dstfield, \
                                src_mask_values=np.array([1]), \
                                regrid_method=ESMF.RegridMethod.CONSERVE, \
                                unmapped_action=ESMF.UnmappedAction.ERROR, \
                                src_frac_field=srcfracfield, \
                                dst_frac_field=dstfracfield)
    dstfield = regridSrc2Dst(srcfield, dstfield)

    return dstfield, srcfracfield, dstfracfield


def compare_fields(interpfield, exactfield, dstfracfield, mass1, mass2):
    '''
    PRECONDITIONS: 'interp_field' is a Field that holds the values 
                   resulting from a regridding operation, 'exact_field' 
                   is a Field containing the values of the exact 
                   solution that is expected, and 'dstfracfield' is a 
                   Field containing the fractions of the 'interp_field' 
                   which contributed to the regridding product.  
                   'mass1' and 'mass2' are the mass values for the 
                   source and destination data fields.  'parallel' is 
                   an internal variable used to determine if this
                   run should be done in serial or parallel.\n
    POSTCONDITIONS: The interpolation accuracy of a regridding 
                    operation is determined by comparing the 
                    'interp_field' to the 'exact_field'.  The mass 
                    conservation is validated by comparing the 'mass1' 
                    to the 'mass2'.\n
    RETURN VALUES: None \n
    '''

    if (interpfield.shape != exactfield.shape != dstfracfield.shape):
        raise TypeError('compare_fields: Fields must be the same size!')

    # initialize to True, and check for False point values
    [x, y] = [0, 1]
    total_error = 0.0
    max_error = 0.0
    min_error = 1000000.0
    for i in range(interpfield.shape[x]):
        for j in range(interpfield.shape[y]):
            if (exactfield[i, j] != 0.0):
                err = abs(interpfield[i, j]/dstfracfield[i, j] - \
                            exactfield[i, j])/abs(exactfield[i, j])
            else:
                err = abs(interpfield[i, j]/dstfracfield[i, j] - \
                            exactfield[i, j])
            total_error = total_error + err
            if (err > max_error):
                max_error = err
            if (err < min_error):
                min_error = err

    # check the mass
    csrv = False
    csrv_error = abs(mass2 - mass1)/mass1
    if (csrv_error < 10e-12):
        csrv = True
    itrp = False
    if (max_error < 1):
        itrp = True

    if (itrp and csrv):
        print "PASS"
    else:
        print "FAIL"
    print "  Total error = "+str(total_error)
    print "  Max error   = "+str(max_error)
    print "  Min error   = "+str(min_error)
    print "  Csrv error  = "+str(csrv_error)
    print "  srcmass     = "+str(mass1)
    print "  dstmass     = "+str(mass2)

    return

def compute_mass_grid(valuefield, areafield, fracfield, dofrac):
    '''
    PRECONDITIONS: Two Fields have been created and initialized.  
                   'valuefield' contains data values of a field built 
                   on the cells of a grid, 'areafield' contains the 
                   areas associated with the grid cells, and 
                   'fracfield' contains the fractions of each cell 
                   which contributed to a regridding operation involving
                   'valuefield.  'dofrac' is a boolean value that gives 
                   the option to not use the 'fracfield'.\n
    POSTCONDITIONS: The mass of the data field is computed.\n
    RETURN VALUES: integer :: mass \n
    '''
    [x, y] = [0, 1]
    mass = 0.0
    areafield.get_area()
    frac = 0
    for i in range(valuefield.shape[x]):
        for j in range(valuefield.shape[y]):
            if dofrac:
                mass += areafield[i, j] * valuefield[i, j] * \
                                fracfield[i, j]
            else:
                mass += areafield[i, j] * valuefield[i, j]

    return mass

def compute_mass_mesh(valuefield, areafield, fracfield, dofrac):
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

def test_main():
    print "\ngrid_mesh_regrid_csrv_mask"

    # start up ESMF
    # this call is not necessary unless you want to to override the
    # default options:
    #  LogKind = NONE
    #  debug = False
    #manager = ESMF.Manager(logkind=ESMF.LogKind.SINGLE, debug=True)

    # create two unique Mesh objects
    grid = grid_create()
    mesh, nodeCoord, elemType, elemConn, elemMask, elemArea = mesh_create_3x3()

    # create Field objects on the Meshes
    srcfield = create_field(mesh, 'srcfield')
    srcfracfield = create_field(mesh, 'srcfracfield')
    srcareafield = create_field(mesh, 'srcareafield')

    # make gridded fields
    dstfield = create_fieldgrid(grid, 'dstfield')
    exactfield = create_fieldgrid(grid, 'exact_field')
    dstfracfield = create_fieldgrid(grid, 'dstfracfield')
    dstareafield = create_fieldgrid(grid, 'dstareafield')

    # initialize the Fields to an analytic function
    srcfield, srcmass = build_analyticfield(srcfield, nodeCoord, elemType,
                                            elemConn, elemMask, elemArea)
    #dstfield, dstmass = build_analyticfieldgrid(dstfield, grid)
    exactfield, dstmass = build_analyticfieldgrid(exactfield, grid)

    # run the ESMF regridding
    dstfield, srcfracfield, dstfracfield = run_regridding(srcfield, dstfield,
                                                          srcfracfield, 
                                                          dstfracfield)

    # compute the mass
    srcmass = compute_mass_mesh(srcfield, srcareafield, srcfracfield, True)
    dstmass = compute_mass_grid(dstfield, dstareafield, 0, False)

    # compare results and output PASS or FAIL
    compare_fields(dstfield, exactfield, dstfracfield, srcmass, dstmass)

    print "\n"
    return 0

if __name__ == '__main__':
    sys.exit(test_main())
