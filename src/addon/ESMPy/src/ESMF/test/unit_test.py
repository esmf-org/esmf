# $Id$

"""
unit test file
"""

import sys
import traceback
import logging

try:
    from ESMF import *
    from ESMF.test.regrid_test.mesh_regridding_utilities import mesh_create_50, mesh_create_50_parallel
    from ESMF.test.regrid_test.grid_regridding_utilities import grid_create
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

def log_test():

    flush = True
    ESMP_LogSet(flush)

    # return True from unit test
    return True

def vm_test():
    # inquire for rank and proc from ESMF Virtual Machine
    localpet = local_pet()
    petcount = pet_count()

    print '\nlocal_pet = {0}\n'.format(localpet)
    print '\npet_count = {0}\n'.format(petcount)

    # return True from unit test
    return True

def grid_create_test():

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

def grid_create_3D_test():

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

def grid_coords_test():

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

    #grid._write("gridcoords2DCart")

    # return correct from unit test
    return correct

def grid_coords_3D_test():

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

    #grid._write("gridcoord3DCart")

    # return correct from unit test
    return correct

def grid_mask_test():

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

    # return True from unit test
    return correct

def field_mask_test():

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
                mask[i, j] = 2
            elif (i == 3.0):
                mask[i,j] = 3
            else:
                mask[i, j] = 0;

    # create a Field on the Grid, should inherit the mask
    field = Field(grid, "FIELD!", mask_values = [2])
     
    if(not field.mask[2][0]):
        correct = False
        print field.mask
        raise ValueError("field mask is incorrect")

    # return True from unit test
    return correct

def grid_mask_3D_test():

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

def grid_area_test():

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

def grid_area_3D_test():

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

def grid_create_from_file_scrip_test():
    reg_decomp = [pet_count(), 1]
    try:
        grid_from_file = Grid(filename="src/ESMF/test/data/T42_grid.nc",
                              filetype=FileFormat.SCRIP,
                              reg_decomp=reg_decomp)
    except:
        raise NameError('grid_create_from_file_scrip failed!')
    return True

def grid_create_from_file_scrip_decomp_balanced_balanced_test():
    reg_decomp = [pet_count(), 1]
    decompflag = np.array([DecompFlag.BALANCED,DecompFlag.BALANCED],
                          dtype=np.int32)
    try:
        grid_from_file = Grid(filename="src/ESMF/test/data/T42_grid.nc",
                              filetype=FileFormat.SCRIP,
                              reg_decomp=reg_decomp, decompflag=decompflag)
    except:
        raise NameError('grid_create_from_file_scrip_balanced_balanced failed!')
    return True

def grid_create_from_file_scrip_decomp_balanced_restfirst_test():
    reg_decomp = [pet_count(), 1]
    decompflag = np.array([DecompFlag.BALANCED,DecompFlag.RESTFIRST],
                          dtype=np.int32)
    try:
        grid_from_file = Grid(filename="src/ESMF/test/data/T42_grid.nc",
                              filetype=FileFormat.SCRIP,
                              reg_decomp=reg_decomp, decompflag=decompflag)
    except:
        raise NameError('grid_create_from_file_scrip_balanced_restfirst failed!')
    return True

def grid_create_from_file_scrip_decomp_balanced_restlast_test():
    reg_decomp = [pet_count(), 1]
    decompflag = np.array([DecompFlag.BALANCED,DecompFlag.RESTLAST],
                          dtype=np.int32)
    try:
        grid_from_file = Grid(filename="src/ESMF/test/data/T42_grid.nc",
                              filetype=FileFormat.SCRIP,
                              reg_decomp=reg_decomp, decompflag=decompflag)
    except:
        raise NameError('grid_create_from_file_scrip_balanced_restlast failed!')
    return True

@expected_failure
def grid_create_from_file_scrip_decomp_balanced_cyclic_test():
    reg_decomp = [pet_count(), 1]
    decompflag = np.array([DecompFlag.BALANCED,DecompFlag.CYCLIC],
                          dtype=np.int32)
    try:
        grid_from_file = Grid(filename="src/ESMF/test/data/T42_grid.nc",
                              filetype=FileFormat.SCRIP,
                              reg_decomp=reg_decomp, decompflag=decompflag)
    except:
        raise NameError('grid_create_from_file_scrip_balanced_cyclic failed!')
    return True

def grid_create_from_file_scrip_decomp_restfirst_balanced_test():
    reg_decomp = [pet_count(), 1]
    decompflag = np.array([DecompFlag.RESTFIRST,DecompFlag.BALANCED],
                          dtype=np.int32)
    try:
        grid_from_file = Grid(filename="src/ESMF/test/data/T42_grid.nc",
                              filetype=FileFormat.SCRIP,
                              reg_decomp=reg_decomp, decompflag=decompflag)
    except:
        raise NameError('grid_create_from_file_scrip_restfirst_balanced failed!')
    return True

def grid_create_from_file_scrip_decomp_restfirst_restfirst_test():
    reg_decomp = [pet_count(), 1]
    decompflag = np.array([DecompFlag.RESTFIRST,DecompFlag.RESTFIRST],
                          dtype=np.int32)
    try:
        grid_from_file = Grid(filename="src/ESMF/test/data/T42_grid.nc",
                              filetype=FileFormat.SCRIP,
                              reg_decomp=reg_decomp, decompflag=decompflag)
    except:
        raise NameError('grid_create_from_file_scrip_restfirst_restfirst failed!')
    return True

def grid_create_from_file_scrip_decomp_restfirst_restlast_test():
    reg_decomp = [pet_count(), 1]
    decompflag = np.array([DecompFlag.RESTFIRST,DecompFlag.RESTLAST],
                          dtype=np.int32)
    try:
        grid_from_file = Grid(filename="src/ESMF/test/data/T42_grid.nc",
                              filetype=FileFormat.SCRIP,
                              reg_decomp=reg_decomp, decompflag=decompflag)
    except:
        raise NameError('grid_create_from_file_scrip_restfirst_restlast failed!')
    return True

@expected_failure
def grid_create_from_file_scrip_decomp_restfirst_cyclic_test():
    reg_decomp = [pet_count(), 1]
    decompflag = np.array([DecompFlag.RESTFIRST,DecompFlag.CYCLIC],
                          dtype=np.int32)
    try:
        grid_from_file = Grid(filename="src/ESMF/test/data/T42_grid.nc",
                              filetype=FileFormat.SCRIP,
                              reg_decomp=reg_decomp, decompflag=decompflag)
    except:
        raise NameError('grid_create_from_file_scrip_restfirst_cyclic failed!')
    return True

def grid_create_from_file_scrip_decomp_restlast_balanced_test():
    reg_decomp = [pet_count(), 1]
    decompflag = np.array([DecompFlag.RESTLAST,DecompFlag.BALANCED],
                          dtype=np.int32)
    try:
        grid_from_file = Grid(filename="src/ESMF/test/data/T42_grid.nc",
                              filetype=FileFormat.SCRIP,
                              reg_decomp=reg_decomp, decompflag=decompflag)
    except:
        raise NameError('grid_create_from_file_scrip_restlast_balanced failed!')
    return True

def grid_create_from_file_scrip_decomp_restlast_restfirst_test():
    reg_decomp = [pet_count(), 1]
    decompflag = np.array([DecompFlag.RESTLAST,DecompFlag.RESTFIRST],
                          dtype=np.int32)
    try:
        grid_from_file = Grid(filename="src/ESMF/test/data/T42_grid.nc",
                              filetype=FileFormat.SCRIP,
                              reg_decomp=reg_decomp, decompflag=decompflag)
    except:
        raise NameError('grid_create_from_file_scrip_restlast_restfirst failed!')
    return True

def grid_create_from_file_scrip_decomp_restlast_restlast_test():
    reg_decomp = [pet_count(), 1]
    decompflag = np.array([DecompFlag.RESTLAST,DecompFlag.RESTLAST],
                          dtype=np.int32)
    try:
        grid_from_file = Grid(filename="src/ESMF/test/data/T42_grid.nc",
                              filetype=FileFormat.SCRIP,
                              reg_decomp=reg_decomp, decompflag=decompflag)
    except:
        raise NameError('grid_create_from_file_scrip_restlast_restlast failed!')
    return True

@expected_failure
def grid_create_from_file_scrip_decomp_restlast_cyclic_test():
    reg_decomp = [pet_count(), 1]
    decompflag = np.array([DecompFlag.RESTLAST,DecompFlag.CYCLIC],
                          dtype=np.int32)
    try:
        grid_from_file = Grid(filename="src/ESMF/test/data/T42_grid.nc",
                              filetype=FileFormat.SCRIP,
                              reg_decomp=reg_decomp, decompflag=decompflag)
    except:
        raise NameError('grid_create_from_file_scrip_restlast_cyclic failed!')
    return True

@expected_failure
def grid_create_from_file_scrip_decomp_cyclic_balanced_test():
    reg_decomp = [pet_count(), 1]
    decompflag = np.array([DecompFlag.CYCLIC,DecompFlag.BALANCED],
                          dtype=np.int32)
    try:
        grid_from_file = Grid(filename="src/ESMF/test/data/T42_grid.nc",
                              filetype=FileFormat.SCRIP,
                              reg_decomp=reg_decomp, decompflag=decompflag)
    except:
        raise NameError('grid_create_from_file_scrip_cyclic_balanced failed!')
    return True

@expected_failure
def grid_create_from_file_scrip_decomp_cyclic_restfirst_test():
    reg_decomp = [pet_count(), 1]
    decompflag = np.array([DecompFlag.CYCLIC,DecompFlag.RESTFIRST],
                          dtype=np.int32)
    try:
        grid_from_file = Grid(filename="src/ESMF/test/data/T42_grid.nc",
                              filetype=FileFormat.SCRIP,
                              reg_decomp=reg_decomp, decompflag=decompflag)
    except:
        raise NameError('grid_create_from_file_scrip_cyclic_restfirst failed!')
    return True

@expected_failure
def grid_create_from_file_scrip_decomp_cyclic_restlast_test():
    reg_decomp = [pet_count(), 1]
    decompflag = np.array([DecompFlag.CYCLIC,DecompFlag.RESTLAST],
                          dtype=np.int32)
    try:
        grid_from_file = Grid(filename="src/ESMF/test/data/T42_grid.nc",
                              filetype=FileFormat.SCRIP,
                              reg_decomp=reg_decomp, decompflag=decompflag)
    except:
        raise NameError('grid_create_from_file_scrip_cyclic_restlast failed!')
    return True

@expected_failure
def grid_create_from_file_scrip_decomp_cyclic_cyclic_test():
    reg_decomp = [pet_count(), 1]
    decompflag = np.array([DecompFlag.CYCLIC,DecompFlag.CYCLIC],
                          dtype=np.int32)
    try:
        grid_from_file = Grid(filename="src/ESMF/test/data/T42_grid.nc",
                              filetype=FileFormat.SCRIP,
                              reg_decomp=reg_decomp, decompflag=decompflag)
    except:
        raise NameError('grid_create_from_file_scrip_cyclic_cyclic failed!')
    return True

def grid_field_test():

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

def grid_field_edge1_test():

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

def grid_field_edge2_test():

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

def grid_field_corner_test():

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

def grid_field_3D_test():

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

def grid_field_3D_edge1vcenter_test():

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

def grid_field_3D_edge2vcenter_test():

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

def grid_field_3D_cornervcenter_test():

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

def grid_field_3D_centervface_test():

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

def grid_field_3D_edge1vface_test():

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

def grid_field_3D_edge2vface_test():

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

def grid_field_3D_cornervface_test():

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

def mesh_test():
    status = True

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
    
    element_count = mesh.size[element]
    print 'local element_count = '+str(element_count)+'\n'

    node_count = mesh.size[node]
    print 'local node_count = '+str(node_count)+'\n'

    element_count = mesh.size_local[element]
    print 'owned element_count = '+str(element_count)+'\n'

    node_count = mesh.size_local[node]
    print 'owned node_count = '+str(node_count)+'\n'

    xcoords = mesh.get_coords(0)
    ycoords = mesh.get_coords(1)

    # use size here because nodeCoord has all nodes (owned and non-owned)
    xcoords2 = np.array([nodeCoord[2*i] for i in range(mesh.size[node])])
    ycoords2 = np.array([nodeCoord[2*i+1] for i in range(mesh.size[node])])

    # find only the owned coords to compare with what comes back from the mesh
    xcoords3 = xcoords2[np.where(nodeOwner==local_pet())]
    ycoords3 = ycoords2[np.where(nodeOwner==local_pet())]

    status = all(xcoords == xcoords3) and all(ycoords == ycoords3)

    # this call fails if nodes and elements have not been added first
    #mesh.free_memory()

    # return status from unit test (True if everything passed)
    return status

def meshvtk_test():
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

    #mesh._write("mesh")

    # return True from unit test
    return True

def mesh_create_from_file_scrip_test():
    try:
        mesh_from_file=Mesh(filename="src/ESMF/test/data/ne4np4-pentagons.nc",
                            filetype=FileFormat.SCRIP)
    except:
        raise NameError('mesh_create_from_file_scrip_test failed!')
    return True

def mesh_create_from_file_esmfmesh_test():
    try:
        mesh_from_file=Mesh(filename="src/ESMF/test/data/ne4np4-esmf.nc",
                            filetype=FileFormat.ESMFMESH)
    except:
        raise NameError('mesh_create_from_file_scrip_test failed!')
    return True

def interfaceint_test():
    Narray = np.array([4,5,6], dtype=np.int32)
    interfaceint = ESMP_InterfaceInt(Narray)

    # return True from unit test
    return True

@expected_failure
def interfaceint2_test():
    # This test should fail
    try:
        a = (ct.c_int*3)()
        a = [1,2,3]
        print a
        interfaceint2 = ESMP_InterfaceInt(a)
    except:
        raise TypeError('FAIL: tuples cannot be used in place of numpy.array')
    # return True from unit test
    return True

@expected_failure
def interfaceint3_test():
    # This test should fail
    try:
        interfaceint2 = ESMP_InterfaceInt(np.array([1,2,3]))
    except:
        raise TypeError('FAIL: tuples cannot be used in place of numpy.array')

    # return True from unit test
    return True

def field_r8_grid_test():
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

def field_r4_grid_test():
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

def field_i8_grid_test():
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

def field_i4_grid_test():
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

def field_r8_mesh_test():
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

def field_r4_mesh_test():
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

def field_i8_mesh_test():
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

def field_i4_mesh_test():
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

def field_uniqueness_test():
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

def field_switchedindices_grid_test():
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

def field_switchedindices_mesh_test():
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

    gridtofieldmap = np.array([1])

    field = Field(mesh, 'Field!',
                  TypeKind.R8,
                  MeshLoc.NODE)
    field2 = Field(mesh, 'Field!',
                   meshloc=MeshLoc.ELEMENT,
                   grid_to_field_map=np.array([1]),
                   ungridded_lower_bound=np.array([1]),
                   ungridded_upper_bound=np.array([5]))

    for i in range(field.shape[0]):
        field[i] = 10

    for i in range(field2.shape[0]):
        field2[i] = 10

    # return True from unit test
    return True

def field_extraindices_mesh_test():
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

    gridtofieldmap = np.array([1])

    field = Field(mesh, 'Field!',
                  TypeKind.R8,
                  MeshLoc.NODE)
    field2 = Field(mesh, 'Field!',
                   meshloc=MeshLoc.ELEMENT,
                   grid_to_field_map=np.array([1]),
                   ungridded_lower_bound=np.array([1]),
                   ungridded_upper_bound=np.array([5]))

    for i in range(field.shape[0]):
        field[i] = 10

    for i in range(field2.shape[0]):
        field2[i] = 10

    # return True from unit test
    return True

def field_regrid_test():
    # create grids
    max_index = np.array([20,20])
    srcgrid = Grid(max_index, coord_sys=CoordSys.CART)
    max_index = np.array([25,25])
    dstgrid = Grid(max_index, coord_sys=CoordSys.CART)

    # Add coordinates
    srcgrid.add_coords()
    dstgrid.add_coords()

    [x,y] = [0, 1]
    gridXCorner = srcgrid.get_coords(x)
    gridYCorner = srcgrid.get_coords(y)

    for i in xrange(gridXCorner.shape[x]):
        gridXCorner[i, :] = float(i)/6.

    for j in xrange(gridYCorner.shape[y]):
        gridYCorner[:, j] = float(j)/6.

    gridXCorner = dstgrid.get_coords(x)
    gridYCorner = dstgrid.get_coords(y)

    for i in xrange(gridXCorner.shape[x]):
        gridXCorner[i, :] = float(i)/4.

    for j in xrange(gridYCorner.shape[y]):
        gridYCorner[:, j] = float(j)/4.

    # create a Field on the Grid
    srcfield = Field(srcgrid, "GRIDFIELD!")
    srcfield[:, :] = 10.
    dstfield = Field(srcgrid, "GRIDFIELD!")
    dstfield[:, :] = 10.

    # regridding
    rh = Regrid(srcfield, dstfield, regrid_method=RegridMethod.BILINEAR)
    dstfield = rh(srcfield, dstfield)

    # test the __repr__ functions
    print srcgrid
    print dstgrid
    print repr(srcfield)
    print "%r" % dstfield
    print rh

    # return True from unit test
    return True

def field_regrid_gridmesh_test():
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
    dstfield = Field(mesh, 'MESHFIELD!', meshloc=MeshLoc.ELEMENT)

    # create grid
    max_index = np.array([16,16])
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

    # test the __repr__ functions
    print mesh
    print grid
    print repr(srcfield)
    print "%r" % dstfield
    print rh

    # return True from unit test
    return True

def field_regrid_zeroregion_test():
    correct = True

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

    # create a field on the mesh
    srcfield = Field(mesh, 'MESHFIELD!', meshloc=MeshLoc.ELEMENT)

    # initialize the source field
    for i in range(srcfield.shape[0]):
        srcfield[i] = 20.0

    # create grid
    grid = grid_create([0,0,8,8], [0,0,4,4], domask=True)

    [x,y] = [0, 1]

    # create a Field on the Grid
    dstfield = Field(grid, "GRIDFIELD!", mask_values=[0])

    # initialize the destination field according to the mask
    dstfield[:, :] = -100

    # regridding
    rh = Regrid(srcfield, dstfield, regrid_method=RegridMethod.CONSERVE,
                dst_mask_values=np.array([0]))
    dstfield = rh(srcfield, dstfield, zero_region=Region.SELECT)

    # validate that the masked values were not zeroed out
    for i in range(dstfield.mask.shape[x]):
        for j in range(dstfield.mask.shape[y]):
            if dstfield.mask[i, j] == True:
                if dstfield[i, j] >= 0:
                    print "DING: {0}".format(dstfield[i, j])
                    correct = False

    # return True from unit test
    return correct

def field_regrid_area_test():
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

    # create grid
    grid = grid_create([0,0,8,8], [0,0,4,4], doarea=True)

    [x,y] = [0, 1]

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
            if (srcarea[i, j] != 5):
                print "Cell area is {0}, but expected 5".format(srcarea[i,j])
                correct = False

    # subtract two because the last two cells of mesh are triangles with half area
    for i in range(dstarea.shape[0]):
        if (dstarea[i] != 0.25):
            if (dstarea[i] == 0.125):
                continue
            print "Cell area is {0}, but expected 0.25 or 0.125".format(dstarea[i])
            correct = False

    # return correct from unit test
    return correct

def scrip_inq_test():
    rank, dims = ESMP_ScripInq("src/ESMF/test/data/T42_grid.nc")
    correct = True
    if (rank != 2):
        print "rank = {0}, but expected 2".format(rank)
        correct = False
    if (dims != np.array([128,64])).all():
        print "dims = ",dims, " but expected [128,64]."
        correct = False
    return correct

def version_compare_test():
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

# The next two functions are not tested, they are just here for documentation

def grid_create_from_file(filename, filetype):
    '''
    PRECONDITIONS: filename contains the name of a grid file in an appropriate
                   format, and filetype is the corresponding type of the file.\n
    POSTCONDITIONS: A Grid has been created.\n
    RETURN VALUES: \n Grid :: grid \n
    '''
    grid = ESMF.Grid(filename=filename, filetype=filetype,
                     staggerloc=ESMF.StaggerLoc.CENTER, is_sphere=True)

    return grid

def mesh_create_from_file(filename, filetype, meshname):
    '''
    PRECONDITIONS: filename contains the name of a mesh file in an appropriate
                   format, and filetype is the corresponding type of the file.
                   The meshname cooresponds to the variable to use to define 
                   the mesh in a UGRID formatted file.\n
    POSTCONDITIONS: A Mesh has been created.\n
    RETURN VALUES: \n Mesh :: mesh \n
    '''
    mesh = ESMF.Mesh(filename=filename,
                     filetype=filetype,
                     meshname=meshname)

    return mesh

def create_field(grid_or_mesh, name):
    '''
    PRECONDITIONS: An Grid or Mesh has been created, and 'name' is a string that
                   will be used to initialize the name of a new Field.\n
    POSTCONDITIONS: A Field has been created.\n
    RETURN VALUES: \n Field :: field \n
    '''
    field = ESMF.Field(grid_or_mesh, name)

    return field

def run_regridding(srcfield, dstfield, srcfracfield, dstfracfield):
    '''
    PRECONDITIONS: Two Fields have been created and a regridding 
                   operation is desired from 'srcfield' to 'dstfield'.  
                   The 'src_mask_values' and 'dst_mask_values' arguments
                   specify the integer values to be considered masked.
                   The 'src_frac_field' and 'dst_frac_field' are Fields
                   created to hold the fractions of the source and 
                   destination fields which contribute to conservative 
                   regridding.\n
    POSTCONDITIONS: A regridding operation has set the data on 
                    'dstfield', 'srcfracfield', and 'dstfracfield'.\n
    RETURN VALUES: \n Field :: dstfield \n 
                      Field :: srcfracfield \n
                      Field :: dstfracfield \n
    '''
    # call the regridding functions
    regridSrc2Dst = ESMF.Regrid(srcfield, dstfield, \
                                regrid_method=ESMF.RegridMethod.CONSERVE, \
                                src_mask_values=np.array([0]), \
                                dst_mask_values=np.array([0]), \
                                unmapped_action=ESMF.UnmappedAction.ERROR, \
                                src_frac_field=srcfracfield, \
                                dst_frac_field=dstfracfield)
    dstfield = regridSrc2Dst(srcfield, dstfield)

    return dstfield, srcfracfield, dstfracfield

def main():

    # make an esmp object that won't go out of scope
    esmp = Manager(logkind=LogKind.MULTI, debug=True)

    try:
        vm = ESMP_VMGetGlobal()
    except:
        return

    # set up a dictionary with the names of the tests to be run
    dispatch = {
    (0,'version comparison') : version_compare_test,
    (1,'LogSet flush immediately') : log_test,
    (2,'VMGetGlobal, VMPrint, and VMGet') : vm_test,
    (3,'Mesh create and destroy') : mesh_test,
    (3.1,'Mesh create from file - SCRIP') : mesh_create_from_file_scrip_test,
    (3.2,'Mesh create from file - ESMFMESH') : mesh_create_from_file_esmfmesh_test,
    (4,'Grid create and destroy') : grid_create_test,
    (4.1,'Grid 3D create and destroy') : grid_create_3D_test,
    (4.2,'Grid coordinates') : grid_coords_test,
    (4.3,'Grid 3D coordinates') : grid_coords_3D_test,
    (4.4,'Grid masking') : grid_mask_test,
    (4.45,'Field masking') : field_mask_test,
    (4.5,'Grid 3D masking') : grid_mask_3D_test,
    (4.6,'Grid area') : grid_area_test,
    (4.7,'Grid 3D area') : grid_area_3D_test,
    (4.8,'Grid create from file - SCRIP') : grid_create_from_file_scrip_test,
    (4.81,'Grid create from file - SCRIP - decompflag=[BALANCED,BALANCED]') : grid_create_from_file_scrip_decomp_balanced_balanced_test,
    (4.811,'Grid create from file - SCRIP - decompflag=[BALANCED,RESTFIRST]') : grid_create_from_file_scrip_decomp_balanced_restfirst_test,
    (4.812,'Grid create from file - SCRIP - decompflag=[BALANCED,RESTLAST]') : grid_create_from_file_scrip_decomp_balanced_restlast_test,
    (4.813,'Grid create from file - SCRIP - decompflag=[BALANCED,CYCLIC]') : grid_create_from_file_scrip_decomp_balanced_cyclic_test,
    (4.814,'Grid create from file - SCRIP - decompflag=[RESTFIRST,BALANCED]') : grid_create_from_file_scrip_decomp_restfirst_balanced_test,
    (4.815,'Grid create from file - SCRIP - decompflag=[RESTFIRST,RESTFIRST]') : grid_create_from_file_scrip_decomp_restfirst_restfirst_test,
    (4.816,'Grid create from file - SCRIP - decompflag=[RESTFIRST,RESTLAST]') : grid_create_from_file_scrip_decomp_restfirst_restlast_test,
    (4.817,'Grid create from file - SCRIP - decompflag=[RESTFIRST,CYCLIC]') : grid_create_from_file_scrip_decomp_restfirst_cyclic_test,
    (4.818,'Grid create from file - SCRIP - decompflag=[RESTLAST,BALANCED]') : grid_create_from_file_scrip_decomp_restlast_balanced_test,
    (4.819,'Grid create from file - SCRIP - decompflag=[RESTLAST,RESTFIRST]') : grid_create_from_file_scrip_decomp_restlast_restfirst_test,
    (4.82,'Grid create from file - SCRIP - decompflag=[RESTLAST,RESTLAST]') : grid_create_from_file_scrip_decomp_restlast_restlast_test,
    (4.821,'Grid create from file - SCRIP - decompflag=[RESTLAST,CYCLIC]') : grid_create_from_file_scrip_decomp_restlast_cyclic_test,
    (4.822,'Grid create from file - SCRIP - decompflag=[CYCLIC,BALANCED]') : grid_create_from_file_scrip_decomp_cyclic_balanced_test,
    (4.823,'Grid create from file - SCRIP - decompflag=[CYCLIC,RESTFIRST]') : grid_create_from_file_scrip_decomp_cyclic_restfirst_test,
    (4.824,'Grid create from file - SCRIP - decompflag=[CYCLIC,RESTLAST]') : grid_create_from_file_scrip_decomp_cyclic_restlast_test,
    (4.825,'Grid create from file - SCRIP - decompflag=[CYCLIC,CYCLIC]') : grid_create_from_file_scrip_decomp_cyclic_cyclic_test,
    (5.0,'Grid field creation') : grid_field_test,
    (5.1,'Grid field creation edge1 stagger') : grid_field_edge1_test,
    (5.2,'Grid field creation edge2 stagger') : grid_field_edge2_test,
    (5.3,'Grid field creation corner stagger') : grid_field_corner_test,
    (6.0,'Grid 3D field creation') : grid_field_3D_test,
    (6.1,'Grid 3D field creation edge1_vcenter stagger') : grid_field_3D_edge1vcenter_test,
    (6.2,'Grid 3D field creation edge2_vcenter stagger') : grid_field_3D_edge2vcenter_test,
    (6.3,'Grid 3D field creation corner_vcenter stagger') : grid_field_3D_cornervcenter_test,
    (6.4,'Grid 3D field creation center_vface stagger') : grid_field_3D_centervface_test,
    (6.5,'Grid 3D field creation edge1_vface stagger') : grid_field_3D_edge1vface_test,
    (6.6,'Grid 3D field creation edge2_vface stagger') : grid_field_3D_edge2vface_test,
    (6.7,'Grid 3D field creation_corner_vface stagger') : grid_field_3D_cornervface_test,
    (7,'InterfaceInt') : interfaceint_test,
    (7.1,'InterfaceInt2') : interfaceint2_test,
    (7.2,'InterfaceInt3') : interfaceint3_test,
    (8,'Field create and destroy R8 from grid') : field_r8_grid_test,
    (8.1,'Field create and destroy R4 from grid') : field_r4_grid_test,
    (8.2,'Field create and destroy I8 from grid') : field_i8_grid_test,
    (8.3,'Field create and destroy I4 from grid') : field_i4_grid_test,
    (8.4,'Field create and destroy R8 from mesh') : field_r8_mesh_test,
    (8.5,'Field create and destroy R4 from mesh') : field_r4_mesh_test,
    (8.6,'Field create and destroy I8 from mesh') : field_i8_mesh_test,
    (8.7,'Field create and destroy I4 from mesh') : field_i4_mesh_test,
    (8.71,'Field uniqueness') : field_uniqueness_test,
    (8.8,'Field create and destroy with switched indices from grid') : field_switchedindices_grid_test,
    (8.9,'Field create and destroy with switched indices from mesh') : field_switchedindices_mesh_test,
    (12,'Field regridding') : field_regrid_test,
    (12.1,'Field regridding grid and mesh') : field_regrid_gridmesh_test,
    (12.2,'Field regridding with zeroregion') : field_regrid_zeroregion_test,
    (13.1,'Field regrid areas') : field_regrid_area_test,
    (14.1,'Retrieve rank and dimensions from SCRIP file') : scrip_inq_test}

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
