"""
Utilities for regridding with Grids
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

def grid_create(bounds, coords, domask=False, doarea=False):
    '''
    PRECONDITIONS: ESMPy has been initialized.
    POSTCONDITIONS: A Grid has been created.
    '''
    lb_x = float(bounds[0])
    lb_y = float(bounds[1])
    ub_x = float(bounds[2])
    ub_y = float(bounds[3])

    min_x = float(coords[0])
    min_y = float(coords[1])
    max_x = float(coords[2])
    max_y = float(coords[3])

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
    gridXCenter = grid.get_coords(x, staggerloc=ESMF.StaggerLoc.CENTER)
    gridYCenter = grid.get_coords(y, staggerloc=ESMF.StaggerLoc.CENTER)
    
    for i in xrange(gridXCenter.shape[x]):
        gridXCenter[i, :] = float(i)*cellwidth_x + lb_x + cellwidth_x/2.0    
 
    for j in xrange(gridYCenter.shape[y]):
        gridYCenter[:, j] = float(j)*cellwidth_y + lb_y + cellwidth_y/2.0

    if domask:
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
    
    if doarea:
        grid.add_item(ESMF.GridItem.AREA)
    
        area = grid.get_item(ESMF.GridItem.AREA)
    
        area[:] = 5.0

    return grid

def grid_create_periodic(bounds, domask=False):
    '''
    PRECONDITIONS: ESMPy has been initialized, 'bounds' contains the 
                   number of indices required for the first two 
                   dimensions of a Grid.  'domask' is a boolean value 
                   that gives the option to put a mask on this Grid.\n
    POSTCONDITIONS: An Grid has been created.\n
    RETURN VALUES: \n Grid :: grid \n
    '''

    nx = float(bounds[0])
    ny = float(bounds[1])

    dx = 360.0/nx
    dy = 180.0/ny

    DEG2RAD = 3.141592653589793/180.0

    max_index = np.array([nx,ny])

    staggerLocs = [ESMF.StaggerLoc.CORNER, ESMF.StaggerLoc.CENTER]
    grid = ESMF.Grid(max_index, num_peri_dims=1, staggerloc=staggerLocs)

    # VM
    vm = ESMF.ESMP_VMGetGlobal()
    localPet, petCount = ESMF.ESMP_VMGet(vm)

 # get the coordinate pointers and set the coordinates
    [x,y] = [0, 1]
    gridXCorner = grid.get_coords(x, staggerloc=ESMF.StaggerLoc.CORNER)
    gridYCorner = grid.get_coords(y, staggerloc=ESMF.StaggerLoc.CORNER)

    # make an array that holds indices from lower_bounds to upper_bounds
    bnd2indX = np.arange(grid.lower_bounds[ESMF.StaggerLoc.CORNER][x],
                         grid.upper_bounds[ESMF.StaggerLoc.CORNER][x], 1)
    bnd2indY = np.arange(grid.lower_bounds[ESMF.StaggerLoc.CORNER][y],
                         grid.upper_bounds[ESMF.StaggerLoc.CORNER][y], 1)

    for i in xrange(gridXCorner.shape[x]):
        gridXCorner[i, :] = float(bnd2indX[i])*dx - 180.0

    for j in xrange(gridYCorner.shape[y]):
        gridYCorner[:, j] = float(bnd2indY[j])*dy - 90.0

    ##     CENTERS

    # get the coordinate pointers and set the coordinates
    [x,y] = [0, 1]
    gridXCenter = grid.get_coords(x, staggerloc=ESMF.StaggerLoc.CENTER)
    gridYCenter = grid.get_coords(y, staggerloc=ESMF.StaggerLoc.CENTER)

    # make an array that holds indices from lower_bounds to upper_bounds
    bnd2indX = np.arange(grid.lower_bounds[ESMF.StaggerLoc.CENTER][x],
                         grid.upper_bounds[ESMF.StaggerLoc.CENTER][x], 1)
    bnd2indY = np.arange(grid.lower_bounds[ESMF.StaggerLoc.CENTER][y],
                         grid.upper_bounds[ESMF.StaggerLoc.CENTER][y], 1)

    for i in xrange(gridXCenter.shape[x]):
        gridXCenter[i, :] = float(bnd2indX[i])*dx + 0.5*dx - 180.0

    for j in xrange(gridYCenter.shape[y]):
        y = (float(bnd2indY[j])*dy - 90.0)
        yp1 = (float(bnd2indY[j]+1)*dy - 90.0)
        gridYCenter[:, j] = (y+yp1)/2.0

    '''
    # use mpi4py to collect values
    try:
        from mpi4py import MPI
    except:
        raise ImportError("mpi4py is not available, cannot compare \
                           global regridding error")

    comm = MPI.COMM_WORLD
    rank = comm.Get_rank()

    print "PROC: "+str(rank)
    print "grid bounds: "+str(gridXCoord.shape)
    print "and    bounds: "+str(exLB)+str(exUB)
    '''

    [x,y] = [0, 1]
    mask = 0
    if domask:
        # set up the grid mask
        mask = grid.add_item(ESMF.GridItem.MASK)

        maskregionX = [175.,185.]
        maskregionY = [-5.,5.]

        for i in range(mask.shape[x]):
            for j in range(mask.shape[y]):
                if (maskregionX[0] < gridXCenter[i,j] < maskregionX[1] and
                    maskregionY[0] < gridYCenter[i,j] < maskregionY[1]):
                    mask[i, j] = 1
                else:
                    mask[i, j] = 0

    return grid

def grid_create_3d(bounds, coords, domask=False, doarea=False):
    '''
    PRECONDITIONS: ESMPy has been initialized.
    POSTCONDITIONS: A Grid has been created.
    '''
    lb_x = float(bounds[0])
    lb_y = float(bounds[1])
    lb_z = float(bounds[2])
    ub_x = float(bounds[3])
    ub_y = float(bounds[4])
    ub_z = float(bounds[5])

    min_x = float(coords[0])
    min_y = float(coords[1])
    min_z = float(coords[2])
    max_x = float(coords[3])
    max_y = float(coords[4])
    max_z = float(coords[5])

    cellwidth_x = (max_x-min_x)/(ub_x-lb_x)
    cellwidth_y = (max_y-min_y)/(ub_y-lb_y)
    cellwidth_z = (max_z-min_z)/(ub_z-lb_z)
    
    cellcenter_x = cellwidth_x/2
    cellcenter_y = cellwidth_y/2
    cellcenter_z = cellwidth_z/2
    
    max_index = np.array([ub_x,ub_y,ub_z])

    grid = ESMF.Grid(max_index, coord_sys=ESMF.CoordSys.CART)

    ##     CORNERS
    grid.add_coords(staggerloc=[ESMF.StaggerLoc.CORNER_VFACE])

    # get the coordinate pointers and set the coordinates
    [x,y,z] = [0,1,2]
    gridCorner = grid.coords[ESMF.StaggerLoc.CORNER_VFACE]
    
    for i in xrange(gridCorner[x].shape[x]):
        gridCorner[x][i, :, :] = float(i)*cellwidth_x + lb_x    
 
    for j in xrange(gridCorner[y].shape[y]):
        gridCorner[y][:, j, :] = float(j)*cellwidth_y + lb_y

    for k in xrange(gridCorner[z].shape[z]):
        gridCorner[z][:, :, k] = float(k)*cellwidth_z + lb_z

    ##     CENTERS
    grid.add_coords(staggerloc=[ESMF.StaggerLoc.CENTER_VCENTER])

    # get the coordinate pointers and set the coordinates
    [x,y,z] = [0,1,2]
    gridXCenter = grid.get_coords(x, staggerloc=ESMF.StaggerLoc.CENTER_VCENTER)
    gridYCenter = grid.get_coords(y, staggerloc=ESMF.StaggerLoc.CENTER_VCENTER)
    gridZCenter = grid.get_coords(z, staggerloc=ESMF.StaggerLoc.CENTER_VCENTER)
    
    for i in xrange(gridXCenter.shape[x]):
        gridXCenter[i, :, :] = float(i)*cellwidth_x + lb_x + cellwidth_x/2.0    
 
    for j in xrange(gridYCenter.shape[y]):
        gridYCenter[:, j, :] = float(j)*cellwidth_y + lb_y + cellwidth_y/2.0

    for k in xrange(gridZCenter.shape[z]):
        gridYCenter[:, :, k] = float(k)*cellwidth_z + lb_z + cellwidth_z/2.0

    if domask:
        mask = grid.add_item(ESMF.GridItem.MASK)
    
        maskregionX = [(ub_x-lb_x)/2.-0.5, (ub_x-lb_x)/2.+0.5]
        maskregionY = [(ub_y-lb_y)/2.-0.5, (ub_y-lb_y)/2.+0.5]
        maskregionZ = [(ub_z-lb_z)/2.-0.5, (ub_z-lb_z)/2.+0.5]
    
        for i in range(mask.shape[x]):
            for j in range(mask.shape[y]):
                for k in range(mask.shape[z]):
                    if (maskregionX[0] < gridXCenter[i, j] < maskregionX[1] and
                            maskregionY[0] < gridYCenter[i, j] < maskregionY[1] and
                            maskregionZ[0] < gridZCenter[i, j] < maskregionZ[1]):
                        mask[i, j, k] = 1
                    else:
                        mask[i, j, k] = 0
    
    if doarea:
        grid.add_item(ESMF.GridItem.AREA)
    
        area = grid.get_item(ESMF.GridItem.AREA)
    
        area[:] = 5.0

    return grid

def initialize_field_grid(field, domask=False, doarea=False):
    '''
    PRECONDITIONS: A Field has been created.
    POSTCONDITIONS: The 'field' has been initialized to an analytic 
                    field.
    '''
    if domask:
        mask = field.grid.get_item(ESMF.GridItem.MASK)
    if doarea:
        area = field.grid.get_item(ESMF.GridItem.AREA)

    # get the coordinate pointers and set the coordinates
    [x,y] = [0,1]
    gridXCoord = field.grid.get_coords(x, ESMF.StaggerLoc.CENTER)
    gridYCoord = field.grid.get_coords(y, ESMF.StaggerLoc.CENTER)

    mass = 0
    for i in range(gridXCoord.shape[x]):
        for j in range(gridYCoord.shape[y]):
            field[i, j] = 20.0 + gridXCoord[i, j]**2 + \
                          gridXCoord[i, j]*gridYCoord[i, j]
            if domask:
                if mask[i, j] == 1:
                    field[i, j] = 10000000000000
            if doarea:
                mass = mass + area[i, j] * field[i, j]

    return field

def initialize_field_grid_periodic(field):
    '''
    PRECONDITIONS: An Field has been created as 'field'.  'grid' has
                   been created and coordinates have been set on both 
                   the center and corner stagger locations. \n
    POSTCONDITIONS: The 'field' has been initialized to an analytic 
                    field.\n
    RETURN VALUES: \n Field :: field \n
    '''
    import math
    DEG2RAD = 3.141592653589793/180.0

    # get the coordinate pointers and set the coordinates
    [x,y] = [0, 1]
    gridXCoord = field.grid.get_coords(x, ESMF.StaggerLoc.CENTER)
    gridYCoord = field.grid.get_coords(y, ESMF.StaggerLoc.CENTER)

    #for i in range(exLB[x], exUB[x]):
    #    for j in range(exLB[y], exUB[y]):
    for i in range(gridXCoord.shape[x]):
        for j in range(gridYCoord.shape[y]):
            theta = DEG2RAD*gridXCoord[i, j]
            phi = DEG2RAD*(90.0 - gridYCoord[i, j])
            field[i, j] = 2.0 + math.cos(theta)**2 * math.cos(2.0*phi)

    return field

def initialize_field_grid_3d(field, domask=False, doarea=False):
    '''
    PRECONDITIONS: A Field has been created.
    POSTCONDITIONS: The 'field' has been initialized to an analytic 
                    field.
    '''
    if domask:
        mask = field.grid.get_item(ESMF.GridItem.MASK)
    if doarea:
        area = field.grid.get_item(ESMF.GridItem.AREA)

    # get the coordinate pointers and set the coordinates
    [x,y,z] = [0,1,2]
    gridXCoord = field.grid.get_coords(x, ESMF.StaggerLoc.CENTER_VCENTER)
    gridYCoord = field.grid.get_coords(y, ESMF.StaggerLoc.CENTER_VCENTER)
    gridZCoord = field.grid.get_coords(z, ESMF.StaggerLoc.CENTER_VCENTER)

    mass = 0
    for i in range(gridXCoord.shape[x]):
        for j in range(gridYCoord.shape[y]):
            for k in range(gridZCoord.shape[z]):
                field[i, j, k] = 20.0 + gridXCoord[i, j, k] + \
                                 gridYCoord[i, j, k] + gridZCoord[i, j, k]
                if domask:
                    if mask[i, j, k] == 1:
                        field[i, j, k] = 10000000000000
                if doarea:
                    mass = mass + area[i, j, k] * field[i, j, k]

    return field

def compute_mass_grid(valuefield, areafield, dofrac=False, fracfield=None):
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

    for i in range(valuefield.shape[x]):
        for j in range(valuefield.shape[y]):
            if dofrac:
                mass += areafield[i, j] * valuefield[i, j] * \
                                fracfield[i, j]
            else:
                mass += areafield[i, j] * valuefield[i, j]

    return mass

def compute_mass_grid_3d(valuefield, areafield, dofrac=False, fracfield=None):
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
    [x, y, z] = [0, 1, 2]
    mass = 0.0
    areafield.get_area()

    for i in range(valuefield.shape[x]):
        for j in range(valuefield.shape[y]):
            for k in range(valuefield.shape[z]):
                if dofrac:
                    mass += areafield[i, j, k] * valuefield[i, j, k] * \
                                    fracfield[i, j, k]
                else:
                    mass += areafield[i, j, k] * valuefield[i, j, k]

    return mass

def compare_fields_grid(field1, field2, itrp_tol, csrv_tol, parallel=False, 
                        dstfracfield=None, mass1=None, mass2=None):
    '''
    PRECONDITIONS: Two Fields have been created and a comparison of the
                   the values is desired between 'field1' and 
                   'field2'.  The fields should be the same size on have
                   rank=2 or 3.
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

    totalErr = 0.0
    max_error = 0.0
    min_error = 1000000.0
    if field1.grid.rank == 2:
        totalErr, min_error, max_error = compare_fields_grid_2d(field1, field2, dstfracfield)
    elif field1.grid.rank == 3:
        totalErr, min_error, max_error = compare_fields_grid_3d(field1, field2, dstfracfield)
    else:
        raise ValueError("field1.grid.rank is not of a supported size")

    mass1_global = 0
    mass2_global = 0
    if parallel:
        # use mpi4py to collect values
        from mpi4py import MPI
        comm = MPI.COMM_WORLD
        total_error_global = comm.reduce(totalErr, op=MPI.SUM)
        max_error_global = comm.reduce(max_error, op=MPI.MAX)
        min_error_global = comm.reduce(min_error, op=MPI.MIN)
        if ((mass1 is not None) and (mass2 is not None)):
            mass1_global = comm.reduce(mass1, op=MPI.SUM)
            mass2_global = comm.reduce(mass2, op=MPI.SUM)
    else:
        total_error_global = totalErr
        max_error_global = max_error
        min_error_global = min_error
        if ((mass1 is not None) and (mass2 is not None)):
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

def compare_fields_grid_2d(field1, field2, dstfracfield):
    # initialize to True, and check for False point values
    [x, y] = [0, 1]
    totalErr = 0.0
    max_error = 0.0
    min_error = 1000000.0

    for i in range(field1.shape[x]):
        for j in range(field2.shape[y]):
            if (field2[i, j] != 0.0):
                err = abs(field1[i, j]/dstfracfield[i, j] - \
                            field2[i, j])/abs(field2[i, j])
            else:
                err = abs(field1[i, j]/dstfracfield[i, j] - \
                            field2[i, j])
            totalErr += err
            if (err > max_error):
                max_error = err
            if (err < min_error):
                min_error = err

    return totalErr, min_error, max_error

def compare_fields_grid_3d(field1, field2, dstfracfield):
    # initialize to True, and check for False point values
    [x, y, z] = [0, 1, 2]
    totalErr = 0.0
    max_error = 0.0
    min_error = 1000000.0

    for i in range(field1.shape[x]):
        for j in range(field2.shape[y]):
            for k in range(field2.shape[z]):
                if (field2[i, j, k] != 0.0):
                    err = abs(field1[i, j, k]/dstfracfield[i, j, k] - \
                                field2[i, j, k])/abs(field2[i, j, k])
                else:
                    err = abs(field1[i, j, k]/dstfracfield[i, j, k] - \
                                field2[i, j, k])
                totalErr += err
                if (err > max_error):
                    max_error = err
                if (err < min_error):
                    min_error = err

    return totalErr, min_error, max_error

