# $Id$

"""
Two Field objects are created on 3d Grids.    The
source Field is set to an analytic function, and a conservative 
regridding operation is performed from the source to the destination 
grid with masking.  After the regridding is completed, the destination 
Field is compared to the exact solution over that domain.
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

def grid_create(bounds):
    '''
    PRECONDITIONS: ESMPy has been initialized, bounds hold 6 elements.
    POSTCONDITIONS: A Grid has been created.
    '''

    lb_x = float(bounds[0])
    lb_y = float(bounds[1])
    lb_z = float(bounds[2])
    ub_x = float(bounds[3])
    ub_y = float(bounds[4])
    ub_z = float(bounds[5])

    cellwidth_x = 1.0
    cellwidth_y = 1.0
    cellwidth_z = 1.0
    cellcenter_x = cellwidth_x/2.0
    cellcenter_y = cellwidth_y/2.0
    cellcenter_z = cellwidth_z/2.0

    max_index = np.array([ub_x,ub_y,ub_z])

    grid = ESMF.Grid(max_index, coord_sys=ESMF.CoordSys.CART)

    # VM
    vm = ESMF.ESMP_VMGetGlobal()
    localPet, petCount = ESMF.ESMP_VMGet(vm)

    ##     CORNERS
    grid.add_coords(staggerloc=[ESMF.StaggerLoc.CORNER_VFACE])

    # get the coordinate pointers and set the coordinates
    [x,y,z] = [0,1,2]
    gridXCorner = grid.get_coords(x, staggerloc=ESMF.StaggerLoc.CORNER_VFACE)
    gridYCorner = grid.get_coords(y, staggerloc=ESMF.StaggerLoc.CORNER_VFACE)
    gridZCorner = grid.get_coords(z, staggerloc=ESMF.StaggerLoc.CORNER_VFACE)

    # make an array that holds indices from lower_bounds to upper_bounds
    bnd2indX = np.arange(grid.lower_bounds[ESMF.StaggerLoc.CORNER_VFACE][x],
                         grid.upper_bounds[ESMF.StaggerLoc.CORNER_VFACE][x], 1)
    bnd2indY = np.arange(grid.lower_bounds[ESMF.StaggerLoc.CORNER_VFACE][y],
                         grid.upper_bounds[ESMF.StaggerLoc.CORNER_VFACE][y], 1)
    bnd2indZ = np.arange(grid.lower_bounds[ESMF.StaggerLoc.CORNER_VFACE][z],
                         grid.upper_bounds[ESMF.StaggerLoc.CORNER_VFACE][z], 1)

    for i in xrange(gridXCorner.shape[x]):
        gridXCorner[i, :, :] = float(bnd2indX[i])*cellwidth_x + lb_x

    for j in xrange(gridYCorner.shape[y]):
        gridYCorner[:, j, :] = float(bnd2indY[j])*cellwidth_y + lb_y

    for k in xrange(gridZCorner.shape[z]):
        gridZCorner[:, :, k] = float(bnd2indZ[k])*cellwidth_z + lb_z

    ##     CENTERS
    grid.add_coords(staggerloc=[ESMF.StaggerLoc.CENTER_VCENTER])

    # get the coordinate pointers and set the coordinates
    [x,y,z] = [0,1,2]
    gridXCenter = grid.get_coords(x, staggerloc=ESMF.StaggerLoc.CENTER_VCENTER)
    gridYCenter = grid.get_coords(y, staggerloc=ESMF.StaggerLoc.CENTER_VCENTER)
    gridZCenter = grid.get_coords(z, staggerloc=ESMF.StaggerLoc.CENTER_VCENTER)

    # make an array that holds indices from lower_bounds to upper_bounds
    bnd2indX = np.arange(grid.lower_bounds[ESMF.StaggerLoc.CENTER_VCENTER][x],
                         grid.upper_bounds[ESMF.StaggerLoc.CENTER_VCENTER][x], 
                         1)
    bnd2indY = np.arange(grid.lower_bounds[ESMF.StaggerLoc.CENTER_VCENTER][y],
                         grid.upper_bounds[ESMF.StaggerLoc.CENTER_VCENTER][y], 
                         1)
    bnd2indZ = np.arange(grid.lower_bounds[ESMF.StaggerLoc.CENTER_VCENTER][z],
                         grid.upper_bounds[ESMF.StaggerLoc.CENTER_VCENTER][z], 
                         1)

    for i in xrange(gridXCenter.shape[x]):
        gridXCenter[i, :, :] = float(bnd2indX[i])*cellwidth_x + \
                                     lb_x + cellwidth_x/2.0

    for j in xrange(gridYCenter.shape[y]):
        gridYCenter[:, j, :] = float(bnd2indY[j])*cellwidth_y + lb_y + \
                                     cellwidth_y/2.0

    for k in xrange(gridZCenter.shape[z]):
        gridZCenter[:, :, k] = float(bnd2indZ[k])*cellwidth_z + lb_z + \
                                     cellwidth_z/2.0

    # set up the grid mask
    mask = grid.add_item(ESMF.GridItem.MASK)

    maskregionX = [(ub_x-lb_x)/2.-0.5, (ub_x-lb_x)/2.+0.5]
    maskregionY = [(ub_y-lb_y)/2.-0.5, (ub_y-lb_y)/2.+0.5]
    maskregionZ = [(ub_z-lb_z)/2.-0.5, (ub_z-lb_z)/2.+0.5]

    for i in range(mask.shape[x]):
        for j in range(mask.shape[y]):
            for k in range(mask.shape[z]):
                if (maskregionX[0] < gridXCenter[i, j, k] < maskregionX[1] and
                    maskregionY[0] < gridYCenter[i, j, k] < maskregionY[1] and
                    maskregionZ[0] < gridZCenter[i, j, k] < maskregionZ[1]):
                    mask[i, j, k] = 1
                else:
                    mask[i, j, k] = 0

    return grid

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
    [x,y,z] = [0,1,2]
    gridXCoord = grid.get_coords(x, ESMF.StaggerLoc.CENTER_VCENTER)
    gridYCoord = grid.get_coords(y, ESMF.StaggerLoc.CENTER_VCENTER)
    gridZCoord = grid.get_coords(z, ESMF.StaggerLoc.CENTER_VCENTER)

    for i in range(gridXCoord.shape[x]):
        for j in range(gridYCoord.shape[y]):
            for k in range(gridZCoord.shape[z]):
                field[i, j, k] = 20.0 + gridXCoord[i, j, k]**2 + \
                                 gridXCoord[i, j, k]*gridYCoord[i, j, k]+\
                                 gridZCoord[i, j, k]**2

    #fieldPtr[:] = 20.0+gridXCoord.flat[:]**2+gridXCoord.flat[:]*\
    #                            gridYCoord.flat[:]+gridZCoord.flat[:]**2
    #fieldPtr [ [i for i in range(len(mask.flat)) 
    #            if mask.flat[i] == 1] ] = 10000000000000

    return field

def run_regridding(srcfield, dstfield):
    '''
    PRECONDITIONS: Two Fields have been created and a regridding 
                   operation is desired from 'srcfield' to 'dstfield'.
    POSTCONDITIONS: A regridding operation has set the data on 
                   'dstfield'.
    '''
    # call the regridding functions
    regridSrc2Dst = ESMF.Regrid(srcfield, dstfield, \
                                src_mask_values=np.array([1]), \
                                dst_mask_values=np.array([1]), \
                                regrid_method=ESMF.RegridMethod.CONSERVE, \
                                unmapped_action=ESMF.UnmappedAction.ERROR)
    dstfield = regridSrc2Dst(srcfield, dstfield)

    return dstfield

def compare_fields(field1, field2, parallel):
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
    [x, y, z] = [0, 1, 2]
    correct = True
    totalErr = 0.0
    for i in range(field1.shape[x]):
        for j in range(field1.shape[y]):
            for k in range(field1.shape[z]):
                err = abs(field1[i, j, k] - field2[i, j, k])/ \
                            abs(field2[i, j, k])
                # if masking is set, the error should be larger for the 
                # masked out cells but not larger than one.  if the 
                # masking doesn't work the error will be much larger 
                # than 1 because of the large analytic field that is set
                if err > 1:
                    correct = False
                    print "ACCURACY ERROR - "+str(err)
                    print "field1 = {0} : field2 = {1}\n".format(\
                          field1[i, j, k], field2[i, j, k])
                totalErr += err

    # this is for parallel
    if parallel:
        # use mpi4py to collect values
        from mpi4py import MPI

        comm = MPI.COMM_WORLD
        rank = comm.Get_rank()

        total_error_global = comm.reduce(totalErr, op=MPI.SUM)

        if rank == 0:
            if (correct):
                print " - PASS -    Total error = "+str(total_error_global)+"\n"
            else:
                print " - FAIL -    Total error = "+str(total_error_global)+"\n"

    # this is for serial
    else:
        if correct:
            print " - PASS - Total Error = "+str(totalErr)+"\n"
        else:
            print " - FAIL - Total Error = "+str(totalErr)+"\n"

    return

def test_main():
    # start up ESMF
    # this call is not necessary unless you want to to override the
    # default options:
    #  LogKind = NONE
    #  debug = False
    #manager = ESMF.Manager(logkind=ESMF.LogKind.SINGLE, debug=True)

    # inquire for rank and proc from ESMF Virtual Machine
    localPet = ESMF.get_localPet()
    petCount = ESMF.get_petCount()

    parallel = False
    if petCount > 1:
        if petCount > 4:
            raise NameError('MPI rank must be <=4 in parallel mode!')
        parallel = True

    # opening remarks
    if localPet == 0:
        print "\ngrid_grid_regrid_csrv_mask_3D"

    # create two unique Grid objects
    srcgrid = grid_create([0,0,0,21,21,21])
    dstgrid = grid_create([0.5,0.5,0.5,19.5,19.5,19.5])

    srcgrid.write("srcgrid3dCart")
    dstgrid.write("dstgrid3dCart")

    # create Field objects on the Meshes
    srcfield = create_fieldgrid(srcgrid, 'srcfield')
    dstfield = create_fieldgrid(dstgrid, 'dstfield')
    dstfield2 = create_fieldgrid(dstgrid, 'dstfield_exact')

    # initialize the Fields to an analytic function
    srcfield = build_analyticfieldgrid(srcfield, srcgrid)
    dstfield2 = build_analyticfieldgrid(dstfield2, dstgrid)

    # run the ESMF regridding
    dstfield = run_regridding(srcfield, dstfield)

    # compare results and output PASS or FAIL
    compare_fields(dstfield, dstfield2, parallel)

    return 0

if __name__ == '__main__':
    sys.exit(test_main())
