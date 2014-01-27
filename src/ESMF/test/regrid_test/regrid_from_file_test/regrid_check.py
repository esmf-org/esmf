# $Id$

"""
Two Field objects are created, one on a source Mesh and the other
on a destination Mesh.    The source Field is set to an analytic
function, and a regridding operation is performed from the source to
the destination Field.    After the regridding is completed, the
destination Field is compared to the exact solution over that domain.
"""

import sys

import ctypes

from getopt import getopt

try:
    import numpy as np
except:
    raise ImportError('The Numpy library cannot be found!')

try:
    import ESMF
except:
    raise ImportError('The ESMF library cannot be found!')

from regrid_from_file_consts import regrid_method_map, file_type_map, pole_method_map, UNINITVAL, EPSILON

def nc_is_mesh(filename, filetype):
    is_mesh = False
    if filetype == ESMF.FileFormat.UGRID:
        is_mesh = True
    elif filetype == ESMF.FileFormat.SCRIP:
        grid_rank = ESMF.ESMP_ScripInqRank(filename)
        print "%s rank is %d" % (filename, grid_rank)
        if grid_rank == 1:
            is_mesh = True
    return is_mesh
    

def create_grid_or_mesh_from_file(filename, filetype, meshname=None, convert_to_dual=None,
                                  isSphere=None):
    is_mesh = False
    if nc_is_mesh(filename, filetype):
        print "Creating ESMF.Mesh object"
        grid_or_mesh = ESMF.Mesh(filename=filename,
                         filetype=filetype,
                         meshname=meshname,
                         convert_to_dual=convert_to_dual)
        is_mesh = True
    else:
        print "Creating ESMF.Grid object"
        grid_dims = ESMF.ESMP_ScripInqDims(filename)
        print filename + " dims are ", grid_dims, "isSphere=", isSphere
        grid_or_mesh = ESMF.Grid(grid_dims, filename=filename, filetype=filetype,
                         staggerloc=ESMF.StaggerLoc.CENTER, isSphere=isSphere)
    return grid_or_mesh, is_mesh

def get_coords_from_grid_or_mesh(grid_or_mesh, is_mesh):
    if is_mesh:
        print "Getting coords from Mesh"
        coords_interleaved, num_nodes, num_dims = ESMF.ESMP_MeshGetCoordPtr(grid_or_mesh)
        coords = np.array([[coords_interleaved[2*i+j] 
                            for j in range(num_dims)] for i in range(num_nodes)])
    else:
        print "Getting coords from Grid"
        lons = grid_or_mesh.get_grid_coords_from_esmc(0, ESMF.StaggerLoc.CENTER)
        lats = grid_or_mesh.get_grid_coords_from_esmc(1, ESMF.StaggerLoc.CENTER)
        print 'get_grid_coords_from_esmc returned lons = ', lons
        print 'get_grid_coords_from_esmc returned lats = ', lats
        lats_flat = lats.ravel()
        lons_flat = lons.ravel()
        coords = np.array([[lons_flat[i],lats_flat[i]] for i in range(len(lats_flat))])
    return coords
        
def create_field(grid, name, regridmethod=None):
    '''
    PRECONDITIONS: A Mesh or Grid has been created, and 'name' is a string that
                   will be used to initialize the name of a new Field.
    POSTCONDITIONS: A Field has been created.
    '''
    if isinstance(grid,ESMF.Mesh):
        if regridmethod == ESMF.RegridMethod.CONSERVE:
            field = ESMF.Field(grid, name, meshloc=ESMF.MeshLoc.ELEMENT)
        else:
            field = ESMF.Field(grid, name, meshloc=ESMF.MeshLoc.NODE)
    else:
        field = ESMF.Field(grid, name)
    return field

def build_analyticfield_const(field):
    '''
    PRECONDITIONS: A Field has been created on the elements of a Mesh or Grid.
    POSTCONDITIONS: The 'field' has been initialized to a constant analytic field.
    '''
    # set the field to a constant value
    for i in range(field.shape[0]):    # this routine assumes this field is on elements
        field[i] = 1.

    return field

def build_analyticfield(field, coords):
    coords = np.reshape(coords, field.shape+(coords.shape[-1],), order='F')
    print 'coords=',coords[0:20]
    field.data[...] = 2.0 + np.cos(coords[...,1])**2 * np.cos(2.0*coords[...,0])
    print 'shape field data = ',field.data.shape
    print 'field.data = ',field.data
    return field

def run_regridding(srcfield, dstfield, regridmethod, unmappedaction,
                   dstFracField, polemethod=None, regridPoleNPnts=None):
    '''
    PRECONDITIONS: Two Fields have been created and a regridding operation
                   is desired from 'srcfield' to 'dstfield'.
    POSTCONDITIONS: A regridding operation has set the data on 'dstfield'.
    '''
    # call the regridding functions
    print 'polemethod = ',polemethod
    print 'regridPoleNPnts = ',regridPoleNPnts
    dstfield.data[...] = UNINITVAL
    regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                                regrid_method=regridmethod,
                                unmapped_action=unmappedaction,
                                dst_frac_field=dstFracField,
                                pole_method=polemethod,
                                regridPoleNPnts=regridPoleNPnts)
    dstfield = regridSrc2Dst(srcfield, dstfield, zero_region=ESMF.Region.SELECT)
    print 'unmappedaction = ',unmappedaction
    print 'dstfield=',dstfield

    return dstfield

def compare_fields(field1, field2, regridmethod, dstFracField, dst_mask, max_err,
                   parallel=False):
    '''
    PRECONDITIONS: Two Fields have been created and a comparison of the
                   the values is desired between 'srcfield' and 'dstfield'.
    POSTCONDITIONS: The values on 'srcfield' and 'dstfield' are compared.
    '''

    # compare point values of field1 to field2
    # first verify they are the same size
    if (field1.shape != field2.shape):
        raise NameError('compare_fields: Fields must be the same size!')

    # initialize to True, and check for False point values
    correct = False
    totalErr = 0.0
    print 'comparing fields'
    print 'field1 = ',field1
    print 'field2 = ',field2
    print 'field1.shape=',field1.shape
    print 'field1.size=',field1.size
    print 'dstFracField.shape=',dstFracField.shape
    print 'dstFracField.size=',dstFracField.size
    print 'dst_mask.shape=',dst_mask.shape
    print 'dst_mask.size=',dst_mask.size
    field1data = np.ravel(field1.data)
    field2data = np.ravel(field2.data)
    dstFracFieldData = np.ravel(dstFracField.data)
    dst_mask_flat = np.ravel(dst_mask)
    for i in range(field1.size):
        #print "i=",i
        #print "field1 %f, field2 %f" % (field1data[i], field2data[i])
        #print "dstFracField %f" % dstFracFieldData[i]
        #print "dst_mask %d" % dst_mask_flat[i]
        if ((field1data[i] != UNINITVAL) and 
            (abs(field2data[i]) > EPSILON) and
            (dst_mask_flat[i] == 1) and 
            (regridmethod != ESMF.RegridMethod.CONSERVE or
            dstFracFieldData[i] >= 0.999)):
            err = abs(field1data[i] - field2data[i])/abs(field2data[i])
            totalErr += err
        #else:
            #print "Partial dest fraction -- skipping"

    relErr = totalErr/field2.size
    if (relErr < max_err):
        correct = True

    # this is for parallel
    if parallel:
        # use mpi4py to collect values
        from mpi4py import MPI

        comm = MPI.COMM_WORLD
        rank = comm.Get_rank()

        rel_error_global = comm.reduce(relErr, op=MPI.SUM)

        if rank == 0:

            if correct:
                print " - PASS - Total error = "+str(rel_error_global)
            else:
                print " - FAIL - Total error = "+str(rel_error_global)

    # this is for serial
    else:
        if correct:
            print " - PASS - Total Error = "+str(relErr)
        else:
            print " - FAIL - Total Error = "+str(relErr)

    return correct

def parse_options(options):
        options = options.split()
        opts, args = getopt(options,'it:p:r', ['src_type=', 'dst_type=', 
                                               'src_meshname=', 'dst_meshname=',
                                               'ignore_unmapped', 'src_regional', 'dst_regional'])
        src_type_str = "SCRIP"
        dst_type_str = "SCRIP"
        src_meshname = "Undefined"
        dst_meshname = "Undefined"
        pole_method_str = None
        unmapped_action = ESMF.UnmappedAction.ERROR
        src_regional = False
        dst_regional = False
        for opt, arg in opts:
            if opt == '--src_type':
                src_type_str = arg
            elif opt == '--dst_type':
                dst_type_str = arg
            elif opt == '--src_meshname':
                src_meshname = arg
            elif opt == '--dst_meshname':
                dst_meshname = arg
            elif opt == '-i' or opt == '--ignore_unmapped':
                unmapped_action = ESMF.UnmappedAction.IGNORE
            elif opt == '-t':
                src_type_str = arg
                dst_type_str = arg
            elif opt == '-p':
                pole_method_str = arg
            elif opt == '-r':
                src_regional = True
                dst_regional = True
            elif opt == '--src_regional':
                src_regional = True
            elif opt == '--dst_regional':
                dst_regional = True
        return (src_type_str, dst_type_str, src_meshname, dst_meshname,
                unmapped_action, pole_method_str, src_regional, dst_regional)

def regrid_check(src_fname, dst_fname, regrid_method, options, max_err):

#    print "\nregrid_weight_gen_check.py: mesh_check()"

    parallel = False
#    if petCount > 1:
#        if petCount != 4:
#            raise NameError('PET count must be 4 in parallel mode!')
#        parallel = True
#
#    if localPet == 0:
#        print "\nmesh_test"

    # Settings for regrid
    (src_type_str, dst_type_str, src_meshname, dst_meshname,
     unmappedaction, pole_method_str, src_regional, dst_regional) = parse_options(options)
    src_type = file_type_map[src_type_str]
    dst_type = file_type_map[dst_type_str]
    regridmethod = regrid_method_map[regrid_method]
    convert_to_dual = (regridmethod != ESMF.RegridMethod.CONSERVE)
    src_is_sphere = not src_regional
    dst_is_sphere = not dst_regional
    pole_method = None
    pole_method_npntavg = 1
    if pole_method_str:
        if pole_method_str in pole_method_map:
            pole_method = pole_method_map[pole_method_str]
        else:
            pole_method = ESMF.PoleMethod.NPNTAVG
            pole_method_npntavg = int(pole_method_str)
            
    srcgrid, src_is_mesh = create_grid_or_mesh_from_file(src_fname, src_type, meshname=src_meshname,
                                                         convert_to_dual=convert_to_dual, 
                                                         isSphere=src_is_sphere)
    dstgrid, dst_is_mesh = create_grid_or_mesh_from_file(dst_fname, dst_type, meshname=dst_meshname,
                                                         convert_to_dual=convert_to_dual, 
                                                         isSphere=dst_is_sphere)

    # Get node coordinates
    src_coords = get_coords_from_grid_or_mesh(srcgrid, src_is_mesh)
    print 'src_coords.shape=',src_coords.shape
    print 'src_coords = ',src_coords
    src_coords = np.radians(src_coords)
    #for i in range(len(src_coords)):
    #    print src_coords[i]
    dst_coords = get_coords_from_grid_or_mesh(dstgrid, dst_is_mesh)
    print 'dst_coords = ',dst_coords
    dst_coords = np.radians(dst_coords)
    print 'dst_coords.shape=',dst_coords.shape

    # get the destination mask
    if dst_is_mesh:
        dst_mask = np.copy(dst_coords)
        dst_mask[...] = 1
    else:
        #dst_mask = dstgrid.get_grid_mask_from_esmc(ESMF.StaggerLoc.CENTER)
        dstgrid.link_item_buffer(ESMF.GridItem.MASK, ESMF.StaggerLoc.CENTER)
        dst_mask = dstgrid.get_item(ESMF.GridItem.MASK, staggerloc=ESMF.StaggerLoc.CENTER)
    print 'dst_mask.shape = ',dst_mask.shape
    print 'dst_mask = ', dst_mask

    # create Field objects on the Grids
    srcfield = create_field(srcgrid, 'srcfield', regridmethod)
    dstfield = create_field(dstgrid, 'dstfield', regridmethod)
    dstfield2 = create_field(dstgrid, 'dstfield_exact', regridmethod)
    dstFracField = create_field(dstgrid, 'dstFracField', regridmethod)

    # initialize the Fields to an analytic function
    srcfield = build_analyticfield(srcfield, src_coords)
    dstfield2 = build_analyticfield(dstfield2, dst_coords)

    # run the ESMF regridding
    dstfield = run_regridding(srcfield, dstfield, regridmethod, unmappedaction, dstFracField,
                              polemethod=pole_method, regridPoleNPnts=pole_method_npntavg)

    # compare results and output PASS or FAIL
    correct = compare_fields(dstfield, dstfield2, regridmethod, dstFracField, dst_mask,
                             max_err, parallel)
    return correct

if __name__ == '__main__':
    #src_fname = "FVCOM_grid2d.nc"
    #dst_fname = "selfe_grid2d.nc"
    #regrid_method = "bilinear"
    #options = "-t UGRID --src_meshname fvcom_mesh --dst_meshname selfe_mesh -i"
    #max_err = 0.06
    #src_fname = "FVCOM_grid2d.nc"
    #dst_fname = "selfe_grid2d.nc"
    #regrid_method = "conserve"
    #options = "-i --src_type UGRID --dst_type UGRID --src_meshname fvcom_mesh --dst_meshname selfe_mesh"
    #max_err = 0.06
    src_fname = "T42_grid.nc"
    dst_fname = "ll2.5deg_grid.nc"
    regrid_method = "bilinear"
    #regrid_method = "conserve"
    options = "-p none -i"
    #options = "-p 4"
    max_err = 10E-03
    #src_fname = "T42_grid.nc"
    #dst_fname = "ar9v4_100920.nc"
    #regrid_method = "patch"
    #options = "--dst_regional"
    #max_err = 10E-04
    #src_fname = "ne60np4_pentagons_100408.nc"
    #dst_fname = "ne30np4-t2.nc"
    #regrid_method = "bilinear"
    #options = "-i"
    #max_err = 10E-04
    #src_fname = "wr50a_090614.nc"
    #dst_fname = "ar9v4_100920.nc"
    #regrid_method = "bilinear"
    #options = "-r -i"
    #max_err = 10E-06
    #src_fname = "T42_grid.nc"
    #dst_fname = "wr50a_090614.nc"
    #regrid_method = "bilinear"
    #options = "--dst_regional"
    #max_err = 10E-04
    sys.exit(regrid_check(src_fname, dst_fname, regrid_method, options, max_err))

