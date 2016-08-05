# $Id$

"""
Two Field objects are created, one on a source Mesh and the other
on a destination Mesh.    The source Field is set to an analytic
function, and a regridding operation is performed from the source to
the destination Field.    After the regridding is completed, the
destination Field is compared to the exact solution over that domain.
"""

from getopt import getopt

try:
    import numpy as np
except:
    raise ImportError('The Numpy library cannot be found!')

try:
    import ESMF
except:
    raise ImportError('The ESMF library cannot be found!')

from ESMF.test.regrid_from_file.regrid_from_file_consts import regrid_method_map, file_type_map, pole_method_map, UNINITVAL
from ESMF.util.grid_utilities import compute_mass_grid
from ESMF.util.mesh_utilities import compute_mass_mesh
from ESMF.util.field_utilities import compare_fields

def nc_is_mesh(filename, filetype):
    is_mesh = False
    if (filetype == ESMF.FileFormat.UGRID) or (filetype == ESMF.FileFormat.ESMFMESH):
        is_mesh = True
    elif filetype == ESMF.FileFormat.SCRIP:
        grid_rank, dims = ESMF.ESMP_ScripInq(filename)
        if grid_rank == 1:
            is_mesh = True
    return is_mesh
    

def create_grid_or_mesh_from_file(filename, filetype, meshname=None, 
                                  convert_to_dual=None,
                                  isSphere=None, add_corner_stagger=False, 
                                  missingvalue=None):
    is_mesh = False
    if nc_is_mesh(filename, filetype):
        grid_or_mesh = ESMF.Mesh(filename=filename,
                         filetype=filetype,
                         meshname=meshname,
                         convert_to_dual=convert_to_dual)
        is_mesh = True
        add_mask = False
    else:
        add_mask = (missingvalue is not None) and (len(missingvalue) > 0)
        grid_or_mesh = ESMF.Grid(filename=filename, filetype=filetype, 
                                 add_corner_stagger=add_corner_stagger,
                                 is_sphere=isSphere, add_mask=add_mask, 
                                 varname=missingvalue)
    return grid_or_mesh, is_mesh, add_mask

def get_coords_from_grid_or_mesh(grid_or_mesh, is_mesh, regrid_method):
    if is_mesh:
        # Mesh
        if regrid_method == ESMF.RegridMethod.CONSERVE:
            lons = grid_or_mesh.get_coords(0, meshloc=ESMF.element)
            lats = grid_or_mesh.get_coords(1, meshloc=ESMF.element)
        else:
            lons = grid_or_mesh.get_coords(0, meshloc=ESMF.node)
            lats = grid_or_mesh.get_coords(1, meshloc=ESMF.node)
    else:
        # Grid
        lons = grid_or_mesh.get_coords(0, staggerloc=ESMF.StaggerLoc.CENTER)
        lats = grid_or_mesh.get_coords(1, staggerloc=ESMF.StaggerLoc.CENTER)
    # Convert to radians
    lons = np.radians(lons)
    lats = np.radians(lats)
    return lons,lats
        
def create_field(grid, name, regrid_method=None):
    '''
    PRECONDITIONS: A Mesh or Grid has been created, and 'name' is a string that
                   will be used to initialize the name of a new Field.
    POSTCONDITIONS: A Field has been created.
    '''
    if isinstance(grid,ESMF.Mesh):
        if regrid_method == ESMF.RegridMethod.CONSERVE:
            field = ESMF.Field(grid, name=name, meshloc=ESMF.MeshLoc.ELEMENT)
        else:
            field = ESMF.Field(grid, name=name, meshloc=ESMF.MeshLoc.NODE)
    else:
        field = ESMF.Field(grid, name=name)

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

def build_analyticfield(field, lons, lats):
    field.data[...] = 2.0 + np.cos(lats[...])**2 * np.cos(2.0*lons[...])
    return field

def run_regridding(srcfield, dstfield, src_mask, dst_mask, 
                   regrid_method, unmapped_action, srcfracfield, 
                   dstfracfield, pole_method=None, regrid_pole_npoints=None):
    '''
    PRECONDITIONS: Two Fields have been created and a regridding operation
                   is desired from 'srcfield' to 'dstfield'.
    POSTCONDITIONS: A regridding operation has set the data on 'dstfield'.
    '''
    # call the regridding functions
    src_mask_vals = None
    dst_mask_vals = None
    if src_mask:
        src_mask_vals = np.array([0])
    if dst_mask:
        dst_mask_vals = np.array([0])

    regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                                src_mask_values=src_mask_vals,
                                dst_mask_values=dst_mask_vals,
                                regrid_method=regrid_method,
                                unmapped_action=unmapped_action,
                                src_frac_field=srcfracfield,
                                dst_frac_field=dstfracfield,
                                pole_method=pole_method,
                                regrid_pole_npoints=regrid_pole_npoints)
    dstfield = regridSrc2Dst(srcfield, dstfield, zero_region=ESMF.Region.SELECT)
    regridSrc2Dst.destroy()

    return dstfield

def parse_options(options):
        options = options.split()
        opts, args = getopt(options,'it:p:r', ['src_type=', 'dst_type=', 
                                               'src_meshname=', 'dst_meshname=',
                                               'ignore_unmapped', 
                                               'src_regional', 'dst_regional',
                                               'src_missingvalue=','dst_missingvalue=',
					       '64bit_offset','netcdf4'])
        src_type_str = "SCRIP"
        dst_type_str = "SCRIP"
        src_meshname = "Undefined"
        dst_meshname = "Undefined"
        pole_method_str = None
        unmapped_action = ESMF.UnmappedAction.ERROR
        src_regional = False
        dst_regional = False
        src_missingvalue = ""
        dst_missingvalue = ""
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
            elif opt == '--src_missingvalue':
                src_missingvalue = arg
            elif opt == '--dst_missingvalue':
                dst_missingvalue = arg
        return (src_type_str, dst_type_str, src_meshname, dst_meshname,
                unmapped_action, pole_method_str, src_regional, dst_regional,
                src_missingvalue, dst_missingvalue)

def regrid_check(src_fname, dst_fname, regrid_method, options, 
                 itrp_mean_err, itrp_max_err, csrv_err):

#    print ("\nregrid_weight_gen_check.py: mesh_check()")

    parallel = False
    if ESMF.pet_count() > 1:
        parallel = True

    # Settings for regrid
    (src_type_str, dst_type_str, src_meshname, dst_meshname,
     unmapped_action, pole_method_str, src_regional, dst_regional,
     src_missingvalue, dst_missingvalue) = parse_options(options)
    src_type = file_type_map[src_type_str]
    dst_type = file_type_map[dst_type_str]
    regrid_method = regrid_method_map[regrid_method]
    convert_to_dual = (regrid_method != ESMF.RegridMethod.CONSERVE)
    add_corner_stagger = (regrid_method == ESMF.RegridMethod.CONSERVE)
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

    src_mask = False
    srcgrid, src_is_mesh, src_mask = create_grid_or_mesh_from_file(src_fname, src_type,
                                                    meshname=src_meshname,
                                                    convert_to_dual=convert_to_dual, 
                                                    isSphere=src_is_sphere,
                                                    add_corner_stagger=add_corner_stagger,
                                                    missingvalue=src_missingvalue)
    dst_mask = False
    dstgrid, dst_is_mesh, dst_mask = create_grid_or_mesh_from_file(dst_fname, dst_type, 
                                                    meshname=dst_meshname,
                                                    convert_to_dual=convert_to_dual, 
                                                    isSphere=dst_is_sphere,
                                                    add_corner_stagger=add_corner_stagger,
                                                    missingvalue=dst_missingvalue)

    # Get coordinates in radians.
    src_lons, src_lats = get_coords_from_grid_or_mesh(srcgrid, src_is_mesh, 
                                                      regrid_method)
    dst_lons, dst_lats = get_coords_from_grid_or_mesh(dstgrid, dst_is_mesh, 
                                                      regrid_method)
    
    # create Field objects on the Grids
    srcfield = create_field(srcgrid, 'srcfield', regrid_method=regrid_method)
    dstfield = create_field(dstgrid, 'dstfield', regrid_method=regrid_method)
    dstfield2 = create_field(dstgrid, 'dstfield_exact', regrid_method=regrid_method)

    #create the frac fields
    srcfracfield = create_field(srcgrid, 'src_frac_field', 
                            regrid_method=regrid_method)
    dstfracfield = create_field(dstgrid, 'dst_frac_field', 
                            regrid_method=regrid_method)

    # initialize the Fields to an analytic function
    srcfield = build_analyticfield(srcfield, src_lons, src_lats)
    dstfield2 = build_analyticfield(dstfield2, dst_lons, dst_lats)

    # run the ESMF regridding
    dstfield.data[...] = UNINITVAL
    dstfield = run_regridding(srcfield, dstfield, src_mask, dst_mask,
                              regrid_method, unmapped_action, 
                              srcfracfield, dstfracfield,
                              pole_method=pole_method, 
                              regrid_pole_npoints=pole_method_npntavg)

    srcmass = None
    dstmass = None
    if regrid_method == ESMF.RegridMethod.CONSERVE:
        if src_is_mesh:
            srcmass = compute_mass_mesh(srcfield, dofrac=True, 
                                        fracfield=srcfracfield)
        else:
            srcmass = compute_mass_grid(srcfield, dofrac=True, 
                                        fracfield=srcfracfield)
        if dst_is_mesh:
            dstmass = compute_mass_mesh(dstfield, uninitval=UNINITVAL)
        else:
            dstmass = compute_mass_grid(dstfield, uninitval=UNINITVAL)

    else:
        srcfracfield.destroy()
        dstfracfield.destroy()
        srcfracfield = None
        dstfracfield = None
        
    total_error,csrv_error,correct=compare_fields(dstfield, dstfield2, 
                                                  itrp_mean_err, 
                                                  itrp_max_err, 
                                                  csrv_err, 
                                                  dstfracfield=dstfracfield,
                                                  mass1=srcmass, mass2=dstmass, 
                                                  regrid_method=regrid_method, 
                                                  uninitval=UNINITVAL)
    
    # Destroy ESMF objects
    srcfield.destroy()
    dstfield.destroy()
    dstfield2.destroy()
    if regrid_method == ESMF.RegridMethod.CONSERVE: 
        srcfracfield.destroy()
        dstfracfield.destroy()
    srcgrid.destroy()
    dstgrid.destroy()

    return correct
