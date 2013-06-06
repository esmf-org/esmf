#!/usr/bin/env python
#
# $Id: regrid_check.py,v 1.1.2.7 2013/04/10 01:44:28 rokuingh Exp $

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

def create_field(mesh, name, regridmethod):
    '''
    PRECONDITIONS: A Mesh has been created, and 'name' is a string that
                   will be used to initialize the name of a new Field.
    POSTCONDITIONS: A Field has been created.
    '''
    if regridmethod == ESMF.RegridMethod.CONSERVE:
        field = ESMF.Field(mesh, name, meshloc=ESMF.MeshLoc.ELEMENT)
    else:
        field = ESMF.Field(mesh, name, meshloc=ESMF.MeshLoc.NODE)

    return field

def build_analyticfield_const_mesh(field):
    '''
    PRECONDITIONS: A Field has been created on the elements of a Mesh.
    POSTCONDITIONS: The 'field' has been initialized to a constant analytic field.
    '''
    # set the field to a constant value
    for i in range(field.shape[0]):    # this routine assumes this field is on elements
        field[i] = 1.

    return field

def run_regridding(srcfield, dstfield, regridmethod, unmappedaction,
                   dstFracField):
    '''
    PRECONDITIONS: Two Fields have been created and a regridding operation
                   is desired from 'srcfield' to 'dstfield'.
    POSTCONDITIONS: A regridding operation has set the data on 'dstfield'.
    '''
    # call the regridding functions
    regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                                regrid_method=regridmethod,
                                unmapped_action=unmappedaction,
                                dst_frac_field=dstFracField)
    dstfield = regridSrc2Dst(srcfield, dstfield, zero_region=ESMF.Region.SELECT)

    return dstfield

def compare_fields(field1, field2, regridmethod, dstFracField, max_err,
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
    correct = True
    totalErr = 0.0
    for i in range(field1.shape[0]):
        #print "field1 %f, field2 %f\n" % (field1ptr[i], field2ptr[i])
        #print "dstFracFieldptr[i] = ", dstFracFieldptr[i]
        if (regridmethod != ESMF.RegridMethod.CONSERVE or
                dstFracField[i] >= 0.999):
                err = abs(field1[i] - field2[i])/abs(field2[i])
                if err > max_err:
                    correct = False
                    print "ACCURACY ERROR - "+str(err)
                totalErr += err
#        else:
#            print "Partial dest fraction -- skipping"


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
                print " - PASS - Total error = "+str(total_error_global)
            else:
                print " - FAIL - Total error = "+str(total_error_global)

    # this is for serial
    else:
        if correct:
            print " - PASS - Total Error = "+str(totalErr)
        else:
            print " - FAIL - Total Error = "+str(totalErr)

    return correct

def parse_options(options):
        options = options.split()
        opts, args = getopt(options,'it:', ['src_type=', 'dst_type=',
                                            'src_meshname=', 'dst_meshname=',
                                            'ignore_unmapped'])
        src_type_str = "SCRIP"
        dst_type_str = "SCRIP"
        src_meshname = "Undefined"
        dst_meshname = "Undefined"
        unmapped_action = ESMF.UnmappedAction.ERROR
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
        return (src_type_str, dst_type_str, src_meshname, dst_meshname,
                unmapped_action)

def mesh_check(src_fname, dst_fname, regrid_method, options, max_err):

#    print "\nregrid_weight_gen_check.py: mesh_check()"

    regrid_method_map = {"bilinear" : ESMF.RegridMethod.BILINEAR,
                         "patch"        : ESMF.RegridMethod.PATCH,
                         "conserve" : ESMF.RegridMethod.CONSERVE}
    file_type_map = {"VTK" : ESMF.FileFormat.VTK,
                     "SCRIP" : ESMF.FileFormat.SCRIP,
                     "ESMFMESH" : ESMF.FileFormat.ESMFMESH,
                     "ESMFGRID" : ESMF.FileFormat.ESMFGRID,
                     "UGRID" : ESMF.FileFormat.UGRID,
                     "GRIDSPEC" : ESMF.FileFormat.GRIDSPEC}

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
     unmappedaction) = parse_options(options)
    src_type = file_type_map[src_type_str]
    dst_type = file_type_map[dst_type_str]

    regridmethod = regrid_method_map[regrid_method]
    convert_to_dual = (regridmethod == ESMF.RegridMethod.BILINEAR)

    if src_type == ESMF.FileFormat.UGRID:
        srcmesh = ESMF.Mesh(filename=src_fname,
                            filetype=src_type,
                            meshname=src_meshname,
                            convert3D=True)
    elif src_type == ESMF.FileFormat.SCRIP:
        srcmesh = ESMF.Mesh(filename=src_fname,
                            filetype=src_type,
                            convert3D=True,
                            convert_to_dual=convert_to_dual)
    if dst_type == ESMF.FileFormat.UGRID:
        dstmesh = ESMF.Mesh(filename=dst_fname,
                            filetype=dst_type,
                            meshname=dst_meshname,
                            convert3D=True)
    elif dst_type == ESMF.FileFormat.SCRIP:
        dstmesh = ESMF.Mesh(filename=dst_fname,
                            filetype=dst_type,
                            convert3D=True,
                            convert_to_dual=convert_to_dual)
#    if mymesh:
#        print "mymesh = ", mymesh
#        print "meshwrite = ", mymesh.write("step7.out")
#        print "mymesh elem cnt = ", mymesh.size_local[1]

    # create Field objects on the Meshes
    srcfield = create_field(srcmesh, 'srcfield', regridmethod)
    dstfield = create_field(dstmesh, 'dstfield', regridmethod)
    dstfield2 = create_field(dstmesh, 'dstfield_exact', regridmethod)
    dstFracField = create_field(dstmesh, 'dstFracField', regridmethod)

    # initialize the Fields to an analytic function
    srcfield = build_analyticfield_const_mesh(srcfield)
    dstfield = build_analyticfield_const_mesh(dstfield)
    dstfield2 = build_analyticfield_const_mesh(dstfield2)

    # run the ESMF regridding
    dstfield = run_regridding(srcfield, dstfield, regridmethod, unmappedaction,
                              dstFracField)

    # compare results and output PASS or FAIL
    correct = compare_fields(dstfield, dstfield2, regridmethod, dstFracField,
                             max_err, parallel)

    return correct

if __name__ == '__main__':
    src_fname = "FVCOM_grid2d.nc"
    dst_fname = "selfe_grid2d.nc"
    regrid_method = "conserve"
    options = "--src_type UGRID --dst_type UGRID --src_meshname fvcom_mesh --dst_meshname selfe_mesh"
    max_err = 0.06

    sys.exit(mesh_check(src_fname, dst_fname, regrid_method, options, max_err))
