# $Id$

"""
unit test file
"""

from ESMF import *
from ESMF.interface.cbindings import *
from ESMF.test.base import TestBase

import numpy as np


class TestCbindings(TestBase):

    def test_log(self):

        Manager()
        flush = True
        ESMP_LogSet(flush)

    def test_vm(self):
        # inquire for rank and proc from ESMF Virtual Machine
        localpet = local_pet()
        petcount = pet_count()

        print '\nlocal_pet = {0}\n'.format(localpet)
        print '\npet_count = {0}\n'.format(petcount)


    def test_interfaceint(self):
        Narray = np.array([4,5,6], dtype=np.int32)
        interfaceint = ESMP_InterfaceInt(Narray)


    @expected_failure
    def test_interfaceint2(self):
        # This test should fail
        try:
            a = (ct.c_int*3)()
            a = [1,2,3]
            print a
            interfaceint2 = ESMP_InterfaceInt(a)
        except:
            raise TypeError('FAIL: tuples cannot be used in place of numpy.array')


    @expected_failure
    def test_interfaceint3(self):
        # This test should fail
        try:
            interfaceint2 = ESMP_InterfaceInt(np.array([1,2,3]))
        except:
            raise TypeError('FAIL: tuples cannot be used in place of numpy.array')

    def test_version_compare(self):
        assert(version_compare("ESMF_5_3_0_ESMP_02","ESMF_5_3_0_ESMP_01") == 1)
        assert (version_compare("ESMF_5_3_0_ESMP_01",
                           "ESMF_5_3_1_beta_snapshot_02_ESMP_01") == -1)
        assert (version_compare("ESMF_5_3_0_ESMP_01",
                           "ESMF_5_3_0_beta_snapshot_42_ESMP_01") == 1)
        assert (version_compare("ESMF_5_3_0_ESMP_01",
                           "ESMF_5_3_0_beta_snapshot_37_ESMP_02") == 1)
        assert (version_compare("ESMF_5_3_0_ESMP_01",
                           "ESMF_5_3_1_beta_snapshot_02_ESMP_01") == -1)
        assert (version_compare("ESMF_5_3_0_ESMP_01",
                           "ESMF_6_1_0_beta_snapshot_00_ESMP_01") == -1)
        assert (version_compare("ESMF_6_1_0_beta_snapshot_00_ESMP_01",
                           "ESMF_5_3_1_beta_snapshot_02_ESMP_01") == 1)
        assert (version_compare("ESMF_6_1_0_beta_snapshot_00_ESMP_01",
                           "ESMF_6_1_0_beta_snapshot_00_ESMP_01") == 0)
        assert (version_compare("ESMPy_620b10_04",
                           "ESMF_6_1_0_beta_snapshot_00_ESMP_01") == 1)

# The next two functions are not tested, they are just here for documentation

def make_index(nx, ny):
    '''
    Return an array, of size (ny, nx) that is
    an index, ie runs from 0 to (nx*ny)-1
    '''

    return np.arange(constants.nx * ny).reshape(ny, nx)


def make_index_3D(nx, ny, nz):
    '''
    Return an array, of size (nz, ny, nx) that is
    an index, ie runs from 0 to (nx*ny*nz)-1
    '''

    return np.arange(nx * ny * nz).reshape(nz, ny, nx)


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
    field = ESMF.NewField(grid_or_mesh, name)

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