# $Id$

"""
examples test file
"""
try:
    from unittest import SkipTest
except ImportError:
    from nose import SkipTest

from ESMF.test.base import TestBase, attr
import ESMF.api.constants as constants

class TestExamples(TestBase):

    def test_helloworld(self):
        from . import hello_world

    # # ESMF IO does not work in mpiuni mode
    # def test_cubed_sphere_to_mesh_regrid(self):
    #     if constants._ESMF_COMM == constants._ESMF_COMM_MPIUNI:
    #         raise SkipTest('ESMF must be built with MPI for test')
    #     else:
    #         from . import cubed_sphere_to_mesh_regrid

    # ESMF IO does not work in mpiuni mode
    # only example, not in documentation
    def test_field_read(self):
        if constants._ESMF_COMM == constants._ESMF_COMM_MPIUNI:
            raise SkipTest('ESMF must be built with MPI for test')
        else:
            from . import field_read

    # only example, not in documentation
    def test_grid_create_peridim_mask(self):
        from . import grid_create_peridim_mask

    def test_grid_locstream_regrid(self):
        from . import grid_locstream_regrid

    def test_locstream_grid_regrid(self):
        from . import locstream_grid_regrid

    def test_mesh_locstream_regrid(self):
        from . import mesh_locstream_regrid

    # ESMF IO does not work in mpiuni mode
    def test_read_write_weight_file(self):
        if constants._ESMF_COMM == constants._ESMF_COMM_MPIUNI:
            raise SkipTest('ESMF must be built with MPI for test')
        else:
            from . import read_write_weight_file

    # ESMF IO does not work in mpiuni mode
    def test_regrid_from_file(self):
        if constants._ESMF_COMM == constants._ESMF_COMM_MPIUNI:
            raise SkipTest('ESMF must be built with MPI for test')
        else:
            from . import regrid_from_file

    # only example, not in documentation
    @attr('slow')
    def test_tripole_regrid(self):
        from . import tripole_regrid

    # only example, not in documentation
    def test_ugrid_latlon_regrid(self):
        from . import ugrid_latlon_regrid

    # only example, not in documentation
    def test_ungridded_dimension_regrid(self):
        from . import ungridded_dimension_regrid
