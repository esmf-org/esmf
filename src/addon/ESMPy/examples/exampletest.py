# $Id$

"""
examples test file
"""

from ESMF.test.base import TestBase, attr

class TestExamples(TestBase):

    def test_helloworld(self):
        from . import hello_world

    @attr('parallel')
    def test_crff(self):
        from . import create_read_from_file

    def test_gridmeshrg(self):
        from . import grid_mesh_regrid

    @attr('parallel')
    def test_field_read(self):
        from . import field_read

    @attr('slow')
    def test_tripolerg(self):
        from . import tripole_regrid

    def test_ungriddedrg(self):
        from . import ungridded_dimension_regrid

    def test_gridcreateperimask(self):
        from . import grid_create_peridim_mask

    def test_meshlocstreamregrid(self):
        from . import mesh_locstream_regrid

    def test_gridlocstreamregrid(self):
        from . import grid_locstream_regrid

    def test_locstreamgridregrid(self):
        from . import locstream_grid_regrid

    def test_ugrid_latlon_regrid(self):
        from . import ugrid_latlon_regrid
