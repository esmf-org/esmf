# $Id$

"""
examples test file
"""

from ESMF.test.base import TestBase, attr

class TestExamples(TestBase):

    def test_helloworld(self):
        import hello_world

    def test_crff(self):
        import create_read_from_file

    def test_gridmeshrg(self):
        import grid_mesh_regrid

    @attr('slow')
    def test_tripolerg(self):
        import tripole_regrid

    def test_ungriddedrg(self):
        import ungridded_dimension_regrid

    def test_gridcreateperimask(self):
        import grid_create_peridim_mask

    def test_meshlocstreamregrid(self):
        import mesh_locstream_regrid

    def test_gridlocstreamregrid(self):
        import grid_locstream_regrid

    def test_locstreamgridregrid(self):
        import locstream_grid_regrid
