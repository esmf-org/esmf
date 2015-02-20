# $Id$

"""
examples test file
"""

from ESMF.test.base import TestBase, attr

class TestExamples(TestBase):

    def test_helloworld(self):
        import hello_world

    def test_gridcff(self):
        import grid_create_from_file

    def test_meshcff(self):
        import mesh_create_from_file

    def test_gridmeshrg(self):
        import grid_mesh_regrid

    @attr('slow')
    def test_tripolerg(self):
        import tripole_regrid

    def test_ungriddedrg(self):
        import ungridded_dimension_regrid

    def test_gridcreateperimask(self):
        import grid_create_peridim_mask
