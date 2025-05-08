# $Id$

"""
examples test file
"""

import pytest

from esmpy.test.base import TestBase
from esmpy.api.constants import _ESMF_NETCDF, _ESMF_PIO
import esmpy.api.constants as constants
from esmpy.api.esmpymanager import Manager

# Start up esmpy
mg = Manager(debug=True)

if mg.pet_count == 1:
    from esmpy.util.cache_data import download_example_data
    download_example_data()

class TestExamples(TestBase):
        
    def test_helloworld(self):
        from . import hello_world

    @pytest.mark.skipif(_ESMF_PIO==False, reason="PIO required in ESMF build")
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_field_read(self):
        from . import field_read

    # only example, not in documentation
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_create_peridim_mask(self):
        from . import grid_create_peridim_mask

    @pytest.mark.skipif(mg.pet_count not in {1, 4}, reason="test requires 1 or 4 cores")
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_grid_locstream_regrid(self):
        from . import grid_locstream_regrid

    @pytest.mark.skipif(mg.pet_count not in {1, 4}, reason="test requires 1 or 4 cores")
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_locstream_grid_regrid(self):
        from . import locstream_grid_regrid

    @pytest.mark.skipif(mg.pet_count not in {1, 4}, reason="test requires 1 or 4 cores")
    def test_mesh_locstream_regrid(self):
        from . import mesh_locstream_regrid

    @pytest.mark.skipif(_ESMF_PIO==False, reason="PIO required in ESMF build")
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_read_write_weight_file(self):
        from . import read_write_weight_file

    @pytest.mark.skipif(_ESMF_PIO==False, reason="PIO required in ESMF build")
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_regrid_from_file(self):
        from . import regrid_from_file

    @pytest.mark.skipif(_ESMF_PIO==False, reason="PIO required in ESMF build")
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_ugrid_latlon_regrid(self):
        from . import ugrid_latlon_regrid

    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_ungridded_dimension_regrid(self):
        from . import ungridded_dimension_regrid

    # this will currently never run because it isn't yet possible to run pytest with mpiexec
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    @pytest.mark.skipif(mg.pet_count!=6, reason="test must be run with 6 cores")
    def test_cubed_sphere_to_mesh_regrid(self):
        from . import cubed_sphere_to_mesh_regrid

    # # datafile missing from repo
    # def test_tripole_regrid(self):
    #     from . import tripole_regrid
