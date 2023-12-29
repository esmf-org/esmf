"""
mesh unit test file
"""
import os

import numpy as np
import pytest
from esmpy.api.constants import _ESMF_NETCDF
from esmpy.api.constants import _ESMF_PIO
from esmpy.api.constants import CoordSys
from esmpy.api.esmpymanager import local_pet
from esmpy.api.esmpymanager import Manager
from esmpy.api.esmpymanager import pet_count
from esmpy.api.field import element
from esmpy.api.field import node
from esmpy.api.mesh import Mesh
from esmpy.test.base import TestBase
from esmpy.util.cache_data import DATA_DIR
from esmpy.util.mesh_utilities import *
from numpy.testing import assert_array_equal


@pytest.mark.parametrize(
    "mask_value",
    (
        None,
        0,
        True,
        1,
        False,
        [[0, 0, 0, 0], [-1, -1, -1, -1], [1, 1, 1, 1]],
    ),
)
def test_add_nodes_with_mask(mask_value):
    if mask_value is None:
        mask = None
    else:
        mask = np.broadcast_to(mask_value, (3, 4))
    mesh = Mesh(parametric_dim=2, spatial_dim=2, coord_sys=CoordSys.CART)

    x_of_points, y_of_points = np.meshgrid([0.0, 1.0, 2.0, 3.0], [0.0, 1.0, 2.0])
    xy_of_points = np.c_[x_of_points.flat, y_of_points.flat]

    mesh.add_nodes(
        len(xy_of_points),
        np.arange(len(xy_of_points)),
        xy_of_points,
        np.zeros(len(xy_of_points), dtype=int),
        node_mask=mask,
    )

    assert mesh.mask[1] is None
    if mask is None:
        assert mesh.mask[0] is None
    else:
        assert_array_equal(mesh.mask[0], np.asarray(mask).flat)


def test_add_nodes_with_mask_bad():
    mesh = Mesh(parametric_dim=2, spatial_dim=2, coord_sys=CoordSys.CART)

    x_of_points, y_of_points = np.meshgrid([0.0, 1.0, 2.0, 3.0], [0.0, 1.0, 2.0])
    xy_of_points = np.c_[x_of_points.flat, y_of_points.flat]

    with pytest.raises(ValueError):
        mesh.add_nodes(
            len(xy_of_points),
            np.arange(len(xy_of_points)),
            xy_of_points,
            np.zeros(len(xy_of_points), dtype=int),
            node_mask=[1, 2, 3],
        )


class TestMesh(TestBase):

    def check_mesh(self, mesh, nodeCoord, nodeOwner, elemCoord=None):

        xcoords = mesh.get_coords(0)
        ycoords = mesh.get_coords(1)

        # use size here because nodeCoord has all nodes (owned and non-owned)
        xcoords2 = np.array([nodeCoord[2 * i] for i in range(mesh.size[node])])
        ycoords2 = np.array([nodeCoord[2 * i + 1] for i in range(mesh.size[node])])

        # find only the owned coords to compare with what comes back from the mesh
        xcoords3 = xcoords2[np.where(nodeOwner == local_pet())]
        ycoords3 = ycoords2[np.where(nodeOwner == local_pet())]

        assert (np.all(xcoords == xcoords3))
        assert (np.all(ycoords == ycoords3))

        if not isinstance(elemCoord, type(None)):
            xelems = mesh.get_coords(0, 1)
            yelems = mesh.get_coords(1, 1)

            # use size here because elemCoord has all nodes (owned and non-owned)
            xelems2 = np.array([elemCoord[2 * i] for i in range(mesh.size[element])])
            yelems2 = np.array([elemCoord[2 * i + 1] for i in range(mesh.size[element])])

            # TODO: no concept of owned elements yet?
            assert (np.all(xelems == xelems2))
            assert (np.all(yelems == yelems2))

        # this call fails if nodes and elements have not been added first
        # mesh.free_memory()

    @pytest.mark.skipif(pet_count() not in {1, 4}, reason="test requires 1 or 4 cores")
    def test_mesh_5(self):
        elemCoord = None
        parallel = False
        if pet_count() == 4:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_5_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, elemCoord = \
                mesh_create_5()

        self.check_mesh(mesh, nodeCoord, nodeOwner, elemCoord=elemCoord)

    @pytest.mark.skipif(pet_count() not in {1, 4}, reason="test requires 1 or 4 cores")
    def test_mesh_10(self):
        elemCoord = None
        parallel = False
        if pet_count() == 4:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_10_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, elemCoord = \
                mesh_create_10()

        self.check_mesh(mesh, nodeCoord, nodeOwner, elemCoord=elemCoord)

    @pytest.mark.skipif(pet_count() not in {1, 4}, reason="test requires 1 or 4 cores")
    def test_mesh_50(self):
        elemCoord = None
        parallel = False
        if pet_count() == 4:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, elemCoord = \
                mesh_create_50()

        self.check_mesh(mesh, nodeCoord, nodeOwner, elemCoord=elemCoord)

    @pytest.mark.skipif(pet_count() not in {1, 4}, reason="test requires 1 or 4 cores")
    def test_mesh_50_moab(self):
        # set this mesh to be created with the MOAB backend
        mg = Manager()
        mg.set_moab()
        
        elemCoord = None
        parallel = False

        if pet_count() == 4:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, elemCoord = \
                mesh_create_50()

        self.check_mesh(mesh, nodeCoord, nodeOwner, elemCoord=elemCoord)

        assert (mg.moab == True)

        # set back to using the native ESMF mesh for the remaining tests
        mg.set_moab(moab_on=False)
        
        assert (mg.moab == False)

    @pytest.mark.skipif(pet_count() not in {1, 4}, reason="test requires 1 or 4 cores")
    def test_mesh_50_ngons(self):
        parallel = False
        if pet_count() == 4:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_ngons_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_ngons()

        self.check_mesh(mesh, nodeCoord, nodeOwner)

    @pytest.mark.skipif(pet_count() not in {1, 4}, reason="test requires 1 or 4 cores")
    def test_mesh_50_mask_area(self):
        elemCoord = None
        parallel = False
        if pet_count() == 4:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, elemMask, elemArea = \
                mesh_create_50_parallel(domask=True, doarea=True)
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, elemMask, elemArea = \
                mesh_create_50(domask=True, doarea=True)

        self.check_mesh(mesh, nodeCoord, nodeOwner, elemCoord=elemCoord)

        self.assertNumpyAll(mesh.mask[1], elemMask, check_arr_dtype=False)

        self.assertNumpyAll(mesh.area, elemArea)

    @pytest.mark.skipif(_ESMF_PIO==False, reason="PIO required in ESMF build")
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_mesh_create_from_file_scrip(self):
        datafile = os.path.join(DATA_DIR, "ne4np4-pentagons.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        mesh_from_file = Mesh(filename=datafile, filetype=FileFormat.SCRIP)

    @pytest.mark.skipif(_ESMF_PIO==False, reason="PIO required in ESMF build")
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    def test_mesh_create_from_file_esmfmesh(self):
        datafile = os.path.join(DATA_DIR, "ne4np4-esmf.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        mesh_from_file = Mesh(filename=datafile, filetype=FileFormat.ESMFMESH)

    @pytest.mark.skipif(pet_count() not in {1, 4}, reason="test requires 1 or 4 cores")
    def test_mesh_copy(self):
        if pet_count() == 4:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_ngons_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_ngons()

        self.check_mesh(mesh, nodeCoord, nodeOwner)

        mesh2 = mesh.copy()
        self.check_mesh(mesh2, nodeCoord, nodeOwner)

    # slicing is disabled in parallel
    @pytest.mark.skipif(pet_count()!=1, reason="test must be run in serial")
    def test_mesh_slicing(self):
        mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_5_pentahexa()

        mesh2 = mesh[0:5]
        mesh3 = mesh2[1:3]
        
        assert mesh.coords[0][0].shape == (12,)
        assert mesh.size == [12, 5]
        assert mesh.size_owned == [12, 5]

        del mesh

        assert mesh2.coords[0][0].shape == (5,)
        assert mesh2.size == [5, None]
        assert mesh2.size_owned == [5, None]

        del mesh2

        assert mesh3.coords[0][0].shape == (2,)
        assert mesh3.size == [2, None]
        assert mesh3.size_owned == [2, None]

    @pytest.mark.skipif(_ESMF_PIO==False, reason="PIO required in ESMF build")
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    @pytest.mark.skipif(pet_count()!=1, reason="test must be run in serial")
    def test_slice_mesh_created_from_file_scrip(self):
        datafile = os.path.join(DATA_DIR, "ne4np4-pentagons.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")


        mesh = Mesh(filename=datafile,
                    filetype=FileFormat.SCRIP,
                    convert_to_dual=True)

        mesh2 = mesh[0:5]

        print ('mesh.coords[0][0].shape = ',mesh.coords[0][0].shape)
        assert mesh.coords[0][0].shape == (866,)
        assert mesh.size == [866,936]
        assert mesh.size_owned == [866,936]

        del mesh

        assert mesh2.coords[0][0].shape == (5,)
        assert mesh2.size == [5, None]
        assert mesh2.size_owned == [5, None]

    @pytest.mark.skipif(_ESMF_PIO==False, reason="PIO required in ESMF build")
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    @pytest.mark.skipif(pet_count()!=1, reason="test must be run in serial")
    def test_slice_mesh_created_from_file_esmfmesh(self):
        datafile = os.path.join(DATA_DIR, "ne4np4-esmf.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        mesh = Mesh(filename=datafile, filetype=FileFormat.ESMFMESH)

        mesh2 = mesh[0:5]

        assert mesh.coords[0][0].shape == (866,)
        assert mesh.size == [866, 936]
        assert mesh.size_owned == [866, 936]

        del mesh

        assert mesh2.coords[0][0].shape == (5,)
        assert mesh2.size == [5, None]
        assert mesh2.size_owned == [5, None]


    @pytest.mark.xfail
    @pytest.mark.skipif(_ESMF_PIO==False, reason="PIO required in ESMF build")
    @pytest.mark.skipif(_ESMF_NETCDF==False, reason="NetCDF required in ESMF build")
    @pytest.mark.skipif(pet_count()!=1, reason="test must be run in serial")
    #TODO: remove expected failure once we have a smaller data file with mesh element coordinates to use
    # TODO: have to define slicing for mesh element coordinates as well..
    def test_slice_mesh_created_from_file_elem_coords(self):
        datafile = os.path.join(DATA_DIR, "ne30np4-t2.nc")

        if not os.path.exists(datafile):
            raise DataMissing("Data not available, try 'make download'.")

        mesh = Mesh(filename=datafile, filetype=FileFormat.SCRIP)

        mesh2 = mesh[0:5]

        assert mesh.coords[node][0].shape == (48600,)
        assert mesh.coords[element][0].shape == (48602,)
        assert mesh.size == [48600, 48602]
        assert mesh.size_owned == [48600, 48602]

        del mesh

        assert mesh2.coords[0][0].shape == (5,)
        assert mesh2.size == [5, None]
        assert mesh2.size_owned == [5, None]
