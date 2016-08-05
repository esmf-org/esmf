
"""
mesh unit test file
"""

import os
import inspect

import ESMF
from ESMF import *
from ESMF.test.base import TestBase, attr
from ESMF.util.mesh_utilities import *

class TestMesh(TestBase):
    Manager(debug=True)
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

        if elemCoord is not None:
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

    def test_mesh_5(self):
        parallel = False
        elemCoord = None
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_5_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, elemCoord = \
                mesh_create_5()

        self.check_mesh(mesh, nodeCoord, nodeOwner, elemCoord=elemCoord)

    def test_mesh_10(self):
        parallel = False
        elemCoord = None
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_10_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, elemCoord = \
                mesh_create_10()

        self.check_mesh(mesh, nodeCoord, nodeOwner, elemCoord=elemCoord)

    def test_mesh_50(self):
        parallel = False
        elemCoord = None
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, elemCoord = \
                mesh_create_50()

        self.check_mesh(mesh, nodeCoord, nodeOwner, elemCoord=elemCoord)

    def test_mesh_50_ngons(self):
        parallel = False
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_ngons_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_ngons()

        self.check_mesh(mesh, nodeCoord, nodeOwner)

    def test_mesh_50_mask_area(self):
        parallel = False
        elemCoord = None
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, elemMask, elemArea = \
                mesh_create_50_parallel(domask=True, doarea=True)
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, elemMask, elemArea = \
                mesh_create_50(domask=True, doarea=True)

        self.check_mesh(mesh, nodeCoord, nodeOwner, elemCoord=elemCoord)

        self.assertNumpyAll(mesh.mask[1], elemMask, check_arr_dtype=False)

        self.assertNumpyAll(mesh.area, elemArea)

    @attr('data')
    def test_mesh_create_from_file_scrip(self):
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            mesh_from_file = Mesh(filename=os.path.join(esmfdir, "test/data/ne4np4-pentagons.nc"),
                                  filetype=FileFormat.SCRIP)
        except:
            raise NameError('mesh_create_from_file_scrip failed!')

    @attr('data')
    def test_mesh_create_from_file_esmfmesh(self):
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            mesh_from_file = Mesh(filename=os.path.join(esmfdir, "test/data/ne4np4-esmf.nc"),
                                  filetype=FileFormat.ESMFMESH)
        except:
            raise NameError('mesh_create_from_file_scrip failed!')

    def test_mesh_copy(self):
        parallel = False
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_ngons_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_ngons()

        self.check_mesh(mesh, nodeCoord, nodeOwner)

        mesh2 = mesh.copy()
        self.check_mesh(mesh2, nodeCoord, nodeOwner)

    @attr('serial')
    def test_mesh_slicing(self):
        parallel = False
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_5_pentahexa_parallel()
        else:
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

    @attr('data')
    @attr('serial')
    def test_slice_mesh_created_from_file_scrip(self):
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            mesh = Mesh(filename=os.path.join(esmfdir, "test/data/ne4np4-pentagons.nc"),
                        filetype=FileFormat.SCRIP,
                        convert_to_dual=True)
        except:
            raise NameError('mesh_create_from_file_scrip failed!')

        mesh2 = mesh[0:5]

        print ('mesh.coords[0][0].shape = ',mesh.coords[0][0].shape)
        assert mesh.coords[0][0].shape == (866,)
        assert mesh.size == [866,936]
        assert mesh.size_owned == [866,936]

        del mesh

        assert mesh2.coords[0][0].shape == (5,)
        assert mesh2.size == [5, None]
        assert mesh2.size_owned == [5, None]

    @attr('data')
    @attr('serial')
    def test_slice_mesh_created_from_file_esmfmesh(self):
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            mesh = Mesh(filename=os.path.join(esmfdir, "test/data/ne4np4-esmf.nc"),
                                  filetype=FileFormat.ESMFMESH)
        except:
            raise NameError('mesh_create_from_file_esmfmesh failed!')

        mesh2 = mesh[0:5]

        assert mesh.coords[0][0].shape == (866,)
        assert mesh.size == [866, 936]
        assert mesh.size_owned == [866, 936]

        del mesh

        assert mesh2.coords[0][0].shape == (5,)
        assert mesh2.size == [5, None]
        assert mesh2.size_owned == [5, None]


    @attr('data')
    @attr('serial')
    @expected_failure
    #TODO: remove expected failure once we have a smaller data file with mesh element coordinates to use
    # TODO: have to define slicing for mesh element coordinates as well..
    def test_slice_mesh_created_from_file_elem_coords(self):
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            mesh = Mesh(filename=os.path.join(esmfdir, "test/data/ne30np4-t2.nc"),
                        filetype=FileFormat.SCRIP)
        except:
            raise NameError('mesh_create_from_file_elem_coords failed!')

        mesh2 = mesh[0:5]

        assert mesh.coords[node][0].shape == (48600,)
        assert mesh.coords[element][0].shape == (48602,)
        assert mesh.size == [48600, 48602]
        assert mesh.size_owned == [48600, 48602]

        del mesh

        assert mesh2.coords[0][0].shape == (5,)
        assert mesh2.size == [5, None]
        assert mesh2.size_owned == [5, None]
