
"""
mesh unit test file
"""

import os
import inspect

import ESMF
from ESMF import *
from ESMF.test.base import TestBase, attr
from ESMF.test.test_api.mesh_utilities import *

class TestMesh(TestBase):
    def check_mesh(self, mesh, nodeCoord, nodeOwner):

        xcoords = mesh.get_coords(0)
        ycoords = mesh.get_coords(1)

        # use size here because nodeCoord has all nodes (owned and non-owned)
        xcoords2 = np.array([nodeCoord[2 * i] for i in range(mesh.size[node])])
        ycoords2 = np.array([nodeCoord[2 * i + 1] for i in range(mesh.size[node])])

        # find only the owned coords to compare with what comes back from the mesh
        xcoords3 = xcoords2[np.where(nodeOwner == local_pet())]
        ycoords3 = ycoords2[np.where(nodeOwner == local_pet())]

        assert (all(xcoords == xcoords3))
        assert (all(ycoords == ycoords3))

        # this call fails if nodes and elements have not been added first
        # mesh.free_memory()

    def test_mesh_5(self):
        parallel = False
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_5_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_5()

        self.check_mesh(mesh, nodeCoord, nodeOwner)

    def test_mesh_50_ngons(self):

        Manager(debug=True)
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

    def test_mesh_10(self):
        parallel = False
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_10_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_10()

        self.check_mesh(mesh, nodeCoord, nodeOwner)

    def test_mesh_50(self):
        parallel = False
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50()

        self.check_mesh(mesh, nodeCoord, nodeOwner)

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
        assert mesh.size_local == [12, 5]

        del mesh

        assert mesh2.coords[0][0].shape == (5,)
        assert mesh2.size == [5, None]
        assert mesh2.size_local == [5, None]

        del mesh2

        assert mesh3.coords[0][0].shape == (2,)
        assert mesh3.size == [2, None]
        assert mesh3.size_local == [2, None]

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

        print 'mesh.coords[0][0].shape = ',mesh.coords[0][0].shape
        assert mesh.coords[0][0].shape == (866,)
        assert mesh.size == [866,936]
        assert mesh.size_local == [866,936]

        del mesh

        assert mesh2.coords[0][0].shape == (5,)
        assert mesh2.size == [5, None]
        assert mesh2.size_local == [5, None]

    @attr('data')
    @attr('serial')
    def test_slice_mesh_created_from_file_esmfmesh(self):
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            mesh = Mesh(filename=os.path.join(esmfdir, "test/data/ne4np4-esmf.nc"),
                                  filetype=FileFormat.ESMFMESH)
        except:
            raise NameError('mesh_create_from_file_scrip failed!')

        mesh2 = mesh[0:5]

        assert mesh.coords[0][0].shape == (866,)
        assert mesh.size == [866, 936]
        assert mesh.size_local == [866, 936]

        del mesh

        assert mesh2.coords[0][0].shape == (5,)
        assert mesh2.size == [5, None]
        assert mesh2.size_local == [5, None]
