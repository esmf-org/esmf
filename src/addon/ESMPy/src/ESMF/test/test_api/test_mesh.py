
"""
mesh unit test file
"""

import os
import inspect

import ESMF
from ESMF import *
from ESMF.test.base import TestBase
from ESMF.test.test_api.mesh_utilities import *

class TestMesh(TestBase):
    def check_mesh(self, mesh, nodeCoord, nodeOwner):
        if pet_count() == 0:
            assert (mesh.size[element] == 50)
            assert (mesh.size[node] == 64)

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

    # TODO: add this test back after issue with hopper is resolved (segfault)
    #def test_mesh_5_pentahexa(self):
    def removed(self):

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

    def test_mesh_create_from_file_scrip(self):
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            mesh_from_file = Mesh(filename=os.path.join(esmfdir, "test/data/ne4np4-pentagons.nc"),
                                  filetype=FileFormat.SCRIP)
        except:
            raise NameError('mesh_create_from_file_scrip failed!')

    def test_mesh_create_from_file_esmfmesh(self):
        try:
            esmfdir = os.path.dirname(inspect.getfile(ESMF))
            mesh_from_file = Mesh(filename=os.path.join(esmfdir, "test/data/ne4np4-esmf.nc"),
                                  filetype=FileFormat.ESMFMESH)
        except:
            raise NameError('mesh_create_from_file_scrip failed!')
