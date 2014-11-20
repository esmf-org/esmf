
"""
mesh unit test file
"""

import os
import inspect

import ESMF
from ESMF import *
from ESMF.test.base import TestBase
from ESMF.test.test_api.mesh_regridding_utilities import mesh_create_50, mesh_create_50_parallel


class TestMesh(TestBase):
    def test_mesh(self):
        parallel = False
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        mesh = None
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50()

        element_count = mesh.size[element]
        print 'local element_count = ' + str(element_count) + '\n'

        node_count = mesh.size[node]
        print 'local node_count = ' + str(node_count) + '\n'

        element_count = mesh.size_local[element]
        print 'owned element_count = ' + str(element_count) + '\n'

        node_count = mesh.size_local[node]
        print 'owned node_count = ' + str(node_count) + '\n'

        xcoords = mesh.get_coords(0)
        ycoords = mesh.get_coords(1)

        # use size here because nodeCoord has all nodes (owned and non-owned)
        xcoords2 = np.array([nodeCoord[2 * i] for i in range(mesh.size[node])])
        ycoords2 = np.array([nodeCoord[2 * i + 1] for i in range(mesh.size[node])])

        # find only the owned coords to compare with what comes back from the mesh
        xcoords3 = xcoords2[np.where(nodeOwner == local_pet())]
        ycoords3 = ycoords2[np.where(nodeOwner == local_pet())]

        status = all(xcoords == xcoords3) and all(ycoords == ycoords3)

        # this call fails if nodes and elements have not been added first
        # mesh.free_memory()


    def test_meshvtk(self):
        parallel = False
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        mesh = None
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50()

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
