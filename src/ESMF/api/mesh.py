#!/usr/bin/env python
#
# $Id: mesh.py,v 1.1.2.7 2013/05/25 00:04:27 rokuingh Exp $

"""
The Mesh API
"""

#### IMPORT LIBRARIES #########################################################

import numpy as np
import numpy.ma as ma

from constants import *
from cbindings import *
from decorators import initialize

from manager import *

#### Mesh class ###############################################################
[node, element] = [0, 1]

class Mesh(object):

    @initialize
    def __init__(self, **kwargs):
        """
        Create an unstructured Mesh. This can be done manually in 3 
        steps: \n
        1. create the Mesh (specifying parametric_dim and spatial_dim), \n
        2. add nodes, \n
        3. add elements. \n\n
        This can also be done in one step if creating the Mesh from a 
        CF compliant UGRID file.\n
        Required Arguments: \n
            None \n
        Optional Arguments: \n
            3 step creation: \n
                parametric_dim: the parametric dimension of the Mesh. \n
                spatial_dim: the spatial dimension of the Mesh. \n
                    2D Cartesian = [2,2] \n
                    3D Cartesian = [2,3] \n
                    3D Spherical = [?,?] \n
            1 step creation: \n
                filetype: the input file type of the Mesh. \n
                    Argument values are: \n
                        FileFormat.SCRIP \n
                        FileFormat.ESMFMESH \n
                        FileFormat.UGRID \n
                convert3D: a boolean value to specify a 3D Mesh. \n
                convert_to_dual: a boolean value to specify if the dual 
                                 Mesh should be calculated. \n
                add_user_area: a boolean value to specify if an area 
                               property should be added to the mesh 
                               (numpy array). \n
                meshname: a string value specifying the name of the 
                          Mesh. \n
                add_mask: a boolean value to specify if a mask should 
                          be added to the Mesh (numpy array). \n
                varname: a string to specify a varname? \n
        Returns: \n
            Mesh \n
        """
        # kwargs
        parametric_dim = None
        if 'parametric_dim' in kwargs:
            parametric_dim = kwargs.get('parametric_dim')

        spatial_dim = None
        if 'spatial_dim' in kwargs:
            spatial_dim = kwargs.get('spatial_dim')

        filename = ""
        if 'filename' in kwargs:
            filename = kwargs.get('filename')

        filetype = None
        if 'filetype' in kwargs:
            filetype = kwargs.get('filetype')

        convert3D = None
        if 'convert3D' in kwargs:
            convert3D = kwargs.get('convert3D')

        convert_to_dual = None
        if 'convert_to_dual' in kwargs:
            convert_to_dual = kwargs.get('convert_to_dual')

        add_user_area = None
        if 'add_user_area' in kwargs:
            add_user_area = kwargs.get('add_user_area')

        meshname = ""
        if 'meshname' in kwargs:
            meshname = kwargs.get('meshname')

        add_mask = None
        if 'add_mask' in kwargs:
            add_mask = kwargs.get('add_mask')

        varname = ""
        if 'varname' in kwargs:
            varname = kwargs.get('varname')

        # ctypes stuff
        self.struct = ESMP_Mesh()
    
        # bookkeeping
        self.size = [None, None]
        self.size_local = [None, None]
        self.parametric_dim = None
        self.spatial_dim = None

        fromfile = False
        if filetype is not None:
            fromfile = True
        
        if not fromfile:
            # initialize not fromfile variables
            self.element_count = None
            self.element_ids = None
            self.element_types = None 
            self.element_conn = None
            self.element_mask = None
            self.element_area = None
            self.node_count = None
            self.node_ids = None
            self.node_coords = None
            self.node_owners = None
            
            # call into ctypes layer
            self.struct = ESMP_MeshCreate(parametricDim=parametric_dim, 
                                          spatialDim=spatial_dim)
            self.parametric_dim = parametric_dim
            self.spatial_dim = spatial_dim
        else:
            # call into ctypes layer
            self.struct = ESMP_MeshCreateFromFile(filename, filetype, convert3D,
                                                  convert_to_dual, 
                                                  add_user_area, meshname, 
                                                  add_mask, varname)
            # get the sizes
            self.size[node] = ESMP_MeshGetOwnedNodeCount(self)
            self.size_local[node] = ESMP_MeshGetLocalNodeCount(self)
            self.size[element] = ESMP_MeshGetOwnedElementCount(self)
            self.size_local[element] = ESMP_MeshGetLocalElementCount(self)

    def __del__(self):
        """
        Release the memory associated with a Mesh. \n
        Required Arguments: \n
            None \n
        Optional Arguments: \n
            None \n
        Returns: \n
            None \n
        """
        ESMP_MeshDestroy(self)

    def add_elements(self, *args, **kwargs):
        """
        Add elements to a Mesh, this must be done after adding nodes. \n
        Required Arguments: \n
            element_count: the number of elements to add to the Mesh. \n
            element_ids: a numpy array (internally cast to 
                dtype=numpy.int32) to specify the element_ids. \n
                    type: numpy.array \n
                    shape: (element_count, 1) \n
            element_types: a numpy array (internally cast to 
                dtype=numpy.int32) to specify the element_types. \n
                    type: numpy.array \n
                    shape: (element_count, 1) \n
                Argument values are: \n
                    TRI = 2D triangular elements with 3 sides
                    QUAD = 2D quadrilateral elements with 4 sides
                    TETRA = 3D tetrahedral elements with 4 faces
                    HEX = 3D hexahedral elements with 6 faces
            element_conn: a numpy array (internally cast to 
                dtype=numpy.int32) to specify the connectivity 
                of the Mesh.  The connectivity array is 
                constructed by concatenating
                the tuples which correspond to the 
                element_ids.  The connectivity tuples are
                constructed by listing the node_ids of each 
                element counterclockwise. \n
                    type: numpy.array \n
                    shape: (sum(element_types[:], 1) \n
        Optional Arguments: \n
            element_mask: a numpy array (internally cast to 
                dtype=numpy.int32) containing 0's and 1's to 
                specify masked elements, where 1 specifies 
                that the element is masked. \n
                    type: numpy.array \n
                    shape: (element_count, 1) \n
            element_area: a numpy array (internally cast to 
                dtype=numpy.float64) to specify the areas of the 
                elements. \n
                    type: numpy.array \n
                    shape: (element_count, 1) \n
        Returns: \n
            None \n
        """
        # args
        try:
            element_count = args[0]
            element_ids   = args[1]
            element_types = args[2]
            element_conn  = args[3]
        except:
            raise RequiredArgs(Mesh.add_elements.__doc__)

        # kwargs
        element_mask = None
        if 'element_mask' in kwargs:
            element_mask = kwargs.get('element_mask')

        element_area = None
        if 'element_area' in kwargs:
            element_area = kwargs.get('element_area')

        # initialize not fromfile variables
        self.element_count = element_count
        if element_ids.dtype is not np.int32:
            self.element_ids = np.array(element_ids, dtype=np.int32)
        else:
            self.element_ids = element_ids
        if element_types.dtype is not np.int32:
            self.element_types = np.array(element_types, dtype=np.int32)
        else:
            self.element_types = element_types
        if element_conn.dtype is not np.int32:
            self.element_conn = np.array(element_conn, dtype=np.int32)
        else:
            self.element_conn = element_conn
        if element_mask is not None:
            if element_mask.dtype is not np.int32:
                self.element_mask = np.array(element_mask, dtype=np.int32)
            else:
                self.element_mask = element_mask
        if element_area is not None:
            if element_area.dtype is not np.float64:
                self.element_area = np.array(element_area, dtype=np.float64)
            else:
                self.element_area = element_area

        # call into ctypes layer
        ESMP_MeshAddElements(self, self.element_count, self.element_ids, 
                             self.element_types, self.element_conn, 
                             self.element_mask, self.element_area)
        
        # get the sizes
        self.size[node] = ESMP_MeshGetOwnedNodeCount(self)
        self.size_local[node] = ESMP_MeshGetLocalNodeCount(self)
        self.size[element] = ESMP_MeshGetOwnedElementCount(self)
        self.size_local[element] = ESMP_MeshGetLocalElementCount(self)
        
    def add_nodes(self, *args):
        """
        Add nodes to a Mesh, this must be done before adding elements. \n
        Required Arguments: \n
            node_count: the number of nodes to add to the Mesh. \n
            node_ids: a numpy array (internally cast to 
                dtype=numpy.int32) to specify the node_ids. \n
                    type: numpy.array \n
                    shape: (node_count, 1) \n
            node_coords: a numpy array (internally cast to 
                dtype=numpy.float64) to specify the coordinates 
                of the Mesh.  The array should be contructed by 
                concatenating the coordinate tuples into a numpy array 
                which correspond to node_ids. \n
                    type: numpy.array \n
                    shape: (spatial_dim*node_count, 1) \n
            node_owners: a numpy array (internally cast to 
                dtype=numpy.int32) to specify the rank of the
                processor which owns each node. \n
                    type: numpy.array \n
                    shape: (node_count, 1) \n
        Optional Arguments: \n
            None \n
        Returns: \n
            None \n
        """
        # args
        try:
            node_count = args[0]
            node_ids = args[1]
            node_coords = args[2]
            node_owners = args[3]
        except:
            raise RequiredArgs(Mesh.add_nodes.__doc__)

        self.node_count = node_count
        if node_ids.dtype is not np.int32:
            self.node_ids = np.array(node_ids, dtype=np.int32)
        else:
            self.node_ids = node_ids
        if node_coords.dtype is not np.float64:
            self.node_coords = np.array(node_coords, dtype=np.float64)
        else:
            self.node_coords = node_coords
        if node_owners.dtype is not np.int32:
            self.node_owners = np.array(node_owners, dtype=np.int32)
        else:
            self.node_owners = node_owners
 
        # call into ctypes layer
        ESMP_MeshAddNodes(self, self.node_count, self.node_ids, 
                          self.node_coords, self.node_owners)
        # can't get the sizes until mesh is "committed" in element call

    def free_memory(self):
        """
        Free memory associated with the creation of a Mesh which is no 
        longer needed. \n
        Required Arguments: \n
            None \n
        Optional Arguments: \n
            None \n
        Returns: \n
            None \n
        """
        # call into ctypes layer
        ESMP_MeshFreeMemory(self)

    def write(self, *args):
        """
        Write the Mesh to a vtk formatted file. \n
        Required Arguments: \n
            filename: the name of the file, .vtk will be appended. \n
        Optional Arguments: \n
            None \n
        Returns: \n
            None \n
        """
        # args
        try:
            filename = args[0]
        except:
            raise RequiredArgs(Mesh.write.__doc__)

        # call into ctypes layer
        ESMP_MeshWrite(self, filename)
