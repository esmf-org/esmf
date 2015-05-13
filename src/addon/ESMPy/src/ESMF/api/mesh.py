# $Id$

"""
The Mesh API
"""

#### IMPORT LIBRARIES #########################################################

from copy import copy, deepcopy

from ESMF.api.constants import *
from ESMF.interface.cbindings import *
from ESMF.util.decorators import initialize

from ESMF.api.array import *
from ESMF.api.esmpymanager import *
from ESMF.util.helpers import get_formatted_slice, get_none_or_slice, get_none_or_bound_list

import warnings

#### Mesh class ###############################################################
[node, element] = [0, 1]

class Mesh(object):

    @initialize
    def __init__(self, parametric_dim=None,
                 spatial_dim=None,
                 filename=None,
                 filetype=None,
                 convert_to_dual=None,
                 add_user_area=None,
                 meshname="",
                 mask_flag=None,
                 varname=""):
        """
        Create an unstructured Mesh. This can be done two different ways, 
        as a Mesh in memory, or from a SCRIP formatted or CF compliant UGRID 
        file. The argument for each type of Mesh creation are outlined below. \n
            Mesh in memory: \n
                The in-memory Mesh can be created manually in 3 steps: \n
                    1. create the Mesh (specifying parametric_dim and spatial_dim), \n
                    2. add nodes, \n
                    3. add elements. \n
                    Required arguments for a Mesh in memory: \n
                        parametric_dim: the dimension of the topology of the Mesh (e.g.
                            a Mesh composed of squares would have a parametric dimension of
                            2 and a Mesh composed of cubes would have a parametric dimension
                            of 3). \n
                        spatial_dim: the number of coordinate dimensions needed to
                            describe the locations of the nodes making up the Mesh.  For a
                            manifold the spatial dimension can be larger than the parametric
                            dimension (e.g. the 2D surface of a sphere in 3D space), but it
                            cannot be smaller. \n
                    Optional arguments for creating a Mesh in memory: \n
                        None \n
            Mesh from file: \n
                Note that Meshes created from file do not have the parametric_dim and
                spatial dim set.  \n
                Required arguments for creating a Mesh from file: \n
                    filename: the name of NetCDF file containing the Mesh. \n
                    filetype: the input file type of the Mesh. \n
                        Argument values are: \n
                            FileFormat.SCRIP \n
                            FileFormat.ESMFMESH \n
                            FileFormat.UGRID \n
                Optional arguments for creating a Mesh from file: \n
                    convert_to_dual: a boolean value to specify if the dual
                        Mesh should be calculated.  Defaults to False.  This
                        argument is only supported with filetype FileFormat.SCRIP.\n
                    add_user_area: a boolean value to specify if an area
                        property should be added to the mesh.  This argument is only
                        supported for filetype FileFormat.SCRIP or FileFormat.ESMFMESH.
                        Defaults to False. \n
                    meshname: a string value specifying the name of the
                        Mesh metadata variable in a UGRID file.  This argument is only
                        supported with filetype FileFormat.UGRID.  Defaults to the empty string. \n
                    mask_flag: an enumerated integer that, if specified, tells whether
                        a mask in a UGRID file should be defined on the nodes (MeshLoc.NODE)
                        or the elements (MeshLoc.ELEMENT) of the Mesh.  This argument is only
                        supported with filetype FileFormat.UGRID.  Defaults to no masking. \n
                    varname: a string to specify a variable name for the mask in a UGRID file
                        if mask_flag is specified.  This argument is only supported for
                        filetype FileFormat.UGRID.  Defaults to the empty string. \n
            Returns: \n
                Mesh \n
        """

        # handle input arguments
        fromfile = False
        # in memory
        if (parametric_dim is not None) or (spatial_dim is not None):
            # parametric_dim and spatial_dim are required for in-memory mesh creation
            if (parametric_dim is None) or (spatial_dim is None):
                warning.warn("both parametric_dim and spatial_dim must be specified")
            # raise warnings for the from-file options
            if filename is not None:
                warning.warn("filename is only used for meshes created from file, this argument will be ignored.")
            if filetype is not None:
                warning.warn("filetype is only used for meshes created from file, this argument will be ignored.")
            if convert_to_dual is not None:
                warning.warn("convert_to_dual is only used for meshes created from file, this argument will be ignored.")
            if add_user_area is not None:
                warning.warn("add_user_area is only used for meshes created from file, this argument will be ignored.")
            if meshname is not "":
                warning.warn("meshname is only used for meshes created from file, this argument will be ignored.")
            if mask_flag is not None:
                warning.warn("mask_flag is only used for meshes created from file, this argument will be ignored.")
            if varname is not "":
                warning.warn("varname is only used for meshes created from file, this argument will be ignored.")
        # filename and filetype are required for from-file mesh creation
        elif (filename is None) or (filetype is None):
            raise MeshArgumentError ("must supply either parametric_dim and spatial_dim for an in-memory mesh or filename and filetype for a from-file mesh")
        # from file
        else:
            fromfile = True
            #raise warnings for all in-memory grid options
            if parametric_dim is not None:
                warning.warn("parametric_dim is only used for meshes created in memory, this argument will be ignored.")
            if spatial_dim is not None:
                warning.warn("spatial_dim is only used for meshes created in memory, this argument will be ignored.")
        
        # ctypes stuff
        self.struct = None
    
        # bookkeeping
        self.size = [None, None]
        self.size_local = [None, None]
        self.parametric_dim = None
        self.spatial_dim = None
        self.rank = 1

        # for arbitrary attributes
        self.meta = {}

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
            self.struct = ESMP_MeshCreateFromFile(filename, filetype,
                                                  convert_to_dual, 
                                                  add_user_area, meshname, 
                                                  mask_flag, varname)
            # get the sizes
            self.size[node] = ESMP_MeshGetLocalNodeCount(self)
            self.size_local[node] = ESMP_MeshGetOwnedNodeCount(self)
            self.size[element] = ESMP_MeshGetLocalElementCount(self)
            self.size_local[element] = ESMP_MeshGetOwnedElementCount(self)

            # link the coords here for meshes created from file, in add_elements for others
            self._link_coords_()
            # NOTE: parametric_dim is set in the _link_coords_ call for meshes created from file

        # register with atexit
        import atexit; atexit.register(self.__del__)
        self._finalized = False

        # set the single stagger flag
        self._singlestagger = False

    # manual destructor
    def destroy(self):
        """
        Release the memory associated with a Mesh. \n
        Required Arguments: \n
            None \n
        Optional Arguments: \n
            None \n
        Returns: \n
            None \n
        """
        if hasattr(self, '_finalized'):
            if not self._finalized:
                ESMP_MeshDestroy(self)
                self._finalized = True

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
        self.destroy()

    def __repr__(self):
        """
        Return a string containing a printable representation of the object
        """
        string = ("Mesh:\n"
                  "    parametric_dim = %r\n"
                  "    spatial_dim = %r\n"
                  "    size = %r\n"
                  "    size_local = %r\n" 
                  "    coords = %r\n"
                  %
                  (
                   self.parametric_dim,
                   self.spatial_dim,
                   self.size,
                   self.size_local,
                   self.coords))

        return string

    def _copy_(self):
        # shallow copy
        ret = copy(self)
        # don't call ESMF destructor twice on the same shallow Python object
        # NOTE: the ESMF Mesh destructor is particularly unsafe in this situation
        ret._finalized = True

        return ret

    def __getitem__(self, slc):
        if pet_count() > 1:
            raise SerialMethod

        slc = get_formatted_slice(slc, self.rank)
        ret = self._copy_()

        # TODO: cannot get element coordinates, so the slice has them set to None
        ret.coords = [[get_none_or_slice(get_none_or_slice(get_none_or_slice(self.coords, 0), coorddim), slc) for
                       coorddim in range(self.parametric_dim)], [None for x in range(self.parametric_dim)]]

        # size is "sliced" by taking the shape of the coords
        ret.size = [get_none_or_bound_list(get_none_or_slice(ret.coords, stagger), 0) for stagger in range(2)]
        ret.size_local = ret.size

        return ret

    def _preslice_(self, meshloc):
        # to be used to slice off one stagger location of a grid for a specific field
        ret = self._copy_()
        ret.coords = get_none_or_slice(self.coords, meshloc)

        # preslice the size to only return the meshloc of this field
        ret.size = get_none_or_slice(self.size, meshloc)
        ret.size_local = ret.size

        ret._singlestagger = True

        return ret

    def _slice_onestagger_(self, slc):
        if pet_count() > 1:
            raise SerialMethod

        # to be used to slice the single stagger grid, one that has already been presliced
        slc = get_formatted_slice(slc, self.rank)
        ret = self._copy_()

        ret.coords = [get_none_or_slice(get_none_or_slice(self.coords, x), slc) for x in range(self.parametric_dim)]

        # size is "sliced" by taking the shape of the coords
        ret.size = get_none_or_bound_list(ret.coords, 0)
        ret.size_local = ret.size

        return ret

    def add_elements(self, element_count,
                     element_ids,
                     element_types,
                     element_conn,
                     element_mask=None,
                     element_area=None):
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
                    MeshElemType.TRI \n
                    MeshElemType.QUAD \n
                    MeshElemType.TETRA \n
                    MeshElemType.HEX \n
            element_conn: a numpy array (internally cast to 
                dtype=numpy.int32) to specify the connectivity 
                of the Mesh.  The connectivity array is 
                constructed by concatenating
                the tuples that correspond to the 
                element_ids.  The connectivity tuples are
                constructed by listing the node_ids of each 
                element in COUNTERCLOCKWISE order. \n
                    type: numpy.array \n
                    shape: (sum(element_types[:], 1) \n
        Optional Arguments: \n
            element_mask: a numpy array (internally cast to 
                dtype=numpy.int32) containing integer values to 
                specify masked elements.  The specific values that are masked
                are specified in the Regrid() constructor.\n
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
        self.size[node] = ESMP_MeshGetLocalNodeCount(self)
        self.size_local[node] = ESMP_MeshGetOwnedNodeCount(self)
        self.size[element] = ESMP_MeshGetLocalElementCount(self)
        self.size_local[element] = ESMP_MeshGetOwnedElementCount(self)
        
        # link the coords here for meshes not created from file
        self._link_coords_()

    def add_nodes(self, node_count,
                  node_ids,
                  node_coords,
                  node_owners):
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
                of the Mesh.  The array should be constructed by 
                concatenating the coordinate tuples into a numpy array 
                that correspond to node_ids. \n
                    type: numpy.array \n
                    shape: (spatial_dim*node_count, 1) \n
            node_owners: a numpy array (internally cast to 
                dtype=numpy.int32) to specify the rank of the
                processor that owns each node. \n
                    type: numpy.array \n
                    shape: (node_count, 1) \n
        Optional Arguments: \n
            None \n
        Returns: \n
            None \n
        """

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

    def get_coords(self, coord_dim, meshloc=node):
        """
        Return a numpy array of coordinates at a specified Mesh 
        location (coordinates can only be returned for the Mesh NODES 
        at this time). The returned array is NOT a copy, it is
        directly aliased to the underlying memory allocated by ESMF.\n
        Required Arguments: \n
           coord_dim: the dimension number of the coordinates to return:
                       e.g. [x, y, z] = (0, 1, 2), or [lat, lon] = (0, 1) \n
        Optional Arguments: \n
             meshloc: the mesh location of the coordinates. \n
                Argument values are: \n
                    node=0 (default) \n
                    element=1 (not implemented) \n
        Returns: \n
            None \n
        """

        ret = None
        # only nodes for now
        if not self._singlestagger:
            assert(self.coords_done[meshloc][coord_dim])
            ret = self.coords[meshloc][coord_dim]
        else:
            assert(self.coords_done[coord_dim])
            ret = self.coords[coord_dim]

        return ret

    def _write_(self, filename):
        """
        Write the Mesh to a vtk formatted file. \n
        Required Arguments: \n
            filename: the name of the file, .vtk will be appended. \n
        Optional Arguments: \n
            None \n
        Returns: \n
            None \n
        """

        # call into ctypes layer
        ESMP_MeshWrite(self, filename)

    def _link_coords_(self, meshloc=node):
        """
        Link Python Mesh to ESMC Mesh coordinates.
        Required Arguments: \n
           None \n
        Optional Arguments: \n
             meshloc: the mesh location of the coordinates. \n
                Argument values are: \n
                    node=0 (default) \n
                    element=1 (not implemented) \n
        Returns: \n
            None \n
        """

        # get the pointer to the underlying ESMF data array for coordinates
        coords_interleaved, num_nodes, num_dims = ESMP_MeshGetCoordPtr(self)

        if not self.parametric_dim:
            self.parametric_dim = num_dims

        # initialize the coordinates structures
        # index order is [meshloc][coord_dim]
        self.coords = [[None for a in range(num_dims)] \
                        for b in range(2)]
        self.coords_done = [[False for a in range(num_dims)] \
                             for b in range(2)]

        dim1 = np.array([coords_interleaved[2*i] for i in range(num_nodes)])
        dim2 = np.array([coords_interleaved[2*i+1] for i in range(num_nodes)])
        dim3 = None
        if num_dims == 3:
            dim3 = np.array([coords_interleaved[2*i+2] for i in range(num_nodes)])

        # alias the coordinates to a grid property
        self.coords[meshloc][0] = dim1.view()
        self.coords[meshloc][1] = dim2.view()
        if num_dims == 3:
            self.coords[meshloc][2] = dim3.view()

        # set flag to tell these coordinate has been aliased
        self.coords_done[meshloc][0] = True
        self.coords_done[meshloc][1] = True
        if num_dims == 3:
            self.coords_done[meshloc][2] = True
