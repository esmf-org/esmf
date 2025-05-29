# $Id$

"""
The Mesh API
"""

#### IMPORT LIBRARIES #########################################################

from copy import copy

from esmpy.api.constants import *
from esmpy.interface.cbindings import *
from esmpy.util.decorators import initialize

from esmpy.api.esmpymanager import *
from esmpy.util.slicing import get_formatted_slice, get_none_or_slice, get_none_or_bound_list

import warnings

#### Mesh class ###############################################################
[node, element] = [0, 1]

class Mesh(object):
    """
    The :class:`~esmpy.api.mesh.Mesh` class is a Python wrapper object for the ESMF Mesh.
    The individual values of all coordinate and mask arrays are referenced to
    those of the underlying Fortran ESMF object.

    The ESMF library provides a class for representing unstructured grids called
    the :class:`~esmpy.api.mesh.Mesh`. :class:`Fields <esmpy.api.field.Field>` can be created on a :class:`~esmpy.api.mesh.Mesh` to hold data. :class:`Fields <esmpy.api.field.Field>` created on a
    :class:`~esmpy.api.mesh.Mesh` can also be used as either the source or destination or both of a
    regrididng operation which allows data to be moved between unstructured
    grids.  A :class:`~esmpy.api.mesh.Mesh` is constructed of nodes and elements. A node, also known as
    a vertex or corner, is a part of a :class:`~esmpy.api.mesh.Mesh` which represents a single point.
    Coordinate information is set in a node. An element, also known as a cell,
    is a part of a mesh which represents a small region of space. Elements are
    described in terms of a connected set of nodes which represent locations
    along their boundaries. :class:`~esmpy.api.field.Field` data may be located on either the nodes or
    elements of a :class:`~esmpy.api.mesh.Mesh`.

    Refer to the Mesh Class of the 
    `ESMF Reference Manual <http://earthsystemmodeling.org/docs/release/latest/ESMF_refdoc/>`_
    for more information.

    An unstructured :class:`~esmpy.api.mesh.Mesh` can be created in two different ways, as a :class:`~esmpy.api.mesh.Mesh` in
    memory, or from a SCRIP formatted or CF compliant UGRID file. The arguments
    for each type of :class:`~esmpy.api.mesh.Mesh` creation are outlined below.

    **Created in-memory:**

    The in-memory :class:`~esmpy.api.mesh.Mesh` can be created manually in 3 steps:
        1. create the :class:`~esmpy.api.mesh.Mesh` (specifying ``parametric_dim`` and ``spatial_dim``),
        2. add nodes,
        3. add elements.

    *REQUIRED:*

    :param int parametric_dim: the dimension of the topology of the
        :class:`~esmpy.api.mesh.Mesh` (e.g. a :class:`~esmpy.api.mesh.Mesh` composed of squares would have a
        parametric dimension of 2 and a :class:`~esmpy.api.mesh.Mesh` composed of cubes
        would have a parametric dimension of 3).
    :param int spatial_dim: the number of coordinate dimensions
        needed to describe the locations of the nodes making up the
        :class:`~esmpy.api.mesh.Mesh`.  For a manifold the spatial dimension can be larger
        than the parametric dimension (e.g. the 2D surface of a
        sphere in 3D space), but it cannot be smaller.

    *OPTIONAL:*

    :param CoordSys coord_sys: Coordinate system for the
        :class:`~esmpy.api.mesh.Mesh`.
        If ``None``, defaults to :attr:`~esmpy.api.constants.CoordSys.SPH_DEG`.

    **Created from file:**

    Note that :class:`Meshes <esmpy.api.mesh.Mesh>` created from file do not use the ``parametric_dim`` and
    ``spatial_dim`` parameters.

    *REQUIRED:*

    :param str filename: the name of NetCDF file containing the :class:`~esmpy.api.mesh.Mesh`.
    :param FileFormat filetype: the input
        :attr:`~esmpy.api.constants.FileFormat` of the :class:`~esmpy.api.mesh.Mesh`.

    *OPTIONAL:*

    :param bool convert_to_dual: a boolean value to specify if the
        dual :class:`~esmpy.api.mesh.Mesh` should be calculated.  Defaults to False.  This
        argument is only supported with
        :attr:`~esmpy.api.constants.FileFormat.SCRIP`.
    :param bool add_user_area: a boolean value to specify if an area
        property should be added to the mesh.  This argument is only
        supported for :attr:`~esmpy.api.constants.FileFormat.SCRIP`
        or :attr:`~esmpy.api.constants.FileFormat.ESMFMESH`.
        If ``None``, defaults to False.
    :param str meshname: the name of the :class:`~esmpy.api.mesh.Mesh` metadata variable in
        a UGRID file.  This argument is only supported with
        :attr:`~esmpy.api.constants.FileFormat.UGRID`.
        If ``None``, defaults to the empty string.
    :param MeshLoc mask_flag: an enumerated integer that, if
        specified, tells whether a mask in a UGRID file should be
        defined on the :attr:`~esmpy.api.constants.MeshLoc.NODE`s, or
        :attr:`~esmpy.api.constants.MeshLoc.ELEMENT`s of the :class:`~esmpy.api.mesh.Mesh`.
        This argument is only supported with
        :attr:`~esmpy.api.constants.FileFormat.UGRID`.
        If ``None``, defaults to no masking.
    :param str varname: a variable name for the mask in a UGRID file
        if mask_flag is specified.  This argument is only supported
        for :attr:`~esmpy.api.constants.FileFormat.UGRID`.
        If ``None``, defaults to the empty string.
    """

    @initialize
    def __init__(self, parametric_dim=None,
                 spatial_dim=None,
                 coord_sys=None,
                 filename=None,
                 filetype=None,
                 convert_to_dual=None,
                 add_user_area=None,
                 meshname="",
                 mask_flag=None,
                 varname=""):

        # handle input arguments
        fromfile = False
        # in memory
        if (not isinstance(parametric_dim, type(None))) or (not isinstance(spatial_dim, type(None))):
            # parametric_dim and spatial_dim are required for in-memory mesh creation
            if (isinstance(parametric_dim, type(None))) or (isinstance(spatial_dim, type(None))):
                warnings.warn("both parametric_dim and spatial_dim must be specified")
            # raise warnings for the from-file options
            if not isinstance(filename, type(None)):
                warnings.warn("filename is only used for meshes created from file, this argument will be ignored.")
            if not isinstance(filetype, type(None)):
                warnings.warn("filetype is only used for meshes created from file, this argument will be ignored.")
            if not isinstance(convert_to_dual, type(None)):
                warnings.warn("convert_to_dual is only used for meshes created from file, this argument will be ignored.")
            if not isinstance(add_user_area, type(None)):
                warnings.warn("add_user_area is only used for meshes created from file, this argument will be ignored.")
            if meshname != "":
                warnings.warn("meshname is only used for meshes created from file, this argument will be ignored.")
            if not isinstance(mask_flag, type(None)):
                warnings.warn("mask_flag is only used for meshes created from file, this argument will be ignored.")
            if varname != "":
                warnings.warn("varname is only used for meshes created from file, this argument will be ignored.")
        # filename and filetype are required for from-file mesh creation
        elif (isinstance(filename, type(None))) or (isinstance(filetype, type(None))):
            raise MeshArgumentError ("must supply either parametric_dim and spatial_dim for an in-memory mesh or filename and filetype for a from-file mesh")
        # from file
        else:
            fromfile = True
            #raise warnings for all in-memory grid options
            if not isinstance(parametric_dim, type(None)):
                warnings.warn("parametric_dim is only used for meshes created in memory, this argument will be ignored.")
            if not isinstance(spatial_dim, type(None)):
                warnings.warn("spatial_dim is only used for meshes created in memory, this argument will be ignored.")
            if not isinstance(coord_sys, type(None)):
                warnings.warn("coord_sys is only used for meshes created in memory, this argument will be ignored.")

        # ctypes stuff
        self._struct = None
    
        # bookkeeping
        self._size = [None, None]
        self._size_owned = [None, None]
        self._parametric_dim = None
        self._spatial_dim = None
        self._coord_sys = None
        self._rank = 1
        self._coords = [None, None]
        self._mask = [None, None]
        self._area = [None, None]

        if not fromfile:
            # initialize not fromfile variables
            self._element_count = None
            self._element_ids = None
            self._element_types = None
            self._element_conn = None
            self._element_mask = None
            self._element_area = None
            self._element_coords = None
            self._node_count = None
            self._node_ids = None
            self._node_coords = None
            self._node_owners = None
            
            # call into ctypes layer
            self._struct = ESMP_MeshCreate(parametricDim=parametric_dim,
                                          spatialDim=spatial_dim,
                                          coordSys=coord_sys)
            self._parametric_dim = parametric_dim
            self._spatial_dim = spatial_dim
            self._coord_sys = coord_sys
        else:
            # call into ctypes layer
            self._struct = ESMP_MeshCreateFromFile(filename, filetype,
                                                  convert_to_dual, 
                                                  add_user_area, meshname, 
                                                  mask_flag, varname)
            # get the sizes
            self._size[node] = ESMP_MeshGetNodeCount(self)
            self._size_owned[node] = ESMP_MeshGetOwnedNodeCount(self)
            self._size[element] = ESMP_MeshGetElementCount(self)
            self._size_owned[element] = ESMP_MeshGetOwnedElementCount(self)

            # link the coords here for meshes created from file, in add_elements for others
            self._link_coords_()
            # NOTE: parametric_dim is set in the _link_coords_ call for meshes created from file

        # for arbitrary metadata
        self._meta = {}

        # register with atexit
        import atexit; atexit.register(self.__del__)
        self._finalized = False

    def __del__(self):
        self.destroy()

    def __getitem__(self, slc):
        if pet_count() > 1:
            raise SerialMethod

        slc = get_formatted_slice(slc, self.rank)
        ret = self.copy()

        # TODO: cannot get element coordinates, so the slice has them set to None
        ret._coords = [[get_none_or_slice(get_none_or_slice(get_none_or_slice(self.coords, 0), coorddim), slc) for
                        coorddim in range(self.parametric_dim)], [None for x in range(self.parametric_dim)]]

        # size is "sliced" by taking the shape of the coords
        ret._size = [get_none_or_bound_list(get_none_or_slice(ret.coords, stagger), 0) for stagger in range(2)]
        ret._size_owned = ret.size

        return ret

    def __repr__(self):
        string = ("Mesh:\n"
                  "    rank = %r\n"
                  "    size = %r\n"
                  "    size_owned = %r\n" 
                  "    coords = %r\n"
                  %
                  (
                   self.rank,
                   self.size,
                   self.size_owned,
                   self.coords))

        return string

    @property
    def area(self):
        """
        :rtype: A two element list of numpy arrays to hold values for the nodes
            and elements of the :class:`~esmpy.api.mesh.Mesh`.
        :return: The :class:`~esmpy.api.mesh.Mesh` area represented as a numpy
            array of floats of the same number of entries as :class:`~esmpy.api.mesh.Mesh` elements.
        """
        return self._area

    @property
    def coords(self):
        """
        :rtype: A two element list of numpy arrays to hold values for the nodes
            and elements of the :class:`~esmpy.api.mesh.Mesh`.
        :return: The coordinates represented as a numpy array of floats
            with a value for each node and/or element of the :class:`~esmpy.api.mesh.Mesh`
            :class:`~esmpy.api.mesh.Mesh`.
        """
        return self._coords

    @property
    def coord_sys(self):
        """
        :rtype: :attr:`~esmpy.api.constants.CoordSys`
        :return: The coordinate system of the :class:`~esmpy.api.mesh.Mesh`.
        """
        return self._coord_sys

    @property
    def element_area(self):
        return self._element_area

    @property
    def element_conn(self):
        return self._element_conn

    @property
    def element_coords(self):
        return self._element_coords

    @property
    def element_count(self):
        return self._element_count

    @property
    def element_ids(self):
        return self._element_ids

    @property
    def element_mask(self):
        return self._element_mask

    @property
    def element_types(self):
        return self._element_types

    @property
    def finalized(self):
        """
        :rtype: bool
        :return: Indicate if the underlying ESMF memory for this object has
            been deallocated.
        """
        return self._finalized

    @property
    def mask(self):
        """
        :rtype: A two element list of numpy arrays to hold values for the nodes
            and elements of the :class:`~esmpy.api.mesh.Mesh`.
        :return: The masked values on the nodes and elements of the
            :class:`~esmpy.api.mesh.Mesh`.
        """
        return self._mask

    @property
    def meta(self):
        """
        :rtype: tdk
        :return: tdk
        """
        return self._meta

    @property
    def node_coords(self):
        return self._node_coords

    @property
    def node_count(self):
        return self._node_count

    @property
    def node_ids(self):
        return self._node_ids

    @property
    def node_owners(self):
        return self._node_owners

    @property
    def parametric_dim(self):
        return self._parametric_dim

    @property
    def rank(self):
        """
        :rtype: int
        :return: The rank of the Mesh, (i.e. always 1).
        """
        return self._rank

    @property
    def size(self):
        """
        :rtype: A two element list of integers.
        :return: The number of nodes and elements in the :class:`~esmpy.api.mesh.Mesh` on the current
            processor.
        """
        return self._size

    @property
    def size_owned(self):
        """
        :rtype: A two element list of integers.
        :return: The number of owned nodes and elements in the :class:`~esmpy.api.mesh.Mesh` on the
            current processor.
        """
        return self._size_owned

    @property
    def spatial_dim(self):
        return self._spatial_dim

    @property
    def struct(self):
        """
        :rtype: pointer
        :return: A pointer to the underlying ESMF allocation for this
            :class:`~esmpy.api.mesh.Mesh`.
        """
        return self._struct

    def add_elements(self, element_count,
                     element_ids,
                     element_types,
                     element_conn,
                     element_mask=None,
                     element_area=None,
                     element_coords=None):
        """
        Add elements to a :class:`~esmpy.api.mesh.Mesh`, this must be done after adding nodes.

        *REQUIRED:*

        :param int element_count: the number of elements to add to the :class:`~esmpy.api.mesh.Mesh`.
        :param ndarray element_ids: a numpy array of of shape
            ``(element_count, 1)`` to specify the element ids.
        :param ndarray element_types: a numpy array of
            :attr:`~esmpy.api.constants.MeshElemType`s of shape
            ``(element_count, 1)`` to specify the element types.
        :param ndarray element_conn: a numpy array of shape
            ``sum(element_types[:], 1)`` to specify the connectivity of the
            :class:`~esmpy.api.mesh.Mesh`. The connectivity array is constructed by concatenating the
            tuples that correspond to the element_ids. The connectivity tuples
            are constructed by listing the node_ids of each element in
            **COUNTERCLOCKWISE** order.

        *OPTIONAL:*

        :param ndarray element_mask: a numpy array of shape
            ``(element_count, 1)`` containing integer values to specify masked
            elements. The specific values that are masked are specified in the
            :class:`~esmpy.api.regrid.Regrid` constructor.
        :param ndarray element_area: a numpy array of shape
            ``(element_count, 1)`` to specify the areas of the elements.
        :param ndarray element_coords: a numpy array of shape
            ``(element_count, 1)`` to specify the coordinates of the elements.
        """

        # initialize not fromfile variables
        self._element_count = element_count
        if not isinstance(element_ids.dtype, np.int32):
            self._element_ids = np.array(element_ids, dtype=np.int32)
        else:
            self._element_ids = element_ids
        if not isinstance(element_types.dtype, np.int32):
            self._element_types = np.array(element_types, dtype=np.int32)
        else:
            self._element_types = element_types
        if not isinstance(element_conn.dtype, np.int32):
            self._element_conn = np.array(element_conn, dtype=np.int32)
        else:
            self._element_conn = element_conn
        if not isinstance(element_mask, type(None)):
            if not isinstance(element_mask.dtype, np.int32):
                self._element_mask = np.array(element_mask, dtype=np.int32)
            else:
                self._element_mask = element_mask
            self._mask[1] = self._element_mask
        if not isinstance(element_area, type(None)):
            if not isinstance(element_area.dtype, np.float64):
                self._element_area = np.array(element_area, dtype=np.float64)
            else:
                self._element_area = element_area
            self._area = self._element_area
        if not isinstance(element_coords, type(None)):
            if not isinstance(element_coords.dtype, np.float64):
                self._element_coords = np.array(element_coords, dtype=np.float64)
            else:
                self._element_coords = element_coords

        # call into ctypes layer
        ESMP_MeshAddElements(self, self.element_count, self.element_ids, 
                             self.element_types, self.element_conn, 
                             self.element_mask, self.element_area,
                             self.element_coords)
        
        # get the sizes
        self.size[node] = ESMP_MeshGetNodeCount(self)
        self.size_owned[node] = ESMP_MeshGetOwnedNodeCount(self)
        self.size[element] = ESMP_MeshGetElementCount(self)
        self.size_owned[element] = ESMP_MeshGetOwnedElementCount(self)
        
        # link the coords here for meshes not created from file
        self._link_coords_()

    def add_nodes(self, node_count,
                  node_ids,
                  node_coords,
                  node_owners,
                  node_mask=None):
        """
        Add nodes to a :class:`~esmpy.api.mesh.Mesh`, this must be done before adding elements.

        :param int node_count: the number of nodes to add to the :class:`~esmpy.api.mesh.Mesh`.
        :param ndarray node_ids: a numpy array of shape (node_count, 1) to
            specify the node_ids.
        :param ndarray node_coords: a numpy array of shape
            (spatial_dim*node_count, 1) to specify the coordinates of the :class:`~esmpy.api.mesh.Mesh`.
            The array should be constructed by concatenating the coordinate
            tuples into a numpy array that correspond to node_ids.
        :param ndarray node_owners: a numpy array of shape (node_count, 1) to
            specify the rank of the processor that owns each node.

        *OPTIONAL:*

        :param ndarray node_mask: a numpy array of shape
            ``(node_count, 1)`` containing integer values to specify masked
            nodes. The specific values that are masked are specified in the
            :class:`~esmpy.api.regrid.Regrid` constructor.
        """

        self._node_count = node_count
        if not isinstance(node_ids.dtype, np.int32):
            self._node_ids = np.array(node_ids, dtype=np.int32)
        else:
            self._node_ids = node_ids
        if not isinstance(node_coords.dtype, np.float64):
            self._node_coords = np.array(node_coords, dtype=np.float64)
        else:
            self._node_coords = node_coords
        if not isinstance(node_owners.dtype, np.int32):
            self._node_owners = np.array(node_owners, dtype=np.int32)
        else:
            self._node_owners = node_owners

        if node_mask is not None:
            node_mask = np.asarray(node_mask).astype(np.int32, copy=False).reshape(-1)
        self._node_mask = node_mask
        self._mask[0] = self._node_mask


        if node_mask is not None and len(node_mask) != node_count:
            raise ValueError(
                "`node_mask` must have the same length as the number of nodes"
                f" (expected {node_count}, got {len(node_mask)})"
            )

        # call into ctypes layer
        ESMP_MeshAddNodes(self, self.node_count, self.node_ids, 
                          self.node_coords, self.node_owners, self._node_mask)
        # can't get the sizes until mesh is "committed" in element call

    def copy(self):
        """
        Copy a :class:`~esmpy.api.mesh.Mesh` in an ESMF-safe manner.

        :return: A :class:`~esmpy.api.mesh.Mesh` shallow copy.
        """
        # shallow copy
        ret = copy(self)
        # don't call ESMF destructor twice on the same shallow Python object
        # NOTE: the ESMF Mesh destructor is particularly unsafe in this situation
        ret._finalized = True

        return ret

    def destroy(self):
        """
        Release the memory associated with a :class:`~esmpy.api.mesh.Mesh`.
        """
        if hasattr(self, '_finalized'):
            if not self._finalized:
                ESMP_MeshDestroy(self)
                self._finalized = True

    def free_memory(self):
        """
        Free memory associated with the creation of a
        :class:`~esmpy.api.mesh.Mesh` which is no longer needed for ongoing
        operations.
        """
        # call into ctypes layer
        ESMP_MeshFreeMemory(self)

    def get_coords(self, coord_dim, meshloc=MeshLoc.NODE):
        """
        Return a numpy array of coordinates at a specified :class:`~esmpy.api.mesh.Mesh` 
        location (coordinates can only be returned for the :class:`~esmpy.api.mesh.Mesh`
        :attr:`~esmpy.api.constants.MeshLoc.NODE`
        at this time). The returned array is NOT a copy, it is
        directly aliased to the underlying memory allocated by esmpy.

        *REQUIRED:*

        :param int coord_dim: the dimension number of the coordinates to return:
            e.g. ``[x, y, z] = (0, 1, 2)``, or ``[lat, lon] = (0, 1)``

        *OPTIONAL:*

        :param MeshLoc meshloc: the :attr:`~esmpy.api.constants.MeshLoc` of the
            coordinates.  If ``None``, defaults to
            :attr:`~esmpy.api.constants.MeshLoc.NODE`.

        :return: A numpy array of coordinate values at the specified
            :attr:`~esmpy.api.constants.MeshLoc`.
        """

        ret = None
        assert(self.coords[meshloc][coord_dim] is not None)
        ret = self.coords[meshloc][coord_dim]

        return ret

    def _link_coords_(self):
        elemcoords = True

        # get the pointer to the underlying ESMF data array for coordinates
        coords_interleaved, num_nodes, num_dims = ESMP_MeshGetCoordPtr(self)
        try:
            coords_elem, num_elems, num_dims_e = ESMP_MeshGetElemCoordPtr(self)
            assert num_dims == num_dims_e
        except:
            elemcoords = False

        if not self.parametric_dim:
            self._parametric_dim = num_dims

        try:
            pass
            # TODO: removed connectivity because the hardcoded array allocation is
            #       eating up too much memory on some systems, this method can be
            #       added back in after mesh connectivity retrieval has been fixed the
            #       C interface
            # self._connectivity, self._nodes_per_elem = ESMP_MeshGetConnectivityPtr(self)
        except:
            warnings.warn("Mesh connectivity could not be read")

        # initialize the coordinates structures
        # index order is [meshloc][coord_dim]
        self._coords = [[None for a in range(num_dims)] \
                        for b in range(2)]

        # alias the coordinates to the mesh
        self._coords[node][0] = np.array([coords_interleaved[2*i] for i in range(num_nodes)])
        self._coords[node][1] = np.array([coords_interleaved[2*i+1] for i in range(num_nodes)])
        if num_dims == 3:
            self._coords[node][2] = np.array([coords_interleaved[2*i+2] for i in range(num_nodes)])

        if elemcoords:
            self._coords[element][0] = np.array([coords_elem[2 * i] for i in range(num_elems)])
            self._coords[element][1] = np.array([coords_elem[2 * i + 1] for i in range(num_elems)])
            if num_dims == 3:
                self._coords[element][2] = np.array([coords_elem[2 * i + 2] for i in range(num_elems)])

    def _write_(self, filename):
        """
        Write the :class:`~esmpy.api.mesh.Mesh` to a vtk formatted file.

        :param str filename: the name of the output file, .vtk will be appended.
        """

        # call into ctypes layer
        ESMP_MeshWrite(self, filename)
