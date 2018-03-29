# $Id$

"""
The Field API
"""

#### IMPORT LIBRARIES #########################################################

from ESMF.api.grid import *
from ESMF.api.mesh import *
from ESMF.api.locstream import *
from ESMF.util.esmpyarray import *

#### Field class ##############################################################
[node, element] = [0, 1]

class Field(object):
    """
    The :class:`~ESMF.api.field.Field` class is a Python wrapper object for the ESMF Field.
    The individual values of all data arrays are referenced to those of the
    underlying Fortran ESMF object.

    A :class:`~ESMF.api.field.Field` represents a physical field, such as temperature.   The :class:`~ESMF.api.field.Field` class
    contains distributed and discretized field data, a reference to its
    associated grid, and metadata. The :class:`~ESMF.api.field.Field` class stores the grid staggering
    for that physical field. This is the relationship of how the data array of
    a field maps onto a grid (e.g. one item per cell located at the cell center,
    one item per cell located at the NW corner, one item per cell vertex, etc.).
    This means that different :class:`Fields <ESMF.api.field.Field>` which are on the same underlying :class:`~ESMF.api.grid.Grid` but
    have different staggerings can share the same :class:`~ESMF.api.grid.Grid` object without needing to
    replicate it multiple times.

    For more information about the ESMF Field class, please see the `ESMF Field documentation
    <http://www.earthsystemmodeling.org/esmf_releases/public/ESMF_7_1_0r/ESMF_refdoc/node5.html#SECTION05030000000000000000>`_.

    The following parameters are used to create a :class:`~ESMF.api.field.Field`
    from a :class:`~ESMF.api.grid.Grid`, :class:`~ESMF.api.mesh.Mesh` or
    :class:`~ESMF.api.locstream.LocStream`.

    *REQUIRED:*

    :param :class:`~ESMF.api.grid.Grid`/Mesh/:class:`~ESMF.api.locstream.LocStream` grid: A :class:`~ESMF.api.grid.Grid`,
        :class:`~ESMF.api.mesh.Mesh` or :class:`~ESMF.api.locstream.LocStream`
        with coordinates allocated on at least one stagger location.

    *OPTIONAL:*

    :param str name: An optional user friendly name for the
        :class:`~ESMF.api.field.Field`.
    :param TypeKind typekind: Type of the :class:`~ESMF.api.field.Field`
        data. If ``None``, defaults to :attr:`~ESMF.api.constants.TypeKind.R8`.
    :param StaggerLoc staggerloc: The stagger location of the
        :class:`~ESMF.api.field.Field` data, only specify this argument when
        using a :class:`~ESMF.api.grid.Grid`.
        If ``None``, defaults to :attr:`~ESMF.api.constants.StaggerLoc.CENTER`
        in 2D and :attr:`~ESMF.api.constants.StaggerLoc.CENTER_VCENTER` in 3D.
    :param MeshLoc meshloc: The mesh location of the
        :class:`~ESMF.api.field.Field` data, only specify this argument when
        using a :class:`~ESMF.api.mesh.Mesh`
        if ``None``, defaults to :attr:`~ESMF.api.constants.MeshLoc.NODE`.
    :param tuple ndbounds: The number of entries in an extra
        :class:`~ESMF.api.field.Field` dimension. This is represented as a
        single value, a list or a tuple containing the number of entries for
        each desired extra dimension of the :class:`~ESMF.api.field.Field`. The
        time dimension must be last, following Fortran indexing conventions.
    """

    @initialize
    def __init__(self, grid, name=None,
                typekind=None,
                staggerloc=None,
                meshloc=None,
                ndbounds=None):
        # optional arguments
        if staggerloc is None:
            staggerloc = StaggerLoc.CENTER
        if typekind is None:
            typekind = TypeKind.R8
        if meshloc is None:
            meshloc = MeshLoc.NODE

        # extra levels?
        grid_to_field_map = None
        ungridded_lower_bound = None
        ungridded_upper_bound = None
        rank = grid.rank
        local_ndbounds = ndbounds
        try:
            local_ndbounds = ndbounds.tolist()
        except AttributeError:
            if ndbounds is not None:
                local_ndbounds = list(ndbounds)

        # TODO: flip ndbounds
        #     also, will have to verify that everything is switched back

        xd = 0
        if local_ndbounds:
            xd = len(local_ndbounds)
            lb = [1 for a in range(len(local_ndbounds))]
            ungridded_lower_bound = np.array(lb, dtype=np.int32)
            ungridded_upper_bound = np.array(local_ndbounds, dtype=np.int32)
            # set this to put gridded dimension in the first available dimensions of the field, dependent on grid rank
            grid_to_field_map = np.array([i+1 for i in range(grid.rank)], dtype=np.int32)
            rank += len(local_ndbounds)

        if isinstance(grid, Grid):
            # call into ctypes layer
            struct = ESMP_FieldCreateGrid(grid, name, typekind, staggerloc,
                                          grid_to_field_map,
                                          ungridded_lower_bound,
                                          ungridded_upper_bound)

        elif isinstance(grid, Mesh):
            # deal with the wacky ESMF node/element convention
            if meshloc == MeshLoc.NODE:
                staggerloc = node
            elif meshloc == MeshLoc.ELEMENT:
                staggerloc = element
            else:
                raise MeshLocationNotSupported

            # call into ctypes layer
            struct = ESMP_FieldCreateMesh(grid, name, typekind, meshloc,
                                      grid_to_field_map,
                                      ungridded_lower_bound,
                                      ungridded_upper_bound)

        elif isinstance(grid, LocStream):
            # call into ctypes layer
            struct = ESMP_FieldCreateLocStream(grid, name, typekind,
                                      grid_to_field_map,
                                      ungridded_lower_bound,
                                      ungridded_upper_bound)

        else:
            raise FieldDOError

        # get data and bounds to create a new instance of this object
        lbounds, ubounds = ESMP_FieldGetBounds(struct, rank)

        # initialize field data
        # TODO: MaskedArray gives better interpolation values than Array (171128 removed .copy from .data below
        # self._data = MaskedArray(ESMP_FieldGetPtr(struct), None, typekind, ubounds-lbounds).data
        self._data = ndarray_from_esmf(ESMP_FieldGetPtr(struct), typekind, ubounds-lbounds)
        self._name = name
        self._type = typekind
        self._rank = rank
        self._struct = struct
        self._xd = xd
        self._staggerloc = staggerloc
        self._lower_bounds = lbounds
        self._upper_bounds = ubounds
        self._ndbounds = local_ndbounds

        self._grid = grid

        # for arbitrary metadata
        self._meta = {}

        # register function with atexit
        import atexit
        atexit.register(self.__del__)
        self._finalized = False

    def __del__(self):
        self.destroy()

    def __getitem__(self, slc):
        if pet_count() > 1:
            raise SerialMethod

        slc = get_formatted_slice(slc, self.rank)

        ret = self.copy()

        ret._data = self._data.__getitem__(slc)

        # set grid to the first two dims of the slice (this will change to last when we get dimension ordering set to python conventions)
        if self.xd > 0:
            # slc_grid = [slc[-1 * x] for x in range(self.rank - self.xd, 0, -1)]
            slc_grid = [slc[x] for x in range(self.rank - self.xd)]
        else:
            slc_grid = slc
        ret._grid = self.grid.__getitem__(slc_grid)

        # upper bounds are "sliced" by taking the shape of the data
        ret._upper_bounds = np.array(ret.data.shape, dtype=np.int32)
        # lower bounds do not need to be sliced yet because slicing is not yet enabled in parallel

        return ret

    def __repr__(self):
        string = ("Field:\n"
                  "    name = %r\n"
                  "    type = %r\n"
                  "    rank = %r\n"
                  "    extra dimensions = %r\n"
                  "    staggerloc = %r\n"
                  "    lower bounds = %r\n"
                  "    upper bounds = %r\n"
                  "    extra bounds = %r\n"
                  "    data = %r\n"
                  "    grid = \n%r\n"
                  ")"
                  %
                  (self.name,
                   self.type,
                   self.rank,
                   self.xd,
                   self.staggerloc,
                   self.lower_bounds,
                   self.upper_bounds,
                   self.ndbounds,
                   self.data,
                   self.grid,
                   ))

        return string

    @property
    def data(self):
        """
        :rtype: :attr:`~ESMF.api.constants.TypeKind`
        :return: The data of the :class:`~ESMF.api.field.Field`
        """
        return self._data

    @property
    def finalized(self):
        """
        :rtype: bool
        :return: Indicate if the underlying ESMF memory for this object has
            been deallocated.
        """

        return self._finalized

    @property
    def grid(self):
        """
        :rtype: :class:`~ESMF.api.grid.Grid`, :class:`~ESMF.api.mesh.Mesh`, or
            :class:`~ESMF.api.locstream.LocStream`
        :return: The discretization object upon which the
            :class:`~ESMF.api.field.Field` is built.
        """
        return self._grid

    @property
    def lower_bounds(self):
        """
        :rtype: ndarray
        :return: The lower bounds of the :class:`~ESMF.api.field.Field`.
        """
        return self._lower_bounds

    @property
    def meta(self):
        """
        :rtype: tdk
        :return: tdk
        """
        return self._meta

    @property
    def name(self):
        """
        :rtype: str
        :return: the name of the :class:`~ESMF.api.field.Field`.
        """
        return self._name

    @property
    def ndbounds(self):
        """
        :rtype: list
        :return: The bounds of the extra dimensions in the
            :class:`~ESMF.api.field.Field`.
        """
        return self._ndbounds

    @property
    def rank(self):
        """
        :rtype: int
        :return: The rank of the :class:`~ESMF.api.field.Field`.
        """
        return self._rank

    @property
    def staggerloc(self):
        """
        :rtype: :attr:`~ESMF.api.constants.StaggerLoc` or
            :attr:`~ESMF.api.constants.MeshLoc`
        :return: The location upon which the :class:`~ESMF.api.field.Field`
            is built.
        """
        return self._staggerloc

    @property
    def struct(self):
        """
        :rtype: pointer
        :return: A pointer to the underlying ESMF allocation for this
            :class:`~ESMF.api.field.Field`.
        """
        return self._struct

    @property
    def type(self):
        """
        :rtype: :attr:`~ESMF.api.constants.TypeKind`
        :return: The type of the data in the :class:`~ESMF.api.field.Field`.
        """
        return self._type

    @property
    def upper_bounds(self):
        """
        :rtype: ndarray
        :return: The upper bounds of the :class:`~ESMF.api.field.Field`.
        """
        return self._upper_bounds

    @property
    def xd(self):
        """
        :rtype: int
        :return: The number of extra (ungridded) dimensions of the
            :class:`~ESMF.api.field.Field`.
        """
        return self._xd

    def copy(self):
        """
        Copy a :class:`~ESMF.api.field.Field` in an ESMF-safe manner.

        :return: A :class:`~ESMF.api.field.Field` shallow copy.
        """
        # shallow copy
        ret = copy(self)
        # don't call ESMF destructor twice on the same shallow Python object
        ret._finalized = True

        return ret

    def destroy(self):
        """
        Release the memory associated with a :class:`~ESMF.api.field.Field`.
        """
        if hasattr(self, '_finalized'):
            if self._finalized is False:
                ESMP_FieldDestroy(self)
                self._finalized = True

    def get_area(self):
        """
        Initialize an existing :class:`~ESMF.api.field.Field` with the areas of
        the cells of the underlying :class:`~ESMF.api.grid.Grid` or
        :class:`~ESMF.api.mesh.Mesh`.
        """

        # call into the ctypes layer
        ESMP_FieldRegridGetArea(self)

    def read(self, filename, variable, timeslice=None):
        """
        Read data into an existing :class:`~ESMF.api.field.Field` from a
        CF-compliant NetCDF file.

        :note: This interface is not supported when ESMF is built with
            ``ESMF_COMM=mpiuni``.

        :note: This interface does not currently support reading ungridded 
               dimensions.

        *REQUIRED:*

        :param str filename: The name of the NetCDF file.
        :param str variable: The name of the data variable to read from file.

        *OPTIONAL:*

        :param list timeslice: The number of timeslices to read.
        """

        import ESMF.api.constants as constants
        if constants._ESMF_COMM is constants._ESMF_COMM_MPIUNI:
            raise ImportError("Field.Read() requires PIO and does not work if ESMF has not been built with MPI support")

        assert (type(filename) is str)
        assert (type(variable) is str)

        # format defaults to NetCDF for now
        format = 1

        if not isinstance(timeslice, int):
            raise TypeError("timeslice must be a single integer value")

        local_timeslice = None
        if timeslice == None:
          local_timeslice = 1
        else:
          local_timeslice = timeslice

        ESMP_FieldRead(self, filename=filename,
                       variablename=variable,
                       timeslice=local_timeslice,
                       iofmt=format)
