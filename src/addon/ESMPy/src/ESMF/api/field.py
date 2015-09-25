# $Id$

"""
The Field API
"""

#### IMPORT LIBRARIES #########################################################

from copy import copy

from ESMF.api.grid import *
from ESMF.api.mesh import *
from ESMF.api.locstream import *
from ESMF.api.array import *
import ESMF.api.constants as constants

#### Field class ##############################################################
[node, element] = [0, 1]

class Field(object):
    """
    The Field class is a Python wrapper object for the ESMF Field.
    The individual values of all data arrays are referenced to those of the underlying Fortran ESMF object.

    A Field represents a physical field, such as temperature.   The Field class contains distributed and
    discretized field data, a reference to its associated grid, and metadata. The Field class stores the grid
    staggering for that physical field. This is the relationship of how the data array of a field maps onto a
    grid (e.g. one item per cell located at the cell center, one item per cell located at the NW corner,
    one item per cell vertex, etc.). This means that different Fields which are on the same underlying
    Grid but have different staggerings can share the same Grid object without needing to replicate it multiple times.

    For more information about the ESMF Field class, please see the `ESMF Field documentation
    <http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_refdoc/node5.html#SECTION05030000000000000000>`_.
    """
    @initialize
    def __init__(self, grid, name=None,
                typekind=None,
                staggerloc=None,
                meshloc=None,
                ndbounds=None):
        """
        Create a Field from a Grid, Mesh or LocStream. \n
        Required Arguments: \n
            grid: a Grid, Mesh or LocStream with coordinates allocated on
                  at least one stagger location. \n
        Optional Arguments: \n
            name: user friendly name for the Field. \n
            typekind: the type of the Field data. \n
                Argument values are: \n
                    TypeKind.I4 \n
                    TypeKind.I8 \n
                    TypeKind.R4 \n
                    (default) TypeKind.R8 \n
            staggerloc: the stagger location on which to locate the
                        Field data, only specify this argument when 
                        using a Grid. \n
                Argument values are: \n
                    2D: \n
                    (default) StaggerLoc.CENTER \n
                    StaggerLoc.EDGE1 \n
                    StaggerLoc.EDGE2 \n
                    StaggerLoc.CORNER \n
                    3D: \n
                    (default) StaggerLoc.CENTER_VCENTER \n
                    StaggerLoc.EDGE1_VCENTER \n
                    StaggerLoc.EDGE2_VCENTER \n
                    StaggerLoc.CORNER_VCENTER \n
                    StaggerLoc.CENTER_VFACE \n
                    StaggerLoc.EDGE1_VFACE \n
                    StaggerLoc.EDGE2_VFACE \n
            meshloc: the mesh location on which to locate the Field
                     data, only specify this argument when using a 
                     Mesh. \n
                Argument values are: \n
                    (default) MeshLoc.NODE \n
                    MeshLoc.ELEMENT \n
            ndbounds: the number of entries in an extra field dimension. This
                      is represented as a single value, a list or a tuple containing
                      a number of entries for each desired extra dimension of a field.\n
        Returns: \n
            Field \n
        """

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
        if ndbounds is None:
            local_ndbounds = ndbounds
        elif type(ndbounds) is list:
            local_ndbounds = ndbounds
        elif type(ndbounds) is tuple:
            local_ndbounds = list(ndbounds)
        else:
            local_ndbounds = [ndbounds]

        xd = 0
        if local_ndbounds:
            xd = len(local_ndbounds)
            lb = [1 for a in range(len(local_ndbounds))]
            ungridded_lower_bound = np.array(lb, dtype=np.int32)
            ungridded_upper_bound = np.array(local_ndbounds, dtype=np.int32)
            # set this to put gridded dimension in the last available dimensions of the field, dependent on grid rank
            grid_to_field_map = np.array([i+xd+1 for i in range(grid.rank)], dtype=np.int32)
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
        #TODO: MaskedArray gives better interpolation values than Array
        self._data = MaskedArray(ESMP_FieldGetPtr(struct), None, typekind, ubounds-lbounds).data
        self._name = name
        self._type = typekind
        self._rank = rank
        self._struct = struct
        self._xd = xd
        self._staggerloc = staggerloc
        self._lower_bounds = lbounds
        self._upper_bounds = ubounds
        self._ndbounds = local_ndbounds

        if not grid.singlestagger:
            self._grid = grid._preslice_(staggerloc)
        else:
            self._grid = grid

        # for arbitrary metadata
        self._meta = {}

        # register function with atexit
        import atexit
        atexit.register(self.__del__)
        self._finalized = False

    @property
    def struct(self):
        return self._struct

    @property
    def data(self):
        """
        :return: the data of the Field
        """
        return self._data

    @property
    def name(self):
        """
        :return: the name of the Field
        """
        return self._name

    @property
    def type(self):
        """
        :return: the type of the data in the Field
        """
        return self._type

    @property
    def rank(self):
        """
        :return: the rank of the Field
        """
        return self._rank

    @property
    def xd(self):
        """
        :return: the number of extra (ungridded) dimensions of the Field
        """
        return self._xd

    @property
    def staggerloc(self):
        """
        :return: the Grid staggerloc or Mesh meshloc upon which this Field is built
        """
        return self._staggerloc

    @property
    def lower_bounds(self):
        """
        :return: the lower bounds of the Field
        """
        return self._lower_bounds

    @property
    def upper_bounds(self):
        """
        :return: the upper bounds of the Field
        """
        return self._upper_bounds

    @property
    def ndbounds(self):
        """
        :return: the bounds of the extra dimensions in the Field
        """
        return self._ndbounds

    @property
    def grid(self):
        """
        :return: the Grid, Mesh or LocStream upon which this Field is built
        """
        return self._grid

    @property
    def meta(self):
        return self._meta

    @property
    def finalized(self):
        return self._finalized

    # manual destructor
    def destroy(self):
        """
        Release the memory associated with a Field. \n
        Required Arguments: \n
            None \n
        Optional Arguments: \n
            None \n
        Returns: \n
            None \n
        """
        if hasattr(self, '_finalized'):
            if self._finalized is False:
                ESMP_FieldDestroy(self)
                self._finalized = True

    def __del__(self):
        """
        Release the memory associated with a Field. \n
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

    def _copy_(self):
        # shallow copy
        ret = copy(self)
        # don't call ESMF destructor twice on the same shallow Python object
        ret._finalized = True

        return ret

    def __getitem__(self, slc):
        if pet_count() > 1:
            raise SerialMethod

        slc = get_formatted_slice(slc, self.rank)

        ret = self._copy_()

        ret._data = self._data.__getitem__(slc)

        # set grid to the last two dims of the slice
        if self.xd > 0:
            slc_grid = [slc[-1*x] for x in range(self.rank-self.xd, 0, -1)]
        else:
            slc_grid = slc
        ret._grid = self.grid._slice_onestagger_(slc_grid)

        # upper bounds are "sliced" by taking the shape of the data
        ret._upper_bounds = np.array(ret.data.shape, dtype=np.int32)
        # lower bounds do not need to be sliced yet because slicing is not yet enabled in parallel

        return ret

    def get_area(self):
        """
        Initialize a Field with the areas of the cells of the 
        underlying Grid or Mesh. \n
        Required Arguments: \n
            None \n
        Optional Arguments: \n
            None \n
        Returns: \n
            None
        """

        # call into the ctypes layer
        ESMP_FieldRegridGetArea(self)

    def read(self, filename, variable, ndbounds=None):
        """
        Read data into a Field from a NetCDF file. \n
        NOTE: This interface is not supported when ESMF is built with ESMF_COMM=mpiuni. \n
        Required Arguments: \n
            filename: the name of the NetCDF file. \n
            variable: the name of the data variable to read. \n
            timeslice: the number of time slices to read. \n
        Optional Arguments: \n
            format: unimplemented (defaults to NetCDF)\n
        Returns: \n
            Field \n
        """

        assert (type(filename) is str)
        assert (type(variable) is str)

        # format defaults to NetCDF for now
        format = 1

        # if ndbounds is not passed in, set it to the first of extra field dimensions, if they exist
        timeslice = 1
        if ndbounds is None:
            if self.ndbounds is not None:
                if type(self.ndbounds) is list:
                    timeslice = self.ndbounds[0]
                elif type(self.ndbounds) is int:
                    timeslice = self.ndbounds
        # if ndbounds is passed in, make sure it is a reasonable value
        else:
            if self.ndbounds is not None:
                if type(ndbounds) is not int:
                    raise ValueError("ndbounds argument can only be a single integer at this time")
                else:
                    timeslice_local = 1
                    if type(self.ndbounds) is list:
                        timeslice_local = self.ndbounds[0]
                    elif type(self.ndbounds) is int:
                        timeslice_local = self.ndbounds
                    assert (ndbounds <= timeslice_local)
                    timeslice = ndbounds

        ESMP_FieldRead(self, filename=filename,
                       variablename=variable,
                       timeslice=timeslice,
                       iofmt=format)
