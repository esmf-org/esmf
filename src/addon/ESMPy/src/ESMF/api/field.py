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

class Field(MaskedArray):

    @initialize
    def __new__(cls, grid, name=None,
                typekind=None,
                staggerloc=None,
                meshloc=None,
                ndbounds=None,
                mask_values=None):
        """
        Create a Field from a Grid or Mesh. \n
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
            mask_values: Python list of integer values to use for masking. \n
                type: Python list \n
                shape: [grid.shape, 1] \n
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

        data = None
        mask = None
        if isinstance(grid, Grid):
            # call into ctypes layer
            struct = ESMP_FieldCreateGrid(grid, name, typekind, staggerloc,
                                          grid_to_field_map,
                                          ungridded_lower_bound,
                                          ungridded_upper_bound)

            # set the grid mask
            domask = False
            if grid._singlestagger:
                if (grid.mask is not None):
                    domask = True
            else:
                if (grid.mask[staggerloc] is not None):
                    domask = True

            if domask:
                lbounds, ubounds = ESMP_FieldGetBounds(struct, rank)
                # verify that grid mask is the same shape as the extra field dimensions
                # if (np.all(grid.mask[staggerloc].shape == ubounds[xd:xd + rank] - lbounds[xd:xd + rank])):
                # initialize the mask to all unmasked values
                mask = np.ones(ubounds - lbounds, dtype=np.int32)
                # reset the mask with integer values according to what is in the grid mask,
                # taking care to propagate masked values through the extra field dimensions
                if grid.rank == 2:
                    mask[..., :, :] = grid.mask[staggerloc]
                elif grid.rank == 3:
                    mask[..., :, :, :] = grid.mask[staggerloc]
                else:
                    raise IndexError("Grid with rank less than 2 or greater than 3 is not allowed")
                # else:
                #     raise IndexError(
                #         "grid mask bounds do not match field bounds: grid.mask.shape != ubounds - lbounds (" \
                #         + str(grid.mask[staggerloc].shape) + " != "
                #         + str(ubounds[xd:xd + rank] - lbounds[xd:xd + rank]) + ")")

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

            # No masking on a Mesh
            mask = None
        elif isinstance(grid, LocStream):
            # call into ctypes layer
            struct = ESMP_FieldCreateLocStream(grid, name, typekind,
                                      grid_to_field_map,
                                      ungridded_lower_bound,
                                      ungridded_upper_bound)

            if "ESMF:Mask" in grid:
                mask = grid["ESMF:Mask"]
        else:
            raise FieldDOError

        # get data and bounds to create a new instance of this object
        data = ESMP_FieldGetPtr(struct)
        lbounds, ubounds = ESMP_FieldGetBounds(struct, rank)

        # set field_mask based on the grid mask and the mask_values input argument
        if mask is None:
            size = reduce(mul, ubounds - lbounds)
            mask = [False] * size
        else:
            # handle mask values
            if mask_values is None:
                maskvals = np.array([])
            else:
                maskvals = mask_values
            # set mask
            mask = [True if x in maskvals else False for x in mask.flatten().tolist()]

        obj = super(Field, cls).__new__(cls, data, mask=mask,
                                           dtype=typekind, shape=ubounds-lbounds)

        # initialize field data
        obj._name = name
        obj._type = typekind
        obj._rank = rank
        obj._struct = struct
        obj._xd = xd
        obj._staggerloc = staggerloc
        obj._lower_bounds = lbounds
        obj._upper_bounds = ubounds
        obj._ndbounds = local_ndbounds
        if not grid.singlestagger:
            obj._grid = grid._preslice_(staggerloc)
        else:
            obj._grid = grid

        # for arbitrary metadata
        obj._meta = {}

        # register function with atexit
        import atexit
        atexit.register(obj.__del__)
        obj._finalized = False

        # load all metadata into _optinfo to appease np.MaskedArray
        obj._optinfo["_name"] = obj._name
        obj._optinfo["_type"] = obj._type
        obj._optinfo["_rank"] = obj._rank
        obj._optinfo["_struct"] = obj._struct
        obj._optinfo["_xd"] = obj._xd
        obj._optinfo["_staggerloc"] = obj._staggerloc
        obj._optinfo["_lower_bounds"] = obj._lower_bounds
        obj._optinfo["_upper_bounds"] = obj._upper_bounds
        obj._optinfo["_ndbounds"] = obj._ndbounds
        obj._optinfo["_grid"] = obj._grid
        obj._optinfo["_meta"] = obj._meta
        obj._optinfo["_finalized"] = obj._finalized

        return obj

    def __array_finalize__(self, obj):
        if obj is None: return
        super(Field, self).__array_finalize__(obj)
        self._name = getattr(obj, 'name', None)
        self._type = getattr(obj, 'type', None)
        self._rank = getattr(obj, 'rank', None)
        self._struct = getattr(obj, 'struct', None)
        self._xd = getattr(obj, 'xd', None)
        self._staggerloc = getattr(obj, 'staggerloc', None)
        self._lower_bounds = getattr(obj, 'lower_bounds', None)
        self._upper_bounds = getattr(obj, 'upper_bounds', None)
        self._ndbounds = getattr(obj, 'ndbounds', None)
        self._grid = getattr(obj, 'grid', None)
        self._meta = getattr(obj, 'meta', None)

    def flatten(self, *args, **kwargs):
        flattened = super(Field, self).flatten(*args, **kwargs)
        flattened._finalized = True
        return flattened

    def reshape(self, *args, **kwargs):
        reshaped = super(Field, self).reshape(*args, **kwargs)
        reshaped._finalized = True
        return reshaped

    def transpose(self, *args, **kwargs):
        transposed = super(Field, self).transpose(*args, **kwargs)
        transposed._finalized = True
        return transposed

    @property
    def struct(self):
        return self._struct

    @property
    def name(self):
        return self._name

    @property
    def type(self):
        return self._type

    @property
    def rank(self):
        return self._rank

    @property
    def xd(self):
        return self._xd

    @property
    def staggerloc(self):
        return self._staggerloc

    @property
    def lower_bounds(self):
        return self._lower_bounds

    @property
    def upper_bounds(self):
        return self._upper_bounds

    @property
    def ndbounds(self):
        return self._ndbounds

    @property
    def grid(self):
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
                  "    grid = \n%r\n"
                  "    mask = %r\n"
                  "    data = %r\n"
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
                   self.grid,
                   self.mask,
                   self.data
                  ))

        return string

    def _copy_(self):
        # shallow copy
        ret = copy(self)
        # don't call ESMF destructor twice on the same shallow Python object
        ret._finalized = True

        return ret

    def _merge_(self, obj):
        # initialize field data
        obj._struct = self.struct
        obj._name = self.name
        obj._rank = self.rank
        obj._xd = self.xd
        obj._type = self.type
        obj._staggerloc = self.staggerloc
        obj._lower_bounds = self.lower_bounds
        obj._upper_bounds = self.upper_bounds
        obj._ndbounds = self.ndbounds
        obj._meta = self.meta

        # set slice to be finalized so it doesn't call the ESMF garbage collector
        obj._finalized = True

        return obj

    def __getitem__(self, slc):
        if pet_count() > 1:
            raise SerialMethod

        slc_field = get_formatted_slice(slc, self.rank)
        ret = super(Field, self).__getitem__(slc_field)

        # have to "merge" ret and self
        ret = self._merge_(ret)

        # set grid to the last two dims of the slice
        if self.xd > 0:
            slc_grid = [slc_field[-1*x] for x in range(self.rank-self.xd, 0, -1)]
        else:
            slc_grid = slc_field
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
