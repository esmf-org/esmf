# $Id$

"""
The Field API
"""

#### IMPORT LIBRARIES #########################################################

from copy import copy

from ESMF.api.grid import *
from ESMF.api.mesh import *
from ESMF.api.array import *
import ESMF.api.constants as constants

#### Field class ##############################################################
[node, element] = [0, 1]

class Field(MaskedArray):

    @initialize
    def __new__(cls, grid, name,
                typekind=None,
                staggerloc=None,
                meshloc=None,
                ndbounds=None,
                mask_values=None):
        """
        Create a Field from a Grid or Mesh. \n
        Required Arguments: \n
            grid: either a Grid or a Mesh with coordinates allocated on
                  at least one stagger location. \n
            name: user friendly name for the Field. \n
        Optional Arguments: \n
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
            levels: the number of vertical layers on an extra field dimension. \n
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
            if (grid.item_done[staggerloc][GridItem.MASK]):
                lbounds, ubounds = ESMP_FieldGetBounds(struct, rank)
                # verify that grid mask is the same shape as the extra field dimensions
                if (np.all(grid.mask[staggerloc].shape == ubounds[xd:xd + rank] - lbounds[xd:xd + rank])):
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
                else:
                    raise IndexError("grid mask bounds do not match field bounds")

        elif isinstance(grid, Mesh):
            # deal with the wacky ESMF node/element convention
            if meshloc == MeshLoc.NODE:
                staggerloc = node
            elif meshloc == MeshLoc.ELEMENT:
                staggerloc = element
            else:
                raise MeshLocationNotSupported

            # call into ctypes layer
            struct = ESMP_FieldCreate(grid, name, typekind, meshloc,
                                      grid_to_field_map,
                                      ungridded_lower_bound,
                                      ungridded_upper_bound)

            # No masking on a Mesh
            mask = None
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
        obj.struct = struct
        obj.rank = rank
        obj.xd = xd
        obj.type = typekind
        obj.staggerloc = staggerloc
        obj.lower_bounds = lbounds
        obj.upper_bounds = ubounds
        obj.ndbounds = local_ndbounds
        obj.grid = grid._preslice_(staggerloc)
        obj.name = name

        # for arbitrary attributes
        obj.meta = {}

        # register function with atexit
        import atexit
        atexit.register(obj.__del__)
        obj._finalized = False

        return obj

    # destructor
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

        if hasattr(self,'_finalized'):
            if self._finalized is False:
                ESMP_FieldDestroy(self)
                self._finalized = True

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
        obj.struct = self.struct
        obj.name = self.name
        obj.rank = self.rank
        obj.xd = self.xd
        obj.type = self.type
        obj.staggerloc = self.staggerloc
        obj.lower_bounds = self.lower_bounds
        obj.upper_bounds = self.upper_bounds
        obj.ndbounds = self.ndbounds
        obj.meta = self.meta

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
        ret.grid = self.grid._slice_onestagger_(slc_grid)

        # upper bounds are "sliced" by taking the shape of the data
        ret.upper_bounds = np.array(ret.data.shape, dtype=np.int32)
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
            None \n
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
            format: unimplemented (defaults to NetCDF)
        Returns: \n
            Field \n
        """
        assert (type(filename) is str)
        assert (type(variable) is str)

        # format defaults to NetCDF for now
        format = 1

        # if ndbounds is not passed in, set it to the first of extra field dimenstions, if they exist
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
