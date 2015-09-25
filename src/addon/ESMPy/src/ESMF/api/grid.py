# $Id$

"""
The Grid API
"""

#### IMPORT LIBRARIES #########################################################

import warnings
from copy import copy

from ESMF.api.esmpymanager import *
from ESMF.api.array import *
import ESMF.api.constants as constants
from ESMF.util.slicing import get_formatted_slice, get_none_or_slice, get_none_or_bound, get_none_or_ssslice


#### Grid class #########################################################

class Grid(object):
    """
    The Grid class is a Python wrapper object for the ESMF Grid.
    The individual values of all coordinate and mask arrays are referenced to those of the
    underlying Fortran ESMF object.

    The Grid class is used to describe the geometry and discretization of logically rectangular physical grids.
    It also contains the description of the grid's underlying topology and the decomposition of the physical grid
    across the available computational resources. The most frequent use of the Grid class is to describe physical
    grids in user code so that sufficient information is available to perform regridding operations.

    For more information about the ESMF Grid class, please see the `ESMF Grid documentation
    <http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_refdoc/node5.html#SECTION05080000000000000000>`_.
    """
    @initialize
    def __init__(self, max_index=None,
                 num_peri_dims=0,
                 periodic_dim=None,
                 pole_dim=None,
                 coord_sys=None,
                 coord_typekind=None,
                 staggerloc=None,
                 filename=None,
                 filetype=None,
                 reg_decomp=None,
                 decompflag=None,
                 is_sphere=None,
                 add_corner_stagger=None,
                 add_user_area=None,
                 add_mask=None,
                 varname=None,
                 coord_names=None):
        """
        Create a logically rectangular Grid object and optionally 
        allocate space for coordinates at a specified stagger location. 
        A grid can be created in memory or from file, there are different
        arguments required for each method, outlined below. \n
        The following arguments apply to a Grid created in memory: \n
            Required arguments: \n
                max_index: a numpy array which specifies the maximum
                           index of each dimension of the Grid. \n
                    type: np.array \n
                    shape: [number of dimensions, 1] \n
            Optional arguments: \n
                num_peri_dims: the number of periodic dimensions (0 or 1). \n
                periodic_dim: the periodic dimension (defaults to 1). \n
                pole_dim: the pole dimension (defaults to 2). \n
                coord_sys: the coordinates system for the Grid. \n
                    Argument values are:\n
                        CoordSys.CART\n
                        (default) CoordSys.SPH_DEG\n
                        CoordSys.SPH_RAD\n
                coord_typekind: the type of the Grid coordinates. \n
                    Argument values are: \n
                        TypeKind.I4\n
                        TypeKind.I8\n
                        TypeKind.R4\n
                        (default) TypeKind.R8\n
        The following arguments apply to a Grid created either from file
        or in memory. \n
            Optional arguments: \n
                staggerloc: the stagger location of the coordinate data. \n
                    Argument values are: \n
                        2D: \n
                        (default) StaggerLoc.CENTER\n
                        StaggerLoc.EDGE1\n
                        StaggerLoc.EDGE2\n
                        StaggerLoc.CORNER\n
                        3D: \n
                        (default) StaggerLoc.CENTER_VCENTER\n
                        StaggerLoc.EDGE1_VCENTER\n
                        StaggerLoc.EDGE2_VCENTER\n
                        StaggerLoc.CORNER_VCENTER\n
                        StaggerLoc.CENTER_VFACE\n
                        StaggerLoc.EDGE1_VFACE\n
                        StaggerLoc.EDGE2_VFACE\n
                        StaggerLoc.CORNER_VFACE\n
        The following argument applies only to a Grid created from file: \n
            Required arguments: \n
                filename: the name of NetCDF file containing the Grid. \n
                filetype: the input file type of the Grid. \n
                    Argument values are: \n
                        FileFormat.SCRIP \n
                        FileFormat.GRIDSPEC \n
            Optional arguments: \n
                reg_decomp: A 2 element integer list specifying how the grid
                            is decomposed.  Each entry is the number of decounts
                            for that dimension.  The total decounts cannot
                            exceed the total number of PETs.  In other words,
                            at most one DE is allowed per processor. \n
                decompflag: List of decomposition flags indicating how each
                            dimension of the tile is to be divided between the
                            DEs. The default setting is BALANCED in all
                            dimensions. \n
                is_sphere: Set to True for a spherical grid, or False for
                           regional. Defaults to True. \n
                add_corner_stagger: Set to True to use the information in the
                                    grid file to add the corner stagger to the
                                    Grid. The coordinates for the corner stagger
                                    are required for conservative regridding. If 
                                    not specified, defaults to False. \n
                add_user_area: Set to True to read in the cell area from the Grid 
                               file; otherwise, ESMF will calculate it. 
                               Defaults to False. \n
                add_mask: Set to True to generate the mask using the missing_value 
                          attribute defined in 'varname'.  This 
                          argument is only supported with filetype 
                          FileFormat.GRIDSPEC.  Defaults to False. \n
                varname: If add_mask is True, provide a variable name stored in 
                         the grid file and the mask will be generated using the 
                         missing value of the data value of this variable.  The 
                         first two dimensions of the variable has to be the 
                         longitude and the latitude dimension and the mask is 
                         derived from the first 2D values of this variable even 
                         if this data is a 3D, or 4D array.  This 
                         argument is only supported with filetype 
                         FileFormat.GRIDSPEC.  Defaults to None. \n
                coord_names:  A two-element array containing the longitude and 
                             latitude variable names in a GRIDSPEC file if there 
                             are multiple coordinates defined in the file. This 
                             argument is only supported with filetype 
                             FileFormat.GRIDSPEC.  Defaults to None. \n
        Returns: \n
            Grid \n
        """
        from operator import mul

        # initialize the from_file flag to False
        from_file = False
        
        # in-memory grid
        if max_index is not None:
            # cast max_index if not already done
            if max_index.dtype is not np.int32:
                self._max_index = np.array(max_index, dtype=np.int32)
            else:
                self._max_index = max_index
            # raise warnings on all from file args
            if filename is not None:
                warnings.warn("filename is only used for grids created from file, this argument will be ignored.")
            if filetype is not None:
                warnings.warn("filetype is only used for grids created from file, this argument will be ignored.")
            if reg_decomp is not None:
                warnings.warn("reg_decomp is only used for grids created from file, this argument will be ignored.")
            if decompflag is not None:
                warnings.warn("decompflag is only used for grids created from file, this argument will be ignored.")
            if is_sphere is not None:
                warnings.warn("is_sphere is only used for grids created from file, this argument will be ignored.")
            if add_corner_stagger is not None:
                warnings.warn("add_corner_stagger is only used for grids created from file, this argument will be ignored.")
            if add_user_area is not None:
                warnings.warn("add_user_area is only used for grids created from file, this argument will be ignored.")
            if add_mask is not None:
                warnings.warn("add_mask is only used for grids created from file, this argument will be ignored.")
            if varname is not None:
                warnings.warn("varname is only used for grids created from file, this argument will be ignored.")
            if coord_names:
                warnings.warn("coord_names is only used for grids created from file, this argument will be ignored.")
        # filename and filetype are required for from-file grids
        elif (filename is None) or (filetype is None):
            # raise error, need max_index to create in memory or filename to create from file
            raise GridArgumentError("must supply either max_index for an in-memory grid or filename and filetype for a from-file grid")
        # from file
        else:
            if (filetype != FileFormat.SCRIP) and (filetype != FileFormat.GRIDSPEC):
                raise GridArgumentError("filetype must be SCRIP or GRIDSPEC for Grid objects")
            # set the from_file flag to True
            from_file = True
            #raise errors for all in-memory grid options
            if max_index is not None:
                warnings.warn("max_index is only used for grids created in memory, this argument will be ignored.")
            if num_peri_dims is not 0:
                warnings.warn("num_peri_dims is only used for grids created in memory, this argument will be ignored.")
            if periodic_dim is not None:
                warnings.warn("periodic_dim is only used for grids created in memory, this argument will be ignored.")
            if pole_dim is not None:
                warnings.warn("pole_dim is only used for grids created in memory, this argument will be ignored.")
            if coord_sys is not None:
                warnings.warn("coord_sys is only used for grids created in memory, this argument will be ignored.")
            if coord_typekind is not None:
                warnings.warn("coord_typekind is only used for grids created in memory, this argument will be ignored.")
            if staggerloc is not None:
                warnings.warn("staggerloc is only used for grids created in memory, this argument will be ignored.")

        # ctypes stuff
        self._struct = None

        # for ocgis compatibility
        self._ocgis = {}

        # type, kind, rank, etc.
        self._type = TypeKind.R8
        self._areatype = TypeKind.R8
        self._rank = None
        self._periodic_dim = periodic_dim
        self._pole_dim = pole_dim
        self._coord_sys = coord_sys
        self._ndims = None # Applies to Gridspec only

        if num_peri_dims is None:
            self._num_peri_dims = 0
        else:
            self._num_peri_dims = num_peri_dims

        # size, type and rank of the grid for bookeeping of coordinates 
        self._size = [None]

        # public facing

        # placeholder for staggerlocs, True if present, False if not
        self._staggerloc = [None]

        # placeholder for the list of numpy arrays which holds the grid bounds
        self._lower_bounds = [None]
        self._upper_bounds = [None]

        # placeholder for the list of numpy arrays which hold the grid coords
        self._coords = [None]

        # mask and area
        self._mask = np.zeros(None)
        self._area = np.zeros(None)

        # create the correct grid
        self._struct = None

        if from_file:
            # create default reg_decomp if it is not passed as an argument
            if reg_decomp is None:
                reg_decomp = [pet_count(), 1]
            # create the grid from file
            self._struct = ESMP_GridCreateFromFile(filename, filetype,
                                                  reg_decomp,
                                                  decompflag=decompflag,
                                                  isSphere=is_sphere,
                                                  addCornerStagger=add_corner_stagger,
                                                  addUserArea=add_user_area,
                                                  addMask=add_mask, 
                                                  varname=varname,
                                                  coordNames=coord_names)
            # grid rank and dims
            if filetype == FileFormat.SCRIP:
                self._rank, self._max_index = ESMP_ScripInq(filename)
                self._ndims = self.rank
            else: # must be GRIDSPEC
                self._rank, self._ndims, self._max_index = ESMP_GridspecInq(filename)
            # stagger is not required for from-file grids, but we need to
            # correctly allocate the space
            staggerloc = [StaggerLoc.CENTER]

            # add corner, this assumes 2D grids right?
            if add_corner_stagger:
                if StaggerLoc.CORNER not in staggerloc:
                    staggerloc.append(StaggerLoc.CORNER)
            
            # set the num_peri_dims so sizes are calculated correctly
            # is_sphere defaults to True
            if is_sphere == False:
                self._num_peri_dims = 0
            else:
                self._num_peri_dims = 1
                self._periodic_dim = 0
                # TODO: we assume that all periodic grids create from file will be periodic across the first
                #       dimension.. is that true?
            
        else:
            # ctypes stuff
            self._struct = ESMP_GridStruct()
            if self.num_peri_dims == 0:
                self._struct = ESMP_GridCreateNoPeriDim(self.max_index,
                                                       coordSys=coord_sys,
                                                       coordTypeKind=coord_typekind)
            elif (self.num_peri_dims == 1):
                self._struct = ESMP_GridCreate1PeriDim(self.max_index,
                                                      periodicDim=periodic_dim,
                                                      poleDim=pole_dim,
                                                      coordSys=coord_sys,
                                                      coordTypeKind=coord_typekind)
                if periodic_dim == None:
                    self._periodic_dim = 0
                    self._pole_dim = 1
            else:
                raise TypeError("Number of periodic dimensions should be 0 or 1")

            # grid rank
            self._rank = self.max_index.size

        # grid type
        if coord_typekind is None:
            self._type = TypeKind.R8
        else:
            self._type = coord_typekind

        # staggerloc
        self._staggerloc = [False for a in range(2**self.rank)]

        # bounds
        self._lower_bounds = [None for a in range(2**self.rank)]
        self._upper_bounds = [None for a in range(2**self.rank)]

        # distributed sizes
        self._size = [None for a in range(2**self.rank)]

        # initialize the coordinates structures
        # index order is [staggerLoc][coord_dim]
        self._coords = [[None for a in range(self.rank)] \
                        for b in range(2**self.rank)]

        # initialize the item structures
        # index order is [staggerLoc][itemDim]
        self._mask = [None for a in range(2**self.rank)]
        self._area = [None for a in range(2**self.rank)]

        # Add coordinates if a staggerloc is specified
        if staggerloc is not None:
            self.add_coords(staggerloc=staggerloc, from_file=from_file)

        # Add items if they are specified, this is done after the
        # mask and area are initialized
        if add_user_area:
            self.add_item(GridItem.AREA, staggerloc=StaggerLoc.CENTER, 
                          from_file=from_file)
        if add_mask:
            self.add_item(GridItem.MASK, staggerloc=StaggerLoc.CENTER, 
                          from_file=from_file)

        # for arbitrary metadata
        self._meta = {}

        # regist with atexit
        import atexit; atexit.register(self.__del__)
        self._finalized = False

        # set the single stagger flag
        self._singlestagger = False

    @property
    def struct(self):
        return self._struct

    @property
    def max_index(self):
        return self._max_index

    @property
    def type(self):
        """
        :return: the ESMF typekind of the Grid coordinates
        """
        return self._type

    @property
    def areatype(self):
        """
        :return: the ESMF typekind of the Grid area
        """
        return self._areatype

    @property
    def periodic_dim(self):
        """
        :return: the periodic dimension of the Grid (e.g. 0 for x or longitude, 1 for y or latitude, etc.)
        """
        return self._periodic_dim

    @property
    def pole_dim(self):
        """
        :return: the pole dimension of the Grid (e.g. 0 for x or longitude, 1 for y or latitude, etc.)
        """
        return self._pole_dim

    @property
    def coord_sys(self):
        """
        :return: the type of the coordinate system for this Grid
        """
        return self._coord_sys

    @property
    def ndims(self):
        return self._ndims

    @property
    def num_peri_dims(self):
        """
        :return: the total number of periodic dimensions in the Grid
        """
        return self._num_peri_dims

    @property
    def rank(self):
        """
        :return: the rank of the Grid
        """
        return self._rank

    @property
    def size(self):
        return self._size

    @property
    def staggerloc(self):
        """
        :return: a boolean list of the stagger locations that have been allocated for this Grid
        """
        return self._staggerloc

    @property
    def lower_bounds(self):
        """
        :return: the lower bounds, a numpy array with an entry for every dimension of the Grid
        """
        return self._lower_bounds

    @property
    def upper_bounds(self):
        """
        :return: the upper bounds, a numpy array with an entry for every dimension of the Grid
        """
        return self._upper_bounds

    @property
    def bounds_done(self):
        return self._bounds_done

    @property
    def coords(self):
        """
        :return: grid coordinates represented as a 2D list of numpy arrays, indexing with the first dimension
        representing the stagger location and the second representing the coordinate dimension will return a numpy
        array of size given by upper_bounds - lower_bounds
        """
        return self._coords

    @property
    def coords_done(self):
        return self._coords_done

    @property
    def mask(self):
        """
        :return: the grid mask represented as a numpy array of integers of size given by upper_bounds - lower_bounds
        """
        return self._mask

    @property
    def area(self):
        """
        :return: the grid area represented as a numpy array of floats of size given by upper_bounds - lower_bounds
        """
        return self._area

    @property
    def item_done(self):
        return self._item_done

    @property
    def meta(self):
        return self._meta

    @property
    def finalized(self):
        return self._finalized

    @property
    def singlestagger(self):
        return self._singlestagger

    # manual destructor
    def destroy(self):
        """
        Release the memory associated with a Grid. \n
        Required Arguments: \n
            None \n
        Optional Arguments: \n
            None \n
        Returns: \n
            None \n
        """
        if hasattr(self, '_finalized'):
            if not self._finalized:
                ESMP_GridDestroy(self)
                self._finalized = True

    def __del__(self):
        """
        Release the memory associated with a Grid. \n
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
        string = ("Grid:\n"
                  "    type = %r \n"
                  "    areatype = %r \n"
                  "    rank = %r \n"
                  "    num_peri_dims = %r \n"
                  "    periodic_dim = %r \n"
                  "    pole_dim = %r \n"
                  "    coord_sys = %r \n"
                  "    staggerloc = %r \n"
                  "    lower bounds = %r \n"
                  "    upper bounds = %r \n"
                  "    coords = %r \n"
                  "    mask = %r \n"
                  "    area = %r \n"
                  %
                  (self.type,
                   self.areatype,
                   self.rank,
                   self.num_peri_dims,
                   self.periodic_dim,
                   self.pole_dim,
                   self.coord_sys,
                   self.staggerloc,
                   self.lower_bounds,
                   self.upper_bounds,
                   self.coords,
                   self.mask,
                   self.area))

        return string

    def _copy_(self):
        # shallow copy
        ret = copy(self)
        # don't call ESMF destructor twice on the same shallow Python object
        ret._finalized = True

        return ret

    def __getitem__(self, slc):
        # no slicing in parallel
        if pet_count() > 1:
            raise SerialMethod

        # format and copy
        slc = get_formatted_slice(slc, self.rank)
        ret = self._copy_()

        # coords, mask and area
        ret._coords = [[get_none_or_ssslice(get_none_or_slice(get_none_or_slice(self.coords, stagger), coorddim), slc, stagger, self.rank)
                       for coorddim in range(self.rank)] for stagger in range(2**self.rank)]
        ret._mask = [get_none_or_slice(get_none_or_slice(self.mask, stagger), slc) for stagger in range(2**self.rank)]
        ret._area = [get_none_or_slice(get_none_or_slice(self.area, stagger), slc) for stagger in range(2**self.rank)]

        # upper bounds are "sliced" by taking the shape of the coords
        ret._upper_bounds = [get_none_or_bound(get_none_or_slice(ret.coords, stagger), 0) for stagger in range(2**self.rank)]
        # lower bounds do not need to be sliced yet because slicing is not yet enabled in parallel

        return ret

    def _preslice_(self, stagger):
        # to be used to slice off one stagger location of a grid for a specific field
        ret = self._copy_()

        # bounds, coords, mask and area
        ret._lower_bounds = get_none_or_slice(self.lower_bounds, stagger)
        ret._upper_bounds = get_none_or_slice(self.upper_bounds, stagger)
        ret._coords = get_none_or_slice(self.coords, stagger)
        ret._mask = get_none_or_slice(self.mask, stagger)
        ret._area = get_none_or_slice(self.area, stagger)

        ret._singlestagger = True

        return ret

    def _slice_onestagger_(self, slc):
        if pet_count() > 1:
            raise SerialMethod

        # to be used to slice the single stagger grid, one that has already been presliced
        slc = get_formatted_slice(slc, self.rank)
        ret = self._copy_()

        # coords, mask and area
        ret._coords = [get_none_or_slice(get_none_or_slice(self.coords, x), slc) for x in range(self.rank)]
        ret._mask = get_none_or_slice(self.mask, slc)
        ret._area = get_none_or_slice(self.area, slc)

        # upper bounds are "sliced" by taking the shape of the coords at first coorddim
        ret._upper_bounds = get_none_or_bound(ret.coords, 0)
        # lower bounds do not need to be sliced yet because slicing is not yet enabled in parallel

        return ret


    def add_coords(self, staggerloc=None, coord_dim=None, from_file=False):
        """
        Add coordinates to a Grid at the specified
        stagger location. \n
        Required Arguments: \n
            None \n
        Optional Arguments: \n
            staggerloc: the stagger location of the coordinate data. \n
                Argument values are: \n
                    2D: \n
                    (default) StaggerLoc.CENTER\n
                    StaggerLoc.EDGE1\n
                    StaggerLoc.EDGE2\n
                    StaggerLoc.CORNER\n
                    3D: \n
                    (default) StaggerLoc.CENTER_VCENTER\n
                    StaggerLoc.EDGE1_VCENTER\n
                    StaggerLoc.EDGE2_VCENTER\n
                    StaggerLoc.CORNER_VCENTER\n
                    StaggerLoc.CENTER_VFACE\n
                    StaggerLoc.EDGE1_VFACE\n
                    StaggerLoc.EDGE2_VFACE\n
            coord_dim: the dimension number of the coordinates to 
                       return (coordinates will not be returned if
                       coord_dim is not specified and staggerlocs is
                       a list with more than one element). \n
            from_file: boolean for internal use to determine whether Grid has already been created from file. \n
        Returns: \n
            A numpy array of coordinate values if staggerloc and coord_dim are specified, otherwise return None. \n
        """
        if staggerloc is None:
            staggerloc = [StaggerLoc.CENTER]
        else:
            try:
                staggerloc = list(staggerloc)
            except TypeError:
                staggerloc = [staggerloc]

        for stagger in staggerloc:
            if self.coords[stagger][0] is not None:
                warnings.warn("This coordinate has already been added.")
            else:
                # request that ESMF allocate space for the coordinates
                if not from_file:
                    ESMP_GridAddCoord(self, staggerloc=stagger)

                # and now for Python
                self._allocate_coords_(stagger, from_file=from_file)

                # set the staggerlocs to be done
                self.staggerloc[stagger] = True

        if len(staggerloc) == 1 and coord_dim is not None:
            return self.coords[staggerloc[0]][coord_dim]

    def add_item(self, item, staggerloc=None, from_file=False):
        """
        Allocate space for a Grid item (mask or areas)
        at a specified stagger location. \n
        Required Arguments: \n
            item: the Grid item to allocate. \n
                Argument values are: \n
                    GridItem.AREA\n
                    GridItem.MASK\n
        Optional Arguments: \n
            staggerloc: the stagger location of the coordinate data. \n
                Argument values are: \n
                    2D: \n
                    (default) StaggerLoc.CENTER\n
                    StaggerLoc.EDGE1\n
                    StaggerLoc.EDGE2\n
                    StaggerLoc.CORNER\n
                    3D: \n
                    (default) StaggerLoc.CENTER_VCENTER\n
                    StaggerLoc.EDGE1_VCENTER\n
                    StaggerLoc.EDGE2_VCENTER\n
                    StaggerLoc.CORNER_VCENTER\n
                    StaggerLoc.CENTER_VFACE\n
                    StaggerLoc.EDGE1_VFACE\n
                    StaggerLoc.EDGE2_VFACE\n
            from_file: boolean for internal use to determine whether Grid has already been created from file. \n
        Returns: \n
            A numpy array of the mask or area values if a single staggerloc is given, otherwise return None. \n
        """
        if staggerloc is None:
            staggerloc = [StaggerLoc.CENTER]
        else:
            try:
                staggerloc = list(staggerloc)
            except TypeError:
                staggerloc = [staggerloc]

        done = True
        for stagger in staggerloc:
            # check to see if they are done
            if item == GridItem.MASK:
                if self.mask[stagger] is not None:
                    raise GridItemAlreadyLinked
                done = False
            elif item == GridItem.AREA:
                if self.area[stagger] is not None:
                    raise GridItemAlreadyLinked
                done = False
            else:
                raise GridItemNotSupported

            if not done:
                # request that ESMF allocate space for the coordinates
                if not from_file:
                    ESMP_GridAddItem(self, item, staggerloc=stagger)

                # and now for Python..
                self._allocate_items_(item, stagger, from_file=from_file)

        if len(staggerloc) is 1:
            if item == GridItem.MASK:
                return self.mask[staggerloc[0]]
            elif item == GridItem.AREA:
                return self.area[staggerloc[0]]
            else:
                raise GridItemNotSupported

    def get_coords(self, coord_dim, staggerloc=None):
        """
        Return a numpy array of coordinates at a specified stagger 
        location. The returned array is NOT a copy, it is
        directly aliased to the underlying memory allocated by ESMF.\n
        Required Arguments: \n
            coord_dim: the dimension number of the coordinates to return:
                       e.g. [x, y, z] = (0, 1, 2), or [lat, lon] = (0, 1) \n
        Optional Arguments: \n
            staggerloc: the stagger location of the coordinate data. \n
                Argument values are: \n
                    2D: \n
                    (default) StaggerLoc.CENTER\n
                    StaggerLoc.EDGE1\n
                    StaggerLoc.EDGE2\n
                    StaggerLoc.CORNER\n
                    3D: \n
                    (default) StaggerLoc.CENTER_VCENTER\n
                    StaggerLoc.EDGE1_VCENTER\n
                    StaggerLoc.EDGE2_VCENTER\n
                    StaggerLoc.CORNER_VCENTER\n
                    StaggerLoc.CENTER_VFACE\n
                    StaggerLoc.EDGE1_VFACE\n
                    StaggerLoc.EDGE2_VFACE\n
        Returns: \n
            A numpy array of coordinate values at the specified staggerloc. \n
        """

        ret = None
        if not self._singlestagger:
            # handle the default case
            if staggerloc is None:
                staggerloc = StaggerLoc.CENTER
            elif type(staggerloc) is list:
                raise GridSingleStaggerloc
            elif type(staggerloc) is tuple:
                raise GridSingleStaggerloc

            assert (self.coords[staggerloc][coord_dim] is not None)
            ret = self.coords[staggerloc][coord_dim]
        else:
            assert (self.coords[coord_dim] is not None)
            ret = self.coords[coord_dim]

        return ret

    def get_item(self, item, staggerloc=None):
        """
        Return a numpy array for a Grid item at a specified stagger 
        location.  The returned array is NOT a copy, it is
        directly aliased to the underlying memory allocated by ESMF.\n
        Required Arguments: \n
            item: the Grid item to allocate. \n
                Argument values are: \n
                    GridItem.AREA\n
                    GridItem.MASK\n
        Optional Arguments: \n
            staggerloc: the stagger location of the coordinate data. \n
                Argument values are: \n
                    2D: \n
                    (default) StaggerLoc.CENTER\n
                    StaggerLoc.EDGE1\n
                    StaggerLoc.EDGE2\n
                    StaggerLoc.CORNER\n
                    3D: \n
                    (default) StaggerLoc.CENTER_VCENTER\n
                    StaggerLoc.EDGE1_VCENTER\n
                    StaggerLoc.EDGE2_VCENTER\n
                    StaggerLoc.CORNER_VCENTER\n
                    StaggerLoc.CENTER_VFACE\n
                    StaggerLoc.EDGE1_VFACE\n
                    StaggerLoc.EDGE2_VFACE\n
        Returns: \n
            A numpy array of mask or area values at the specified staggerloc. \n
        """

        ret = None
        if not self._singlestagger:
            # handle the default case
            if staggerloc is None:
                staggerloc = StaggerLoc.CENTER
            elif type(staggerloc) is list:
                raise GridSingleStaggerloc
            elif type(staggerloc) is tuple:
                raise GridSingleStaggerloc

            # selec the grid item
            if item == GridItem.MASK:
                assert (self.mask[staggerloc] is not None)
                ret = self.mask[staggerloc]
            elif item == GridItem.AREA:
                assert (self.area[staggerloc] is not None)
                ret = self.area[staggerloc]
            else:
                raise GridItemNotSupported
        else:
            if item == GridItem.MASK:
                assert (self.mask is not None)
                ret = self.mask
            elif item == GridItem.AREA:
                assert (self.area is not None)
                ret = self.area
            else:
                raise GridItemNotSupported

        return ret

    def set_item(self, item, staggerloc, item_data):
        raise MethodNotImplemented
        # check sizes
        # set item_out = item_data.copy()
        # set the item as a grid property and return this pointer
        #self.item[staggerloc_local] = item_out

    def set_coords(self, staggerloc, item_data):
        raise MethodNotImplemented
        # check sizes
        # assert (self.coords[staggerloc][:,:,coord_dim].shape == 
        #         coord_data.shape)
        # use user coordinates to initialize underlying ESMF data
        # self.coords[staggerloc][:,:,coord_dim] = coord_data.copy()

    def _write_(self, filename, staggerloc=None):
        """
        Write a Grid to vtk formatted file at a specified stagger 
        location. \n
        Required Arguments: \n
            filename: the name of the file, .vtk will be appended. \n
        Optional Arguments: \n
            staggerloc: the stagger location of the coordinate data. \n
                Argument values are: \n
                    2D: \n
                    (default) StaggerLoc.CENTER\n
                    StaggerLoc.EDGE1\n
                    StaggerLoc.EDGE2\n
                    StaggerLoc.CORNER\n
                    3D: \n
                    (default) StaggerLoc.CENTER_VCENTER\n
                    StaggerLoc.EDGE1_VCENTER\n
                    StaggerLoc.EDGE2_VCENTER\n
                    StaggerLoc.CORNER_VCENTER\n
                    StaggerLoc.CENTER_VFACE\n
                    StaggerLoc.EDGE1_VFACE\n
                    StaggerLoc.EDGE2_VFACE\n
        Returns: \n
            None \n
        """

        # handle the default case
        if staggerloc is None:
            staggerloc = StaggerLoc.CENTER
        elif type(staggerloc) is list:
            raise GridSingleStaggerloc
        elif type(staggerloc) is tuple:
            raise GridSingleStaggerloc

        ESMP_GridWrite(self, filename, staggerloc=staggerloc)

    ################ Helper functions ##########################################

    def verify_grid_bounds(self, stagger):
        if self.lower_bounds[stagger] is None:
            try:
                lb, ub = ESMP_GridGetCoordBounds(self, staggerloc=stagger)
            except:
                raise GridBoundsNotCreated

            self._lower_bounds[stagger] = np.copy(lb)
            self._upper_bounds[stagger] = np.copy(ub)

            # find the local size of this stagger
            self._size[stagger] = np.array(self.upper_bounds[stagger] -
                                       self.lower_bounds[stagger])
        else:
            lb, ub = ESMP_GridGetCoordBounds(self, staggerloc=stagger)
            assert(self.lower_bounds[stagger].all() == lb.all())
            assert(self.upper_bounds[stagger].all() == ub.all())
            assert(self.size[stagger].all() == np.array(ub-lb).all())

    def _allocate_coords_(self, stagger, from_file=False):
        # this could be one of several entry points to the grid,
        # verify that bounds and other necessary data are available
        self.verify_grid_bounds(stagger)

        # allocate space for the coordinates on the Python side
        self._coords[stagger][0] = np.zeros(shape = (self.size[stagger]),
                                            dtype = constants._ESMF2PythonType[self.type])
        self._coords[stagger][1] = np.zeros(shape = (self.size[stagger]),
                                            dtype = constants._ESMF2PythonType[self.type])
        if self.rank == 3:
            self._coords[stagger][2] = np.zeros(shape = (self.size[stagger]),
                                                dtype = constants._ESMF2PythonType[self.type])

        # link the ESMF allocations to the Python grid properties
        [x, y, z] = [0, 1, 2]
        self._link_coord_buffer_(x, stagger)
        self._link_coord_buffer_(y, stagger)
        if self.rank == 3:
            self._link_coord_buffer_(z, stagger)

        # initialize to zeros, because ESMF doesn't handle that
        if not from_file:
            self._coords[stagger][0][...] = 0
            self._coords[stagger][1][...] = 0
            if self.rank == 3:
               self._coords[stagger][2][...] = 0

    def _link_coord_buffer_(self, coord_dim, stagger):

        # if self.coords[stagger][coord_dim] is not None:
        #     raise GridCoordsAlreadyLinked

        # get the data pointer and bounds of the ESMF allocation
        data = ESMP_GridGetCoordPtr(self, coord_dim, staggerloc=stagger)
        lb, ub = ESMP_GridGetCoordBounds(self, staggerloc=stagger)

        gridCoordP = esmf_array(data, self.type, ub-lb)

        # alias the coordinates to a grid property
        self._coords[stagger][coord_dim] = gridCoordP.view()

    def _allocate_items_(self, item, stagger, from_file=False):
        # this could be one of several entry points to the grid,
        # verify that bounds and other necessary data are available
        self.verify_grid_bounds(stagger)

        # if the item is a mask it is of type I4
        if item == GridItem.MASK:
            self._mask[stagger] = np.zeros(\
                                          shape = (self.size[stagger]),
                                          dtype = constants._ESMF2PythonType[TypeKind.I4])
        # if the item is area then it is of type R8
        elif item == GridItem.AREA:
            self._area[stagger] = np.zeros(\
                                          shape = (self.size[stagger]),
                                          dtype = constants._ESMF2PythonType[TypeKind.R8])
        else:
            raise GridItemNotSupported

        # link the ESMF allocations to the grid properties
        self._link_item_buffer_(item, stagger)
        
        # initialize to zeros, because ESMF doesn't handle that
        if not from_file:
            if item == GridItem.MASK:
               self._mask[stagger][...] = 1
            elif item == GridItem.AREA:
               self._area[stagger][...] = 0
            else:
               raise GridItemNotSupported


    def _link_item_buffer_(self, item, stagger):

        # # check to see if they are done
        # if item == GridItem.MASK:
        #     if self.mask[stagger] is not None:
        #         raise GridItemAlreadyLinked
        # elif item == GridItem.AREA:
        #     if self.area[stagger] is not None:
        #         raise GridItemAlreadyLinked
        # else:
        #     raise GridItemNotSupported

        # get the data pointer and bounds of the ESMF allocation
        data = ESMP_GridGetItem(self, item, staggerloc=stagger)
        lb, ub = ESMP_GridGetCoordBounds(self, staggerloc=stagger)

        # create Array of the appropriate type the appropriate type
        if item == GridItem.MASK:
            self._mask[stagger] = esmf_array(data, TypeKind.I4, ub-lb)
        elif item == GridItem.AREA:
            self._area[stagger] = esmf_array(data, TypeKind.R8, ub-lb)
        else:
            raise GridItemNotSupported
