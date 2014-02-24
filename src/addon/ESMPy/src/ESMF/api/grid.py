# $Id$

"""
The Grid API
"""

#### IMPORT LIBRARIES #########################################################

from ESMF.api.constants import *
from ESMF.interface.cbindings import *
from ESMF.util.decorators import initialize

from ESMF.api.esmpymanager import *

import warnings

#### Grid class #########################################################

class Grid(object):

    @initialize
    def __init__(self, max_index=None,
                 num_peri_dims=0,
                 coord_sys=None,
                 coord_typekind=None,
                 staggerloc=None,
                 filename=None,
                 filetype=None,
                 is_sphere=None,
                 add_corner_stagger=None,
                 add_user_area=None,
                 add_mask=None,
                 varname="",
                 coord_names=None):
        """
        Create a logically rectangular Grid object and optionally 
        allocate space for coordinates at a specified stagger location. 
        A grid can be created in-memory or from file, there are two sets
        of argument for these methods, outlined below. \n
        Grid in memory: \n
            Required arguments for creating a Grid in memory: \n
                max_index: a numpy array which specifies the maximum
                           index of each dimension of the Grid. \n
                    type: np.array \n
                    shape: [number of dimensions, 1] \n
            Optional arguments for creating a grid in memory: \n
                num_peri_dims: the number of periodic dimensions (0 or 1). \n
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
        Grid from file: \n
            Required arguments for creating a Grid from file: \n
                filename: the name of NetCDF file containing the Grid. \n
                filetype: the input file type of the Grid. \n
                    Argument values are: \n
                        FileFormat.SCRIP \n
            Optional arguments for creating a Grid from file: \n
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
                          attribute defined in 'varname'.  Defaults to False. \n
                varname: If add_mask is True, provide a variable name stored in 
                         the grid file and the mask will be generated using the 
                         missing value of the data value of this variable.  The 
                         first two dimensions of the variable has to be the 
                         longitude and the latitude dimension and the mask is 
                         derived from the first 2D values of this variable even 
                         if this data is a 3D, or 4D array. Defaults to the 
                         empty string. \n
                coord_names:  A two-element array containing the longitude and 
                             latitude variable names in a GRIDSPEC file if there 
                             are multiple coordinates defined in the file. 
                             Defaults to None. \n
        The following optional arguments apply to a Grid created either from file 
        or in memory. \n
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
                self.max_index = np.array(max_index, dtype=np.int32)
            else:
                self.max_index = max_index
            # raise warnings on all from file args
            if filename is not None:
                warnings.warn("filename is only used for grids created from file, this argument will be ignored.")
            if filetype is not None:
                warnings.warn("filetype is only used for grids created from file, this argument will be ignored.")
            if is_sphere is not None:
                warnings.warn("is_sphere is only used for grids created from file, this argument will be ignored.")
            if add_corner_stagger is not None:
                warnings.warn("add_corner_stagger is only used for grids created from file, this argument will be ignored.")
            if add_user_area is not None:
                warnings.warn("add_user_area is only used for grids created from file, this argument will be ignored.")
            if add_mask is not None:
                warnings.warn("add_mask is only used for grids created from file, this argument will be ignored.")
            if varname is not "":
                warnings.warn("varname is only used for grids created from file, this argument will be ignored.")
            if coord_names:
                warnings.warn("coord_names is only used for grids created from file, this argument will be ignored.")
        # filename and filetype are required for from-file grids
        elif (filename is None) or (filetype is None):
            # raise error, need max_index to create in memory or filename to create from file
            raise GridArgumentError("must supply either max_index for an in-memory grid or filename and filetype for a from-file grid")
        # from file
        else:
            # set the from_file flag to True
            from_file = True
            #raise errors for all in-memory grid options
            if max_index is not None:
                warnings.warn("max_index is only used for grids created in memory, this argument will be ignored.")
            if num_peri_dims is not 0:
                warnings.warn("num_peri_dims is only used for grids created in memory, this argument will be ignored.")
            if coord_sys is not None:
                warnings.warn("coord_sys is only used for grids created in memory, this argument will be ignored.")
            if coord_typekind is not None:
                warnings.warn("coord_typekind is only used for grids created in memory, this argument will be ignored.")
            if staggerloc is not None:
                warnings.warn("staggerloc is only used for grids created in memory, this argument will be ignored.")

        # ctypes stuff
        self.struct = None

        # type, kind, rank, etc.
        self.type = TypeKind.R8
        self.areatype = TypeKind.R8
        self.rank = None
        self.num_peri_dims = num_peri_dims
        self.coord_sys = coord_sys

        # size, type and rank of the grid for bookeeping of coordinates 
        self.size = [None]
        # size_local holds the sizes according to the parallel distribution
        self.size_local = [None]

        # placeholder for staggerlocs, True if present, False if not
        self.staggerloc = [None]

        # placeholder for the list of numpy arrays which holds the grid bounds
        self.lower_bounds = [None]
        self.upper_bounds = [None]
        self.bounds_done = [None]

        # placeholder for the list of numpy arrays which hold the grid coords
        self.coords = [None]
        self.coords_done = [None]

        # mask and area
        self.mask =np.zeros(None)
        self.area = np.zeros(None)
        self.item_done = [None]

        # create the correct grid
        self.struct = None

        if from_file:
            #print 'Creating grid from ', filename
            self.struct = ESMP_GridCreateFromFile(filename, filetype,
                                                  isSphere=is_sphere,
                                                  addCornerStagger=add_corner_stagger,
                                                  addUserArea=add_user_area,
                                                  addMask=add_mask, varname=varname,
                                                  coordNames=coord_names)
            # grid rank
            self.rank = ESMP_ScripInqRank(filename)

            # grid dims            
            self.max_index = ESMP_ScripInqDims(filename)
            
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
                self.num_peri_dims = 0
            else:
                self.num_peri_dims = 1
            
        else:
            # ctypes stuff
            self.struct = ESMP_GridStruct()
            if self.num_peri_dims == 0:
                self.struct = ESMP_GridCreateNoPeriDim(self.max_index, 
                                                       coordSys=coord_sys,
                                                       coordTypeKind=coord_typekind)
            elif (self.num_peri_dims == 1):
                self.struct = ESMP_GridCreate1PeriDim(self.max_index, 
                                                      coordSys=coord_sys,
                                                      coordTypeKind=coord_typekind)
            else:
                raise TypeError("Number of periodic dimension should be 2 or 3")

            # grid rank
            self.rank = self.max_index.size

        # grid type
        if coord_typekind == None:
            self.type = TypeKind.R8
        else:
            self.type = coord_typekind

        # staggerloc
        self.staggerloc = [False for a in range(2**self.rank)]

        # bounds
        self.lower_bounds = [np.zeros(None) for a in range(2**self.rank)]
        self.upper_bounds = [np.zeros(None) for a in range(2**self.rank)]
        self.bounds_done = [False for a in range(2**self.rank)]

        # distributed sizes
        self.size_local = [np.zeros(None) for a in range(2**self.rank)]

        # grid size according to stagger locations
        peri_dim_offset = 1 - self.num_peri_dims
        
        if self.rank == 2:
            self.size = [\
                # ESMF_STAGGERLOC_CENTER
                reduce(mul,self.max_index),
                # ESMF_STAGGERLOC_EDGE1
                (self.max_index[0] + peri_dim_offset) * self.max_index[1],
                # ESMF_STAGGERLOC_EDGE2
                self.max_index[0] * (self.max_index[1] + 1),
                # ESMF_STAGGERLOC_CORNER
                (self.max_index[0] + peri_dim_offset) * (self.max_index[1] + 1)]
        elif self.rank == 3:
            self.size = [\
                # ESMF_STAGGERLOC_CENTER_VCENTER
                reduce(mul,self.max_index),
                # ESMF_STAGGERLOC_EDGE1_VCENTER
                (self.max_index[0] + peri_dim_offset) * 
                self.max_index[1] * 
                self.max_index[2],
                # ESMF_STAGGERLOC_EDGE2_VCENTER
                self.max_index[0] * (self.max_index[1] + 1) * self.max_index[2],
                # ESMF_STAGGERLOC_CORNER_VCENTER
                (self.max_index[0] + peri_dim_offset) * 
                (self.max_index[1] + 1) * 
                self.max_index[2],
                # ESMF_STAGGERLOC_CENTER_VFACE
                self.max_index[0] * self.max_index[1] * (self.max_index[2] + 1),
                # ESMF_STAGGERLOC_EDGE1_VFACE
                (self.max_index[0] + peri_dim_offset) * 
                self.max_index[1] * 
                (self.max_index[2] + 1),
                # ESMF_STAGGERLOC_EDGE2_VFACE
                self.max_index[0] * (self.max_index[1] + 1) * 
                (self.max_index[2] + 1),
                # ESMF_STAGGERLOC_CORNER_VFACE
                (self.max_index[0] + peri_dim_offset) * 
                (self.max_index[1] + 1) * 
                (self.max_index[2] + 1)]
        else:
            raise TypeError("max_index must be either a 2 or 3 dimensional \
                             Numpy array!")

        # initialize the coordinates structures
        # index order is [staggerLoc][coord_dim]
        self.coords = [[np.zeros(None) for a in range(self.rank)] \
                        for b in range(2**self.rank)]
        self.coords_done = [[False for a in range(self.rank)] \
                             for b in range(2**self.rank)]

        # initialize the item structures
        # index order is [staggerLoc][itemDim]
        self.mask = [[0] for a in range(2**self.rank)]
        self.area = [[0] for a in range(2**self.rank)]
        self.item_done = [[False for a in range(2)] \
                           for b in range(2**self.rank)]

        # Add coordinates if a staggerloc is specified
        if staggerloc != None:
            self.add_coords(staggerloc=staggerloc, from_file=from_file)

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
        ESMP_GridDestroy(self)

    def __repr__(self):
        """
        Return a string containing a printable representation of the object
        """
        string = ("Grid:\n"
                  "    struct = %r \n"
                  "    type = %r \n"
                  "    areatype = %r \n"
                  "    rank = %r \n"
                  "    num_peri_dims = %r \n"
                  "    coord_sys = %r \n"
                  "    size = %r \n"
                  "    size_local = %r \n"
                  "    max_index = %r \n"
                  "    staggerloc = %r \n"
                  "    lower_bounds = %r \n"
                  "    upper_bounds = %r \n"
                  "    coords = %r \n"
                  "    mask = %r \n"
                  "    area = %r \n" 
                  %
                  (self.struct,
                   self.type,
                   self.areatype,
                   self.rank,
                   self.num_peri_dims,
                   self.coord_sys,
                   self.size,
                   self.size_local,
                   self.max_index,
                   self.staggerloc,
                   self.lower_bounds,
                   self.upper_bounds,
                   self.coords,
                   self.mask,
                   self.area))

        return string
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
        Returns: \n
            None \n
        """
   
        # handle the default case
        staggerlocs = 0
        if staggerloc == None:
            staggerlocs = [StaggerLoc.CENTER]
        elif type(staggerloc) is list:
            staggerlocs = staggerloc
        elif type(staggerloc) is tuple:
            staggerlocs = list(staggerloc)
        else:
            staggerlocs = [staggerloc]

        for stagger in staggerlocs:
            if self.coords_done[stagger] == 1:
                warnings.warn("This coordinate has already been added.")

            # request that ESMF allocate space for the coordinates
            if not from_file:
                ESMP_GridAddCoord(self, staggerloc=stagger)

            # and now for Python
            self.allocate_coords(stagger)

            # set the staggerlocs to be done
            self.staggerloc[stagger] = True

        if len(staggerlocs) == 1 and coord_dim is not None:
            return self.coords[staggerlocs[0]][coord_dim]

    def add_item(self, item, staggerloc=None):
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
        Returns: \n
            None \n
        """

        # handle the default case
        staggerlocs = 0
        if staggerloc == None:
            staggerlocs = [StaggerLoc.CENTER]
        elif type(staggerloc) is list:
            staggerlocs = staggerloc
        elif type(staggerloc) is tuple:
            staggerlocs = list(staggerloc)
        else:
            staggerlocs = [staggerloc]

        for stagger in staggerlocs:
            if self.item_done[stagger][item] == 1:
                warnings.warn("This item has already been added.")

            # request that ESMF allocate space for this item
            ESMP_GridAddItem(self, item, staggerloc=stagger)

            # and now for Python..
            self.allocate_items(item, stagger)

        if len(staggerlocs) is 1:
            if item == GridItem.MASK:
                return self.mask[staggerlocs[0]]
            elif item == GridItem.AREA:
                return self.area[staggerlocs[0]]
            else:
                raise GridItemNotSupported

    def get_coords(self, coord_dim, staggerloc=None):
        """
        Return a numpy array of coordinates at a specified stagger 
        location. \n
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
            None \n
        """

        # handle the default case
        #TODO: return full coordinates, and by dimension as optional
        if staggerloc == None:
            staggerloc = StaggerLoc.CENTER
        elif type(staggerloc) is list:
            raise GridSingleStaggerloc
        elif type(staggerloc) is tuple:
            raise GridSingleStaggerloc

        assert(self.coords_done[staggerloc][coord_dim])
        return self.coords[staggerloc][coord_dim]

    def get_item(self, item, staggerloc=None):
        """
        Return a numpy array for a Grid item at a specified stagger 
        location. \n
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
            None \n
        """

        # handle the default case
        if staggerloc == None:
            staggerloc = StaggerLoc.CENTER
        elif type(staggerloc) is list:
            raise GridSingleStaggerloc
        elif type(staggerloc) is tuple:
            raise GridSingleStaggerloc

        assert(self.item_done[staggerloc][item])
        if item == GridItem.MASK:
            return self.mask[staggerloc]
        elif item == GridItem.AREA:
            return self.area[staggerloc]
        else:
            raise GridItemNotSupported

    def _set_item_(self, item, staggerloc, item_data):
        raise MethodNotImplemented
        # check sizes
        # set item_out = item_data.copy()
        # set the item as a grid property and return this pointer
        #self.item[staggerloc_local] = item_out

    def _set_coords_(self, staggerloc, item_data):
        raise MethodNotImplemented
        # check sizes
        # assert (self.coords[staggerloc][:,:,coord_dim].shape == 
        #         coord_data.shape)
        # use user coordinates to initialize underlying ESMF data
        # self.coords[staggerloc][:,:,coord_dim] = coord_data.copy()

    def _write(self, filename, staggerloc=None):
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
        if staggerloc == None:
            staggerloc = StaggerLoc.CENTER
        elif type(staggerloc) is list:
            raise GridSingleStaggerloc
        elif type(staggerloc) is tuple:
            raise GridSingleStaggerloc

        ESMP_GridWrite(self, filename, staggerloc=staggerloc)

    ################ Helper functions ##########################################

    def verify_grid_bounds(self, stagger):
        if not self.bounds_done[stagger]:
            try:
                lb, ub = ESMP_GridGetCoordBounds(self, staggerloc=stagger)
            except:
                raise GridBoundsNotCreated

            self.lower_bounds[stagger] = np.copy(lb)
            self.upper_bounds[stagger] = np.copy(ub)

            # find the local size of this stagger
            self.size_local[stagger] = np.array(self.upper_bounds[stagger] - \
                                       self.lower_bounds[stagger])
            # set these to be done
            self.bounds_done[stagger] = True
        else:
            lb, ub = ESMP_GridGetCoordBounds(self, staggerloc=stagger)
            assert(self.lower_bounds[stagger].all() == lb.all())
            assert(self.upper_bounds[stagger].all() == ub.all())
            assert(self.size_local[stagger].all() == np.array(ub-lb).all())

    def allocate_coords(self, stagger):
        # this could be one of several entry points to the grid,
        # verify that bounds and other necessary data are available
        self.verify_grid_bounds(stagger)

        # allocate space for the coordinates on the Python side
        self.coords[stagger][0] = np.zeros(\
                                           shape = (self.size_local[stagger]),
                                           dtype = ESMF2PythonType[self.type])
        self.coords[stagger][1] = np.zeros(\
                                           shape = (self.size_local[stagger]),
                                           dtype = ESMF2PythonType[self.type])
        if self.rank == 3:
            self.coords[stagger][2] = np.zeros(\
                                        shape = (self.size_local[stagger]),
                                        dtype = ESMF2PythonType[self.type])

        # link the ESMF allocations to the Python grid properties
        [x, y, z] = [0, 1, 2]
        self.link_coord_buffer(x, stagger)
        self.link_coord_buffer(y, stagger)
        if self.rank == 3:
            self.link_coord_buffer(z, stagger)

        # initialize to zeros, because ESMF doesn't handle that
        #RLO: this has to be removed because grids created from file
        #     have coordinates set by the time this is called, so it
        #     doesn't make sense to initialize them to 0.
        #self.coords[stagger][0][...] = 0
        #self.coords[stagger][1][...] = 0
        #if self.rank == 3:
        #    self.coords[stagger][2][...] = 0

    def link_coord_buffer(self, coord_dim, stagger):

        if self.coords_done[stagger][coord_dim]:
            raise GridCoordsAlreadyLinked

        gridCoordP = self.get_grid_coords_from_esmc(coord_dim, stagger)

        # alias the coordinates to a grid property
        self.coords[stagger][coord_dim] = gridCoordP.view()

        # set flag to tell this coordinate has been aliased
        self.coords_done[stagger][coord_dim] = True

    def get_grid_coords_from_esmc(self, coord_dim, stagger):
        # get the pointer to the underlying ESMF data array for coordinates
        coords_out = ESMP_GridGetCoordPtr(self, coord_dim, staggerloc=stagger)

        # find the size of the local coordinates at this stagger location
        from operator import mul
        size = reduce(mul,self.size_local[stagger])

        # create a numpy array to point to the ESMF allocation
        gridbuffer = np.core.multiarray.int_asbuffer(
            ct.addressof(coords_out.contents),
            np.dtype(ESMF2PythonType[self.type]).itemsize*size)
        gridCoordP = np.frombuffer(gridbuffer, ESMF2PythonType[self.type])

        # reshape the numpy array of coordinates using Fortran ordering in Grid
        gridCoordP = np.reshape(gridCoordP, self.size_local[stagger], order='F')

        return gridCoordP


    def allocate_items(self, item, stagger):
        # this could be one of several entry points to the grid,
        # verify that bounds and other necessary data are available
        self.verify_grid_bounds(stagger)

        # if the item is a mask it is of type I4
        if item == GridItem.MASK:
            self.mask[stagger] = np.zeros(\
                                          shape = (self.size_local[stagger]),
                                          dtype = ESMF2PythonType[TypeKind.I4])
        # if the item is area then it is of type R8
        elif item == GridItem.AREA:
            self.area[stagger] = np.zeros(\
                                          shape = (self.size_local[stagger]),
                                          dtype = ESMF2PythonType[TypeKind.R8])
        else:
            raise GridItemNotSupported

        # link the ESMF allocations to the grid properties
        self.link_item_buffer(item, stagger)
        
        # initialize to zeros, because ESMF doesn't handle that
        #RLO: this has to be removed because grids created from file
        #     have mask and area set by the time this is called, so it
        #     doesn't make sense to initialize them to 0.
        #if item == GridItem.MASK:
        #    self.mask[stagger][...] = 0
        #elif item == GridItem.AREA:
        #    self.area[stagger][...] = 0
        #else:
        #    raise GridItemNotSupported

    def link_item_buffer(self, item, stagger):
        from operator import mul

        # mask is 0, area is 1
        if self.item_done[stagger][item]:
            raise GridItemAlreadyLinked

        # get the pointer to the underlying ESMF data array for the item
        item_out = ESMP_GridGetItem(self, item, staggerloc=stagger)

        # find the size of the local mask at this stagger location
        size = reduce(mul,self.size_local[stagger])

        # create a numpy array to point to the ESMF allocation
        if item == GridItem.MASK:
            gridbuffer = np.core.multiarray.int_asbuffer(
                ct.addressof(item_out.contents),
                np.dtype(ESMF2PythonType[TypeKind.I4]).itemsize*size)
            gridItemP = np.frombuffer(gridbuffer, ESMF2PythonType[TypeKind.I4])

            # reshape the numpy array of item with Fortran ordering in Grid
            gridItemP = np.reshape(gridItemP, self.size_local[stagger], 
                                   order='F')

            # alias the item to a grid property
            self.mask[stagger] = gridItemP.view()
        elif item == GridItem.AREA:
            gridbuffer = np.core.multiarray.int_asbuffer(
                ct.addressof(item_out.contents),
                np.dtype(ESMF2PythonType[TypeKind.R8]).itemsize*size)
            gridItemP = np.frombuffer(gridbuffer, ESMF2PythonType[TypeKind.R8])

            # reshape the numpy array of item with Fortran ordering in Grid
            gridItemP = np.reshape(gridItemP, self.size_local[stagger], 
                                   order='F')

            # alias the item to a grid property
            self.area[stagger] = gridItemP.view()
        else:
            raise GridItemNotSupported

        # set flag to tell this mask has been aliased
        # mask is 0, area is 1
        self.item_done[stagger][item] = True

    def dump_gridinfo(self, stagger):

        [x,y,z] = [0,1,2]

        print "bounds - low, high"
        print self.lower_bounds[stagger], \
                    self.upper_bounds[stagger]
        print "shape - [x, y, z] or [lat, lon]"
        print self.coords[stagger][0].shape

        if self.rank == 2:
            return [self.coords[stagger][x], self.coords[stagger][y]]
        elif self.rank == 3:
            return [self.coords[stagger][x], self.coords[stagger][y], self.coords[stagger][z]]

    def dump_gridinfo_lower(self, stagger):
        [x, y, z] = [0, 1, 2]

        print "bounds - low, high"
        print self.lower_bounds[stagger], \
                    self.upper_bounds[stagger]
        print "shape - [x, y, z] or [lat, lon]"
        print self.coords[stagger][0].shape

        # retrieve buffers to esmf coordinate memory
        gridptrX = self.get_grid_coords_from_esmc(x, stagger)
        gridptrY = self.get_grid_coords_from_esmc(y, stagger)

        #print esmf_coords
        if self.rank == 2:
            return [gridptrX, gridptrY]
        elif self.rank == 3:
            gridptrZ = self.get_grid_coords_from_esmc(z, stagger)
            return [gridptrX, gridptrY, gridptrZ]
        
    def dump_gridinfo_ctypes(self, stagger, dim=2):    
        
        # find the size of the local coordinates at this stagger location
        from operator import mul
        size = reduce(mul,self.size_local[stagger])
        [x, y, z] = [0, 1, 2]
        
        # get the pointer to the underlying ESMF data array for coordinates
        
        xptr = ESMP_GridGetCoordPtr(self, x, staggerloc=stagger)

        import ctypes;
        buffer_from_memory = ctypes.pythonapi.PyBuffer_FromMemory
        buffer_from_memory.restype = ctypes.py_object
        xbuffer = buffer_from_memory(xptr, int(8*size))
        '''
        xbuffer = np.core.multiarray.int_asbuffer(
            ct.addressof(xptr.contents),
            np.dtype(ESMF2PythonType[self.type]).itemsize*size)
        '''
        xcoords = np.frombuffer(xbuffer, np.float64)
        #xcoordarray = numpy.ndarray((nrows, ncols), dtype=numpy.float32, order='F',
        #                 buffer=buf)

        yptr = ESMP_GridGetCoordPtr(self, y, staggerloc=stagger)
        ybuffer = np.core.multiarray.int_asbuffer(
            ct.addressof(yptr.contents),
            np.dtype(ESMF2PythonType[self.type]).itemsize*size)
        ycoords = np.frombuffer(ybuffer, ESMF2PythonType[self.type])

        print "DIAGNOSTICS:"
        print "self.type = ", self.type
        print "ESMF2PythonType", ESMF2PythonType[self.type]
        print "ESMF2PythonType.itemsize", ESMF2PythonType[self.type].itemsize
        
        
        # find the size of the local coordinates at this stagger location
        from operator import mul
        size = reduce(mul,self.size_local[stagger])

        # these appear to both return bounds information only
        lb, ub = ESMP_GridGetCoordBounds(self, staggerloc=stagger)
        
        print "Bounds:"
        print "  ESMPy.Grid:    ", size
        print "  ESMPy.ctypes1: ", lb, ub
        

        print "Coordinates:"
        I = ub[x]-lb[x]
        J = ub[y]-lb[y]
        if dim == 3:
            K = ub[z]-lb[z]
        
        coordcount = 0
        if dim == 2:
            for i in range(I):
                for j in range(J):
                    ind = i*J + j
                    
                    if  (1e-10 > xcoords[ind] and xcoords[ind] != 0) or \
                        xcoords[ind] > 360 or \
                        -90 > ycoords[ind] or \
                        (ycoords[ind] > -1e-10 and 1e-10 > ycoords[ind] and ycoords[ind] != 0) or \
                        ycoords[ind] > 90:
                    
                        print "[", i, ", ", j, "] = [", xcoords[ind], ", ", ycoords[ind], "]"
                    
                    coordcount += 1
        elif dim == 3:
            zptr = ESMP_GridGetCoordPtr(self, z, staggerloc=stagger)
            zbuffer = np.core.multiarray.int_asbuffer(
                ct.addressof(zptr.contents),
                np.dtype(ESMF2PythonType[self.type]).itemsize*size)
            zcoords = np.frombuffer(zbuffer, ESMF2PythonType[self.type])

            for i in xrange(I):
                for j in xrange(J):
                    for k in xrange(K):
                        ind = i*I + j*J + k
                        if  (1e-10 > xcoords[ind] and xcoords[ind] != 0) or \
                            xcoords[ind] > 360 or \
                            -90 > ycoords[ind] or \
                            (ycoords[ind] > -1e-10 and 1e-10 > ycoords[ind] and ycoords[ind] != 0) or \
                            ycoords[ind] > 90:
                        
                            print "[", i, ", ", j, "] = [", xcoords[ind], ", ", ycoords[ind], "]"
                        coordcount += 1
            
        print "Coordcount = ", coordcount
        '''
        # create a numpy array to point to the ESMF allocation
        # reshape the numpy array of coordinates using Fortran ordering in Grid
        gridCoordP = np.reshape(gridCoordP, self.size_local[stagger], order='F')
        '''
