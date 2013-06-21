# $Id$

"""
The Grid API
"""

#### IMPORT LIBRARIES #########################################################

from ESMF.api.constants import *
from ESMF.interface.cbindings import *
from ESMF.util.decorators import initialize

from ESMF.api.manager import *

#### Grid class #########################################################

class Grid(object):

    @initialize
    def __init__(self, *args, **kwargs):
        """
        Create a logically rectangular Grid object and optionally 
        allocate space for coordinates at a specified stagger location. \n
        Required Arguments: \n
            max_index: a numpy array which specifies the maximum
                       index of each dimension of the Grid. \n
                type: np.array \n
                shape: [dimCount, 1] \n
        Optional Arguments: \n
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
            Grid \n
        """
        from operator import mul

        # args
        try:
            max_index = args[0]
        except:
            raise RequiredArgs(Grid.__init__.__doc__)

        # kwargs
        num_peri_dims = 0
        if 'num_peri_dims' in kwargs:
            num_peri_dims = kwargs.get('num_peri_dims')

        coord_sys = None
        if 'coord_sys' in kwargs:
            coord_sys = kwargs.get('coord_sys')

        coord_typekind = None
        if 'coord_typekind' in kwargs:
            coord_typekind = kwargs.get('coord_typekind')

        staggerloc = None
        if 'staggerloc' in kwargs:
            staggerloc = kwargs.get('staggerloc')

        # ctypes stuff
        self.struct = ESMP_GridStruct()

        # type, kind, rank
        self.type = TypeKind.R8
        self.areatype = TypeKind.R8
        self.rank = None

        # size, type and rank of the grid for bookeeping of coordinates 
        self.size = [None]
        # size_local holds the sizes according to the parallel distribution
        self.size_local = [None]

        # grid properties
        if max_index.dtype is not np.int32:
            self.max_index = np.array(max_index, dtype=np.int32)
        else:
            self.max_index = max_index

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
        self.mask = np.array(None)
        self.area = np.array(None)
        self.item_done = [None]

        # create the correct grid
        if num_peri_dims == 0:
            self.struct = ESMP_GridCreateNoPeriDim(self.max_index, 
                                                   coordSys=coord_sys,
                                                   coordTypeKind=coord_typekind)
        elif (num_peri_dims == 1):
            self.struct = ESMP_GridCreate1PeriDim(self.max_index, 
                                                  coordSys=coord_sys,
                                                  coordTypeKind=coord_typekind)
        else:
            raise TypeError("Number of periodic dimension should be 2 or 3")

        # grid type
        if coord_typekind == None:
            self.type = TypeKind.R8
        else:
            self.type = coord_typekind
        # grid rank
        self.rank = self.max_index.size

        # staggerloc
        self.staggerloc = [False for a in range(2**self.rank)]

        # bounds
        self.lower_bounds = [np.array(a) for a in range(2**self.rank)]
        self.upper_bounds = [np.array(a) for a in range(2**self.rank)]
        self.bounds_done = [False for a in range(2**self.rank)]

        # distributed sizes
        self.size_local = [np.array(a) for a in range(2**self.rank)]

        # grid size according to stagger locations
        if self.rank == 2:
            self.size = \
                [reduce(mul,self.max_index),             # ESMF_STAGGERLOC_CENTER
                (self.max_index[0] + 1) * self.max_index[1],  # ESMF_STAGGERLOC_EDGE1
                self.max_index[0] * (self.max_index[1] + 1),  # ESMF_STAGGERLOC_EDGE2
                reduce(mul,self.max_index+1)]            # ESMF_STAGGERLOC_CORNER
        elif self.rank == 3:
            self.size = [\
                # ESMF_STAGGERLOC_CENTER_VCENTER
                reduce(mul,self.max_index),
                # ESMF_STAGGERLOC_EDGE1_VCENTER
                (self.max_index[0] + 1) * self.max_index[1] * self.max_index[2],
                # ESMF_STAGGERLOC_EDGE2_VCENTER
                self.max_index[0] * (self.max_index[1] + 1) * self.max_index[2],
                # ESMF_STAGGERLOC_CORNER_VCENTER
                (self.max_index[0] + 1) * (self.max_index[1] + 1) * 
                self.max_index[2],
                # ESMF_STAGGERLOC_CENTER_VFACE
                self.max_index[0] * self.max_index[1] * (self.max_index[2] + 1),
                # ESMF_STAGGERLOC_EDGE1_VFACE
                (self.max_index[0] + 1) + self.max_index[1] * 
                (self.max_index[2] + 1),
                # ESMF_STAGGERLOC_EDGE2_VFACE
                self.max_index[0] + (self.max_index[1] + 1) * 
                (self.max_index[2] + 1),
                # ESMF_STAGGERLOC_CORNER_VFACE
                reduce(mul,self.max_index+1)]
        else:
            raise TypeError("max_index must be either a 2 or 3 dimensional \
                             Numpy array!")

        # initialize the coordinates structures
        # index order is [staggerLoc][coord_dim]
        self.coords = [[np.array(a) for a in range(self.rank)] \
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
            self.add_coords(staggerloc=staggerloc)

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

    def add_coords(self, **kwargs):
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
        # kwargs
        staggerloc = None
        if 'staggerloc' in kwargs:
            staggerloc = kwargs.get('staggerloc')

        coord_dim = None
        if 'coord_dim' in kwargs:
            coord_dim = kwargs.get('coord_dim')
   
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
                raise Warning("This coordinate has already been added.")

            # request that ESMF allocate space for the coordinates
            ESMP_GridAddCoord(self, staggerloc=stagger)

            # and now for Python
            self.allocate_coords(stagger)

            # set the staggerlocs to be done
            self.staggerloc[stagger] = True

        if len(staggerlocs) == 1 and coord_dim is not None:
            return self.coords[staggerlocs[0]][coord_dim]

    def add_item(self, *args, **kwargs):
        """
        Allocate space for a Grid item (mask or areas)
        at a specified stagger location. \n
        Required Arguments: \n
            item: the Grid item to allocate. \n
                Argument values are: \n
                    GridItem.AREA\n
                    GridItem.MASK\n
        Optional Arguments: \n
            staggerlocs: the stagger location of the coordinate data. \n
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
        # args
        try:
            item = args[0]
        except:
            raise RequiredArgs(Grid.add_item.__doc__)

        # kwargs
        staggerloc = None
        if 'staggerloc' in kwargs:
            staggerloc = kwargs.get('staggerloc')

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
                    raise Warning("This item has already been added.")

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

    def get_coords(self, *args, **kwargs):
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
        #args
        try:
            coord_dim = args[0]
        except:
            raise RequiredArgs(Grid.get_coords.__doc__)        
        
        # kwargs
        staggerloc = None
        if 'staggerloc' in kwargs:
            staggerloc = kwargs.get('staggerloc')

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

    def get_item(self, *args, **kwargs):
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
        # args
        try:
            item = args[0]
        except:
            raise RequiredArgs(Grid.get_item.__doc__)

        # kwargs
        staggerloc = None
        if 'staggerloc' in kwargs:
            staggerloc = kwargs.get('staggerloc')

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

    def write(self, *args, **kwargs):
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
        # args
        try:
            filename = args[0]
        except:
            raise RequiredArgs(Grid.write.__doc__)

        # kwargs
        staggerloc = None
        if 'staggerloc' in kwargs:
            staggerloc = kwargs.get('staggerloc')

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

    def link_coord_buffer(self, coord_dim, stagger):
        from operator import mul

        if self.coords_done[stagger][coord_dim]:
            raise GridCoordsAlreadyLinked

        # get the pointer to the underlying ESMF data array for coordinates
        coords_out = ESMP_GridGetCoordPtr(self, coord_dim, staggerloc=stagger)

        # find the size of the local coordinates at this stagger location
        size = reduce(mul,self.size_local[stagger])

        # create a numpy array to point to the ESMF allocation
        gridbuffer = np.core.multiarray.int_asbuffer(
            ct.addressof(coords_out.contents),
            np.dtype(ESMF2PythonType[self.type]).itemsize*size)
        gridCoordP = np.frombuffer(gridbuffer, ESMF2PythonType[self.type])

        # reshape the numpy array of coordinates using Fortran ordering in Grid
        gridCoordP = np.reshape(gridCoordP, self.size_local[stagger], order='F')

        # alias the coordinates to a grid property
        self.coords[stagger][coord_dim] = gridCoordP.view()

        # set flag to tell this coordinate has been aliased
        self.coords_done[stagger][coord_dim] = True

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

    def dump_coords(self, stagger):

        [x,y,z] = [0,1,2]

        print "bounds - low, high"
        print self.lower_bounds[stagger], \
                    self.upper_bounds[stagger]
        print "shape - [x, y, z] or [lat, lon]"
        print self.coords[stagger][x].shape, \
                             self.coords[stagger][y].shape
        if self.rank == 3:
            print self.coords[stagger][z].shape


        print "x or latitude"
        print self.coords[stagger][x]

        print "y or longitude"
        print self.coords[stagger][y]

        if self.rank == 3:
            print "z"
            print self.coords[stagger][z]

    def dump_ESMF_coords(self, stagger):
        from operator import mul

        [x, y, z] = [0, 1, 2]

        # retrieve buffers to esmf coordinate memory
        gridptrX = ESMP_GridGetCoordPtr(self, x, staggerloc=stagger)
        gridptrY = ESMP_GridGetCoordPtr(self, y, staggerloc=stagger)
        if self.rank == 3:
            gridptrZ = ESMP_GridGetCoordPtr(self, z, staggerloc=stagger)

        # find the reduced size of the coordinate arrays
        size = reduce(mul,self.size_local[stagger])

        # create a numpy array for the esmf coordinate
        esmf_coords = 999*np.zeros(shape=(size,self.rank))

        # loop through and alias esmf data to numpy arrays
        gridbuffer = np.core.multiarray.int_asbuffer(
            ct.addressof(gridptrX.contents),
            np.dtype(ESMF2PythonType[self.type]).itemsize*size)
        esmf_coords[:,x] = np.frombuffer(gridbuffer, ESMF2PythonType[self.type])
        gridbuffer = np.core.multiarray.int_asbuffer(
            ct.addressof(gridptrY.contents),
            np.dtype(ESMF2PythonType[self.type]).itemsize*size)
        esmf_coords[:,y] = np.frombuffer(gridbuffer, ESMF2PythonType[self.type])
        if self.rank == 3:
            gridbuffer = np.core.multiarray.int_asbuffer(
                ct.addressof(gridptrZ.contents),
                np.dtype(ESMF2PythonType[self.type]).itemsize*size)
            esmf_coords[:,z] = np.frombuffer(gridbuffer, 
                                             ESMF2PythonType[self.type])

        print esmf_coords
