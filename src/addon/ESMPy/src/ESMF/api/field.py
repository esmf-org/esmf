# $Id$

"""
The Field API
"""

#### IMPORT LIBRARIES #########################################################

import numpy.ma as ma

from ESMF.interface.cbindings import *
from ESMF.util.decorators import initialize
from ESMF.api.esmpymanager import *
from ESMF.api.grid import *
from ESMF.api.mesh import *

import ESMF.api.constants as constants


#### Field class ##############################################################
[node, element] = [0, 1]

class Field(ma.MaskedArray):

    @initialize
    def __new__(cls, grid, name, 
                typekind=TypeKind.R8, 
                staggerloc=StaggerLoc.CENTER,
                meshloc=MeshLoc.NODE,
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

        if local_ndbounds:
            xd = len(local_ndbounds)
            lb = [1 for a in range(len(local_ndbounds))]
            ungridded_lower_bound = np.array(lb, dtype=np.int32)
            ungridded_upper_bound = np.array(local_ndbounds, dtype=np.int32)
            grid_to_field_map = np.array([xd+1, xd+2], dtype=np.int32)
            rank += len(local_ndbounds)

        data = None
        mask = None
        if isinstance(grid, Grid):
            # call into ctypes layer
            struct = ESMP_FieldCreateGrid(grid, name, typekind, staggerloc,
                                          grid_to_field_map,
                                          ungridded_lower_bound,
                                          ungridded_upper_bound)

            # set the mask
            if (grid.item_done[staggerloc][GridItem.MASK]):
                lbounds, ubounds = ESMP_FieldGetBounds(struct, rank)
                if (list(grid.mask[staggerloc].shape) == list(ubounds-lbounds)):
                    mask = grid.mask[staggerloc]

        elif isinstance(grid, Mesh):
            # deal with the wacky ESMF node/element convention
            if meshloc == MeshLoc.NODE:
                staggerloc = node
            elif meshloc == MeshLoc.ELEMENT:
                staggerloc = element
            else:
                raise MeshLocationNotSupported

            if local_ndbounds:
                grid_to_field_map = np.array([1], dtype=np.int32)
            # call into ctypes layer
            struct = ESMP_FieldCreate(grid, name, typekind, meshloc,
                                      grid_to_field_map,
                                      ungridded_lower_bound,
                                      ungridded_upper_bound)

            # No masking on a Mesh
            mask = None
        else:
            raise FieldDOError
     
        #  set field_mask based on the grid mask and the mask_values input argument
        field_mask = False
        if mask is not None and mask_values is not None:
            field_mask = [True if x in mask_values else False for x in mask.flatten().tolist()]

        # link the field data
        data = cls.link_field_data(struct, grid, staggerloc, typekind, rank)

        # create the new Field instance
        obj = super(Field, cls).__new__(cls, data = data, mask = field_mask)

        # register function with atexit
        import atexit; atexit.register(obj.__del__)
        obj._finalized = False

        # initialize field data
        obj.struct = struct
        obj.rank = rank
        obj.type = typekind
        obj.staggerloc = staggerloc
        obj.lower_bounds, obj.upper_bounds = ESMP_FieldGetBounds(struct, rank)
        obj.ndbounds = local_ndbounds
        obj.grid = grid
        obj.name = name
 
        return obj

    @staticmethod
    def link_field_data(struct, grid, staggerloc, typekind, rank):
        from operator import mul
        
        # get a pointer to the field data
        data_out = ESMP_FieldGetPtr(struct)

        # get the field bounds
        lbounds, ubounds = ESMP_FieldGetBounds(struct, rank)

        # find the size of the local coordinates
        size = reduce(mul,ubounds-lbounds)

        # create a numpy array to point to the ESMF allocation of field data
        fieldbuffer = np.core.multiarray.int_asbuffer(
            ct.addressof(data_out.contents),
            np.dtype(constants._ESMF2PythonType[typekind]).itemsize*size)
        fieldDataP = np.frombuffer(fieldbuffer, constants._ESMF2PythonType[typekind])

        # reshape the numpy array of coordinates, account for Fortran
        fieldDataP = np.reshape(fieldDataP,
                                 newshape = ubounds-lbounds,
                                 order='F')

        return fieldDataP

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

        if hasattr(self, '_finalized'):
            if self._finalized is False:
                ESMP_FieldDestroy(self)
                self._finalized = True



    def __repr__(self):
        """
        Return a string containing a printable representation of the object
        """
        string = ("Field:\n"
                  "    struct = %r\n"
                  "    ndbounds = %r\n"
                  "    staggerloc = %r\n"
                  "    type = %r\n"
                  "    grid = \n%r\n)" 
                  %
                  (self.struct, 
                   self.ndbounds,
                   self.staggerloc,
                   self.type,
                   self.grid))

        return string
    
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

    #def write(self, filename):
    #    ESMP_FieldWrite(self, filename)

    def dump_ESMF_coords(self):
        from operator import mul

        # retrieve buffers to esmf coordinate memory
        field_data = ESMP_FieldGetPtr(self.struct)

        # find the reduced size of the coordinate arrays
        size = reduce(mul,self.grid.size_local[self.staggerloc])

        # loop through and alias esmf data to numpy arrays
        buffer = np.core.multiarray.int_asbuffer(
            ct.addressof(field_data.contents),
            np.dtype(constants._ESMF2PythonType[self.type]).itemsize*size)
        esmf_coords = np.frombuffer(buffer, constants._ESMF2PythonType[self.type])

        print esmf_coords
