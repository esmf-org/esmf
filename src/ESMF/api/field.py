#!/usr/bin/env python
#
# $Id: field.py,v 1.1.2.10 2013/05/25 00:04:26 rokuingh Exp $

"""
The Field API
"""

#### IMPORT LIBRARIES #########################################################

import numpy as np
import numpy.ma as ma

from constants import *
from cbindings import *
from decorators import initialize

from manager import *
from grid import *
from mesh import *

#### Field class ##############################################################
[node, element] = [0, 1]

class Field(ma.MaskedArray):

    @initialize
    def __new__(cls, *args, **kwargs):
        """
        Create a Field from a Grid or Mesh. \n
        Required Arguments: \n
            grid: either a Grid or a Mesh with coordinates allocated on
                  at least one stagger location. \n
            name: user friendly name for the Grid or Mesh. \n
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
            grid_to_field_map: A numpy array (internally cast to 
                               dtype=numpy.int32) which 
                               specifies a mapping from the dimensions
                               of the grid to those of the field. \n
                type: np.array \n
                shape: [number of gridded dimensions, 1] \n
            ungridded_lower_bound: A numpy array (internally cast to 
                                   dtype=numpy.int32) which  
                                   specifies the lower bounds of the
                                   ungridded dimensions of the field. \n
                type: np.array \n
                shape: [number of ungridded dimensions, 1] \n
            ungridded_upper_bound: A numpy array (internally cast to 
                                   dtype=numpy.int32) which 
                                   specifies the upper bounds of the 
                                   ungridded dimensions of the field. \n
                type: np.array \n
                shape: [number of ungridded dimensions, 1] \n
        Returns: \n
            Field \n
        """
        # args
        try:
            grid = args[0]
            name = args[1]
        except:
            raise RequiredArgs(Field.__init__.__doc__)

        # kwargs
        typekind = TypeKind.R8
        if 'typekind' in kwargs:
            typekind = kwargs.get('typekind')

        staggerloc = StaggerLoc.CENTER
        if 'staggerloc' in kwargs:
            staggerloc = kwargs.get('staggerloc')

        meshloc = MeshLoc.NODE
        if 'meshloc' in kwargs:
            meshloc = kwargs.get('meshloc')

        grid_to_field_map = None
        if 'grid_to_field_map' in kwargs:
            grid_to_field_map = kwargs.get('grid_to_field_map')

        ungridded_lower_bound = None
        if 'ungridded_lower_bound' in kwargs:
            ungridded_lower_bound = kwargs.get('ungridded_lower_bound')

        ungridded_upper_bound = None
        if 'ungridded_upper_bound' in kwargs:
            ungridded_upper_bound = kwargs.get('ungridded_upper_bound')

        # type handling
        local_grid_to_field_map = None
        local_ungridded_lower_bound = None
        local_ungridded_upper_bound = None
        if grid_to_field_map is not None:
            if grid_to_field_map.dtype is not np.int32:
                local_grid_to_field_map = np.array(grid_to_field_map, 
                                                  dtype=np.int32)
            else:
                local_grid_to_field_map = grid_to_field_map
        # else case handled by initialization to None
        if ungridded_lower_bound is not None:
            if ungridded_lower_bound.dtype is not np.int32:
                local_ungridded_lower_bound = np.array(ungridded_lower_bound, 
                                                      dtype=np.int32)
            else:
                local_ungridded_lower_bound = ungridded_lower_bound
        # else case handled by initialization to None
        if ungridded_upper_bound is not None:
            if ungridded_upper_bound.dtype is not np.int32:
                local_ungridded_upper_bound = np.array(ungridded_upper_bound, 
                                                      dtype=np.int32)
            else:
                local_ungridded_upper_bound = ungridded_upper_bound


        # switch on grid or mesh
        if isinstance(grid, Grid):
            # check some stuff
            assert (grid.staggerloc[staggerloc])
            staggerloc2 = staggerloc
            # call into ctypes layer
            struct = ESMP_FieldCreateGrid(grid, name, typekind, staggerloc2,
                                          local_grid_to_field_map, 
                                          local_ungridded_lower_bound, 
                                          local_ungridded_upper_bound)
        elif isinstance(grid, Mesh):
            # call into ctypes layer
            staggerloc2 = meshloc
            struct = ESMP_FieldCreate(grid, name, typekind, staggerloc2,
                                      local_grid_to_field_map, 
                                      local_ungridded_lower_bound, 
                                      local_ungridded_upper_bound)
        else:
            raise ValueError("Field must be created on a Grid or Mesh")

        # link the field data
        data = cls.link_field_data(struct, grid, staggerloc2, typekind)

        # obtain the mask
        if isinstance(grid, Grid):
            if (grid.item_done[staggerloc2][GridItem.MASK]):
                if (grid.mask[staggerloc2].shape == data.shape):
                    mask = grid.mask[staggerloc2]
            else:
                mask = None
        elif isinstance(grid, Mesh):
            # No masking on a Mesh
            mask = None
        else:
            raise ValueError("Field must be created on a Grid or Mesh")
     
        # create the new Field instance
        obj = super(Field, cls).__new__(cls, data = data, mask = mask)

        # initialize field data
        obj.struct = struct
        obj.grid_to_field_map = local_grid_to_field_map
        obj.ungridded_lower_bound = local_ungridded_lower_bound
        obj.ungridded_upper_bound = local_ungridded_upper_bound
        
        return obj
    
    @staticmethod
    def link_field_data(struct, grid, staggerloc, typekind):
        from operator import mul
        
        # request the ESMF pointer to the Field coordinates
        data_out = ESMP_FieldGetPtr(struct)

        size = 0
        ind = 0
        # find the size of the local coordinates at this stagger location
        if isinstance(grid, Grid):
            size = reduce(mul,grid.size_local[staggerloc])
        elif isinstance(grid, Mesh):
            if staggerloc == MeshLoc.NODE:
                ind = node
            elif staggerloc == MeshLoc.ELEMENT:
                ind = element
            else:
                raise MeshLocationNotSupported
            size = grid.size_local[ind]
        else:
            raise FieldDOError

        # create a numpy array to point to the ESMF allocation
        fieldbuffer = np.core.multiarray.int_asbuffer(
            ct.addressof(data_out.contents),
            np.dtype(ESMF2PythonType[typekind]).itemsize*size)
        fieldDataP = np.frombuffer(fieldbuffer, ESMF2PythonType[typekind])

        # reshape the numpy array of coordinates, account for Fortran
        if isinstance(grid, Grid):
            fieldDataP = np.reshape(fieldDataP,
                                     newshape = grid.size_local[staggerloc],
                                     order='F')
        elif isinstance(grid, Mesh):
            fieldDataP = np.reshape(fieldDataP,
                                     newshape = grid.size_local[ind],
                                     order='F')
        else:
            raise FieldDOError

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
        ESMP_FieldDestroy(self)

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
