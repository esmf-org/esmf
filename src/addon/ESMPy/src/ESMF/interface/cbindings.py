# $Id$

"""
The bindings to the ESMF C API
"""

import ctypes as ct
import numpy as np

import ESMF.api.constants as constants
from ESMF.util.decorators import deprecated, netcdf
from ESMF.interface.loadESMF import _ESMF

def copy_struct(src):
    dst = type(src)()
    ct.pointer(dst)[0] = src
    return dst

class ESMP_Field(ct.Structure):
    _fields_ = [("ptr", ct.c_void_p)]

class ESMP_GridStruct(ct.Structure):
    _fields_ = [("ptr", ct.c_void_p)]

class ESMP_Mesh(ct.Structure):
    _fields_ = [("ptr", ct.c_void_p)]

class ESMP_LocStream(ct.Structure):
    _fields_ = [("ptr", ct.c_void_p)]

class ESMP_VM(ct.Structure):
        _fields_ = [("ptr", ct.c_void_p)]

# this class allows optional arguments to be passed in place of
# numpy float64 arrays
class OptionalNumpyArrayFloat64(object):
        @classmethod
        def from_param(self, param):
            if param is None:
                return None
            elif param.dtype != np.float64:
                raise TypeError("array must have data type Numpy.float64")
            else:
                return param.ctypes

# this class allows ESMP_InterfaceInts to be passed in place of 
# pointers to ctypes structures
class OptionalStructPointer(object):
        @classmethod
        def from_param(self, param):
            if param is None:
                return None
            else:
                ptr = ct.POINTER(ESMP_InterfaceInt)
                interfaceintptr = ptr(param)
                return interfaceintptr

# this class allows optional array of strings to be passed in plass of
# a python list of strings
class OptionalArrayOfStrings(object):
        @classmethod
        def from_param(self, param):
            if param:
                lparam = (ct.c_char_p * len(param))()
                lparam[:] = param
            else:
                lparam = None
            return lparam

# this class allows optional Fields to be passed in place of pointers to
# ctypes structures
class OptionalField(object):
        @classmethod
        def from_param(self, param):
            if param is None:
                return None
            else:
                ptr = ct.POINTER(ct.c_void_p)
                fieldptr = ptr(ct.c_void_p(param.struct.ptr))
                return fieldptr

# this class allows optional arguments to be passed in place of named constants
class OptionalNamedConstant(object):
        @classmethod
        def from_param(self, param):
            if param is None:
                return None
            else:
                ptr = ct.POINTER(ct.c_int)
                paramptr = ptr(ct.c_int(param))
                return paramptr

# this class allows optional arguments to be passed in place of
# numpy int32 arrays
class OptionalNumpyArrayInt32(object):
        @classmethod
        def from_param(self, param):
            if param is None:
                return None
            elif param.dtype != np.int32:
                raise TypeError("array must have data type Numpy.int32")
            else:
                return param.ctypes

# this class allows optional arguments to be passed in place of
# booleans
class OptionalBool(object):
        @classmethod
        def from_param(self, param):
            if param is None:
                return None
            else:
                ptr = ct.POINTER(ct.c_bool)
                paramptr = ptr(ct.c_bool(param))
                return paramptr


#### INIT/FINAL ###################################################

_ESMF.ESMC_Initialize.restype = ct.c_int
#_ESMF.ESMC_Initialize.argtypes = []
@deprecated
def ESMP_Initialize(logkind = constants.LogKind.MULTI):
    """
    Preconditions: An ESMF shared library must have been loaded.
    Postconditions: ESMP has been initialized, further ESMF calls may 
                    be issued.  This method can only be called once per 
                    execution, and must be followed by one and only one 
                    call to ESMP_Finalize(). \n
    Arguments:\n
        LogKind (optional)    :: logkind\n
            Argument Values:\n
                (default) MULTI\n
                NONE\n
        """
    # need to add this to the Initialize statement

    rc = _ESMF.ESMC_Initialize(None, constants._ESMP_InitArgLogKindFlagID, 
                               logkind, constants._ESMP_ArgLast)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_Initialize() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

_ESMF.ESMC_Finalize.restype = ct.c_int
_ESMF.ESMC_Finalize.argtypes = []
@deprecated
def ESMP_Finalize():
    """
    Preconditions: ESMF has been initialized.
    Postconditions: ESMF has been finalized, all heap memory has been 
                    released, and all MPI states have been cleaned up.  
                    This method can only be called once per execution, 
                    and must be preceded by one and only one call to 
                    ESMP_Initialize(). \n
    """
    rc = _ESMF.ESMC_Finalize()
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_Finalize() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

#### INTERFACEINT #############################################################

class ESMP_InterfaceInt(ct.Structure):
    _fields_ = [("shallowMem", ct.c_char*80)]
                
    def __init__(self, arrayArg):
        # initialize the InterfaceInt on the ESMF side
        ESMP_InterfaceIntSet(self, arrayArg, len(arrayArg))
        super(ESMP_InterfaceInt, self).__init__()

_ESMF.ESMC_InterfaceIntSet.restype = ct.c_int
_ESMF.ESMC_InterfaceIntSet.argtypes = [ct.POINTER(ESMP_InterfaceInt),
                                       np.ctypeslib.ndpointer(dtype=np.int32),
                                       ct.c_int]
def ESMP_InterfaceIntSet(iiptr, arrayArg, lenArg):
    """
    Preconditions: ESMP has been initialized and 'arrayArg' is a Numpy 
                   array of type int and 'lenArg' is the length of 'arrayArg'.\n
    Postconditions: An ESMP_InterfaceInt pointer has been created.\n
    Arguments:\n
        :RETURN: ESMP_InterfaceInt.ptr :: grid\n
        ESMP_InterfaceIntStruct.ptr    :: iiptr\n
        Numpy.array(dtype=np.int32)    :: arrayArg\n
        integer                        :: lenArg\n
    """
    rc = _ESMF.ESMC_InterfaceIntSet(ct.byref(iiptr), arrayArg, lenArg)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_InterfaceIntSet() failed with rc = '+str(rc)+
                        '.    '+constants._errmsg)

#### VM #######################################################################

_ESMF.ESMC_VMGet.restype = ct.c_int
_ESMF.ESMC_VMGet.argtypes = [ct.c_void_p, ct.POINTER(ct.c_int),
                             ct.POINTER(ct.c_int), ct.POINTER(ct.c_int),
                             ct.c_void_p, ct.POINTER(ct.c_int),
                             ct.POINTER(ct.c_int)]
@deprecated
def ESMP_VMGet(vm):
    """
    Preconditions: An ESMP_VM object has been retrieved.\n
    Postconditions: Information has been returned about 'vm' in the 
                    form of a tuple containing: [localPet, petCount].\n
    Arguments:\n
        :RETURN: integer :: localPet\n
        :RETURN: integer :: petCount\n
        ESMP_VM                    :: vm\n
    """
    lpet = ct.c_int(0)
    lcount = ct.c_int(0)
    ltemp = ct.c_int(0)

    rc = _ESMF.ESMC_VMGet(vm, ct.byref(lpet), ct.byref(lcount),
                          None, None, None, ct.byref(ltemp))
    # TODO: workaround for a pointer off-by-one problem on Jaguarpf

    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_VMGet() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)
    localPet = lpet.value
    petCount = lcount.value

    return localPet, petCount

_ESMF.ESMC_VMGetGlobal.restype = ESMP_VM
_ESMF.ESMC_VMGetGlobal.argtypes = [ct.POINTER(ct.c_int)]
@deprecated
def ESMP_VMGetGlobal():
    """
    Preconditions: ESMP has been initialized.\n
    Postconditions: The global 'vm' object is returned.\n
    Arguments:\n
        :RETURN: ESMP_VM :: vm\n
    """
    lrc = ct.c_int(0)
    vm = _ESMF.ESMC_VMGetGlobal(ct.byref(lrc))
    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_VMGetGlobal() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)
    return vm.ptr

_ESMF.ESMC_VMPrint.restype = ct.c_int
_ESMF.ESMC_VMPrint.argtypes = [ct.c_void_p]
@deprecated
def ESMP_VMPrint(vm):
    """
    Preconditions: An ESMP_VM object has been retrieved.\n
    Postconditions: The contents of 'vm' have been printed to standard 
                    output.\n
    Arguments:\n
        ESMP_VM :: vm\n
    """
    rc = _ESMF.ESMC_VMPrint(vm)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_VMPrint() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

#### LOG ######################################################################

_ESMF.ESMC_LogSet.restype = ct.c_int
_ESMF.ESMC_LogSet.argtypes = [ct.c_int]
@deprecated
def ESMP_LogSet(flush):
    """
    Preconditions: ESMP has been initialized\n
    Postconditions: The default Log has been set to flush after every message.\n
    Arguments:\n
        bool                     :: flush\n
    """
    lflush = ct.c_int(flush)
    rc = _ESMF.ESMC_LogSet(lflush)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_LogSet() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

#### GRID #####################################################

#TODO: InterfaceInt should be passed by value when ticket 3613642 is resolved
_ESMF.ESMC_GridCreate1PeriDim.restype = ESMP_GridStruct
_ESMF.ESMC_GridCreate1PeriDim.argtypes = [ct.POINTER(ESMP_InterfaceInt),
                                          OptionalNamedConstant,
                                          OptionalNamedConstant,
                                          OptionalNamedConstant,
                                          OptionalNamedConstant,
                                          OptionalNamedConstant,
                                          OptionalNamedConstant,
                                          ct.POINTER(ct.c_int)]
@deprecated
def ESMP_GridCreate1PeriDim(maxIndex, periodicDim=None, poleDim=None, 
                            coordSys=None, coordTypeKind=None):
    """
    Preconditions: ESMP has been initialized.\n
    Postconditions: An ESMP_Grid has been created.\n
    Arguments:\n
        :RETURN: ESMP_Grid    :: grid\n
        Numpy.array(dtype=int32) :: maxIndex\n
        integer (optional) :: periodicDim\n
        integer (optional) :: poleDim\n
        CoordSys (optional)   :: coordSys\n
            Argument Values:\n
                CoordSys.CART\n
                (default) CoordSys.SPH_DEG\n
                CoordSys.SPH_RAD\n
        TypeKind (optional)   :: coordTypeKind\n
            Argument Values:\n
                TypeKind.I4\n
                TypeKind.I8\n
                TypeKind.R4\n
                (default) TypeKind.R8\n
    """
    lrc = ct.c_int(0)

    #InterfaceInt requires int32 type numpy arrays
    if (maxIndex.dtype != np.int32):
        raise TypeError('maxIndex must have dtype=int32')

    # set up the max index interface int
    maxIndex_i = ESMP_InterfaceInt(maxIndex)

    # reset the periodic_dim and pole_dim to be 1 based for ESMF
    if periodicDim is not None:
        periodicDim += 1
    if poleDim is not None:
        poleDim += 1

    # dummy value to correspond to ESMF_INDEX_GLOBAL = 1 for global indexing
    indexflag = 1

    # create the ESMF Grid and retrieve a ctypes pointer to it
    gridstruct = _ESMF.ESMC_GridCreate1PeriDim(ct.byref(maxIndex_i),
                                               periodicDim, poleDim, coordSys,
                                               coordTypeKind, None, indexflag,
                                               ct.byref(lrc))

    # check the return code from ESMF
    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_GridCreate() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

    # create the ESMP Grid object from ctypes pointer
    return gridstruct

#TODO: InterfaceInt should be passed by value when ticket 3613642 is resolved
_ESMF.ESMC_GridCreateNoPeriDim.restype = ESMP_GridStruct
_ESMF.ESMC_GridCreateNoPeriDim.argtypes = [ct.POINTER(ESMP_InterfaceInt),
                                           OptionalNamedConstant,
                                           OptionalNamedConstant,
                                           OptionalNamedConstant,
                                           ct.POINTER(ct.c_int)]
@deprecated
def ESMP_GridCreateNoPeriDim(maxIndex, coordSys=None, coordTypeKind=None):
    """
    Preconditions: ESMP has been initialized.\n
    Postconditions: An ESMP_Grid has been created.\n
    Arguments:\n
        :RETURN: ESMP_Grid :: grid\n
        Numpy.array(dtype=int32) :: maxIndex\n
        CoordSys (optional)    :: coordSys\n
            Argument Values:\n
                CoordSys.CART\n
                (default) CoordSys.SPH_DEG\n
                CoordSys.SPH_RAD\n
        TypeKind (optional)    :: coordTypeKind\n
            Argument Values:\n
                TypeKind.I4\n
                TypeKind.I8\n
                TypeKind.R4\n
                (default) TypeKind.R8\n
    """
    lrc = ct.c_int(0)

    #InterfaceInt requires int32 type numpy arrays
    if (maxIndex.dtype != np.int32):
        raise TypeError('maxIndex must have dtype=int32')
        return None

    # this was not working in 32 bit mode because the array was not copied
    maxIndex_i = ESMP_InterfaceInt(maxIndex)

    # dummy value to correspond to ESMF_INDEX_GLOBAL = 1 for global indexing
    indexflag = 1

    # create the ESMF Grid and retrieve a ctypes pointer to it
    gridstruct = _ESMF.ESMC_GridCreateNoPeriDim(ct.byref(maxIndex_i), coordSys,
                                                coordTypeKind, indexflag, ct.byref(lrc))

    # check the return code from ESMF
    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_GridCreateNoPeriDim() failed with rc = '+str(rc)+
                        '.    '+constants._errmsg)

    # create the ESMP Grid object from ctypes pointer
    return gridstruct

_ESMF.ESMC_GridCreateFromFile.restype = ESMP_GridStruct
_ESMF.ESMC_GridCreateFromFile.argtypes = [ct.c_char_p, ct.c_int,
                                          ct.POINTER(ct.c_int),
                                          OptionalNumpyArrayInt32,
                                          OptionalNamedConstant,
                                          OptionalNamedConstant,
                                          OptionalNamedConstant,
                                          OptionalNamedConstant,
                                          OptionalNamedConstant,
                                          ct.c_char_p,
                                          OptionalArrayOfStrings,
                                          ct.POINTER(ct.c_int)]
@deprecated
@netcdf
def ESMP_GridCreateFromFile(filename, fileTypeFlag, regDecomp,
                            decompflag=None, isSphere=None, 
                            addCornerStagger=None, addUserArea=None,
                            addMask=None, varname=None, coordNames=None):
    """
    Preconditions: ESMP has been initialized.\n
    Postconditions: An ESMP_GridStruct has been created.\n
    Arguments:\n
        :RETURN: ESMP_GridStruct            :: gridstruct\n
        String                              :: filename\n
        FileFormat                          :: fileTypeFlag\n
            Argument Values:\n
                SCRIP\n
                GRIDSPEC\n
        List of Integers                    :: regDecomp\n
        List of Integers (optional)         :: decompflag\n
        Boolean (optional)                  :: isSphere\n
        Boolean (optional)                  :: addCornerStagger\n
        Boolean (optional)                  :: addUserArea\n
        Boolean (optional)                  :: addMask\n
        String (optional)                   :: varname\n
        List of Strings (optional)          :: coordNames\n
    """
    lrc = ct.c_int(0)

    # dummy value to correspond to ESMF_INDEX_GLOBAL = 1 for global indexing
    indexflag = 1

    gridstruct = _ESMF.ESMC_GridCreateFromFile(filename, fileTypeFlag,
                                               None, decompflag,
                                               isSphere, addCornerStagger,
                                               addUserArea, indexflag, addMask, varname,
                                               coordNames, ct.byref(lrc))
    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise NameError('ESMC_GridCreateFromFile() failed with rc = '+str(rc))
    return gridstruct
    
_ESMF.ESMC_GridDestroy.restype = ct.c_int
_ESMF.ESMC_GridDestroy.argtypes = [ct.c_void_p]
@deprecated
def ESMP_GridDestroy(grid):
    """
    Preconditions: An ESMP_Grid has been created.\n
    Postconditions: The 'grid' has been destroyed.\n
    Arguments:\n
        ESMP_Grid :: grid\n
    """
    ptr = ct.POINTER(ct.c_void_p)
    gridptr = ptr(ct.c_void_p(grid.struct.ptr))
    rc = _ESMF.ESMC_GridDestroy(gridptr)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_GridDestroy() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

_ESMF.ESMC_GridAddCoord.restype = ct.c_int
_ESMF.ESMC_GridAddCoord.argtypes = [ct.c_void_p, ct.c_uint]
@deprecated
def ESMP_GridAddCoord(grid, staggerloc=constants.StaggerLoc.CENTER):
    """
    Preconditions: An ESMP_Grid has been created.\n
    Postconditions: Space for coordinates have been allocated at the 
                    specified stagger location of the grid.\n
    Arguments:\n
        ESMP_Grid             :: grid\n
        StaggerLoc (optional) :: staggerloc\n
            Argument Values:\n
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
    """
    rc = _ESMF.ESMC_GridAddCoord(grid.struct.ptr, staggerloc)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_GridAddCoord() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

    # now we have to get the coordinate array bounds to set the grid.size
    coordDim = ct.c_int(1)
    lrc = ct.c_int(0)
    lbound = np.array(np.zeros(grid.rank),dtype=np.int32)
    ubound = np.array(np.zeros(grid.rank),dtype=np.int32)
    gridCoordPtr = _ESMF.ESMC_GridGetCoord(grid.struct.ptr,
                                           coordDim, staggerloc,
                                           lbound, ubound,
                                           ct.byref(lrc))
    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_GridGetCoord() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

    # adjust for 0 based bounds
    lbound = lbound - 1

_ESMF.ESMC_GridAddItem.restype = ct.c_int
_ESMF.ESMC_GridAddItem.argtypes = [ct.c_void_p, ct.c_uint, ct.c_uint]
@deprecated
def ESMP_GridAddItem(grid, item,
                     staggerloc=constants.StaggerLoc.CENTER):
    """
    Preconditions: An ESMP_Grid has been created.\n
    Postconditions: Space for a grid item has been allocated at the 
                    specified stagger location of the grid.  Grid items 
                    such as a mask or and area field can be added with 
                    this interface, and then retrieved and modified with 
                    ESMP_GridGetItem().\n
    Arguments:\n
        ESMP_Grid             :: grid\n
        GridItem              :: item\n
            Argument Values:\n
                GridItem.AREA\n
                GridItem.MASK\n
        StaggerLoc (optional) :: staggerloc\n
            Argument Values:\n
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
    """
    rc = _ESMF.ESMC_GridAddItem(grid.struct.ptr, item, staggerloc)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_GridAddItem() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

_ESMF.ESMC_GridGetCoord.restype = ct.POINTER(ct.c_void_p)
_ESMF.ESMC_GridGetCoord.argtypes = [ct.c_void_p, ct.c_int, ct.c_uint,
                                    np.ctypeslib.ndpointer(dtype=np.int32),
                                    np.ctypeslib.ndpointer(dtype=np.int32),
                                    ct.POINTER(ct.c_int)]
@deprecated
def ESMP_GridGetCoordPtr(grid, coordDim,
                         staggerloc=constants.StaggerLoc.CENTER):
    """
    Preconditions: An ESMP_Grid has been created and coordinates have 
                   been added via ESMP_GridAddCoord().\n
    Postconditions: A numpy array containing writeable Grid coordinate 
                    data has been returned into 'gridCoordPtr'\n
    Arguments:\n
        :RETURN: Numpy.array  :: gridCoordPtr\n
        ESMP_Grid             :: grid\n
        integer               :: coordDim\n
        StaggerLoc (optional) :: staggerloc\n
            Argument Values:\n
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
    """
    lrc = ct.c_int(0)

    # change coordDim to 1 based indexing
    lcd = ct.c_int(coordDim+1)

    # these are just placeholders in this routine..
    exLB = np.array(np.zeros(grid.rank),dtype=np.int32)
    exUB = np.array(np.zeros(grid.rank),dtype=np.int32)

    gridCoordPtr = _ESMF.ESMC_GridGetCoord(grid.struct.ptr, lcd, staggerloc,
                                           exLB, exUB, ct.byref(lrc))

    # adjust bounds to be 0 based, even though it's just a placeholder..
    exLB = exLB - 1

    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_GridGetCoord() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

    return gridCoordPtr


_ESMF.ESMC_GridGetCoordBounds.restype = ct.c_int
_ESMF.ESMC_GridGetCoordBounds.argtypes = [ct.c_void_p, ct.c_uint,
                                         np.ctypeslib.ndpointer(dtype=np.int32),
                                         np.ctypeslib.ndpointer(dtype=np.int32),
                                         ct.POINTER(ct.c_int)]
@deprecated
def ESMP_GridGetCoordBounds(grid, staggerloc=constants.StaggerLoc.CENTER):
    """
    Preconditions: An ESMP_Grid has been created and coordinates have 
                   been added via ESMP_GridAddCoord().\n
    Postconditions: Two numpy arrays containing the grid coordinate 
                    bounds have been returned in a tuple.\n
    Arguments:\n
        :RETURN: Numpy.array  :: exclusiveLBound\n
        :RETURN: Numpy.array  :: exclusiveUBound\n
        ESMP_Grid             :: grid\n
        StaggerLoc (optional) :: staggerloc\n
            Argument Values:\n
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
    """
    lrc = ct.c_int(0)
    exclusiveLBound = np.array(np.zeros(grid.rank),dtype=np.int32)
    exclusiveUBound = np.array(np.zeros(grid.rank),dtype=np.int32)
    rc = _ESMF.ESMC_GridGetCoordBounds(grid.struct.ptr, staggerloc,
                                       exclusiveLBound, exclusiveUBound,
                                       ct.byref(lrc))

    # adjust bounds to be 0 based
    exclusiveLBound = exclusiveLBound - 1

    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_GridGetCoord() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

    return exclusiveLBound, exclusiveUBound

_ESMF.ESMC_GridGetItem.restype = ct.POINTER(ct.c_void_p)
_ESMF.ESMC_GridGetItem.argtypes = [ct.c_void_p, ct.c_uint, ct.c_uint,
                                   ct.POINTER(ct.c_int)]
@deprecated
def ESMP_GridGetItem(grid, item, staggerloc=constants.StaggerLoc.CENTER):
    """
    Preconditions: An ESMP_Grid has been created and an appropriate 
                   item has been added via ESMP_GridAddItem().  The 
                   Grid must have coordinates added via 
                   ESMP_GridAddCoord() for this call to succeed.\n
    Postconditions: A pointer to the data array containing the item is 
                    returned.  This can be used to modify a grid item, 
                    such as a mask or area field.\n
    Arguments:\n
        :RETURN: Numpy.array  :: mask or area\n
        ESMP_Grid             :: grid\n
        GridItem              :: item\n
            Argument Values:\n
                GridItem.AREA\n
                GridItem.MASK\n
        StaggerLoc (optional) :: staggerloc\n
            Argument Values:\n
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
    """
    lrc = ct.c_int(0)
    gridItemPtr = _ESMF.ESMC_GridGetItem(grid.struct.ptr, item, staggerloc,
                                         ct.byref(lrc))

    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_GridGetItem() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

    return gridItemPtr

_ESMF.ESMC_GridWrite.restype = ct.c_int
_ESMF.ESMC_GridWrite.argtypes = [ct.c_void_p, ct.c_uint,ct.c_char_p]
@deprecated
def ESMP_GridWrite(grid, filename, staggerloc=constants.StaggerLoc.CENTER):
    """
    Preconditions: An ESMP_Grid has been created.\n
    Postconditions: The grid has been written to file in vtk format.\n
    Arguments:\n
        ESMP_Grid             :: grid\n
        string                :: filename\n
        StaggerLoc (optional) :: staggerloc\n
            Argument Values:\n
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
    """
    rc = _ESMF.ESMC_GridWrite(grid.struct.ptr, staggerloc, filename)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_GridWrite() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

#### MESH #####################################################

_ESMF.ESMC_MeshAddElements.restype = ct.c_int
_ESMF.ESMC_MeshAddElements.argtypes = [ct.c_void_p, ct.c_int,
                                       np.ctypeslib.ndpointer(dtype=np.int32),
                                       np.ctypeslib.ndpointer(dtype=np.int32),
                                       np.ctypeslib.ndpointer(dtype=np.int32),
                                       OptionalNumpyArrayInt32,
                                       OptionalNumpyArrayFloat64,
                                       OptionalNumpyArrayFloat64]
@deprecated
def ESMP_MeshAddElements(mesh, elementCount,
                         elementIds, elementTypes,
                         elementConn,
                         elementMask=None,
                         elementArea=None, elementCoords=None):
    """
    Preconditions: An ESMP_Mesh has been created.  'elementIds' holds 
                   the IDs of the elements, 'elementTypes' holds the 
                   types of the elements, and 'elementConn' holds the 
                   indices of the locations in the 'nodeIDs' 
                   array from ESMP_MeshAddNodes() which correspond to 
                   the nodes which make up this element's connectivity.  
                   Optional arguments 'elementMask' and 'elementArea' 
                   hold the mask and areas of the elements, 
                   respectively.\n
    Postconditions: Elements have been added to 'mesh', this should 
                    only be called once.\n
    Arguments:\n
        ESMP_Mesh                             :: mesh\n
        integer                               :: elementCount\n
        Numpy.array(dtype=int32)              :: elementIds\n
        Numpy.array(dtype=int32)              :: elementTypes\n
        Numpy.array(dtype=int32)              :: elementConn\n
        Numpy.array(dtype=int32) (optional)   :: elementMask\n
        Numpy.array(dtype=float64) (optional) :: elementArea\n
        Numpy.array(dtype=float64) (optional) :: elementCoords\n
    """
    lec = ct.c_int(elementCount)
    # ESMC expects the elementConn array to be 1 based..
    elementConn = elementConn + 1
    rc = _ESMF.ESMC_MeshAddElements(mesh.struct.ptr, lec,
                                    elementIds, elementTypes,
                                    elementConn, elementMask, elementArea,
                                    elementCoords)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_MeshAddElement() failed with rc = '+str(rc)+
                        '.    '+constants._errmsg)

_ESMF.ESMC_MeshAddNodes.restype = ct.c_int
_ESMF.ESMC_MeshAddNodes.argtypes = [ct.c_void_p, ct.c_int,
                                    np.ctypeslib.ndpointer(dtype=np.int32),
                                    np.ctypeslib.ndpointer(dtype=np.float64),
                                    np.ctypeslib.ndpointer(dtype=np.int32)]
@deprecated
def ESMP_MeshAddNodes(mesh, nodeCount,
                      nodeIds, nodeCoords, nodeOwners):
    """
    Preconditions: An ESMP_Mesh has been created.  'nodeIds' holds the 
                   IDs of the nodes, 'nodeCoords' holds the coordinates 
                   of the nodes, and 'nodeOwners' holds the number 
                   (0-based) of the processor which owns this node.\n
    Postconditions: Nodes have been added to 'mesh', this should only 
                    be called once.\n
    Arguments:\n
        ESMP_Mesh                  :: mesh\n
        integer                    :: nodeCount\n
        Numpy.array(dtype=int32)   :: nodeIds\n
        Numpy.array(dtype=float64) :: nodeCoords\n
        Numpy.array(dtype=int32)   :: nodeOwners\n
    """
    lnc = ct.c_int(nodeCount)

    # copy the numpy arrays to a specific type
    nodeIdsD = np.array(nodeIds, dtype=np.int32)
    nodeCoordsD = np.array(nodeCoords, dtype=np.float64)
    # this variant uses the ndarray.astype casting function
    nodeOwnersD = np.ndarray.astype(nodeOwners, np.int32)

    rc = _ESMF.ESMC_MeshAddNodes(mesh.struct.ptr, lnc,
                                 nodeIdsD, nodeCoordsD, nodeOwnersD)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_MeshAddNodes() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

_ESMF.ESMC_MeshCreate.restype = ESMP_Mesh
_ESMF.ESMC_MeshCreate.argtypes = [ct.c_int, ct.c_int,
                                  OptionalNamedConstant, ct.POINTER(ct.c_int)]
@deprecated
def ESMP_MeshCreate(parametricDim, spatialDim, coordSys=None):
    """
    Preconditions: ESMP has been initialized.\n
    Postconditions: An ESMP_Mesh has been created.\n
    Arguments:\n
        :RETURN: ESMP_Mesh    :: mesh\n
        integer               :: parametricDim\n
        integer               :: spatialDim\n
        CoordSys (optional)   :: coordSys\n
            Argument Values:\n
                (default) CoordSys.CART\n
                CoordSys.SPH_DEG\n
                CoordSys.SPH_RAD\n

    """
    lrc = ct.c_int(0)
    # NOTE: for some reason the default argument does not come through correctly
    coordSys = coordSys or constants.CoordSys.CART
    mesh = _ESMF.ESMC_MeshCreate(parametricDim, spatialDim, coordSys, ct.byref(lrc))
    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_MeshCreate() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

    # handle the ctypes structure
    return mesh

_ESMF.ESMC_MeshCreateFromFile.restype = ESMP_Mesh
_ESMF.ESMC_MeshCreateFromFile.argtypes = [ct.c_char_p, ct.c_int,
                                          OptionalNamedConstant,
                                          OptionalNamedConstant,
                                          ct.c_char_p,
                                          OptionalNamedConstant,
                                          ct.c_char_p]
@deprecated
@netcdf
def ESMP_MeshCreateFromFile(filename, fileTypeFlag,
                            convertToDual=None, addUserArea=None,
                            meshname="", maskFlag=None, varname=""):
    """
    Preconditions: ESMP has been initialized.\n
    Postconditions: An ESMP_Mesh has been created.\n
    Arguments:\n
        :RETURN: ESMP_Mesh :: mesh\n
        string             :: filename\n
        FileFormat         :: fileTypeFlag\n
            Argument Values:\n
                FileFormat.SCRIP\n
                FileFormat.ESMFMESH\n
                FileFormat.UGRID\n
        bool (optional)    :: convertToDual\n
        bool (optional)    :: addUserArea\n
        string (optional)  :: meshname\n
        MeshLoc (optional) :: maskFlag\n
            Argument Values:\n
                MeshLoc.NODE\n
                MeshLoc.ELEMENT\n
        string (optional)  :: varname\n
        """
    lrc = ct.c_int(0)
    mesh = _ESMF.ESMC_MeshCreateFromFile(filename, fileTypeFlag,
                                         convertToDual, addUserArea,
                                         meshname, maskFlag, varname,
                                         ct.byref(lrc))
    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_MeshCreateFromFile() failed with rc = '+str(rc)+
                        '.    '+constants._errmsg)

    # copy to the mesh struct
    return mesh

_ESMF.ESMC_MeshDestroy.restype = ct.c_int
_ESMF.ESMC_MeshDestroy.argtypes = [ct.c_void_p]
@deprecated
def ESMP_MeshDestroy(mesh):
    """
    :KNOWN BUG: This function does not work if ESMP_MeshFreeMemory has 
                previously been called.\n
    Preconditions: An ESMP_Mesh has been created.\n
    Postconditions: The 'mesh' has been destroyed.\n
    Arguments:\n
        ESMP_Mesh :: mesh\n
    """
    ptr = ct.POINTER(ct.c_void_p)
    meshptr = ptr(ct.c_void_p(mesh.struct.ptr))
    rc = _ESMF.ESMC_MeshDestroy(meshptr)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_MeshDestroy() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

_ESMF.ESMC_MeshFreeMemory.restype = ct.c_int
_ESMF.ESMC_MeshFreeMemory.argtypes = [ct.c_void_p]
@deprecated
def ESMP_MeshFreeMemory(mesh):
    """
    :KNOWN BUG: This function does not work if called on an ESMP_Mesh 
                that has not had nodes or elements added.\n
    Preconditions: An ESMP_Mesh has been created and nodes or elements 
                   have been added to the Mesh.\n
    Postconditions: The noncritical information used to create 'mesh' 
                    has been released back to the heap.\n
    Arguments:\n
        ESMP_Mesh :: mesh\n
    """
    ptr = ct.POINTER(ct.c_void_p)
    meshptr = ptr(ct.c_void_p(mesh.struct.ptr))
    rc = _ESMF.ESMC_MeshFreeMemory(meshptr)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_MeshFreeMemory() failed with rc = '+str(rc)+
                        '.    '+constants._errmsg)

_ESMF.ESMC_MeshGetCoord.restype = None
_ESMF.ESMC_MeshGetCoord.argtypes = [ct.c_void_p,
                                    np.ctypeslib.ndpointer(dtype=np.float64),
                                    ct.POINTER(ct.c_int),
                                    ct.POINTER(ct.c_int), ct.POINTER(ct.c_int)]
@deprecated
def ESMP_MeshGetCoordPtr(mesh):
    """
    Preconditions: An ESMP_Mesh has been created with coordinates 
                   specified.\n
    Postconditions: An array containing Mesh coordinate data has been
                    returned into 'nodeCoords', number of nodes in
                    'num_nodes', and number of dimensions in 'num_dims'.\n
    Arguments:\n
        :RETURN: Numpy.array(dtype=float64) :: nodeCoords\n
        :RETURN: int             :: num_nodes\n
        :RETURN: int             :: num_dims\n
        ESMP_Mesh                :: mesh\n
    """
    lrc = ct.c_int(0)
    lnum_nodes = ct.c_int(0)
    lnum_dims = ct.c_int(0)
    num_nodes = ESMP_MeshGetLocalNodeCount(mesh)
    nodeCoords = np.array(np.zeros(num_nodes*3),dtype=np.float64)
    _ESMF.ESMC_MeshGetCoord(mesh.struct.ptr, nodeCoords,
                            ct.byref(lnum_nodes), 
                            ct.byref(lnum_dims), ct.byref(lrc))
    num_nodes = lnum_nodes.value
    num_dims = lnum_dims.value
    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_MeshGetCoord() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)
    return nodeCoords, num_nodes, num_dims

_ESMF.ESMC_MeshGetElemCoord.restype = None
_ESMF.ESMC_MeshGetElemCoord.argtypes = [ct.c_void_p,
                                    np.ctypeslib.ndpointer(dtype=np.float64),
                                    ct.POINTER(ct.c_int),
                                    ct.POINTER(ct.c_int), ct.POINTER(ct.c_int)]
def ESMP_MeshGetElemCoordPtr(mesh):
    """
    Preconditions: An ESMP_Mesh has been created with element coordinates 
                   specified.\n
    Postconditions: An array containing Mesh element coordinate data has been
                    returned into 'elemCoords', number of elements in
                    'num_elems', and number of dimensions in 'num_dims'.\n
    Arguments:\n
        :RETURN: Numpy.array(dtype=float64) :: elemCoords\n
        :RETURN: int             :: num_elems\n
        :RETURN: int             :: num_dims\n
        ESMP_Mesh                :: mesh\n
    """
    lrc = ct.c_int(0)
    lnum_elems = ct.c_int(0)
    lnum_dims = ct.c_int(0)
    num_elems = ESMP_MeshGetLocalElementCount(mesh)
    elemCoords = np.array(np.zeros(num_elems*3),dtype=np.float64)
    _ESMF.ESMC_MeshGetElemCoord(mesh.struct.ptr, elemCoords,
                            ct.byref(lnum_elems), 
                            ct.byref(lnum_dims), ct.byref(lrc))
    num_elems = lnum_elems.value
    num_dims = lnum_dims.value
    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_MeshGetElemCoord() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)
    return elemCoords, num_elems, num_dims

_ESMF.ESMC_MeshGetConnectivity.restype = None
_ESMF.ESMC_MeshGetConnectivity.argtypes = [ct.c_void_p,
                                    np.ctypeslib.ndpointer(dtype=np.float64),
                                    np.ctypeslib.ndpointer(dtype=np.int32),
                                    ct.POINTER(ct.c_int)]
def ESMP_MeshGetConnectivityPtr(mesh):
    """
    Preconditions: An ESMP_Mesh has been created.\n
    Postconditions: An array containing the Mesh connectivity is
                    returned into 'connCoord'.\n
    Arguments:\n
        :RETURN: Numpy.array(dtype=float64) :: connCoord\n
        ESMP_Mesh                           :: mesh\n
    """
    lrc = ct.c_int(0)
    num_elems = ESMP_MeshGetLocalElementCount(mesh)
    num_nodes = ESMP_MeshGetLocalNodeCount(mesh)
    # NOTE: the size of the connectivity array is hardcoded way too big to handle the GIS data case until we can
    #       pull the correct info from file (nMaxMesh2_face_nodes*faces)
    connCoord = np.zeros(num_nodes*num_elems*3, dtype=np.float64)
    nodesPerElem = np.zeros(num_elems*num_nodes, dtype=np.int32)
    _ESMF.ESMC_MeshGetConnectivity(mesh.struct.ptr, connCoord, nodesPerElem, ct.byref(lrc))

    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_MeshGetConnectivity() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)
    return connCoord, nodesPerElem

_ESMF.ESMC_MeshGetLocalElementCount.restype = ct.c_int
_ESMF.ESMC_MeshGetLocalElementCount.argtypes = [ct.c_void_p, 
                                                ct.POINTER(ct.c_int)]
@deprecated
def ESMP_MeshGetLocalElementCount(mesh):
    """
    Preconditions: An ESMP_Mesh has been created.\n
    Postconditions: The local elementCount for 'mesh' has been 
                    returned.\n
    Arguments:\n
        :RETURN: integer :: elementCount\n
        ESMP_Mesh        :: mesh\n
    """
    lec = ct.c_int(0)
    rc = _ESMF.ESMC_MeshGetLocalElementCount(mesh.struct.ptr, ct.byref(lec))
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_MeshGetLocalElementCount() failed with rc = '+
                        str(rc)+'.    '+constants._errmsg)
    elementCount = lec.value
    return elementCount

_ESMF.ESMC_MeshGetLocalNodeCount.restype = ct.c_int
_ESMF.ESMC_MeshGetLocalNodeCount.argtypes = [ct.c_void_p, ct.POINTER(ct.c_int)]
@deprecated
def ESMP_MeshGetLocalNodeCount(mesh):
    """
    Preconditions: An ESMP_Mesh has been created.\n
    Postconditions: The local nodeCount for 'mesh' is returned.\n
    Arguments:\n
        :RETURN: integer :: nodeCount\n
        ESMP_Mesh        :: mesh\n
    """
    lnc = ct.c_int(0)
    rc = _ESMF.ESMC_MeshGetLocalNodeCount(mesh.struct.ptr, ct.byref(lnc))
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_MeshGetLocalNodeCount() failed with rc = '+
                        str(rc)+'.    '+constants._errmsg)
    nodeCount = lnc.value
    return nodeCount

_ESMF.ESMC_MeshGetOwnedElementCount.restype = ct.c_int
_ESMF.ESMC_MeshGetOwnedElementCount.argtypes = [ct.c_void_p, 
                                                ct.POINTER(ct.c_int)]
@deprecated
def ESMP_MeshGetOwnedElementCount(mesh):
    """
    Preconditions: An ESMP_Mesh has been created.\n
    Postconditions: The owned elementCount for 'mesh' has been 
                    returned.\n
    Arguments:\n
        :RETURN: integer :: elementCount\n
        ESMP_Mesh        :: mesh\n
    """
    lec = ct.c_int(0)
    rc = _ESMF.ESMC_MeshGetOwnedElementCount(mesh.struct.ptr, ct.byref(lec))
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_MeshGetOwnedElementCount() failed with rc = '+
                        str(rc)+'.    '+constants._errmsg)
    elementCount = lec.value
    return elementCount

_ESMF.ESMC_MeshGetOwnedNodeCount.restype = ct.c_int
_ESMF.ESMC_MeshGetOwnedNodeCount.argtypes = [ct.c_void_p, ct.POINTER(ct.c_int)]
@deprecated
def ESMP_MeshGetOwnedNodeCount(mesh):
    """
    Preconditions: An ESMP_Mesh has been created.\n
    Postconditions: The owned nodeCount for 'mesh' is returned.\n
    Arguments:\n
        :RETURN: integer :: nodeCount\n
        ESMP_Mesh        :: mesh\n
    """
    lnc = ct.c_int(0)
    rc = _ESMF.ESMC_MeshGetOwnedNodeCount(mesh.struct.ptr, ct.byref(lnc))
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_MeshGetOwnedNodeCount() failed with rc = '+
                        str(rc)+'.    '+constants._errmsg)
    nodeCount = lnc.value
    return nodeCount

_ESMF.ESMC_MeshWrite.restype = ct.c_int
_ESMF.ESMC_MeshWrite.argtypes = [ct.c_void_p, ct.c_char_p]
@deprecated
def ESMP_MeshWrite(mesh, filename):
    """
    Preconditions: An ESMP_Mesh has been created.\n
    Postconditions: The mesh has been written to file in vtk format.\n
    Arguments:\n
        ESMP_Mesh :: mesh\n
        string    :: filename\n
    """
    rc = _ESMF.ESMC_MeshWrite(mesh.struct.ptr, filename)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_MeshWrite() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

#### LOCSTREAM #####################################################

_ESMF.ESMC_LocStreamCreateLocal.restype = ESMP_LocStream
_ESMF.ESMC_LocStreamCreateLocal.argtypes = [ct.c_int,
                                            OptionalNamedConstant,
                                            OptionalNamedConstant,
                                            ct.POINTER(ct.c_int)]
def ESMP_LocStreamCreateLocal(localCount, coordSys=None):
    """
    Preconditions: ESMP has been initialized.\n
    Postconditions: An ESMP_LocStream has been created.\n
    Arguments:\n
        :RETURN: ESMP_LocStream  :: locstream\n
        Integer                  :: localCount \n
        CoordSys (optional)      :: coordSys\n
            Argument Values:\n
                (default) CoordSys.CART\n
                CoordSys.SPH_DEG\n
                CoordSys.SPH_RAD\n

    """
    lrc = ct.c_int(0)

    # NOTE: for some reason the default argument does not come through correctly
    coordSys = coordSys or constants.CoordSys.CART

    # dummy value to correspond to ESMF_INDEX_GLOBAL = 1 for global indexing
    indexflag = 1

    # create the ESMF Grid and retrieve a ctypes pointer to it
    locstream = _ESMF.ESMC_LocStreamCreateLocal(localCount, indexflag, coordSys, ct.byref(lrc))

    # check the return code from ESMF
    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_LocStreamCreateLocal() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

    # create the ESMP LocStream object from ctypes pointer
    return locstream

_ESMF.ESMC_LocStreamGetBounds.restype = ct.c_int
_ESMF.ESMC_LocStreamGetBounds.argtypes = [ct.c_void_p,
                                          ct.c_int,
                                          np.ctypeslib.ndpointer(dtype=np.int32),
                                          np.ctypeslib.ndpointer(dtype=np.int32)]
def ESMP_LocStreamGetBounds(locstream, localDe=0):
    """
    Preconditions: An ESMP_LocStream has been created.\n
    Postconditions: .\n
    Arguments:\n
        :RETURN: Numpy.array  :: \n
        :RETURN: Numpy.array  :: \n
        ESMP_LocStream        :: locstream\n
    """
    llde = ct.c_int(localDe)

    # locstream rank is always one
    locstreamrank = 1

    exLB = np.zeros(locstreamrank, dtype=np.int32)
    exUB = np.zeros(locstreamrank, dtype=np.int32)

    rc = _ESMF.ESMC_LocStreamGetBounds(locstream.ptr, llde, exLB, exUB)

    # adjust bounds to be 0 based
    exLB = exLB - 1

    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_LocStreamGetBounds() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

    return exLB, exUB

_ESMF.ESMC_LocStreamAddKeyAlloc.restype = ct.c_int
_ESMF.ESMC_LocStreamAddKeyAlloc.argtypes = [ct.c_void_p, ct.c_char_p,
                                            OptionalNamedConstant]
def ESMP_LocStreamAddKeyAlloc(locstream, keyName, keyTypeKind=None):
    """
    Preconditions: An ESMP_LocStream has been created.\n
    Postconditions: .\n
    Arguments:\n
        :RETURN: integer      :: \n
        ESMP_LocStream        :: locstream\n
        String                :: keyName\n
        TypeKind (optional)   :: keyTypeKind\n
            Argument Values:\n
                TypeKind.I4\n
                TypeKind.I8\n
                TypeKind.R4\n
                (default) TypeKind.R8\n

    """

    rc = _ESMF.ESMC_LocStreamAddKeyAlloc(locstream.ptr, keyName, keyTypeKind)

    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_LocStreamAddKeyAlloc() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

_ESMF.ESMC_LocStreamGetKeyPtr.restype = ct.POINTER(ct.c_void_p)
_ESMF.ESMC_LocStreamGetKeyPtr.argtypes = [ct.c_void_p,
                                          ct.c_char_p,
                                          ct.c_int,
                                          ct.POINTER(ct.c_int)]
def ESMP_LocStreamGetKeyPtr(locstream, keyName, localDe=0):
    """
    Preconditions: An ESMP_LocStream has been created.\n
    Postconditions: .\n
    Arguments:\n
        ESMP_LocStream        :: locstream\n
    """
    lrc = ct.c_int(0)
    llde = ct.c_int(localDe)

    keyPtr = _ESMF.ESMC_LocStreamGetKeyPtr(locstream.ptr, keyName, llde, lrc)

    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_LocStreamGetKeyPtr() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

    return keyPtr

_ESMF.ESMC_LocStreamDestroy.restype = ct.c_int
_ESMF.ESMC_LocStreamDestroy.argtypes = [ct.c_void_p]
def ESMP_LocStreamDestroy(locstream):
    """
    Preconditions: An ESMP_LocStream has been created.\n
    Postconditions: The 'locstream' has been destroyed.\n
    Arguments:\n
        ESMP_LocStream :: locstream\n
    """
    ptr = ct.POINTER(ct.c_void_p)
    locstreamptr = ptr(ct.c_void_p(locstream.struct.ptr))
    rc = _ESMF.ESMC_LocStreamDestroy(locstreamptr)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_LocStreamDestroy() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)


#### Field #####################################################

_ESMF.ESMC_FieldCreateGridTypeKind.restype = ESMP_Field
_ESMF.ESMC_FieldCreateGridTypeKind.argtypes = [ct.c_void_p, ct.c_uint,
                                               ct.c_uint, OptionalStructPointer,
                                               OptionalStructPointer, 
                                               OptionalStructPointer,
                                               ct.c_char_p, 
                                               ct.POINTER(ct.c_int)]
@deprecated
def ESMP_FieldCreateGrid(grid, name=None,
                         typekind=constants.TypeKind.R8,
                         staggerloc=constants.StaggerLoc.CENTER,
                         gridToFieldMap=None,
                         ungriddedLBound=None,
                         ungriddedUBound=None):
    """
    Preconditions: ESMP has been initialized and an ESMP_Grid has 
                   been created.\n
    Postconditions: An ESMP_Field has been created.\n
    Arguments:\n
        :RETURN: ESMP_Field   :: field\n
        ESMP_Grid             :: grid\n
        string (optional)     :: name\n
        TypeKind (optional)   :: typekind\n
            Argument Values:\n
                TypeKind.I4\n
                TypeKind.I8\n
                TypeKind.R4\n
                (default) TypeKind.R8\n
        StaggerLoc (optional) :: staggerloc\n
            Argument Values:\n
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
        Numpy.array(dtype=int32) (optional) :: gridToFieldMap\n
        Numpy.array(dtype=int32) (optional) :: ungriddedLBound\n
        Numpy.array(dtype=int32) (optional) :: ungriddedUBound\n
    """
    # local parameters
    lrc = ct.c_int(0)

    # InterfaceInt requires int32 type numpy arrays
    gridToFieldMap_i = gridToFieldMap
    if (gridToFieldMap is not None):
        if (gridToFieldMap.dtype != np.int32):
            raise TypeError('gridToFieldMap must have dtype=int32')
        gridToFieldMap_i = ESMP_InterfaceInt(gridToFieldMap)

    # InterfaceInt requires int32 type numpy arrays
    ungriddedLBound_i = ungriddedLBound
    if (ungriddedLBound is not None):
        if (ungriddedLBound.dtype != np.int32):
            raise TypeError('ungriddedLBound must have dtype=int32')
        ungriddedLBound_i = ESMP_InterfaceInt(ungriddedLBound)

    # InterfaceInt requires int32 type numpy arrays
    ungriddedUBound_i = ungriddedUBound
    if (ungriddedUBound is not None):
        if (ungriddedUBound.dtype != np.int32):
            raise TypeError('ungriddedUBound must have dtype=int32')
        ungriddedUBound_i = ESMP_InterfaceInt(ungriddedUBound)

    # call into the FieldCreate C interface
    field = _ESMF.ESMC_FieldCreateGridTypeKind(grid.struct.ptr, typekind, 
                                               staggerloc,
                                               gridToFieldMap_i, 
                                               ungriddedLBound_i,
                                               ungriddedUBound_i, 
                                               name, ct.byref(lrc))
    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_FieldCreateGridTK() failed with rc = '+str(rc)+
                        '.    '+constants._errmsg)

    return field

_ESMF.ESMC_FieldCreateLocStreamTypeKind.restype = ESMP_Field
_ESMF.ESMC_FieldCreateLocStreamTypeKind.argtypes = [ct.c_void_p,
                                                    ct.c_uint,
                                                    OptionalStructPointer,
                                                    OptionalStructPointer,
                                                    OptionalStructPointer,
                                                    ct.c_char_p,
                                                    ct.POINTER(ct.c_int)]
def ESMP_FieldCreateLocStream(locstream, name=None,
                     typekind=constants.TypeKind.R8,
                     gridToFieldMap=None,
                     ungriddedLBound=None,
                     ungriddedUBound=None):
    """
    Preconditions: ESMP has been initialized and an ESMP_LocStream has
                   been created.\n
    Postconditions: An ESMP_Field has been created.\n
    Arguments:\n
        :RETURN: ESMP_Field   :: field\n
        ESMP_LocStream        :: locstream\n
        string (optional)     :: name\n
        TypeKind (optional)   :: typekind\n
            Argument Values:\n
                TypeKind.I4\n
                TypeKind.I8\n
                TypeKind.R4\n
                (default) TypeKind.R8\n
        Numpy.array(dtype=int32) (optional) :: gridToFieldMap\n
        Numpy.array(dtype=int32) (optional) :: ungriddedLBound\n
        Numpy.array(dtype=int32) (optional) :: ungriddedUBound\n
    """
    lrc = ct.c_int(0)

    # InterfaceInt requires int32 type numpy arrays
    gridToFieldMap_i = gridToFieldMap
    if (gridToFieldMap is not None):
        if (gridToFieldMap.dtype != np.int32):
            raise TypeError('gridToFieldMap must have dtype=int32')
        gridToFieldMap_i = ESMP_InterfaceInt(gridToFieldMap)

    # InterfaceInt requires int32 type numpy arrays
    ungriddedLBound_i = ungriddedLBound
    if (ungriddedLBound is not None):
        if (ungriddedLBound.dtype != np.int32):
            raise TypeError('ungriddedLBound must have dtype=int32')
        ungriddedLBound_i = ESMP_InterfaceInt(ungriddedLBound)

    # InterfaceInt requires int32 type numpy arrays
    ungriddedUBound_i = ungriddedUBound
    if (ungriddedUBound is not None):
        if (ungriddedUBound.dtype != np.int32):
            raise TypeError('ungriddedUBound must have dtype=int32')
        ungriddedUBound_i = ESMP_InterfaceInt(ungriddedUBound)

    # call into the FieldCreate C interface
    field = _ESMF.ESMC_FieldCreateLocStreamTypeKind(locstream.struct.ptr, typekind,
                                               gridToFieldMap_i,
                                               ungriddedLBound_i,
                                               ungriddedUBound_i, name,
                                               ct.byref(lrc))
    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_FieldCreateLocStreamTK() failed with rc = ' + \
                        str(rc) + '.    ' + constants._errmsg)

    return field

_ESMF.ESMC_FieldCreateMeshTypeKind.restype = ESMP_Field
_ESMF.ESMC_FieldCreateMeshTypeKind.argtypes = [ct.c_void_p, ct.c_uint,
                                               ct.c_uint,
                                               OptionalStructPointer,
                                               OptionalStructPointer,
                                               OptionalStructPointer,
                                               ct.c_char_p,
                                               ct.POINTER(ct.c_int)]
def ESMP_FieldCreateMesh(mesh, name=None,
                     typekind=constants.TypeKind.R8,
                     meshloc=constants.MeshLoc.NODE,
                     gridToFieldMap=None,
                     ungriddedLBound=None,
                     ungriddedUBound=None):
    """
    Preconditions: ESMP has been initialized and an ESMP_Mesh has
                   been created.\n
    Postconditions: An ESMP_Field has been created.\n
    Arguments:\n
        :RETURN: ESMP_Field   :: field\n
        ESMP_Mesh             :: mesh\n
        string (optional)     :: name\n
        TypeKind (optional)   :: typekind\n
            Argument Values:\n
                TypeKind.I4\n
                TypeKind.I8\n
                TypeKind.R4\n
                (default) TypeKind.R8\n
        MeshLoc    (optional) :: meshloc\n
            Argument Values:\n
                (default) MeshLoc.NODE\n
                MeshLoc.ELEMENT\n
        Numpy.array(dtype=int32) (optional) :: gridToFieldMap\n
        Numpy.array(dtype=int32) (optional) :: ungriddedLBound\n
        Numpy.array(dtype=int32) (optional) :: ungriddedUBound\n
    """
    lrc = ct.c_int(0)

    # InterfaceInt requires int32 type numpy arrays
    gridToFieldMap_i = gridToFieldMap
    if (gridToFieldMap is not None):
        if (gridToFieldMap.dtype != np.int32):
            raise TypeError('gridToFieldMap must have dtype=int32')
        gridToFieldMap_i = ESMP_InterfaceInt(gridToFieldMap)

    # InterfaceInt requires int32 type numpy arrays
    ungriddedLBound_i = ungriddedLBound
    if (ungriddedLBound is not None):
        if (ungriddedLBound.dtype != np.int32):
            raise TypeError('ungriddedLBound must have dtype=int32')
        ungriddedLBound_i = ESMP_InterfaceInt(ungriddedLBound)

    # InterfaceInt requires int32 type numpy arrays
    ungriddedUBound_i = ungriddedUBound
    if (ungriddedUBound is not None):
        if (ungriddedUBound.dtype != np.int32):
            raise TypeError('ungriddedUBound must have dtype=int32')
        ungriddedUBound_i = ESMP_InterfaceInt(ungriddedUBound)

    # call into the FieldCreate C interface
    field = _ESMF.ESMC_FieldCreateMeshTypeKind(mesh.struct.ptr, typekind,
                                               meshloc,
                                               gridToFieldMap_i,
                                               ungriddedLBound_i,
                                               ungriddedUBound_i, name,
                                               ct.byref(lrc))
    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_FieldCreateMeshTK() failed with rc = ' + \
                        str(rc) + '.    ' + constants._errmsg)

    return field

_ESMF.ESMC_FieldDestroy.restype = ct.c_int
_ESMF.ESMC_FieldDestroy.argtypes = [ct.c_void_p]
@deprecated
def ESMP_FieldDestroy(field):
    """
    Preconditions: An ESMP_Field has been created.\n
    Postconditions: The 'field' has been destroyed.\n
    Arguments:\n
        ESMP_Field :: field\n
    """
    ptr = ct.POINTER(ct.c_void_p)
    fieldptr = ptr(ct.c_void_p(field.struct.ptr))
    rc = _ESMF.ESMC_FieldDestroy(fieldptr)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_FieldDestroy() failed with rc = ' + \
                                        str(rc) + '.    ' + constants._errmsg)

_ESMF.ESMC_FieldGetPtr.restype = ct.POINTER(ct.c_void_p)
_ESMF.ESMC_FieldGetPtr.argtypes = [ct.c_void_p, ct.c_int, ct.POINTER(ct.c_int)]
@deprecated
def ESMP_FieldGetPtr(field, localDe=0):
    """
    Preconditions: An ESMP_Field has been created.\n
    Postconditions: A numpy array containing writeable Field data has been
                    returned into 'fieldPtr'\n
    Arguments:\n
        :RETURN: Numpy.array      :: fieldPtr\n
        ESMP_Field                :: field\n
        integer        (optional) :: localDe\n
    """
    lrc = ct.c_int(0)
    fieldPtr = _ESMF.ESMC_FieldGetPtr(field.ptr, localDe, ct.byref(lrc))
    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_FieldGetPtr failed! rc = \n'+str(rc)+'.    '+
                        constants._errmsg)

    return fieldPtr

_ESMF.ESMC_FieldGetBounds.restype = ct.c_int
_ESMF.ESMC_FieldGetBounds.argtypes = [ct.c_void_p, ct.POINTER(ct.c_int),
                                      np.ctypeslib.ndpointer(dtype=np.int32),
                                      np.ctypeslib.ndpointer(dtype=np.int32),
                                      ct.c_int]
@deprecated
def ESMP_FieldGetBounds(field, rank, localDe=0):
    """
    Preconditions: An ESMP_Field has been created.\n
    Postconditions: Two numpy arrays containing the Field coordinate
                    bounds have been returned in a tuple.\n
    Arguments:\n
        :RETURN: Numpy.array  :: exclusiveLBound\n
        :RETURN: Numpy.array  :: exclusiveUBound\n
        ESMP_Field            :: field\n
        integer    (optional) :: localDe\n
    """
    llde = ct.c_int(localDe)

    exLB = np.zeros(rank, dtype=np.int32)
    exUB = np.zeros(rank, dtype=np.int32)

    rc = _ESMF.ESMC_FieldGetBounds(field.ptr, ct.byref(llde), exLB, exUB, rank)

    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_FieldGetBounds failed! rc = \n' + str(rc) + '.    ' +
                     constants._errmsg)

    # adjust bounds to be 0 based
    exLB = exLB - 1

    return exLB, exUB

_ESMF.ESMC_FieldPrint.restype = ct.c_int
_ESMF.ESMC_FieldPrint.argtypes = [ct.c_void_p]
@deprecated
def ESMP_FieldPrint(field):
    """
    Preconditions: An ESMP_Field has been created.\n
    Postconditions: The contents of 'field' have been printed to
                    standard out.\n
    Arguments:\n
        ESMP_Field :: field\n
    """
    rc = _ESMF.ESMC_FieldPrint(field.ptr)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_FieldPrint() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)

_ESMF.ESMC_FieldRead.restype = ct.c_int
_ESMF.ESMC_FieldRead.argtypes = [ct.c_void_p,
                                 ct.c_char_p,
                                 ct.c_char_p,
                                 ct.c_uint,
                                 ct.c_uint]
def ESMP_FieldRead(field, filename, variablename, timeslice, iofmt=1):
    #TODO: C doc says it defaults to NETCDF(1), but actually defaults to BIN(0)
    """
    Preconditions: An ESMP_Field has been created.\n
    Postconditions: The contents of 'field' have been read from file.\n
    Arguments:\n
        ESMP_Field :: field\n
        string     :: filename\n
        string     :: variablename\n
        integer    :: timeslice\n
        IOFmt      :: iofmt\n
    """
    rc = _ESMF.ESMC_FieldRead(field.struct.ptr, filename, variablename, timeslice, iofmt)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_FieldRead() failed with rc = '+str(rc)+'.    '+
                        constants._errmsg)



_ESMF.ESMC_FieldRegridGetArea.restype = ct.c_int
_ESMF.ESMC_FieldRegridGetArea.argtypes = [ct.c_void_p]
@deprecated
def ESMP_FieldRegridGetArea(field):
    """
    Preconditions: An ESMP_Field has been created.\n
    Postconditions: The ESMP_Field has been initialized with the areas
                    of the cells of the underlying Grid or Mesh on 
                    which the Field has been built.  Note that in the 
                    Mesh case, this call only works for Fields built on 
                    the elements of a Mesh.\n
    Arguments:\n
        ESMP_Field :: field\n
    """
    rc = _ESMF.ESMC_FieldRegridGetArea(field.struct.ptr)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_FieldRegridGetArea failed! rc = '+str(rc)+
                        '.    '+constants._errmsg)


#### Regrid #####################################################


_ESMF.ESMC_FieldRegridRelease.restype = ct.c_int
_ESMF.ESMC_FieldRegridRelease.argtypes = [ct.POINTER(ct.c_void_p)]
@deprecated
def ESMP_FieldRegridRelease(routehandle):
    """
    Preconditions: A routehandle has been created with 
                   ESMP_RegridStore().\n
    Postconditions: All heap data associated with the regridding 
                    operation has been released.\n
    Arguments:\n
        ESMP_RouteHandle :: routehandle\n
    """
    rc = _ESMF.ESMC_FieldRegridRelease(ct.byref(routehandle))
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_FieldRegridRelease() failed with rc = '+str(rc)+
                        '.    '+constants._errmsg)

_ESMF.ESMC_FieldRegridStore.restype = ct.c_int
_ESMF.ESMC_FieldRegridStore.argtypes = [ct.c_void_p, ct.c_void_p,
                                        OptionalStructPointer,
                                        OptionalStructPointer,
                                        ct.POINTER(ct.c_void_p),
                                        OptionalNamedConstant,
                                        OptionalNamedConstant,
                                        ct.POINTER(ct.c_void_p),
                                        OptionalNamedConstant,
                                        OptionalNamedConstant,
                                        OptionalNamedConstant,
                                        OptionalBool,
                                        OptionalField,
                                        OptionalField]
@deprecated
def ESMP_FieldRegridStore(srcField, dstField,
                          srcMaskValues=None, dstMaskValues=None,
                          regridmethod=None,
                          polemethod=None, regridPoleNPnts=None,
                          lineType=None, normType=None, unmappedaction=None,
                          ignoreDegenerate=None,
                          srcFracField=None, dstFracField=None):
    """
    Preconditions: Two ESMP_Fields have been created and initialized
                   sufficiently for a regridding operation to take 
                   place.  'srcMaskValues' and 'dstMaskValues' are 
                   Numpy arrays which hold the values of a field which 
                   represent a masked cell.\n
    Postconditions: A handle to the regridding operation has been 
                    returned into 'routehandle' and Fields containing 
                    the fractions of the source and destination cells 
                    participating in the regridding operation are 
                    optionally returned into 'srcFracField' and 
                    'dstFracField'.\n
    Arguments:\n
        :RETURN: ESMP_RouteHandle           :: routehandle\n
        ESMP_Field                          :: srcField\n
        ESMP_Field                          :: dstField\n
        Numpy.array(dtype=int32) (optional) :: srcMaskValues\n
        Numpy.array(dtype=int32) (optional) :: dstMaskValues\n
        regridMethod (optional)             :: regridmethod\n
            Argument values:\n
                (default) RegridMethod.BILINEAR\n
                RegridMethod.PATCH\n
                RegridMethod.CONSERVE\n
        poleMethod (optional)               :: polemethod\n
            Argument values:\n
                (default for regridmethod == RegridMethod.CONSERVE) PoleMethod.NONE\n
                (default for regridmethod != RegridMethod.CONSERVE) PoleMethod.ALLAVG\n
                PoleMethod.NPNTAVG\n
                PoleMethod.TEETH\n
        integer (optional)                  :: regridPoleNPnts\n
        lineType (optional)                 :: normType\n
            Argument values:\n
                NOTE: default is dependent on the value of regridMethod
                LineType.CART \n
                LineType.GREAT_CIRCLE \n
        normType (optional)                 :: normType\n
            Argument values:\n
                (default) NormType.DSTAREA \n
                NormType.DSTFRAC \n
        unmappedAction (optional)           :: unmappedaction\n
            Argument values:\n
                (default) UnmappedAction.ERROR\n
                UnmappedAction.IGNORE\n
        boolean (option)                    :: ignoreDegenerate\n
        ESMP_Field (optional)               :: srcFracField\n
        ESMP_Field (optional)               :: dstFracField\n
    """
    routehandle = ct.c_void_p(0)
    if regridPoleNPnts:
        regridPoleNPnts_ct = ct.byref(ct.c_void_p(regridPoleNPnts))
    else:
        regridPoleNPnts_ct = None
        
    #InterfaceInt requires int32 type numpy arrays
    srcMaskValues_i = srcMaskValues
    if (srcMaskValues is not None):
        if (srcMaskValues.dtype != np.int32):
            raise TypeError('srcMaskValues must have dtype=int32')
        srcMaskValues_i = ESMP_InterfaceInt(srcMaskValues)

    #InterfaceInt requires int32 type numpy arrays
    dstMaskValues_i = dstMaskValues
    if (dstMaskValues is not None):
        if (dstMaskValues.dtype != np.int32):
            raise TypeError('dstMaskValues must have dtype=int32')
        dstMaskValues_i = ESMP_InterfaceInt(dstMaskValues)

    rc = _ESMF.ESMC_FieldRegridStore(srcField.struct.ptr,
                                     dstField.struct.ptr,
                                     srcMaskValues_i,
                                     dstMaskValues_i,
                                     ct.byref(routehandle),
                                     regridmethod,
                                     polemethod,
                                     regridPoleNPnts_ct,
                                     lineType,
                                     normType,
                                     unmappedaction,
                                     ignoreDegenerate,
                                     srcFracField,
                                     dstFracField)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_FieldRegridStore() failed with rc = '+str(rc)+
                        '.    '+constants._errmsg)
    return routehandle

_ESMF.ESMC_FieldRegrid.restype = ct.c_int
_ESMF.ESMC_FieldRegrid.argtypes = [ct.c_void_p, ct.c_void_p, ct.c_void_p,
                                   OptionalNamedConstant]
@deprecated
def ESMP_FieldRegrid(srcField, dstField, routehandle, zeroregion=None):
    """
    Preconditions: ESMP_RegridStore() has been called.\n
    Postconditions: An ESMP regridding operation has been performed,
                    and the results are stored in 'dstField'\n
    Arguments:\n
        ESMP_Field       :: srcField\n
        ESMP_Field       :: dstField\n
        ESMP_RouteHandle :: routehandle\n
        RegionFlag       :: zeroregion\n
    """
    rc = _ESMF.ESMC_FieldRegrid(srcField.struct.ptr, dstField.struct.ptr, \
                                routehandle, zeroregion)
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_FieldRegrid() failed with rc = '+str(rc)+
                        '.    '+constants._errmsg)


_ESMF.ESMC_ScripInq.restype = None
_ESMF.ESMC_ScripInq.argtypes = [ct.c_char_p, 
                                np.ctypeslib.ndpointer(dtype=np.int32), 
                                ct.POINTER(ct.c_int),
                                ct.POINTER(ct.c_int)]
@netcdf
def ESMP_ScripInq(filename):
    """
    Preconditions: ESMP has been initialized.\n
    Postconditions:  The rank and grid dimensions of the specified SCRIP 
                     NetCDF file or an error code have been returned.\n
    Arguments:\n
        String :: filename\n
    """
    lrc = ct.c_int(0)
    lrank = ct.c_int(0)
    grid_dims = np.array([0,0], dtype=np.int32)
    _ESMF.ESMC_ScripInq(filename, grid_dims, ct.byref(lrank), ct.byref(lrc))
    rank = lrank.value
    if rank == 1:
        grid_dims = grid_dims[0:1]
    rc = lrc.value
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_ScripInq() failed with rc = '+str(rc)+'.    '+
                         constants._errmsg)
    return rank, grid_dims

_ESMF.ESMC_GridspecInq.restype = None
_ESMF.ESMC_GridspecInq.argtypes = [ct.c_char_p, ct.POINTER(ct.c_int), np.ctypeslib.ndpointer(dtype=np.int32), 
                                   ct.POINTER(ct.c_int)]
@netcdf
def ESMP_GridspecInq(filename):
    """
    Preconditions: ESMP has been initialized.\n
    Postconditions:  The rank, dimension of the coordinates, and grid dimensions of the 
                     specified GRIDSPEC NetCDF file or an error code have been returned.\n
    Arguments:\n
        String :: filename\n
    """
    lrc = ct.c_int(0)
    lndims = ct.c_int(0)
    grid_dims = np.array([0,0], dtype=np.int32)
    _ESMF.ESMC_GridspecInq(filename, ct.byref(lndims), grid_dims, ct.byref(lrc))
    ndims = lndims.value
    rc = lrc.value
    rank = 2
    if rc != constants._ESMP_SUCCESS:
        raise ValueError('ESMC_GridspecInq() failed with rc = '+str(rc)+'.    '+
                         constants._errmsg)
    return rank, ndims, grid_dims
