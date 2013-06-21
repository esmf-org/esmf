# $Id$

"""
The bindings to the ESMF C API
"""

import ctypes as ct
import numpy as np

import ESMF.api.constants as constants
from ESMF.util.decorators import deprecated
from ESMF.interface.loadESMF import _ESMF

def copy(src):
    dst = type(src)()
    ct.pointer(dst)[0] = src
    return dst

class ESMP_GridStruct(ct.Structure):
    _fields_ = [("ptr", ct.c_void_p)]

class ESMP_Field(ct.Structure):
    _fields_ = [("ptr", ct.c_void_p)]

class ESMP_Mesh(ct.Structure):
    _fields_ = [("ptr", ct.c_void_p)]

class ESMP_InterfaceIntStruct(ct.Structure):
    _fields_ = [("ptr", ct.c_void_p)]

class ESMP_InterfaceInt(object):
    def __init__(self, array):
        # segfault in 32 bit mode because was not copying array
        arraycopy = array.copy()
        self.ptr = ESMP_InterfaceIntCreate(array, len(array))
        self.size = len(array)
    def __del__(self):
        ESMP_InterfaceIntDestroy(self.ptr)

class ESMP_VM(ct.Structure):
        _fields_ = [("ptr", ct.c_void_p)]

# this class allows optional arguments to be passed in place of
# numpy float64 arrays
class OptionalNumpyArrayFloat64(object):
        @classmethod
        def from_param(self, param):
            if param == None:
                return None
            elif param.dtype != np.float64:
                raise TypeError, "array must have data type Numpy.float64"
            else:
                return param.ctypes

# this class allows optional arguments to be passed in place of pointers to
# ctypes structures, such as Fields and InterfaceInts
class OptionalStructPointer(object):
        @classmethod
        def from_param(self, param):
            if param == None:
                return None
            else:
                ptr = ct.POINTER(ct.c_void_p)
                fieldptr = ptr(ct.c_void_p(param.ptr))
                return fieldptr

# this class allows optional Fields to be passed in place of pointers to
# ctypes structures
class OptionalField(object):
        @classmethod
        def from_param(self, param):
            if param == None:
                return None
            else:
                ptr = ct.POINTER(ct.c_void_p)
                fieldptr = ptr(ct.c_void_p(param.struct.ptr))
                return fieldptr

# this class allows optional arguments to be passed in place of named constants
class OptionalNamedConstant(object):
        @classmethod
        def from_param(self, param):
            if param == None:
                return None
            else:
                ptr = ct.POINTER(ct.c_uint)
                paramptr = ptr(ct.c_uint(param))
                return paramptr

# this class allows optional arguments to be passed in place of
# numpy int32 arrays
class OptionalNumpyArrayInt32(object):
        @classmethod
        def from_param(self, param):
            if param == None:
                return None
            elif param.dtype != np.int32:
                raise TypeError, "array must have data type Numpy.int32"
            else:
                return param.ctypes


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
                SINGLE\n
                (default) MULTI\n
                NONE\n
        """
    # need to add this to the Initialize statement

    rc = _ESMF.ESMC_Initialize(None, constants._ESMP_InitArgLogKindFlagID, 
                               logkind, constants._ESMP_ArgLast)
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_Initialize() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)

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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_Finalize() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)

#### INTERFACEINT #############################################################

_ESMF.ESMC_InterfaceIntCreate.restype = ESMP_InterfaceIntStruct
_ESMF.ESMC_InterfaceIntCreate.argtypes = [np.ctypeslib.ndpointer(dtype=np.int32),
                                          ct.c_int, ct.POINTER(ct.c_int)]
@deprecated
def ESMP_InterfaceIntCreate(arrayArg, lenArg):
    """
    Preconditions: ESMP has been initialized and 'arrayArg' is a Numpy 
                   array of type int.\n
    Postconditions: An ESMP_InterfaceInt has been created.\n
    Arguments:\n
        :RETURN: ESMP_InterfaceInt  :: interfaceInt\n
        Numpy.array(dtype=np.int32) :: arrayArg\n
        integer                     :: lenArg\n
    """
    lrc = ct.c_int(0)
    interfaceInt = _ESMF.ESMC_InterfaceIntCreate(arrayArg, lenArg, 
                                                 ct.byref(lrc))
    rc = lrc.value
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_InterfaceIntCreate() failed with rc = '+str(rc)+
                        '.    '+constants.errmsg)
    return interfaceInt.ptr

_ESMF.ESMC_InterfaceIntDestroy.restype = ct.c_int
_ESMF.ESMC_InterfaceIntDestroy.argtypes = [ct.c_void_p]
@deprecated
def ESMP_InterfaceIntDestroy(interfaceInt):
    """
    Preconditions: An ESMP_InterfaceInt has been created.\n
    Postconditions: The 'interfaceInt' has been destroyed.\n
    Arguments:\n
        ESMP_InterfaceInt :: interfaceInt\n
    """
    # segfault in Python with 'pointer being freed was not allocated' 
    # without ct.byref()
    rc = _ESMF.ESMC_InterfaceIntDestroy(ct.byref(ct.c_void_p(interfaceInt)))
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_InterfaceIntDestroy() failed with rc = '+str(rc)+
                        '.    '+constants.errmsg)

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

    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_VMGet() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)
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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_VMGetGlobal() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)
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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_VMPrint() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)

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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_LogSet() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)

#### GRID #####################################################

_ESMF.ESMC_GridCreate1PeriDim.restype = ESMP_GridStruct
_ESMF.ESMC_GridCreate1PeriDim.argtypes = [ct.c_void_p,
                                          OptionalNamedConstant,
                                          OptionalNamedConstant,
                                          OptionalNamedConstant,
                                          ct.POINTER(ct.c_int)]
@deprecated
def ESMP_GridCreate1PeriDim(maxIndex, coordSys=None, coordTypeKind=None):
    """
    Preconditions: ESMP has been initialized.\n
    Postconditions: An ESMP_Grid has been created.\n
    Arguments:\n
        :RETURN: ESMP_Grid    :: grid\n
        Numpy.array(dtype=int32) :: maxIndex\n
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

    # create the ESMF Grid and retrieve a ctypes pointer to it
    gridstruct = _ESMF.ESMC_GridCreate1PeriDim(maxIndex_i.ptr, coordSys,
                                               coordTypeKind, None, 
                                               ct.byref(lrc))

    # check the return code from ESMF
    rc = lrc.value
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_GridCreate() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)

    # create the ESMP Grid object from ctypes pointer
    return copy(gridstruct)

_ESMF.ESMC_GridCreateNoPeriDim.restype = ESMP_GridStruct
_ESMF.ESMC_GridCreateNoPeriDim.argtypes = [ct.c_void_p,
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

    # create the ESMF Grid and retrieve a ctypes pointer to it
    gridstruct = _ESMF.ESMC_GridCreateNoPeriDim(maxIndex_i.ptr, coordSys,
                                                coordTypeKind, ct.byref(lrc))

    # check the return code from ESMF
    rc = lrc.value
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_GridCreateNoPeriDim() failed with rc = '+str(rc)+
                        '.    '+constants.errmsg)

    # create the ESMP Grid object from ctypes pointer
    return copy(gridstruct)

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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_GridDestroy() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)

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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_GridAddCoord() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)

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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_GridGetCoord() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)

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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_GridAddItem() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)

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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_GridGetCoord() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)

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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_GridGetCoord() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)

    return exclusiveLBound, exclusiveUBound

_ESMF.ESMC_GridGetCoord.restype = ct.POINTER(ct.c_void_p)
_ESMF.ESMC_GridGetCoord.argtypes = [ct.c_void_p, ct.c_int, ct.c_uint,
                                    np.ctypeslib.ndpointer(dtype=np.int32),
                                    np.ctypeslib.ndpointer(dtype=np.int32),
                                    ct.POINTER(ct.c_int)]
@deprecated
def ESMP_GridGetCoord(grid, staggerloc=constants.StaggerLoc.CENTER):
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
    coordDim = ct.c_int(1)
    exclusiveLBound = np.array(np.zeros(grid.rank),dtype=np.int32)
    exclusiveUBound = np.array(np.zeros(grid.rank),dtype=np.int32)
    gridCoordPtr = _ESMF.ESMC_GridGetCoord(grid.struct.ptr, coordDim, 
                                           staggerloc,
                                           exclusiveLBound, exclusiveUBound,
                                           ct.byref(lrc))

    # adjust bounds to be 0 based
    exclusiveLBound = exclusiveLBound - 1

    rc = lrc.value
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_GridGetCoord() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)

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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_GridGetItem() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)

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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_GridWrite() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)

#### MESH #####################################################

_ESMF.ESMC_MeshAddElements.restype = ct.c_int
_ESMF.ESMC_MeshAddElements.argtypes = [ct.c_void_p, ct.c_int,
                                       np.ctypeslib.ndpointer(dtype=np.int32),
                                       np.ctypeslib.ndpointer(dtype=np.int32),
                                       np.ctypeslib.ndpointer(dtype=np.int32),
                                       OptionalNumpyArrayInt32,
                                       OptionalNumpyArrayFloat64]
@deprecated
def ESMP_MeshAddElements(mesh, elementCount,
                         elementIds, elementTypes,
                         elementConn,
                         elementMask=None,
                         elementArea=None):
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
    """
    lec = ct.c_int(elementCount)
    # ESMC expects the elementConn array to be 1 based..
    elementConn = elementConn + 1;
    rc = _ESMF.ESMC_MeshAddElements(mesh.struct.ptr, lec,
                                    elementIds, elementTypes,
                                    elementConn, elementMask, elementArea)
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_MeshAddElement() failed with rc = '+str(rc)+
                        '.    '+constants.errmsg)

_ESMF.ESMC_MeshAddNodes.restype = ct.c_int
_ESMF.ESMC_MeshAddNodes.argtypes = [ct.c_void_p, ct.c_int,
                                    np.ctypeslib.ndpointer(dtype=np.int32),
                                    np.ctypeslib.ndpointer(dtype=np.float64),
                                    np.ctypeslib.ndpointer(dtype=np.int32)]
@deprecated
def ESMP_MeshAddNodes(mesh, nodeCount, \
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

    rc = _ESMF.ESMC_MeshAddNodes(mesh.struct.ptr, lnc, \
                                 nodeIdsD, nodeCoordsD, nodeOwnersD)
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_MeshAddNodes() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)

_ESMF.ESMC_MeshCreate.restype = ESMP_Mesh
_ESMF.ESMC_MeshCreate.argtypes = [ct.c_int, ct.c_int, ct.POINTER(ct.c_int)]
@deprecated
def ESMP_MeshCreate(parametricDim, spatialDim):
    """
    Preconditions: ESMP has been initialized.\n
    Postconditions: An ESMP_Mesh has been created.\n
    Arguments:\n
        :RETURN: ESMP_Mesh :: mesh\n
        integer            :: parametricDim\n
        integer            :: spatialDim\n
    """
    lrc = ct.c_int(0)
    mesh = _ESMF.ESMC_MeshCreate(parametricDim, spatialDim, ct.byref(lrc))
    rc = lrc.value
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_MeshCreate() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)

    # handle the ctypes structure
    return copy(mesh)

_ESMF.ESMC_MeshCreateFromFile.restype = ESMP_Mesh
_ESMF.ESMC_MeshCreateFromFile.argtypes = [ct.c_char_p, ct.c_int,
                                          OptionalNamedConstant,
                                          OptionalNamedConstant,
                                          OptionalNamedConstant,
                                          ct.c_char_p,
                                          OptionalNamedConstant,
                                          ct.c_char_p]
@deprecated
def ESMP_MeshCreateFromFile(filename, fileTypeFlag, convert3D=None,
                            convertToDual=None, addUserArea=None,
                            meshname="", addMask=None, varname=""):
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
        bool (optional)    :: convert3D\n
        bool (optional)    :: convertToDual\n
        bool (optional)    :: addUserArea\n
        string (optional)  :: meshname\n
        bool (optional)    :: addMask\n
        string (optional)  :: varname\n
        """
    lrc = ct.c_int(0)
    mesh = _ESMF.ESMC_MeshCreateFromFile(filename, fileTypeFlag, convert3D,
                                         convertToDual, addUserArea,
                                         meshname, addMask, varname,
                                         ct.byref(lrc))
    rc = lrc.value
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_MeshCreateFromFile() failed with rc = '+str(rc)+
                        '.    '+constants.errmsg)

    # copy to the mesh struct
    return copy(mesh)

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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_MeshDestroy() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)

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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_MeshFreeMemory() failed with rc = '+str(rc)+
                        '.    '+constants.errmsg)

_ESMF.ESMC_MeshGetCoord.restype = ct.POINTER(ct.c_double)
_ESMF.ESMC_MeshGetCoord.argtypes = [ct.c_void_p, ct.POINTER(ct.c_int),
                                    ct.POINTER(ct.c_int)]
@deprecated
def ESMP_MeshGetCoordPtr(mesh):
    """
    Preconditions: An ESMP_Mesh has been created with coordinates 
                   specified.\n
    Postconditions: An array containing Mesh coordinate data has been
                    returned into 'meshCoordPtr' and number of nodes in
                    'num_nodes'.\n
    Arguments:\n
        :RETURN: list of doubles :: meshCoordPtr\n
        :RETURN: int             :: num_nodes\n
        ESMP_Mesh                :: mesh\n
    """
    lrc = ct.c_int(0)
    lnum_nodes = ct.c_int(0)
    meshCoordPtr = _ESMF.ESMC_MeshGetCoord(mesh, ct.byref(lnum_nodes),
                                           ct.byref(lrc))
    num_nodes = lnum_nodes.value
    rc = lrc.value
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_MeshGetCoord() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)
    return meshCoordPtr, num_nodes

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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_MeshGetLocalElementCount() failed with rc = '+
                        str(rc)+'.    '+constants.errmsg)
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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_MeshGetLocalNodeCount() failed with rc = '+
                        str(rc)+'.    '+constants.errmsg)
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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_MeshGetOwnedElementCount() failed with rc = '+
                        str(rc)+'.    '+constants.errmsg)
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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_MeshGetOwnedNodeCount() failed with rc = '+
                        str(rc)+'.    '+constants.errmsg)
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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_MeshWrite() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)

#### Field #####################################################

_ESMF.ESMC_FieldCreateGridTypeKind.restype = ESMP_Field
_ESMF.ESMC_FieldCreateGridTypeKind.argtypes = [ct.c_void_p, ct.c_uint,
                                               ct.c_uint, OptionalStructPointer,
                                               OptionalStructPointer, 
                                               OptionalStructPointer,
                                               ct.c_char_p, 
                                               ct.POINTER(ct.c_int)]
@deprecated
def ESMP_FieldCreateGrid(grid, name,
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
        string                :: name\n
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
    if (gridToFieldMap != None):
        if (gridToFieldMap.dtype != np.int32):
            raise TypeError('gridToFieldMap must have dtype=int32')
        gridToFieldMap_i = ESMP_InterfaceInt(gridToFieldMap)

    # InterfaceInt requires int32 type numpy arrays
    ungriddedLBound_i = ungriddedLBound
    if (ungriddedLBound != None):
        if (ungriddedLBound.dtype != np.int32):
            raise TypeError('ungriddedLBound must have dtype=int32')
        ungriddedLBound_i = ESMP_InterfaceInt(ungriddedLBound)

    # InterfaceInt requires int32 type numpy arrays
    ungriddedUBound_i = ungriddedUBound
    if (ungriddedUBound != None):
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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_FieldCreateGridTK() failed with rc = '+str(rc)+
                        '.    '+constants.errmsg)

    return field

_ESMF.ESMC_FieldCreateMeshTypeKind.restype = ESMP_Field
_ESMF.ESMC_FieldCreateMeshTypeKind.argtypes = [ct.c_void_p, ct.c_uint, 
                                               ct.c_uint, 
                                               OptionalStructPointer,
                                               OptionalStructPointer, 
                                               OptionalStructPointer,
                                               ct.c_char_p, 
                                               ct.POINTER(ct.c_int)]
@deprecated
def ESMP_FieldCreate(mesh, name,
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
        string                :: name\n
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
    if (gridToFieldMap != None):
        if (gridToFieldMap.dtype != np.int32):
            raise TypeError('gridToFieldMap must have dtype=int32')
        gridToFieldMap_i = ESMP_InterfaceInt(gridToFieldMap)

    # InterfaceInt requires int32 type numpy arrays
    ungriddedLBound_i = ungriddedLBound
    if (ungriddedLBound != None):
        if (ungriddedLBound.dtype != np.int32):
            raise TypeError('ungriddedLBound must have dtype=int32')
        ungriddedLBound_i = ESMP_InterfaceInt(ungriddedLBound)

    # InterfaceInt requires int32 type numpy arrays
    ungriddedUBound_i = ungriddedUBound
    if (ungriddedUBound != None):
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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_FieldCreateMeshTK() failed with rc = ' + \
                        str(rc) + '.    ' + constants.errmsg)

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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_FieldDestroy() failed with rc = ' + \
                                        str(rc) + '.    ' + constants.errmsg)

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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_FieldPrint() failed with rc = '+str(rc)+'.    '+
                        constants.errmsg)

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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_FieldGetPtr failed! rc = \n'+str(rc)+'.    '+
                        constants.errmsg)

    return fieldPtr

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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_FieldRegridGetArea failed! rc = '+str(rc)+
                        '.    '+constants.errmsg)


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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_FieldRegridRelease() failed with rc = '+str(rc)+
                        '.    '+constants.errmsg)

_ESMF.ESMC_FieldRegridStore.restype = ct.c_int
_ESMF.ESMC_FieldRegridStore.argtypes = [ct.c_void_p, ct.c_void_p,
                                        OptionalStructPointer,
                                        OptionalStructPointer,
                                        ct.POINTER(ct.c_void_p),
                                        OptionalNamedConstant,
                                        OptionalNamedConstant,
                                        OptionalField,
                                        OptionalField]
@deprecated
def ESMP_FieldRegridStore(srcField, dstField,
                          srcMaskValues=None, dstMaskValues=None,
                          regridmethod=None, unmappedaction=None,
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
        RegridMethod (optional)             :: regridmethod\n
            Argument values:\n
                (default) RegridMethod.BILINEAR\n
                RegridMethod.PATCH\n
                RegridMethod.CONSERVE\n
        UnmappedAction (optional)           :: unmappedaction\n
            Argument values:\n
                (default) UnmappedAction.ERROR\n
                UnmappedAction.IGNORE\n
        ESMP_Field (optional)               :: srcFracField\n
        ESMP_Field (optional)               :: dstFracField\n
    """
    routehandle = ct.c_void_p(0)

    #InterfaceInt requires int32 type numpy arrays
    srcMaskValues_i = srcMaskValues
    if (srcMaskValues != None):
        if (srcMaskValues.dtype != np.int32):
            raise TypeError('srcMaskValues must have dtype=int32')
        srcMaskValues_i = ESMP_InterfaceInt(srcMaskValues)

    #InterfaceInt requires int32 type numpy arrays
    dstMaskValues_i = dstMaskValues
    if (dstMaskValues != None):
        if (dstMaskValues.dtype != np.int32):
            raise TypeError('dstMaskValues must have dtype=int32')
        dstMaskValues_i = ESMP_InterfaceInt(dstMaskValues)

    rc = _ESMF.ESMC_FieldRegridStore(srcField.struct.ptr, \
                                     dstField.struct.ptr, \
                                     srcMaskValues_i, \
                                     dstMaskValues_i, \
                                     ct.byref(routehandle), \
                                     regridmethod, unmappedaction, \
                                     srcFracField, \
                                     dstFracField)
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_FieldRegridStore() failed with rc = '+str(rc)+
                        '.    '+constants.errmsg)
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
    if rc != constants.ESMP_SUCCESS:
        raise NameError('ESMC_FieldRegrid() failed with rc = '+str(rc)+
                        '.    '+constants.errmsg)
