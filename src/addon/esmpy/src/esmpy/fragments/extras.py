# $Id$

"""
utilities module - most of these routines are not yet implemented.
"""

#### IMPORT LIBRARIES #########################################################

import ctypes as ct
import numpy as np

from constants import *
from loadESMF import _ESMF

#### CLASS DEFINITIONS ########################################################

# shallow type 
#class ESMP_ArraySpec(Structure):
#    _fields_ = [("shallowMem", c_char*192)]
#    _fields_ = [("shallowMem", create_string_buffer(192))]


#### STATE ####################################################################

_ESMF.ESMC_StateCreate.restype = c_void_p
_ESMF.ESMC_StateCreate.argtypes = [c_char_p, POINTER(c_int)]
def ESMP_StateCreate(name):
    """
    Preconditions: ESMP has been initialized.
    Postconditions: An ESMP_State has been created.
    Arguments:
        RETURN ESMP_State :: state
        string                        :: name
    """
    lrc = c_int(INIT)
    state = _ESMF.ESMC_StateCreate(name,byref(lrc))
    rc = lrc.value
    if rc != _ESMP_SUCCESS:
        raise NameError('ESMC_StateCreate() failed with rc = '+str(rc))
    return state

_ESMF.ESMC_StateDestroy.restype = c_int
_ESMF.ESMC_StateDestroy.argtypes = [c_void_p]
def ESMP_StateDestroy(state):
    """
    Preconditions: An ESMP_State has been created.
    Postconditions: The 'state' has been destroyed.
    Arguments:
        ESMP_State :: state
    """
    ptr = POINTER(c_void_p)
    stateptr = ptr(c_void_p(state))
    rc = _ESMF.ESMC_StateDestroy(stateptr)
    if rc != _ESMP_SUCCESS:
        raise NameError('ESMC_StateDestroy() failed with rc = '+str(rc))
    return

_ESMF.ESMC_StatePrint.restype = c_int
_ESMF.ESMC_StatePrint.argtypes = [c_void_p]
def ESMP_StatePrint(state):
    """
    Preconditions: An ESMP_State has been created.
    Postconditions: The contents of 'state' are printed to standard out.
    Arguments:
        ESMP_State :: state
    """
    rc = _ESMF.ESMC_StatePrint(state)
    if rc != _ESMP_SUCCESS:
        raise NameError('ESMC_StatePrint() failed with rc = '+str(rc))
    return

#### ARRAYSPEC ################################################################

_ESMF.ESMC_ArraySpecGet.restype = c_int
_ESMF.ESMC_ArraySpecGet.argtypes = [c_void_p, POINTER(c_int), POINTER(c_uint)]
def ESMP_ArraySpecGet(arrayspec, rank, typekind):
    '''
    Preconditions: ESMP has been initialized and 'arrayspec' has been Set.
    Postconditions: Information about 'arrayspec' has been returned in the form
                    of a tuple containing: [rank, typekind].
    Arguments:
        RETURN integer       :: rank
        RETURN TypeKind      :: typekind
        type(ESMP_ArraySpec) :: arrayspec
    '''
    las = create_string_buffer(192)
    las.value = arrayspec.shallowMem
    lrank = c_int(INIT)
    ltk = c_uint(INIT)
    rc = _ESMF.ESMC_ArraySpecGet(las, byref(lrank), byref(ltk))
    if rc != _ESMP_SUCCESS:
        raise NameError('ESMC_ArraySpecGet() failed with rc = '+str(rc))
    rank = lrank.value
    typekind.tk = ltk.value
    return rank, typekind

_ESMF.ESMC_ArraySpecSet.restype = c_int
_ESMF.ESMC_ArraySpecSet.argtypes = [c_void_p, c_int, c_uint]
def ESMP_ArraySpecSet(arrayspec, rank, typekind):
    '''
    KNOWN BUG: This function is not yet implemented.
    Preconditions: ESMP has been initialized
    Postconditions: An ESMP_ArraySpec object has been setup.
    Arguments:
        RETURN ESMP_ArraySpec :: arrayspec
        integer               :: rank
        TypeKind              :: typekind
    '''
    las = create_string_buffer(192)
    
    rc = _ESMF.ESMC_ArraySpecSet(byref(las), rank, typekind.tk)
    if rc != _ESMP_SUCCESS:
        raise NameError('ESMC_ArraySpecSet() failed with rc = '+str(rc))
    arrayspec.shallowMem = las.value
    print 'sizeof(arrayspec) = '+str(sizeof(arrayspec))
    print 'arrayspec = '+str(arrayspec)
    print 'sizeof(las) = '+str(sizeof(las))
    print 'las = '+str(las)
    print "\n"
    
    return arrayspec

#### MESH ################################################################

_ESMF.ESMC_MeshVTKHeader.restype = c_int
_ESMF.ESMC_MeshVTKHeader.argtypes = [c_char_p, POINTER(c_int), \
                                     POINTER(c_int), POINTER(c_int)]
def ESMP_MeshVTKHeader(fname, num_elem, num_node, conn_size):
    """
    KNOWN BUG: This function is not yet implemented.
    Preconditions: An ESMP_Mesh has been created and 'fname' is a handle to a 
                   valid (set of) vtk file(s).
    Postconditions: The header information for 'fname' is returned in a tuple
                    containing [num_elem, num_node, conn_size].
    Arguments:
        RETURN integer :: num_elem
        RETURN integer :: num_node
        RETURN integer :: conn_size
        character      :: fname
    """
    nes = c_int(INIT)
    nns = c_int(INIT)
    cns = c_int(INIT)
    rc = _ESMF.ESMC_MeshVTKHeader(c_char_p(fname), byref(nes), byref(nns), byref(cns))
    if rc != _ESMP_SUCCESS:
        raise NameError('ESMC_MeshVTKHeader() failed with rc = '+str(rc))
    num_elem = nes.value
    num_node = nns.value
    conn_size = cns.value
    return num_elem, num_node, conn_size
    
_ESMF.ESMC_MeshVTKBody.restype = c_int
_ESMF.ESMC_MeshVTKBody.argtypes = [c_char_p, POINTER(c_int), \
                                   POINTER(c_double), POINTER(c_int), \
                                   POINTER(c_int), POINTER(c_int), \
                                   POINTER(c_int)]
def ESMP_MeshVTKBody(fname, nodeId, nodeCoord, nodeOwner,\
                     elemId, elemType, elemConn):
    """
    KNOWN BUG: This function is not yet implemented.
    Preconditions: An ESMP_Mesh has been created and 'fname' is a handle to a 
                   valid (set of) vtk file(s).
    Postconditions: The body information for 'fname' is returned in a tuple
                    containing [nodeId, nodeCoord, nodeOwner, 
                    elemId, elemType, elemConn].
    Arguments:
        RETURN integer :: nodeId
        RETURN double  :: nodeCoord
        RETURN integer :: nodeOwner
        RETURN integer :: elemId
        RETURN integer :: elemType
        RETURN integer :: elemConn
        character      :: fname
    """
    nis = c_int(INIT)
    ncs = c_double(INIT)
    nos = c_int(INIT)
    eis = c_int(INIT)
    ets = c_int(INIT)
    ecs = c_int(INIT)
    rc = _ESMF.ESMC_MeshVTKBody(fname, byref(nis), byref(ncs), byref(nos), \
                                byref(eis), byref(ets), byref(ecs))
    if rc != _ESMP_SUCCESS:
        raise NameError('ESMC_MeshVTKBody() failed with rc = '+str(rc))
    nodeId = nis.value
    nodeCoord = ncs.value
    nodeOwner = nos.value
    elemId = eis.value
    elemType = ets.value
    elemConn = ecs.value
    return nodeId, nodeCoord, nodeOwner, elemId, elemType, elemConn
