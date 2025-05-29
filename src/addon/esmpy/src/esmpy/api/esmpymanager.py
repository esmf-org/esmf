# $Id$

"""
The ESMPyManager API
"""

#### IMPORT LIBRARIES #########################################################

from esmpy.interface.cbindings import *

from esmpy.api.constants import *
from esmpy.util.exceptions import *
from esmpy.util.decorators import initialize

import re

#### UTILITIES ################################################################

def _preprocess(v, separator, ignorecase):
    if ignorecase: v = v.lower()
    return [int(x) if x.isdigit() else [int(y) if y.isdigit() else y for y in
        re.findall(r"\d+|[a-zA-Z]+", x)] for x in v.split(separator)]

def version_compare(a, b, separator = '.', ignorecase = True):
    a = _preprocess(a, separator, ignorecase)
    b = _preprocess(b, separator, ignorecase)
    try:
        return (a > b) - (a < b)
    except:
        return False

@initialize
def local_pet():
    vm = ESMP_VMGetGlobal()
    local_pet, _ = ESMP_VMGet(vm)
    return local_pet

@initialize
def pet_count():
    vm = ESMP_VMGetGlobal()
    _, pet_count = ESMP_VMGet(vm)
    return pet_count

#### Manager class #########################################################

class Manager(object):
    '''
    This singleton class is designed to ensure that ESMF is properly initialized 
    and finalized.  ESMF is initialized at 
    :class:`~esmpy.api.esmpymanager.Manager` creation, and the __del__ 
    method is registered with atexit to ensure ESMF is always finalized prior to 
    exiting Python.  If the object is copied, the copy will always be an alias 
    to the original :class:`~esmpy.api.esmpymanager.Manager` object.  The 
    :class:`~esmpy.api.esmpymanager.Manager` will be created when the first 
    ESMPy object is created if it is not created explicitly by the user.

    Explicit creation of a :class:`~esmpy.api.esmpymanager.Manager` object allows
    for setting a flag which 
    results in the output of debug information from the ESMF logging capability 
    during the application runtime.  The output log files are named 
    PET<PET number>.ESMF_LogFile.

    The PET rank (local_pet) and total number of PETs (pet_count) 
    can also be retrieved from the :class:`~esmpy.api.esmpymanager.Manager` 
    using the following calls::

        esmpy.local_pet()
        esmpy.pet_count()

    ``local_pet`` and ``pet_count`` are also properties of the 
    :class:`~esmpy.api.esmpymanager.Manager`.

    Calls ESMP_Initialize and registers __del__ with atexit when called the
    first time.  Subsequent calls only return whether or not ESMF is
    initialized.  Registering __del__ with atexit ensures the ESMP_Finalize
    will always be called prior to exiting Python.  Calling __init__
    explicitly results in a no-op.

    The :class:`~esmpy.api.esmpymanager.Manager` can be used to enable the 
    `MOAB <https://sigma.mcs.anl.gov/moab-library/>`_
    mesh backend to the :class:`~esmpy.api.esmpymanager.Mesh`. This is done by calling ``set_moab()`` with ``moab_on=True``.
    
    The :class:`~esmpy.api.esmpymanager.Manager` has a `test_exhaustive` member 
    variable that can be enabled to run 
    combinatorial expansions of :class:`~esmpy.api.esmpymanager.Grid` and 
    :class:`~esmpy.api.esmpymanager.Field` creation parameters.

    :param bool debug: outputs logging information to ESMF logfiles. If
        ``None``, defaults to False.
    :param bool endFlag: determines the action to take on ESMF finalization.
        See :class:`~esmpy.api.constants.EndAction` docstring for details. Defaults to ``EndAction.NORMAL``.
    '''
    # The singleton instance for this class
    __singleton = None
    
    def __new__(cls, debug=False, endFlag=EndAction.NORMAL):
        '''
        Returns the singleton instance of this class, creating it if it does 
        not already exist.
        '''

        # If this is the first call, create the singleton object
        # and initialize its attributes.
        if isinstance(cls.__singleton, type(None)):
            cls.__singleton = super(Manager, cls).__new__(cls)
            cls.__singleton.__esmp_initialized = False
            cls.__singleton.__esmp_finalized = False
        return cls.__singleton


    def __init__(self, debug=False, endFlag=EndAction.NORMAL):
        # Return no-op
        if self.__esmp_finalized:
            return
        # Call ESMP_Initialize if not already done previously
        if not self.__esmp_initialized:
            # set up logging
            logkind = LogKind.NONE
            if debug:
                logkind = LogKind.MULTI

            # initialize ESMF
            ESMP_Initialize(logkind=logkind)
            import atexit; atexit.register(self.__del__)
            self.__esmp_initialized = True
            self.__esmp_end_flag = endFlag

            # set information related to the ESMF Virtual Machine
            vm = ESMP_VMGetGlobal()
            self._local_pet, self._pet_count = ESMP_VMGet(vm)
            self._vm = vm

            # Increase frequency of log buffering upon user request
            ESMP_LogSet(debug)
            
            # set up to use the ESMF native mesh backend by default
            self._moab = False
            
            # exhaustive testing set to False by default
            self._test_exhaustive = False

        return

    @property
    def local_pet(self):
        """
        :rtype: int
        :return: The id of the current Persistent Execution Thread (PET i.e. processing core).
        """
        return self._local_pet

    @property
    def moab(self):
        """
        :rtype: bool
        :return: A boolean value to indicate if the MOAB mesh backend is in use.
        """
        ESMP_MeshGetMOAB(self._moab)
        return self._moab

    @property
    def pet_count(self):
        """
        :rtype: int
        :return: The number of Persistent Execution Threads (PETs i.e. processing cores)
                 available in this execution.
        """
        return self._pet_count

    @property
    def vm(self):
        return self._vm

    def __del__(self):
        '''
        Calls ESMP_Finalize if ESMF has been initialized. This function is
        registered with atexit by __init__ when it calls ESMP_Initialize to
        ensure ESMP_Finalize is always called before exiting Python.  However,
        this function can be called directly (presumably to delete the log file)
        before exiting without causing problems.
        '''
        # If ESMP not initialize, or already finalized, just return
        if not self.__esmp_initialized:
            return
        if self.__esmp_finalized:
            return

        # Call ESMP_Finalize and set flags indicating this has been done
        ESMP_Finalize(self.__esmp_end_flag)
        self.__esmp_initialized = False
        self.__esmp_finalized = True

    def __repr__(self):
        string = ("ESMPyManager:\n"
                  "    local_pet = %r\n"
                  "    pet_count = %r\n" 
                  "    moab = %r\n" 
                  %
                  (self.local_pet,
                   self.pet_count,
                   self.moab))

        return string

    def barrier(self):
        '''
        Collective VM communication call that blocks calling PET until 
        all PETs of the VM have issued the call.
        '''
        ESMP_VMBarrier(self.vm)
        
    def set_moab(self, moab_on=True):
        """
        Set the Mesh backend to use MOAB or the Native ESMF mesh.
        """
        
        ESMP_MeshSetMOAB(moab_on)
        self._moab = moab_on

    def _broadcast_(self, bcstBuf, count, rootPet=0):
        '''
        Broadcast data from bcstBuf across the VM.\n
            Arguments:\n
                Numpy.array(dtype=float64) :: bcstBuf\n
                int :: count\n
                int :: rootPet\n

        '''
        ESMP_VMBroadcast(self.vm, bcstBuf, count, rootPet)

    def _logmem_(self, str):
        '''
        Send memory measurement to the log file.\n
            Arguments:\n
                string :: str\n

        '''
        ESMP_VMLogMemInfo(str)

    def _reduce_(self, sendBuf, recvBuf, count, reduceflag=Reduce.SUM, rootPet=0):
        '''
        Reduce data from sendBuf into recvBuf across the VM.\n
            Arguments:\n
                Numpy.array(dtype=float64) :: sendBuf\n
                Numpy.array(dtype=float64) :: recvBuf\n
                int :: count\n
                Reduce :: reduceflag\n
                    Argument Values:\n
                        Reduce.SUM\n
                        Reduce.MIN\n
                        Reduce.MAX\n
                int :: rootPet\n

        '''
        ESMP_VMReduce(self.vm, sendBuf, recvBuf, count, reduceflag, rootPet)
