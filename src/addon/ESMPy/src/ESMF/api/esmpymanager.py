# $Id$

"""
The ESMPyManager API
"""

#### IMPORT LIBRARIES #########################################################

from ESMF.api.constants import *
from ESMF.util.exceptions import *
from ESMF.interface.cbindings import *
from ESMF.util.decorators import initialize

import re

#### UTILITIES ################################################################

def _preprocess(v, separator, ignorecase):
    if ignorecase: v = v.lower()
    return [int(x) if x.isdigit() else [int(y) if y.isdigit() else y for y in
        re.findall("\d+|[a-zA-Z]+", x)] for x in v.split(separator)]

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
    and finalized.  ESMF is initialized at Manager creation, and the __del__ 
    method is registered with atexit to ensure ESMF is always finalized prior to 
    exiting Python.  If the object is copied, the copy will always be an alias 
    to the original Manager object.  The Manager will be created when the first 
    ESMPy object is created if it is not created explicitly by the user.

    Explicit creation of a Manager object allows for setting a flag which 
    results in the output of debug information from the ESMF logging capability 
    during the application runtime.  The output log files are named 
    PET<processor number>.ESMF_LogFile.

    The processor rank (local_pet) and total number of processers (pet_count) 
    can also be retrieved from the Manager using the following calls::

        ESMF.local_pet()
        ESMF.pet_count()

    *local_pet* and *pet_count* are also properties of the Manager.

    Calls ESMP_Initialize and registers __del__ with atexit when called the
    first time.  Subsequent calls only return whether or not ESMF is
    initialized.  Registering __del__ with atexit ensures the ESMP_Finalize
    will always be called prior to exiting Python.  Calling __init__
    explicitly results in a no-op.

    :param bool debug: outputs logging information to ESMF logfiles. If
        ``None``, defaults to False.
    '''
    # The singleton instance for this class
    __singleton = None
    
    def __new__(cls, debug=False):
        '''
        Returns the singleton instance of this class, creating it if it does 
        not already exist.
        '''

        # If this is the first call, create the singleton object
        # and initialize its attributes.
        if cls.__singleton is None:
            cls.__singleton = super(Manager, cls).__new__(cls)
            cls.__singleton.__esmp_initialized = False
            cls.__singleton.__esmp_finalized = False
        return cls.__singleton


    def __init__(self, debug=False):
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

            # set information related to the ESMF Virtual Machine
            vm = ESMP_VMGetGlobal()
            self._local_pet, self._pet_count = ESMP_VMGet(vm)

            # Increase frequency of log buffering upon user request
            ESMP_LogSet(debug)
        return

    @property
    def local_pet(self):
        return self._local_pet

    @property
    def pet_count(self):
        return self._pet_count

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
        ESMP_Finalize()
        self.__esmp_initialized = False
        self.__esmp_finalized = True

    def __repr__(self):
        string = ("ESMPyManager:\n"
                  "    local_pet = %r\n"
                  "    pet_count = %r\n)" 
                  %
                  (self.local_pet,
                   self.pet_count))

        return string
