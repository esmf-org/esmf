# $Id$

"""
The ESMPyManager API
"""

#### IMPORT LIBRARIES #########################################################

from ESMF.api.constants import *
from ESMF.interface.cbindings import *
from ESMF.util.exceptions import *
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
    This singleton class and its pair of methods __init__
    and __del__ are designed to called ESMP_Initialize and
    ESMP_Finalize once, and only once, in a Python session.

    ESMF is initialized at ESMPyManager object creation,
    __del__ is registered with atexit to ensure ESMF is always
    finalized prior to exiting Python.  If the object is copied,
    the copy will always be an alias to the original Manager object.
    '''

    # The singleton instance for this class
    __singleton = None
    
    def __new__(cls, logkind=LogKind.NONE, debug=False):
        '''
        Returns the singleton instance of this class,
        creating it if it does not already exist.
        '''

        # If this is the first call, create the singleton object
        # and initialize its attributes.
        if cls.__singleton == None:
            cls.__singleton = super(Manager, cls).__new__(cls)
            cls.__singleton.__esmp_initialized = False
            cls.__singleton.__esmp_finalized = False
        return cls.__singleton


    def __init__(self, logkind=LogKind.NONE, debug=False):
        '''
        Calls ESMP_Initialize and registers __del__ with atexit
        when called the first time.  Subsequent calls only return
        whether or not ESMF is initialized.  Registering __del__ with
        atexit ensures the ESMP_Finalize will always be called
        prior to exiting Python.  Calling __init__ explicitly
        results in a no-op. \n
        Required Arguments: \n
            None \n
        Optional Arguments: \n
            logkind: allows user to specify if there should be a single
            log file, multiple log files, or none at all.  The default
            is to not have a log file.  Argument values are: \n
                                LogKind.SINGLE\n
                                LogKind.MULTI\n
                                (default) LogKind.NONE\n
            debug: allows user to log after every call, instead of the
            default of every 10 calls. \n
                type: boolean (defaults to False)
        Returns: \n
            Manager \n
        '''

        # Return no-op
        if self.__esmp_finalized:
            return
        # Call ESMP_Initialize if not already done previously
        if not self.__esmp_initialized:
            ESMP_Initialize(logkind=logkind)
            # KNOWN_BUG: this causes the manager to be deallocated before
            # the objects in some cases which creates a neverending
            # segfault from ESMF when trying to call Destroy routines
            # on objects which have already been deallocated by the ESMF
            # garbage collector.
            import atexit; atexit.register(self.__del__)
            self.__esmp_initialized = True
            vm = ESMP_VMGetGlobal()
            self.local_pet, self.pet_count = ESMP_VMGet(vm)
            ESMP_LogSet(debug)
        return


    def __del__(self):
        '''
        Calls ESMP_Finalize if ESMF has been initialized.
        This function is registered with atexit
        by __init__ when it calls ESMP_Initialize to
        ensure ESMP_Finalize is always called before exiting
        Python.  However, this function can be called directly
        (presumably to delete the log file) before exiting without
        causing problems. \n
        Required Arguments: \n
            None \n
        Optional Arguments: \n
            None \n
        Returns: \n
            None \n
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
        """
        Return a string containing a printable representation of the object
        """
        string = ("ESMPyManager:\n"
                  "    local_pet = %r\n"
                  "    pet_count = %r\n)" 
                  %
                  (self.local_pet,
                   self.pet_count))

        return string
    
