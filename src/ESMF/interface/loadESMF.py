# $Id$

#### IMPORT LIBRARIES #########################################################

import os
import sys
import traceback

import constants as constants

try:
    from esmfmkfile import ESMFMKFILE as esmfmk
except:
    print 'The ESMFMKFILE cannot be found!'
    raise ImportError(constants._ESMP_ERROR_LIBS)

try:
    import numpy as np
except:
    print 'The Numpy library cannot be found!'
    raise ImportError(constants._ESMP_ERROR_LIBS)

# this library is loaded here so that it can be pulled back up without sys
try:
    import ctypes as ct
except:
    print 'The CTypes library cannot be found!'
    raise ImportError(constants._ESMP_ERROR_LIBS)

#### INVESTIGATE esmf.mk ######################################################

# TODO: look for various dependecies in the ESMF build log
#       - NetCDF
#       - LAPACK
#       - mpirun
#       use this information to set variables that can be checked at beginning
#       of the routines that require an ESMF build with these dependencies

try:
    try:
        MKFILE = open(esmfmk, 'r')
    except:
        raise IOError("File not found\n  %s") % esmfmk
    
    # investigate esmf.mk
    libsdir = 0
    esmfos = 0
    esmfabi = 0
    
    for line in MKFILE:
        if 'ESMF_LIBSDIR' in line:
            libsdir = line.split("=")[1]
        elif 'ESMF_OS:' in line:
            esmfos = line.split(":")[1]
        elif 'ESMF_ABI:' in line:
            esmfabi = line.split(":")[1]
    MKFILE.close()
    if not libsdir:
        raise "ESMF_LIBSDIR not found!"
    if not esmfos:
        raise "ESMF_OS not found!"
    if not esmfabi:
        raise "ESMF_ABI not found!"
    libsdir = libsdir.rstrip()
    esmfos = esmfos.rstrip()
    
    # set ESMF_MACHINE
    if "Darwin" in esmfos:
        constants._ESMF_OS = constants._ESMF_OS_DARWIN
    elif "Linux" in esmfos:
        constants._ESMF_OS = constants._ESMF_OS_LINUX
    elif "Unicos" in esmfos:
        constants._ESMF_OS = constants._ESMF_OS_UNICOS
    else:
        raise ImportError("Unrecognized ESMF_OS setting!")
    
    # set _ESMF_ABI for 32/64 switching
    if "64" in esmfabi:
        constants._ESMF_ABI=constants._ESMF_ABI_64
    elif "32" in esmfabi:
        constants._ESMF_ABI=constants._ESMF_ABI_32
except:
    print 'There is no ESMF shared library object available \
           (libesmf_fullylinked.so).'
    print 'Please set ESMFMKFILE to a current ESMF installation to proceed.'
    raise ImportError(constants._ESMP_ERROR_ESMFMKFILE)

#### SHARED LIBRARY ###########################################################

# load the shared library for esmf
try:
    if constants._ESMF_OS == constants._ESMF_OS_DARWIN:
        _ESMF = np.ctypeslib.load_library('libesmf_fullylinked',libsdir)
    else:
        _ESMF = ct.CDLL(os.path.join(libsdir,'libesmf_fullylinked.so'),
                        mode=ct.RTLD_GLOBAL)
except:
    traceback.print_exc(file=sys.stdout)
    raise ImportError(constants._ESMP_ERROR_SHAREDLIB)
