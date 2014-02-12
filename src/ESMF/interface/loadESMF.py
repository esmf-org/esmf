# $Id$

#### IMPORT LIBRARIES #########################################################

import os
import sys
import traceback

import ESMF.api.constants as constants

try:
    from esmfmkfile import ESMFMKFILE as esmfmk
except:
    raise ImportError('The ESMFMKFILE cannot be found!')

try:
    import numpy as np
except:
    raise ImportError('The Numpy library cannot be found!')

# this library is loaded here so that it can be pulled back up without sys
try:
    import ctypes as ct
except:
    raise ImportError('The CTypes library cannot be found!')

#### INVESTIGATE esmf.mk ######################################################

# TODO: look for various dependecies in the ESMF build log
#       - NetCDF
#       - LAPACK
#       - mpirun
#       use this information to set variables that can be checked at beginning
#       of the routines that require an ESMF build with these dependencies

try:
    MKFILE = open(esmfmk, 'r')
    
    # investigate esmf.mk
    libsdir = 0
    esmfos = 0
    esmfabi = 0
    netcdf = [False, False, False, False]
    
    for line in MKFILE:
        if 'ESMF_LIBSDIR' in line:
            libsdir = line.split("=")[1]
        elif 'ESMF_OS:' in line:
            esmfos = line.split(":")[1]
        elif 'ESMF_ABI:' in line:
            esmfabi = line.split(":")[1]
        elif 'ESMF_NETCDF:' in line:
            netcdf[0] = True
        elif 'ESMF_NETCDF_INCLUDE:' in line:
            netcdf[1] = True
        elif 'ESMF_NETCDF_LIBS:' in line:
            netcdf[2] = True
        elif 'ESMF_NETCDF_LIBPATH:' in line:
            netcdf[3] = True
        
            
    MKFILE.close()
    if not libsdir:
        raise ValueError("ESMF_LIBSDIR not found!")
    if not esmfos:
        raise ValueError("ESMF_OS not found!")
    if not esmfabi:
        raise ValueError("ESMF_ABI not found!")
    libsdir = libsdir.rstrip()
    esmfos = esmfos.rstrip()
    
    # set _ESMF_OS
    if "Darwin" in esmfos:
        constants._ESMF_OS = constants._ESMF_OS_DARWIN
    elif "Linux" in esmfos:
        constants._ESMF_OS = constants._ESMF_OS_LINUX
    elif "Unicos" in esmfos:
        constants._ESMF_OS = constants._ESMF_OS_UNICOS
    else:
        raise ValueError("Unrecognized ESMF_OS setting!")
    
    # set _ESMF_ABI for 32/64 switching
    if "64" in esmfabi:
        constants._ESMF_ABI=constants._ESMF_ABI_64
    elif "32" in esmfabi:
        constants._ESMF_ABI=constants._ESMF_ABI_32

    # set _ESMF_NETCDF
    if all(netcdf):
        constants._ESMF_NETCDF = True
except:
    raise ValueError('There is no ESMF shared library object available \
           (libesmf_fullylinked.so).\nPlease set ESMFMKFILE to a current \
           ESMF installation to proceed.')

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
    raise ImportError('The ESMF shared library did not load!')
