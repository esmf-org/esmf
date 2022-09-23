# $Id$

#### IMPORT LIBRARIES #########################################################

import os
import sys
import traceback
import re

import esmpy.api.constants as constants
from esmpy.util.exceptions import VersionWarning, VersionMismatch

try:
    import numpy as np
except:
    raise ImportError('The Numpy library cannot be found.')

# this library is loaded here so that it can be pulled back up without sys
try:
    import ctypes as ct
except:
    raise ImportError('The CTypes library cannot be found.')

esmfmk = None
try:
    esmfmk = os.environ["ESMFMKFILE"]
except:
    raise ImportError('The ESMFMKFILE environment variable is not available.')

#### INVESTIGATE esmf.mk ######################################################

# TODO: look for various dependecies in the ESMF build log
#       - NetCDF
#       - PIO
#       - LAPACK
#       - mpirun
#       use this information to set variables that can be checked at beginning
#       of the routines that require an ESMF build with these dependencies

with open(esmfmk, 'r') as MKFILE:
    
    # investigate esmf.mk
    libsdir = None
    esmfos = None
    esmfabi = None
    esmfcomm = None
    esmfversion = None
    netcdf = False
    pio = False
    use_inmem_factors = False
    
    for line in MKFILE:
        if 'ESMF_LIBSDIR' in line:
            libsdir = line.split("=")[1]
        elif 'ESMF_OS:' in line:
            esmfos = line.split(":")[1]
        elif 'ESMF_ABI:' in line:
            esmfabi = line.split(":")[1]
        elif 'ESMF_NETCDF:' in line:
            netcdf = True
        elif 'ESMF_COMM:' in line:
            esmfcomm = line.split(":")[1]
        elif 'ESMF_VERSION_STRING=' in line:
            esmfversion = line.split("=")[1]
            esmfversion = esmfversion.rstrip('\n')
        elif 'ESMF_COMPILER' in line:
            if "gfortran" in line:
                use_inmem_factors = True

# check and set _ESMF_VERSION_STRING
esmfvs = re.split(r'\D+',esmfversion)
esmpyvs = re.split(r'\D+',constants._ESMPY_VERSION)

if esmfversion != constants._ESMPY_VERSION:
    # check if major, minor and patch version numbers are equivalent
    if esmfvs[0:2] != esmpyvs[0:2]:
        raise VersionMismatch("ESMF installation version {}, ESMPy version {}".format(
            esmfversion, constants._ESMPY_VERSION))
    # otherwise warn that beta versions may be in use
    else:
        import warnings
        warnings.warn("ESMF installation version {}, ESMPy version {}".format(
            esmfversion, constants._ESMPY_VERSION), VersionWarning)

constants._ESMF_VERSION = esmfversion


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
else:
    raise ValueError("Unrecognized ESMF_ABI setting!")

# set _ESMF_NETCDF
if netcdf:
    constants._ESMF_NETCDF = True

# set _ESMF_PIO
if "mpiuni" not in esmfcomm:
    constants._ESMF_PIO = True

# set _ESMF_COMM
if "mpiuni" in esmfcomm:
    constants._ESMF_COMM = constants._ESMF_COMM_MPIUNI

# in-memory factors only supported on the GNU stack given inability to
# deallocate Fortran pointers associated using c_f_pointer in other compilers.
# some compilers may support this, but only the GNU stack is guaranteed at this
# point.
constants._ESMF_USE_INMEM_FACTORS = use_inmem_factors

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
    raise ImportError('The ESMF shared library did not load properly.')
