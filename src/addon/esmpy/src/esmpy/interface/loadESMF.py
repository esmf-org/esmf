# $Id$

#### IMPORT LIBRARIES #########################################################

import os
import sys
import traceback

from esmpy.interface.loadESMF_helpers import (_check_version, _find_esmf_mk,
                                              _esmf_mk_is_bundled)
import esmpy.api.constants as constants

try:
    import numpy as np
except:
    raise ImportError('The Numpy library cannot be found.')

# this library is loaded here so that it can be pulled back up without sys
try:
    import ctypes as ct
except:
    raise ImportError('The CTypes library cannot be found.')

# The esmpy package directory (two levels up from this file: esmpy/interface/).
# Used both to find an esmf.mk bundled inside a pip wheel and to detect that case.
_PACKAGE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

esmfmk = _find_esmf_mk(os.environ, sys.prefix, _PACKAGE_DIR)

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
        elif 'ESMF_PIO:' in line:
            pio = True
        elif 'ESMF_COMM:' in line:
            esmfcomm = line.split(":")[1]
        elif 'ESMF_VERSION_STRING=' in line:
            esmfversion = line.split("=")[1]
            esmfversion = esmfversion.rstrip('\n')
        elif 'ESMF_COMPILER' in line:
            if "gfortran" in line:
                use_inmem_factors = True

_check_version(esmfversion=esmfversion,
               esmpyversion=constants._ESMPY_VERSION)
constants._ESMF_VERSION = esmfversion


if not libsdir:
    raise ValueError("ESMF_LIBSDIR not found!")
if not esmfos:
    raise ValueError("ESMF_OS not found!")
if not esmfabi:
    raise ValueError("ESMF_ABI not found!")
libsdir = libsdir.rstrip()
esmfos = esmfos.rstrip()

# For a pip wheel that bundles the ESMF library, the absolute ESMF_LIBSDIR baked
# into esmf.mk at build time is invalid once the package is relocated into
# site-packages. The bundled library sits next to esmf.mk, so resolve the library
# directory relative to it. This is a no-op for conda/source/HPC installs.
if _esmf_mk_is_bundled(esmfmk, _PACKAGE_DIR):
    libsdir = os.path.dirname(os.path.abspath(esmfmk))

# set _ESMF_OS
if "Darwin" in esmfos:
    constants._ESMF_OS = constants._ESMF_OS_DARWIN
elif "Linux" in esmfos:
    constants._ESMF_OS = constants._ESMF_OS_LINUX
elif "Unicos" in esmfos:
    constants._ESMF_OS = constants._ESMF_OS_UNICOS
elif os.name == "nt":
    # Windows (e.g. a MinGW build); the esmf.mk ESMF_OS string varies, so key off
    # the running interpreter, which for a wheel always matches the target OS.
    constants._ESMF_OS = constants._ESMF_OS_WINDOWS
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
if pio:
    constants._ESMF_PIO = True

# set _ESMF_COMM
if "mpiuni" in esmfcomm:
    constants._ESMF_COMM = constants._ESMF_COMM_MPIUNI

# in-memory factors only supported on the GNU stack given inability to
# deallocate Fortran pointers associated using c_f_pointer in other compilers.
# some compilers may support this, but only the GNU stack is guaranteed at this
# point.
constants._ESMF_USE_INMEM_FACTORS = use_inmem_factors

#### MPI RUNTIME (real-MPI builds) ############################################

# A real-MPI build (ESMF_COMM != mpiuni) links libesmf_fullylinked against the C
# MPI runtime (libmpi). For a pip wheel we do NOT vendor libmpi: it is supplied by a
# separate runtime wheel (e.g. `mpich` for the esmpy-mpich distribution, or
# `openmpi` for esmpy-openmpi), which installs libmpi into <prefix>/lib via the
# wheel "data" scheme -- a directory not on the default dynamic-loader search path.
# Preload it here with RTLD_GLOBAL, before libesmf is dlopened, so libesmf's MPI
# symbols bind to it. This stays comm-agnostic: try the known C libmpi sonames
# (MPICH's libmpi.so.12, Open MPI's libmpi.so.40) and the unversioned fallback, and
# load the first that is present. Best-effort: if none is there (a conda/HPC/system
# MPI already on the loader path, or a serial build), skip and let the normal loader
# search apply.
if esmfcomm is not None and "mpiuni" not in esmfcomm:
    import sysconfig
    _mpi_libdir = os.path.join(sysconfig.get_paths()["data"], "lib")
    if constants._ESMF_OS == constants._ESMF_OS_DARWIN:
        _mpi_names = ("libmpi.12.dylib", "libmpi.40.dylib", "libmpi.dylib")
    else:
        _mpi_names = ("libmpi.so.12", "libmpi.so.40", "libmpi.so")
    for _mpi_name in _mpi_names:
        _mpi_path = os.path.join(_mpi_libdir, _mpi_name)
        if os.path.exists(_mpi_path):
            try:
                ct.CDLL(_mpi_path, mode=ct.RTLD_GLOBAL)
            except OSError:
                pass
            break

#### SHARED LIBRARY ###########################################################

# load the shared library for esmf
try:
    if constants._ESMF_OS == constants._ESMF_OS_DARWIN:
        _ESMF = np.ctypeslib.load_library('libesmf_fullylinked',libsdir)
    elif constants._ESMF_OS == constants._ESMF_OS_WINDOWS:
        # Ensure the bundled dependency DLLs (also in libsdir) are on the search path.
        if hasattr(os, "add_dll_directory"):
            os.add_dll_directory(libsdir)
        _ESMF = ct.CDLL(os.path.join(libsdir,'libesmf_fullylinked.dll'),
                        mode=ct.RTLD_GLOBAL)
    else:
        _ESMF = ct.CDLL(os.path.join(libsdir,'libesmf_fullylinked.so'),
                        mode=ct.RTLD_GLOBAL)
except:
    traceback.print_exc(file=sys.stdout)
    raise ImportError('The ESMF shared library did not load properly.')
