"""
This file contains helper functions used by loadESMF.py.

These functions need to be in a separate file to be unit testable, since loadESMF.py
contains top-level code that would be executed when it's imported in a test file.

"Private" functions here (i.e., functions with a leading underscore) are meant to be used
only by loadESMF.py - i.e., they should be thought of as private to this file and
loadESMF.py.
"""

import os
import re
import warnings

from esmpy.util.exceptions import VersionWarning, VersionMismatch

def _find_esmf_mk(environ, sys_prefix, package_dir):
    """
    Locate the esmf.mk makefile fragment that describes the ESMF installation.

    Resolution order:
      1. The ESMFMKFILE environment variable, if set (returned as-is, matching the
         historical behavior where a missing file surfaces later as an open() error).
      2. A copy bundled inside the installed esmpy package (a pip wheel that ships
         libesmf_fullylinked alongside esmf.mk); see _esmf_mk_is_bundled.
      3. Common conda layouts under sys.prefix.

    Raises ImportError if ESMFMKFILE is unset and no esmf.mk can be found.
    """
    if "ESMFMKFILE" in environ:
        return environ["ESMFMKFILE"]

    guesses = [
        os.path.join(package_dir, "_esmf", "lib", "esmf.mk"),  # bundled in a wheel
        os.path.join(sys_prefix, "lib", "esmf.mk"),            # conda build of esmf
        os.path.join(sys_prefix, "Library", "lib", "esmf.mk"), # conda on Windows
    ]
    for path in guesses:
        if os.path.isfile(path):
            return path
    raise ImportError("The esmf.mk file cannot be found. Pass its path in the "
                      "ESMFMKFILE environment variable.")


def _esmf_mk_is_bundled(esmf_mk, package_dir):
    """
    Return True if esmf_mk lives inside the installed esmpy package (i.e. a wheel
    that bundles the ESMF library).

    In that case the absolute ESMF_LIBSDIR baked into esmf.mk at build time is
    meaningless once pip relocates the package, so the caller resolves the library
    directory relative to esmf.mk instead. Returns False for conda/source/HPC
    installs, where the baked path is authoritative and behavior is unchanged.
    """
    pkg = os.path.realpath(package_dir)
    mk = os.path.realpath(esmf_mk)
    return mk == pkg or mk.startswith(pkg + os.sep)


def _check_version(esmfversion, esmpyversion):
    """
    Check the ESMF version (from ESMF_VERSION_STRING in the esmf.mk file) against the
    ESMPy package version; if they differ, either raise an exception or give a warning,
    depending on how much they differ.
    """
    if esmfversion == esmpyversion:
        # Identical versions: we're all good: nothing to do here
        return

    esmfvs = re.split(r'\D+',esmfversion)
    esmpyvs = re.split(r'\D+',esmpyversion)

    # check if major, minor and patch version numbers are equivalent
    if esmfvs[0:3] != esmpyvs[0:3]:
        raise VersionMismatch(f"ESMF installation version {esmfversion} "
                              f"differs from ESMPy version {esmpyversion}")

    # Check for beta status in each version
    esmf_is_beta = "beta" in esmfversion
    esmpy_is_beta = bool(re.search(r"b\d+", esmpyversion))
    if esmf_is_beta and not esmpy_is_beta:
        raise VersionMismatch(f"Cannot use an ESMF development version ({esmfversion}) "
                              f"with an ESMPy release version ({esmpyversion})")
    elif esmpy_is_beta and not esmf_is_beta:
        raise VersionMismatch(f"Cannot use an ESMF release version ({esmfversion}) "
                              f"with an ESMPy development version ({esmpyversion})")
    elif esmf_is_beta and esmpy_is_beta:
        warnings.warn("You are using development versions of ESMF and ESMPy; "
                      "we cannot verify if these versions are compatible",
                      VersionWarning)
    else:
        # Versions don't match, but the version triplet is identical, and neither appears
        # to be a beta version. This situation is unexpected, so handle it generically and
        # cautiously.
        raise VersionMismatch(f"ESMF installation version {esmfversion} "
                              f"differs in an unexpected way from ESMPy version {esmpyversion} ")
