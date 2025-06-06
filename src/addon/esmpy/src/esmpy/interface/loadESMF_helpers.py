"""
This file contains helper functions used by loadESMF.py.

These functions need to be in a separate file to be unit testable, since loadESMF.py
contains top-level code that would be executed when it's imported in a test file.

"Private" functions here (i.e., functions with a leading underscore) are meant to be used
only by loadESMF.py - i.e., they should be thought of as private to this file and
loadESMF.py.
"""

import re
import warnings

from esmpy.util.exceptions import VersionWarning, VersionMismatch

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
