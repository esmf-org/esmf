"""
This file contains helper functions used by loadESMF.py.

These functions need to be in a separate file to be unit testable, since loadESMF.py
contains top-level code that would be executed when it's imported in a test file.

"Private" functions here (i.e., functions with a leading underscore) are meant to be used
only by loadESMF.py - i.e., they should be thought of as private to this file and
loadESMF.py.
"""

import re

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
        raise VersionMismatch("ESMF installation version {}, ESMPy version {}".format(
            esmfversion, esmpyversion))
    # otherwise warn that beta versions may be in use
    else:
        import warnings
        warnings.warn("ESMF installation version {}, ESMPy version {}".format(
            esmfversion, esmpyversion), VersionWarning)
