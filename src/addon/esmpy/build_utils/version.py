"""
This file contains functions to get the version at build/install time.

This module is not installed with the ESMPy package.
"""

import os
import re

_ESMF_ROOT = os.path.normpath(
    os.path.join(os.path.dirname(os.path.abspath(__file__)),
                 os.pardir,
                 os.pardir,
                 os.pardir,
                 os.pardir)
)

_MACROS_PATH = os.path.join(_ESMF_ROOT,
                            "src",
                            "Infrastructure",
                            "Util",
                            "include",
                            "ESMC_Macros.h")

def _search_macros_line_for_version(line):
    match = re.match(r'#define\s+ESMF_VERSION_STRING\s+"([^"]+)"', line)
    if match:
        return match.group(1)
    return None

def _sanitize_version(version):
    """
    Sanitize the version string so that it is acceptable to setuptools-git-versioning
    """
    # setuptools-git-versioning doesn't know how to handle "beta snapshot", so remove the
    # "snapshot", transforming something like "8.9.0 beta snapshot" to simply "8.9.0 beta"
    # (note that trailing whitespace is okay, so we don't bother to remove it)
    clean_version = version.replace("snapshot", "")
    return clean_version

def get_source_esmf_version():
    """
    Return the ESMF version for this source directory
    """
    with open(_MACROS_PATH, "r") as file:
        for line in file:
            version = _search_macros_line_for_version(line)
            if version:
                return _sanitize_version(version)

    raise RuntimeError("Cannot find ESMF version")
