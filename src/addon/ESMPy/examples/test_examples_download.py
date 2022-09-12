# $Id$

"""
examples data download
"""

import sys

from esmpy.util.cache_data import cache_data_files

DATAURL = None
if len(sys.argv) > 1:
    DATAURL = sys.argv[1]

cache_data_files(DATAURL)
