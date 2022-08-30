# $Id$

"""
examples test file
"""

import pytest

from esmpy.test.base import TestBase, attr

class TestRFFDryrun(TestBase):

    def test_regrid_from_file_dryrun(self):
        from esmpy.test.regrid_from_file import run_regrid_from_file_dryrun
