# $Id$

"""
examples test file
"""

import pytest

from esmpy.test.base import TestBase, attr

class TestRFF(TestBase):

    # # '0' in the name is so it is run first
    # def test_0_regrid_from_file_dryrun(self):
    #     from esmpy.test.regrid_from_file import run_regrid_from_file_dryrun

    def test_regrid_from_file(self):
        from esmpy.test.regrid_from_file import run_regrid_from_file
