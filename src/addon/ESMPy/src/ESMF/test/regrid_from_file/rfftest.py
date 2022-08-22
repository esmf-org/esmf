# $Id$

"""
examples test file
"""

import pytest

from ESMF.test.base import TestBase, attr

class TestRFF(TestBase):

    # # '0' in the name is so it is run first
    # def test_0_regrid_from_file_dryrun(self):
    #     from ESMF.test.regrid_from_file import run_regrid_from_file_dryrun

    def test_regrid_from_file(self):
        from ESMF.test.regrid_from_file import run_regrid_from_file
