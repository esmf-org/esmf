# $Id$

"""
examples dryrun test file
"""

import pytest

from esmpy.test.base import TestBase, attr

class TestExamplesDryrun(TestBase):

    def test_examples_dryrun(self):
        from esmpy.util.cache_data import cache_data_files
        cache_data_files()
