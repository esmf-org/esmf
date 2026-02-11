"""
Unit tests of build_utils/version.py
"""

from build_utils.version import _search_macros_line_for_version
from esmpy.test.base import TestBase

class TestVersion(TestBase):

    def test_search_macros_line_for_version_found(self):
        result = _search_macros_line_for_version('#define ESMF_VERSION_STRING "8.8.0"')
        self.assertEqual(result, "8.8.0")

    def test_search_macros_line_for_version_not_found(self):
        result = _search_macros_line_for_version('#define ESMF_VERSION_STRING_FOO "8.8.0"')
        self.assertIsNone(result)
