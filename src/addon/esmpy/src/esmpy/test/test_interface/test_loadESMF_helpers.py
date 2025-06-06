"""
Unit tests of loadESMF_helpers.py
"""

import pytest
import warnings

from esmpy.interface.loadESMF_helpers import _check_version
from esmpy.test.base import TestBase
from esmpy.util.exceptions import VersionWarning, VersionMismatch

class TestLoadESMFHelpers(TestBase):

    def test_check_version_equal(self):
        """
        Given identical esmf and esmpy versions,
        when _check_version is called,
        then no exception should be raised and no warning issued.
        """
        # This context manager restores the default warning behavior when the test exits:
        with warnings.catch_warnings():
            # Turn warnings into errors so that the test will fail if a warning is issued:
            warnings.simplefilter("error")
            _check_version("8.8.0", "8.8.0")

    def test_check_version_major_differs(self):
        """
        Given versions that differ in their major version (1st digit),
        when _check_version is called,
        then a VersionMismatch exception should be raised.
        """
        with pytest.raises(VersionMismatch):
            _check_version("9.0.0", "8.0.0")

    def test_check_version_minor_differs(self):
        """
        Given versions that differ in their minor version (2nd digit),
        when _check_version is called,
        then a VersionMismatch exception should be raised.
        """
        with pytest.raises(VersionMismatch):
            _check_version("8.8.0", "8.9.0")

    def test_check_version_patch_differs(self):
        """
        Given versions that differ in their patch version (3rd digit),
        when _check_version is called,
        then a VersionWarning should be issued.
        """
        with pytest.warns(VersionWarning):
            _check_version("8.8.0", "8.8.1")

    def test_check_version_betas_from_same_version(self):
        """
        Given two beta versions of the same major.minor.patch version,
        when _check_version is called,
        then a VersionWarning should be issued.

        (Note that we don't have a good way to verify if the same beta version is being
        used in both, so we always issue a warning whenever using beta versions.)
        """
        with pytest.warns(VersionWarning):
            _check_version("8.9.0 beta snapshot", "8.9.0b0")

    def test_check_version_betas_from_different_version(self):
        """
        Given two beta versions of different major.minor versions,
        when _check_version is called,
        then a VersionMismatch exception should be raised.
        """
        with pytest.raises(VersionMismatch):
            _check_version("8.8.0 beta snapshot", "8.9.0b0")

    def test_check_version_beta_vs_release(self):
        """
        Given an esmf beta version and an esmpy release version,
        when _check_version is called,
        then a VersionWarning should be issued.
        """
        with pytest.warns(VersionWarning):
            _check_version("8.9.0 beta snapshot", "8.9.0")

    def test_check_version_release_vs_beta(self):
        """
        Given an esmf release version and an esmpy beta version,
        when _check_version is called,
        then a VersionWarning should be issued.
        """
        with pytest.warns(VersionWarning):
            _check_version("8.9.0", "8.9.0b0")
