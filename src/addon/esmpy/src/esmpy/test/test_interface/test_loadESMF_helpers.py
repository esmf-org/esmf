"""
Unit tests of loadESMF_helpers.py
"""

import os
import tempfile

import pytest
import warnings

from esmpy.interface.loadESMF_helpers import (_check_version, _find_esmf_mk,
                                              _esmf_mk_is_bundled)
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
        then a VersionMismatch exception should be raised.
        """
        with pytest.raises(VersionMismatch):
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
        then a VersionMismatch exception should be raised.
        """
        with pytest.raises(VersionMismatch):
            _check_version("8.9.0 beta snapshot", "8.9.0")

    def test_check_version_release_vs_beta(self):
        """
        Given an esmf release version and an esmpy beta version,
        when _check_version is called,
        then a VersionMismatch exception should be raised.
        """
        with pytest.raises(VersionMismatch):
            _check_version("8.9.0", "8.9.0b0")

    def test_find_esmf_mk_env_var_takes_precedence(self):
        """
        Given ESMFMKFILE set in the environment,
        when _find_esmf_mk is called,
        then that value is returned verbatim (even if the file does not exist),
        preserving the historical behavior.
        """
        environ = {"ESMFMKFILE": "/some/explicit/esmf.mk"}
        result = _find_esmf_mk(environ, sys_prefix="/unused", package_dir="/unused")
        assert result == "/some/explicit/esmf.mk"

    def test_find_esmf_mk_bundled_in_package(self):
        """
        Given no ESMFMKFILE and an esmf.mk bundled inside the esmpy package,
        when _find_esmf_mk is called,
        then the bundled esmf.mk path is returned.
        """
        with tempfile.TemporaryDirectory() as tmp:
            pkg = os.path.join(tmp, "esmpy")
            bundled = os.path.join(pkg, "_esmf", "lib", "esmf.mk")
            os.makedirs(os.path.dirname(bundled))
            open(bundled, "w").close()
            result = _find_esmf_mk({}, sys_prefix=tmp, package_dir=pkg)
            assert result == bundled

    def test_find_esmf_mk_bundled_preferred_over_conda(self):
        """
        Given both a bundled esmf.mk and a conda-style sys.prefix/lib/esmf.mk,
        when _find_esmf_mk is called,
        then the bundled copy wins.
        """
        with tempfile.TemporaryDirectory() as tmp:
            pkg = os.path.join(tmp, "esmpy")
            bundled = os.path.join(pkg, "_esmf", "lib", "esmf.mk")
            conda = os.path.join(tmp, "lib", "esmf.mk")
            for path in (bundled, conda):
                os.makedirs(os.path.dirname(path))
                open(path, "w").close()
            result = _find_esmf_mk({}, sys_prefix=tmp, package_dir=pkg)
            assert result == bundled

    def test_find_esmf_mk_conda_prefix(self):
        """
        Given no ESMFMKFILE and only a conda-style sys.prefix/lib/esmf.mk,
        when _find_esmf_mk is called,
        then that path is returned.
        """
        with tempfile.TemporaryDirectory() as tmp:
            conda = os.path.join(tmp, "lib", "esmf.mk")
            os.makedirs(os.path.dirname(conda))
            open(conda, "w").close()
            pkg = os.path.join(tmp, "esmpy")  # no bundled esmf.mk here
            result = _find_esmf_mk({}, sys_prefix=tmp, package_dir=pkg)
            assert result == conda

    def test_find_esmf_mk_not_found_raises(self):
        """
        Given no ESMFMKFILE and no esmf.mk in any known location,
        when _find_esmf_mk is called,
        then an ImportError is raised.
        """
        with tempfile.TemporaryDirectory() as tmp:
            with pytest.raises(ImportError):
                _find_esmf_mk({}, sys_prefix=tmp,
                              package_dir=os.path.join(tmp, "esmpy"))

    def test_esmf_mk_is_bundled_true(self):
        """
        Given an esmf.mk located inside the esmpy package directory,
        when _esmf_mk_is_bundled is called,
        then it returns True.
        """
        with tempfile.TemporaryDirectory() as tmp:
            pkg = os.path.join(tmp, "esmpy")
            mk = os.path.join(pkg, "_esmf", "lib", "esmf.mk")
            os.makedirs(os.path.dirname(mk))
            open(mk, "w").close()
            assert _esmf_mk_is_bundled(mk, pkg) is True

    def test_esmf_mk_is_bundled_false_outside_package(self):
        """
        Given an esmf.mk outside the esmpy package (conda/source install),
        when _esmf_mk_is_bundled is called,
        then it returns False.
        """
        with tempfile.TemporaryDirectory() as tmp:
            pkg = os.path.join(tmp, "esmpy")
            os.makedirs(pkg)
            mk = os.path.join(tmp, "lib", "esmf.mk")
            os.makedirs(os.path.dirname(mk))
            open(mk, "w").close()
            assert _esmf_mk_is_bundled(mk, pkg) is False

    def test_esmf_mk_is_bundled_false_for_sibling_prefix(self):
        """
        Given a package dir and an esmf.mk under a sibling directory whose path
        merely starts with the package dir's name (e.g. esmpy vs esmpy-data),
        when _esmf_mk_is_bundled is called,
        then it returns False (the separator guard prevents a false match).
        """
        with tempfile.TemporaryDirectory() as tmp:
            pkg = os.path.join(tmp, "esmpy")
            sibling = os.path.join(tmp, "esmpy-data")
            os.makedirs(pkg)
            os.makedirs(sibling)
            mk = os.path.join(sibling, "esmf.mk")
            open(mk, "w").close()
            assert _esmf_mk_is_bundled(mk, pkg) is False

