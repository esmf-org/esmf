# $Id$

"""
decorators
"""

#### DECORATORS #########################################################

import warnings
import functools

from ESMF.api.constants import LogKind, _ESMF_NETCDF
from ESMF.util.exceptions import NetCDFMissing

try:
    import nose

    def expected_failure(test):
        @functools.wraps(test)
        def inner(*args, **kwargs):
            try:
                test(*args, **kwargs)
            except Exception:
                raise nose.SkipTest
        return inner
except:
    def expected_failure(test):
        @functools.wraps(test)
        def inner(*args, **kwargs):
            try:
                test(*args, **kwargs)
            except:
                raise AssertionError('SkipTest: Failure expected')
        return inner


def beta(func):
    '''This is a decorator that can be used to mark functions
    as beta.  Other decorators must be upper.'''

    @functools.wraps(func)
    def new_func(*args, **kwargs):
        # for Python 2.6 to act like 2.7
        warnings.simplefilter(action="default", append=True)
        warnings.warn_explicit(
            message="Call to a beta function {0}.".format(func.__name__),
            category=UserWarning,
            filename=func.__code__.co_filename,
            lineno=func.__code__.co_firstlineno + 1,
        )
        return func(*args, **kwargs)
    return new_func

def deprecated(func):
    '''This is a decorator that can be used to mark functions
    as deprecated. It will result in a warning being emitted
    when the function is used.  Other decorators must be upper.'''

    @functools.wraps(func)
    def new_func(*args, **kwargs):
        # for Python 2.6 to act like 2.7
        warnings.simplefilter(action="ignore", category=DeprecationWarning, append=True)
        warnings.warn_explicit(
            message="Call to deprecated function {0}.".format(func.__name__),
            category=DeprecationWarning,
            filename=func.__code__.co_filename,
            lineno=func.__code__.co_firstlineno + 1,
        )
        return func(*args, **kwargs)
    return new_func

def initialize(func):
    '''This is a decorator that can be used to initialize ESMF, by
    creating a Manager object, if it has not already been done.'''

    @functools.wraps(func)
    def new_func(*args, **kwargs):
        from ESMF.api import esmpymanager

        esmp = esmpymanager.Manager(debug = False)
        return func(*args, **kwargs)
    return new_func

def netcdf(func):
    '''This is a decorator that can be used to error out of functions
    if NetCDF is not available.'''

    @functools.wraps(func)
    def new_func(*args, **kwargs):
        from ESMF.api.constants import _ESMF_NETCDF
        
        if _ESMF_NETCDF:
            return func(*args, **kwargs)
        else:
            raise NetCDFMissing("This function requires ESMF to have been built with NetCDF.")
        
    return new_func

