#!/usr/bin/env python
#
# $Id: decorators.py,v 1.1.2.1 2013/06/06 19:09:06 rokuingh Exp $

"""
decorators
"""

#### DECORATORS #########################################################

import warnings
import functools
import nose

def expected_failure(test):
    @functools.wraps(test)
    def inner(*args, **kwargs):
        try:
            test(*args, **kwargs)
        except Exception:
            raise nose.SkipTest
        else:
            raise AssertionError('Failure expected')
    return inner

def deprecated(func):
    '''This is a decorator which can be used to mark functions
    as deprecated. It will result in a warning being emitted
    when the function is used.  Other decorators must be upper.'''

    @functools.wraps(func)
    def new_func(*args, **kwargs):
        warnings.warn_explicit(
            "Call to deprecated function {}.".format(func.__name__),
            category=DeprecationWarning,
            filename=func.func_code.co_filename,
            lineno=func.func_code.co_firstlineno + 1
        )
        return func(*args, **kwargs)
    return new_func

import manager
from constants import LogKind

def initialize(func):
    '''This is a decorator which can be used to initialize ESMF, by
    creating a Manager object, if it has not already been done.'''

    @functools.wraps(func)
    def new_func(*args, **kwargs):
        esmp = manager.Manager(logkind = LogKind.SINGLE, debug = False)
        return func(*args, **kwargs)
    return new_func
