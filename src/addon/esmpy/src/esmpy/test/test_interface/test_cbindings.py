# $Id$

"""
unit test file
"""

import pytest

from esmpy import *
from esmpy.interface.cbindings import *
from esmpy.test.base import TestBase

import numpy as np

class TestCbindings(TestBase):

    def test_log(self):

        Manager()
        flush = True
        ESMP_LogSet(flush)

    def test_vm(self):
        # inquire for rank and proc from ESMF Virtual Machine
        localpet = local_pet()
        petcount = pet_count()

        print ('\nlocal_pet = {0}\n'.format(localpet))
        print ('\npet_count = {0}\n'.format(petcount))


    def test_interfaceint(self):
        Narray = np.array([4,5,6], dtype=np.int32)
        interfaceint = ESMP_InterfaceInt(Narray)


    @pytest.mark.xfail
    def test_interfaceint2(self):
        # This test should fail
        try:
            a = (ct.c_int*3)()
            a = [1,2,3]
            interfaceint2 = ESMP_InterfaceInt(a)
        except:
            raise TypeError('FAIL: tuples cannot be used in place of numpy.array')

    @pytest.mark.xfail
    def test_interfaceint3(self):
        # This test should fail
        try:
            interfaceint2 = ESMP_InterfaceInt(np.array([1,2,3]))
        except:
            raise TypeError('FAIL: tuples cannot be used in place of numpy.array')
