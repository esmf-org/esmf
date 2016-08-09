# $Id$

"""
unit test file
"""

from ESMF import *
from ESMF.interface.cbindings import *
from ESMF.test.base import TestBase, attr

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


    @expected_failure
    def test_interfaceint2(self):
        # This test should fail
        try:
            a = (ct.c_int*3)()
            a = [1,2,3]
            interfaceint2 = ESMP_InterfaceInt(a)
        except:
            raise TypeError('FAIL: tuples cannot be used in place of numpy.array')

    @expected_failure
    def test_interfaceint3(self):
        # This test should fail
        try:
            interfaceint2 = ESMP_InterfaceInt(np.array([1,2,3]))
        except:
            raise TypeError('FAIL: tuples cannot be used in place of numpy.array')

    def test_version_compare(self):
        assert(version_compare("ESMF_5_3_0_ESMP_02","ESMF_5_3_0_ESMP_01") == 1)
        assert (version_compare("ESMF_5_3_0_ESMP_01",
                           "ESMF_5_3_1_beta_snapshot_02_ESMP_01") == -1)
        assert (version_compare("ESMF_5_3_0_ESMP_01",
                           "ESMF_5_3_0_beta_snapshot_42_ESMP_01") == 1)
        assert (version_compare("ESMF_5_3_0_ESMP_01",
                           "ESMF_5_3_0_beta_snapshot_37_ESMP_02") == 1)
        assert (version_compare("ESMF_5_3_0_ESMP_01",
                           "ESMF_5_3_1_beta_snapshot_02_ESMP_01") == -1)
        assert (version_compare("ESMF_5_3_0_ESMP_01",
                           "ESMF_6_1_0_beta_snapshot_00_ESMP_01") == -1)
        assert (version_compare("ESMF_6_1_0_beta_snapshot_00_ESMP_01",
                           "ESMF_5_3_1_beta_snapshot_02_ESMP_01") == 1)
        assert (version_compare("ESMF_6_1_0_beta_snapshot_00_ESMP_01",
                           "ESMF_6_1_0_beta_snapshot_00_ESMP_01") == 0)
        assert (version_compare("ESMPy_620b10_04",
                           "ESMF_6_1_0_beta_snapshot_00_ESMP_01") == 1)
