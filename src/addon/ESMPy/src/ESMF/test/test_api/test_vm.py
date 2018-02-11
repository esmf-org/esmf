"""
vm unit test file
"""

from ESMF import *
from ESMF.interface.cbindings import *
from ESMF.test.base import TestBase, attr

@attr('parallel')
class TestVM(TestBase):
    # this is for the documentation, do not modify
    def test_vm_reduce(self):
        send = np.ones(4, dtype=np.float64)
        recv = np.zeros(4, dtype=np.float64)

        mg = Manager()

        mg._reduce_(send, recv, 4, Reduce.SUM, 0)

        if mg.local_pet == 0:
            assert(np.all(recv == mg.pet_count))
        else:
            assert(np.all(recv == 0))
