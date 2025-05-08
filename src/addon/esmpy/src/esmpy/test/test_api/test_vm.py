"""
vm unit test file
"""

import pytest

from esmpy import *
from esmpy.test.base import TestBase

class TestVM(TestBase):
    def test_vm_broadcast(self):

        bcst = np.ones(4, dtype=np.float64)
        if self.mg.local_pet != 0:
            bcst[:] = 2

        if self.mg.local_pet == 0:
            assert(np.all(bcst == 1))
        else:
            assert(np.all(bcst == 2))

        self.mg.barrier()
        self.mg._broadcast_(bcst, 4)
        self.mg.barrier()
        assert(np.all(bcst == 1))

        self.mg.barrier()

    def test_vm_reduce(self):
        send = np.ones(4, dtype=np.float64)
        recv = np.zeros(4, dtype=np.float64)

        mg = Manager()

        self.mg.barrier()
        self.mg._reduce_(send, recv, 4, Reduce.SUM, 0)
        self.mg.barrier()

        if self.mg.local_pet == 0:
            assert(np.all(recv == self.mg.pet_count))
        else:
            assert(np.all(recv == 0))

        self.mg.barrier()
