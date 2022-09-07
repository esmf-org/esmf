"""
vm unit test file
"""

import pytest

from esmpy import *
from esmpy.test.base import TestBase

class TestVM(TestBase):
    def test_vm_broadcast(self):
        mg = Manager()

        bcst = np.ones(4, dtype=np.float64)
        if mg.local_pet != 0:
            bcst[:] = 2

        if mg.local_pet == 0:
            assert(np.all(bcst == 1))
        else:
            assert(np.all(bcst == 2))

        mg.barrier()
        mg._broadcast_(bcst, 4)
        mg.barrier()
        assert(np.all(bcst == 1))

        mg.barrier()

    def test_vm_reduce(self):
        send = np.ones(4, dtype=np.float64)
        recv = np.zeros(4, dtype=np.float64)

        mg = Manager()

        mg.barrier()
        mg._reduce_(send, recv, 4, Reduce.SUM, 0)
        mg.barrier()

        if mg.local_pet == 0:
            assert(np.all(recv == mg.pet_count))
        else:
            assert(np.all(recv == 0))

        mg.barrier()
