"""
pytest tests
"""

import pytest

from esmpy.test.base import TestBase
from esmpy.api.esmpymanager import pet_count

class TestPyTest(TestBase):

    @pytest.mark.xfail
    @pytest.mark.skipif(pet_count()<3, reason="test must be run with more than 3 cores")
    def test_pytest_singlecorefailure(self):

        print ("Test2: I AM PET {}".format(self.mg.local_pet))
        
        if self.mg.local_pet == 2:
            raise ValueError("Test failure on a single PET")

    # # this requires pytest-mpi package
    # @pytest.mark.mpi(min_size=2)
    # def test_pytest_mpi4py(self):
    #     from mpi4py import MPI
    # 
    #     comm = MPI.COMM_WORLD
    #     rank = comm.Get_rank()
    # 
    #     print ("Test1: I AM PET {} and rank {}".format(local_pet(), rank))
