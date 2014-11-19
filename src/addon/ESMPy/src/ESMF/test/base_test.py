import unittest
from ESMF.test.base import TestBase
from unittest.case import SkipTest
import numpy as np


class Test(TestBase):
    
    def test_assertNumpyAll_bad_mask(self):
        arr = np.ma.array([1,2,3],mask=[True,False,True])
        arr2 = np.ma.array([1,2,3],mask=[False,True,False])
        with self.assertRaises(AssertionError):
            self.assertNumpyAll(arr,arr2)
            
    def test_assertNumpyAll_type_differs(self):
        arr = np.ma.array([1,2,3],mask=[True,False,True])
        arr2 = np.array([1,2,3])
        with self.assertRaises(AssertionError):
            self.assertNumpyAll(arr,arr2)

if __name__ == "__main__":
    unittest.main()