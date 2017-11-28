import unittest
import numpy as np
import ESMF
from ESMF.util.itester import iter_product_keywords


class TestBase(unittest.TestCase):

    def __init__(self, *args, **kwds):
        super(TestBase, self).__init__(*args, **kwds)

    def assertNumpyAll(self, arr1, arr2, check_fill_value_dtype=True, check_arr_dtype=True):
        """
        :type arr1: :class:`numpy.ndarray`
        :type arr2: :class:`numpy.ndarray`
        :param check_fill_value_dtype: If ``True``, check that the data type for masked array fill values are equal.
        :type check_fill_value_dtype: bool
        """

        self.assertEqual(type(arr1), type(arr2))
        self.assertEqual(arr1.shape, arr2.shape)
        if check_arr_dtype:
            self.assertEqual(arr1.dtype, arr2.dtype)
        if isinstance(arr1, np.ma.MaskedArray) or isinstance(arr2, np.ma.MaskedArray):
            self.assertTrue(np.all(arr1.data == arr2.data))
            self.assertTrue(np.all(arr1.mask == arr2.mask))
            if check_fill_value_dtype:
                self.assertEqual(arr1.fill_value, arr2.fill_value)
            else:
                self.assertTrue(np.equal(arr1.fill_value, arr2.fill_value.astype(arr1.fill_value.dtype)))
            return True
        else:
            return self.assertTrue(np.all(arr1 == arr2))

    def assertNumpyAllClose(self, arr1, arr2):
        self.assertEqual(type(arr1), type(arr2))
        self.assertEqual(arr1.shape, arr2.shape)
        if isinstance(arr1, np.ma.MaskedArray) or isinstance(arr2, np.ma.MaskedArray):
            self.assertTrue(np.allclose(arr1.data, arr2.data))
            self.assertTrue(np.all(arr1.mask == arr2.mask))
            self.assertEqual(arr1.fill_value, arr2.fill_value)
            return True
        else:
            return self.assertTrue(np.allclose(arr1, arr2))

    def assertNumpyNotAll(self, arr1, arr2):
        try:
            self.assertNumpyAll(arr1, arr2)
        except AssertionError:
            ret = True
        else:
            raise AssertionError('Arrays are equivalent.')
        return ret

    def assertNumpyNotAllClose(self, arr1, arr2):
        try:
            self.assertNumpyAllClose(arr1, arr2)
        except AssertionError:
            ret = True
        else:
            raise AssertionError('Arrays are equivalent within precision.')
        return ret

    def assertDictEqual(self, d1, d2, msg=None):
        try:
            unittest.TestCase.assertDictEqual(self, d1, d2, msg=msg)
        except AssertionError:
            for k, v in d1.items():
                self.assertEqual(v, d2[k])
            self.assertEqual(set(d1.keys()), set(d2.keys()))

    def assertWeightFileIsRational(self, filename, src_size, dst_size):
        try:
            from netCDF4 import Dataset
        except ImportError:
            pass
        else:
            ds = Dataset(filename)
            try:
                S = ds.variables['S'][:]
                row = ds.variables['row'][:]
                col = ds.variables['col'][:]
                actual_col_max = col.max()
                actual_row_max = row.max()

                self.assertEqual(actual_col_max, src_size)
                self.assertEqual(actual_row_max, dst_size)

                for urow in np.unique(row):
                    select = row == urow
                    self.assertAlmostEqual(S[select].sum(), 1.0)
            finally:
                ds.close()


    @staticmethod
    def iter_product_keywords(keywords, as_namedtuple=True):
        return iter_product_keywords(keywords, as_namedtuple=as_namedtuple)


def attr(*args, **kwargs):
    """
    Decorator that adds attributes to classes or functions for use with the Attribute (-a) plugin.

    http://nose.readthedocs.org/en/latest/plugins/attrib.html
    """

    def wrap_ob(ob):
        for name in args:
            setattr(ob, name, True)
        for name, value in kwargs.items():
            setattr(ob, name, value)
        return ob

    return wrap_ob

