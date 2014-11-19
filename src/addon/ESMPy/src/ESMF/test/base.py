import unittest
import numpy as np

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
            for k, v in d1.iteritems():
                self.assertEqual(v, d2[k])
            self.assertEqual(set(d1.keys()), set(d2.keys()))

    def assertNcEqual(self, uri_src, uri_dest, check_types=True, close=False, metadata_only=False,
                      ignore_attributes=None):
        """
        :param dict ignore_attributes:

        >>> ignore_attributes = {'global': ['history']}
        """
        src = nc.Dataset(uri_src)
        dest = nc.Dataset(uri_dest)

        ignore_attributes = ignore_attributes or {}

        try:
            for dimname, dim in src.dimensions.iteritems():
                self.assertEqual(len(dim), len(dest.dimensions[dimname]))
            self.assertEqual(set(src.dimensions.keys()), set(dest.dimensions.keys()))

            for varname, var in src.variables.iteritems():
                dvar = dest.variables[varname]
                try:
                    if not metadata_only:
                        if close:
                            self.assertNumpyAllClose(var[:], dvar[:])
                        else:
                            self.assertNumpyAll(var[:], dvar[:], check_arr_dtype=check_types)
                except AssertionError:
                    cmp = var[:] == dvar[:]
                    if cmp.shape == (1,) and cmp.data[0] == True:
                        pass
                    else:
                        raise
                if check_types:
                    self.assertEqual(var[:].dtype, dvar[:].dtype)
                for k, v in var.__dict__.iteritems():
                    to_test_attr = getattr(dvar, k)
                    try:
                        self.assertNumpyAll(v, to_test_attr)
                    except AttributeError:
                        self.assertEqual(v, to_test_attr)
                self.assertEqual(var.dimensions, dvar.dimensions)
            self.assertEqual(set(src.variables.keys()), set(dest.variables.keys()))

            if 'global' not in ignore_attributes:
                self.assertDictEqual(src.__dict__, dest.__dict__)
            else:
                for k, v in src.__dict__.iteritems():
                    if k not in ignore_attributes['global']:
                        to_test = dest.__dict__[k]
                        try:
                            self.assertNumpyAll(v, to_test)
                        except AttributeError:
                            self.assertEqual(v, to_test)
        finally:
            src.close()
            dest.close()