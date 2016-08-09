
"""
locstream unit test file
"""

from ESMF import *
from ESMF.interface.cbindings import *
from ESMF.test.base import TestBase, attr

class TestLocStream(TestBase):

    def test_create(self):
        # LocStream creation and simple validation
        locstream = LocStream(5, name="Test LocStream")
        assert locstream.size == 5

        locstream["ESMF:X"] = [1, 2, 3, 4, 5]
        locstream["ESMF:Y"] = (7, 7, 7, 7, 7)
        locstream["ESMF:Mask"] = np.array([0, 0, 1, 1, 1])

        if local_pet() == 0:
            assert locstream.lower_bounds == [0]
            assert locstream.upper_bounds == [5]

        print (locstream)

        # test the deleting a LocStream has no ill effects
        del locstream

    def test_mask(self):
        locstream = LocStream(5, name="Test LocStream")
        locstream["ESMF:X"] = (1, 2, 3, 4, 5)
        locstream["ESMF:Y"] = np.array([7, 7, 7, 7, 7])

        assert (locstream.mask == None)

        locstream["ESMF:Mask"] = np.array([0, 0, 1, 1, 1])

        self.assertNumpyAll(locstream.mask, np.array([0, 0, 1, 1, 1]))

    def test_copy(self):
        # LocStream creation and simple validation
        locstream = LocStream(5, name="Test LocStream")
        assert locstream.size == 5
        locstream["ESMF:X"] = [0, 1, 2, 3, 4]

        l2 = locstream.copy()

        assert l2.size == 5
        assert np.all(l2["ESMF:X"] == [0, 1, 2, 3, 4])


    @attr('serial')
    def test_slice(self):
        locstream = LocStream(5, name="Test LocStream")

        locstream2 = locstream[0:2]
        assert(locstream2.size == 2)

        locstream["ESMF:X"] = [0, 1, 2, 3, 4]
        locstream["ESMF:X"] = [0, 1, 2, 3, 4]
        locstream["ESMF:Y"] = (7, 7, 7, 7, 7)
        locstream["ESMF:Mask"] = np.array([0, 0, 1, 1, 1])

        locstream2 = locstream[0:2]
        assert(locstream2.size == 2)

        assert(np.all(locstream["ESMF:X"] == np.array([0, 1, 2, 3, 4])))

    @expected_failure
    def test_pickle(self):
        locstream = LocStream(10, name="Test LocStream")

        import pickle

        pickle.dumps(locstream)

    @expected_failure
    def test_properties(self):
        locstream = LocStream(10, name="Test LocStream")
        locstream["ESMF:X"] = (1, 2, 3, 4, 5)
        locstream["ESMF:Y"] = np.array([7, 7, 7, 7, 7])

        locstream.size = 5
