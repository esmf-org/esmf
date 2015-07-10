# $Id$

"""
The LocStream API
"""

#### IMPORT LIBRARIES #########################################################

from ESMF.api.esmpymanager import *
from ESMF.api.array import Array1D
import ESMF.api.constants as constants
from copy import copy
from ESMF.util.slicing import get_formatted_slice


#### LocStream class #########################################################

class LocStream(dict):

    @initialize
    def __init__(self, location_count, coord_sys=None, name=None, esmf=True):
        '''
        Create a LocStream. \n
        Required Arguments: \n
            location_count: the number of locations in this stream. \n
        Optional Arguments: \n
            coord_sys: the coordinates system for the Grid. \n
                    Argument values are:\n
                        (default) CoordSys.CART\n
                        CoordSys.SPH_DEG\n
                        CoordSys.SPH_RAD\n
            name: user friendly name for the LocStream. \n
            esmf: internal parameter to allow "shallow" copies. \n
        '''

        # for ocgis compatibility
        self._ocgis = {}

        # bookkeeping
        self._rank = 1
        self._name = name
        self._size = location_count

        # call the ESMP layer
        if esmf:
            self._struct = ESMP_LocStreamCreateLocal(location_count,
                                                     coordSys=coord_sys)

            # get bounds
            lbounds, ubounds = ESMP_LocStreamGetBounds(self.struct)
            self._lower_bounds = lbounds
            self._upper_bounds = ubounds

        # regist with atexit
        import atexit; atexit.register(self.__del__)
        self._finalized = False

        # set the single stagger flag
        self._singlestagger = True

        super(LocStream, self).__init__()

    # manual destructor
    def destroy(self):
        """
        Release the memory associated with a LocStream. \n
        Required Arguments: \n
            None \n
        Optional Arguments: \n
            None \n
        Returns: \n
            None \n
        """
        if hasattr(self, '_finalized'):
            if not self._finalized:
                ESMP_LocStreamDestroy(self)
                self._finalized = True

    def __del__(self):
        """
        Release the memory associated with a LocStream. \n
        Required Arguments: \n
            None \n
        Optional Arguments: \n
            None \n
        Returns: \n
            None \n
        """
        self.destroy()

    def __repr__(self):
        """
        Return a string containing a printable representation of the object
        """
        string = ("LocStream:\n"
                  "    name = %r \n"
                  "    size = %r \n"
                  "    lower_bounds = %r \n"
                  "    upper_bounds = %r \n"
                  "    keys = %r \n"
                  %
                  (self.name,
                   self.size,
                   self.lower_bounds,
                   self.upper_bounds,
                   self.items(),
                   ))

        return string

    def __getitem__(self, slc):
        # initialize slc_ls
        slc_ls = slc

        # check that this is actually a slicing operation and not just regular item retrieval
        if not isinstance(slc, basestring):
            # parallel slicing is not yet enabled (collective operation)
            if pet_count() > 1:
                raise SerialMethod
            # re-initialize slc_ls
            slc_ls = get_formatted_slice(slc, self.rank)

        # slice at will
        try:
            ret = super(LocStream, self).__getitem__(slc_ls)
        except TypeError:
            ret = self.copy()

            # upper bounds and size
            ret._upper_bounds = len(range(slc_ls.stop - slc_ls.start))
            ret._size = ret.upper_bounds - ret.lower_bounds

            # keys
            for x in ret.iterkeys():
                ret[x] = super(LocStream, self).__getitem__(x)[slc_ls]

        return ret

    def __setitem__(self, key, value):
        # check types
        if not isinstance(value, (list, tuple, np.ndarray, Array1D)):
            raise ValueError("type of value must be list, tuple, or numpy array")
        if type(value) is not np.ndarray:
            value = np.array(value)
        if len(value) != self.size:
            raise ValueError("value must be of length "+str(self.size))

        if key not in self:
            self.add(key, typekind=constants._Python2ESMFType[type(value[0])])

        ret = dict.__setitem__(self, key, value)

        return ret

    def add(self, key_name, typekind=None):
        '''
        Add a key to a LocStream. \n
        Required Arguments: \n
            key_name: the name of the key. \n
        Optional Arguments: \n
            typekind: the type of the LocStream key data. \n
                Argument values are: \n
                    TypeKind.I4 \n
                    TypeKind.I8 \n
                    TypeKind.R4 \n
                    (default) TypeKind.R8 \n
        '''

        # allocation the key
        ESMP_LocStreamAddKeyAlloc(self.struct, key_name, keyTypeKind=typekind)

        # get a pointer to the Fortran buffer to the key
        key_ptr = ESMP_LocStreamGetKeyPtr(self.struct, key_name)

        # create an Array1D object out of the pointer
        value = Array1D(key_ptr, dtype=typekind, size=self.size)

        return value

    def copy(self):
        # shallow copy
        ret = LocStream(self._size, name=self._name, esmf=False)

        ret._struct = self._struct
        ret._lower_bounds = self._lower_bounds
        ret._upper_bounds = self._upper_bounds

        for key, value in self.iteritems():
            super(LocStream, ret).__setitem__(key, value)

        # don't call ESMF destructor twice on the same shallow Python object
        ret._finalized = True

        return ret

    @property
    def struct(self):
        return self._struct

    @property
    def rank(self):
        return self._rank

    @property
    def size(self):
        return self._size

    @property
    def name(self):
        return self._name

    @property
    def lower_bounds(self):
        return self._lower_bounds

    @property
    def upper_bounds(self):
        return self._upper_bounds

    @property
    def singlestagger(self):
        return self._singlestagger

