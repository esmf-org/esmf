# $Id$

"""
The Regrid API
"""

#### IMPORT LIBRARIES #########################################################
from ESMF.api.constants import *
from ESMF.interface.cbindings import *
from ESMF.util.decorators import initialize

from ESMF.api.esmpymanager import *
from ESMF.api.grid import *
from ESMF.api.mesh import *
from ESMF.api.field import *

#### Regrid class ##############################################################

class Regrid(object):
    """
    The :class:`~ESMF.api.regrid.Regrid` object represents a regridding operator between two :class:`Fields <ESMF.api.field.Field>`.  The
    creation of this object is analogous to ESMF_FieldRegridStore(), and
    calling this object corresponds to ESMF_FieldRegrid().
    ESMF_FieldRegridRelease() is called when the :class:`~ESMF.api.regrid.Regrid` object goes out of
    scope (this only happens when the :class:`~ESMF.api.esmpymanager.Manager` goes out of scope, there is a
    destroy() call for explicit deallocation of the :class:`~ESMF.api.regrid.Regrid`).

    For more information about the ESMF Regridding functionality, please see
    the `ESMF Regrid documentation
    <http://www.earthsystemmodeling.org/esmf_releases/public/ESMF_7_1_0r/ESMF_refdoc/node5.html#SECTION05012000000000000000>`_.

    The following arguments are used to create a handle to a Regridding
    operation between two :class:`Fields <ESMF.api.field.Field>`.

    *REQUIRED:*

    :param Field srcfield: source :class:`~ESMF.api.field.Field` associated with an underlying :class:`~ESMF.api.grid.Grid`,
        :class:`~ESMF.api.mesh.Mesh` or :class:`~ESMF.api.locstream.LocStream`.
    :param Field dstfield: destination :class:`~ESMF.api.field.Field` associated with an underlying
        :class:`~ESMF.api.grid.Grid`, :class:`~ESMF.api.mesh.Mesh` or :class:`~ESMF.api.locstream.LocStream`.  The data in this :class:`~ESMF.api.field.Field` may be overwritten
        by this call.

    *OPTIONAL:*

    :param string filename: the name of the file for writing weights.
    :param ndarray src_mask_values: a numpy array of values that should be
        considered masked value on the source :class:`~ESMF.api.field.Field`.
    :param ndarray dst_mask_values: a numpy array of values that should be
        considered masked value on the destination :class:`~ESMF.api.field.Field`.
    :param RegridMethod regrid_method: specifies which
        :attr:`~ESMF.api.constants.RegridMethod` to use.  If ``None``, defaults
        to :attr:`~ESMF.api.constants.RegridMethod.BILINEAR`.
    :param PoleMethod pole_method: specifies which type of artificial pole
        to construct on the source :class:`~ESMF.api.grid.Grid` for regridding.  If ``None``, defaults
        to: :attr:`~ESMF.api.constants.PoleMethod.NONE` for
        regridmethod == :attr:`~ESMF.api.constants.RegridMethod.CONSERVE`, or
        :attr:`~ESMF.api.constants.PoleMethod.ALLAVG` for
        regridmethod != :attr:`~ESMF.api.constants.RegridMethod.CONSERVE`.
    :param int regrid_pole_npoints: specifies how many points to average over
        if polemethod == :attr:`~ESMF.api.constants.PoleMethod.ALLAVG`.
    :param LineType line_type: select the path of the line that connects two
        points on the surface of a sphere.  This in turn controls the path along
        which distances are calculated and the shape of the edges that make up a
        cell.  If ``None``, defaults to:
        :attr:`~ESMF.api.constants.LineType.GREAT_CIRCLE` for
        regridmethod == :attr:`~ESMF.api.constants.RegridMethod.CONSERVE`, or
        :attr:`~ESMF.api.constants.LineType.CART` for
        regridmethod != :attr:`~ESMF.api.constants.RegridMethod.CONSERVE`.
    :param NormType norm_type: control which type of normalization to do when
        generating conservative regridding weights. If ``None``, defaults to
        :attr:`~ESMF.api.constants.NormType.DSTAREA`.
    :param ExtrapMethod extrap_method: Specify which extrapolation method to use on 
        unmapped destination points after regridding.
    :param int extrap_num_src_pnts: The number of source points to use for the 
        extrapolation methods that use more than one source point 
        (e.g. :attr:`~ESMF.api.constants.ExtrapMethod.NEAREST_IDAVG`). If not 
        specified, defaults to 8.
    :param float extrap_dist_exponent: The exponent to raise the distance to when 
        calculating weights for the 
        :attr:`~ESMF.api.constants.ExtrapMethod.NEAREST_IDAVG` extrapolation 
        method. A higher value reduces the influence of more distant points. If 
        not specified, defaults to 2.0.
    :param UnmappedAction unmapped_action: specifies which action to take if a
        destination point is found which does not map to any source point. If
        ``None``, defaults to :attr:`~ESMF.api.constants.UnmappedAction.ERROR`.
    :param bool ignore_degenerate: Ignore degenerate cells when checking the
        input :class:`Grids <ESMF.api.grid.Grid>` or :class:`Meshes <ESMF.api.mesh.Mesh>` for errors. If this is set to True, then the
        regridding proceeds, but degenerate cells will be skipped. If set to
        False, a degenerate cell produces an error. This currently only applies
        to :attr:`~ESMF.api.constants.RegridMethod.CONSERVE`, other regrid
        methods currently always skip degenerate cells. If ``None``, defaults to
        False.
    :param ndarray src_frac_field: return a numpy array of values containing
        weights corresponding to the amount of each :class:`~ESMF.api.field.Field` value which
        contributes to the total mass of the :class:`~ESMF.api.field.Field`.
    :param ndarray dst_frac_field: return a numpy array of values containing
        weights corresponding to the amount of each :class:`~ESMF.api.field.Field` value which
        contributes to the total mass of the :class:`~ESMF.api.field.Field`.
    """

    # call RegridStore
    @initialize
    def __init__(self, srcfield=None, dstfield=None, filename=None,
                 src_mask_values=None,
                 dst_mask_values=None,
                 regrid_method=None,
                 pole_method=None,
                 regrid_pole_npoints=None,
                 line_type=None,
                 norm_type=None,
                 extrap_method=None,
                 extrap_num_src_pnts=None,
                 extrap_dist_exponent=None,
                 unmapped_action=None,
                 ignore_degenerate=None,
                 create_rh=None,
                 src_frac_field=None,
                 dst_frac_field=None):

        # routehandle storage
        self._routehandle = 0

        # type checking
        if src_mask_values is not None:
            src_mask_values = np.array(src_mask_values, dtype=np.int32)

        # else case handled by initialization to None
        if dst_mask_values is not None:
            dst_mask_values = np.array(dst_mask_values, dtype=np.int32)

        # call into the ctypes layer
        if (filename):
            import ESMF.api.constants as constants
            if constants._ESMF_COMM is constants._ESMF_COMM_MPIUNI:
                raise ImportError("Regrid(filename) requires PIO and does not work if ESMF has not been built with MPI support")

            self._routehandle = ESMP_FieldRegridStoreFile(srcfield, dstfield,
                               filename,
                               srcMaskValues=src_mask_values,
                               dstMaskValues=dst_mask_values,
                               regridmethod=regrid_method,
                               polemethod=pole_method,
                               regridPoleNPnts=regrid_pole_npoints,
                               lineType=line_type,
                               normType=norm_type,
                               unmappedaction=unmapped_action,
                               ignoreDegenerate=ignore_degenerate,
                               createRH=create_rh,
                               srcFracField=src_frac_field,
                               dstFracField=dst_frac_field)
        else:
            self._routehandle = ESMP_FieldRegridStore(srcfield, dstfield,
                               srcMaskValues=src_mask_values,
                               dstMaskValues=dst_mask_values,
                               regridmethod=regrid_method,
                               polemethod=pole_method,
                               regridPoleNPnts=regrid_pole_npoints,
                               lineType=line_type,
                               normType=norm_type,
                               extrapMethod=extrap_method,
                               extrapNumSrcPnts=extrap_num_src_pnts,
                               extrapDistExponent=extrap_dist_exponent,
                               unmappedaction=unmapped_action,
                               ignoreDegenerate=ignore_degenerate,
                               srcFracField=src_frac_field,
                               dstFracField=dst_frac_field)
        
        self._srcfield = srcfield
        self._dstfield = dstfield
        self._src_mask_values = src_mask_values
        self._dst_mask_values = dst_mask_values
        self._regrid_method = regrid_method
        self._pole_method = pole_method
        self._regrid_pole_npoints = regrid_pole_npoints
        self._norm_type = norm_type
        self._extrap_method = extrap_method
        self._extrap_num_src_pnts = extrap_num_src_pnts
        self._extrap_dist_exponent = extrap_dist_exponent
        self._unmapped_action = unmapped_action
        self._ignore_degenerate = ignore_degenerate
        self._src_frac_field = src_frac_field
        self._dst_frac_field = dst_frac_field

        # for arbitrary metadata
        self._meta = {}

        # regist with atexit
        import atexit; atexit.register(self.__del__)
        self._finalized = False

    def __call__(self, srcfield, dstfield, zero_region=None):
        """
        Call a regridding operation from srcfield to dstfield.

        *REQUIRED:*

        :param Field srcfield: the :class:`~ESMF.api.field.Field` of source data to regrid.
        :param Field dstfield: the :class:`~ESMF.api.field.Field` to hold the regridded data.

        *OPTIONAL:*

        :param Region zero_region: specify which region of the field indices
            will be zeroed out before adding the values resulting from the
            interpolation.  If ``None``, defaults to
            :attr:`~ESMF.api.constants.Region.TOTAL`.

        :return: dstfield
        """
        # call into the ctypes layer
        ESMP_FieldRegrid(srcfield, dstfield,
                         self._routehandle, zeroregion=zero_region)
        return dstfield

    def __del__(self):
        self.destroy()

    def __repr__(self):
        string = ("Regrid:\n"
                  "    routehandle = %r\n"
                  "    src_mask_values = %r\n"
                  "    dst_mask_values = %r\n"
                  "    regrid_method = %r\n"
                  "    unmapped_action = %r\n"
                  "    src_frac_field = %r\n"
                  "    dst_frac_field = %r\n"
                  "    srcfield = %r\n"
                  "    dstfield = %r\n"
                  %
                  (self._routehandle,
                   self.src_mask_values,
                   self.dst_mask_values,
                   self.regrid_method,
                   self.unmapped_action,
                   self.src_frac_field,
                   self.dst_frac_field,
                   self.srcfield,
                   self.dstfield))

        return string

    @property
    def dstfield(self):
        return self._dstfield

    @property
    def dst_frac_field(self):
        return self._dst_frac_field

    @property
    def dst_mask_values(self):
        return self._dst_mask_values

    @property
    def extrap_method(self):
        return self._extrap_method

    @property
    def extrap_num_src_pnts(self):
        return self._extrap_num_src_pnts

    @property
    def extrap_dist_exponent(self):
        return self._extrap_dist_exponent

    @property
    def finalized(self):
        """
        :rtype: bool
        :return: Indicate if the underlying ESMF memory for this object has
            been deallocated.
        """

        return self._finalized

    @property
    def ignore_degenerate(self):
        return self._ignore_degenerate

    @property
    def meta(self):
        """
        :rtype: tdk
        :return: tdk
        """

        return self._meta

    @property
    def norm_type(self):
        return self._norm_type

    @property
    def pole_method(self):
        return self._pole_method

    @property
    def regrid_method(self):
        return self._regrid_method

    @property
    def regrid_pole_npoints(self):
        return self._regrid_pole_npoints

    @property
    def routehandle(self):
        return self._routehandle

    @property
    def srcfield(self):
        return self._srcfield

    @property
    def src_frac_field(self):
        return self._src_frac_field

    @property
    def src_mask_values(self):
        return self._src_mask_values

    @property
    def struct(self):
        """
        :rtype: pointer
        :return: A pointer to the underlying ESMF allocation for this
            :class:`~ESMF.api.regrid.Regrid`.
        """

        return self.struct

    @property
    def unmapped_action(self):
        return self._unmapped_action

    def copy(self):
        """
        Copy a :class:`~ESMF.api.regrid.Regrid` in an ESMF-safe manner.

        :return: A :class:`~ESMF.api.regrid.Regrid` shallow copy.
        """

        # shallow copy
        ret = copy(self)
        # don't call ESMF destructor twice on the same shallow Python object
        ret._finalized = True

        return ret

    def destroy(self):
        """
        Release the memory associated with a :class:`~ESMF.api.regrid.Regrid`.
        """

        if hasattr(self, '_finalized'):
            if not self._finalized:
                ESMP_FieldRegridRelease(self.routehandle)
                self._finalized = True

class RegridFromFile(object):
    """
    The :class:`~ESMF.api.regrid.RegridFromFile` object represents a regridding operator between two
    :class:`Fields <ESMF.api.field.Field>` that is read from a file. The creation of this object is analogous to
    ESMF_FieldSMMStore(), and calling this object corresponds to
    ESMF_FieldRegrid(). ESMF_FieldRegridRelease() is called when the
    :class:`~ESMF.api.regrid.RegridFromFile` object goes out of scope (this only happens when the :class:`~ESMF.api.esmpymanager.Manager`
    goes out of scope, there is a destroy() call for explicit deallocation of
    the :class:`~ESMF.api.regrid.RegridFromFile`).

    For more information about the ESMF Regridding functionality, please see
    the `ESMF Regrid documentation
    <http://www.earthsystemmodeling.org/esmf_releases/public/ESMF_7_1_0r/ESMF_refdoc/node5.html#SECTION05012000000000000000>`_.

    The following arguments are used to create a handle to a regridding
    operation between two :class:`Fields <ESMF.api.field.Field>`.

    *REQUIRED:*

    :param Field srcfield: source :class:`~ESMF.api.field.Field` associated with an underlying :class:`~ESMF.api.grid.Grid`,
        :class:`~ESMF.api.mesh.Mesh` or :class:`~ESMF.api.locstream.LocStream`.
    :param Field dstfield: destination :class:`~ESMF.api.field.Field` associated with an underlying
        :class:`~ESMF.api.grid.Grid`, :class:`~ESMF.api.mesh.Mesh` or :class:`~ESMF.api.locstream.LocStream`.  The data in this :class:`~ESMF.api.field.Field` may be overwritten
        by this call.
    :param string filename: the name of the file from which to retrieve the
        weights.
    """
    # call RegridStore
    @initialize
    def __init__(self, srcfield, dstfield, filename):

        self._routehandle = ESMP_FieldSMMStore(srcfield, dstfield, filename)

        # for arbitrary metadata
        self._meta = {}

        # regist with atexit
        import atexit; atexit.register(self.__del__)
        self._finalized = False

    def __call__(self, srcfield, dstfield, zero_region=None):
        """
        Call a regridding operation from srcfield to dstfield.

        *REQUIRED:*

        :param Field srcfield: the :class:`~ESMF.api.field.Field` of source data to regrid.
        :param Field dstfield: the :class:`~ESMF.api.field.Field` to hold the regridded data.

        *OPTIONAL:*

        :param Region zero_region: specify which region of the field indices
            will be zeroed out before adding the values resulting from the
            interpolation.  If ``None``, defaults to
            :attr:`~ESMF.api.constants.Region.TOTAL`.

        :return: dstfield
        """

        # call into the ctypes layer
        ESMP_FieldRegrid(srcfield, dstfield,
                         self._routehandle, zeroregion=zero_region)
        return dstfield

    def __del__(self):
        self.destroy()

    def __repr__(self):
        string = "RegridFromFile:\n    routehandle = {}\n".format(self._routehandle)
        return string

    @property
    def dstfield(self):
        return self._dstfield

    @property
    def finalized(self):
        """
        :rtype: bool
        :return: Indicate if the underlying ESMF memory for this object has
            been deallocated.
        """

        return self._finalized

    @property
    def meta(self):
        """
        :rtype: tdk
        :return: tdk
        """

        return self._meta

    @property
    def routehandle(self):
        return self._routehandle

    @property
    def struct(self):
        """
        :rtype: pointer
        :return: A pointer to the underlying ESMF allocation for this
            :class:`~ESMF.api.regrid.Regrid`.
        """

        return self.struct

    def copy(self):
        """
        Copy a :class:`~ESMF.api.regrid.Regrid` in an ESMF-safe manner.

        :return: A :class:`~ESMF.api.regrid.Regrid` shallow copy.
        """

        # shallow copy
        ret = copy(self)
        # don't call ESMF destructor twice on the same shallow Python object
        ret._finalized = True

        return ret

    def destroy(self):
        """
        Release the memory associated with a :class:`~ESMF.api.regrid.Regrid`.
        """

        if hasattr(self, '_finalized'):
            if not self._finalized:
                ESMP_FieldRegridRelease(self.routehandle)
                self._finalized = True

