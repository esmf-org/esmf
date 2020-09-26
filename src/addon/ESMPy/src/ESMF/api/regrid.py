# $Id$

"""
The Regrid API
"""
from ESMF.api import constants
from ESMF.api.field import *


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

    :param string filename: path to the output netCDF weight file.
    :param string rh_filename: path to the output RouteHandle file.
    :param ndarray src_mask_values: a numpy array of values that should be
        considered masked value on the source :class:`~ESMF.api.field.Field`.
    :param ndarray dst_mask_values: a numpy array of values that should be
        considered masked value on the destination :class:`~ESMF.api.field.Field`.
    :param RegridMethod regrid_method: specifies which
        :attr:`~ESMF.api.constants.RegridMethod` to use.  If ``None``, defaults
        to :attr:`~ESMF.api.constants.RegridMethod.BILINEAR`.
    :param PoleMethod pole_method: specifies which type of artificial pole
        to construct on the source :class:`~ESMF.api.grid.Grid` for regridding.  If
        ``None``, defaults to: :attr:`~ESMF.api.constants.PoleMethod.NONE` for
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
        calculating weights for the :attr:`~ESMF.api.constants.ExtrapMethod.NEAREST_IDAVG`
        extrapolation method. A higher value reduces the influence of more distant
        points. If not specified, defaults to ``2.0``.
    :param int extrap_num_levels: The number of levels to output for the extrapolation 
        methods that fill levels (e.g. :attr:`~ESMF.api.constants.ExtrapMethod.CREEP`). 
        When a method is used that requires this, then an error will be returned if it 
        is not specified.
    :param UnmappedAction unmapped_action: specifies which action to take if a
        destination point is found which does not map to any source point. If
        ``None``, defaults to :attr:`~ESMF.api.constants.UnmappedAction.ERROR`.
    :param bool ignore_degenerate: Ignore degenerate cells when checking the
        input :class:`Grids <ESMF.api.grid.Grid>` or :class:`Meshes <ESMF.api.mesh.Mesh>`
        for errors. If this is set to True, then the regridding proceeds, but
        degenerate cells will be skipped. If set to False, a degenerate cell produces
        an error. This currently only applies to :attr:`~ESMF.api.constants.RegridMethod.CONSERVE`,
        other regrid methods currently always skip degenerate cells. If ``None``, defaults
        to ``False``.
    :param ndarray src_frac_field: return a numpy array of values containing
        weights corresponding to the amount of each :class:`~ESMF.api.field.Field`
        value which contributes to the total mass of the :class:`~ESMF.api.field.Field`.
    :param ndarray dst_frac_field: return a numpy array of values containing
        weights corresponding to the amount of each :class:`~ESMF.api.field.Field`
        value which contributes to the total mass of the :class:`~ESMF.api.field.Field`.
    :param bool factors: If ``True``, return the factor and factor index list
        when calling into ``ESMF``'s regrid store method. These lists are converted
        to NumPy arrays and attached to the regrid object. The factor arrays
        are retrievable via :meth:`~ESMF.api.regrid.get_factors` or :meth:`~ESMF.api.regrid.get_weights_dict`.
        See the respective documentation on those methods for additional information.
        For more information on how ``ESMF`` treats factor retrieval see the
        documentation for `ESMF_FieldRegridStore <http://www.earthsystemmodeling.org/esmf_releases/public/ESMF_8_0_1/ESMF_refdoc/node5.html#SECTION050366000000000000000>`_.
    :param bool large_file: If ``True``, create the weight file in NetCDF using the 
        64-bit  offset format to allow variables larger than 2GB. Note the 64-bit offset 
        format is not supported in the NetCDF version earlier than 3.6.0.  An error message 
        will be generated if this flag is specified while the application is linked with a 
        NetCDF library earlier than 3.6.0. Defaults to ``False``.

    """

    @initialize
    def __init__(self, srcfield=None, dstfield=None, filename=None, rh_filename=None, 
                 src_mask_values=None,
                 dst_mask_values=None, regrid_method=None, pole_method=None,
                 regrid_pole_npoints=None, line_type=None, norm_type=None, extrap_method=None,
                 extrap_num_src_pnts=None, extrap_dist_exponent=None, extrap_num_levels=None,
                 unmapped_action=None, ignore_degenerate=None, create_rh=None, filemode=None, 
                 src_file=None, dst_file=None, src_file_type=None, dst_file_type=None, 
                 factors=False, large_file=None,
                 src_frac_field=None, dst_frac_field=None):

        # Confirm the ESMF compiler will suport in-memory factor retrieval
        if factors and not constants._ESMF_USE_INMEM_FACTORS:
            raise RuntimeError("in-memory factors only supported with GNU (gfortran)")


        # Routehandle storage
        self._routehandle = 0

        # Factor storage - only used when "factors=True"
        self._factor_list = None
        self._factor_index_list = None
        self._num_factors = None
        # We need to reference the pointers for deallocation
        self._ptr_fl = None
        self._ptr_fil = None

        # Convert source and destination mask values to NumPy arrays if they
        # are present.
        if not isinstance(src_mask_values, type(None)):
            src_mask_values = np.array(src_mask_values, dtype=np.int32)
        if not isinstance(dst_mask_values, type(None)):
            dst_mask_values = np.array(dst_mask_values, dtype=np.int32)

        # Write weights to file if requested.
        if not isinstance(filename, type(None)):
            if constants._ESMF_COMM == constants._ESMF_COMM_MPIUNI:
                msg = "Regrid(filename) requires PIO and does not work if ESMF has " \
                      "not been built with MPI support"
                raise ImportError(msg)

            self._routehandle = ESMP_FieldRegridStoreFile(
                srcfield,
                dstfield,
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
                filemode=filemode,
                srcFile=src_file,
                dstFile=dst_file,
                srcFileType=src_file_type,
                dstFileType=dst_file_type,
                largeFileFlag=large_file,
                srcFracField=src_frac_field,
                dstFracField=dst_frac_field)
        else:
            # Initialize the factor array pointers if we are returning factors.
            if factors:
                fl = ct.POINTER(ct.c_double)()
                fil = ct.POINTER(ct.c_int)()
                num_factors = ct.c_int(0)  # This is an int*
            else:
                fl = None
                fil = None
                num_factors = None

            self._routehandle = ESMP_FieldRegridStore(
                srcfield,
                dstfield,
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
                extrapNumLevels=extrap_num_levels,
                unmappedaction=unmapped_action,
                ignoreDegenerate=ignore_degenerate,
                factorList=fl,
                factorIndexList=fil,
                numFactors=num_factors,
                srcFracField=src_frac_field,
                dstFracField=dst_frac_field
            )

            # If we are returning factors, store them and cast/convert from
            # ctypes
            if factors:
                self._handle_factors_(fil, fl, num_factors)

        if not isinstance(rh_filename, type(None)):
            ESMP_RouteHandleWrite(self._routehandle, rh_filename)

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
        self._Print = filemode
        self._src_file = src_file
        self._dst_file = dst_file
        self._src_file_type = src_file_type
        self._dst_file_type = dst_file_type
        self._src_frac_field = src_frac_field
        self._dst_frac_field = dst_frac_field
        # factors and large_file are not considered persistent object metadata

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
    def dst_file(self):
        return self._dst_file

    @property
    def dst_file_type(self):
        return self._dst_file_type

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
    def filemode(self):
        return self._filemode

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
    def src_file(self):
        return self._src_file

    @property
    def src_file_type(self):
        return self._src_file_type

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

        # This detects if the object has made it through initialization
        # before the destroy method has been called
        if hasattr(self, '_finalized'):
            if not self._finalized:
                ESMP_FieldRegridRelease(self.routehandle)

                # Also destroy factor allocations in Fortran
                if not isinstance(self._ptr_fl, type(None)):
                    numfac = ct.c_int(self._num_factors)
                    self._factor_list = None
                    self._factor_index_list = None
                    self._num_factors = None
                    ESMP_FieldRegridReleaseFactors(self._ptr_fl,
                                                   self._ptr_fil,
                                                   numfac)
                    self._ptr_fl = None
                    self._ptr_fil = None

                self._finalized = True

    def get_factors(self, deep_copy=False):
        """
        Return factor and factor index arrays. These arrays will only be
        available if the ``Regrid`` object was initialized with ``factors=True``.
        See the `ESMF documentation <http://www.earthsystemmodeling.org/esmf_releases/public/ESMF_8_0_1/ESMF_refdoc/node5.html#SECTION050366000000000000000>`_
        for additional information on these arrays (see below for indexing in
        Python though).

        >>> factors, factors_index = get_factors(...)

        The first tuple element ``factors`` will have shape ``(m,)`` where
        ``m`` is the number of factors or weights. It will be ``dtype(float64)``.
        The second tupe element ``factors_index`` will have shape ``(m, 2)``
        where ``m`` is the number of factors or weights. The source/col indices
        are selected by ``factors_index[:, 0]``. The destination/row indices
        are selected by ``factors_index[:, 1]``. It will be ``dtype(int32)``.

        .. note:: If ``deep_copy=True``, array memory is C contiguous according
            to NumPy array flags (``<array>.flags``).

        .. warning:: Remember to call :meth:`~ESMF.api.regrid.destroy` to deallocate
            memory associated with a regrid operation. This will be called by
            the Python garbage collector. However, if numerous regridding operations
            are called in a tight loop, a memory leak will occur without a call
            to ``destroy``.

        :param bool deep_copy: If ``True``, make deep copies of the returned
            arrays. If ``False`` (the default), the returned arrays will reference
            the underlying ``ESMF`` memory.
        :return: tuple of NumPy array objects
        """

        factor_list = self._factor_list
        factor_index_list = self._factor_index_list
        if deep_copy:
            factor_list = factor_list.copy()
            factor_index_list = factor_index_list.copy()
        return factor_list, factor_index_list

    def get_weights_dict(self, deep_copy=False):
        """
        Return a dictionary mapping that is more user-friendly for weight/factor
        retrieval. Please read the documentation for :meth:`~ESMF.api.regrid.get_factors`
        before using this function.

        =========== =======================
        Key         Value
        =========== =======================
        ``weights`` Weight value array
        ``row_dst`` Destination/row indices
        ``col_src`` Source/col indices
        =========== =======================

        .. note:: If ``deep_copy=True``, array memory is C contiguous according
            to NumPy array flags (``<array>.flags``).

        :param bool deep_copy: If ``True``, make deep copies of the returned
            arrays. If ``False`` (the default), the returned arrays will reference
            the underlying ``ESMF`` memory.
        :return: dict
        """

        fl, fil = self.get_factors()

        col = fil[:, 0].flatten()  # Source indices
        row = fil[:, 1].flatten()  # Destination indices

        if deep_copy:
            row = row.copy()
            col = col.copy()
            fl = fl.copy()

        ret = {'row_dst': row, 'col_src': col, 'weights': fl}

        return ret

    def _handle_factors_(self, fil, fl, num_factors):
        """Handle factor array creation and referencing."""

        self._num_factors = num_factors.value  # Hold integer factor count

        # Only create arrays if we have any factors. There are no factors when
        # grids don't overlap and we are ignoring unmapped.
        if self._num_factors > 0:
            # Pointer to factor list memory. We need to hold on to this for
            # deallocation.
            self._ptr_fl = fl
            # Cast the pointer to the appropriate size.
            cptr_fl = ct.cast(fl, ct.POINTER(ct.c_double * self._num_factors))
            self._factor_list = np.frombuffer(cptr_fl.contents,
                                              count=self._num_factors,
                                              dtype=np.float64)

            # The factor index list is (m, 2) hence the multiplication
            # of the factor count by 2.
            self._ptr_fil = fil  # Hold onto the pointer for deallocation
            cptr_fil = ct.cast(fil,
                               ct.POINTER(ct.c_int * self._num_factors * 2))
            self._factor_index_list = np.frombuffer(cptr_fil.contents,
                                                    count=self._num_factors * 2,
                                                    dtype=np.int32)
            self._factor_index_list = self._factor_index_list.reshape(
                self._num_factors, 2)
        else:
            self._factor_list = np.zeros((0,), dtype=np.float64)
            self._factor_index_list = np.zeros((0, 2), dtype=np.int32)


class RegridFromFile(object):
    """
    The :class:`~ESMF.api.regrid.RegridFromFile` object represents a regridding
    operator between two :class:`Fields <ESMF.api.field.Field>` that is read
    from a file. The creation of this object is analogous to= ESMF_FieldSMMStore(),
    and calling this object corresponds to ESMF_FieldRegrid(). ESMF_FieldRegridRelease()
    is called when the :class:`~ESMF.api.regrid.RegridFromFile` object goes
    out of scope (this only happens when the :class:`~ESMF.api.esmpymanager.Manager`
    goes out of scope, there is a destroy() call for explicit deallocation of
    the :class:`~ESMF.api.regrid.RegridFromFile`).

    For more information about the ESMF Regridding functionality, please see
    the `ESMF Regrid documentation
    <http://www.earthsystemmodeling.org/esmf_releases/public/ESMF_7_1_0r/ESMF_refdoc/node5.html#SECTION05012000000000000000>`_.

    The following arguments are used to create a handle to a regridding
    operation between two :class:`Fields <ESMF.api.field.Field>`.

    *REQUIRED:*

    :param Field srcfield: source :class:`~ESMF.api.field.Field` associated
        with an underlying :class:`~ESMF.api.grid.Grid`, :class:`~ESMF.api.mesh.Mesh`
        or :class:`~ESMF.api.locstream.LocStream`.
    :param Field dstfield: destination :class:`~ESMF.api.field.Field` associated
        with an underlying :class:`~ESMF.api.grid.Grid`, :class:`~ESMF.api.mesh.Mesh`
        or :class:`~ESMF.api.locstream.LocStream`.  The data in this :class:`~ESMF.api.field.Field`
        may be overwritten by this call.
    :param string filename: the name of the file from which to retrieve the
        weights.
    :param string rh_filename: the name of the file from which to retrieve the
        routehandle information.
    """

    @initialize
    def __init__(self, srcfield, dstfield, filename=None, rh_filename=None):

        if (not isinstance(filename, type(None))) and (not isinstance(rh_filename, type(None))):
            raise ValueError('only a regrid file or a routehandle file can be specified')
        elif (isinstance(filename, type(None))) and (isinstance(rh_filename, type(None))):
            raise ValueError('either a regrid file or a routehandle file must be specified')

        if not isinstance(filename, type(None)):
            self._routehandle = ESMP_FieldSMMStore(srcfield, dstfield, filename)
        elif not isinstance(rh_filename, type(None)):
            self._routehandle = ESMP_RouteHandleCreateFromFile(rh_filename)

        # Holds arbitrary metadata if needed by the client.
        self._meta = {}

        # Register with "atexit" to attempt and ensure __del__ is called by
        # the Python garbage collector.
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
        Release the memory associated with the :class:`~ESMF.api.regrid.RegridFromFile`
        object.
        """

        if hasattr(self, '_finalized'):
            if not self._finalized:
                ESMP_FieldRegridRelease(self.routehandle)
                self._finalized = True
