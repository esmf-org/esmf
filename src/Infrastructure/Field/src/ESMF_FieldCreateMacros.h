#if 0
! $Id: ESMF_FieldCreateMacros.h,v 1.29 2007/12/27 21:30:32 feiliu Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
#endif
#if 0
!------------------------------------------------------------------------------
! Macros for the Field Create functions.
!------------------------------------------------------------------------------
#endif

#if 0
!------------------------------------------------------------------------------
! Documentation for the general FieldSetCommit<> macros.
!------------------------------------------------------------------------------
#endif

#define FieldSetCommitDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOPI @\
! !IROUTINE: ESMF_FieldSetCommit - finishes the Field started with FieldCreateEmpty @\
! @\
! !INTERFACE: @\
!   ! Private name; call using ESMF_FieldSetCommit() @\
!   subroutine ESMF_FieldSetCommit<rank><type><kind>(field, grid, & @\
!                                         farray, copyflag, staggerloc, & @\
!                                         gridToFieldMap, ungriddedLBound, & @\
!                                         ungriddedUBound, maxHaloLWidth, & @\
!                                         maxHaloUWidth, rc) @\
! @\
! !ARGUMENTS: @\
!      type(ESMF_Field) :: field @\
!      type(ESMF_Grid) :: grid                  @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: farray @\
!      type(ESMF_CopyFlag), intent(in), optional   :: copyflag @\
!      type(ESMF_StaggerLoc), intent(in), optional ::staggerloc  @\
!      integer, intent(in), optional :: gridToFieldMap(:)     @\
!      integer, intent(in), optional :: ungriddedLBound(:) @\
!      integer, intent(in), optional :: ungriddedUBound(:) @\
!      integer, intent(in), optional :: maxHaloLWidth(:) @\
!      integer, intent(in), optional :: maxHaloUWidth(:) @\
!      integer, intent(out), optional :: rc                @\
! @\
! !DESCRIPTION: @\
!     An interface function to {\tt ESMF\_FieldSetCommit()}. @\
!     finishes the {\tt ESMF\_Field} started with FieldCreateEmpty. @\
! @\
!     The arguments are: @\
!     \begin{description} @\
!     \item [field]  @\
!           Pointer to an {\tt ESMF\_Field} object.  @\
!     \item [grid]  @\
!           Pointer to an {\tt ESMF\_Grid} object.  @\
!     \item [farray] @\
!           Native fortran data pointer to be copied/referenced in Field @\
!     \item [copyflag] @\
!           Whether to copy the existing data space or reference directly. Valid @\
!           values are {\tt ESMF\_DATA\_COPY} or {\tt ESMF\_DATA\_REF}. @\
!     \item [{[staggerloc]}] @\
!           Stagger location of data in grid cells.  For valid  @\
!           predefined values see Section \ref{sec:opt:staggerloc}. @\
!           To create a custom stagger location see Section @\
!           \ref{sec:usage:staggerloc:adv}. @\
!     \item [{[gridToFieldMap]}] @\
!           List that contains as many elements as is indicated by the {\tt grid}'s rank.  @\
!           The list elements map each dimension of the Grid object to a dimension in the @\
!           Field's Array by specifying the appropriate Array dimension index. The default is to @\
!           map all of the grid's dimensions against the lower dimensions of the Field's @\
!           Array in sequence, i.e. gridDimmap = (/1, 2, .../). Unmapped dimensions are @\
!           undistributed dimensions.  The total undistributed dimensions are the total  @\
!           Array dimensions - the distributed dimensions in the Grid (distRank).  All @\
!           gridToFieldMap entries must be greater than or equal to one and smaller than or equal @\
!           to the Array rank. It is erroneous to specify the same entry multiple times @\
!           unless it is zero.  If the Array rank is less than the Grid dimCount then @\
!           the default gridToFieldMap will contain zeros for the dimCount. @\
!           A zero entry in the dimmap indicates that the particular Grid dimension will @\
!           be replicating the Array across the DEs along this direction. @\
!     \item [{[ungriddedLBound]}] @\
!           Lower bounds of the ungridded dimensions of the Field. @\
!     \item [{[ungriddedUBound]}] @\
!           Upper bounds of the ungridded dimensions of the Field. @\
!     \item [{[maxHaloLWidth]}] @\
!           Lower bound of halo region.  Defaults to 0. @\
!     \item [{[maxHaloUWidth]}] @\
!           Upper bound of halo region.  Defaults to 0. @\
!     \item [{[rc]}]  @\
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!     \end{description} @\
! @\
!EOPI @\

#if 0
!------------------------------------------------------------------------------
! Finishes Field created with FieldCreateEmpty
!------------------------------------------------------------------------------
#endif

#define FieldSetCommitMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
^undef  ESMF_METHOD @\
^define ESMF_METHOD "ESMF_FieldSetCommit" @\
    subroutine ESMF_FieldSetCommit##mrank##D##mtypekind(field, grid, farray, & @\
                                         copyflag, staggerloc, & @\
                                         gridToFieldMap, ungriddedLBound, & @\
                                         ungriddedUBound, maxHaloLWidth, & @\
                                         maxHaloUWidth, rc) @\
 @\
! input arguments @\
        type(ESMF_Field) :: field @\
        type(ESMF_Grid) :: grid                  @\
        mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: farray @\
        type(ESMF_CopyFlag), intent(in), optional   :: copyflag @\
        type(ESMF_StaggerLoc), intent(in), optional ::staggerloc  @\
        integer, intent(in), optional :: gridToFieldMap(:)     @\
        integer, intent(in), optional :: ungriddedLBound(:) @\
        integer, intent(in), optional :: ungriddedUBound(:) @\
        integer, intent(in), optional :: maxHaloLWidth(:) @\
        integer, intent(in), optional :: maxHaloUWidth(:) @\
        integer, intent(out), optional :: rc                @\
! local variables @\
        integer                        :: localrc @\
        type(ESMF_Array)               :: array @\
        type(ESMF_DistGrid)            :: distgrid @\
        type(ESMF_ArraySpec)           :: arrayspec @\
 @\
        if (present(rc)) rc = ESMF_RC_NOT_IMPL @\
 @\
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc) @\
 @\
        call ESMF_ArraySpecSet(arrayspec, mrank, ESMF_TYPEKIND_##mtypekind, rc=localrc) @\
        if (ESMF_LogMsgFoundError(localrc, & @\
            ESMF_ERR_PASSTHRU, & @\
            ESMF_CONTEXT, rc)) return @\
 @\
        call ESMF_GridGet(grid, distgrid=distgrid, rc=localrc) @\
        if (ESMF_LogMsgFoundError(localrc, & @\
            ESMF_ERR_PASSTHRU, & @\
            ESMF_CONTEXT, rc)) return @\
 @\
        array = ESMF_ArrayCreate(farray, distgrid=distgrid, & @\
            staggerloc=0, computationalEdgeUWidth=(/-1,-1/), rc=localrc)  @\
        if (ESMF_LogMsgFoundError(localrc, & @\
            ESMF_ERR_PASSTHRU, & @\
            ESMF_CONTEXT, rc)) return @\
 @\
        ! Make sure localStaggerloc has a value before Array create call @\
        if (present(staggerloc)) then @\
            field%ftypep%staggerloc = staggerloc @\
        else @\
            field%ftypep%staggerloc = ESMF_STAGGERLOC_CENTER @\
        endif @\
 @\
        if(present(gridToFieldMap)) then @\
            field%ftypep%gridToFieldMap  = gridToFieldMap @\
        end if @\
        if(present(ungriddedLBound)) then @\
            field%ftypep%ungriddedLBound = ungriddedLBound @\
        end if @\
        if(present(ungriddedUBound)) then @\
            field%ftypep%ungriddedUBound = ungriddedUBound @\
        end if @\
        if(present(maxHaloLWidth)) then @\
            field%ftypep%maxHaloLWidth = maxHaloLWidth @\
        end if @\
        if(present(maxHaloUWidth)) then @\
           field%ftypep%maxHaloUWidth = maxHaloUWidth @\
        end if @\
 @\
        field%ftypep%arrayspec = arrayspec @\
        field%ftypep%array = array @\
        field%ftypep%datastatus = ESMF_STATUS_READY @\
        field%ftypep%grid  = grid @\
        field%ftypep%gridstatus = ESMF_STATUS_READY @\
        field%ftypep%fieldstatus = ESMF_STATUS_READY  @\
 @\
        call ESMF_FieldValidate(field, rc=localrc) @\
 @\
        if (ESMF_LogMsgFoundError(localrc, & @\
          ESMF_ERR_PASSTHRU, & @\
          ESMF_CONTEXT, rc)) return @\
 @\
        if (present(rc)) rc = localrc @\
    end subroutine ESMF_FieldSetCommit##mrank##D##mtypekind  @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Documentation for the general FieldCreateFromDataPtr<> macros.
!------------------------------------------------------------------------------
#endif

#define FieldCreateFromDataPtrDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOPI @\
! !IROUTINE: ESMF_FieldCreateFromDataPtr - creates a Field from fortran array @\
! @\
! !INTERFACE: @\
!   ! Private name; call using ESMF_FieldCreateFromDataPtr() @\
!   function ESMF_FieldCreateFromDataPtr<rank><type><kind>(grid, & @\
!                                         farray, copyflag, staggerloc, & @\
!                                         gridToFieldMap, ungriddedLBound, & @\
!                                         ungriddedUBound, maxHaloLWidth, & @\
!                                         maxHaloUWidth, name, iospec, rc) @\
! @\
! !RETURN VALUE: @\
!      type(ESMF_Field) :: ESMF_FieldCreateFromDataPtr<rank><type><kind> @\
! @\
! !ARGUMENTS: @\
!      type(ESMF_Grid) :: grid                  @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: farray @\
!      type(ESMF_CopyFlag), intent(in), optional   :: copyflag @\
!      type(ESMF_StaggerLoc), intent(in), optional ::staggerloc  @\
!      integer, intent(in), optional :: gridToFieldMap(:)     @\
!      integer, intent(in), optional :: ungriddedLBound(:) @\
!      integer, intent(in), optional :: ungriddedUBound(:) @\
!      integer, intent(in), optional :: maxHaloLWidth(:) @\
!      integer, intent(in), optional :: maxHaloUWidth(:) @\
!      character (len=*), intent(in), optional :: name  @\
!      type(ESMF_IOSpec), intent(in), optional :: iospec @\
!      integer, intent(out), optional :: rc                @\
! @\
! !DESCRIPTION: @\
!     An interface function to {\tt ESMF\_FieldCreateFromDataPtr()}. @\
!     Creates a {\tt ESMF\_Field} from a fortran data pointer and grid. @\
! @\
!     The arguments are: @\
!     \begin{description} @\
!     \item [grid]  @\
!           Pointer to an {\tt ESMF\_Grid} object.  @\
!     \item [farray] @\
!           Native fortran data pointer to be copied/referenced in Field @\
!   \item [copyflag] @\
!           Whether to copy the existing data space or reference directly. Valid @\
!           values are {\tt ESMF\_DATA\_COPY} or {\tt ESMF\_DATA\_REF}. @\
!     \item [{[staggerloc]}] @\
!           Stagger location of data in grid cells.  For valid  @\
!           predefined values see Section \ref{sec:opt:staggerloc}. @\
!           To create a custom stagger location see Section @\
!           \ref{sec:usage:staggerloc:adv}. @\
!     \item [{[gridToFieldMap]}] @\
!           List that contains as many elements as is indicated by the {\tt grid}'s rank.  @\
!           The list elements map each dimension of the Grid object to a dimension in the @\
!           Field's Array by specifying the appropriate Array dimension index. The default is to @\
!           map all of the grid's dimensions against the lower dimensions of the Field's @\
!           Array in sequence, i.e. gridDimmap = (/1, 2, .../). Unmapped dimensions are @\
!           undistributed dimensions.  The total undistributed dimensions are the total  @\
!           Array dimensions - the distributed dimensions in the Grid (distRank).  All @\
!           gridToFieldMap entries must be greater than or equal to one and smaller than or equal @\
!           to the Array rank. It is erroneous to specify the same entry multiple times @\
!           unless it is zero.  If the Array rank is less than the Grid dimCount then @\
!           the default gridToFieldMap will contain zeros for the dimCount. @\
!           A zero entry in the dimmap indicates that the particular Grid dimension will @\
!           be replicating the Array across the DEs along this direction. @\
!     \item [{[ungriddedLBound]}] @\
!           Lower bounds of the ungridded dimensions of the Field. @\
!     \item [{[ungriddedUBound]}] @\
!           Upper bounds of the ungridded dimensions of the Field. @\
!     \item [{[maxHaloLWidth]}] @\
!           Lower bound of halo region.  Defaults to 0. @\
!     \item [{[maxHaloUWidth]}] @\
!           Upper bound of halo region.  Defaults to 0. @\
!     \item [{[name]}]  @\
!           {\tt Field} name.  @\
!     \item [{[iospec]}]  @\
!           I/O specification. ! NOT IMPLEMENTED @\
!     \item [{[rc]}]  @\
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!     \end{description} @\
! @\
!EOPI @\

#if 0
!------------------------------------------------------------------------------
! Create a field from fortran data pointer
!------------------------------------------------------------------------------
#endif

#define FieldCreateFromDataPtrMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
^undef  ESMF_METHOD @\
^define ESMF_METHOD "ESMF_FieldCreateFromDataPtr" @\
    function ESMF_FieldCreateFromDataPtr##mrank##D##mtypekind(grid, & @\
                                         farray, copyflag, staggerloc, & @\
                                         gridToFieldMap, ungriddedLBound, & @\
                                         ungriddedUBound, maxHaloLWidth, & @\
                                         maxHaloUWidth, name, iospec, rc) @\
! return value @\
 @\
      type(ESMF_Field) :: ESMF_FieldCreateFromDataPtr##mrank##D##mtypekind @\
 @\
! input arguments @\
      type(ESMF_Grid) :: grid                  @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: farray @\
      type(ESMF_CopyFlag), intent(in), optional   :: copyflag @\
      type(ESMF_StaggerLoc), intent(in), optional ::staggerloc  @\
      integer, intent(in), optional :: gridToFieldMap(:)     @\
      integer, intent(in), optional :: ungriddedLBound(:) @\
      integer, intent(in), optional :: ungriddedUBound(:) @\
      integer, intent(in), optional :: maxHaloLWidth(:) @\
      integer, intent(in), optional :: maxHaloUWidth(:) @\
      character (len=*), intent(in), optional :: name  @\
      type(ESMF_IOSpec), intent(in), optional :: iospec @\
      integer, intent(out), optional :: rc                @\
 @\
! local variables @\
      integer          :: localrc @\
 @\
      if (present(rc)) rc = ESMF_RC_NOT_IMPL @\
 @\
      ESMF_FieldCreateFromDataPtr##mrank##D##mtypekind = & @\
          ESMF_FieldCreateEmpty(name, iospec, rc=localrc) @\
 @\
      if (ESMF_LogMsgFoundError(localrc, & @\
          ESMF_ERR_PASSTHRU, & @\
          ESMF_CONTEXT, rc)) return @\
 @\
      call ESMF_FieldSetCommit##mrank##D##mtypekind( & @\
          ESMF_FieldCreateFromDataPtr##mrank##D##mtypekind, & @\
          grid, farray, copyflag, & @\
          staggerloc, gridToFieldMap, & @\
          ungriddedLBound, ungriddedUBound, maxHaloLWidth, maxHaloUWidth, & @\
          rc=localrc) @\
 @\
      if (ESMF_LogMsgFoundError(localrc, & @\
          ESMF_ERR_PASSTHRU, & @\
          ESMF_CONTEXT, rc)) return @\
 @\
      call ESMF_FieldValidate(ESMF_FieldCreateFromDataPtr##mrank##D##mtypekind, & @\
          rc=localrc) @\
 @\
      if (ESMF_LogMsgFoundError(localrc, & @\
          ESMF_ERR_PASSTHRU, & @\
          ESMF_CONTEXT, rc)) return @\
 @\
      ESMF_INIT_SET_CREATED(ESMF_FieldCreateFromDataPtr##mrank##D##mtypekind) @\
      if (present(rc)) rc = localrc @\
    end function ESMF_FieldCreateFromDataPtr##mrank##D##mtypekind  @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

