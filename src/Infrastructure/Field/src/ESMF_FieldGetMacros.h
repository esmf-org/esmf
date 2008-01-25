#if 0
! $Id: ESMF_FieldGetMacros.h,v 1.20 2008/01/25 21:37:07 feiliu Exp $
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
! Macros for the Field class Get methods.
!------------------------------------------------------------------------------
#endif

#define FieldGetDataPtrDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_FieldGetDataPtr - Get the Fortran data pointer from a Field @\
! @\
! !INTERFACE: @\
! ! Private name; call using ESMF_FieldGetDataPtr() @\
!   subroutine ESMF_FieldGetDataPtr<rank><type><kind>(field, farray, & @\
!          localDE, exclusiveLBound, exclusiveUBound, exclusiveCount, & @\
!          computationalLBound, computationalUBound, computationalCount, & @\
!          totalLBound, totalUBound, totalCount, & @\
!          rc) @\
! @\
! !ARGUMENTS: @\
!      type(ESMF_Field), intent(inout) :: field                  @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: farray @\
!      integer, intent(out), optional :: rc                @\
! @\
! !DESCRIPTION: @\
!     An interface subroutine to {\tt ESMF\_FieldGetDataPtr()}. @\
!     Get the data pointer among other bound information from a {\tt ESMF\_Field}. @\
! @\
!     The arguments are: @\
!     \begin{description} @\
!     \item [field]  @\
!           Pointer to an {\tt ESMF\_Field} object.  @\
!     \item [farray] @\
!           Native fortran data pointer to be copied/referenced in Field @\
!\item[{[localDE]}] @\
!     The local DE from which to get the information.  If not set, defaults to  @\
!     the first DE on this processor. (localDE starts at 0) @\
!\item[{staggerloc}] @\
!     The stagger location to get the information for.  @\
!     Please see Section~\ref{sec:opt:staggerloc} for a list  @\
!     of predefined stagger locations. If not present, defaults to @\
!     ESMF\_STAGGERLOC\_CENTER. @\
!\item[{[exclusiveLBound]}] @\
!     Upon return this holds the lower bounds of the exclusive region. @\
!     {\tt exclusiveLBound} must be allocated to be of size equal to the field rank. @\
!     Please see Section~\ref{sec:field:usage:bounds} for a description @\
!     of the regions and their associated bounds and counts.  @\
!\item[{[exclusiveUBound]}] @\
!     Upon return this holds the upper bounds of the exclusive region. @\
!     {\tt exclusiveUBound} must be allocated to be of size equal to the field rank. @\
!     Please see Section~\ref{sec:field:usage:bounds} for a description @\
!     of the regions and their associated bounds and counts.  @\
!\item[{[exclusiveCount]}] @\
!     Upon return this holds the number of items in the exclusive region per dimension @\
!     (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must @\
!     be allocated to be of size equal to the field rank. @\
!     Please see Section~\ref{sec:field:usage:bounds} for a description @\
!     of the regions and their associated bounds and counts.  @\
!\item[{[computationalLBound]}] @\
!     Upon return this holds the lower bounds of the stagger region. @\
!     {\tt computationalLBound} must be allocated to be of size equal to the field rank. @\
!     Please see Section~\ref{sec:field:usage:bounds} for a description @\
!     of the regions and their associated bounds and counts.  @\
!\item[{[computationalUBound]}] @\
!     Upon return this holds the upper bounds of the stagger region. @\
!     {\tt computationalUBound} must be allocated to be of size equal to the field rank. @\
!     Please see Section~\ref{sec:field:usage:bounds} for a description @\
!     of the regions and their associated bounds and counts.  @\
!\item[{[computationalCount]}] @\
!     Upon return this holds the number of items in the computational region per dimension @\
!     (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount} @\
!      must be allocated to be of size equal to the field rank. @\
!     Please see Section~\ref{sec:field:usage:bounds} for a description @\
!     of the regions and their associated bounds and counts.  @\
!\item[{[totalLBound]}] @\
!     Upon return this holds the lower bounds of the total region. @\
!     {\tt totalLBound} must be allocated to be of size equal to the field rank. @\
!     Please see Section~\ref{sec:field:usage:bounds} for a description @\
!     of the regions and their associated bounds and counts.  @\
!\item[{[totalUBound]}] @\
!     Upon return this holds the upper bounds of the total region. @\
!     {\tt totalUBound} must be allocated to be of size equal to the field rank. @\
!     Please see Section~\ref{sec:field:usage:bounds} for a description @\
!     of the regions and their associated bounds and counts.  @\
!\item[{[totalCount]}] @\
!     Upon return this holds the number of items in the total region per dimension @\
!     (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must @\
!      be allocated to be of size equal to the field rank. @\
!     Please see Section~\ref{sec:field:usage:bounds} for a description @\
!     of the regions and their associated bounds and counts.  @\
!     \item [{[rc]}]  @\
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!     \end{description} @\
! @\
!EOP @\

#if 0
!------------------------------------------------------------------------------
! Get the data pointer from a ESMF_Field
!------------------------------------------------------------------------------
#endif

#define FieldGetDataPtrMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
^undef  ESMF_METHOD @\
^define ESMF_METHOD "ESMF_FieldGetDataPtr" @\
    subroutine ESMF_FieldGetDataPtr##mrank##D##mtypekind(field, farray, & @\
          localDE, exclusiveLBound, exclusiveUBound, exclusiveCount, & @\
          computationalLBound, computationalUBound, computationalCount, & @\
          totalLBound, totalUBound, totalCount, & @\
          rc) @\
 @\
! input arguments @\
      type(ESMF_Field), intent(inout) :: field                  @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: farray @\
      integer,                intent(in) , optional :: localDE @\
      integer,                intent(out), optional :: exclusiveLBound(:) @\
      integer,                intent(out), optional :: exclusiveUBound(:) @\
      integer,                intent(out), optional :: exclusiveCount(:) @\
      integer,                intent(out), optional :: computationalLBound(:) @\
      integer,                intent(out), optional :: computationalUBound(:) @\
      integer,                intent(out), optional :: computationalCount(:) @\
      integer,                intent(out), optional :: totalLBound(:) @\
      integer,                intent(out), optional :: totalUBound(:) @\
      integer,                intent(out), optional :: totalCount(:) @\
      integer, intent(out), optional :: rc                @\
 @\
! local variables @\
      integer          :: localrc, lde @\
 @\
      if (present(rc)) then @\
        rc = ESMF_RC_NOT_IMPL @\
      endif @\
 @\
      ! check variables @\
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc) @\
 @\
      call ESMF_FieldValidate(field, rc=localrc) @\
 @\
      if (ESMF_LogMsgFoundError(localrc, & @\
          ESMF_ERR_PASSTHRU, & @\
          ESMF_CONTEXT, rc)) return @\
 @\
      call ESMF_ArrayGet(field%ftypep%array, farrayPtr=farray, rc=localrc) @\
 @\
      if (ESMF_LogMsgFoundError(localrc, & @\
          ESMF_ERR_PASSTHRU, & @\
          ESMF_CONTEXT, rc)) return @\
 @\
      if(present(localDE)) then @\
            lde = localDE @\
      else @\
            lde = 0 @\
      end if @\
      call ESMF_FieldGetDataBounds(field, lde, & @\
          exclusiveLBound, exclusiveUBound, exclusiveCount, & @\
          computationalLBound, computationalUBound, computationalCount, & @\
          totalLBound, totalUBound, totalCount, & @\
          rc = localrc) @\
 @\
      if (ESMF_LogMsgFoundError(localrc, & @\
          ESMF_ERR_PASSTHRU, & @\
          ESMF_CONTEXT, rc)) return @\
      if (present(rc)) rc = localrc @\
    end subroutine ESMF_FieldGetDataPtr##mrank##D##mtypekind  @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

