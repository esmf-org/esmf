#if 0
! $Id: ESMF_InternArrayGetMacros.h,v 1.7.2.2 2009/01/21 21:25:22 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
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
! Declare a wrapper for a Fortran array pointer, a local variable of that type,
!  type which wraps every pointer type, and a declaration of local variables,
!  one of each wrapper type.
!------------------------------------------------------------------------------
#endif

#define ArrayTypeMacro(mname, mtypekind, mrank, mdim) \
! <Created by macro - do not edit directly > @\
      type ESMF_ArrWrap##mrank##D##mtypekind @\
        mname (ESMF_KIND_##mtypekind),dimension(mdim),pointer :: ptr##mrank##D##mtypekind @\
      end type ESMF_ArrWrap##mrank##D##mtypekind @\


#if 0
!------------------------------------------------------------------------------
! Get a Fortran pointer to the data contained in this array
!------------------------------------------------------------------------------
#endif

#define ArrayGetDataDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly >  @\
!BOP @\
! !IROUTINE: ESMF_InternArrayGetData - Retrieve a Fortran pointer to Array data @\
! @\
! !INTERFACE: @\
!      ! Private name; call using ESMF_InternArrayGetData() @\
!      subroutine ESMF_ArrayGetData<rank><type><kind>(array, fptr, docopy, rc) @\
! @\
! !ARGUMENTS: @\
!      type(ESMF_InternArray) :: array @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: fptr @\
!      type(ESMF_CopyFlag), intent(in), optional :: docopy @\
!      integer, intent(out), optional :: rc @\
! @\
! !DESCRIPTION: @\
!      Given an {\tt ESMF\_Array} @\
!      return a Fortran pointer to the existing data buffer, @\
!      or return a Fortran pointer to a new copy of the data. @\
! Valid type/kind/rank combinations supported by the @\
! framework are: ranks 1 to 7, type real of kind *4 or *8, @\
! and type integer of kind *1, *2, *4, or *8. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[array] @\
!    An {\tt ESMF\_Array}. @\
!  \item[farr] @\
!    An allocatable (but currently unallocated) Fortran array pointer.  @\
!  \item[docopy] @\
!   Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference @\
!   the existing data array.  If set to {\tt ESMF\_DATA\_COPY} this routine @\
!   allocates new space and copies the data from the pointer into the space. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
!EOP @\
 @\

#define ArrayGetDataMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly >  @\
^undef  ESMF_METHOD @\
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" @\
^define ESMF_METHOD "ESMF_InternArrayGetData" @\
  subroutine ESMF_ArrayGetData##mrank##D##mtypekind(array, fptr, docopy, rc) @\
 @\
    type(ESMF_InternArray) :: array @\
    mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: fptr @\
    type(ESMF_CopyFlag), intent(in), optional :: docopy @\
    integer, intent(out), optional :: rc @\
 @\
    integer :: status                   ! local error status @\
    logical :: rcpresent                ! did user specify rc? @\
    logical :: copyreq                  ! did user specify copy? @\
 @\
    type (ESMF_ArrWrap##mrank##D##mtypekind) :: wrap     ! for passing f90 ptr to C++ @\
    integer :: lb(mrank), ub(mrank)  ! size info for the array @\
    mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: localp ! local copy @\
 @\
    ! initialize return code; assume routine not implemented @\
    status = ESMF_RC_NOT_IMPL @\
    rcpresent = .FALSE. @\
    if (present(rc)) then @\
      rcpresent = .TRUE. @\
      rc = ESMF_RC_NOT_IMPL @\
    endif @\
 @\
    ! Check init status of arguments @\
    ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) @\
 @\
    copyreq = .FALSE. @\
 @\
    ! check copyflag to see if we are making a reference @\
    ! or making a new array and a copy @\
    if (present(docopy)) then @\
      if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. @\
    endif @\
 @\
    call c_ESMC_IArrayGetF90Ptr(array, wrap, status) @\
    if (ESMF_LogMsgFoundError(status, & @\
                              ESMF_ERR_PASSTHRU, & @\
                              ESMF_CONTEXT, rc)) return @\
 @\
    ! Allocate a new buffer if requested and return a copy @\
    if (copyreq) then @\
      call c_ESMC_IArrayGetLbounds(array, mrank, lb, status) @\
      if (ESMF_LogMsgFoundError(status, & @\
                              ESMF_ERR_PASSTHRU, & @\
                              ESMF_CONTEXT, rc)) return @\
      call c_ESMC_IArrayGetUbounds(array, mrank, ub, status) @\
      if (ESMF_LogMsgFoundError(status, & @\
                              ESMF_ERR_PASSTHRU, & @\
                              ESMF_CONTEXT, rc)) return @\
      allocate(localp( mrng ), stat=status) @\
      if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & @\
                                   ESMF_CONTEXT, rc)) return @\
      ! this must do a contents assignment @\
      localp = wrap % ptr##mrank##D##mtypekind @\
      fptr => localp  @\
    else @\
      fptr => wrap % ptr##mrank##D##mtypekind @\
    endif @\
 @\
    if (rcpresent) rc = ESMF_SUCCESS @\
 @\
  end subroutine ESMF_ArrayGetData##mrank##D##mtypekind @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

