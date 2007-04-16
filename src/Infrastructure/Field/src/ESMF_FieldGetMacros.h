#if 0
! $Id: ESMF_FieldGetMacros.h,v 1.13 2007/04/16 21:30:28 rosalind Exp $
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
! Macros for the Field class.
!------------------------------------------------------------------------------
#endif

#if 0
!------------------------------------------------------------------------------
! Documentation for the general FieldGetDataPointer<> macros.
!------------------------------------------------------------------------------
#endif

#define FieldGetDataPointerDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_FieldGetDataPointer - Retrieve Fortran pointer directly from a Field @\
! @\
! !INTERFACE: @\
!      ! Private name; call using ESMF_FieldGetDataPointer() @\
!      subroutine ESMF_FieldGetDataPointer<rank><type><kind>(field, ptr, copyflag, rc) @\
! @\
! !ARGUMENTS: @\
!      type(ESMF_Field), intent(inout) :: field @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: ptr @\
!      type(ESMF_CopyFlag), intent(in), optional :: copyflag @\
!      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Returns a direct Fortran pointer to the data in an {\tt ESMF\_Field}. @\
! Valid type/kind/rank combinations supported by the @\
! framework are: ranks 1 to 7, type real of kind *4 or *8, @\
! and type integer of kind *1, *2, *4, or *8. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[field] @\
!   The {\tt ESMF\_Field} to query. @\
!  \item[ptr] @\
!   An unassociated Fortran pointer of the proper Type, Kind, and Rank as @\
!   the data in the Field.  When this call returns successfully, the pointer @\
!   will now point to the data in the Field.  This is either a reference or @\
!   a copy, depending on the setting of the following argument.  @\
!  \item[{[copyflag]}] @\
!   Defaults to {\tt ESMF\_DATA\_REF}.  If set to {\tt ESMF\_DATA\_COPY}, @\
!   a separate copy of the data will be allocated and the pointer will point @\
!   at the copy.  If a new copy of the data is made, the caller is @\
!   responsible for deallocating the space. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
!EOP @\

#if 0
!------------------------------------------------------------------------------
! Create a new array based on an unallocated Fortran array and a list of counts.
!------------------------------------------------------------------------------
#endif

#define FieldGetDataPointerMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
^undef  ESMF_METHOD @\
^define ESMF_METHOD "ESMF_FieldGetDataPointer" @\
      subroutine ESMF_FieldGetDataPointer##mrank##D##mtypekind(field, ptr, copyflag, counts, rc) @\
 @\
      type(ESMF_Field), intent(inout) :: field @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: ptr @\
      type(ESMF_CopyFlag), intent(in), optional :: copyflag @\
      integer, intent(out), optional :: counts(:) @\
      integer, intent(out), optional :: rc   @\
@\
        ! Local variables @\
        type(ESMF_InternArray) :: array          ! array object @\
        integer :: localrc                   ! local error status @\
        logical :: rcpresent                ! did user specify rc? @\
 @\
        ! Initialize return code; assume failure until success is certain @\
        localrc = ESMF_RC_NOT_IMPL @\
        rcpresent = .FALSE. @\
        array%this = ESMF_NULL_POINTER @\
 @\
        if (present(rc)) then @\
          rcpresent = .TRUE. @\
          rc = ESMF_RC_NOT_IMPL @\
        endif @\
 @\
        ! check variables @\
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc) @\
 @\
        ! Test to see if pointer already associated, and fail if so. @\
        ! TODO: check this - this test seems to always be true, even if @\
        !  the pointer has been nullified first? @\
        !if (associated(ptr)) then @\
        !  if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, & @\
        !                      "Data Pointer cannot already be associated", & @\
        !                      ESMF_CONTEXT, rc)) return @\
        !endif @\
 @\
        call ESMF_FieldGetInternArray(field, array, rc=localrc) @\
        if (ESMF_LogMsgFoundError(localrc, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
 @\
        call ESMF_InternArrayGetData(array, ptr, copyflag, rc=localrc) @\
        if (ESMF_LogMsgFoundError(localrc, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
        if (present(counts)) then @\
          call ESMF_InternArrayGet(array, counts=counts, rc=localrc) @\
          if (ESMF_LogMsgFoundError(localrc, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
        endif @\
 @\
        if (rcpresent) rc = localrc @\
 @\
        end subroutine ESMF_FieldGetDataPointer##mrank##D##mtypekind  @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

