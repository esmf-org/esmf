#if 0
! $Id: ESMF_StateMacros.h,v 1.15 2007/04/24 01:15:25 rosalind Exp $
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
! Macros for the State class.
!------------------------------------------------------------------------------
#endif

#if 0
!------------------------------------------------------------------------------
! Documentation for the general StateGetDataPointer<> macros.
!------------------------------------------------------------------------------
#endif

#define StateGetDataPointerDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_StateGetDataPointer - Retrieve Fortran pointer directly from a State @\
! @\
! !INTERFACE: @\
!   ! Private name; call using ESMF_StateGetDataPointer() @\
!   subroutine ESMF_StateGetDataPointer<rank><type><kind>(state, itemName, @\
!                                 dataPointer, copyflag, nestedStateName, rc) @\
! @\
! !ARGUMENTS: @\
!      type(ESMF_State), intent(in) :: state @\
!      character(len=*), intent(in) :: itemName @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: dataPointer @\
!      type(ESMF_CopyFlag), intent(in), optional :: copyflag @\
!      character(len=*), intent(in), optional :: nestedStateName @\
!      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Retrieves data from a state, returning a direct Fortran pointer to @\
! the data array.  @\
! Valid type/kind/rank combinations supported by the @\
! framework are: ranks 1 to 7, type real of kind *4 or *8, @\
! and type integer of kind *1, *2, *4, or *8. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[state] @\
!   The {\tt ESMF\_State} to query. @\
!  \item[itemName] @\
!   The name of the Bundle, Field, or Array to return data from. @\
!  \item[dataPointer] @\
!   An unassociated Fortran pointer of the proper Type, Kind, and Rank as the data @\
!   in the State.  When this call returns successfully, the pointer will now reference @\
!   the data in the State.  This is either a reference or a copy, depending on the @\
!   setting of the following argument.  The default is to return a reference. @\
!  \item[{[copyflag]}] @\
!   Defaults to {\tt ESMF\_DATA\_REF}.  If set to {\tt ESMF\_DATA\_COPY}, a separate @\
!   copy of the data will be made and the pointer will point at the copy. @\
!  \item[{[nestedStateName]}] @\
!   Optional.  If multiple states are present, a specific state name must be given. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
!EOP @\
 @\

#if 0
!------------------------------------------------------------------------------
! Get an F90 pointer back directly to the data.
!------------------------------------------------------------------------------
#endif

#define StateGetDataPointerMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
^undef  ESMF_METHOD @\
^define ESMF_METHOD "ESMF_StateGetDataPointer" @\
      subroutine ESMF_StateGetDataPointer##mrank##D##mtypekind(state, & @\
                        itemName, dataPointer, copyflag, nestedStateName, rc) @\
 @\
      type(ESMF_State), intent(in) :: state @\
      character(len=*), intent(in) :: itemName @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: dataPointer @\
      type(ESMF_CopyFlag), intent(in), optional :: copyflag @\
      character(len=*), intent(in), optional :: nestedStateName @\
      integer, intent(out), optional :: rc   @\
 @\
        ! Local variables @\
        !!type (ESMF_Bundle) :: bundle        ! bundle object @\
        type (ESMF_Field) :: field          ! field object @\
        type(ESMF_InternArray) :: array          ! array object @\
        integer :: status                   ! local error status @\
        logical :: rcpresent                ! did user specify rc? @\
 @\
        ! Initialize return code; assume failure until success is certain @\
        status = ESMF_RC_NOT_IMPL @\
        rcpresent = .FALSE. @\
        array%this = ESMF_NULL_POINTER @\
 @\
        if (present(rc)) then @\
          rcpresent = .TRUE. @\
          rc = ESMF_RC_NOT_IMPL @\
        endif @\
 @\
        ! check input variables @\
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc) @\
 @\
 @\
        ! Test to see if array already associated, and fail if so. @\
        if (associated(dataPointer)) then @\
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, & @\
                              "Data Pointer cannot already be associated", & @\
                               ESMF_CONTEXT, rc)) return @\
        endif @\
 @\
        ! TODO: make this check the data type, and switch based on that. @\
        ! For now, assume field only. @\
        call ESMF_StateGetField(state, itemName, field, nestedStateName, status) @\
        if (ESMF_LogMsgFoundError(status, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
 @\
        call ESMF_FieldGetInternArray(field, array, rc=status) @\
        if (ESMF_LogMsgFoundError(status, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
 @\
        call ESMF_InternArrayGetData(array, dataPointer, copyflag, rc=status) @\
        if (ESMF_LogMsgFoundError(status, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
 @\
        if (rcpresent) rc = status @\
 @\
        end subroutine ESMF_StateGetDataPointer##mrank##D##mtypekind   @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\










