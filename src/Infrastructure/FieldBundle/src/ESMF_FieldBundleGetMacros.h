#if 0
! $Id: ESMF_FieldBundleGetMacros.h,v 1.1.2.5 2009/01/21 21:25:21 cdeluca Exp $
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
! Macros for the FieldBundle class.
!------------------------------------------------------------------------------
#endif

#if 0
!------------------------------------------------------------------------------
! Documentation for the general FieldBundleGetDataPointer macro
!------------------------------------------------------------------------------
#endif

#define FieldBundleGetDataPointerDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOPI @\
! !IROUTINE: ESMF_FieldBundleGetDataPointer - Retrieve Fortran pointer directly from a FieldBundle @\
! @\
! !INTERFACE: @\
!     ! Private name; call using ESMF_FieldBundleGetDataPointer() @\
!      subroutine ESMF_FieldBundleGetDataPointer<rank><type><kind>(bundle, & @\
!                                 fieldName, dataPointer, copyflag, rc) @\
! @\
! !ARGUMENTS: @\
!      type(ESMF_FieldBundle), intent(in) :: bundle @\
!      character(len=*), intent(in) :: fieldName @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: dataPointer @\
!      type(ESMF_CopyFlag), intent(in), optional :: copyflag @\
!      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Retrieves data from the {\tt bundle}, returning a direct Fortran pointer to @\
!   the data.  @\
! Valid type/kind/rank combinations supported by the @\
! framework are: ranks 1 to 7, type real of kind *4 or *8, @\
! and type integer of kind *1, *2, *4, or *8. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[bundle] @\
!   The {\tt ESMF\_FieldBundle} to query. @\
!  \item[fieldName] @\
!   The name of the {\tt ESMF\_Field} inside the {\tt bundle} @\
!   to return.  The {\tt bundle} cannot have packed data. @\
!  \item[dataPointer] @\
!   An unassociated Fortran pointer of the proper Type, Kind, and Rank as the data @\
!   in the FieldBundle.  When this call returns successfully, the pointer will now point to @\
!   the data in the FieldBundle.  This is either a reference or a copy, depending on the @\
!   setting of the following argument.  The default is to return a reference. @\
!  \item[{[copyflag]}] @\
!   Defaults to {\tt ESMF\_DATA\_REF}.  If set to {\tt ESMF\_DATA\_COPY}, a separate @\
!   copy of the data will be made and the pointer will point at the copy. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
!EOPI @\
 @\

#if 0
!------------------------------------------------------------------------------
! Get a Fortran pointer back to FieldBundle Data macro
!------------------------------------------------------------------------------
#endif

#define FieldBundleGetDataPointerMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
^undef  ESMF_METHOD @\
^define ESMF_METHOD "ESMF_FieldBundleGetDataPointer" @\
      subroutine ESMF_FieldBundleGetDataPointer##mrank##D##mtypekind(bundle, & @\
                      fieldName, dataPointer, copyflag, rc) @\
 @\
      type(ESMF_FieldBundle), intent(inout) :: bundle @\
      character(len=*), intent(in) :: fieldName @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: dataPointer @\
      type(ESMF_CopyFlag), intent(in), optional :: copyflag @\
      integer, intent(out), optional :: rc   @\
 @\
        ! Local variables @\
        type (ESMF_Field) :: field          ! field object @\
        type(ESMF_InternArray) :: array          ! array object @\
        integer :: status                   ! local error status @\
        logical :: rcpresent                ! did user specify rc? @\
 @\
        ! Initialize return code; assume routine not implemented @\
        status = ESMF_RC_NOT_IMPL @\
        rcpresent = .FALSE. @\
        array%this = ESMF_NULL_POINTER @\
 @\
 @\
        ! check variables @\
        ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc) @\
 @\
        if (present(rc)) then @\
          rcpresent = .TRUE. @\
          rc = ESMF_RC_NOT_IMPL @\
        endif @\
 @\
        ! Test to see if array already allocated, and fail if so. @\
        if (associated(dataPointer)) then @\
           if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, & @\
                             "Data Pointer cannot already be associated", & @\
                              ESMF_CONTEXT, rc)) return @\
        endif @\
 @\
        call ESMF_FieldBundleGet(bundle, fieldName, field, status) @\
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
        end subroutine ESMF_FieldBundleGetDataPointer##mrank##D##mtypekind   @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

