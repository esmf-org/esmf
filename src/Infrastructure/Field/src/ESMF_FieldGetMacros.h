#if 0
! $Id: ESMF_FieldGetMacros.h,v 1.16 2007/12/27 20:46:37 feiliu Exp $
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
!BOPI @\
! !IROUTINE: ESMF_FieldGetDataPtr - get the data pointer from a Field @\
! @\
! !INTERFACE: @\
!   ! Private name; call using ESMF_FieldGetDataPtr() @\
!   subroutine ESMF_FieldGetDataPtr<rank><type><kind>(field, farray, rc) @\
! @\
! !ARGUMENTS: @\
!      type(ESMF_Field), intent(in) :: field                  @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: farray @\
!      integer, intent(out), optional :: rc                @\
! @\
! !DESCRIPTION: @\
!     An interface subroutine to {\tt ESMF\_FieldGetDataPtr()}. @\
!     Get the data pointer from a {\tt ESMF\_Field}. @\
! @\
!     The arguments are: @\
!     \begin{description} @\
!     \item [field]  @\
!           Pointer to an {\tt ESMF\_Field} object.  @\
!     \item [farray] @\
!           Native fortran data pointer to be copied/referenced in Field @\
!     \item [{[rc]}]  @\
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!     \end{description} @\
! @\
!EOPI @\

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
    subroutine ESMF_FieldGetDataPtr##mrank##D##mtypekind(field, farray, rc) @\
 @\
! input arguments @\
      type(ESMF_Field), intent(in) :: field                  @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: farray @\
      integer, intent(out), optional :: rc                @\
 @\
! local variables @\
      integer          :: localrc @\
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
      if (present(rc)) rc = localrc @\
    end subroutine ESMF_FieldGetDataPtr##mrank##D##mtypekind  @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

