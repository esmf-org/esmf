#if 0
! $Id: ESMF_TypeKindGetMacros.h,v 1.5 2007/03/31 05:51:27 cdeluca Exp $
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


#define ESMF_TypeKindGetDoc() \
!TODO: add interface documentation @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_TypeKindGet - Return the ESMF_TypeKind parameter corresponding to a scalar @\
! @\
! !INTERFACE: @\
!   ! Private name; call using ESMF_TypeKindGet() @\
!   function ESMF_TypeKindGet<typekind>(var, rc) @\
! @\
! !RETURN VALUE: @\
!      type(ESMF_TypeKind) :: ESMF_TypeKindGet<typekind> @\
! @\
! !ARGUMENTS: @\
!     <type>(ESMF_KIND_<typekind>), intent(in) :: var @\
!     integer, intent(out), optional :: rc @\
! @\
! !DESCRIPTION: @\
!   Return the ESMF_TypeKind parameter that corresponds to the scalar @\
!   (var) argument. Valid typekind supported by the framework are: @\
!   integers of 1-byte, 2-byte, 4-byte, and 8-byte size, and @\
!   reals of 4-byte and 8-bytes size.  @\
! @\
!   The arguments are: @\
!   \begin{description} @\
!   \item [var] @\
!      Scalar of any supported type and kind
!   \item [rc] @\
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!   \end{description} @\
! @\
!EOP @\

#if 0
!------------------------------------------------------------------------------
! Return the ESMF_TypeKind parameter that corresponds to a scalar
!------------------------------------------------------------------------------
#endif
 
#define ESMF_TypeKindGetMacro(mtypename, mtypekind) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!------------------------------------------------------------------------------ @\
@\
^undef  ESMF_METHOD @\
!define ESMF_METHOD "ESMF_TypeKindGet##mtypekind" @\
^define ESMF_METHOD "ESMF_TypeKindGet" @\
 type(ESMF_TypeKind) function ESMF_TypeKindGet##mtypekind(var, rc) @\
@\
    mtypename(ESMF_KIND_##mtypekind), intent(in) :: var @\
    integer, intent(out), optional :: rc @\
    type(ESMF_TypeKind) :: TypeKindGet##mtypekind @\
@\
    if (present(rc)) rc = ESMF_FAILURE @\
    ESMF_TypeKindGet##mtypekind = ESMF_TYPEKIND_##mtypekind @\
@\
    rc = ESMF_SUCCESS @\
@\
    end function ESMF_TypeKindGet##mtypekind @\
@\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @
\


