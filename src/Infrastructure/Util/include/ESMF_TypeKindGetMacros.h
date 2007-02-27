#if 0
! $Id: ESMF_TypeKindGetMacros.h,v 1.2 2007/02/27 22:35:45 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
#endif

#define ESMF_TypeKindGetMacro(mname, mtypekind) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!------------------------------------------------------------------------------ @\
@\
^undef  ESMF_METHOD @\
!define ESMF_METHOD "ESMF_TypeKindGet##mtypekind" @\
^define ESMF_METHOD "ESMF_TypeKindGet" @\
 type(ESMF_TypeKind) function ESMF_TypeKindGet##mtypekind(var, rc) @\
@\
    mname(ESMF_KIND_##mtypekind), intent(in) :: var @\
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


