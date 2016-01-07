!  $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
! F77 interface files for C++ layer calling into F90 implementation layer.
!  This cannot use any F90 syntax, including modules, or allocatable 
!   arrays, or ...
!
!==============================================================================
#define ESMF_FILENAME "ESMF_LocStream_C.F90"
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
!      character(*), parameter, private :: version = &
!      '$Id$'
!==============================================================================

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_locstreamcreate"
  subroutine f_esmf_locstreamcreate(locstream, ls_size, indexflag, coordSys, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_LocStreamMod

    implicit none

    ! arguments
    type(ESMF_LocStream)               :: locstream
    integer, intent(in)           :: ls_size
    type(ESMF_Index_Flag), optional :: indexflag
    type(ESMF_CoordSys_Flag), optional :: coordSys
    integer, intent(out)           :: rc              
  
    ! local variables  
  
    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    locstream=ESMF_LocStreamCreate(localCount=ls_size, &
                                   coordSys=coordSys, &
                                   indexflag=indexflag, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
                           ESMF_CONTEXT, rcToReturn=rc)) return

    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_locstreamcreate


#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_locstreamgetbounds"
  subroutine f_esmf_locstreamgetbounds(locstream, localDe, cLBound, cUBound, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_LocStreamMod

    implicit none

    type(ESMF_LocStream) :: locstream
    integer              :: localDe
    integer              :: cLBound
    integer              :: cUBound
    integer, intent(out) :: rc

  ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    call ESMF_LocStreamGetBounds(locstream, localDe=localDe, &
      computationalLBound=cLBound, computationalUBound=cUBound, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    rc = ESMF_SUCCESS

  end subroutine f_esmf_locstreamgetbounds


#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_locstreamaddkeyalloc"
  subroutine f_esmf_locstreamaddkeyalloc(locstream, keyName, typekind, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_LocStreamMod

    implicit none

    type(ESMF_LocStream)         :: locstream
    character(len=*),intent(in)  :: keyName
    type(ESMF_TypeKind_Flag),optional     :: typekind
    integer, intent(out)         :: rc

  ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    call ESMF_LocStreamAddKey(locstream, keyName=keyName, keyTypeKind=typekind, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    rc = ESMF_SUCCESS

  end subroutine f_esmf_locstreamaddkeyalloc


#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_locstreamgetkeyarray"
  subroutine f_esmf_locstreamgetkeyarray(locstream, keyName, array, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_ArraySpecMod
    use ESMF_ArrayMod
    use ESMF_LocStreamMod

    implicit none

    type(ESMF_LocStream)         :: locstream
    character(len=*),intent(in)  :: keyName
    type(ESMF_Array)             :: array
    integer, intent(out)         :: rc

    ! local
    type(ESMF_Array)               :: l_array

  ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    call ESMF_LocStreamGetKey(locstream, keyName=keyName, keyArray=l_array, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! because ESMF_Array.this is private, it cannot be accessed directly
    ! we use the public interface to do the ptr copy;
    ! the array object returned to the C interface must consist only of the
    ! this pointer. It must not contain the isInit member.
    call ESMF_ArrayCopyThis(l_array, array, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    rc = ESMF_SUCCESS

  end subroutine f_esmf_locstreamgetkeyarray

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_locstreamdestroy"
  subroutine f_esmf_locstreamdestroy(locstream, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_LocStreamMod

    implicit none

    type(ESMF_LocStream)               :: locstream
    integer, intent(out)           :: rc

  ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    call ESMF_LocStreamDestroy(locstream, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    rc = ESMF_SUCCESS

  end subroutine f_esmf_locstreamdestroy

