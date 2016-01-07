! $Id$
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
#define ESMF_FILENAME "ESMF_ArraySpec_C.F90"
!==============================================================================
!
! F77 interface files for C layer calling into F90 implementation layer.
!  This cannot use any F90 syntax, including modules, or allocatable
!   arrays, or ...
!
!==============================================================================
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

!------------------------------------------------------------------------------
!BOP
!  !DESCRIPTION:
!
! The code in this file implements the interface code between C and F90
!  for the {\tt ArraySpec} entry points.  When the user calls an
!  ESMC_ArraySpecXXX method, that code calls these functions, which
!  in turn call the F90 module code.  C cannot call directly into an
!  F90 module because the module routine names are altered in a similar
!  fashion as C++ name mangling.
!
!EOP
!------------------------------------------------------------------------------

  subroutine f_esmf_arrayspecset(arrayspec, rank, typekind, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_arrayspecset()"
    use ESMF_ArraySpecMod
    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    
    implicit none

    type(ESMF_ArraySpec) :: arrayspec
    integer :: rank
    type(ESMF_TypeKind_Flag) :: typekind
    integer :: rc

    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL

    call ESMF_ArraySpecSet(arrayspec=arrayspec, rank=rank, typekind=typekind, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return
      
    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine f_esmf_arrayspecset

!---------------------------------------------------------------

  subroutine f_esmf_arrayspecget(arrayspec, rank, typekind, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_arrayspecget()"
    use ESMF_ArraySpecMod
    use ESMF_UtilTypesMod
    use ESMF_LogErrMod

    implicit none

    type(ESMF_ArraySpec) :: arrayspec
    integer :: rank
    type(ESMF_TypeKind_Flag) :: typekind
    integer :: rc

    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL

    call ESMF_ArraySpecGet(arrayspec=arrayspec, rank=rank, typekind=typekind, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return

    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine f_esmf_arrayspecget

!---------------------------------------------------------------

  subroutine f_esmf_arrayspecgetrank(arrayspec, rank, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_arrayspecgetrank()"
    use ESMF_ArraySpecMod
    use ESMF_UtilTypesMod
    use ESMF_LogErrMod

    implicit none

    type(ESMF_ArraySpec) :: arrayspec
    integer :: rank
    integer :: rc

    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL

    call ESMF_ArraySpecGet(arrayspec=arrayspec, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return

    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine f_esmf_arrayspecgetrank

!---------------------------------------------------------------

  subroutine f_esmf_arrayspecgettypekind(arrayspec, typekind, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_arrayspecgettypekind()"
    use ESMF_ArraySpecMod
    use ESMF_UtilTypesMod
    use ESMF_LogErrMod

    implicit none
    
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_TypeKind_Flag) :: typekind
    integer :: rc

    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL

    call ESMF_ArraySpecGet(arrayspec=arrayspec, typekind=typekind, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return

    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine f_esmf_arrayspecgettypekind
