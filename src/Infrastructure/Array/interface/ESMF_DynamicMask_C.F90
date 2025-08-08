! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_dynamicmask_C.F90"
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
!  for the {\tt dynamicmask} entry points.  When the user calls an
!  ESMC_dynamicmaskXXX method, that code calls these functions, which
!  in turn call the F90 module code.  C cannot call directly into an
!  F90 module because the module routine names are altered in a similar
!  fashion as C++ name mangling.
!
!EOP
!------------------------------------------------------------------------------

  subroutine f_esmf_dynamicmaskpredefinedsetr8r8r8(dynamicmask, maskType, &
      handleAllElements, haePresent, dynamicSrcMaskValue, dsmPresent, &
      dynamicDstMaskValue, ddmPresent, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_dynamicmaskpredefinedsetr8r8r8()"
    use ESMF_DynamicMaskMod
    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    
    implicit none

    type(ESMF_DynamicMask) :: dynamicmask
    type(ESMF_PredefinedDynamicMask_Flag) :: maskType
    integer  :: handleAllElements
    integer  :: haePresent
    real(ESMF_KIND_R8) :: dynamicSrcMaskValue
    integer  :: dsmPresent
    real(ESMF_KIND_R8) :: dynamicDstMaskValue
    integer  :: ddmPresent
    integer :: rc

    integer                 :: localrc      ! local return code
    real(ESMF_KIND_R8), allocatable :: dynamicSrcMaskValue_, dynamicDstMaskValue_
    logical, allocatable :: handleAllElements_
    

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL

    if (haePresent==1) then
        if (handleAllElements==0) then
           allocate(handleAllElements_,source=.false.)
        else if (handleAllElements==1) then
           allocate(handleAllElements_,source=.true.)
        end if
    end if
    if (dsmPresent==1) then
       allocate(dynamicSrcMaskValue_,source= dynamicSrcMaskValue)
    end if
    if (ddmPresent==1) allocate(dynamicDstMaskValue_,source= dynamicdstMaskValue)

     call ESMF_DynamicMaskPredefinedSetR8R8R8(dynamicmask=dynamicmask, maskType=maskType, &
       handleAllElements=handleAllElements_, dynamicSrcMaskValue=dynamicSrcMaskValue_, &
       dynamicDstMaskValue=dynamicDstMaskValue_, rc=localrc)

    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return
      
    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine f_esmf_dynamicmaskpredefinedsetr8r8r8

  subroutine f_esmf_dynamicmaskpredefinedsetr8r8r8v(dynamicmask, maskType, &
      handleAllElements, haePresent, dynamicSrcMaskValue, dsmPresent, &
      dynamicDstMaskValue, ddmPresent, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_dynamicmaskpredefinedsetr8r8r8v()"
    use ESMF_DynamicMaskMod
    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    
    implicit none

    type(ESMF_DynamicMask) :: dynamicmask
    type(ESMF_PredefinedDynamicMask_Flag) :: maskType
    integer  :: handleAllElements
    integer  :: haePresent
    real(ESMF_KIND_R8) :: dynamicSrcMaskValue
    integer  :: dsmPresent
    real(ESMF_KIND_R8) :: dynamicDstMaskValue
    integer  :: ddmPresent
    integer :: rc

    integer                 :: localrc      ! local return code
    real(ESMF_KIND_R8), allocatable :: dynamicSrcMaskValue_, dynamicDstMaskValue_
    logical, allocatable :: handleAllElements_
    

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL

    if (haePresent==1) then
        if (handleAllElements==0) then
           allocate(handleAllElements_,source=.false.)
        else if (handleAllElements==1) then
           allocate(handleAllElements_,source=.true.)
        end if
    end if
    if (dsmPresent==1) then
       allocate(dynamicSrcMaskValue_,source= dynamicSrcMaskValue)
    end if
    if (ddmPresent==1) allocate(dynamicDstMaskValue_,source= dynamicdstMaskValue)

     call ESMF_DynamicMaskPredefinedSetR8R8R8V(dynamicmask=dynamicmask, maskType=maskType, &
       handleAllElements=handleAllElements_, dynamicSrcMaskValue=dynamicSrcMaskValue_, &
       dynamicDstMaskValue=dynamicDstMaskValue_, rc=localrc)

    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return
      
    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine f_esmf_dynamicmaskpredefinedsetr8r8r8v

  subroutine f_esmf_dynamicmaskpredefinedsetr4r8r4(dynamicmask, maskType, &
      handleAllElements, haePresent, dynamicSrcMaskValue, dsmPresent, &
      dynamicDstMaskValue, ddmPresent, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_dynamicmaskpredefinedsetr4r8r4()"
    use ESMF_DynamicMaskMod
    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    
    implicit none

    type(ESMF_DynamicMask) :: dynamicmask
    type(ESMF_PredefinedDynamicMask_Flag) :: maskType
    integer  :: handleAllElements
    integer  :: haePresent
    real(ESMF_KIND_R4) :: dynamicSrcMaskValue
    integer  :: dsmPresent
    real(ESMF_KIND_R4) :: dynamicDstMaskValue
    integer  :: ddmPresent
    integer :: rc

    integer                 :: localrc      ! local return code
    real(ESMF_KIND_R4), allocatable :: dynamicSrcMaskValue_, dynamicDstMaskValue_
    logical, allocatable :: handleAllElements_
    

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL

    if (haePresent==1) then
        if (handleAllElements==0) then
           allocate(handleAllElements_,source=.false.)
        else if (handleAllElements==1) then
           allocate(handleAllElements_,source=.true.)
        end if
    end if
    if (dsmPresent==1) then
       allocate(dynamicSrcMaskValue_,source= dynamicSrcMaskValue)
    end if
    if (ddmPresent==1) allocate(dynamicDstMaskValue_,source= dynamicdstMaskValue)

     call ESMF_DynamicMaskPredefinedSetR4R8R4(dynamicmask=dynamicmask, maskType=maskType, &
       handleAllElements=handleAllElements_, dynamicSrcMaskValue=dynamicSrcMaskValue_, &
       dynamicDstMaskValue=dynamicDstMaskValue_, rc=localrc)

    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return
      
    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine f_esmf_dynamicmaskpredefinedsetr4r8r4

  subroutine f_esmf_dynamicmaskpredefinedsetr4r8r4v(dynamicmask, maskType, &
      handleAllElements, haePresent, dynamicSrcMaskValue, dsmPresent, &
      dynamicDstMaskValue, ddmPresent, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_dynamicmaskpredefinedsetr4r8r4v()"
    use ESMF_DynamicMaskMod
    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    
    implicit none

    type(ESMF_DynamicMask) :: dynamicmask
    type(ESMF_PredefinedDynamicMask_Flag) :: maskType
    integer  :: handleAllElements
    integer  :: haePresent
    real(ESMF_KIND_R4) :: dynamicSrcMaskValue
    integer  :: dsmPresent
    real(ESMF_KIND_R4) :: dynamicDstMaskValue
    integer  :: ddmPresent
    integer :: rc

    integer                 :: localrc      ! local return code
    real(ESMF_KIND_R4), allocatable :: dynamicSrcMaskValue_, dynamicDstMaskValue_
    logical, allocatable :: handleAllElements_
    

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL

    if (haePresent==1) then
        if (handleAllElements==0) then
           allocate(handleAllElements_,source=.false.)
        else if (handleAllElements==1) then
           allocate(handleAllElements_,source=.true.)
        end if
    end if
    if (dsmPresent==1) then
       allocate(dynamicSrcMaskValue_,source= dynamicSrcMaskValue)
    end if
    if (ddmPresent==1) allocate(dynamicDstMaskValue_,source= dynamicdstMaskValue)

     call ESMF_DynamicMaskPredefinedSetR4R8R4V(dynamicmask=dynamicmask, maskType=maskType, &
       handleAllElements=handleAllElements_, dynamicSrcMaskValue=dynamicSrcMaskValue_, &
       dynamicDstMaskValue=dynamicDstMaskValue_, rc=localrc)

    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return
      
    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine f_esmf_dynamicmaskpredefinedsetr4r8r4v

!---------------------------------------------------------------

