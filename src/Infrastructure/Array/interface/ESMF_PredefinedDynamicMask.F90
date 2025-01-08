! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
#include "ESMF.h"

module ESMF_PredefinedDynamicMaskMod

  ! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_DynamicMaskMod
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_LogErrMod
  use ESMF_srcDynamicMaskMod
  implicit none
  private

!------------------------------------------------------------------------------
! ! ESMF_CubedSphereTransform_Args
!
!------------------------------------------------------------------------------
  type ESMF_PredefinedDynamicMask
    real(ESMF_KIND_R4) :: srcMaskValue_R4
    real(ESMF_KIND_R4) :: dstMaskValue_R4
    real(ESMF_KIND_R8) :: srcMaskValue_R8
    real(ESMF_KIND_R8) :: dstMaskValue_R8
    type(ESMF_PredefinedDynamicMask_Flag) :: maskType
    contains 
       procedure :: create_DynamicMask
  end type


  public :: ESMF_PredefinedDynamicMask 
  public :: ESMF_PredefinedDynamicMaskSet

  contains

  subroutine ESMF_PredefinedDynamicMaskSet(preDefinedDynamicMask,srcMaskValue,dstMaskValue,maskType,rc)
    type(ESMF_PredefinedDynamicMask), intent(out) :: preDefinedDynamicMask
    real(ESMF_KIND_R4), intent(in), optional :: srcMaskValue
    real(ESMF_KIND_R4), intent(in), optional :: dstMaskValue
    type(ESMF_PredefinedDynamicMask_Flag), intent(in), optional :: maskType
    integer, optional, intent(out) :: rc

    if (present(srcMaskValue)) preDefinedDynamicMask%srcMaskValue_R4=srcMaskValue
    if (present(dstMaskValue)) preDefinedDynamicMask%dstMaskValue_R4=dstMaskValue
    if (present(srcMaskValue)) preDefinedDynamicMask%srcMaskValue_R8=srcMaskValue
    if (present(dstMaskValue)) preDefinedDynamicMask%dstMaskValue_R8=dstMaskValue
    if (present(maskType)) preDefinedDynamicMask%maskType=maskType

    if (present(rc)) rc =ESMF_SUCCESS
  end subroutine ESMF_PredefinedDynamicMaskSet 

  function create_DynamicMask(this, src_type, dst_type, rc) result(dynamicMask)
     type(ESMF_DynamicMask) :: dynamicMask
     class(ESMF_PredefinedDynamicMask), intent(in) :: this
     type(ESMF_TypeKind_Flag), intent(in) :: src_type
     type(ESMF_TypeKind_Flag), intent(in) :: dst_type
     integer, optional, intent(out) :: rc
     integer :: localrc
     type(ESMF_TypeKind_Flag) :: mask_type
     
     print*,'bmaa ',__FILE__,__LINE__
     mask_type = get_mask_type(src_type, dst_type, rc) 
     if (this%maskType == ESMF_PREDEFINEDDYNAMICMASK_MASKSRC) then
        if (mask_type == ESMF_TYPEKIND_R4) then 
           print*,'bmaa ',__FILE__,__LINE__
           call ESMF_DynamicMaskSetR4R8R4V(dynamicMask, srcDynMaskProcR4R8R4V ,dynamicSrcMaskValue=this%srcMaskValue_R4)
        else if (mask_type == ESMF_TYPEKIND_R8) then
           print*,'bmaa ',__FILE__,__LINE__
           call ESMF_DynamicMaskSetR8R8R8V(dynamicMask, srcDynMaskProcR8R8R8V ,dynamicSrcMaskValue=this%srcMaskValue_R8)
        end if
     else
        localrc = ESMF_RC_NOT_IMPL
        if (ESMF_LogFoundError(rcTocheck=localrc, &
           ESMF_ERR_PASSTHRU, &
           line=__LINE__, file=__FILE__, rcToReturn=rc)) return
     end if
     if (present(rc)) rc=ESMF_SUCCESS
  end function create_DynamicMask

  function get_mask_type(src_type, dst_type, rc) result(mask_type)
     type(ESMF_TypeKind_Flag) :: mask_type
     type(ESMF_TypeKind_Flag), intent(in) :: src_type
     type(ESMF_TypeKind_Flag), intent(in) :: dst_type
     integer, optional, intent(out)       :: rc

     integer :: localrc
     
     if ((src_type .eq. ESMF_NOKIND) .and. (dst_type .eq. ESMF_NOKIND)) then
        mask_type = ESMF_NOKIND
     end if
     if ((src_type .eq. ESMF_TYPEKIND_R4) .and. (dst_type .eq. ESMF_NOKIND)) then
        mask_type = ESMF_TYPEKIND_R4
     end if
     if ((src_type .eq. ESMF_TYPEKIND_R8) .and. (dst_type .eq. ESMF_NOKIND)) then
        mask_type = ESMF_TYPEKIND_R8
     end if
     if ((dst_type .eq. ESMF_TYPEKIND_R4) .and. (src_type .eq. ESMF_NOKIND)) then
        mask_type = ESMF_TYPEKIND_R4
     end if
     if ((dst_type .eq. ESMF_TYPEKIND_R8) .and. (src_type .eq. ESMF_NOKIND)) then
        mask_type = ESMF_TYPEKIND_R8
     end if
     if ((src_type .eq. ESMF_TYPEKIND_R8) .and. (dst_type .eq. ESMF_TYPEKIND_R8)) then
        mask_type = ESMF_TYPEKIND_R8
     end if
     if ((src_type .eq. ESMF_TYPEKIND_R4) .and. (dst_type .eq. ESMF_TYPEKIND_R4)) then
        mask_type = ESMF_TYPEKIND_R4
     end if
     if (present(rc)) rc=ESMF_SUCCESS

  end function get_mask_type

end module ESMF_PredefinedDynamicMaskMod

