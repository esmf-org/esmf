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
#define ESMF_FILENAME "ESMF_UtilCubedSphere.F90"

module ESMF_UtilPredefinedDynamicMaskMod

  ! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  implicit none
  private

!------------------------------------------------------------------------------
! ! ESMF_CubedSphereTransform_Args
!
!------------------------------------------------------------------------------
  type ESMF_PredefinedDynamicMask
    real(ESMF_KIND_R8) :: srcMaskValue
    real(ESMF_KIND_R8) :: dstMaskValue
    type(ESMF_PredefinedDynamicMask_Flag) :: maskType
  end type


  public :: ESMF_PredefinedDynamicMask 

  contains

  subroutine simpleDynMaskProcV(dynamicMaskList, dynamicSrcMaskValue, dynamicDstMaskValue, rc)
    type(ESMF_DynamicMaskElementR4R8R4V), pointer              :: dynamicMaskList(:)
    real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
    real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
    integer,                       intent(out)          :: rc
    integer :: i, j, k, n
    real(ESMF_KIND_R4), allocatable  :: renorm(:)

    if (associated(dynamicMaskList)) then
      n = size(dynamicMaskList(1)%srcElement(1)%ptr)
      allocate(renorm(n))

      do i=1, size(dynamicMaskList)
        dynamicMaskList(i)%dstElement = 0.0 ! set to zero

        renorm = 0.d0 ! reset
        do j=1, size(dynamicMaskList(i)%factor)
          do k = 1, size(dynamicMaskList(i)%srcElement(j)%ptr)
            if (.not. &
              match(dynamicSrcMaskValue,dynamicMaskList(i)%srcElement(j)%ptr(k))) then
              dynamicMaskList(i)%dstElement(k) = dynamicMaskList(i)%dstElement(k) &
              + dynamicMaskList(i)%factor(j) * dynamicMaskList(i)%srcElement(j)%ptr(k)
                     renorm(k) = renorm(k) + dynamicMaskList(i)%factor(j)
            endif
          end do
        end do
        where (renorm > 0.d0)
          dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement / renorm
        elsewhere
            dynamicMaskList(i)%dstElement = dynamicSrcMaskValue
        end where
      enddo
    endif
    rc = ESMF_SUCCESS
  end subroutine simpleDynMaskProcV

  logical function match(missing,b)
    real(ESMF_KIND_R4), intent(in) :: missing, b
    match = (missing==b)
  end function match

end module ESMF_UtilPredefinedDynamicMaskMod

