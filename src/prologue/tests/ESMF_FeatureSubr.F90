! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

module ESMF_FeatureSubr_mod
  use ESMF
  implicit none

contains

  real(ESMF_KIND_R8) function ESMF_FeatureSum1D (array1, array2) result (ret)
    real(ESMF_KIND_R8), intent(in), optional :: array1(:), array2(:)

    ret = 0.0
    if (present (array1)) ret = ret + sum (array1)
    if (present (array2)) ret = ret + sum (array2)

  end function

  real(ESMF_KIND_R8) function ESMF_FeatureSum2D (array1, array2) result (ret)
    real(ESMF_KIND_R8), intent(in), optional :: array1(:,:), array2(:,:)

    ret = 0.0
    if (present (array1)) ret = ret + sum (array1)
    if (present (array2)) ret = ret + sum (array2)

  end function

end module

function ESMF_Optional_arg_pos (i, j) result (ret)
  implicit none

  integer, intent(in), optional :: i, j

  integer :: ret

  ret = 0
  if (present (i)) ret = 1
  if (present (j)) ret = ret + 2

end function

function ESMF_Optional_arg_sum_a1d (a1, a1_size, a2, a2_size) result (ret)
  use ESMF
  use ESMF_FeatureSubr_mod
  implicit none

! Demonstrate passing adjustable size arrays
  integer, intent(in) :: a1_size, a2_size  ! leading dimensions
  real(ESMF_KIND_R8), intent(in), optional :: a1(a1_size), a2(a2_size)

  real(ESMF_KIND_R8) :: ret

  ret = ESMF_FeatureSum1D (a1, a2)

end function

function ESMF_Optional_arg_sum_a2d (a1, a1_size, a2, a2_size) result (ret)
  use ESMF
  use ESMF_FeatureSubr_mod
  implicit none

! Demonstrate passing assumed size arrays
  integer, intent(in) :: a1_size, a2_size  ! leading dimensions
  real(ESMF_KIND_R8), intent(in), optional :: a1(a1_size,*), a2(a2_size,*)

  real(ESMF_KIND_R8) :: ret

! Assume arrays have square shape
  ret = ESMF_FeatureSum2D (a1(:,:a1_size), a2(:,:a2_size))

end function

