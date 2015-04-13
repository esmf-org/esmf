! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2015, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

integer function ESMF_Optional_arg_pos (i, j) result (ret)
  implicit none

  integer, intent(in), optional :: i, j

  ret = 0
  if (present (i)) ret = 1
  if (present (j)) ret = ret + 2

end function

integer function ESMF_Optional_arg_pos_a1d (a1, a2) result (ret)
  implicit none

! Arrays are passed by assumed size, not shape
  integer, intent(in), optional :: a1(*), a2(*)

  ret = 0
  if (present (a1)) ret = 1
  if (present (a2)) ret = ret + 2

end function

integer function ESMF_Optional_arg_pos_a2d (a1, a1_size, a2, a2_size) result (ret)
  implicit none

! Arrays are passed by assumed size, not shape
  integer, intent(in) :: a1_size, a2_size  ! leading dimensions
  integer, intent(in), optional :: a1(a1_size,*), a2(a2_size,*)

  ret = 0
  if (present (a1)) ret = 1
  if (present (a2)) ret = ret + 2

end function

