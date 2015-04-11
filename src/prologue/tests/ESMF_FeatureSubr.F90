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

integer function ESMF_Optional_arg_pos (i, j)
  implicit none

  integer, intent(in), optional :: i, j
  integer :: ret

  ret = 0
  if (present (i)) ret = 1
  if (present (j)) ret = ret + 2

  ESMF_Optional_arg_pos = ret

end function

