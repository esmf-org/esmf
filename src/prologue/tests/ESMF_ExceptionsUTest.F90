! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

program ExceptionsUTest
    
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  implicit none

  integer :: rc, result
  character(len=ESMF_MAXSTR) :: failMsg, name

  result = 0

  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------

  ! NEX_UTest
  call esmc_bare_throw_test(result)

  ! NEX_UTest
  call esmc_deep_throw_test(result)

  ! NEX_UTest
  call esmc_class_no_destructor_throw_test(result)

  ! NEX_UTest
  call esmc_class_with_destructor_throw_test(result)

  call ESMF_TestEnd(ESMF_SRCLINE)

end program ExceptionsUTest
