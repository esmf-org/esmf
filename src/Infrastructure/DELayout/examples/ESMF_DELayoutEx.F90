! $Id: ESMF_DELayoutEx.F90,v 1.1 2004/06/18 21:27:24 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================

!==============================================================================
!EXAMPLE        String used by test script to count examples.
!==============================================================================

program ESMF_DELayoutEx

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: rc
  type(ESMF_VM):: vm
  integer:: localPet, petCount, peCount, ssiId
  type(ESMF_DELayout):: delayout
  ! result code
  integer :: finalrc
  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOE
! \subsubsection{Create default 1-D DELayout}
! 
! The default DELayout holds as many DEs as there are PETs in the 
! associated VM object.
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(vm, rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC  
  call ESMF_DELayoutPrint(delayout, rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC  
  call ESMF_DELayoutDestroy(delayout, rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC  
!EOC  
  call ESMF_Finalize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_DELayoutEx.F90"
  else
    print *, "FAIL: ESMF_DELayoutEx.F90"
  endif
end program
