! $Id: ESMF_VMDefaultBasicsEx.F90,v 1.1 2004/06/17 16:20:03 theurich Exp $
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

!------------------------------------------------------------------------------
!BOE
!
! \subsubsection{VM Default Basics Example}
!
! This complete example program demonstrates the simplest ESMF application, 
! consisting of only a main program without any components. The global default
! VM, which is automatically created during the {\tt ESMF\_Initialize()} call,
! is obtained and then used in its print method and several VM query calls.
!
!EOE
!------------------------------------------------------------------------------

!BOC
program ESMF_VMDefaultBasicsEx

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: rc
  type(ESMF_VM):: vm
  integer:: localPet, petCount, peCount, ssiId
!EOC
  ! result code
  integer :: finalrc
  finalrc = ESMF_SUCCESS
!BOC
  call ESMF_Initialize(vm=vm, rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
  call ESMF_VMPrint(vm, rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, peCount=peCount, &
    rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
  print *, 'localPet is: ', localPet,' out of a total of ',petCount,' PETs.'
  print *, 'there are ', peCount,' PEs referenced by this VM'

  call ESMF_VMGetPET(vm, localPet, peCount=peCount, ssiId=ssiId, rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
  print *, 'localPet is: ', localPet,' and it is claiming ', peCount, &
    ' PEs on SSI ', ssiId

  call ESMF_Finalize(rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_VMDefaultBasicsEx.F90"
  else
    print *, "FAIL: ESMF_VMDefaultBasicsEx.F90"
  endif
!BOC
end program
!EOC
