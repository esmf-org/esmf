! $Id: ESMF_VMDefaultBasicsEx.F90,v 1.9.2.1 2010/02/05 20:01:28 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!------------------------------------------------------------------------------
!BOE
!
! \subsubsection{Global VM}
!
! This complete example program demonstrates the simplest ESMF application, 
! consisting of only a main program without any Components. The global
! VM, which is automatically created during the {\tt ESMF\_Initialize()} call,
! is obtained using two different methods. First the global VM will be returned
! by {\tt ESMF\_Initialize()} if the optional {\tt vm} argument is specified.
! The example uses the VM object obtained this way to call the VM print method.
! Second, the global VM can be obtained anywhere in the user application using
! the {\tt ESMF\_VMGetGlobal()} call. The identical VM is returned and several
! VM query methods are called to inquire about the associated resources.
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
  integer:: localPet, petCount, peCount, ssiId, vas
!EOC
  ! result code
  integer :: finalrc
  finalrc = ESMF_SUCCESS
!BOC
  call ESMF_Initialize(vm=vm, rc=rc)
  ! Providing the optional vm argument to ESMF_Initialize() is one way of
  ! obtaining the global VM.
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
  call ESMF_VMPrint(vm, rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
  call ESMF_VMGetGlobal(vm=vm, rc=rc)
  ! Calling ESMF_VMGetGlobal() anywhere in the user application is the other
  ! way to obtain the global VM object.
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, peCount=peCount, &
    rc=rc)
  ! The VM object contains information about the associated resources. If the
  ! user code requires this information it must query the VM object.
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
  print *, "This PET is localPet: ", localPet
  print *, "of a total of ",petCount," PETs in this VM."
  print *, "There are ", peCount," PEs referenced by this VM"

  call ESMF_VMGetPETLocalInfo(vm, localPet, peCount=peCount, ssiId=ssiId, &
    vas=vas, rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
  print *, "This PET is executing in virtual address space (VAS) ", vas
  print *, "located on single system image (SSI) ", ssiId
  print *, "and is associated with ", peCount, " PEs."

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
