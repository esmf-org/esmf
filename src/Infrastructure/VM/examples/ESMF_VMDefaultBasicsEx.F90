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
!EOC
#include "ESMF.h"
!BOC
  use ESMF
!EOC
  use ESMF_TestMod
!BOC
  implicit none

  ! local variables
  integer:: rc
  type(ESMF_VM):: vm
  integer:: localPet, petCount, peCount, ssiId, vas
!EOC
  ! result code
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_VMDefaultBasicsEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


  finalrc = ESMF_SUCCESS
!BOE
! Providing the optional vm argument to {\tt ESMF\_Initialize()} is one way of
! obtaining the global VM.
!EOE
!BOC
  call ESMF_Initialize(vm=vm, defaultlogfilename="VMDefaultBasicsEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_VMPrint(vm, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Calling {\tt ESMF\_VMGetGlobal()} anywhere in the user application is the
! other way to obtain the global VM object.
!EOE
!BOC
  call ESMF_VMGetGlobal(vm=vm, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, peCount=peCount, &
    rc=rc)
!EOC
!BOE
! The VM object contains information about the associated resources. If the
! user code requires this information it must query the VM object.
!EOE
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  print *, "This PET is localPet: ", localPet
  print *, "of a total of ",petCount," PETs in this VM."
  print *, "There are ", peCount," PEs referenced by this VM"

  call ESMF_VMGet(vm, localPet, peCount=peCount, ssiId=ssiId, vas=vas, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  print *, "This PET is executing in virtual address space (VAS) ", vas
  print *, "located on single system image (SSI) ", ssiId
  print *, "and is associated with ", peCount, " PEs."
!EOC
  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)
!BOC
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
