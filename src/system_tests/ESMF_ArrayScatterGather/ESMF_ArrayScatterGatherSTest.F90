! $Id: ESMF_ArrayScatterGatherSTest.F90,v 1.2.2.5 2008/04/18 23:12:53 svasquez Exp $
!
!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!-------------------------------------------------------------------------
!
! !DESCRIPTION:
! System test ArrayScatterGather.  
!    Two gridded components and one coupler component, two-way coupling
!
!    The first gridded component runs on 3 PETs (0,1,3) and defines two 2D 
!    Arrays: srcArray1 and dstArray1. Both Arrays have a total index space of
!    100x150 elements and use the default DELayout. However, srcArray1
!    decomposes into (petCount x 1) = (3 x 1) DEs while dstArray1 decomposes
!    into (1 x petCount) = (1 x 3) DEs.
!
!    During Initialize() srcArray1 is initialized via ESMF_ArrayScatter() from
!    an initialized Fortran array and then added to the coupler's importState.
!    The dstArray1 is left uninitialized and added to the coupler's exportState.
!
!    The second gridded component runs on 2 PETs (4,5) and defines two 2D 
!    Arrays: srcArray2 and dstArray2. Both Arrays have a total index space of
!    100x150 elements and use the default DELayout. However, srcArray2
!    decomposes into (petCount x 1) = (2 x 1) DEs while dstArray2 decomposes
!    into (1 x petCount) = (1 x 2) DEs. Both srcArray2 and dstArray2 are left
!    uninitialized and are added to the coupler's export state.
!
!    The coupler component runs on 6 PETs (0,1,2,3,4,5,6). Notice that PET 2
!    is neither part of gridded component 1 nor gridded component 2.
!
!    During the coupler's Initialize() the import and export states are 
!    reconciled. Further ESMF_ArrayGather() is used to gather srcArray1 data
!    in a Fortran array on coupler PET 5, which corresponds to a PET where
!    srcArray1 only exists as a proxy object. Finally the gathered data is
!    scattered via ESMF_ArrayScatter() into srcArray2.
!
!    The Run() method of the second gridded component uses ESMF_ArrayGather()
!    on srcArray2 in order to gather the data into a Fortran array on 
!    localPet 0 which corresponds to global PET 4. There a constant integer
!    value is added to all array elements before the data is scattered into
!    dstArray2.
!
!    The coupler during its Run() method gathers the dstArray2 data via 
!    ESMF_ArrayGather() on coupler PET 2 where dstArray2 is represented as
!    proxy object. The data is then scattered using ESMF_ArrayScatter() into
!    dstArray1 which also exists as proxy object on PET 2.
!
!    Finally the data of dstArray1 is gathered during the Run() call fo gridded
!    component 1 on PET 0 where it is compared to the original source data.
!
!-------------------------------------------------------------------------
!\begin{verbatim}

program ESMF_ArrayScatterGatherSTest
#define ESMF_METHOD "program ESMF_ArrayScatterGatherSTest"

#include "ESMF.h"

  ! ESMF Framework module
  use ESMF_Mod
  use ESMF_TestMod

  use user_model1, only : userm1_register
  use user_model2, only : userm2_register
  use user_coupler, only : usercpl_register

  implicit none
    
  ! Local variables
  integer :: localPet, petCount, localrc, rc=ESMF_SUCCESS
  character(len=ESMF_MAXSTR) :: cname1, cname2, cplname
  type(ESMF_VM):: vm
  type(ESMF_State) :: importState, exportState
  type(ESMF_GridComp) :: comp1, comp2
  type(ESMF_CplComp) :: cpl

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: testresult = 0

  ! individual test name
  character(ESMF_MAXSTR) :: testname

  ! individual test failure message, and final status msg
  character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "System Test failure"
  write(testname, *) "System Test ESMF_ArrayScatterGatherSTest"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  print *, "--------------------------------------- "
  print *, "Start of ", trim(testname)
  print *, "--------------------------------------- "

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
  ! Initialize framework and get back default global VM
  call ESMF_Initialize(vm=vm, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  ! Get number of PETs we are running with
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  ! Create the 2 model components and coupler
  cname1 = "user model 1"
  ! use petList to define comp1 on PET 0,1,3
  comp1 = ESMF_GridCompCreate(name=cname1, petList=(/0,1,3/), rc=localrc)
  print *, "Created component ", trim(cname1), "rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  cname2 = "user model 2"
  ! use petList to define comp2 on PET 4,5
  comp2 = ESMF_GridCompCreate(name=cname2, petList=(/4,5/), rc=localrc)
  print *, "Created component ", trim(cname2), "rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  cplname = "user one-way coupler"
  ! no petList means that coupler component runs on all PETs
  cpl = ESMF_CplCompCreate(name=cplname, rc=localrc)
  print *, "Created component ", trim(cplname), ", rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  print *, "Comp Creates finished"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompSetServices(comp1, userm1_register, localrc)
  print *, "Comp SetServices finished, rc= ", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_GridCompSetServices(comp2, userm2_register, localrc)
  print *, "Comp SetServices finished, rc= ", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_CplCompSetServices(cpl, usercpl_register, localrc)
  print *, "Comp SetServices finished, rc= ", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  ! the terms import and export are used from the coupler's perspecitve,
  ! model component 1 and 2 add Arrays to both import and export states
  importState = ESMF_StateCreate("import", ESMF_STATE_IMPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  exportState = ESMF_StateCreate("export", ESMF_STATE_EXPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  
  ! comp1's sense of import/export is reverse to coupler's
  call ESMF_GridCompInitialize(comp1, importState=exportState, &
    exportState=importState, rc=localrc)
  print *, "Comp 1 Initialize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
 
  ! comp2 uses the coupler's export state for import and export
  call ESMF_GridCompInitialize(comp2, importState=exportState, &
    exportState=exportState, rc=localrc)
  print *, "Comp 2 Initialize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
 
  ! coupler's import/export states are as defined
  call ESMF_CplCompInitialize(cpl, importState=importState, &
    exportState=exportState, rc=localrc)
  print *, "Coupler Initialize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompRun(comp2, importState=exportState, &
    exportState=exportState, rc=localrc)
  print *, "Comp 2 Run returned, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
 
  call ESMF_CplCompRun(cpl, importState=importState, &
    exportState=exportState, rc=localrc)
  print *, "Coupler Run returned, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_GridCompRun(comp1, importState=exportState, &
    exportState=importState, rc=localrc)
  print *, "Comp 1 Run returned, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_CplCompFinalize(cpl, importState=importState, &
    exportState=exportState, rc=localrc)
  print *, "Coupler Finalize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_GridCompFinalize(comp1, importState=exportState, &
    exportState=importState, rc=localrc)
  print *, "Comp 1 Finalize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_GridCompFinalize(comp2, importState=exportState, &
    exportState=exportState, rc=localrc)
  print *, "Comp 2 Finalize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompDestroy(comp1, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_GridCompDestroy(comp2, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_CplCompDestroy(cpl, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_StateDestroy(importState, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_StateDestroy(exportState, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

10 continue

  ! Normal ESMF Test output
  print *, trim(testname), " complete."

  ! IMPORTANT: TestGlobal() prints the PASS: string that the scripts grep for.
  call ESMF_TestGlobal((rc.eq.ESMF_SUCCESS), testname, failMsg, testresult, &
    ESMF_SRCLINE)

  if ((localPet .eq. 0) .and. (rc .eq. ESMF_SUCCESS)) then
    ! Separate message to console, for quick confirmation of success/failure
    write(finalMsg, *) "SUCCESS: ",trim(testname)," finished correctly."
    write(0, *) ""
    write(0, *) trim(testname)
    write(0, *) trim(finalMsg)
    write(0, *) ""
  endif
  
  print *, "------------------------------------------------------------"
  print *, "------------------------------------------------------------"
  print *, "Test finished, localPet = ", localPet
  print *, "------------------------------------------------------------"
  print *, "------------------------------------------------------------"

  call ESMF_Finalize()

end program ESMF_ArrayScatterGatherSTest
    
!\end{verbatim}
