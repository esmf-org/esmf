! $Id$
!
!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_SYSTEM_TEST   String used by test script to count system tests.
!=========================================================================

!-------------------------------------------------------------------------
!
! !DESCRIPTION:
! System test ArrayRedistOpenACC.
!
!   Two Gridded Components and one Coupler Component, one-way coupling.
!
!   The first Gridded Component receives 8 PETs from the driver. However, using
!   GridCompSetVM calls in its SetVM routine the Component requests 8 PEs to be
!   associated with a single PET. If this request can be accommodated (i.e. the
!   8 PEs received from the driver were on the same single system image), then
!   the Component will execute on a single PET that has access to all 8 PEs. If
!   the request cannot be accommodated, the Component will execute with 8 PETs,
!   holding 1 PE each. In either case the first Gridded Component defines a 2D
!   source Array of 100x1500. The decomposition of the source Array is defined
!   as (petCount x 1), i.e. either (1 x 1) or (8 x 1).
!
!   The second Gridded Component defines a destination Array also of 100x1500,
!   but runs on only 2 PETs. The destination Array is decomposed as
!   (1 x petCount), i.e. (1 x 2).
!
!   Using OpenACC directives for loop-parallelization the first Component
!   initializes the source Array to a geometric function:
!
!     10.0 + 5.0*sin((I/Imax)*pi) + 2.0*sin((J/Jmax)*pi)
!
!   The Coupler Component runs on all 8 PETs and reconciles import and export
!   States, which contain the source and destination Arrays, respectively. The
!   Coupler Component then calls ArrayRedist() to redistribute the source
!   Array data onto the destination Array.
!
!   Finally the second Gridded Component compares the data stored in the
!   destination Array to the analytic solution of the above function as a
!   measure of the accuracy of the ArrayRedist() method.
!
!-------------------------------------------------------------------------
!\begin{verbatim}

program ESMF_ArrayRedistOpenACCSTest
#define ESMF_METHOD "program ESMF_ArrayRedistOpenACCSTest"

#include "ESMF.h"

  ! ESMF Framework module
  use ESMF
  use ESMF_TestMod

  use user_model1, only : userm1_setvm, userm1_register
  use user_model2, only : userm2_setvm, userm2_register
  use user_coupler, only : usercpl_setvm, usercpl_register

  implicit none

  ! Local variables
  integer :: localPet, petCount, localrc, rc=ESMF_SUCCESS, userrc
  character(len=ESMF_MAXSTR) :: cname1, cname2, cplname
  type(ESMF_VM):: vm
  type(ESMF_State) :: c1exp, c2imp
  type(ESMF_GridComp) :: comp1, comp2
  type(ESMF_CplComp) :: cpl

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test name
  character(ESMF_MAXSTR) :: testname

  ! individual test failure message, and final status msg
  character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "System Test failure"
  write(testname, *) "System Test ESMF_ArrayRedistOpenACCSTest"

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
  call ESMF_Initialize(vm=vm, &
    defaultlogfilename="ArrayRedistOpenACCSTest.Log", &
    logkindflag=ESMF_LOGKIND_MULTI, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_LogSet (flush=.true.)

  ! Get number of PETs we are running with
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

   ! Check for correct number of PETs
  if ( petCount < 8 ) then
     call ESMF_LogSetError(ESMF_RC_ARG_BAD,&
         msg="This system test does not run on fewer than 8 PETs.",&
         ESMF_CONTEXT, rcToReturn=rc)
     call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
   endif

  ! Create the 2 model components and coupler
  cname1 = "user model 1"
  ! use no petList to simply obtain _all_ parent PETs. The Component SetVM 
  ! method will try to have only a single PET execute and use user-level
  ! OpenACC loop parallelization to utilize a GPU that is available to the 
  ! single executing PET. This essentially is wasting the 8PEs that are
  ! attached to the single PET, because the work is offloaded to the accelerator
  ! GPU.
  comp1 = ESMF_GridCompCreate(name=cname1, rc=localrc)
  print *, "Created component ", trim(cname1), "rc =", localrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  cname2 = "user model 2"
  ! use petList to define comp2 on PET 4,5
  comp2 = ESMF_GridCompCreate(name=cname2, petList=(/4,5/), rc=localrc)
  print *, "Created component ", trim(cname2), "rc =", localrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  cplname = "user one-way coupler"
  ! no petList means that coupler component runs on all PETs
  cpl = ESMF_CplCompCreate(name=cplname, rc=localrc)
  print *, "Created component ", trim(cplname), ", rc =", localrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  print *, "Comp Creates finished"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompSetVM(comp1, userRoutine=userm1_setvm, rc=localrc, &
    userRc=userrc)
  print *, "Comp1 SetVM finished, rc= ", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompSetServices(comp1, userRoutine=userm1_register, rc=localrc,&
    userRc=userrc)
  print *, "Comp1 SetServices finished, rc= ", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetVM(comp2, userRoutine=userm2_setvm, rc=localrc, &
    userRc=userrc)
  print *, "Comp2 SetVM finished, rc= ", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompSetServices(comp2, userRoutine=userm2_register, rc=localrc,&
    userRc=userrc)
  print *, "Comp2 SetServices finished, rc= ", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_CplCompSetVM(cpl, userRoutine=usercpl_setvm, rc=localrc, &
    userRc=userrc)
  print *, "Cpl SetVM finished, rc= ", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_CplCompSetServices(cpl, userRoutine=usercpl_register, rc=localrc, &
    userRc=userrc)
  print *, "Cpl SetServices finished, rc= ", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  c1exp = ESMF_StateCreate(name="comp1 export",  &
                           stateintent=ESMF_STATEINTENT_EXPORT, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompInitialize(comp1, exportState=c1exp, rc=localrc, &
    userRc=userrc)
  print *, "Comp 1 Initialize finished, rc =", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  c2imp = ESMF_StateCreate(name="comp2 import",  &
                           stateintent=ESMF_STATEINTENT_IMPORT, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompInitialize(comp2, importState=c2imp, rc=localrc, &
    userRc=userrc)
  print *, "Comp 2 Initialize finished, rc =", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! note that the coupler's import is comp1's export state
  ! and coupler's export is comp2's import state
  call ESMF_CplCompInitialize(cpl, importState=c1exp, &
    exportState=c2imp, rc=localrc, userRc=userrc)
  print *, "Coupler Initialize finished, rc =", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompRun(comp1, exportState=c1exp, rc=localrc, userRc=userrc)
  print *, "Comp 1 Run returned, rc =", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_CplCompRun(cpl, importState=c1exp, &
    exportState=c2imp, rc=localrc, userRc=userrc)
  print *, "Coupler Run returned, rc =", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompRun(comp2, importState=c2imp, rc=localrc, userRc=userrc)
  print *, "Comp 2 Run returned, rc =", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_CplCompFinalize(cpl, importState=c1exp, &
    exportState=c2imp, rc=localrc, userRc=userrc)
  print *, "Coupler Finalize finished, rc =", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompFinalize(comp1, exportState=c1exp, rc=localrc, &
    userRc=userrc)
  print *, "Comp 1 Finalize finished, rc =", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompFinalize(comp2, importState=c2imp, rc=localrc, &
    userRc=userrc)
  print *, "Comp 2 Finalize finished, rc =", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompDestroy(comp1, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompDestroy(comp2, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_CplCompDestroy(cpl, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateDestroy(c1exp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_StateDestroy(c2imp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

10 continue

  ! Normal ESMF Test output
  print *, testname, " complete."

  if (rc .eq. ESMF_SUCCESS) then
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

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors 
  ! in the log file that the scripts grep for.
  call ESMF_STest((rc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize()

end program ESMF_ArrayRedistOpenACCSTest

!\end{verbatim}
