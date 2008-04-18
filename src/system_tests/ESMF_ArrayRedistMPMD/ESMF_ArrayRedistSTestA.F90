! $Id: ESMF_ArrayRedistSTestA.F90,v 1.1.2.4 2008/04/18 23:12:52 svasquez Exp $
!
!-------------------------------------------------------------------------
!ESMF_MPMD_SYSTEM_TEST   String used by test script to count system tests.
!=========================================================================

! ************************************************************************
! This is the MPMD version of the ArrayRedist system test
#define MODEL1
! ArrayRedistSTestA contains MODEL1 and ArrayRedistSTestB contains MODEL2
! ************************************************************************

!-------------------------------------------------------------------------
!
! !DESCRIPTION:
! System test ArrayRedistMPMD.  
!    Two gridded components and one coupler component, one-way coupling.
!
!    The two gridded components are compiled and linked into two separate
!    executables. The coupler component which must run on the union of
!    PETs is linked into both executables. The first executable is started
!    on the first 4 PETs and the second executable is started on the last
!    2 PETs.
!
!    First gridded component runs on 4 PETs and defines a 2D source Array 
!    100x150. Second gridded component defines a destination Array also 
!    100x150 but runs on only 2 PETs. Both gridded components use DELayouts 
!    with 1 DE per PET. The decomposition of the source Array is defined as 
!    (petCount x 1) = (4 x 1) while the destination Array is decomposed as 
!    (1 x petCount) = (1 x 2).
!
!    The first component initializes the source Array to a geometric function:
!
!       10.0 + 5.0*sin((I/Imax)*pi) + 2.0*sin((J/Jmax)*pi)
!
!    The coupler component runs on all 6 PETs and reconciles import and export
!    states which contain source and destination Array, respectively. The 
!    coupler component then calls ArrayRedist() to a redistribute he source
!    Array data onto the destination Array.
!    
!    Finally the second gridded component compares the data stored in the
!    destination Array to the exact solution of the above function as a measure
!    of the accuracy of the ArrayRedist() method.
!
!-------------------------------------------------------------------------
!\begin{verbatim}

program ESMF_ArrayRedistSTest
#define ESMF_METHOD "program ESMF_ArrayRedistSTest"

#include "ESMF.h"

  ! ESMF Framework module
  use ESMF_Mod
  use ESMF_TestMod

#ifdef MODEL1
  use user_model1, only : userm1_register
#endif
#ifdef MODEL2
  use user_model2, only : userm2_register
#endif
  use user_coupler, only : usercpl_register

  implicit none
    
  ! Local variables
  integer :: localPet, petCount, localrc, rc=ESMF_SUCCESS
  character(len=ESMF_MAXSTR) :: cname1, cname2, cplname
  type(ESMF_VM):: vm
  type(ESMF_State) :: c1exp, c2imp
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

#ifdef MODEL1
  write(testname, *) "System Test ESMF_ArrayRedistMPMD STest executable A"
#endif
#ifdef MODEL2
  write(testname, *) "System Test ESMF_ArrayRedistMPMD STest executable B"
#endif
  write(failMsg, *) "System Test failure"

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
  ! use petList to define comp1 on PET 0,1,2,3
  comp1 = ESMF_GridCompCreate(name=cname1, petList=(/0,1,2,3/), rc=localrc)
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

#ifdef MODEL1
  call ESMF_GridCompSetServices(comp1, userm1_register, localrc)
  print *, "Comp SetServices finished, rc= ", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
#endif

#ifdef MODEL2
  call ESMF_GridCompSetServices(comp2, userm2_register, localrc)
  print *, "Comp SetServices finished, rc= ", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
#endif

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
 
  c1exp = ESMF_StateCreate("comp1 export", ESMF_STATE_EXPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
#ifdef MODEL1
  call ESMF_GridCompInitialize(comp1, exportState=c1exp, rc=localrc)
  print *, "Comp 1 Initialize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
#endif
 
  c2imp = ESMF_StateCreate("comp2 import", ESMF_STATE_IMPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
#ifdef MODEL2
  call ESMF_GridCompInitialize(comp2, importState=c2imp, rc=localrc)
  print *, "Comp 2 Initialize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
#endif

  ! note that the coupler's import is comp1's export state
  ! and coupler's export is comp2's import state
  call ESMF_CplCompInitialize(cpl, c1exp, c2imp, rc=localrc)
  print *, "Coupler Initialize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

#ifdef MODEL1
  call ESMF_GridCompRun(comp1, exportState=c1exp, rc=localrc)
  print *, "Comp 1 Run returned, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
#endif

  call ESMF_CplCompRun(cpl, c1exp, c2imp, rc=localrc)
  print *, "Coupler Run returned, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

#ifdef MODEL2
  call ESMF_GridCompRun(comp2, importState=c2imp, rc=localrc)
  print *, "Comp 2 Run returned, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
#endif

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_CplCompFinalize(cpl, c1exp, c2imp, rc=localrc)
  print *, "Coupler Finalize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

#ifdef MODEL1
  call ESMF_GridCompFinalize(comp1, exportState=c1exp, rc=localrc)
  print *, "Comp 1 Finalize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
#endif

#ifdef MODEL2
  call ESMF_GridCompFinalize(comp2, importState=c2imp, rc=localrc)
  print *, "Comp 2 Finalize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
#endif

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

  call ESMF_StateDestroy(c1exp, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_StateDestroy(c2imp, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

10 continue

  ! Normal ESMF Test output
  print *, testname, " complete."

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

end program ESMF_ArrayRedistSTest
    
!\end{verbatim}
