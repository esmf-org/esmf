! $Id: ESMF_ArrayRedistOpenMPSTest.F90,v 1.6 2009/10/30 15:47:14 theurich Exp $
!
!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_SYSTEM_TEST   String used by test script to count system tests.
!=========================================================================

!-------------------------------------------------------------------------
!
! !DESCRIPTION:
! System test ArrayRedistOpenMP.  
!    Two gridded components and one coupler component, one-way coupling.
!
!    First gridded component receives 8 PETs from the driver. However, using
!    GridCompSetVM calls in its SetVM routine it requests 2PEs to be provided
!    to each of its PETs. If this request can be accommodated the component will
!    execute on 4 PETs with 2PEs each. If the request cannot be accommodated the
!    component will execute with 8PETs. In either case it defines a 2D source
!    Array 100x1500. Second gridded component defines a destination Array also 
!    100x1500 but runs on only 2 PETs. Both gridded components use DELayouts 
!    with 1 DE per PET. The decomposition of the source Array is defined as 
!    (petCount x 1) = (4 x 1) or (8 x 1), depending on whether each PET received
!    2PEs or not. The destination Array is decomposed as (1 x petCount) = 
!    (1 x 2).
!
!    Using OpenMP directives for loop-parallelization the first component
!    initializes the source Array to a geometric function:
!
!       10.0 + 5.0*sin((I/Imax)*pi) + 2.0*sin((J/Jmax)*pi)
!
!    The coupler component runs on all 8 PETs and reconciles import and export
!    states which contain source and destination Array, respectively. The 
!    coupler component then calls ArrayRedist() to redistribute the source
!    Array data onto the destination Array.
!    
!    Finally the second gridded component compares the data stored in the
!    destination Array to the exact solution of the above function as a measure
!    of the accuracy of the ArrayRedist() method.
!
!-------------------------------------------------------------------------
!\begin{verbatim}

program ESMF_ArrayRedistOpenMPSTest
#define ESMF_METHOD "program ESMF_ArrayRedistOpenMPSTest"

#include "ESMF.h"

  ! ESMF Framework module
  use ESMF_Mod
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
  write(testname, *) "System Test ESMF_ArrayRedistOpenMPSTest"

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
  call ESMF_Initialize(vm=vm, defaultlogfilename="ArrayRedistOpenMPSTest.Log", &
    defaultlogtype=ESMF_LOG_MULTI, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Get number of PETs we are running with
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Create the 2 model components and coupler
  cname1 = "user model 1"
  ! use petList to define comp1 on all 8 PETs. The chosen sequence of PETs will
  ! ensure that when comp1 sets its VM properties to associate 2 PEs with each
  ! PET, the pairs will be 0+4, 1+5, 2+6, 3+7. This is unless these PETs are
  ! not on the same single system id, in which case pairs will be formed
  ! differently (if possible). The above mentioned pairs will operate, i.e.
  ! execute comp1, on the virtual address space of PET 0, 1, 2, 3. The srcArray
  ! created by comp1 will only exist in these 4 virtual address spaces. Comp1
  ! code will use user-level OpenMP loop parallelization to utilize the fact
  ! that there are 2 PEs for each executing PET.
  comp1 = ESMF_GridCompCreate(name=cname1, petList=(/0,4,1,5,2,6,3,7/), rc=localrc)
  print *, "Created component ", trim(cname1), "rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  cname2 = "user model 2"
  ! use petList to define comp2 on PET 4,5
  comp2 = ESMF_GridCompCreate(name=cname2, petList=(/4,5/), rc=localrc)
  print *, "Created component ", trim(cname2), "rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  cplname = "user one-way coupler"
  ! no petList means that coupler component runs on all PETs
  cpl = ESMF_CplCompCreate(name=cplname, rc=localrc)
  print *, "Created component ", trim(cplname), ", rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  print *, "Comp Creates finished"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompSetVM(comp1, userRoutine=userm1_setvm, rc=localrc, &
    userRc=userrc)
  print *, "Comp1 SetVM finished, rc= ", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_GridCompSetServices(comp1, userRoutine=userm1_register, rc=localrc,&
    userRc=userrc)
  print *, "Comp1 SetServices finished, rc= ", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridCompSetVM(comp2, userRoutine=userm2_setvm, rc=localrc, &
    userRc=userrc)
  print *, "Comp2 SetVM finished, rc= ", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_GridCompSetServices(comp2, userRoutine=userm2_register, rc=localrc,&
    userRc=userrc)
  print *, "Comp2 SetServices finished, rc= ", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_CplCompSetVM(cpl, userRoutine=usercpl_setvm, rc=localrc, &
    userRc=userrc)
  print *, "Cpl SetVM finished, rc= ", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_CplCompSetServices(cpl, userRoutine=usercpl_register, rc=localrc, &
    userRc=userrc)
  print *, "Cpl SetServices finished, rc= ", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
  c1exp = ESMF_StateCreate("comp1 export", ESMF_STATE_EXPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_GridCompInitialize(comp1, exportState=c1exp, rc=localrc, &
    userRc=userrc)
  print *, "Comp 1 Initialize finished, rc =", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
 
  c2imp = ESMF_StateCreate("comp2 import", ESMF_STATE_IMPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_GridCompInitialize(comp2, importState=c2imp, rc=localrc, &
    userRc=userrc)
  print *, "Comp 2 Initialize finished, rc =", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
 
  ! note that the coupler's import is comp1's export state
  ! and coupler's export is comp2's import state
  call ESMF_CplCompInitialize(cpl, c1exp, c2imp, rc=localrc, userRc=userrc)
  print *, "Coupler Initialize finished, rc =", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompRun(comp1, exportState=c1exp, rc=localrc, userRc=userrc)
  print *, "Comp 1 Run returned, rc =", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_CplCompRun(cpl, c1exp, c2imp, rc=localrc, userRc=userrc)
  print *, "Coupler Run returned, rc =", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridCompRun(comp2, importState=c2imp, rc=localrc, userRc=userrc)
  print *, "Comp 2 Run returned, rc =", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_CplCompFinalize(cpl, c1exp, c2imp, rc=localrc, userRc=userrc)
  print *, "Coupler Finalize finished, rc =", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridCompFinalize(comp1, exportState=c1exp, rc=localrc, &
    userRc=userrc)
  print *, "Comp 1 Finalize finished, rc =", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridCompFinalize(comp2, importState=c2imp, rc=localrc, &
    userRc=userrc)
  print *, "Comp 2 Finalize finished, rc =", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompDestroy(comp1, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_GridCompDestroy(comp2, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_CplCompDestroy(cpl, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_StateDestroy(c1exp, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_StateDestroy(c2imp, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

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

  ! Print final PASS/FAIL and add # of procs message to log file.
  call ESMF_STest((rc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)
  
  call ESMF_Finalize()

end program ESMF_ArrayRedistOpenMPSTest
    
!\end{verbatim}
