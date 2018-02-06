! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

#define FILENAME "src/Superstructure/Component/tests/ESMF_CompTunnelUTest.F90"

#include "ESMF_Macros.inc"
#include "ESMF.h"

! The SLEEPTIME macro defines the time in seconds that Finalize will delay
#define SLEEPTIME 3.0_ESMF_KIND_R8

module ESMF_CompTunnelUTest_comp_mod

  ! modules
  use ESMF_TestMod     ! test methods
  use ESMF
  
  implicit none
  
  private
  
  public setvm, setservices

  contains !--------------------------------------------------------------------

  subroutine setvm(gcomp, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
    
    ! Initialize
    rc = ESMF_SUCCESS

    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetGlobal(vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
!    if (pthreadsEnabled) then
!      call ESMF_GridCompSetVMMinThreads(gcomp, rc=rc)
!      if (rc/=ESMF_SUCCESS) return ! bail out
!    endif

  end subroutine !--------------------------------------------------------------

  recursive subroutine setservices(gcomp, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
    
    ! Initialize
    rc = ESMF_SUCCESS

    ! register RUN method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=initialize, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run, rc=rc)
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, phase=2, &
      userRoutine=run2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=finalize, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine !--------------------------------------------------------------
  
  recursive subroutine initialize(gcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc
    
    ! Initialize
    rc = ESMF_SUCCESS
        
    call ESMF_LogWrite("Actual Component Initialize", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end subroutine !--------------------------------------------------------------

  recursive subroutine run(gcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc
    
    ! Initialize
    rc = ESMF_SUCCESS
    
    call ESMF_LogWrite("Actual Component Run", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! return with a unique userRc so in order to test return code handling
    rc = 13141516 ! unique return code

  end subroutine !--------------------------------------------------------------

  recursive subroutine run2(gcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc
    
    ! local variables
    type(ESMF_VM)          :: vm 

    ! Initialize
    rc = ESMF_SUCCESS
    
    call ESMF_LogWrite("Actual Component Run", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! In order to test blocking/non-blocking dual component feature put the
    ! actual component to sleep for a few seconds:
    call ESMF_VMWtimeDelay(SLEEPTIME) ! sleep a few seconds
    
    ! Need a barrier so that time tests on the dual component PETs will be
    ! as expected. This is only for testing! Generally this kind of
    ! synchronization is not desirable.
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_VMBarrier(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_LogWrite("Actual Component exit Run2 after sleep", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! return with a unique userRc so in order to test return code handling
    rc = 27282920 ! unique return code

  end subroutine !--------------------------------------------------------------

  recursive subroutine finalize(gcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc
    
    ! local variables
    type(ESMF_VM)          :: vm 

    ! Initialize
    rc = ESMF_SUCCESS
    
    call ESMF_LogWrite("Actual Component Finalize", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    ! In order to test blocking/non-blocking dual component feature put the
    ! actual component to sleep for a few seconds:
    call ESMF_VMWtimeDelay(SLEEPTIME) ! sleep a few seconds
    
    ! Need a barrier so that time tests on the dual component PETs will be
    ! as expected. This is only for testing! Generally this kind of
    ! synchronization is not desirable.
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_VMBarrier(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_LogWrite("Actual Component exit Finalize after sleep", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine !--------------------------------------------------------------

end module

!==============================================================================
!==============================================================================
!==============================================================================

program ESMF_CompTunnelUTest

!==============================================================================
!BOP
! !PROGRAM: ESMF_CompTunnelUTest -  Tests Dual Component Concept
!
! !DESCRIPTION:
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF

  use ESMF_CompTunnelUTest_comp_mod, only: setvm, setservices

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  integer                :: rc, userRc, petCount, localPet, i
  integer, allocatable   :: petList(:)
  type(ESMF_VM)          :: vm
  type(ESMF_GridComp)    :: actualComp, dualComp
  real(ESMF_KIND_R8)     :: startTime, endTime, precTime, delayTime
  character(ESMF_MAXSTR) :: logString
  
#ifdef ESMF_TESTEXHAUSTIVE
  type(ESMF_GridComp)    :: actualCompA, dualCompA
  type(ESMF_GridComp)    :: actualCompB, dualCompB
  type(ESMF_GridComp)    :: actualCompC, dualCompC
  type(ESMF_GridComp)    :: actualCompD, dualCompD
  type(ESMF_GridComp)    :: actualCompE, dualCompE, unionCompE
  integer                :: timeout
  logical                :: timeoutFlag
#endif  
  
  ! cumulative result: count failures; no failures equals "all pass"
  integer                :: result = 0

!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  !------------------------------------------------------------------------
  ! construct petList for the actual Component
  allocate(petList(ceiling(0.5 * petCount)))
  do i=1, size(petList)
    petList(i) = (i-1)*2  ! only the even numbered PETs
  enddo
  
  write(logString, *) "Actual Component petList = ", petList
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create the Actual Component"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  actualComp = ESMF_GridCompCreate(petList=petList, name="actualComp", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  deallocate(petList)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "SetVM for the Actual Component"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetVM(actualComp, userRoutine=setvm, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !NEX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Actual Component"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetServices(actualComp, userRoutine=setservices, &
    userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !NEX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! construct petList for the dual Component
  allocate(petList(floor(0.5 * petCount)))
  do i=1, size(petList)
    petList(i) = (i-1)*2+1  ! only the odd numbered PETs
  enddo

  write(logString, *) "Dual Component petList = ", petList
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create the Dual Component"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dualComp = ESMF_GridCompCreate(petList=petList, name="dualComp", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  deallocate(petList)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Dual Component only on participating PETs"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  if (ESMF_GridCompIsPetLocal(dualComp)) then
    call ESMF_GridCompSetServices(dualComp, actualGridcomp=actualComp, rc=rc)
  else
    rc=ESMF_SUCCESS ! manually set the ESMF_SUCCESS for the following tests
  endif
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ServiceLoop for the Actual Component"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompServiceLoop(actualComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Initialize for the Dual Component on all PETs -> Initialize Actual"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompInitialize(dualComp, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !NEX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Initialize for the Dual Component only on participating PETs -> Initialize Actual"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  if (ESMF_GridCompIsPetLocal(dualComp)) then
    call ESMF_GridCompInitialize(dualComp, userRc=userRc, rc=rc)
  else
    userRc=ESMF_SUCCESS ! manually set the ESMF_SUCCESS for the following tests
    rc=ESMF_SUCCESS ! manually set the ESMF_SUCCESS for the following tests
  endif
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !NEX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Run for the Dual Component -> Run Actual"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompRun(dualComp, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !NEX_UTest_Multi_Proc_Only
  if (ESMF_GridCompIsPetLocal(dualComp)) then
    write(failMsg, *) "Did not return 13141516 in userRc" 
    call ESMF_Test((userRc.eq.13141516), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
    call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Run for the Dual Component (phase 2) -> Run Actual (phase 2)"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompRun(dualComp, phase=2, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !NEX_UTest_Multi_Proc_Only
  if (ESMF_GridCompIsPetLocal(dualComp)) then
    write(failMsg, *) "Did not return 27282920 in userRc" 
    call ESMF_Test((userRc.eq.27282920), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
    call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  endif
  !------------------------------------------------------------------------

  call ESMF_VMWtimePrec(precTime, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  write(logString, *) "precTime = ", precTime
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMWtime(startTime, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Finalize for the Dual Component (blocking) -> Finalize Actual"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompFinalize(dualComp, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !NEX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Check time delay on blocking Dual Finalize -> Finalize Actual"
  write(failMsg, *) "Incorrect time delay" 
  call ESMF_VMWtime(endTime, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  delayTime = endTime - startTime
  if (ESMF_GridCompIsPetLocal(dualComp)) then
    ! PETs in Dual Component petList must be blocking
    call ESMF_Test(delayTime > SLEEPTIME-2*precTime, name, failMsg, result, ESMF_SRCLINE)
  else
    ! PETs not in Dual Component petList must not be blocking
    call ESMF_Test(delayTime < SLEEPTIME+2*precTime, name, failMsg, result, ESMF_SRCLINE)
  endif
  write(logString, *) "delayTime (blocking) = ", delayTime
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  call ESMF_VMWtime(startTime, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Finalize for the Dual Component (non-blocking) -> Finalize Actual"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompFinalize(dualComp, syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Check time delay on non-blocking Dual Finalize -> Finalize Actual"
  write(failMsg, *) "Incorrect time delay" 
  call ESMF_VMWtime(endTime, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  delayTime = endTime - startTime
  ! all PETs must not be blocking
  call ESMF_Test(delayTime < SLEEPTIME+2*precTime, name, failMsg, result, ESMF_SRCLINE)
  write(logString, *) "delayTime (non-blocking) = ", delayTime
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Wait for non-blocking Dual Finalize -> wait for Actual Finalize"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompWait(dualComp, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !NEX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Check time delay on Wait for non-blocking Dual Finalize -> Finalize Actual"
  write(failMsg, *) "Incorrect time delay" 
  call ESMF_VMWtime(endTime, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  delayTime = endTime - startTime
  if (ESMF_GridCompIsPetLocal(dualComp)) then
    ! PETs in Dual Component petList must be blocking
    call ESMF_Test(delayTime > SLEEPTIME-2*precTime, name, failMsg, result, ESMF_SRCLINE)
  else
    ! PETs not in Dual Component petList must not be blocking
    call ESMF_Test(delayTime < SLEEPTIME+2*precTime, name, failMsg, result, ESMF_SRCLINE)
  endif
  write(logString, *) "delayTime (wait) = ", delayTime
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------
 
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy the Dual Component"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompDestroy(dualComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy the Actual Component"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompDestroy(actualComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
#ifdef ESMF_TESTEXHAUSTIVE
!-------------------------------------------------------------------------------  
!-------------------------------------------------------------------------------  
! Start of exhaustive block  
!-------------------------------------------------------------------------------  
  
  if (petCount /= 8) goto 10  ! skip all of the exhaustive tests if not on 8PET
  
  !------------------------------------------------------------------------
  ! The exhaustive tests break into three dual-actual pairs: A, B, C. The 
  ! dual, i.e. front-end component for all these pairs is always defined on
  ! the same two PETs. The actual components are on different PETs.
  ! Consequently,  calling into the dual components with non-blocking syncflag
  ! allows concurrent execution of the actual components. The layout across the
  ! 8 PETs is as follows:
  !
  !   PET:        0     1     2     3     4     5     6     7
  !   pair-A:           dual  act.  act.  dual        act.
  !   pair-B:     act.  dual              dual
  !   pair-C:           dual              dual  act.        act.
  !
  ! Pairs "D" and "E" are created that test the socket based component tunnel
  ! implementation. They must be created up here with the other component pairs,
  ! because of how component C is testing implicit termination through automatic
  ! garbage collection on Finalize (no explicit Destroy call). Pairs "D" and "E"
  ! are socket based and look like this:
  !
  !   PET:        0     1     2     3     4     5     6     7
  !   pair-D:           dual  act.  act.  dual        act.
  !   pair-E:     act.  act.  dual  act.  dual        act.
  !------------------------------------------------------------------------
  
  ! --- create As ---

  !------------------------------------------------------------------------
  ! construct petList for the actual Component A
  allocate(petList(3))
  petList = (/2,3,6/)
  
  write(logString, *) "Actual Component A petList = ", petList
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Create the Actual Component A"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  actualCompA = ESMF_GridCompCreate(petList=petList, name="actualCompA", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  deallocate(petList)

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetVM for the Actual Component A"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetVM(actualCompA, userRoutine=setvm, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Actual Component A"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetServices(actualCompA, userRoutine=setservices, &
    userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! construct petList for the dual Component A
  allocate(petList(2))
  petList = (/1,4/)

  write(logString, *) "Dual Component A petList = ", petList
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Create the Dual Component A"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dualCompA = ESMF_GridCompCreate(petList=petList, name="dualCompA", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  deallocate(petList)

  ! --- create Bs ---

  !------------------------------------------------------------------------
  ! construct petList for the actual Component B
  allocate(petList(1))
  petList = (/0/)
  
  write(logString, *) "Actual Component B petList = ", petList
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Create the Actual Component B"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  actualCompB = ESMF_GridCompCreate(petList=petList, name="actualCompB", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  deallocate(petList)

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetVM for the Actual Component B"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetVM(actualCompB, userRoutine=setvm, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Actual Component B"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetServices(actualCompB, userRoutine=setservices, &
    userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! construct petList for the dual Component B
  allocate(petList(2))
  petList = (/1,4/)

  write(logString, *) "Dual Component B petList = ", petList
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Create the Dual Component B"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dualCompB = ESMF_GridCompCreate(petList=petList, name="dualCompB", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  deallocate(petList)

  ! --- create Cs ---

  !------------------------------------------------------------------------
  ! construct petList for the actual Component C
  allocate(petList(2))
  petList = (/5,7/)
  
  write(logString, *) "Actual Component C petList = ", petList
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Create the Actual Component C"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  actualCompC = ESMF_GridCompCreate(petList=petList, name="actualCompC", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  deallocate(petList)

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetVM for the Actual Component C"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetVM(actualCompC, userRoutine=setvm, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Actual Component C"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetServices(actualCompC, userRoutine=setservices, &
    userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! construct petList for the dual Component C
  allocate(petList(2))
  petList = (/1,4/)

  write(logString, *) "Dual Component C petList = ", petList
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Create the Dual Component C"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dualCompC = ESMF_GridCompCreate(petList=petList, name="dualCompC", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  deallocate(petList)

  ! --- create Ds ---

  !------------------------------------------------------------------------
  ! construct petList for the actual Component D
  allocate(petList(3))
  petList = (/2,3,6/)
  
  write(logString, *) "Actual Component D petList = ", petList
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Create the Actual Component D"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  actualCompD = ESMF_GridCompCreate(petList=petList, name="actualCompD", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  deallocate(petList)

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetVM for the Actual Component D"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetVM(actualCompD, userRoutine=setvm, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Actual Component D"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetServices(actualCompD, userRoutine=setservices, &
    userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! construct petList for the dual Component D
  allocate(petList(2))
  petList = (/1,4/)

  write(logString, *) "Dual Component D petList = ", petList
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Create the Dual Component D"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dualCompD = ESMF_GridCompCreate(petList=petList, name="dualCompD", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  deallocate(petList)
  
  ! --- create Es ---

  !------------------------------------------------------------------------
  ! construct petList for the actual Component E
  allocate(petList(4))
  petList = (/0,1,3,6/)
  
  write(logString, *) "Actual Component E petList = ", petList
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Create the Actual Component E"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  actualCompE = ESMF_GridCompCreate(petList=petList, name="actualCompE", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  deallocate(petList)

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetVM for the Actual Component E"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetVM(actualCompE, userRoutine=setvm, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Actual Component E"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetServices(actualCompE, userRoutine=setservices, &
    userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! construct petList for the dual Component E
  allocate(petList(2))
  petList = (/4,2/) ! PET 4 is first, to make it rootPet in dual comp E

  write(logString, *) "Dual Component E petList = ", petList
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Create the Dual Component E"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dualCompE = ESMF_GridCompCreate(petList=petList, name="dualCompE", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  deallocate(petList)
  
  !------------------------------------------------------------------------
  ! construct petList for the union Component E
  ! -> this is a dummy component, only created for its VM which is used to
  ! -> synchronize PETs that participate in dual and actual components E
  allocate(petList(6))
  petList = (/0,1,2,3,4,6/) ! union of dual and actual PETs

  write(logString, *) "Union Component E petList = ", petList
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Create the Union Component E"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  unionCompE = ESMF_GridCompCreate(petList=petList, name="unionCompE", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  deallocate(petList)
  
  ! -> must call SetServices() for union component for it to have valid vm
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Union Component E"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetServices(unionCompE, userRoutine=setservices, &
    userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! --- connect As ---

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ServiceLoop for the Actual Component A - with timeout"
  write(failMsg, *) "Did return ESMF_SUCCESS" 
  call ESMF_GridCompServiceLoop(actualCompA, timeout=10, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ServiceLoop for the Actual Component A"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompServiceLoop(actualCompA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Dual Component A on all PETs"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetServices(dualCompA, actualGridcomp=actualCompA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! --- connect Bs ---

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Dual Component B on all PETs"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetServices(dualCompB, actualGridcomp=actualCompB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ServiceLoop for the Actual Component B"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompServiceLoop(actualCompB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! --- connect Cs ---

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Dual Component C on all PETs"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetServices(dualCompC, actualGridcomp=actualCompC, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ServiceLoop for the Actual Component C"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompServiceLoop(actualCompC, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! --- initialize A, B, C in blocking mode

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Initialize for the Dual Component A -> Initialize Actual A"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompInitialize(dualCompA, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Initialize for the Dual Component B -> Initialize Actual B"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompInitialize(dualCompB, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Initialize for the Dual Component C -> Initialize Actual C"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompInitialize(dualCompC, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! --- finalize A, B in non-blocking mode, C in blocking mode

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Finalize for the Dual Component A (non-blocking) -> Finalize Actual A"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompFinalize(dualCompA, syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Finalize for the Dual Component B (non-blocking) -> Finalize Actual B"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompFinalize(dualCompB, syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Finalize for the Dual Component C -> Finalize Actual C"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompFinalize(dualCompC, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! --- wait for A's non-blocking finalize, but don't wait for B, showing that
  ! --- the destroy call will correctly call the wait internally, discard the
  ! --- received return codes, and terminate the ServiceLoop on the actual side

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Wait for non-blocking Dual Finalize A -> wait for Actual Finalize A"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompWait(dualCompA, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "2nd Wait for non-blocking Dual Finalize A -> should not hang"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompWait(dualCompA, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! --- destroy As ---
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Destroy the Dual Component A (before actual)"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompDestroy(dualCompA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Destroy the Actual Component A (after dual)"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompDestroy(actualCompA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  ! --- destroy Bs ---
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Destroy the Actual Component B (before dual)"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompDestroy(actualCompB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Destroy the Dual Component B (after actual) - testing integrated wait mechanism"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompDestroy(dualCompB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! --- don't destroy Cs in order to test that automatic garbage collection
  ! --- during ESMF_Finalize() correctly terminates Cs ServiceLoop & cleans up

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
#ifdef ESMF_TESTCOMPTUNNEL
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  ! -- socket based component pair "D" with the following petLists:
  !
  !   PET:        0     1     2     3     4     5     6     7
  !   pair-D:           dual  act.  act.  dual        act.
  !------------------------------------------------------------------------
  
  ! --- connect Ds ---

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ServiceLoop for the Actual Component D"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompServiceLoop(actualCompD, port=61010, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Dual Component D - using default 'localhost'"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetServices(dualCompD, port=61010, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! --- initialize Ds blocking

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Initialize (blocking) for the Dual Component D -> Initialize Actual D"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompInitialize(dualCompD, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! --- run Ds blocking and non-blocking

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Run (blocking) for the Dual Component D-> Run Actual D"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompRun(dualCompD, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  if (ESMF_GridCompIsPetLocal(dualCompD)) then
    write(failMsg, *) "Did not return 13141516 in userRc" 
    call ESMF_Test((userRc.eq.13141516), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
    call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  endif
  !------------------------------------------------------------------------

  call ESMF_VMWtime(startTime, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Run phase=2 (blocking) for the Dual Component D-> Run Actual D"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompRun(dualCompD, phase=2, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  if (ESMF_GridCompIsPetLocal(dualCompD)) then
    write(failMsg, *) "Did not return 27282920 in userRc" 
    call ESMF_Test((userRc.eq.27282920), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
    call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Check time delay on blocking Component D Run2"
  write(failMsg, *) "Incorrect time delay" 
  call ESMF_VMWtime(endTime, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  delayTime = endTime - startTime
  if (ESMF_GridCompIsPetLocal(dualCompD)) then
    ! PETs in Dual Component petList must be blocking
    call ESMF_Test(delayTime > SLEEPTIME-2*precTime, name, failMsg, result, ESMF_SRCLINE)
  else
    ! PETs not in Dual Component petList must not be blocking
    call ESMF_Test(delayTime < SLEEPTIME+2*precTime, name, failMsg, result, ESMF_SRCLINE)
  endif
  write(logString, *) "delayTime (blocking) = ", delayTime
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Run phase=2 (non-blocking) for the Dual Component D-> Run Actual D"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  userRc = -999 ! set to something obvious -> must change to SUCCESS on all PETs
  call ESMF_GridCompRun(dualCompD, phase=2, syncflag=ESMF_SYNC_NONBLOCKING, &
    userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Wait for non-blocking Run phase=2 for Component pair D"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompWait(dualCompD, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  if (ESMF_GridCompIsPetLocal(dualCompD)) then
    write(failMsg, *) "Did not return 27282920 in userRc" 
    call ESMF_Test((userRc.eq.27282920), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
    call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  endif
  !------------------------------------------------------------------------

  ! --- finalize Ds non-blocking

  call ESMF_VMWtime(startTime, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Finalize (non-blocking) for the Dual Component D-> Run Actual D"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompFinalize(dualCompD, syncflag=ESMF_SYNC_NONBLOCKING, &
    userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Wait for non-blocking Finalize for Component pair D"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompWait(dualCompD, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Check time delay on non-blocking Component D Finalize/wait"
  write(failMsg, *) "Incorrect time delay" 
  call ESMF_VMWtime(endTime, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  delayTime = endTime - startTime
  if (ESMF_GridCompIsPetLocal(dualCompD)) then
    ! PETs in Dual Component petList must be blocking
    call ESMF_Test(delayTime > SLEEPTIME-2*precTime, name, failMsg, result, ESMF_SRCLINE)
  else
    ! PETs not in Dual Component petList must not be blocking
    call ESMF_Test(delayTime < SLEEPTIME+2*precTime, name, failMsg, result, ESMF_SRCLINE)
  endif
  write(logString, *) "delayTime (wait) = ", delayTime
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------
  
  ! --- destroy Ds ---
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Destroy the Dual Component D (before actual), releasing Actual Component D"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompDestroy(dualCompD, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Destroy the Actual Component D (after dual)"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompDestroy(actualCompD, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  ! -- socket based component pair "E" with the following petLists:
  !
  !   PET:        0     1     2     3     4     5     6     7
  !   pair-E:     act.  act.  dual  act.  dual        act.
  !------------------------------------------------------------------------
  
  ! --- actual side: port number in ServiceLoop ---
 
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ServiceLoop for the Actual Component E - invalid port number"
  write(failMsg, *) "Did return ESMF_SUCCESS" 
  call ESMF_GridCompServiceLoop(actualCompE, port=600, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! --- actual side: timeout on ServiceLoop ---
  
  call ESMF_GridCompGet(actualCompE, vm=vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! synchronize PETs across actualCompE for time delay testing of timeout
  if (ESMF_GridCompIsPetLocal(actualCompE)) then
    call ESMF_VMBarrier(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
 
  call ESMF_VMWtime(startTime, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! double sync here so rootPet has no chance of getting ahead before the
  ! other PETs get their startTime  
  if (ESMF_GridCompIsPetLocal(actualCompE)) then
    call ESMF_VMBarrier(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  timeout = 5
 
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ServiceLoop for the Actual Component E - timeout"
  call ESMF_GridCompServiceLoop(actualCompE, port=61011, timeout=timeout, rc=rc)
  if (ESMF_GridCompIsPetLocal(actualCompE)) then
    write(failMsg, *) "Did return ESMF_SUCCESS" 
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Check time delay on timeout ServiceLoop Actual Component E"
  write(failMsg, *) "Incorrect time delay" 
  call ESMF_VMWtime(endTime, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  delayTime = endTime - startTime
  if (ESMF_GridCompIsPetLocal(actualCompE)) then
    ! PETs in Dual Component petList must be blocking
    call ESMF_Test(delayTime > real(timeout, ESMF_KIND_R8)-2*precTime, name, failMsg, result, ESMF_SRCLINE)
  else
    ! PETs not in Dual Component petList must not be blocking
    call ESMF_Test(delayTime < real(timeout,ESMF_KIND_R8)+2*precTime, name, failMsg, result, ESMF_SRCLINE)
  endif
  write(logString, *) "delayTime (wait) = ", delayTime
  call ESMF_LogWrite(logString, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  ! make sure the dual component is not executed while the actual component waits
  if (ESMF_GridCompIsPetLocal(dualCompE)) then
    call ESMF_VMWtimeDelay(real(timeout+1,ESMF_KIND_R8)) ! sleep a few seconds
  endif
  
  ! --- actual side: timeout on ServiceLoop again, but this time with timeoutFlag ---
  
  timeoutFlag = .false.
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ServiceLoop for the Actual Component E - timeoutFlag"
  write(failMsg, *) "Did return ESMF_SUCCESS" 
  call ESMF_GridCompServiceLoop(actualCompE, port=61011, timeout=timeout, &
    timeoutFlag=timeoutFlag, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Testing timeoutFlag"
  write(failMsg, *) "timeoutFlag wrong" 
  if (ESMF_GridCompIsPetLocal(actualCompE)) then
    call ESMF_Test(timeoutFlag, name, failMsg, result, ESMF_SRCLINE)
  else
    call ESMF_Test(.not.timeoutFlag, name, failMsg, result, ESMF_SRCLINE)
  endif
  !------------------------------------------------------------------------

  ! make sure the dual component is not executed while the actual component waits
  if (ESMF_GridCompIsPetLocal(dualCompE)) then
    call ESMF_VMWtimeDelay(real(timeout+1,ESMF_KIND_R8)) ! sleep a few seconds
  endif
  
  ! --- dual side: port number in SetServices ---
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Dual Component E - invalid port number"
  write(failMsg, *) "Did return ESMF_SUCCESS" 
  call ESMF_GridCompSetServices(dualCompE, port=600, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  ! --- dual side: timeout on SetServices ---
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Dual Component E - timeout"
  call ESMF_GridCompSetServices(dualCompE, port=61012, timeout=timeout, rc=rc)
  if (ESMF_GridCompIsPetLocal(dualCompE)) then
    write(failMsg, *) "Did return ESMF_SUCCESS" 
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  endif
  !------------------------------------------------------------------------
  
  timeoutFlag = .false.

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Dual Component E - timeout with timeoutFlag"
  call ESMF_GridCompSetServices(dualCompE, port=61012, timeout=timeout, &
    timeoutFlag=timeoutFlag, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Testing timeoutFlag"
  write(failMsg, *) "timeoutFlag wrong" 
  if (ESMF_GridCompIsPetLocal(dualCompE)) then
    call ESMF_Test(timeoutFlag, name, failMsg, result, ESMF_SRCLINE)
  else
    call ESMF_Test(.not.timeoutFlag, name, failMsg, result, ESMF_SRCLINE)
  endif
  !------------------------------------------------------------------------

  ! --- connect Es with timeout on actual side: ServiceLoop ---

  call ESMF_GridCompGet(unionCompE, vm=vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! synchronize PETs across actual and dual components E
  if (ESMF_GridCompIsPetLocal(unionCompE)) then
    call ESMF_VMBarrier(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Dual Component E"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetServices(dualCompE, port=61013, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ServiceLoop for the Actual Component E - timeout after connect"
  call ESMF_GridCompServiceLoop(actualCompE, port=61013, timeout=timeout, rc=rc)
  if (ESMF_GridCompIsPetLocal(actualCompE)) then
    write(failMsg, *) "Did return ESMF_SUCCESS" 
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  endif
  !------------------------------------------------------------------------

  ! wait for all union PETs to make it here -> ensure timeout on actual side
  if (ESMF_GridCompIsPetLocal(unionCompE)) then
    call ESMF_VMBarrier(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! --- dual side E connected, but actual side timed out, i.e. disconnected ---

  ! - first non-blocking Initialize will not timeout because of send side buffer
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "First Initialize (non-blocking) on dual E, while actual E is disconnected"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompInitialize(dualCompE, syncflag=ESMF_SYNC_NONBLOCKING, &
    timeout=timeout, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! - a second Initialize will fail due first not finished
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Second Initialize (non-blocking) on dual E, while actual E is disconnected"
  call ESMF_GridCompInitialize(dualCompE, syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
  write(failMsg, *) "Did return ESMF_SUCCESS" 
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! - also a wait on the outstanding non-blocking Initialize will timeout
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Wating for non-blocking Initialize on dual E, while actual E is disconnected"
  call ESMF_GridCompWait(dualCompE, timeout=timeout, rc=rc)
  if (ESMF_GridCompIsPetLocal(dualCompE)) then
    write(failMsg, *) "Did return ESMF_SUCCESS" 
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  endif
  !------------------------------------------------------------------------
  
  timeoutFlag = .false.

  ! - all further waits on the outstanding non-blocking Initialize will timeout
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Wating for non-blocking Initialize on dual E, while actual E is disconnected"
  call ESMF_GridCompWait(dualCompE, timeout=timeout, timeoutFlag=timeoutFlag, &
    rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Testing timeoutFlag"
  write(failMsg, *) "timeoutFlag wrong" 
  if (ESMF_GridCompIsPetLocal(dualCompE)) then
    call ESMF_Test(timeoutFlag, name, failMsg, result, ESMF_SRCLINE)
  else
    call ESMF_Test(.not.timeoutFlag, name, failMsg, result, ESMF_SRCLINE)
  endif
  !------------------------------------------------------------------------
  
  ! --- re-connect Es with timeout on actual side: ServiceLoop ---

  ! synchronize PETs across actual and dual components E
  if (ESMF_GridCompIsPetLocal(unionCompE)) then
    call ESMF_VMBarrier(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Dual Component E"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetServices(dualCompE, port=61013, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ServiceLoop for the Actual Component E - timeout after connect"
  call ESMF_GridCompServiceLoop(actualCompE, port=61013, timeout=timeout, rc=rc)
  if (ESMF_GridCompIsPetLocal(actualCompE)) then
    write(failMsg, *) "Did return ESMF_SUCCESS" 
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  endif
  !------------------------------------------------------------------------

  ! wait for all union PETs to make it here -> ensure timeout on actual side
  if (ESMF_GridCompIsPetLocal(unionCompE)) then
    call ESMF_VMBarrier(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! --- again, dual side E connected, but actual side timed out, i.e. disconnected ---

  ! - blocking Initialize will timeout
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Initialize (blocking) on dual E, while actual E is disconnected"
  call ESMF_GridCompInitialize(dualCompE, timeout=timeout, rc=rc)
  if (ESMF_GridCompIsPetLocal(dualCompE)) then
    write(failMsg, *) "Did return ESMF_SUCCESS" 
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  endif
  !------------------------------------------------------------------------

  ! - blocking Run will timeout
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Run (blocking) on dual E, while actual E is disconnected"
  call ESMF_GridCompRun(dualCompE, timeout=timeout, rc=rc)
  if (ESMF_GridCompIsPetLocal(dualCompE)) then
    write(failMsg, *) "Did return ESMF_SUCCESS" 
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  endif
  !------------------------------------------------------------------------

  ! - blocking Finalize will timeout
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Finalize (blocking) on dual E, while actual E is disconnected"
  call ESMF_GridCompFinalize(dualCompE, timeout=timeout, rc=rc)
  if (ESMF_GridCompIsPetLocal(dualCompE)) then
    write(failMsg, *) "Did return ESMF_SUCCESS" 
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  endif
  !------------------------------------------------------------------------

  ! --- again re-connect Es ---

  ! synchronize PETs across actual and dual components E
  if (ESMF_GridCompIsPetLocal(unionCompE)) then
    call ESMF_VMBarrier(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Dual Component E"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetServices(dualCompE, port=61013, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ServiceLoop for the Actual Component E - default timeout"
  call ESMF_GridCompServiceLoop(actualCompE, port=61013, rc=rc)
!  call ESMF_GridCompServiceLoop(actualCompE, port=61013, timeout=60, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! --- initialize Es blocking

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Initialize (blocking) for the Dual Component E -> Initialize Actual E"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompInitialize(dualCompE, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! --- run Es blocking with timeout

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Run (blocking) for the Dual Component E-> Run Actual E"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  timeout = 2 ! 2s timeout
  call ESMF_GridCompRun(dualCompE, timeout=timeout, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  if (ESMF_GridCompIsPetLocal(dualCompE)) then
    write(failMsg, *) "Did not return 13141516 in userRc" 
    call ESMF_Test((userRc.eq.13141516), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
    call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Dual Component E blocking Run(phase=2) with timeout shorter than Actual Component delay"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  timeout = 1 ! 1s timeout
  call ESMF_GridCompRun(dualCompE, phase=2, timeout=timeout, &
    timeoutFlag=timeoutFlag, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "timeoutFlag wrong" 
  if (ESMF_GridCompIsPetLocal(dualCompE)) then
    call ESMF_Test(timeoutFlag, name, failMsg, result, ESMF_SRCLINE)
  else
    call ESMF_Test(.not.timeoutFlag, name, failMsg, result, ESMF_SRCLINE)
  endif
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! - a wait on the outstanding Run with sufficient timeout will succeed
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Waiting for outstanding Run on dual E with sufficient timeout"
  timeout = 5 ! 5s timeout
  call ESMF_GridCompWait(dualCompE, timeout=timeout, userRc=userRc, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! - the userRc of the successful wait call will be correct
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Checking userRc after wait connects to outstanding Run for pair E"
  if (ESMF_GridCompIsPetLocal(dualCompE)) then
    write(failMsg, *) "Did not return 27282920 in userRc" 
    call ESMF_Test((userRc.eq.27282920), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
    call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  endif
  !------------------------------------------------------------------------

  ! - repeat timeout between blocking dual component and actual component
  ! - let garbage collection clean up the mess this time
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Dual Component E blocking Finalize with timeout shorter than Actual Component delay"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  timeout = 1 ! 1s timeout
  call ESMF_GridCompFinalize(dualCompE, timeout=timeout, &
    timeoutFlag=timeoutFlag, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "timeoutFlag wrong" 
  if (ESMF_GridCompIsPetLocal(dualCompE)) then
    call ESMF_Test(timeoutFlag, name, failMsg, result, ESMF_SRCLINE)
  else
    call ESMF_Test(.not.timeoutFlag, name, failMsg, result, ESMF_SRCLINE)
  endif
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! --- don't destroy Es in order to test that automatic garbage collection
  ! --- during ESMF_Finalize() correctly terminates E's ServiceLoop & cleans up

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
#else
  write(name, *) "Dummy test to satisfy scripts for ESMF_TESTCOMPTUNNEL=OFF"
  write(failMsg, *) "Did not succeed" 
  do i=1,56
    call ESMF_Test((.true.), name, failMsg, result, ESMF_SRCLINE)
  enddo
#endif
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
10 continue
!-------------------------------------------------------------------------------  
! End of exhaustive block  
!-------------------------------------------------------------------------------  
!-------------------------------------------------------------------------------  
#endif

  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

end program ESMF_CompTunnelUTest
