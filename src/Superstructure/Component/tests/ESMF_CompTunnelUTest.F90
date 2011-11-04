! $Id: ESMF_CompTunnelUTest.F90,v 1.8 2011/11/04 22:21:01 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research,
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
#define SLEEPTIME 2

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
    
    ! local variables
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name

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
    
    ! local variables
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name

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
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name

    ! Initialize
    rc = ESMF_SUCCESS
    
    call ESMF_LogWrite("Actual Component Run", ESMF_LOGMSG_INFO, rc=rc)
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
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
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
    call ESMF_VMWtimeDelay(SLEEPTIME._ESMF_KIND_R8) ! sleep a few seconds
    
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
    '$Id: ESMF_CompTunnelUTest.F90,v 1.8 2011/11/04 22:21:01 theurich Exp $'
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
  actualComp = ESMF_GridCompCreate(petList=petList, rc=rc)
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
  call ESMF_GridCompSetServices(actualComp, userRoutine=setservices, userRc=userRc, rc=rc)
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
  dualComp = ESMF_GridCompCreate(petList=petList, rc=rc)
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
  !NEX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ServiceLoop for the Actual Component"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompServiceLoop(actualComp, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !NEX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
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
    call ESMF_Test(delayTime > SLEEPTIME._ESMF_KIND_R8-precTime, name, failMsg, result, ESMF_SRCLINE)
  else
    ! PETs not in Dual Component petList must not be blocking
    call ESMF_Test(delayTime < SLEEPTIME._ESMF_KIND_R8+precTime, name, failMsg, result, ESMF_SRCLINE)
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
  call ESMF_Test(delayTime < SLEEPTIME._ESMF_KIND_R8+precTime, name, failMsg, result, ESMF_SRCLINE)
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
    call ESMF_Test(delayTime > SLEEPTIME._ESMF_KIND_R8-precTime, name, failMsg, result, ESMF_SRCLINE)
  else
    ! PETs not in Dual Component petList must not be blocking
    call ESMF_Test(delayTime < SLEEPTIME._ESMF_KIND_R8+precTime, name, failMsg, result, ESMF_SRCLINE)
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
  
  if (petCount /= 8) goto 10  ! skip all of the exhaustive tests of not on 8PET
  
  !------------------------------------------------------------------------
  ! The exhaustive tests break into three dual-actual pairs: A, B, C. The dual,
  ! i.e. front-end component for all these pairs is always defined on the same
  ! PETs, but the actual components are on different PETs. Calling into the
  ! dual components with non-blocking syncflag thus allows concurrent execution
  ! of the actual components. The layout across the 8 PETs is as follows:
  !
  !   PET:        0     1     2     3     4     5     6     7
  !   pair-A:           dual  act.  act.  dual        act.
  !   pair-B:     act.  dual              dual
  !   pair-C:           dual              dual  act.        act.
  !------------------------------------------------------------------------
  
  ! --- create A's ---

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
  actualCompA = ESMF_GridCompCreate(petList=petList, rc=rc)
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
  call ESMF_GridCompSetServices(actualCompA, userRoutine=setservices, userRc=userRc, rc=rc)
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
  dualCompA = ESMF_GridCompCreate(petList=petList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  deallocate(petList)

  ! --- create B's ---

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
  actualCompB = ESMF_GridCompCreate(petList=petList, rc=rc)
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
  call ESMF_GridCompSetServices(actualCompB, userRoutine=setservices, userRc=userRc, rc=rc)
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
  dualCompB = ESMF_GridCompCreate(petList=petList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  deallocate(petList)

  ! --- create C's ---

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
  actualCompC = ESMF_GridCompCreate(petList=petList, rc=rc)
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
  call ESMF_GridCompSetServices(actualCompC, userRoutine=setservices, userRc=userRc, rc=rc)
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
  dualCompC = ESMF_GridCompCreate(petList=petList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  deallocate(petList)

  ! --- connect A's ---

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ServiceLoop for the Actual Component A"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompServiceLoop(actualCompA, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Dual Component A on all PETs"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetServices(dualCompA, actualGridcomp=actualCompA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! --- connect B's ---

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Dual Component B on all PETs"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetServices(dualCompB, actualGridcomp=actualCompB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ServiceLoop for the Actual Component B"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompServiceLoop(actualCompB, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! --- connect C's ---

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "SetServices for the Dual Component C on all PETs"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompSetServices(dualCompC, actualGridcomp=actualCompC, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ServiceLoop for the Actual Component C"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompServiceLoop(actualCompC, userRc=userRc, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS in userRc" 
  call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
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

  ! --- destroy A's ---
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Destroy the Dual Component A"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompDestroy(dualCompA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Destroy the Actual Component A"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompDestroy(actualCompA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  ! --- destroy B's ---
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Destroy the Actual Component B (before Dual B)"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompDestroy(actualCompB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Destroy the Dual Component B (after Actual B) - testing integrated wait mechanism"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridCompDestroy(dualCompB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! --- don't destroy C's in order to test that automatic garbage collection
  ! --- during ESMF_Finalize() correctly terminates C's ServiceLoop

10 continue
!-------------------------------------------------------------------------------  
! End of exhaustive block  
!-------------------------------------------------------------------------------  
!-------------------------------------------------------------------------------  
#endif

  !------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

end program ESMF_CompTunnelUTest
