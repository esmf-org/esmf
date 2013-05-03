! $Id$
!
!=========================================================================

#include "ESMF.h"

! The SLEEPTIME macro defines the time in seconds that Finalize will delay
#define SLEEPTIME 2

!-------------------------------------------------------------------------
!
! !DESCRIPTION:
! System test ESMF_FaultTolerance.
!    Testing fault tolerance implementation Component A
!
!-------------------------------------------------------------------------
!\begin{verbatim}

#define ESMF_METHOD "module ESMF_FaultToleranceCompASTest_comp_mod"
module ESMF_FaultToleranceCompASTest_comp_mod

  ! modules
  use ESMF_TestMod     ! test methods
  use ESMF
  
  implicit none
  
  private
  
  public setservices

  contains !--------------------------------------------------------------------

  recursive subroutine setservices(gcomp, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
    
    ! Initialize
    rc = ESMF_SUCCESS

    ! register RUN method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=initialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, phase=2, &
      userRoutine=run2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      return  ! bail out

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
      ESMF_CONTEXT, rcToReturn=rc)) &
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
      ESMF_CONTEXT, rcToReturn=rc)) &
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
      ESMF_CONTEXT, rcToReturn=rc)) &
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
      ESMF_CONTEXT, rcToReturn=rc)) &
      return  ! bail out
      
    ! In order to test blocking/non-blocking dual component feature put the
    ! actual component to sleep for a few seconds:
    call ESMF_VMWtimeDelay(SLEEPTIME._ESMF_KIND_R8) ! sleep a few seconds
    
    ! Need a barrier so that time tests on the dual component PETs will be
    ! as expected. This is only for testing! Generally this kind of
    ! synchronization is not desirable.
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_VMBarrier(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      return  ! bail out

    call ESMF_LogWrite("Actual Component exit Finalize after sleep", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      return  ! bail out
    
  end subroutine !--------------------------------------------------------------

end module
#undef ESMF_METHOD

!==============================================================================
!==============================================================================
!==============================================================================

#define ESMF_METHOD "program ESMF_FaultToleranceCompASTest"
program ESMF_FaultToleranceCompASTest

  ! ESMF Framework module
  use ESMF
  use ESMF_TestMod

  use ESMF_FaultToleranceCompASTest_comp_mod, only: setservices

  implicit none

  ! Local variables
  integer             :: localPet, petCount, userRc, localrc, rc
  type(ESMF_VM)       :: vm
  type(ESMF_GridComp) :: actualComp

  character(ESMF_MAXSTR) :: testname, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(testname, *) "System Test ESMF_FaultToleranceCompA STest"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  print *, "--------------------------------------- "
  print *, "Start of ", trim(testname)
  print *, "--------------------------------------- "

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Initialize ESMF
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
  ! Initialize framework and get back default global VM
  call ESMF_Initialize(vm=vm, defaultlogfilename="FaultToleranceCompASTest.Log", &
    logkindflag=ESMF_LOGKIND_MULTI, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_LogSet(flush=.true., rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  ! Get number of PETs we are running with
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
  
  ! - Sanity test the socket based connection 
  ! - TODO: remove
  
!  if (localPet == petCount-1) then
!    call c_ESMCI_vmkSocketServer(localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) &
!      call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
!  endif
  
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  ! - This is the top level driver cap for an actual component running
  ! - remotely as an independent executable.
  
  ! - Create the actual component and go into the ServiceLoop, finally
  ! - destroy the component after returning from the ServiceLoop.
  
  actualComp = ESMF_GridCompCreate(name="actual component A", rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(actualComp, userRoutine=setservices, &
    userRc=userRc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userRc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompServiceLoop(actualComp, port=60000, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompDestroy(actualComp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  ! Normal ESMF Test output
  print *, testname, " complete."

  ! Separate message to console, for quick confirmation of success/failure
  write(finalMsg, *) "SUCCESS: ",trim(testname)," finished correctly."
  write(0, *) ""
  write(0, *) trim(testname)
  write(0, *) trim(finalMsg)
  write(0, *) ""

  print *, "------------------------------------------------------------"
  print *, "------------------------------------------------------------"
  print *, "Test finished, localPet = ", localPet
  print *, "------------------------------------------------------------"
  print *, "------------------------------------------------------------"

  call ESMF_Finalize()

end program ESMF_FaultToleranceCompASTest

!\end{verbatim}
