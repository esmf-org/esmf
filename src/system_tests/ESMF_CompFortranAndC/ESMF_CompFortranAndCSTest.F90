! $Id: ESMF_CompFortranAndCSTest.F90,v 1.17 2009/10/23 17:44:38 theurich Exp $
!
!-------------------------------------------------------------------------
!ESMF_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!-------------------------------------------------------------------------
! !DESCRIPTION:
! System test CompFortranAndC.
! This system test checks that states are transfered accurately between
! components that are implemented in different languages (Fortran and C).
! Two components are created by the driver code and their SetServices()
! are invoked.
!  The rest of the code works on an array within a specific state that is 
!  on turns modified by one component followed by the other component 
!  verifying those changes. Specifically on,
!
!  "Init section":
!  --The Fortran Component adds an array to the export state and initializes 
!    its data.
!  --The C Component re-initializes the data values of the same state array.
!
!  "Run section":
!  --The Fortran Component first verifies the array values just initialized by the C
!    component, and then modifies it again before returning.
!  --The C component verifies the array values just modifed by the Fortran 
!    component and returns.
!
!  "Finalize section":
!  --The Fortran component cleans up the state contents (i.e. it Destroys the
!    Array object and deallocates the Fortran array it points to).
!
!-------------------------------------------------------------------------
!\begin{verbatim}

program ESMF_CompFortranAndC
#define ESMF_METHOD "program ESMF_CompFortranAndC"

#include "ESMF.h"

  ! ESMF Framework module
  use ESMF_Mod
  use ESMF_TestMod
  use ESMF_CompMod
    
  use user_FortranComponent, only: mySetVMInFortran, mySetServicesInFortran

  implicit none

  ! Explicit interface for 
  interface 
    subroutine my_SetServicesInC(gcomp, rc)
      use ESMF_Mod
      type(ESMF_GridComp) :: gcomp
      integer, intent(out) :: rc
    end subroutine my_SetServicesInC
  end interface
    
  ! Local variables
  integer :: localPet, localrc, rc=ESMF_SUCCESS, userrc
  type(ESMF_VM):: vm
  type(ESMF_GridComp) :: compInFortran
  type(ESMF_GridComp) :: compInC
  type(ESMF_State) :: imp, exp
  character(len=ESMF_MAXSTR) :: cname
        
  ! Variables related to the Clock
  type(ESMF_Clock) :: clock
  type(ESMF_TimeInterval) :: timeStep
  type(ESMF_Time) :: startTime
  type(ESMF_Time) :: stopTime

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test name
  character(ESMF_MAXSTR) :: testname

  ! individual test failure message and status msg
  character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "System Test failure"
  write(testname, *) "System Test ESMF_CompFortranAndC"

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

  call ESMF_Initialize(defaultCalendar=ESMF_CAL_GREGORIAN, &
    defaultlogfilename="CompFortranAndCSTest.Log", &
    defaultlogtype=ESMF_LOG_MULTI, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Get the global VM
  call ESMF_VMGetGlobal(vm, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Get our pet number for output print statements
  call ESMF_VMGet(vm, localPet=localPet, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  cname = "Fortran Component"
  compInFortran = ESMF_GridCompCreate(name=cname, gridcompType=ESMF_ATM, &
    rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  print *, "Comp Create (Fortran) finished, name = ", trim(cname)

  cname = "C Component"
  compInC = ESMF_GridCompCreate(name=cname, gridcompType=ESMF_ATM, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  print *, "Comp Create (C) finished, name = ", trim(cname)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompSetVM(compInFortran, userRoutine=mySetVMInFortran, &
    userRc=userrc, rc=localrc)
  print *, "CompInFortran SetVM finished, rc= ", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_GridCompSetServices(compInFortran, &
    userRoutine=mySetServicesInFortran, userRc=userrc, rc=localrc)
  print *, "CompInFortran SetServices finished, rc= ", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridCompSetServices(compInC, userRoutine=my_SetServicesInC, &
    userRc=userrc, rc=localrc)
  print *, "CompInC SetServices finished, rc= ", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

!------------------------------------------------------------------------------
! Create and initialize a Clock.
!------------------------------------------------------------------------------

  call ESMF_TimeIntervalSet(timeStep, s=2, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  print *, "Time Interval set"

  call ESMF_TimeSet(startTime, yy=2004, mm=9, dd=25, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  print *, "Start Time set"

  call ESMF_TimeSet(stopTime, yy=2004, mm=9, dd=26, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  print *, "Stop Time set"

  clock = ESMF_ClockCreate("Application Clock", timeStep=timeStep, &
    startTime=startTime, stopTime=stopTime, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  print *, "Clock created"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
  imp = ESMF_StateCreate("igrid import state", ESMF_STATE_IMPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
    
  exp = ESMF_StateCreate("igrid export state", ESMF_STATE_EXPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridCompInitialize(compInFortran, importState=imp, &
    exportState=exp, clock=clock, userRc=userrc, rc=localrc)
  print *, "CompInFortran Initialize returned, rc=", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
 
  call ESMF_GridCompInitialize(compInC, importState=imp, &
    exportState=exp, clock=clock, userRc=userrc, rc=localrc)
  print *, "CompInC Initialize returned, rc=", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompRun(compInFortran, importState=imp, &
    exportState=exp, clock=clock, userRc=userrc, rc=localrc)
  print *, "CompInFortran Run returned, rc=", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
    
  call ESMF_GridCompRun(compInC, importState=imp, &
    exportState=exp, clock=clock, userRc=userrc, rc=localrc)
  print *, "CompInC Run returned, rc=", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompFinalize(compInFortran, importState=imp, &
    exportState=exp, clock=clock, userRc=userrc, rc=localrc)
  print *, "CompInFortran Finalize returned, rc=", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
    
  call ESMF_GridCompFinalize(compInC, importState=imp, &
    exportState=exp, clock=clock, userRc=userrc, rc=localrc)
  print *, "CompInC Finalize returned, rc=", localrc, userrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompDestroy(compInFortran, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridCompDestroy(compInC, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_StateDestroy(imp, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_StateDestroy(exp, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_ClockDestroy(clock, rc=localrc)
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

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors
  ! in the log file that the scripts grep for.
  call ESMF_STest((rc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize()

end program ESMF_CompFortranAndC
    
!\end{verbatim}
    
