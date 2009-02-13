! $Id: ESMF_InternalStateEnsembleSTest.F90,v 1.2 2009/02/13 00:54:37 theurich Exp $
!
!-------------------------------------------------------------------------
!ESMF_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!-------------------------------------------------------------------------
!
! !DESCRIPTION:
! System test InternalStateEnsemble.  
!    The purpose of this system test is to demonstrate how a ensemble can 
!    be written using internal States
!
!    A gridded component defines three internal States each with a
!    2D source Array 100x150. A second gridded component also defines internal
!    States with three destination Arrays also 100x150.
!
!    The coupler component reconciles import and export States which contain 
!    source and destination Arrays, respectively.    
!
!    The following concurrent loop is repeated five times using a clock.
!   
!    On the first time through the loop, Component 1 intializes it's source Array
!    in its first internal State to all ones, initializes its source Array in its
!    second internal State to all twos, and initializes its source array in its  
!    third internal State to all threes. On subsequent times through the loop it 
!    mutiplies the Array elements in each internal State by 10. It stores the Arrays
!    in its export State.
!
!    The Coupler Component redistribures the Arrays in the export State
!    to the import State of the second gridded Component by calling ArrayRedist.
!    Finally the second gridded component compares the data stored in the
!    destination Arrays to the expected values as a measure of the accuracy 
!    of the ArrayRedist() method.

!
!-------------------------------------------------------------------------
!\begin{verbatim}

program ESMF_InternalStateEnsembleSTest
#define ESMF_METHOD "program ESMF_InternalStateEnsembleSTest"

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
  type(ESMF_State) :: c1exp
  type(ESMF_State) :: c2imp
  type(ESMF_GridComp) :: comp1, comp2
  type(ESMF_CplComp) :: cpl


  ! instantiate a clock, a calendar, and timesteps
  type(ESMF_Clock) :: clock
  type(ESMF_Calendar) :: gregorianCalendar
  type(ESMF_TimeInterval) :: timeStep
  type(ESMF_Time) :: startTime
  type(ESMF_Time) :: stopTime



  ! cumulative result: count failures; no failures equals "all pass"
  integer :: testresult = 0

  ! individual test name
  character(ESMF_MAXSTR) :: testname

  ! individual test failure message, and final status msg
  character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "System Test failure"
  write(testname, *) "System Test ESMF_InternalStateEnsembleSTest"

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


  ! Create the 2 model components and coupler either for multiprocessor or uni
  cname1 = "user model 1"
  comp1 = ESMF_GridCompCreate(name=cname1, rc=localrc)
  print *, "Created component ", trim(cname1), "rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  cname2 = "user model 2"
  comp2 = ESMF_GridCompCreate(name=cname2, rc=localrc)
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

  call ESMF_GridCompSetServices(comp1, routine=userm1_register, rc=localrc)
  print *, "Comp SetServices finished, rc= ", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_GridCompSetServices(comp2, routine=userm2_register, rc=localrc)
  print *, "Comp SetServices finished, rc= ", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_CplCompSetServices(cpl, routine=usercpl_register, rc=localrc)
  print *, "Comp SetServices finished, rc= ", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Create and initialize a clock.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      ! initialize calendar to be Gregorian type
      gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize time interval to 1 hour
      call ESMF_TimeIntervalSet(timeStep, h=1, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize start time to January 1, 2009, 9:00 am
      call ESMF_TimeSet(startTime, yy=2009, mm=1, dd=1, h=9, &
                        calendar=gregorianCalendar, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize stop time to January 1, 2009, 2:00 pm
      ! to keep runtime down
      call ESMF_TimeSet(stopTime, yy=2009, mm=1, dd=1, h=14, &
                        calendar=gregorianCalendar, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize the clock with the above values
      clock = ESMF_ClockCreate("Clock 1", timeStep, startTime, stopTime, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10



!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
  c1exp = ESMF_StateCreate("comp1 export", ESMF_STATE_EXPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_GridCompInitialize(comp1, exportState=c1exp, rc=localrc)
  print *, "Comp 1 Initialize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
 
  c2imp = ESMF_StateCreate("comp2 import", ESMF_STATE_IMPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_GridCompInitialize(comp2, importState=c2imp, rc=localrc)
  print *, "Comp 2 Initialize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  ! note that the coupler's import is comp1 export states
  ! and coupler's export is comp2's import states
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

  print *, "Run Loop Start time"
  call ESMF_ClockPrint(clock, "currtime string", rc)
  if (rc .ne. ESMF_SUCCESS) goto 10

  do while (.not. ESMF_ClockIsStopTime(clock, rc))


  	call ESMF_GridCompRun(comp1, exportState=c1exp, clock=clock, rc=localrc)
  	print *, "Comp 1 Run returned, rc =", localrc
  	if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  	call ESMF_CplCompRun(cpl, c1exp, c2imp, clock=clock, rc=localrc)
  	print *, "Coupler Run with c1exp returned, rc =", localrc
  	if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  	call ESMF_GridCompRun(comp2, importState=c2imp, clock=clock, rc=localrc)
  	print *, "Comp 2 Run with c2imp returned, rc =", localrc
  	if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
 
        ! Advance the time
        call ESMF_ClockAdvance(clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
        !call ESMF_ClockPrint(clock, "currtime string", rc)


  enddo
  print *, "Run Loop End time"
  call ESMF_ClockPrint(clock, "currtime string", rc)
  if (rc .ne. ESMF_SUCCESS) goto 10

 
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

  call ESMF_GridCompFinalize(comp1, exportState=c1exp, rc=localrc)
  print *, "Comp 1 Finalize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_GridCompFinalize(comp2, importState=c2imp, rc=localrc)
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

end program ESMF_InternalStateEnsembleSTest
    
!\end{verbatim}
