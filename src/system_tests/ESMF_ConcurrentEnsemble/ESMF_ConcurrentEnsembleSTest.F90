! $Id: ESMF_ConcurrentEnsembleSTest.F90,v 1.5 2009/02/25 20:18:33 svasquez Exp $
!
!-------------------------------------------------------------------------
!ESMF_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!-------------------------------------------------------------------------
!
! !DESCRIPTION:
! System test ConcurrentEnsemble.  
!    The purpose of this system test is to demonstrate how a concurrent
!    ensemble can be written using the ESMF using different initial 
!    conditions.
!
!    Three gridded components running on 2 independent PETs each define a 
!    2D source Array 100x150. The fourth gridded component defines three
!    destination Arrays also 100x150 and also runs on 2 PETs. None of the components
!    have any PETs in common to allow them to run concurrently
!
!    The coupler component runs on all 8 PETs and reconciles import and export
!    States which contain source and destination Arrays, respectively.  
!
!    The following concurrent loop is repeated five times using a clock.
!   
!    On the first time through the loop, Component 1 intializes it's source Array
!    to all ones. On subsequent times through the loop it mutiplies the Array
!    elements by 10. It stores the Array in its export State.
!
!    On the first time through the loop, Component 2 intializes it's source Array
!    to all twos. On subsequent times through the loop it mutiplies the Array
!    elements by 10. It stores the Array in its export State.
!
!    On the first time through the loop, Component 3 intializes it's source Array
!    to all three. On subsequent times through the loop it mutiplies the Array
!    elements by 10. It stores the Array in its export State.
!    
!    The Coupler Component redistribures the Arrays in the three export States
!    to the three import States of Component 4 by calling ArrayRedist.
!    Finally the fourth gridded component compares the data stored in the
!    destination Arrays to the expected values as a measure of the accuracy 
!    of the ArrayRedist() method.
!
!    On the first time through the loop, the first 3 components run concurrently,
!    on subsequent times all the components run concurrently.
!
!    Note: This system test runs on 8 or a single PET. When run on a single PET
!          the test runs sequentially.
!
!-------------------------------------------------------------------------
!\begin{verbatim}

program ESMF_ConcurrentEnsembleSTest
#define ESMF_METHOD "program ESMF_ConcurrentEnsembleSTest"

#include "ESMF.h"

  ! ESMF Framework module
  use ESMF_Mod
  use ESMF_TestMod

  use user_model1, only : userm1_register
  use user_model2, only : userm2_register
  use user_model3, only : userm3_register
  use user_model4, only : userm4_register
  use user_coupler, only : usercpl_register

  implicit none
    
  ! Local variables
  integer :: localPet, petCount, localrc, rc=ESMF_SUCCESS
  character(len=ESMF_MAXSTR) :: cname1, cname2, cname3, cname4, cplname
  type(ESMF_VM):: vm
  type(ESMF_State) :: c1exp, c2exp, c3exp
  type(ESMF_State) :: c4imp1, c4imp2, c4imp3
  type(ESMF_GridComp) :: comp1, comp2, comp3, comp4
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
  write(testname, *) "System Test ESMF_ConcurrentEnsembleSTest"

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


  ! Create the 4 model components and coupler either for multiprocessor or uni
  cname1 = "user model 1"
  if (petCount .lt. 8) then
	comp1 = ESMF_GridCompCreate(name=cname1, rc=localrc)
  else
	! use petList to define comp1 on PET 0,1
  	comp1 = ESMF_GridCompCreate(name=cname1, petList=(/0,1/), rc=localrc)
  endif
  print *, "Created component ", trim(cname1), "rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  cname2 = "user model 2"
  if (petCount .lt. 8) then
	comp2 = ESMF_GridCompCreate(name=cname2, rc=localrc)
  else
  	! use petList to define comp2 on PET 2,3
  	comp2 = ESMF_GridCompCreate(name=cname2, petList=(/2,3/), rc=localrc)
  endif
  print *, "Created component ", trim(cname2), "rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  cname3 = "user model 3"
  if (petCount .lt. 8) then
	comp3 = ESMF_GridCompCreate(name=cname3, rc=localrc)
  else
  	! use petList to define comp3 on PET 4,5
  	comp3 = ESMF_GridCompCreate(name=cname3, petList=(/4,5/), rc=localrc)
  endif
  print *, "Created component ", trim(cname3), "rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  cname4 = "user model 4"
  if (petCount .lt. 8) then
	comp4 = ESMF_GridCompCreate(name=cname4, rc=localrc)
  else
  	! use petList to define comp4 on PET 6,7
  	comp4 = ESMF_GridCompCreate(name=cname4, petList=(/6,7/), rc=localrc)
  endif
  print *, "Created component ", trim(cname4), "rc =", localrc
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

  call ESMF_GridCompSetServices(comp3, routine=userm3_register, rc=localrc)
  print *, "Comp SetServices finished, rc= ", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_GridCompSetServices(comp4, routine=userm4_register, rc=localrc)
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
 
  c2exp = ESMF_StateCreate("comp2 export", ESMF_STATE_EXPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_GridCompInitialize(comp2, exportState=c2exp, rc=localrc)
  print *, "Comp 2 Initialize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
 
  c3exp = ESMF_StateCreate("comp3 export", ESMF_STATE_EXPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_GridCompInitialize(comp3, exportState=c3exp, rc=localrc)
  print *, "Comp 3 Initialize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
 
  c4imp1 = ESMF_StateCreate("comp4 import1", ESMF_STATE_IMPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_GridCompInitialize(comp4, importState=c4imp1, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  c4imp2 = ESMF_StateCreate("comp4 import2", ESMF_STATE_IMPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_GridCompInitialize(comp4, importState=c4imp2, rc=localrc)
  print *, "Comp 4 Initialize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  c4imp3 = ESMF_StateCreate("comp4 import3", ESMF_STATE_IMPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_GridCompInitialize(comp4, importState=c4imp3, rc=localrc)
  print *, "Comp 4 Initialize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
 
  ! note that the coupler's import is comp1, comp2, & comp3 export states
  ! and coupler's export is comp4's import states
  call ESMF_CplCompInitialize(cpl, c1exp, c4imp1, rc=localrc)
  print *, "Coupler Initialize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
 
  call ESMF_CplCompInitialize(cpl, c2exp, c4imp2, rc=localrc)
  print *, "Coupler Initialize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
 
  call ESMF_CplCompInitialize(cpl, c3exp, c4imp3, rc=localrc)
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

  	call ESMF_GridCompRun(comp2, exportState=c2exp, clock=clock, rc=localrc)
  	print *, "Comp 2 Run returned, rc =", localrc
  	if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  	call ESMF_GridCompRun(comp3, exportState=c3exp, clock=clock, rc=localrc)
  	print *, "Comp 3 Run returned, rc =", localrc
  	if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  	call ESMF_CplCompRun(cpl, c1exp, c4imp1, clock=clock, rc=localrc)
  	print *, "Coupler Run with c1exp returned, rc =", localrc
  	if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  	call ESMF_CplCompRun(cpl, c2exp, c4imp2, clock=clock, rc=localrc)
  	print *, "Coupler Run with c2exp returned, rc =", localrc
  	if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
	
  	call ESMF_CplCompRun(cpl, c3exp, c4imp3, clock=clock, rc=localrc)
  	print *, "Coupler Run with c3exp returned, rc =", localrc
  	if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
	
  	call ESMF_GridCompRun(comp4, importState=c4imp1, clock=clock, rc=localrc)
  	print *, "Comp 4 Run with c4imp1 returned, rc =", localrc
  	if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
 
  	call ESMF_GridCompRun(comp4, importState=c4imp2, clock=clock, rc=localrc)
  	print *, "Comp 4 Run with c4imp2 returned, rc =", localrc
  	if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
 
  	call ESMF_GridCompRun(comp4, importState=c4imp3, clock=clock, rc=localrc)
  	print *, "Comp 4 Run with c4imp3 returned, rc =", localrc
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

  call ESMF_GridCompFinalize(comp1, exportState=c1exp, rc=localrc)
  print *, "Comp 1 Finalize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_GridCompFinalize(comp2, exportState=c2exp, rc=localrc)
  print *, "Comp 2 Finalize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_GridCompFinalize(comp3, exportState=c3exp, rc=localrc)
  print *, "Comp 3 Finalize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_GridCompFinalize(comp4, importState=c4imp1, rc=localrc)
  print *, "Comp 4 Finalize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_GridCompFinalize(comp4, importState=c4imp2, rc=localrc)
  print *, "Comp 4 Finalize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_GridCompFinalize(comp4, importState=c4imp3, rc=localrc)
  print *, "Comp 4 Finalize finished, rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_CplCompFinalize(cpl, c1exp, c2exp, rc=localrc)
  print *, "Coupler Finalize finished, rc =", localrc
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
  call ESMF_GridCompDestroy(comp3, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_GridCompDestroy(comp4, rc=localrc)
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
  call ESMF_StateDestroy(c2exp, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_StateDestroy(c3exp, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_StateDestroy(c4imp1, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_StateDestroy(c4imp2, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_StateDestroy(c4imp3, rc=localrc)
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

end program ESMF_ConcurrentEnsembleSTest
    
!\end{verbatim}
