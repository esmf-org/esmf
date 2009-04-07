! $Id: ESMF_SeqEnsemExSTest.F90,v 1.5 2009/04/07 05:34:49 theurich Exp $
!
!-------------------------------------------------------------------------
!ESMF_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!
! !DESCRIPTION:
! System test ESMF_SeqEnsemEx.  
!    The purpose of this system test is to demonstrate how a sequencial
!    ensemble example can be written using the ESMF.
!
!    Three Components A, B, and C  are created with different initial
!    conditons. A Coupler component couples the results of the three 
!    Components to Component D. Component D averages and prints out 
!    the results.
!    
!    The following sequential loop is repeated five times using a clock.
!    Component A runs increments its initial conditions and stores it
!    in its output State.
!    The Coupler Component runs taking Component's A output State 
!    value, squaring it and storing the results in Component's D
!    input State.
!    Component B runs increments its initial conditions and stores it
!    in its output State.
!    The Coupler Component runs taking Component's B output State 
!    value, squaring it and storing the results in Component's D
!    input State.
!    Component C runs increments its initial conditions and stores it
!    in its output State.
!    The Coupler Component runs taking Component's C output State 
!    value, squaring it and storing the results in Component's D
!    input State.
!    Component D runs reads the data from its input State averages and
!    prints out the results.
!    
!    In the finalize step all components verify that their States contain
!    the expected results.
!
!\begin{verbatim}

program ESMF_SeqEnsemExSTest
#define ESMF_METHOD "program ESMF_SeqEnsemExSTest"

#include "ESMF.h"

  ! ESMF Framework module
  use ESMF_Mod
  use ESMF_TestMod

  use user_modelA, only : userA_setvm, userA_register
  use user_modelB, only : userB_setvm, userB_register
  use user_modelC, only : userC_setvm, userC_register
  use user_modelD, only : userD_setvm, userD_register
  use user_coupler, only : usercpl_setvm, usercpl_register

  implicit none
    
  ! Local variables
  integer :: localPet, petCount, rc
  character(len=ESMF_MAXSTR) :: componentA, componentB, componentC, componentD, coupler
  type(ESMF_VM):: vm
  type(ESMF_State) :: compAexp, compBexp, compCexp, compDimp
  type(ESMF_GridComp) :: compA, compB, compC, compD
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

  print *, "--------------------------------------- "
  print *, "Start of System Test SeqEnsemEx: "
  print *, "--------------------------------------- "

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
  ! Initialize framework and get back default global VM
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10

  ! Get number of PETs we are running with
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10

  ! Create the 4 model components and coupler
  componentA = "user model A"
  compA = ESMF_GridCompCreate(name=componentA, rc=rc)
  print *, "Created component ", trim(componentA), " rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
    !call ESMF_GridCompPrint(compA, "", rc=rc)

  componentB = "user model B"
  compB = ESMF_GridCompCreate(name=componentB, rc=rc)
  print *, "Created component ", trim(componentB), " rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
    !call ESMF_GridCompPrint(compB, "", rc=rc)

  componentC = "user model C"
  compC = ESMF_GridCompCreate(name=componentC, rc=rc)
  print *, "Created component ", trim(componentC), " rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
    !call ESMF_GridCompPrint(compC, "", rc=rc)

  componentD = "user model D"
  compD = ESMF_GridCompCreate(name=componentD, rc=rc)
  print *, "Created component ", trim(componentD), " rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
    !call ESMF_GridCompPrint(compC, "", rc=rc)

  coupler = "user one-way coupler"
  cpl = ESMF_CplCompCreate(name=coupler, rc=rc)
  print *, "Created component ", trim(coupler), ", rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
  !  call ESMF_CplCompPrint(coupler, "", rc=rc)

  print *, "Comp Creates finished"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompSetVM(compA, userRoutine=userA_setvm, rc=rc)
  print *, "CompA SetVM finished, rc= ", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
  call ESMF_GridCompSetServices(compA, userRoutine=userA_register, rc=rc)
  print *, "CompA SetServices finished, rc= ", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

  call ESMF_GridCompSetVM(compB, userRoutine=userB_setvm, rc=rc)
  print *, "CompB SetVM finished, rc= ", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
  call ESMF_GridCompSetServices(compB, userRoutine=userB_register, rc=rc)
  print *, "CompB SetServices finished, rc= ", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

  call ESMF_GridCompSetVM(compC, userRoutine=userC_setvm, rc=rc)
  print *, "CompA SetVM finished, rc= ", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
  call ESMF_GridCompSetServices(compC, userRoutine=userC_register, rc=rc)
  print *, "CompC SetServices finished, rc= ", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

  call ESMF_GridCompSetVM(compD, userRoutine=userD_setvm, rc=rc)
  print *, "CompD SetVM finished, rc= ", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
  call ESMF_GridCompSetServices(compD, userRoutine=userD_register, rc=rc)
  print *, "CompD SetServices finished, rc= ", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

  call ESMF_CplCompSetVM(cpl, userRoutine=usercpl_setvm, rc=rc)
  print *, "Cpl SetVM finished, rc= ", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
  call ESMF_CplCompSetServices(cpl, userRoutine=usercpl_register, rc=rc)
  print *, "Cpl SetServices finished, rc= ", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Create and initialize a clock.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      ! initialize calendar to be Gregorian type
      gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize time interval to 1 hour
      call ESMF_TimeIntervalSet(timeStep, h=1, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize start time to 1January2007, 9:00 am
      call ESMF_TimeSet(startTime, yy=2007, mm=1, dd=1, h=9, &
                        calendar=gregorianCalendar, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize stop time to 1January2007, 2:00 pm
      ! to keep runtime down
      call ESMF_TimeSet(stopTime, yy=2007, mm=1, dd=1, h=14, &
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
 
  ! Create the export State for Component A
  compAexp = ESMF_StateCreate("compA export", ESMF_STATE_EXPORT, rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10
  call ESMF_GridCompInitialize(compA, exportState=compAexp, rc=rc)
  print *, "Comp A Initialize finished, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
 
  ! Create the export State for Component B
  compBexp = ESMF_StateCreate("compB export", ESMF_STATE_EXPORT, rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10
  call ESMF_GridCompInitialize(compB, exportState=compBexp, rc=rc)
  print *, "Comp B Initialize finished, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
 
  ! Create the export State for Component C
  compCexp = ESMF_StateCreate("compC export", ESMF_STATE_EXPORT, rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10
  call ESMF_GridCompInitialize(compC, exportState=compCexp, rc=rc)
  print *, "Comp C Initialize finished, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
 
  ! Create the import State for Component D
  compDimp = ESMF_StateCreate("compD import", ESMF_STATE_IMPORT, rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10
  call ESMF_GridCompInitialize(compD, importState=compDimp, rc=rc)
  print *, "Comp D Initialize finished, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
 
  ! initialize the coupler information going from Components A,
  ! B and C to Component D.
  call ESMF_CplCompInitialize(cpl, compDimp, compAexp, rc=rc)
  call ESMF_CplCompInitialize(cpl, compDimp, compBexp, rc=rc)
  call ESMF_CplCompInitialize(cpl, compDimp, compCexp, rc=rc)
  print *, "Coupler Initialize finished, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  print *, "Run Loop Start time"
  call ESMF_ClockPrint(clock, "currtime string", rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10

  do while (.not. ESMF_ClockIsStopTime(clock, rc=rc))

	! Run Component A
  	call ESMF_GridCompRun(compA, exportState=compAexp, rc=rc)
  	print *, "Comp 1 Run returned, rc =", rc
  	if (rc .ne. ESMF_SUCCESS) goto 10
	
	! Couple the export State of Component A to import State of Component D
  	call ESMF_CplCompRun(cpl, compDimp, compAexp, rc=rc)
  	print *, "Coupler Run returned, rc =", rc
  	if (rc .ne. ESMF_SUCCESS) goto 10
	
	! Run Component B
  	call ESMF_GridCompRun(compB, exportState=compBexp, rc=rc)
  	print *, "Comp B Run returned, rc =", rc
  	if (rc .ne. ESMF_SUCCESS) goto 10
	 
	! Couple the export State of Component B to import State of Component D
  	call ESMF_CplCompRun(cpl, compDimp, compBexp, rc=rc)
  	print *, "Coupler Run returned, rc =", rc
  	if (rc .ne. ESMF_SUCCESS) goto 10

	! Run Component C
  	call ESMF_GridCompRun(compC, exportState=compCexp, rc=rc)
  	print *, "Comp B Run returned, rc =", rc
  	if (rc .ne. ESMF_SUCCESS) goto 10
 
	! Couple the export State of Component C to import State of Component D
  	call ESMF_CplCompRun(cpl, compDimp, compCexp, rc=rc)
  	print *, "Coupler Run returned, rc =", rc
  	if (rc .ne. ESMF_SUCCESS) goto 10

	! Run Component D
  	call ESMF_GridCompRun(compD, importState=compDimp, rc=rc)
  	print *, "Comp D Run returned, rc =", rc
  	if (rc .ne. ESMF_SUCCESS) goto 10

        ! Advance the time
        call ESMF_ClockAdvance(clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
        !call ESMF_ClockPrint(clock, "currtime string", rc=rc)


  enddo
  print *, "Run Loop End time"
  call ESMF_ClockPrint(clock, "currtime string", rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10


 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompFinalize(compA, exportState=compAexp, rc=rc)
  print *, "Comp A Finalize finished, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

  call ESMF_GridCompFinalize(compB, exportState=compBexp, rc=rc)
  print *, "Comp B Finalize finished, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

  call ESMF_GridCompFinalize(compC, exportState=compCexp, rc=rc)
  print *, "Comp C Finalize finished, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

  call ESMF_GridCompFinalize(compD, importState=compDimp, rc=rc)
  print *, "Comp D Finalize finished, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

  call ESMF_CplCompFinalize(cpl, importState=compDimp, exportState=compAexp,  clock=clock, rc=rc)
  print *, "Coupler Finalize finished, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_StateDestroy(compAexp, rc=rc)
  call ESMF_StateDestroy(compBexp, rc=rc)
  call ESMF_StateDestroy(compCexp, rc=rc)
  call ESMF_StateDestroy(compDimp, rc=rc)

  call ESMF_GridCompDestroy(compA, rc=rc)
  call ESMF_GridCompDestroy(compB, rc=rc)
  call ESMF_GridCompDestroy(compC, rc=rc)
  call ESMF_GridCompDestroy(compD, rc=rc)
  call ESMF_CplCompDestroy(cpl, rc=rc)

  print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

10 continue
  print *, "System Test ESMF_SeqEnsemEx complete."

  ! Normal ESMF Test output
  write(failMsg, *) "System Test failure"
  write(testname, *) "System Test ESMF_SeqEnsemEx"

  call ESMF_TestGlobal((rc.eq.ESMF_SUCCESS), testname, failMsg, testresult, &
    ESMF_SRCLINE)

  print *, "------------------------------------------------------------"
  print *, "------------------------------------------------------------"
  print *, "Test finished, localPet = ", localPet
  print *, "------------------------------------------------------------"
  print *, "------------------------------------------------------------"

  call ESMF_Finalize()

end program ESMF_SeqEnsemExSTest
    
!\end{verbatim}
    
