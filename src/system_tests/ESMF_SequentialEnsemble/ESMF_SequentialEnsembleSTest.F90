! $Id$
!
!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!-------------------------------------------------------------------------
!
! !DESCRIPTION:
! System test Ensemble.
!    The purpose of this system test is to demonstrate how to build
!    sequential ensemble components in ESMF using different components
!    or the same component with different initial  conditions.
!
!    In this test, we have two different ensemble components, compA and compB; each one
!    creates a 2D array of size 100x150 but with different decomposition: CompA decomposes
!    the array in columns and compB decomposes the array in rows.   We create two
!    instances of each component and that makes up a total of four ensemble components.
!    The ensemble components are named A-1, A-2, B-1, B-2.  The initial values to the
!    array are perturbated by a small value passed in using ESMF_Attribute when the
!    components were created.  This allows each component to set different initial
!    values to the array.  A third gridded component, CompC is used as the composite component
!    to composite the output from the four ensemble components.
!    A couple component, Cpl, is also provided to couple the ensemble components with
!    the composite component.
!
!
!    Two techniques are used to implement the ensemble components that are instantiated from
!    the same component definition.
!    1.  Using Attributes to set different initial condition.  Since the ensemble components
!        share the same code, we will need to pass information to the component so that it
!        can be used to set different initial conditions.  Several ways can be used in ESMF
!        to achieve the goal.  One can use a different config file when the component is created.
!        In this example, we use Attribute to pass a different perturbation value to the
!        component.
!    2.  Use InternalState to pass private data values between component's functions. One way
!        to share component's private data between the init/run/finalize functions is to use
!        the module's global variables.  When the ensemble components are run in sequential mode,
!        i.e., there are multiple instances of the same component module  running at the same PETs,
!        we can no longer use module variables to share the private data.  Alternatively, ESMF provides
!        Component Internal State to serve the function.  In this example, we set different offset for
!        different ensemble components at the init function and pass the offset to the run routine using
!        the Internal State as a demonstration.  Note that you have to define a data wrapper that
!        contains a single pointer to point to a data block containing all the data to be shared
!        in between functions.
!
!    Each ensemble component exports the array using its ExportState.  Those ExportStates are added
!    into the ImportState of the Coupler.  The Coupler then creates four arrays each of size 100x150
!    but distributed across all the PETs in a block decomposition (i.e. 2x(n/2), if totalPET=n). These
!    four arrays are then added into the Coupler's ExportState. The Couple's ExportState is then
!    passed to the Composite component as its ImportState.
!
!    In Coupler's Init routine, it runs StateReconcile to each ensemble components array passed in
!    via the ImportState to populate the array information to all the PETs.  Then the Coupler
!    run an ArrayRedistStore to calculate the redistribution of the ensemble's array to the
!    corresponding destination array to be passed to the composite component.  In Coupler's run
!    routine, the couple will run ArrayRedist to redistribute the arrays.
!
!    The composite component receives the four arrays from the coupler and averages the array contents
!    and put them into a destination array with the same decomponsition as the incoming array.
!    The destination array is then exported via its ExportState.  The composite component also
!    checks the averaged values and reports the errors if any.
!
!    In each run cylce, the four ensemble components will run, followed by the coupler, that
!    performs the array redistributions for all the four incoming arrays at once, and the composite component.
!    We run the run cycles 5 times.
!
!-------------------------------------------------------------------------
!\begin{verbatim}

program ESMF_SequentialEnsembleSTest
#define ESMF_METHOD "program ESMF_SequentialEnsembleSTest"

#include "ESMF.h"

  ! ESMF Framework module
  use ESMF
  use ESMF_TestMod

  use user_modelA, only : usermA_register
  use user_modelB, only : usermB_register
  use user_modelC, only : usermC_register
  use user_coupler, only :usercpl_register

  implicit none

  ! Local variables
  integer :: localPet, petCount, userrc, localrc, rc=ESMF_SUCCESS
  character(len=ESMF_MAXSTR) :: cnameA1, cnameA2, cnameB1, cnameB2, cnameC, cplname
  type(ESMF_VM):: vm
  type(ESMF_State) :: cA1exp, cA2exp, cB1exp, cB2exp, compCexp
  type(ESMF_State) :: cplexp, cplimp
  type(ESMF_GridComp) :: compA1, compA2, compB1, compB2, compC
  type(ESMF_CplComp) :: cpl
  real(ESMF_KIND_R8)::perturb

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
  write(testname, *) "System Test ESMF_SequentialEnsembleSTest"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
  ! Initialize framework and get back default global VM
  call ESMF_Initialize(vm=vm, defaultlogfilename="SequentialEnsembleSTest.Log", &
	logkindflag=ESMF_LOGKIND_MULTI, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  ! Get number of PETs we are running with
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

   ! Check for correct number of PETs
  if ( petCount < 4 ) then
     call ESMF_LogSetError(ESMF_RC_ARG_BAD,&
         msg="This system test does not run on fewer than 4 PETs.",&
         ESMF_CONTEXT, rcToReturn=rc)
     call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
   endif

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  if (localPet == 0) then
    print *, "--------------------------------------- "
    print *, "Start of ", trim(testname)
    print *, "--------------------------------------- "
  endif

  ! Create the 4 ensemble model components, the composite component and the coupler on disjoint PETs
  ! components on all the 8 PETs.
  cnameA1 = "user model A-1"
  cnameA2 = "user model A-2"
  cnameB1 = "user model B-1"
  cnameB2 = "user model B-2"
  cnameC = "user model C"
  cplname = "user one-way coupler"

  compA1 = ESMF_GridCompCreate(name=cnameA1, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
	ESMF_CONTEXT, rcToReturn=rc)) &
	call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  compA2 = ESMF_GridCompCreate(name=cnameA2, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
	ESMF_CONTEXT, rcToReturn=rc)) &
	call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  compB1 = ESMF_GridCompCreate(name=cnameB1, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
	ESMF_CONTEXT, rcToReturn=rc)) &
	call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  compB2 = ESMF_GridCompCreate(name=cnameB2, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
	ESMF_CONTEXT, rcToReturn=rc)) &
	call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  ! create the composite composite and the coupler on all PETs
  compC = ESMF_GridCompCreate(name=cnameC, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  cpl = ESMF_CplCompCreate(name=cplname, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  if (localPet == 0)  print *, "Comp Creates finished"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  !compA1 and compA2 are two instances for user_modelA
  call ESMF_GridCompSetServices(compA1, userRoutine=usermA_register, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(compA2, userRoutine=usermA_register, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  !compB1 and compB2 are two instances for user_modelB
  call ESMF_GridCompSetServices(compB1, userRoutine=usermB_register, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(compB2, userRoutine=usermB_register, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  ! CompC is user_modelC
  call ESMF_GridCompSetServices(compC, userRoutine=usermC_register, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_CplCompSetServices(cpl, userRoutine=usercpl_register, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  if (localPet == 0) print *, "Comp SetServices finished"

!------------------------------------------------------------------------------
! Set different attribute to distinguish the two instances from the same component
!------------------------------------------------------------------------------
perturb=1
call ESMF_AttributeSet(compA1, name="perturbation", value=perturb, rc=rc);
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
perturb=-1
call ESMF_AttributeSet(compA2, name="perturbation", value=perturb, rc=rc);
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
perturb=0.5
call ESMF_AttributeSet(compB1, name="perturbation", value=perturb, rc=rc);
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
perturb=-0.5
call ESMF_AttributeSet(compB2, name="perturbation", value=perturb, rc=rc);
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  print *, "Comp AttributeSet finished"

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Create and initialize a clock.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
  ! initialize calendar to be Gregorian type
  gregorianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, &
                                          name="Gregorian", rc=rc)
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
  clock = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                           name="Clock 1", rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  ! Create export state for each of the ensemble components and init
  cA1exp = ESMF_StateCreate(name="compA1 export",  &
                            stateintent=ESMF_STATEINTENT_EXPORT, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_GridCompInitialize(compA1, exportState=cA1exp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  cA2exp = ESMF_StateCreate(name="compA2 export",  &
                            stateintent=ESMF_STATEINTENT_EXPORT, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_GridCompInitialize(compA2, exportState=cA2exp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  cB1exp = ESMF_StateCreate(name="compB1 export",  &
                            stateintent=ESMF_STATEINTENT_EXPORT, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_GridCompInitialize(compB1, exportState=cB1exp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  cB2exp = ESMF_StateCreate(name="compB2 export",  &
                            stateintent=ESMF_STATEINTENT_EXPORT, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_GridCompInitialize(compB2, exportState=cB2exp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  ! Create the import state for the coupler and add the four export states
  ! from the four ensemble components to the coupler's import state
  cplimp = ESMF_StateCreate(name="coupler import",  &
                            stateintent=ESMF_STATEINTENT_IMPORT, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_StateAdd(cplimp, (/cA1exp/), rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_StateAdd(cplimp, (/cA2exp/), rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_StateAdd(cplimp, (/cB1exp/), rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_StateAdd(cplimp, (/cB2exp/), rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  ! Create the export state for the coupler
  cplexp = ESMF_StateCreate(name="coupler export",  &
                            stateintent=ESMF_STATEINTENT_EXPORT, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_CplCompInitialize(cpl, importState=cplimp, exportState=cplexp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  ! create the export state for the composite component compC and init
  ! the import state of compC is the export state of the coupler
  compCexp = ESMF_StateCreate(name="comp C export",  &
                              stateintent=ESMF_STATEINTENT_EXPORT, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_GridCompInitialize(compC, importState=cplexp, &
    exportState=compCexp, userRc=userrc, rc=localrc)
  print *, "Coupler Initialize finished, rc =", localrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  if (localPet == 0) print *, "Comp Initialize finished, rc =", localrc

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  print *, "Run Loop Start time"
  call ESMF_ClockPrint(clock, options="currtime string", rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10

  do while (.not. ESMF_ClockIsStopTime(clock, rc=rc))

        ! Sequence:  A1, A2, B1, B2 Coupler, C
  	call ESMF_GridCompRun(compA1, exportState=cA1exp, clock=clock, &
           userRc=userrc, rc=localrc)
  	if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  	if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  	call ESMF_GridCompRun(compA2, exportState=cA2exp, clock=clock, &
           userRc=userrc, rc=localrc)
  	if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  	if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  	call ESMF_GridCompRun(compB1, exportState=cB1exp, clock=clock, &
           userRc=userrc, rc=localrc)
  	if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  	if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  	call ESMF_GridCompRun(compB2, exportState=cB2exp, clock=clock, &
           userRc=userrc, rc=localrc)
  	if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  	if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  	call ESMF_CplCompRun(cpl, importState=cplimp, &
      exportState=cplexp, clock=clock, userRc=userrc, rc=localrc)
  	if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  	if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  	call ESMF_GridCompRun(compC, importState=cplexp, &
      exportState=compCexp, clock=clock, userRc=userrc, rc=localrc)
  	if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  	if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    	ESMF_CONTEXT, rcToReturn=rc)) &
    	call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Advance the time
        call ESMF_ClockAdvance(clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
        !call ESMF_ClockPrint(clock, "currtime string", rc)

  	print *, "Comp Run finished one step returned, rc =", localrc

  enddo
  if (localPet==0) print *, "Run Loop End time"
  if (localPet==0) call ESMF_ClockPrint(clock, options="currtime string", rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompFinalize(compA1, exportState=cA1exp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompFinalize(compA2, exportState=cA2exp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompFinalize(compB1, exportState=cB1exp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompFinalize(compB2, exportState=cB2exp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompFinalize(compC, importState=cplimp, &
    exportState=compCexp, userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_CplCompFinalize(cpl, importState=cplimp, &
    exportState=cplexp, userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  if (localPet==0) print *, "Comp Finalize finished, rc =", localrc

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompDestroy(compA1, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_GridCompDestroy(compA2, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_GridCompDestroy(compB1, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_GridCompDestroy(compB2, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_GridCompDestroy(compC, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_CplCompDestroy(cpl, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_StateDestroy(cA1exp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_StateDestroy(cA2exp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_StateDestroy(cB1exp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_StateDestroy(cB2exp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_StateDestroy(compCexp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_StateDestroy(cplimp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_StateDestroy(cplexp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_ClockDestroy(clock, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_CalendarDestroy(gregorianCalendar, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  if (localPet==0) print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

10 continue

  ! Normal ESMF Test output
  print *, testname, " complete."

  ! IMPORTANT: Test() prints the PASS: string that the scripts grep for.
  call ESMF_STest((rc.eq.ESMF_SUCCESS), testname, failMsg, testresult, &
    ESMF_SRCLINE)

  if ((localPet .eq. 0) .and. (rc .eq. ESMF_SUCCESS)) then
    ! Separate message to console, for quick confirmation of success/failure
    write(finalMsg, *) "SUCCESS: ",trim(testname)," finished correctly."
    write(0, *) ""
    write(0, *) trim(testname)
    write(0, *) trim(finalMsg)
    write(0, *) ""
  endif


  call ESMF_Finalize()

end program ESMF_SequentialEnsembleSTest

!\end{verbatim}
