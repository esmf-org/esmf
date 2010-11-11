! $Id: Comp1Driver.F90,v 1.1 2010/11/11 19:35:53 ksaint Exp $
!
!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!-------------------------------------------------------------------------
!
! !DESCRIPTION:
! System test ArrayRedist.  
!    Two gridded components and one coupler component, one-way coupling.
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
!    coupler component then calls ArrayRedist() to redistribute the source
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

  use user_model1, only : userm1_setvm, userm1_register
  use ESMF_WebServMod

  implicit none
    
  ! Local variables
  integer :: localPet, petCount, localrc, rc=ESMF_SUCCESS
  character(len=ESMF_MAXSTR) :: cname1
  type(ESMF_VM):: vm
  type(ESMF_State) :: c1exp
  type(ESMF_GridComp) :: comp1

    type(ESMF_State) :: defaultstate
    type(ESMF_Clock) :: clock
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime


  ! cumulative result: count failures; no failures equals "all pass"
  integer :: testresult = 0

  ! individual test name
  character(ESMF_MAXSTR) :: testname

  ! individual test failure message, and final status msg
  character(ESMF_MAXSTR) :: failMsg, finalMsg

  ! mpi stuff
  integer :: mpierr, rank, size
  integer :: mpibuf, tag, status

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "System Test failure"
  write(testname, *) "System Test ESMF_ArrayRedistSTest"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  print *, "--------------------------------------- "
  print *, "Start of ", trim(testname)
  print *, "--------------------------------------- "

!-------------------------------------------------------------------------
! Initialize MPI
!-------------------------------------------------------------------------

  !call MPI_Init(mpierr)
  !call MPI_Comm_rank(0, rank, mpierr)

  !print *, "called MPI_Comm_rank", rank

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
  ! Initialize framework and get back default global VM
!  call ESMF_Initialize(vm=vm, rc=localrc)
    call ESMF_Initialize(vm=vm, defaultCalendar=ESMF_CAL_GREGORIAN, rc=localrc)

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
  !comp1 = ESMF_GridCompCreate(name=cname1, rc=localrc)
  print *, "Created component ", trim(cname1), "rc =", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  !print *, "Driver - comp is init", comp1%isInit

  print *, "Comp Creates finished"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompSetVM(comp1, userRoutine=userm1_setvm, rc=localrc)
  print *, "Comp1 SetVM finished, rc= ", localrc
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_GridCompSetServices(comp1, userRoutine=userm1_register, rc=localrc)
  print *, "Comp1 SetServices finished, rc= ", localrc
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

!  call ESMF_GridCompInitialize(comp1, exportState=c1exp, rc=localrc)
!  print *, "Comp 1 Initialize finished, rc =", localrc
!  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
!    ESMF_CONTEXT, rcToReturn=rc)) &
!    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!  call ESMF_GridCompRun(comp1, exportState=c1exp, rc=localrc)
!  print *, "Comp 1 Run returned, rc =", localrc
!  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
!    ESMF_CONTEXT, rcToReturn=rc)) &
!    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!  call ESMF_GridCompFinalize(comp1, exportState=c1exp, rc=localrc)
!  print *, "Comp 1 Finalize finished, rc =", localrc
!  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
!    ESMF_CONTEXT, rcToReturn=rc)) &
!    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)


!------------------------------------------------------------------------------
!  Create and initialize a Clock.
!------------------------------------------------------------------------------

      call ESMF_TimeIntervalSet(timeStep, s=2, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

      call ESMF_TimeSet(startTime, yy=2004, mm=9, dd=25, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

      call ESMF_TimeSet(stopTime, yy=2004, mm=9, dd=26, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

      clock = ESMF_ClockCreate("Application Clock", timeStep, startTime, &
                                stopTime, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)


!------------------------------------------------------------------------------
!  Create and initialize a State to use for both import and export.
!  In a real code, separate import and export States would normally be
!  created.
!------------------------------------------------------------------------------

      defaultstate = ESMF_StateCreate("Default State", rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)



!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Enter the service loop and wait for requests
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

   if (localPet == 0)  then
     call c_ESMC_RegisterComponent("Comp 1:First Component:localhost:27060", &
                                   rc=localrc)
     call c_ESMC_ComponentSvcLoop(comp1, defaultstate, c1exp, clock, 27060, &
                                  rc=localrc)
     call c_ESMC_UnregisterComponent("Comp 1:localhost:27060", rc=localrc)
   else
     call ESMF_WebServWaitForRequest(comp1, exportState=c1exp, rc=localrc)
   end if

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompDestroy(comp1, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_StateDestroy(c1exp, rc=localrc)
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

  !call MPI_Finalize(mpierr)

end program ESMF_ArrayRedistSTest
    
!\end{verbatim}
