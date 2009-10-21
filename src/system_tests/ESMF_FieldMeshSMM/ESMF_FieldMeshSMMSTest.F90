! $Id: ESMF_FieldMeshSMMSTest.F90,v 1.6 2009/10/21 22:29:59 feiliu Exp $
!
! System test code FieldMeshSMM
!  Description on Sourceforge under System Test #79497

!-------------------------------------------------------------------------
!EremoveSMF_MULTI_PROC_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test FieldMeshSMM.  
!   Field SMM test.  2 components and 1 coupler, one-way coupling.
!                 The first component has a quadruple type Mesh with
!                 a Field whose data is set to constant value 1
!                 and then transformed to the second component through
!                 sparse matrix multiply operation. The destination Field  builds upon
!                 a Grid congruent with the nodal distribution of the source Mesh. 
!                 The transformed data is then compared
!                 to predetermined result based on the structure of SMM operation.
!
!\begin{verbatim}

    program ESMF_FieldMeshSMMSTest
#define ESMF_METHOD "program ESMF_FieldMeshSMMSTest"

#include "ESMF.h"

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod

    use user_model1, only : userm1_register
    use user_model2, only : userm2_register
    use user_coupler, only : usercpl_register

    implicit none
    
    ! Local variables
    integer :: pet_id, npets, rc, localrc, userrc
    character(len=ESMF_MAXSTR) :: cname1, cname2, cplname
    type(ESMF_VM):: vm
    type(ESMF_State) :: c1exp, c2imp
    type(ESMF_GridComp) :: comp1, comp2
    type(ESMF_CplComp) :: cpl

    ! instantiate a clock, a calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime

    !cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0
    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message, and final status msg
    character(ESMF_MAXSTR) :: failMsg, finalMsg

        
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "-------------------------------- "
    print *, "Start of System Test FieldMeshSMM:"
    print *, "-------------------------------- "

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
    ! Initialize framework and get back default global VM
    call ESMF_Initialize(vm=vm, defaultlogfilename="FieldMeshSMMSTest.Log", &
              defaultlogtype=ESMF_LOG_MULTI, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)

    ! Get number of PETs we are running with
    call ESMF_VMGet(vm, petCount=npets, localPet=pet_id, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)

    if (npets .lt. 3) then
      print *, "This system test needs to run at least 3-way, current np = ", &
               npets
      goto 10
    endif

    ! Create the 2 model components and coupler
    cname1 = "user model 1"
    comp1 = ESMF_GridCompCreate(name=cname1, rc=localrc)
    print *, "Created component ", trim(cname1), " rc =", localrc
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)
    !  call ESMF_GridCompPrint(comp1, "", rc)

    cname2 = "user model 2"
    comp2 = ESMF_GridCompCreate(name=cname2, rc=localrc)
    print *, "Created component ", trim(cname2), " rc =", localrc
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)
    !  call ESMF_GridCompPrint(comp2, "", rc)

    cplname = "user one-way coupler"
    cpl = ESMF_CplCompCreate(name=cplname, rc=localrc)
    print *, "Created component ", trim(cplname), " rc =", localrc
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)
    !  call ESMF_CplCompPrint(cpl, "", rc)

    print *, "Comp Creates finished"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    call ESMF_GridCompSetServices(comp1, userm1_register, rc=localrc)
    print *, "Comp SetServices finished, rc= ", localrc
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)

    call ESMF_GridCompSetServices(comp2, userm2_register, rc=localrc)
    print *, "Comp SetServices finished, rc= ", localrc
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)

    call ESMF_CplCompSetServices(cpl, usercpl_register, rc=localrc)
    print *, "Comp SetServices finished, rc= ", localrc
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create and initialize a clock.
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    ! initialize calendar to be Gregorian type
    gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                            ESMF_CAL_GREGORIAN, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)

    ! initialize time interval to 6 hours
    call ESMF_TimeIntervalSet(timeStep, h=6, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)

    ! initialize start time to 5/01/2003
    call ESMF_TimeSet(startTime, yy=2003, mm=5, dd=1, &
                      calendar=gregorianCalendar, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)

    ! initialize stop time to 5/02/2003
    call ESMF_TimeSet(stopTime, yy=2003, mm=5, dd=1, h=6, &
                      calendar=gregorianCalendar, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)

    ! initialize the clock with the above values
    clock = ESMF_ClockCreate("Clock 1", timeStep, startTime, stopTime, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
    c1exp = ESMF_StateCreate("comp1 export", ESMF_STATE_EXPORT, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)
    call ESMF_GridCompInitialize(comp1, exportState=c1exp, clock=clock, &
      userRc=userrc, rc=localrc)
    print *, "Comp 1 Initialize finished, rc =", localrc
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
    call ESMF_GridCompInitialize(comp2, importState=c2imp, clock=clock, &
      userRc=userrc, rc=localrc)
    print *, "Comp 2 Initialize finished, rc =", localrc
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)
    if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)
 
    ! note that the coupler's import is comp1's export
    call ESMF_CplCompInitialize(cpl, c1exp, c2imp, clock=clock, &
      userRc=userrc, rc=localrc)
    print *, "Coupler Initialize finished, rc =", localrc
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)
    if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    do while (.not. ESMF_ClockIsStopTime(clock, rc))

      call ESMF_GridCompRun(comp1, exportState=c1exp, clock=clock, &
        userRc=userrc, rc=localrc)
      print *, "Comp 1 Run returned, rc =", localrc
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(terminationflag=ESMF_ABORT)
      if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(terminationflag=ESMF_ABORT)

      call ESMF_CplCompRun(cpl, c1exp, c2imp, clock=clock, &
        userRc=userrc, rc=localrc)
      print *, "Coupler Run returned, rc =", localrc
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(terminationflag=ESMF_ABORT)
      if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(terminationflag=ESMF_ABORT)

      call ESMF_GridCompRun(comp2, importState=c2imp, clock=clock, &
        userRc=userrc, rc=localrc)
      print *, "Comp 2 Run returned, rc =", localrc
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(terminationflag=ESMF_ABORT)
      if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(terminationflag=ESMF_ABORT)

      call ESMF_ClockAdvance(clock, rc=localrc)
      !call ESMF_ClockPrint(clock, rc=rc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(terminationflag=ESMF_ABORT)

    enddo
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Print result

    call ESMF_GridCompFinalize(comp1, exportState=c1exp, clock=clock, &
      userRc=userrc, rc=localrc)
    print *, "Comp 1 Finalize finished, rc =", localrc
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)
    if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)

    call ESMF_GridCompFinalize(comp2, importState=c2imp, clock=clock, &
      userRc=userrc, rc=localrc)
    print *, "Comp 2 Finalize finished, rc =", localrc
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)
    if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)

    call ESMF_CplCompFinalize(cpl, c1exp, c2imp, clock=clock, &
      userRc=userrc, rc=localrc)
    print *, "Coupler Finalize finished, rc =", localrc
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)
    if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(terminationflag=ESMF_ABORT)




    print *, "------------------------------------------------------------"
    print *, "------------------------------------------------------------"
    print *, "Test finished, pet_id = ", pet_id
    print *, "------------------------------------------------------------"
    print *, "------------------------------------------------------------"

    print *, "Comp Finalize returned"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Clean up

    call ESMF_StateDestroy(c1exp, rc)
    call ESMF_StateDestroy(c2imp, rc)

    call ESMF_ClockDestroy(clock, rc)
    call ESMF_CalendarDestroy(gregorianCalendar, rc)

    call ESMF_GridCompDestroy(comp1, rc)
    call ESMF_GridCompDestroy(comp2, rc)
    call ESMF_CplCompDestroy(cpl, rc)

    print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
10  print *, "System Test FieldMeshSMM complete."


    ! Normal ESMF Test output
    write(failMsg, *) "System Test failure"
    write(testname, *) "System Test FieldMeshSMM: Mesh based Field SMM"

    if (rc .ne. ESMF_SUCCESS) then
      ! Separate message to console, for quick confirmation of success/failure
      if (rc .eq. ESMF_SUCCESS) then
        write(finalMsg, *) "SUCCESS: Mesh based Field SMM test finished correctly."
      else
        write(finalMsg, *) "System Test did not succeed.  Error code ", rc
      endif
      write(0, *) ""
      write(0, *) trim(testname)
      write(0, *) trim(finalMsg)
      write(0, *) ""

    endif
  
    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
    ! file that the scripts grep for.
    call ESMF_STest((rc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)
    call ESMF_Finalize(rc=rc) 

    end program ESMF_FieldMeshSMMSTest
    
!\end{verbatim}
    
