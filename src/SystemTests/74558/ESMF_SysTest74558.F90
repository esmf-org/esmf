! $Id: ESMF_SysTest74558.F90,v 1.12 2003/06/06 20:24:14 nscollins Exp $
!
! System test code #74558

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test number 74558.  1 components solving fluid flow PDE's using.
! ESMF Infrastructure.
!
!\begin{verbatim}

    program ESMF_SysTest74558

#include <ESMF_Macros.inc>

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod
    
    use flowmod

    implicit none
    
    ! Local variables
    integer :: i, de_id, ndes, delist(64)
    integer :: status

    character(len=ESMF_MAXSTR) :: aname, cname1
    type(ESMF_DELayout) :: layout1, layout2
    type(ESMF_State) :: c1exp
    type(ESMF_AppComp) :: app
    type(ESMF_GridComp) :: comp1

    ! instantiate a clock, a calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime
    integer(ESMF_IKIND_I8) :: advanceCount

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: testresult = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
! Set initial values
!
    status = ESMF_FAILURE

    print *, "System Test #74558:"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    ! Create the top level application component.
    aname = "System Test #74558"
    app = ESMF_AppCompCreate(aname, rc=status)
    print *, "Created component ", trim(aname), ",  rc =", status

    ! Query application for layout.
    call ESMF_AppCompGet(app, layout=layout1, rc=status)
    call ESMF_DELayoutGetNumDEs(layout1, ndes, status)
    if (ndes .lt. 4) then
        print *, "This system test needs to run at least 4-way, current np = ", ndes
        goto 10
    endif


    ! Create the model component
    cname1 = "fluid flow"
    delist = (/ (i, i=0, ndes-1) /)
    layout2 = ESMF_DELayoutCreate(delist, 2, (/ ndes/2, 2 /), (/ 0 ,0 /), &
              status)
    comp1 = ESMF_GridCompCreate(cname1, layout=layout2, rc=status)
    print *, "Created component ", trim(cname1), "rc =", status 

    print *, "Comp Creates finished"


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      call ESMF_GridCompSetServices(comp1, FlowMod_register, status)
      print *, "Comp SetServices finished, rc= ", status

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create and initialize a clock.
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      ! initialize calendar to be Gregorian type
      call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, &
                             status)

      ! initialize time interval to 1 second
      call ESMF_TimeIntervalInit(time_step, s_=1.0D0, rc=status)

      ! initialize start time to 3/28/2003
      call ESMF_TimeInit(startTime, YR=int(2003,kind=ESMF_IKIND_I8), &
                         MM=3, DD=28, H=10, M=0, &
                         cal=gregorianCalendar, rc=status)

      ! initialize stop time to 3/29/2003
      call ESMF_TimeInit(stopTime, YR=int(2003,kind=ESMF_IKIND_I8), &
                         MM=3, DD=28, H=10, M=1, &
                         cal=gregorianCalendar, rc=status)

      ! initialize the clock with the above values
      call ESMF_ClockInit(clock, time_step, startTime, stopTime, &
                          rc=status)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
      c1exp = ESMF_StateCreate("comp1 export", ESMF_STATEEXPORT, cname1)
      call ESMF_GridCompInitialize(comp1, exportstate=c1exp, clock=clock, &
                                   rc=status)
      print *, "Comp 1 Initialize finished, rc =", status
 
      print *, "Component Initialize finished, rc =", status
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      do while (.not. ESMF_ClockIsStopTime(clock, status))

        call ESMF_GridCompRun(comp1, exportstate=c1exp, clock=clock, &
                              rc=status)
        print *, "Comp 1 Run returned, rc =", status

        call ESMF_ClockAdvance(clock, rc=status)
        !call ESMF_ClockPrint(clock, rc=status)

      enddo

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Print result

      call ESMF_GridCompFinalize(comp1, exportstate=c1exp, clock=clock, &
                                 rc=status)
      print *, "Comp 1 Finalize finished, rc =", status

      ! Figure out our local processor id for message below.
      call ESMF_DELayoutGetDEID(layout1, de_id, status)

      print *, "-----------------------------------------------------------------"
      print *, "-----------------------------------------------------------------"
      print *, "Test finished, de_id = ", de_id
      print *, "-----------------------------------------------------------------"
      print *, "-----------------------------------------------------------------"

      print *, "Comp Finalize returned"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Clean up

      call ESMF_StateDestroy(c1exp, status)

      call ESMF_GridCompDestroy(comp1, status)

      call ESMF_DELayoutDestroy(layout2, status)

      call ESMF_AppCompDestroy(app, status)

      print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
10    print *, "System Test #74558 complete!"


      write(failMsg, *)  "System Test failure"
      write(testname, *) "System Test 74558: Fluid Solver, single component"
  
      if (de_id .eq. 0) then
        call ESMF_Test((status.eq.ESMF_SUCCESS), &
                          testname, failMsg, testresult, ESMF_SRCLINE)
      endif
    
      end program ESMF_SysTest74558
    
!\end{verbatim}
    
