! $Id: ESMF_SysTest74558.F90,v 1.3 2003/04/04 23:24:22 jwolfe Exp $
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

    ! ESMF Framework module
    use ESMF_Mod
    
    use flowmod

    implicit none
    
    ! Local variables
    integer :: de_id, rc, delist(4)
    character(len=ESMF_MAXSTR) :: aname, cname1
    type(ESMF_DELayout) :: layout1, layout2
    type(ESMF_State) :: c1exp
    type(ESMF_AppComp) :: app
    type(ESMF_GridComp) :: comp1

    ! instantiate a clock, a calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime
    integer(ESMF_IKIND_I8) :: advanceCount

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "System Test #74558:"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    ! Create the top level application component.
    aname = "System Test #74558"
    app = ESMF_AppCompCreate(aname, rc=rc)
    print *, "Created component ", trim(aname), ",  rc =", rc

    ! Query application for layout.
    call ESMF_AppCompGet(app, layout=layout1, rc=rc)

    ! Create the model component
    cname1 = "fluid flow"
    delist = (/ 0, 1, 2, 3 /)
    layout2 = ESMF_DELayoutCreate(delist, 2, (/ 2, 2 /), (/ 0, 0, 0 ,0 /), rc)
    ! TODO: add 1D layout support to DELayout and DistGrid
    !layout2 = ESMF_DELayoutCreate(delist, 1, (/ 1 /), (/ 0 /), rc)
    comp1 = ESMF_GridCompCreate(cname1, layout=layout2, rc=rc)
    print *, "Created component ", trim(cname1), "rc =", rc

    print *, "Comp Creates finished"


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      call ESMF_GridCompSetServices(comp1, FlowMod_register, rc)
      print *, "Comp SetServices finished, rc= ", rc

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create and initialize a clock.
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      ! initialize calendar to be Gregorian type
      call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      ! initialize time interval to 1 hour
      call ESMF_TimeIntervalInit(timeStep, H=1, rc=rc)

      ! initialize start time to 3/28/2003
      call ESMF_TimeInit(startTime, YR=2003, MM=3, DD=28, &
                         cal=gregorianCalendar, rc=rc)

      ! initialize stop time to 3/29/2003
      call ESMF_TimeInit(stopTime, YR=2003, MM=3, DD=29, &
                         cal=gregorianCalendar, rc=rc)

      ! initialize the clock with the above values
      call ESMF_ClockInit(clock, timeStep, startTime, stopTime, rc=rc)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
      c1exp = ESMF_StateCreate(cname1, ESMF_STATEEXPORT, "comp1 export")
      call ESMF_GridCompInitialize(comp1, exportstate=c1exp, clock=clock, rc=rc)
      print *, "Comp 1 Initialize finished, rc =", rc
 
      print *, "Component Initialize finished, rc =", rc
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      do while (.not. ESMF_ClockIsStopTime(clock, rc))

        call ESMF_GridCompRun(comp1, exportstate=c1exp, clock=clock, rc=rc)
        print *, "Comp 1 Run returned, rc =", rc

        call ESMF_ClockAdvance(clock, rc=rc)
        !call ESMF_ClockPrint(clock, rc=rc)

      enddo

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Print result

      call ESMF_GridCompFinalize(comp1, exportstate=c1exp, clock=clock, rc=rc)
      print *, "Comp 1 Finalize finished, rc =", rc

      ! Figure out our local processor id for message below.
      call ESMF_DELayoutGetDEID(layout1, de_id, rc)

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

      call ESMF_StateDestroy(c1exp, rc)

      call ESMF_GridCompDestroy(comp1, rc)

      call ESMF_DELayoutDestroy(layout2, rc)

      print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      print *, "System Test #74558 complete!"

      end program ESMF_SysTest74558
    
!\end{verbatim}
    
