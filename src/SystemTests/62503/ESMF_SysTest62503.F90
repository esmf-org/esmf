! $Id: ESMF_SysTest62503.F90,v 1.5 2003/04/08 23:09:57 nscollins Exp $
!
! System test code #62503

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test number 62503.  2 components and 1 coupler, one-way coupling.
!   Non-overlapping layouts.
!
!
!\begin{verbatim}

    program ESMF_SysTest62503

    ! ESMF Framework module
    use ESMF_Mod

    use user_model1, only : userm1_register
    use user_model2, only : userm2_register
    use user_coupler, only : usercpl_register

    implicit none
    
    ! Local variables
    integer :: i, de_id, ndes, mid, rc, delist(64), pid, cid
    character(len=ESMF_MAXSTR) :: aname, cname1, cname2, cplname
    type(ESMF_DELayout) :: layout1, layout2, layout3
    type(ESMF_State) :: c1exp, c2imp, cplstate(2)
    type(ESMF_AppComp) :: app
    type(ESMF_GridComp) :: comp1, comp2
    type(ESMF_CplComp) :: cpl

    ! instantiate a clock, a calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime
    integer(ESMF_IKIND_I8) :: advanceCount

        
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "--------------------------- "
    print *, "Start of System Test #62503:"
    print *, "--------------------------- "

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    ! Create the top level application component.
    aname = "System Test #62503"
    app = ESMF_AppCompCreate(aname, rc=rc)
    print *, "Created component ", trim(aname), ",  rc =", rc
    call ESMF_AppCompPrint(app, "", rc)

    ! Query application for layout.
    call ESMF_AppCompGet(app, layout=layout1, rc=rc)
    call ESMF_DELayoutGetNumDEs(layout1, ndes, rc=rc)
    mid = ndes / 2


    ! Create the 2 model components and coupler
    cname1 = "user model 1"
    delist = (/ (i, i=0, mid-1) /)
    layout2 = ESMF_DELayoutCreate(layout1, 2, (/ 2, ndes/4 /), (/ 0, 0 /), &
                                                      de_indices=delist, rc=rc)
    comp1 = ESMF_GridCompCreate(cname1, layout=layout2, rc=rc)
    print *, "Created component ", trim(cname1), "rc =", rc
    call ESMF_GridCompPrint(comp1, "", rc)


    cname2 = "user model 2"
    delist = (/ (i, i=mid, ndes-1) /)
    layout3 = ESMF_DELayoutCreate(layout1, 2, (/ 1, ndes/2 /), (/ 0, 0 /), &
                                                      de_indices=delist, rc=rc)


    comp2 = ESMF_GridCompCreate(cname2, layout=layout3, rc=rc)
    print *, "Created component ", trim(cname2), "rc =", rc
    call ESMF_GridCompPrint(comp2, "", rc)

    cplname = "user one-way coupler"
    cpl = ESMF_CplCompCreate(cplname, layout=layout1, rc=rc)
    print *, "Created component ", trim(cplname), ", rc =", rc
    call ESMF_CplCompPrint(cpl, "", rc)


    print *, "Comp Creates finished"


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      call ESMF_GridCompSetServices(comp1, userm1_register, rc)
      print *, "Comp SetServices finished, rc= ", rc

      call ESMF_GridCompSetServices(comp2, userm2_register, rc)
      print *, "Comp SetServices finished, rc= ", rc

      call ESMF_CplCompSetServices(cpl, usercpl_register, rc)
      print *, "Comp SetServices finished, rc= ", rc

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create and initialize a clock.
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      ! initialize calendar to be Gregorian type
      call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      ! initialize time interval to 6 hours
      call ESMF_TimeIntervalInit(timeStep, H=6, rc=rc)

      ! initialize start time to 3/28/2003
      call ESMF_TimeInit(startTime, YR=2003, MM=5, DD=1, &
                         cal=gregorianCalendar, rc=rc)

      ! initialize stop time to 3/29/2003
      call ESMF_TimeInit(stopTime, YR=2003, MM=5, DD=2, &
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
 
      c2imp = ESMF_StateCreate(cname2, ESMF_STATEIMPORT, "comp2 import")
      call ESMF_GridCompInitialize(comp2, importstate=c2imp, clock=clock, rc=rc)
      print *, "Comp 1a Initialize finished, rc =", rc
 
      cplstate(1) = c1exp
      cplstate(2) = c2imp

      call ESMF_CplCompInitialize(cpl, statelist=cplstate, clock=clock, rc=rc)
      print *, "Coupler Initialize finished, rc =", rc
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      do while (.not. ESMF_ClockIsStopTime(clock, rc))

        call ESMF_GridCompRun(comp1, exportstate=c1exp, clock=clock, rc=rc)
        print *, "Comp 1 Run returned, rc =", rc
  
        call ESMF_CplCompRun(cpl, statelist=cplstate, clock=clock, rc=rc)
        print *, "Coupler Run returned, rc =", rc
  
        call ESMF_GridCompRun(comp2, importstate=c2imp, clock=clock, rc=rc)
        print *, "Comp 2 Run returned, rc =", rc

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

      call ESMF_GridCompFinalize(comp2, importstate=c2imp, clock=clock, rc=rc)
      print *, "Comp 2 Finalize finished, rc =", rc

      call ESMF_CplCompFinalize(cpl, statelist=cplstate, clock=clock, rc=rc)
      print *, "Coupler Finalize finished, rc =", rc


      ! Figure out our local processor id for message below.
      call ESMF_DELayoutGetDEID(layout1, de_id, rc)


      print *, "------------------------------------------------------------"
      print *, "------------------------------------------------------------"
      print *, "Test finished, de_id = ", de_id
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

      call ESMF_GridCompDestroy(comp1, rc)
      call ESMF_GridCompDestroy(comp2, rc)
      call ESMF_CplCompDestroy(cpl, rc)

      call ESMF_DELayoutDestroy(layout2, rc)
      call ESMF_DELayoutDestroy(layout3, rc)

      call ESMF_AppCompDestroy(app, rc)
      print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      print *, "System Test #62503 complete!"

      end program ESMF_SysTest62503
    
!\end{verbatim}
    
