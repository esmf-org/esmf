! $Id: ESMF_SysTest62502.F90,v 1.5 2003/04/01 23:49:00 nscollins Exp $
!
! System test code #62502

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test number 62502.  2 components and 1 coupler, one-way coupling.
!
!
!\begin{verbatim}

    program ESMF_SysTest62502

#include "ESMF.h"

    ! Modules needed
    ! TODO: (these will be collapsed into a single ESMF_Mod soon)
    use ESMF_BaseMod
    use ESMF_IOMod
    use ESMF_DELayoutMod
    use ESMF_TimeMod
    use ESMF_TimeIntervalMod
    use ESMF_CalendarMod
    use ESMF_ClockMod
    use ESMF_StateMod
    use ESMF_CompMod
    
    use user_model1
    use user_model2
    use user_coupler

    implicit none
    
    ! Local variables
    integer :: de_id, rc, delist(4)
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

    print *, "System Test #62502:"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    ! Create the top level application component.
    aname = "System Test #62502"
    app = ESMF_AppCompCreate(aname, rc=rc)
    print *, "Created component ", trim(aname), ",  rc =", rc

    ! Query application for layout.
    call ESMF_AppCompGet(app, layout=layout1, rc=rc)


    ! Create the 2 model components and coupler
    cname1 = "user model 1"
    delist = (/ 0, 1, 2, 3 /)
    layout2 = ESMF_DELayoutCreate(4, 1, delist, ESMF_XFAST, rc)
    comp1 = ESMF_GridCompCreate(cname1, layout=layout2, rc=rc)
    print *, "Created component ", trim(cname1), "rc =", rc

    cname2 = "user model 2"
    layout3 = ESMF_DELayoutCreate(2, 2, delist, ESMF_XFAST, rc)
    comp2 = ESMF_GridCompCreate(cname2, layout=layout3, rc=rc)
    print *, "Created component ", trim(cname2), "rc =", rc

    cplname = "user one-way coupler"
    cpl = ESMF_CplCompCreate(cplname, layout=layout1, rc=rc)
    print *, "Created component ", trim(cplname), ", rc =", rc


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
      print *, "System Test #62502 complete!"

      end program ESMF_SysTest62502
    
!\end{verbatim}
    
