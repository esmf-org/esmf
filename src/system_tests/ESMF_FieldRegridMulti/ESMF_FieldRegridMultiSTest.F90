! $Id: ESMF_FieldRegridMultiSTest.F90,v 1.4 2004/03/18 23:15:49 nscollins Exp $
!
! System test code FieldRegridMulti
!  Description on Sourceforge under System Test #xxxxx

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test FieldRegridMulti.  
!   Regrid test with a 2D grid and a 3D data field, where the 3rd dimension
!                 is multiple values corresponding to the same grid cell.
!
!                 2 components and 1 coupler, one-way coupling.
!                 The first component has a uniform A-grid.  It has
!                 a Field whose data is set to a given geometric function,
!
!                 10.0 + 5.0*sin((X/Xmax)*pi) + 2.0*sin((Y/Ymax)*pi)
!
!                 and then regridded to the second component, which has a
!                 non-uniform D-grid.  The regridded data is then compared
!                 to the function's solution for a measurement of the
!                 accuracy of the Regrid.  Those values are output for
!                 each DE.
!
!\begin{verbatim}

    program FieldRegridMulti

#include <ESMF_Macros.inc>

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod

    use user_model1, only : userm1_register
    use user_model2, only : userm2_register
    use user_coupler, only : usercpl_register

    implicit none
    
    ! Local variables
    integer :: i, de_id, ndes, mid, rc, delist(64), pid, cid
    character(len=ESMF_MAXSTR) :: cname1, cname2, cplname
    type(ESMF_DELayout) :: layout1, layout2, layout3
    type(ESMF_State) :: c1exp, c2imp
    type(ESMF_GridComp) :: comp1, comp2
    type(ESMF_CplComp) :: cpl

    ! instantiate a clock, a calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime
    integer(ESMF_KIND_I8) :: advanceCount

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: testresult = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message, and final status msg
    character(ESMF_MAXSTR) :: failMsg, finalMsg

        
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "------------------------------------- "
    print *, "Start of System Test FieldRegridMulti:"
    print *, "------------------------------------- "

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
    ! Initialize framework
    call ESMF_Initialize(rc)

    ! Query for default layout.
    layout1 = ESMF_DELayoutCreate(rc)

    call ESMF_DELayoutGetNumDEs(layout1, ndes, rc=rc)
    if (ndes .lt. 6) then
      print *, "This system test needs to run at least 6-way, current np = ", ndes
      goto 10
    endif

    mid = ndes / 2

    ! Create the 2 model components and coupler
    cname1 = "user model 1"
    delist = (/ (i, i=0, ndes-1) /)
    layout2 = ESMF_DELayoutCreate(layout1, 2, (/ 2, mid /), (/ 0, 0 /), &
                                  de_indices=delist, rc=rc)
    comp1 = ESMF_GridCompCreate(cname1, delayout=layout2, rc=rc)
    print *, "Created component ", trim(cname1), "rc =", rc
  !  call ESMF_GridCompPrint(comp1, "", rc)


    cname2 = "user model 2"
    delist = (/ (i, i=0, ndes-1) /)
    layout3 = ESMF_DELayoutCreate(layout1, 2, (/ mid, 2 /), (/ 0, 0 /), &
                                  de_indices=delist, rc=rc)

    comp2 = ESMF_GridCompCreate(cname2, delayout=layout3, rc=rc)
    print *, "Created component ", trim(cname2), "rc =", rc
  !  call ESMF_GridCompPrint(comp2, "", rc)

    cplname = "user one-way coupler"
    cpl = ESMF_CplCompCreate(cplname, delayout=layout1, rc=rc)
    print *, "Created component ", trim(cplname), ", rc =", rc
  !  call ESMF_CplCompPrint(cpl, "", rc)

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
      gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc)

      ! initialize time interval to 6 hours
      call ESMF_TimeIntervalSet(timeStep, h=6, rc=rc)

      ! initialize start time to 5/01/2003
      call ESMF_TimeSet(startTime, yy=2003, mm=5, dd=1, &
                        calendar=gregorianCalendar, rc=rc)

      ! initialize stop time to 5/02/2003
      call ESMF_TimeSet(stopTime, yy=2003, mm=5, dd=1, h=6, &
                        calendar=gregorianCalendar, rc=rc)

      ! initialize the clock with the above values
      clock = ESMF_ClockCreate("Clock 1", timeStep, startTime, stopTime, rc=rc)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
      c1exp = ESMF_StateCreate("comp1 export", ESMF_STATEEXPORT, cname1)
      call ESMF_GridCompInitialize(comp1, exportState=c1exp, clock=clock, rc=rc)
      print *, "Comp 1 Initialize finished, rc =", rc
 
      c2imp = ESMF_StateCreate("comp2 import", ESMF_STATEIMPORT, cname2)
      call ESMF_GridCompInitialize(comp2, importState=c2imp, clock=clock, rc=rc)
      print *, "Comp 2 Initialize finished, rc =", rc
 
      ! note that the coupler's import is comp1's export
      call ESMF_CplCompInitialize(cpl, c1exp, c2imp, clock=clock, rc=rc)
      print *, "Coupler Initialize finished, rc =", rc
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      do while (.not. ESMF_ClockIsStopTime(clock, rc))

        call ESMF_GridCompRun(comp1, exportState=c1exp, clock=clock, rc=rc)
        print *, "Comp 1 Run returned, rc =", rc
  
        call ESMF_CplCompRun(cpl, c1exp, c2imp, clock=clock, rc=rc)
        print *, "Coupler Run returned, rc =", rc
  
        call ESMF_GridCompRun(comp2, importState=c2imp, clock=clock, rc=rc)
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

      call ESMF_GridCompFinalize(comp1, exportState=c1exp, clock=clock, rc=rc)
      print *, "Comp 1 Finalize finished, rc =", rc

      call ESMF_GridCompFinalize(comp2, importState=c2imp, clock=clock, rc=rc)
      print *, "Comp 2 Finalize finished, rc =", rc

      call ESMF_CplCompFinalize(cpl, c1exp, c2imp, clock=clock, rc=rc)
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

      call ESMF_ClockDestroy(clock, rc)
      call ESMF_CalendarDestroy(gregorianCalendar, rc)

      call ESMF_GridCompDestroy(comp1, rc)
      call ESMF_GridCompDestroy(comp2, rc)
      call ESMF_CplCompDestroy(cpl, rc)

      call ESMF_DELayoutDestroy(layout2, rc)
      call ESMF_DELayoutDestroy(layout3, rc)

      print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
10    print *, "System Test FieldRegridMulti complete!"

      ! Only on de 0 or any DE with an error. 
      if ((de_id .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then

        ! Normal ESMF Test output
        write(failMsg, *) "System Test failure"
        write(testname, *) "System Test FieldRegrid: Field Regrid Multi"
  
        call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                          testname, failMsg, testresult, ESMF_SRCLINE)
  
        ! Separate message to console, for quick confirmation of success/failure
        if (rc .eq. ESMF_SUCCESS) then
          write(finalMsg, *) "SUCCESS!! Regrid Multi test finished correctly."
        else
          write(finalMsg, *) "System Test did not succeed.  Error code ", rc
        endif
        write(0, *) ""
        write(0, *) trim(testname)
        write(0, *) trim(finalMsg)
        write(0, *) ""
  
      endif
    
      call ESMF_Finalize(rc) 

      end program FieldRegridMulti
    
!\end{verbatim}
    
