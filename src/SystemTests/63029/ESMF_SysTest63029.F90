! $Id: ESMF_SysTest63029.F90,v 1.13 2003/05/07 16:07:38 nscollins Exp $
!
! System test code #63029

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test number 63029.
!
!
!\begin{verbatim}

    program ESMF_SysTest63029

    ! ESMF Framework module
    use ESMF_Mod
    
    use user_model

    implicit none
    
!   Local variables
    integer :: delist(64), dummy(2)
    integer :: i, de_id, ndes, mid, by2, rc
    character(len=ESMF_MAXSTR) :: cname
    type(ESMF_DELayout) :: layout1, layout2
    type(ESMF_GridComp) :: comp1
    type(ESMF_State) :: imp, exp
        
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "System Test #63029:"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    call ESMF_FrameworkInitialize(rc)

    ! Create a default 1xN DELayout
    layout1 = ESMF_DELayoutCreate(rc)
    call ESMF_DELayoutGetNumDEs(layout1, ndes, rc)

    if (ndes .le. 1) then
     mid = 1
     by2 = 1
    else
     mid = ndes/2
     by2 = 2
    endif

    ! Create a child DELayout for the Component which is 2 by half the
    !  total number of procs.
    delist = (/ (i, i=0, ndes-1) /)
    layout2 = ESMF_DELayoutCreate(layout1, 2, (/ mid, by2 /), (/ 0, 0 /), &
                                                   de_indices=delist, rc=rc)

    cname = "System Test #63029"
    comp1 = ESMF_GridCompCreate(cname, layout=layout2, mtype=ESMF_ATM, rc=rc)
    call ESMF_GridCompPrint(comp1)

    print *, "Comp Create finished, name = ", trim(cname)


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      call ESMF_GridCompSetServices(comp1, user_register, rc)
      print *, "Comp Register finished, rc= ", rc

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
      imp = ESMF_StateCreate("grid import state", ESMF_STATEIMPORT, rc=rc)
      exp = ESMF_StateCreate("grid export state", ESMF_STATEEXPORT, rc=rc)

      call ESMF_GridCompInitialize(comp1, imp, exp, phase=1, rc=rc)
      print *, "Comp Initialize 1 finished"
 
      call ESMF_GridCompInitialize(comp1, imp, exp, phase=2, rc=rc)
      print *, "Comp Initialize 2 finished"
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      call ESMF_GridCompRun(comp1, imp, exp, rc=rc)
      print *, "Comp Run returned first time"

      call ESMF_GridCompRun(comp1, imp, exp, rc=rc)
      print *, "Comp Run returned second time"
 
      call ESMF_GridCompRun(comp1, imp, exp, rc=rc)
      print *, "Comp Run returned third time"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Print result

      call ESMF_GridCompFinalize(comp1, imp, exp, rc=rc)

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

      call ESMF_GridCompDestroy(comp1, rc)
      call ESMF_StateDestroy(imp, rc)
      call ESMF_StateDestroy(exp, rc)
      call ESMF_DELayoutDestroy(layout2, rc)
      call ESMF_DELayoutDestroy(layout1, rc)
      print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
 10   print *, "System Test #63029 complete!"

      call ESMF_FrameworkFinalize(rc)

      end program ESMF_SysTest63029
    
!\end{verbatim}
    
