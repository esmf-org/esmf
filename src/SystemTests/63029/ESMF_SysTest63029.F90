! $Id: ESMF_SysTest63029.F90,v 1.8 2003/04/04 16:11:57 nscollins Exp $
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
    integer, dimension(4) :: delist
    integer :: de_id, rc
    character(len=ESMF_MAXSTR) :: cname
    type(ESMF_DELayout) :: layout1 
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

!   Create a DELayout for the Component
    delist = (/ 0, 1, 2, 3 /)
    layout1 = ESMF_DELayoutCreate(2, 2, delist, ESMF_XFAST, rc)

    cname = "System Test #63029"
    comp1 = ESMF_GridCompCreate(cname, layout=layout1, mtype=ESMF_ATM, rc=rc)

!   Figure out our local processor id
    call ESMF_DELayoutGetDEID(layout1, de_id, rc)

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
 
      imp = ESMF_StateCreate(cname, ESMF_STATEIMPORT, rc)
      exp = ESMF_StateCreate(cname, ESMF_STATEEXPORT, rc)

      call ESMF_GridCompInitialize(comp1, imp, exp, rc=rc)
      print *, "Comp Initialize finished"
 
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
      call ESMF_DELayoutDestroy(layout1, rc)
      print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      print *, "System Test #63029 complete!"

      end program ESMF_SysTest63029
    
!\end{verbatim}
    
