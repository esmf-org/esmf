! $Id: ESMF_CompCreateSTest.F90,v 1.5 2004/04/13 22:00:42 nscollins Exp $
!
! System test CompCreate
!  Description on Sourceforge under System Test #63029

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test CompCreate.
!
!
!\begin{verbatim}

    program CompCreate

#include <ESMF_Macros.inc>

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod
    
    use user_model

    implicit none
    
!   Local variables
    integer :: delist(64), dummy(2)
    integer :: i, de_id, ndes, mid, by2, rc
    character(len=ESMF_MAXSTR) :: cname
    type(ESMF_VM):: vm
    type(ESMF_newDELayout) :: layout1, layout2
    type(ESMF_GridComp) :: comp1
    type(ESMF_State) :: imp, exp
        
    ! cumulative result: count failures; no failures equals "all pass"
    integer :: testresult = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message and status msg
    character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "System Test CompCreate:"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    call ESMF_Initialize(rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! get the default global VM
    call ESMF_VMGetGlobal(vm, rc)

    ! Create a default 1xN DELayout
    layout1 = ESMF_newDELayoutCreate(vm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_newDELayoutGetNumDEs(layout1, ndes, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

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
    layout2 = ESMF_newDELayoutCreate(vm, rc=rc)
    !layout2 = ESMF_DELayoutCreate(vm, layout1, 2, (/ mid, by2 /), (/ 0, 0 /), &
    !                                               de_indices=delist, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    cname = "System Test CompCreate"
    comp1 = ESMF_GridCompCreate(cname, delayout=layout2, gridcompType=ESMF_ATM, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_GridCompPrint(comp1)

    print *, "Comp Create finished, name = ", trim(cname)


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      call ESMF_GridCompSetServices(comp1, user_register, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp Register finished, rc= ", rc

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
      imp = ESMF_StateCreate("grid import state", ESMF_STATEIMPORT, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      exp = ESMF_StateCreate("grid export state", ESMF_STATEEXPORT, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_GridCompInitialize(comp1, imp, exp, phase=1, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp Initialize 1 finished"
 
      call ESMF_GridCompInitialize(comp1, imp, exp, phase=2, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp Initialize 2 finished"
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      call ESMF_GridCompRun(comp1, imp, exp, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp Run returned first time"

      call ESMF_GridCompRun(comp1, imp, exp, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp Run returned second time"
 
      call ESMF_GridCompRun(comp1, imp, exp, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp Run returned third time"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Print result

      call ESMF_GridCompFinalize(comp1, imp, exp, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_newDELayoutGetDEID(layout1, de_id, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

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
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(imp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(exp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_newDELayoutDestroy(layout2, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_newDELayoutDestroy(layout1, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
 10   print *, "System Test CompCreate complete!"

      if ((de_id .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then
        ! Standard ESMF Test output to log file
        write(failMsg, *) "System Test failure"
        write(testname, *) "System Test CompCreate: Component Create Test"
  
        call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                          testname, failMsg, testresult, ESMF_SRCLINE)

        ! Separate message to console, for quick confirmation of success/failure
        if (rc .eq. ESMF_SUCCESS) then
          write(finalMsg, *) "SUCCESS!! Component Create complete"
        else
          write(finalMsg, *) "System Test did not succeed.  Error code ", rc
        endif
        write(0, *) ""
        write(0, *) trim(testname)
        write(0, *) trim(finalMsg)
        write(0, *) ""

      endif

      call ESMF_Finalize(rc)

      end program CompCreate
    
!\end{verbatim}
    
