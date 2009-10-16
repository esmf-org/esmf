! $Id: ESMF_CompCreateSTest.F90,v 1.29 2009/10/16 20:37:05 svasquez Exp $
!
! System test CompCreate
!  Description on Sourceforge under System Test #63029

!-------------------------------------------------------------------------
!ESMF_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test CompCreate.
!
!
!\begin{verbatim}

    program CompCreate

#include "ESMF_Macros.inc"

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod
    
    use user_model, only : user_setvm, user_register

    implicit none
    
!   Local variables
    integer :: my_pet, rc
    type(ESMF_VM):: vm
    type(ESMF_GridComp) :: comp1
    type(ESMF_State) :: imp, exp
    character(len=ESMF_MAXSTR) :: cname
        
    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

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

    call ESMF_Initialize(defaultlogfilename="CompCreateSTest.Log", &
                        defaultlogtype=ESMF_LOG_MULTI, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Get the default global VM
    call ESMF_VMGetGlobal(vm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Get our pet number for output print statements
    call ESMF_VMGet(vm, localPet=my_pet, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    cname = "System Test CompCreate"
    comp1 = ESMF_GridCompCreate(name=cname, gridcompType=ESMF_ATM, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_GridCompPrint(comp1)

    print *, "Comp Create finished, name = ", trim(cname)


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      call ESMF_GridCompSetVM(comp1, userRoutine=user_setvm, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp SetVM finished, rc= ", rc
      call ESMF_GridCompSetServices(comp1, userRoutine=user_register, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp Register finished, rc= ", rc

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
      imp = ESMF_StateCreate("igrid import state", ESMF_STATE_IMPORT, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      exp = ESMF_StateCreate("igrid export state", ESMF_STATE_EXPORT, rc=rc)
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


      print *, "-----------------------------------------------------------------"
      print *, "-----------------------------------------------------------------"
      print *, "Test finished, my_pet = ", my_pet
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

      call ESMF_GridCompDestroy(comp1, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(imp, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(exp, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
 10   print *, "System Test CompCreate complete"

      ! Standard ESMF Test output to log file
      write(failMsg, *) "System Test failure"
      write(testname, *) "System Test CompCreate: Component Create Test"
  
      if (rc .ne. ESMF_SUCCESS) then
        ! Separate message to console, for quick confirmation of success/failure
        if (rc .eq. ESMF_SUCCESS) then
          write(finalMsg, *) "SUCCESS: Component Create complete."
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

      end program CompCreate
    
!\end{verbatim}
    
