! $Id$
!
! System test CompCreateWithResNeg
!  Description on Sourceforge under System Test #63029

!-------------------------------------------------------------------------
!ESMF_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test CompCreateWithResNeg.
!
!
!\begin{verbatim}

    program CompCreateWithResNeg

    ! ESMF Framework module
    use ESMF
    use ESMF_TestMod

    use user_model1, only : user_neg_routine1 => user_neg_routine,&
                            user_setvm1 => user_setvm,&
                            user_register1 => user_register

    use user_model2, only : user_neg_routine2 => user_neg_routine,&
                            user_setvm2 => user_setvm,&
                            user_register2 => user_register

    implicit none

!   Local variables
    integer :: my_pet, rc, userrc
    type(ESMF_VM):: vm
    type(ESMF_GridComp) :: comp1, comp2
    type(ESMF_State) :: imp1, imp2, exp1, exp2
    character(len=ESMF_MAXSTR) :: cname1, cname2

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message and status msg
    character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "System Test CompCreateWithResNeg:"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    call ESMF_Initialize(defaultlogfilename="CompCreateWithResNegSTest.Log", &
                        logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Get the default global VM
    call ESMF_VMGetGlobal(vm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Get our pet number for output print statements
    call ESMF_VMGet(vm, localPet=my_pet, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    cname1 = "System Test CompCreateWithResNeg Comp 1"
    !comp1 = ESMF_GridCompCreate(name=cname1, userNegRoutine=user_neg_routine1, rc=rc)
    comp1 = ESMF_GridCompCreate(name=cname1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_GridCompPrint(comp1)

    print *, "Comp Create finished, name = ", trim(cname1)

    cname2 = "System Test CompCreateWithResNeg Comp 2"
    !comp2 = ESMF_GridCompCreate(name=cname2, userNegRoutine=user_neg_routine1, rc=rc)
    comp2 = ESMF_GridCompCreate(name=cname2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_GridCompPrint(comp2)

    print *, "Comp Create finished, name = ", trim(cname2)


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      call ESMF_GridCompSetUserNegRoutine(comp1, userNegRoutine=user_neg_routine1, &
        rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp SetUserNegRoutine (comp 1) finished, rc= ", rc
      call ESMF_GridCompSetUserNegRoutine(comp2, userNegRoutine=user_neg_routine2, &
        rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp SetUserNegRoutine (comp 2) finished, rc= ", rc

      call ESMF_GridCompSetVM(comp1, userRoutine=user_setvm1, &
        userRc=userrc, rc=rc)
      if ((rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS)) goto 10
      print *, "Comp SetVM finished (comp 1), rc= ", rc
      call ESMF_GridCompSetVM(comp2, userRoutine=user_setvm2, &
        userRc=userrc, rc=rc)
      if ((rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS)) goto 10
      print *, "Comp SetVM finished (comp 2), rc= ", rc

      call ESMF_GridCompSetServices(comp1, userRoutine=user_register1, &
        userRc=userrc, rc=rc)
      if ((rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS)) goto 10
      print *, "Comp Register finished (comp 1), rc= ", rc
      call ESMF_GridCompSetServices(comp2, userRoutine=user_register2, &
        userRc=userrc, rc=rc)
      if ((rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS)) goto 10
      print *, "Comp Register finished (comp 2), rc= ", rc

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      imp1 = ESMF_StateCreate(name="igrid import state Comp1",  &
                             stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
      if ( (rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS) ) goto 10
      exp1 = ESMF_StateCreate(name="igrid export state Comp1",  &
                             stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      imp2 = ESMF_StateCreate(name="igrid import state Comp2",  &
                             stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
      if ( (rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS) ) goto 10
      exp2 = ESMF_StateCreate(name="igrid export state Comp2",  &
                             stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_GridCompInitialize(comp1, importState=imp1, &
        exportState=exp1, phase=1, userRc=userrc, rc=rc)
      if ( (rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS) ) goto 10
      print *, "Comp Initialize 1 finished (Comp 1)"

      call ESMF_GridCompInitialize(comp1, importState=imp1, &
        exportState=exp1, phase=2, userRc=userrc, rc=rc)
      if ( (rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS) ) goto 10
      print *, "Comp Initialize 2 finished (Comp 1)"

      call ESMF_GridCompInitialize(comp2, importState=imp2, &
        exportState=exp2, phase=1, userRc=userrc, rc=rc)
      if ( (rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS) ) goto 10
      print *, "Comp Initialize 1 finished (Comp 2)"

      call ESMF_GridCompInitialize(comp2, importState=imp2, &
        exportState=exp2, phase=2, userRc=userrc, rc=rc)
      if ( (rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS) ) goto 10
      print *, "Comp Initialize 2 finished (Comp 2)"
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      call ESMF_GridCompRun(comp1, importState=imp1, &
        exportState=exp1, userRc=userrc, rc=rc)
      if ((rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS)) goto 10
      print *, "Comp Run returned first time (Comp 1)"

      call ESMF_GridCompRun(comp2, importState=imp2, &
        exportState=exp2, userRc=userrc, rc=rc)
      if ((rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS)) goto 10
      print *, "Comp Run returned first time (Comp 2)"

      call ESMF_GridCompRun(comp1, importState=imp1, &
        exportState=exp1, userRc=userrc, rc=rc)
      if ((rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS)) goto 10
      print *, "Comp Run returned second time (Comp 1)"

      call ESMF_GridCompRun(comp2, importState=imp2, &
        exportState=exp2, userRc=userrc, rc=rc)
      if ((rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS)) goto 10
      print *, "Comp Run returned second time (Comp 2)"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Print result

      call ESMF_GridCompFinalize(comp1, importState=imp1, &
        exportState=exp1, userRc=userrc, rc=rc)
      if ( (rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS) ) goto 10

      call ESMF_GridCompFinalize(comp2, importState=imp2, &
        exportState=exp2, userRc=userrc, rc=rc)
      if ( (rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS) ) goto 10


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
      call ESMF_GridCompDestroy(comp2, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(imp1, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(imp2, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(exp1, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(exp2, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

 10   print *, "System Test CompCreateWithResNeg complete"

      ! Standard ESMF Test output to log file
      write(failMsg, *) "System Test failure"
      write(testname, *) "System Test CompCreateWithResNeg: Component Create Test"

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

      ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors
      ! into the Log file that the scripts grep for.
      call ESMF_STest((rc.eq.ESMF_SUCCESS), testname, failMsg, result, &
        __FILE__, &
        __LINE__)

      call ESMF_Finalize(rc=rc)

      end program CompCreateWithResNeg

!\end{verbatim}

