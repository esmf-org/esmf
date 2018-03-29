! $Id$
!
! Test code which creates a new Component.

#include "ESMF.h"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! Tests, cursory and exahustive, for Web Services Component Service
!
!
!\begin{verbatim}

module ESMF_WebServUserModel

  ! ESMF Framework module
  use ESMF
  use ESMF_TestMod

  implicit none
    
  public userm1_setvm, ESMF_WebServUserModelRegister
        
  contains


!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Set VM routine.
 
  subroutine userm1_setvm(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

  end subroutine


!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.

  subroutine ESMF_WebServUserModelRegister(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Register starting"

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, &
                                    userRoutine=user_init, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, &
                                    userRoutine=user_run, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, &
                                    userRoutine=user_final, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "Registered Initialize, Run, and Finalize routines"
    print *, "User Comp1 Register returning"
    
  end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
    
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: comp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)         :: vm
    integer               :: petCount

    ! local test variables
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
    integer                :: result = 0

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *
    print *, "User Comp1 Init starting: "

    !------------------------------------------------------------------------
    !NEX_disable_UTest
    ! Verifing that a ESMF Component Web Service can be registered
    write(failMsg, *) "Return not equal to ESMF_SUCCESS"
    write(name, *) "ESMF Initialize GridCompGet"

    call ESMF_GridCompGet(comp, vm=vm, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_disable_UTest
    ! Verifing that a ESMF Component Web Service can be registered
    rc = ESMF_SUCCESS
    write(failMsg, *) "Return not equal to ESMF_SUCCESS"
    write(name, *) "ESMF Initialize VMGet"

    call ESMF_VMGet(vm, petCount=petCount, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------
    
    print *, "User Comp1 Init returning"
    print *

  end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: comp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)         :: vm
    integer               :: petCount

    ! local test variables
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
    integer                :: result = 0

    ! Local variables
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *
    print *, "User Comp1 Run starting"

    !------------------------------------------------------------------------
    !NEX_disable_UTest
    ! Verifing that a ESMF Component Web Service can be registered
    write(failMsg, *) "Return not equal to ESMF_SUCCESS"
    write(name, *) "ESMF Run GridCompGet"

    call ESMF_GridCompGet(comp, vm=vm, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_disable_UTest
    ! Verifing that a ESMF Component Web Service can be registered
    rc = ESMF_SUCCESS
    write(failMsg, *) "Return not equal to ESMF_SUCCESS"
    write(name, *) "ESMF Run VMGet"

    call ESMF_VMGet(vm, petCount=petCount, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    print *, "User Comp1 Run returning"
    print *

  end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: comp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)         :: vm
    integer               :: petCount

    ! local test variables
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
    integer                :: result = 0
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *
    print *, "User Comp1 Final starting"

    !------------------------------------------------------------------------
    !NEX_disable_UTest
    ! Verifing that a ESMF Component Web Service can be registered
    write(failMsg, *) "Return not equal to ESMF_SUCCESS"
    write(name, *) "ESMF Finalize GridCompGet"

    call ESMF_GridCompGet(comp, vm=vm, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_disable_UTest
    ! Verifing that a ESMF Component Web Service can be registered
    rc = ESMF_SUCCESS
    write(failMsg, *) "Return not equal to ESMF_SUCCESS"
    write(name, *) "ESMF Finalize VMGet"

    call ESMF_VMGet(vm, petCount=petCount, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    print *, "User Comp1 Final returning"
    print *

  end subroutine user_final


end module ESMF_WebServUserModel
    

program ESMF_WebServComponentUTest
    
!   ! ESMF Framework module
    use ESMF
    use ESMF_TestMod
    use ESMF_WebServMod
    use ESMF_WebServUserModel
    
    implicit none
    
    ! Local variables
    integer                :: rc
    character(ESMF_MAXSTR) :: cname
    character(ESMF_MAXSTR) :: cdesc
    type(ESMF_GridComp)    :: comp1
    type(ESMF_CplComp)     :: cplComp
    integer                :: portNum
    character(ESMF_MAXSTR) :: clientId
    character(ESMF_MAXSTR) :: argBuffer

    ! local test variables
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
    integer                :: result = 0

#ifdef ESMF_TESTEXHAUSTIVE
#endif


        
!-------------------------------------------------------------------------
!   !
!   !  Quick Test - Setup the Web Services Loop

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
 
    !------------------------------------------------------------------------
    ! Setup data parameters for tests
    cname = "ATM"
    cdesc = "Atmosphere - default context"
    comp1 = ESMF_GridCompCreate(name=cname, rc=rc)  

    call ESMF_UtilGetArg (1, argvalue=argBuffer)
    print *, "Arg 1: ", argBuffer
    read(argBuffer,*) portNum
    print *, "Port Num: ", portNum

    call ESMF_UtilGetArg (2, argvalue=argBuffer)
    print *, "Arg 2: ", argBuffer
    read(argBuffer,*) clientId
    print *, "Client ID: ", clientId

    call ESMF_GridCompSetServices(comp1, &
          userRoutine=ESMF_WebServUserModelRegister, rc=rc)

    print *

    !------------------------------------------------------------------------
    !NEX_disable_UTest
    ! This test added to provide a placeholder when revisiting web services
    ! testing.
    rc = ESMF_SUCCESS
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "ESMF_WebServicesCplCompLoop Unit Test"

    call ESMF_WebServicesCplCompLoop(cplComp, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_disable_UTest
    ! Verifing that a ESMF Component Web Service can be registered
    rc = ESMF_SUCCESS
!!    portNum = 27060

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Web Services Loop"

    call ESMF_WebServicesLoop(comp1, portNum=portNum, clientId=clientId, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *
    !------------------------------------------------------------------------
 

    call ESMF_TestEnd(ESMF_SRCLINE)

end program ESMF_WebServComponentUTest
    
!\end{verbatim}
    
