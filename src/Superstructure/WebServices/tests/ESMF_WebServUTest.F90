! $Id: ESMF_WebServUTest.F90,v 1.4 2011/04/13 01:28:55 ksaint Exp $
!
! Test code which creates a new Component.

#include "ESMF.h"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! Tests, cursory and exahustive, for Component Create code.
!
!
!\begin{verbatim}

module ESMF_WebServUserModel

  ! ESMF Framework module
  use ESMF_Mod
  use ESMF_IO_NetCDFMod

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

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, &
                                    userRoutine=user_init, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, &
                                    userRoutine=user_run, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, &
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

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Init starting: "

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    print *, "User Comp1 Init returning: "

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
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Run starting"

    print *, "User Comp1 Run returning"

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
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Final starting"

    ! call ESMF_StatePrint(exportState)

    print *, "User Comp1 Final returning"

  end subroutine user_final


end module ESMF_WebServUserModel
    

program ESMF_WebServComponentUTest
    
!   ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod
    use ESMF_WebServMod
    use ESMF_WebServUserModel
    
    implicit none
    
    ! Local variables
    integer :: result = 0
    integer :: rc

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name

    ! other local variables
    character(ESMF_MAXSTR) :: cname
    type(ESMF_GridComp)    :: comp1
    type(ESMF_State)       :: impState
    type(ESMF_State)       :: expState
    integer                :: portNum

#ifdef ESMF_TESTEXHAUSTIVE
    type(ESMF_VM)       :: vm
    logical             :: bool
    integer             :: localPet
    type(ESMF_GridComp) :: comp2
#endif


        
!-------------------------------------------------------------------------
!   !
!   !  Quick Test - Register and unregister a Component Service.

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
 
    !------------------------------------------------------------------------
    ! Setup data parameters for tests
    cname = "Atmosphere - default context"
    comp1 = ESMF_GridCompCreate(name=cname, rc=rc)  

    call ESMF_GridCompSetServices(comp1, &
          userRoutine=ESMF_WebServUserModelRegister, rc=rc)

    impState = ESMF_StateCreate(name="comp1 import", &
          statetype=ESMF_STATE_IMPORT, rc=rc)
    expState = ESMF_StateCreate(name="comp1 export", &
          statetype=ESMF_STATE_EXPORT, rc=rc)
 
    !------------------------------------------------------------------------
    !NEX_disable_UTest
    ! Verifing that a ESMF Component Web Service can be registered
    rc = ESMF_SUCCESS
    portNum = 27060
    call ESMF_WebServicesLoop(comp1, portNum, rc=rc)
 

#ifdef ESMF_TESTEXHAUSTIVE

    !------------------------------------------------------------------------
    !EX_disable_UTest
    ! Verifing that a Gridded Component can be created in parent VM context
!    cname = "Atmosphere - child in parent VM context"
!    comp1 = ESMF_GridCompCreate(name=cname, gridcompType=ESMF_ATM, &
!      configFile="grid.rc", contextflag=ESMF_CHILD_IN_PARENT_VM, rc=rc)  
!    write(failMsg, *) "Did not return ESMF_SUCCESS"
!    write(name, *) "Creating a Gridded Component"
!    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_disable_UTest
    ! Query the run status of a Gridded Component 
!    bool = ESMF_GridCompIsPetLocal(comp1, rc=rc)  
!    write(failMsg, *) "Did not return ESMF_SUCCESS"
!    write(name, *) "Query run status of a Gridded Component"
!    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

    call ESMF_TestEnd(result, ESMF_SRCLINE)

end program ESMF_WebServComponentUTest
    
!\end{verbatim}
    
