! $Id: ESMF_WebServUTest.F90,v 1.3 2011/03/09 14:16:39 ksaint Exp $
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
    
