! $Id: ESMF_ComponentUTest.F90,v 1.18 2009/08/31 22:24:21 svasquez Exp $
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

    program ESMF_ComponentUTest
    
!   ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod
    
    implicit none
    
!   ! Local variables
    integer :: result = 0
    integer :: rc

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name

    ! other local variables
    character(ESMF_MAXSTR) :: cname
    type(ESMF_GridComp) :: comp1
#ifdef ESMF_TESTEXHAUSTIVE
    type(ESMF_VM) :: vm
    logical :: bool
    integer :: localPet
    type(ESMF_GridComp) :: comp2
#endif
        
!-------------------------------------------------------------------------
!   !
!   !  Quick Test - Create, Print, Destroy a Component.

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
 
    !------------------------------------------------------------------------
    !NEX_UTest
    ! Verifing that a Gridded Component can be created
    cname = "Atmosphere - default context"
    comp1 = ESMF_GridCompCreate(name=cname, gridcompType=ESMF_ATM, &
      configFile="grid.rc", rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    ! Verifing that a Gridded Component can be printed
    call ESMF_GridCompPrint(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Printing a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    ! Verifing that a Gridded Component can be destroyed
    call ESMF_GridCompDestroy(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Destroying a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

    !------------------------------------------------------------------------
    !EX_UTest
    ! Query the run status of a deleted Gridded Component 
    bool = ESMF_GridCompIsPetLocal(comp1, rc=rc)  
    write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
    write(name, *) "Query run status of a deleted Gridded Component"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Verify that the run status is false
    write(failMsg, *) "Did not return false"
    write(name, *) "Query run status of a deleted Gridded Component"
    call ESMF_Test((.not.bool), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Verifing that a Gridded Component can be created in parent VM context
    cname = "Atmosphere - child in parent VM context"
    comp1 = ESMF_GridCompCreate(name=cname, gridcompType=ESMF_ATM, &
      configFile="grid.rc", contextflag=ESMF_CHILD_IN_PARENT_VM, rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Query the run status of a Gridded Component 
    bool = ESMF_GridCompIsPetLocal(comp1, rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Query run status of a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Verify that the run status is true
    write(failMsg, *) "Did not return true"
    write(name, *) "Query run status of a deleted Gridded Component"
    call ESMF_Test((bool), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Create a Gridded Component setting the petlist to be out of range
    ! to force an error
    cname = "GridComp with out of range PetList"
    comp2 = ESMF_GridCompCreate(name=cname, petList=(/0,1,5,8/), rc=rc)  
    write(failMsg, *) "Did not return ESMF_RC_ARG_VALUE"
    write(name, *) "Creating a Gridded Component with wrong petList "
    call ESMF_Test((rc.eq.ESMF_RC_ARG_VALUE), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Create a Gridded Component setting the petlist to a negative number
    ! to force an error
    cname = "GridComp with out of range PetList"
    comp2 = ESMF_GridCompCreate(name=cname, petList=(/0,-3/), rc=rc)  
    write(failMsg, *) "Did not return ESMF_RC_ARG_VALUE"
    write(name, *) "Creating a Gridded Component with negative number in petList "
    call ESMF_Test((rc.eq.ESMF_RC_ARG_VALUE), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Query the run status of a non created Gridded Component 
    bool = ESMF_GridCompIsPetLocal(comp2, rc=rc)  
    write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
    write(name, *) "Query run status of a Gridded Component"
    ! most compilers return "ESMF_RC_OBJ_NOT_CREATED
    ! pgi returns "ESMF_RC_OBJ_BAD"
    call ESMF_Test(((rc.eq.ESMF_RC_OBJ_NOT_CREATED).or.(rc.eq.ESMF_RC_OBJ_BAD)), &
				name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Try to destroy a non created Gridded Component 
    call ESMF_GridCompDestroy(comp2, rc=rc)
    write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
    write(name, *) "Destroying a Gridded Component"
    ! most compilers return "ESMF_RC_OBJ_NOT_CREATED
    ! pgi returns "ESMF_RC_OBJ_BAD"
    call ESMF_Test(((rc.eq.ESMF_RC_OBJ_NOT_CREATED).or.(rc.eq.ESMF_RC_OBJ_BAD)), &
				name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Verifing that a Gridded Component can be printed
    call ESMF_GridCompPrint(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Printing a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Verifing that a Gridded Component can be destroyed
    call ESMF_GridCompDestroy(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Destroying a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Verifing that a Gridded Component can be created in new VM context
    cname = "Atmosphere - child in new VM context"
    comp1 = ESMF_GridCompCreate(name=cname, gridcompType=ESMF_ATM, &
      configFile="grid.rc", contextflag=ESMF_CHILD_IN_NEW_VM, rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Verifing that a Gridded Component can be printed
    call ESMF_GridCompPrint(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Printing a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Verifing that a Gridded Component can be destroyed
    call ESMF_GridCompDestroy(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Destroying a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

    call ESMF_TestEnd(result, ESMF_SRCLINE)

    end program ESMF_ComponentUTest
    
!\end{verbatim}
    
