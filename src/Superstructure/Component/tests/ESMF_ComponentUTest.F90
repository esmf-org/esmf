! $Id: ESMF_ComponentUTest.F90,v 1.7 2007/06/22 23:21:48 cdeluca Exp $
!
! Test code which creates a new Component.

#include <ESMF.h>

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
    integer :: rc, localPet
    integer :: result = 0
    logical :: bool

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name

    ! other local variables
    character(ESMF_MAXSTR) :: cname
    type(ESMF_InternGridComp) :: comp1, comp2
    type(ESMF_VM) :: vm
        
!-------------------------------------------------------------------------
!   !
!   !  Quick Test - Create, Print, Destroy a Component.

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
 
    !------------------------------------------------------------------------
    !NEX_UTest
    ! Verifing that a InternGridded Component can be created
    cname = "Atmosphere - default context"
    comp1 = ESMF_InternGridCompCreate(name=cname, interngridcompType=ESMF_ATM, &
      configFile="interngrid.rc", rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a InternGridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    ! Verifing that a InternGridded Component can be printed
    call ESMF_InternGridCompPrint(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Printing a InternGridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    ! Verifing that a InternGridded Component can be destroyed
    call ESMF_InternGridCompDestroy(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Destroying a InternGridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_EXHAUSTIVE

    !------------------------------------------------------------------------
    !EX_UTest
    ! Query the run status of a deleted InternGridded Component 
    bool = ESMF_InternGridCompIsPetLocal(comp1, rc=rc)  
    write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
    write(name, *) "Query run status of a deleted InternGridded Component"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Verify that the run status is false
    write(failMsg, *) "Did not return false"
    write(name, *) "Query run status of a deleted InternGridded Component"
    call ESMF_Test((.not.bool), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Verifing that a InternGridded Component can be created in parent VM context
    cname = "Atmosphere - child in parent VM context"
    comp1 = ESMF_InternGridCompCreate(name=cname, interngridcompType=ESMF_ATM, &
      configFile="interngrid.rc", contextflag=ESMF_CHILD_IN_PARENT_VM, rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a InternGridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Query the run status of a InternGridded Component 
    bool = ESMF_InternGridCompIsPetLocal(comp1, rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Query run status of a InternGridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Verify that the run status is true
    write(failMsg, *) "Did not return true"
    write(name, *) "Query run status of a deleted InternGridded Component"
    call ESMF_Test((bool), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Create a InternGridded Component setting the petlist to 1
    ! to force run status to be set to false for all other PETs
    cname = "InternGridComp with PetList"
    comp2 = ESMF_InternGridCompCreate(name=cname, petList=(/1/), rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a InternGridded Component with petList"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Query the run status of a InternGridded Component 
    bool = ESMF_InternGridCompIsPetLocal(comp2, rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Query run status of a InternGridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Get global VM
    call ESMF_VMGetGlobal(vm, rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Get global VM"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Get localPet from VM
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Get localPet from VM"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Verify that the run status is correct
    write(failMsg, *) "Did not return true on PET 1, false otherwise"
    write(name, *) "Verify run status of a InternGridded Component"
    if (localPet==1) then
      call ESMF_Test((bool), name, failMsg, result, ESMF_SRCLINE)
    else
      call ESMF_Test((.not.bool), name, failMsg, result, ESMF_SRCLINE)
    endif

    !------------------------------------------------------------------------
    !EX_UTest
    ! Verifing that a InternGridded Component can be destroyed
    call ESMF_InternGridCompDestroy(comp2, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Destroying a InternGridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Verifing that a InternGridded Component can be printed
    call ESMF_InternGridCompPrint(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Printing a InternGridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Verifing that a InternGridded Component can be destroyed
    call ESMF_InternGridCompDestroy(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Destroying a InternGridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Verifing that a InternGridded Component can be created in new VM context
    cname = "Atmosphere - child in new VM context"
    comp1 = ESMF_InternGridCompCreate(name=cname, interngridcompType=ESMF_ATM, &
      configFile="interngrid.rc", contextflag=ESMF_CHILD_IN_NEW_VM, rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a InternGridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Verifing that a InternGridded Component can be printed
    call ESMF_InternGridCompPrint(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Printing a InternGridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    ! Verifing that a InternGridded Component can be destroyed
    call ESMF_InternGridCompDestroy(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Destroying a InternGridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

    call ESMF_TestEnd(result, ESMF_SRCLINE)

    end program ESMF_ComponentUTest
    
!\end{verbatim}
    
