! $Id: ESMF_ComponentUTest.F90,v 1.2.2.1 2006/10/19 21:33:34 theurich Exp $
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
    integer :: rc
    integer :: result = 0

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name

    ! other local variables
    character(ESMF_MAXSTR) :: cname
    type(ESMF_GridComp) :: comp1
        
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

#ifdef ESMF_EXHAUSTIVE

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
    
