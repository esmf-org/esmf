! $Id: ESMF_ComponentUTest.F90,v 1.1 2005/03/14 23:54:07 nscollins Exp $
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

    call ESMF_TestStart(ESMF_SRCLINE, rc)
 
    !------------------------------------------------------------------------
    !NEX_UTest
    ! Verifing that a Gridded Component can be created
    cname = "Atmosphere"
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

    ! add more tests here

#endif

    call ESMF_TestEnd(result, ESMF_SRCLINE)

    end program ESMF_ComponentUTest
    
!\end{verbatim}
    
