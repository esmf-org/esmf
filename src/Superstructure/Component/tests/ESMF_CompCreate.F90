! $Id: ESMF_CompCreate.F90,v 1.12 2003/10/20 20:13:58 cdeluca Exp $
!
! Test code which creates a new Component.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! Tests, cursory and exahustive, for Component Create code.
!
!
!\begin{verbatim}

    program ESMF_CompCreateTest
    
!   ! ESMF Framework module
    use ESMF_Mod
    
    implicit none
    
!   ! Local variables
    integer :: rc
    character(ESMF_MAXSTR) :: cname
    type(ESMF_GridComp) :: comp1
        
!-------------------------------------------------------------------------
!   ! Test 1:
!   !
!   !  Quick Test - Create, Init, Run, Finalize, Destroy a Component.

    call ESMF_Initialize(rc)
 
    print *, "Component Test 1:"

    cname = "Atmosphere"
    comp1 = ESMF_GridCompCreate(name=cname, mtype=ESMF_ATM, &
                                             configfile="grid.rc", rc=rc)  

    print *, "Grid Comp Create returned, name = ", trim(cname)

    call ESMF_GridCompPrint(comp1, rc=rc)
    print *, "Comp Print returned"

    call ESMF_GridCompDestroy(comp1, rc=rc)
    print *, "Comp Run returned"

    print *, "Component Test 1 finished"

    call ESMF_Finalize(rc)

    end program ESMF_CompCreateTest
    
!\end{verbatim}
    
