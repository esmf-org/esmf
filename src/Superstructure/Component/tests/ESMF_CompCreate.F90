! $Id: ESMF_CompCreate.F90,v 1.10 2003/04/24 23:35:06 nscollins Exp $
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

    call ESMF_FrameworkInitialize(rc)
 
    print *, "Component Test 1:"

    cname = "Atmosphere"
    comp1 = ESMF_GridCompCreate(name=cname, mtype=ESMF_ATM, &
                                             configfile="~/grid.rc", rc=rc)  

    print *, "Grid Comp Create returned, name = ", trim(cname)

    call ESMF_GridCompPrint(comp1, rc=rc)
    print *, "Comp Print returned"

    call ESMF_GridCompDestroy(comp1, rc=rc)
    print *, "Comp Run returned"

    print *, "Component Test 1 finished"

    call ESMF_FrameworkFinalize(rc)

    end program ESMF_CompCreateTest
    
!\end{verbatim}
    
