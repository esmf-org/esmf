! $Id: ESMF_CompCreate.F90,v 1.8 2003/04/04 15:38:32 nscollins Exp $
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
    
!   ! Other ESMF modules which are needed by Comps
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


    end program ESMF_CompCreateTest
    
!\end{verbatim}
    
