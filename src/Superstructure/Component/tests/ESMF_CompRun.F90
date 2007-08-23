! $Id: ESMF_CompRun.F90,v 1.15 2007/08/23 17:16:03 cdeluca Exp $
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

    program ESMF_GridCompCreateTest
    
!   ! ESMF Framework module
    use ESMF_Mod

    use UserGridCompMod, only: User_SetServices
    
    implicit none
    
!   ! Local variables
    integer :: rc
    character(ESMF_MAXSTR) :: cname
    type(ESMF_GridComp) :: comp1
    type(ESMF_Clock) :: clock
        
!-------------------------------------------------------------------------
!   ! Test 1:
!   !
!   !  Quick Test - Create, Init, Run, Finalize, Destroy a Component.

    call ESMF_Initialize(rc=rc)
 
    print *, "Component Test 1:"

    cname = "Atmosphere"
    comp1 = ESMF_GridCompCreate(name=cname, gridcompType=ESMF_ATM, &
                                               configFile="grid.rc", rc=rc)  

    print *, "GridComp Create returned, name = ", trim(cname)

    call ESMF_GridCompPrint(comp1, rc=rc)
    print *, "GridComp Print returned"

    ! register other entry points
    call ESMF_GridCompSetServices(comp1, User_SetServices, rc)

    call ESMF_GridCompInitialize(comp1, rc=rc)
    print *, "GridComp Initialize returned"

    call ESMF_GridCompRun(comp1, clock=clock, rc=rc)
    print *, "GridComp Run returned"

    call ESMF_GridCompRun(comp1, clock=clock, rc=rc)
    print *, "GridComp Run returned"

    call ESMF_GridCompFinalize(comp1, rc=rc)
    print *, "GridComp Finalize returned"

    call ESMF_GridCompDestroy(comp1, rc=rc)
    print *, "GridComp Destroy returned"

    print *, "Component Test 1 finished"

    call ESMF_Finalize(rc=rc)

    end program ESMF_GridCompCreateTest
    
!\end{verbatim}
    
