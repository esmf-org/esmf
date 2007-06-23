! $Id: ESMF_CompRun.F90,v 1.12 2007/06/23 04:01:00 cdeluca Exp $
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

    program ESMF_IGridCompCreateTest
    
!   ! ESMF Framework module
    use ESMF_Mod

    use UserIGridCompMod, only: User_SetServices
    
    implicit none
    
!   ! Local variables
    integer :: rc
    character(ESMF_MAXSTR) :: cname
    type(ESMF_IGridComp) :: comp1
    type(ESMF_Clock) :: clock
        
!-------------------------------------------------------------------------
!   ! Test 1:
!   !
!   !  Quick Test - Create, Init, Run, Finalize, Destroy a Component.

    call ESMF_Initialize(rc=rc)
 
    print *, "Component Test 1:"

    cname = "Atmosphere"
    comp1 = ESMF_IGridCompCreate(name=cname, igridcompType=ESMF_ATM, &
                                               configFile="igrid.rc", rc=rc)  

    print *, "IGrid Comp Create returned, name = ", trim(cname)

    call ESMF_IGridCompPrint(comp1, rc=rc)
    print *, "IGrid Comp Print returned"

    ! register other entry points
    call ESMF_IGridCompSetServices(comp1, User_SetServices, rc)

    call ESMF_IGridCompInitialize(comp1, rc=rc)
    print *, "IGrid Comp Initialize returned"

    call ESMF_IGridCompRun(comp1, clock=clock, rc=rc)
    print *, "IGrid Comp Run returned"

    call ESMF_IGridCompRun(comp1, clock=clock, rc=rc)
    print *, "IGrid Comp Run returned"

    call ESMF_IGridCompFinalize(comp1, rc=rc)
    print *, "IGrid Comp Finalize returned"

    call ESMF_IGridCompDestroy(comp1, rc=rc)
    print *, "IGrid Comp Destroy returned"

    print *, "Component Test 1 finished"


    call ESMF_Finalize(rc=rc)

    end program ESMF_IGridCompCreateTest
    
!\end{verbatim}
    
