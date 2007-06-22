! $Id: ESMF_CompRun.F90,v 1.11 2007/06/22 23:21:48 cdeluca Exp $
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

    program ESMF_InternGridCompCreateTest
    
!   ! ESMF Framework module
    use ESMF_Mod

    use UserInternGridCompMod, only: User_SetServices
    
    implicit none
    
!   ! Local variables
    integer :: rc
    character(ESMF_MAXSTR) :: cname
    type(ESMF_InternGridComp) :: comp1
    type(ESMF_Clock) :: clock
        
!-------------------------------------------------------------------------
!   ! Test 1:
!   !
!   !  Quick Test - Create, Init, Run, Finalize, Destroy a Component.

    call ESMF_Initialize(rc=rc)
 
    print *, "Component Test 1:"

    cname = "Atmosphere"
    comp1 = ESMF_InternGridCompCreate(name=cname, interngridcompType=ESMF_ATM, &
                                               configFile="interngrid.rc", rc=rc)  

    print *, "InternGrid Comp Create returned, name = ", trim(cname)

    call ESMF_InternGridCompPrint(comp1, rc=rc)
    print *, "InternGrid Comp Print returned"

    ! register other entry points
    call ESMF_InternGridCompSetServices(comp1, User_SetServices, rc)

    call ESMF_InternGridCompInitialize(comp1, rc=rc)
    print *, "InternGrid Comp Initialize returned"

    call ESMF_InternGridCompRun(comp1, clock=clock, rc=rc)
    print *, "InternGrid Comp Run returned"

    call ESMF_InternGridCompRun(comp1, clock=clock, rc=rc)
    print *, "InternGrid Comp Run returned"

    call ESMF_InternGridCompFinalize(comp1, rc=rc)
    print *, "InternGrid Comp Finalize returned"

    call ESMF_InternGridCompDestroy(comp1, rc=rc)
    print *, "InternGrid Comp Destroy returned"

    print *, "Component Test 1 finished"


    call ESMF_Finalize(rc=rc)

    end program ESMF_InternGridCompCreateTest
    
!\end{verbatim}
    
