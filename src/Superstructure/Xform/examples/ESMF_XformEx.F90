! $Id: ESMF_XformEx.F90,v 1.4 2005/01/06 20:43:59 jwolfe Exp $
!
! Example code for creating Xforms.

!-------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! Examples of creating {\tt Xforms}.
!
!
!\begin{verbatim}

    program ESMF_XformExample
    
    ! ESMF module

    use ESMF_Mod
    
    implicit none
    
!   ! Local variables
    integer :: x, y, rc
    character(ESMF_MAXSTR) :: cname, sname, bname, fname
    type(ESMF_State) :: state1, state2
    type(ESMF_Xform) :: xformlist(2)
        
    ! should be:
    ! external :: A2OCPLxform, O2ACPLxform
    integer :: A2OCPLxform, O2ACPLxform
!   !Set finalrc to success
    integer:: finalrc = ESMF_SUCCESS

    call ESMF_Initialize(rc=rc)

!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  Initializing a Transform.
 
    print *, "Xform Example 1"

    ! The third arguments here are names of subroutines.
    call ESMF_XformInit(xformlist(1), "AtmToOcn", A2OCPLxform, rc)  
    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if
    call ESMF_XformInit(xformlist(2), "OcnToAtm", O2ACPLxform, rc)  
    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    print *, "Xform Example 1 finished"


!-------------------------------------------------------------------------
!   ! Example 2:
!   !
!   !  Calling a Transform from within a concurrently running Component
 
    print *, "Xform Example 2: Using a Xform from within a Component"

    call ESMF_StateTransform(state1, "AtmToOcn", xformlist, rc=rc) 
    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    ! When this returns, the transform code has been executed.

    print *, "Xform Example 2 finished"

    call ESMF_Finalize(rc)

    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_XformEx.F90"
    else
        print *, "FAIL: ESMF_XformEx.F90"
    end if


    end program ESMF_XformExample
    
!\end{verbatim}
    
