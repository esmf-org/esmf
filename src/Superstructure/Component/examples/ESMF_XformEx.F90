! $Id: ESMF_XformEx.F90,v 1.2 2003/02/04 20:19:24 nscollins Exp $
!
! Example code for creating Xforms.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! Examples of creating {\tt Xforms}.
!
!
!\begin{verbatim}

    program ESMF_XformExample
    
!   ! Some common definitions.  This requires the C preprocessor.
    #include "ESMF.h"

!   ! Other ESMF modules which are needed by Xforms
    use ESMF_IOMod
    use ESMF_ArrayMod
    use ESMF_FieldMod
    use ESMF_BundleMod
    use ESMF_StateMod
    use ESMF_XformMod
    
    implicit none
    
!   ! Local variables
    integer :: x, y, rc
    character(ESMF_MAXSTR) :: cname, sname, bname, fname
    type(ESMF_State) :: state1, state2
    type(ESMF_Xform) :: xformlist(2)
        
!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  Initializing a Transform.
 
    print *, "Xform Example 1"

    ! The third arguments here are names of subroutines.
    call ESMF_XformInit(xformlist(1), "AtmToOcn", A2OCPLxform, rc)  
    call ESMF_XformInit(xformlist(2), "OcnToAtm", O2ACPLxform, rc)  

    print *, "Xform Example 1 finished"


!-------------------------------------------------------------------------
!   ! Example 2:
!   !
!   !  Calling a Transform from within a concurrently running Component
 
    print *, "Xform Example 2: Using a Xform from within a Component"

    call ESMF_StateTransform(state1, "AtmToOcn", xformlist) 

    ! When this returns, the transform code has been executed.

    print *, "Xform Example 2 finished"



    end program ESMF_XformExample
    
!\end{verbatim}
    
