! $Id: user_model.F90,v 1.1 2003/02/26 01:18:02 nscollins Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Component, Version A (minimalist)
!
!
!\begin{verbatim}

    module user_model
    
!   ! Some common definitions.  This requires the C preprocessor.
#include "ESMF.h"

!   ! ESMF modules
    use ESMF_IOMod
    use ESMF_LayoutMod
    use ESMF_ClockMod
    use ESMF_FieldMod
    use ESMF_StateMod
    use ESMF_CompMod
    
    implicit none
    
    public user_register
        
    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine user_register(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

        ! Register the callback routines.
        call ESMF_CompSetRoutine(comp, "init", 1, user_init, rc)
        call ESMF_CompSetRoutine(comp, "run", 1, user_run, rc)
        call ESMF_CompSetRoutine(comp, "final", 1, user_final, rc)

        ! If desired, this routine can register a private data block
        ! to be passed in to the routines above:
        ! call ESMF_CompSetData(comp, mydatablock, rc)

    end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
    subroutine user_init(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

!     ! Local variables
        type(ESMF_State), pointer :: import, export

        print *, "User Comp Init starting"
    

        ! This is where the model specific setup code goes.  

        call ESMF_CompGet(comp, export=export, rc=rc)
 
        ! If the initial Export state needs to be filled, do it here.

        print *, "User Comp Init returning"
   
    end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is exchanged.
!   !
 
    subroutine user_run(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

!     ! Local variables
        type(ESMF_State), pointer :: import, export
        type(ESMF_Field) :: humidity

        print *, "User Comp Run starting"

        call ESMF_CompGet(comp, import=import, rc=rc)
        call ESMF_StateGetData(import, "humidity", humidity, rc)

        ! This is where the model specific computation goes.


        call ESMF_CompGet(comp, export=export, rc=rc)
        call ESMF_StateAddData(export, humidity, rc)
 
        print *, "User Comp Run returning"

    end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine user_final(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

!     ! Local variables

        print *, "User Comp Final starting"
    
        ! Add whatever code here needed

        print *, "User Comp Final returning"
   
    end subroutine user_final


    end module user_model
    
!\end{verbatim}
    
