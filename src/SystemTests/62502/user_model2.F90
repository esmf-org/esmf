! $Id: user_model2.F90,v 1.1 2003/03/02 19:01:45 nscollins Exp $
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

    module user_model2
    
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
    
    public userm2_register
        
    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine userm2_register(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

        print *, "In user register routine"

        ! Register the callback routines.

        call ESMF_CompSetRoutine(comp, ESMF_CALLINIT, 1, user_init, rc)
        call ESMF_CompSetRoutine(comp, ESMF_CALLRUN, 1, user_run, rc)
        call ESMF_CompSetRoutine(comp, ESMF_CALLFINAL, 1, user_final, rc)

        print *, "Registered Initialize, Run, and Finalize routines"

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
        type(ESMF_State) :: myimport
        type(ESMF_Field) :: humidity
        type(ESMF_Layout) :: mylayout
        integer :: de_id                        ! the current DE


        print *, "User Comp Init starting"

        ! This is where the model specific setup code goes.  

        ! Query component for information.
        call ESMF_CompGet(comp, layout=mylayout, rc=rc)
 
        ! Something to show we are running on different procs
        call ESMF_LayoutGetDEId(mylayout, de_id, rc)
        print *, "User Comp Init running on DE ", de_id

        ! Initially import state is empty.
        call ESMF_CompGet(comp, import=myimport, rc=rc)
        call ESMF_StatePrint(myimport, rc=rc)

        print *, "User Comp Init returning"
   
    end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
    subroutine user_run(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

!     ! Local variables
        type(ESMF_State) :: myimport
        type(ESMF_Field) :: humidity
        type(ESMF_Layout) :: mylayout
        integer :: de_id                        ! the current DE
        integer :: status

        print *, "User Comp Run starting"

        ! Something to show we are running on different procs
        call ESMF_CompGet(comp, import=myimport, layout=mylayout, rc=status)
        call ESMF_LayoutGetDEId(mylayout, de_id, rc=status)
        print *, "User Comp Run running on DE ", de_id

        ! Get information from the component.
        call ESMF_StatePrint(myimport, rc=status)
        call ESMF_StateGetData(myimport, "humidity", humidity, rc=status)
        call ESMF_FieldPrint(humidity, "", rc=status)

        ! This is where the model specific computation goes.


 
        print *, "User Comp Run returning"

        rc = status

    end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine user_final(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

!     ! Local variables
        type(ESMF_Layout) :: mylayout
        integer :: de_id                        ! the current DE

        print *, "User Comp Final starting"
    
        ! Query component for information.
        call ESMF_CompGet(comp, layout=mylayout, rc=rc)
 
        ! Something to show we are running on different procs
        call ESMF_LayoutGetDEId(mylayout, de_id, rc)
        print *, "User Comp Final running on DE ", de_id

        print *, "User Comp Final returning"
   
    end subroutine user_final


    end module user_model2
    
!\end{verbatim}
    
