! $Id: user_coupler.F90,v 1.1 2003/03/02 19:01:45 nscollins Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Coupler, Version A (minimalist)
!
!
!\begin{verbatim}

    module user_coupler
    
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
    
    public usercpl_register
        
    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine usercpl_register(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

        print *, "in user register routine"

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
        type(ESMF_State) :: mystatelist(2)
        type(ESMF_Field) :: humidity
        type(ESMF_Layout) :: mylayout
        integer :: de_id                        ! the current DE


        print *, "User Coupler Init starting"

        ! This is where the model specific setup code goes.  

        ! Query component for information.
        call ESMF_CompGet(comp, statelist=mystatelist, layout=mylayout, rc=rc)
 
        ! Something to show we are running on different procs
        call ESMF_LayoutGetDEId(mylayout, de_id, rc)
        print *, "User Coupler Init running on DE ", de_id

        ! Print initial states
        call ESMF_StatePrint(mystatelist(1), rc=rc)
        call ESMF_StatePrint(mystatelist(2), rc=rc)

        print *, "User Coupler Init returning"
   
    end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !
 
    subroutine user_run(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

!     ! Local variables
        type(ESMF_State) :: mystatelist(2), mysource, mydest
        type(ESMF_Field) :: humidity
        type(ESMF_Layout) :: mylayout
        integer :: de_id                        ! the current DE
        integer :: status

        print *, "User Coupler Run starting"

        ! Something to show we are running on different procs
        call ESMF_CompGet(comp, layout=mylayout, rc=status)
        call ESMF_LayoutGetDEId(mylayout, de_id, rc=status)
        print *, "User Coupler Run running on DE ", de_id

        ! Get information from the component.
        call ESMF_CompGet(comp, statelist=mystatelist, rc=status)
        mysource = mystatelist(1)
        mydest = mystatelist(2)

        ! Get input data
        call ESMF_StatePrint(mysource, rc=status)
        call ESMF_StateGetData(mysource, "humidity", humidity, rc=status)
        call ESMF_FieldPrint(humidity, "", rc=status)

        ! This is where the model specific computation goes.

        ! Set output data
        call ESMF_StateAddData(mydest, humidity, rc=status)
        call ESMF_StatePrint(mydest, rc=status)

 
        print *, "User Coupler Run returning"

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

        print *, "User Coupler Final starting"
    
        ! Query component for information.
        call ESMF_CompGet(comp, layout=mylayout, rc=rc)
 
        ! Something to show we are running on different procs
        call ESMF_LayoutGetDEId(mylayout, de_id, rc)
        print *, "User Coupler Final running on DE ", de_id

        print *, "User Coupler Final returning"
   
    end subroutine user_final


    end module user_coupler
    
!\end{verbatim}
    
