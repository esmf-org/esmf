! $Id: user_model1.F90,v 1.2 2003/03/03 17:33:39 nscollins Exp $
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

    module user_model1
    
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
    
    public userm1_register
        
    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine userm1_register(comp, rc)
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
        type(ESMF_State) :: myexport
        type(ESMF_Field) :: humidity


        print *, "User Comp Init starting"

        ! This is where the model specific setup code goes.  

        ! Query component for information.
        call ESMF_CompGet(comp, export=myexport, rc=rc)

        ! Add an empty "humidity" field to the export state.
        humidity = ESMF_FieldCreateNoData(name="humidity", rc=rc)
        call ESMF_StateAddData(myexport, humidity, rc)
        call ESMF_StatePrint(myexport, rc=rc)

        print *, "User Comp Init returning"
   
    end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
    subroutine user_run(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

!     ! Local variables
        type(ESMF_State) :: myexport
        type(ESMF_Field) :: humidity
        integer :: status

        print *, "User Comp Run starting"


        ! This is where the model specific computation goes.


        ! Here is where the output state is updated.
        call ESMF_CompGet(comp, export=myexport, rc=status)
        call ESMF_StateGetData(myexport, "humidity", humidity, rc=status)

        ! update field values here

        call ESMF_StatePrint(myexport, rc=status)
        call ESMF_FieldPrint(humidity, rc=status)
 
        print *, "User Comp Run returning"

        rc = status

    end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine user_final(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

        print *, "User Comp Final starting"
    
        print *, "User Comp Final returning"
   
    end subroutine user_final


    end module user_model1
    
!\end{verbatim}
    
