! $Id: user_model.F90,v 1.7 2003/03/10 05:40:49 cdeluca Exp $
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
    use ESMF_DELayoutMod
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
        type(ESMF_State) :: import, export
        type(ESMF_Field) :: humidity
        type(ESMF_DELayout) :: mylayout
        integer :: de_id                        ! the current DE


        print *, "User Comp Init starting"

        ! This is where the model specific setup code goes.  

        ! Query component for information.
        call ESMF_CompGet(comp, export=export, layout=mylayout, rc=rc)
 
        ! Something to show we are running on different procs
        call ESMF_DELayoutGetDEID(mylayout, de_id, rc)
        print *, "User Comp Init running on DE ", de_id

        ! Add an empty "humidity" field to the export state.
        humidity = ESMF_FieldCreateNoData(name="humidity", rc=rc)
        call ESMF_StateAddData(export, humidity, rc)
        call ESMF_StatePrint(export, rc=rc)

        print *, "User Comp Init returning"
   
    end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
    subroutine user_run(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

!     ! Local variables
        type(ESMF_State) :: myimport, myexport
        type(ESMF_Field) :: humidity
        type(ESMF_DELayout) :: mylayout
        integer, save :: onetime=1              ! static variable
        integer :: de_id                        ! the current DE
        integer :: status

        print *, "User Comp Run starting"

        ! In a real application, the coupler would move the export from
        ! one component to the import before this call.  For now, copy the
        ! field from the export state to import state by hand.
        if (onetime .gt. 0) then
          call ESMF_CompGet(comp, import=myimport, export=myexport, rc=status)
          call ESMF_StateGetData(myexport, "humidity", humidity, rc=status)
          call ESMF_StateAddData(myimport, humidity, rc=status)
          onetime = 0
        endif

        ! Get information from the component.
        call ESMF_CompGet(comp, import=myimport, export=myexport, &
                                                  layout=mylayout, rc=status)
        ! Something to show we are running on different procs
        call ESMF_DELayoutGetDEID(mylayout, de_id, rc=status)
        print *, "User Comp Run running on DE ", de_id

        call ESMF_StatePrint(myimport, rc=status)
        call ESMF_StateGetData(myimport, "humidity", humidity, rc=status)
        call ESMF_FieldPrint(humidity, "", rc=status)

        ! This is where the model specific computation goes.



        ! Here is where the output state is updated.
        call ESMF_StateAddData(myexport, humidity, rc=status)
        call ESMF_StatePrint(myexport, rc=status)
 
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
        type(ESMF_DELayout) :: mylayout
        integer :: de_id                        ! the current DE

        print *, "User Comp Final starting"
    
        ! Query component for information.
        call ESMF_CompGet(comp, layout=mylayout, rc=rc)
 
        ! Something to show we are running on different procs
        call ESMF_DELayoutGetDEID(mylayout, de_id, rc)
        print *, "User Comp Final running on DE ", de_id

        print *, "User Comp Final returning"
   
    end subroutine user_final


    end module user_model
    
!\end{verbatim}
    
