! $Id: user_coupler.F90,v 1.4 2003/03/24 22:56:24 nscollins Exp $
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
    use ESMF_DELayoutMod
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

        print *, "User Coupler Init starting"

        ! This is where the model specific setup code goes.  

        ! Query component for information.
        call ESMF_CompGet(comp, statelist=mystatelist, rc=rc)
 
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
        type(ESMF_Field) :: humidity1, humidity2
        type(ESMF_DELayout) :: cpllayout

       
        integer :: status

        print *, "User Coupler Run starting"

        ! Get information from the component.
        call ESMF_CompGet(comp, statelist=mystatelist, layout=cpllayout, &
                                                                  rc=status)
        mysource = mystatelist(1)
        mydest = mystatelist(2)

        ! Get input data
        call ESMF_StatePrint(mysource, rc=status)
        call ESMF_StateGetData(mysource, "humidity", humidity1, rc=status)

        ! Get location of output data
        call ESMF_StatePrint(mydest, rc=status)
        call ESMF_StateGetData(mydest, "humidity", humidity2, rc=status)


        ! These are fields on different layouts - call Route to rearrange
        !  the data using the Comm routines.
        call ESMF_FieldRoute(humidity1, humidity2, cpllayout, status)


        ! Set output data
        call ESMF_StateAddData(mydest, humidity2, rc=status)
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

        print *, "User Coupler Final starting"
    
        print *, "User Coupler Final returning"
   
    end subroutine user_final


    end module user_coupler
    
!\end{verbatim}
    
