! $Id: user_model.F90,v 1.4 2004/04/15 19:55:59 nscollins Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Component
!
!
!\begin{verbatim}

    module user_model

    ! ESMF Framework module
    use ESMF_Mod

    implicit none
    
    public user_register
        
    type mydata 
      integer :: index
      real :: scale_factor
      integer :: flag
    end type

    type wrapper
      type(mydata), pointer :: wrap
    end type

    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine user_register(comp, rc)
        type(ESMF_GridComp) :: comp
        integer :: rc

        ! Local variables
        type(mydata), pointer :: mydatablock
        type(wrapper) :: mywrapper

        print *, "In user register routine"

        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, user_init1, 1, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, user_init2, 2, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, user_run, &
                                                          ESMF_SINGLEPHASE, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, &
                                                          ESMF_SINGLEPHASE, rc)

        print *, "Registered Initialize, Run, and Finalize routines"

        ! Set Internal State

        allocate(mydatablock)
        mydatablock%index = 13
        mydatablock%scale_factor = 0.66
        mydatablock%flag = 25

        mywrapper%wrap => mydatablock

        call ESMF_GridCompSetInternalState(comp, mywrapper, rc)

        print *, "Registered local datablock"
        print *, " initial data =", mydatablock%index, &
                     mydatablock%scale_factor, mydatablock%flag

        rc = ESMF_SUCCESS

    end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   first Initialization routine.
 
    
    subroutine user_init1(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer :: rc

        print *, "first init routine called"
        rc = ESMF_SUCCESS

    end subroutine user_init1

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   second and main Initialization routine.
 
    
    subroutine user_init2(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer :: rc

!     ! Local variables
        type(ESMF_Field) :: humidity
        type(mydata), pointer :: mydatablock
        type(wrapper) :: mywrapper
        integer :: dtype

        print *, "User Comp Init 2 starting"

        ! This is where the model specific setup code goes.  

        call ESMF_GridCompPrint(comp, "", rc)
        call ESMF_StatePrint(exportState, "", rc)

        print *, "init, ready to call get data ptr"
        nullify(mydatablock)
        mywrapper%wrap => mydatablock
        call ESMF_GridCompGetInternalState(comp, mywrapper, rc)
        mydatablock => mywrapper%wrap
        print *, "init, back from data ptr"

        print *, "init, data = ", mydatablock%index, &
                     mydatablock%scale_factor, mydatablock%flag

        ! Add an empty "humidity" field to the export state.
        humidity = ESMF_FieldCreateNoData(name="humidity", rc=rc)
        call ESMF_StateAddData(exportState, humidity, rc)
        call ESMF_StatePrint(exportState, rc=rc)

        print *, "User Comp Init returning"
   
        rc = ESMF_SUCCESS

    end subroutine user_init2


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
    subroutine user_run(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer :: rc

        type(ESMF_Field) :: humidity
        integer, save :: onetime=1              ! static variable

        integer :: status
        integer :: dtype
        type(mydata), pointer :: mydatablock
        type(wrapper) :: mywrapper

        print *, "User Comp Run starting"

        ! In a real application, the coupler would move the export from
        ! one component to the import before this call.  For now, copy the
        ! field from the export state to import state by hand.
        if (onetime .gt. 0) then
          call ESMF_StateGetData(exportState, "humidity", humidity, rc=status)
          call ESMF_StateAddData(importState, humidity, rc=status)
          onetime = 0
        endif

        ! Get private data block
        print *, "in run, ready to get data block"
        nullify(mydatablock)
        mywrapper%wrap => mydatablock
        call ESMF_GridCompGetInternalState(comp, mywrapper, rc)
        mydatablock => mywrapper%wrap

        print *, "run, local data =", mydatablock%index, &
                        mydatablock%scale_factor, mydatablock%flag
   
        call ESMF_StatePrint(importState, rc=status)
        call ESMF_StateGetData(importState, "humidity", humidity, rc=status)
        call ESMF_FieldPrint(humidity, "", rc=status)

        ! This is where the model specific computation goes.

        ! Here is where the output state is updated.
        !call ESMF_StateAddData(exportState, humidity, rc=status)
        call ESMF_StatePrint(exportState, rc=status)
 
        print *, "User Comp Run returning"

        rc = status

    end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine user_final(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer :: rc

!     ! Local variables
        type(mydata), pointer :: mydatablock
        type(wrapper) :: mywrapper

        print *, "User Comp Final starting"
    
        ! Query component for information.
        print *, "final, ready to call get data ptr"
        nullify(mydatablock)
        mywrapper%wrap => mydatablock
        call ESMF_GridCompGetInternalState(comp, mywrapper, rc)
        mydatablock => mywrapper%wrap
   
        print *, "final, local data =", mydatablock%index, &
                        mydatablock%scale_factor, mydatablock%flag
 
        print *, "User Comp Final returning"
   
        rc = ESMF_SUCCESS

    end subroutine user_final


    end module user_model
    
!\end{verbatim}
    
