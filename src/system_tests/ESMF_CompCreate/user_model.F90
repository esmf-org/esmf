! $Id: user_model.F90,v 1.24 2009/05/29 19:24:42 theurich Exp $
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
    
    public user_setvm, user_register
        
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
 
  subroutine user_setvm(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for
    ! your own code development you probably don't want to include the
    ! following call unless you are interested in exploring ESMF's
    ! threading features.

    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetGlobal(vm, rc=rc)
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (pthreadsEnabled) then
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    endif
#endif

  end subroutine

    subroutine user_register(comp, rc)
        type(ESMF_GridComp) :: comp
        integer, intent(out) :: rc

        ! Local variables
        type(mydata), pointer :: mydatablock
        type(wrapper) :: mywrapper

        print *, "In user register routine"

        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, userRoutine=user_init1,&
          phase=1, rc=rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, userRoutine=user_init2,&
          phase=2, rc=rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, userRoutine=user_run, &
          rc=rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, userRoutine=user_final, &
          rc=rc)

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
        integer, intent(out) :: rc

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
        integer, intent(out) :: rc

!     ! Local variables
        type(ESMF_Field) :: humidity
        type(mydata), pointer :: mydatablock
        type(wrapper) :: mywrapper

        print *, "User Comp Init 2 starting"

        ! This is where the model specific setup code goes.  

        call ESMF_GridCompPrint(comp, "", rc=rc)
        call ESMF_StatePrint(exportState, "", rc=rc)

        print *, "init, ready to call get data ptr"
        nullify(mydatablock)
        mywrapper%wrap => mydatablock
        call ESMF_GridCompGetInternalState(comp, mywrapper, rc)
        mydatablock => mywrapper%wrap
        print *, "init, back from data ptr"

        print *, "init, data = ", mydatablock%index, &
                     mydatablock%scale_factor, mydatablock%flag

        ! Add an empty "humidity" field to the export state.
        humidity = ESMF_FieldCreateEmpty(name="humidity", rc=rc)
        call ESMF_StateAdd(exportState, humidity, rc=rc)
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
        integer, intent(out) :: rc

        type(ESMF_Field) :: humidity
        integer, save :: onetime=1              ! static variable

        integer :: status
        type(mydata), pointer :: mydatablock
        type(wrapper) :: mywrapper

        print *, "User Comp Run starting"

        ! In a real application, the coupler would move the export from
        ! one component to the import before this call.  For now, copy the
        ! field from the export state to import state by hand.
        if (onetime .gt. 0) then
          call ESMF_StateGet(exportState, "humidity", humidity, rc=status)
          call ESMF_StateAdd(importState, humidity, rc=status)
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
        call ESMF_StateGet(importState, "humidity", humidity, rc=status)
        call ESMF_FieldPrint(humidity, "", rc=status)

        ! This is where the model specific computation goes.

        ! Here is where the output state is updated.
        !call ESMF_StateAdd(exportState, humidity, rc=status)
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
        integer, intent(out) :: rc

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
    
