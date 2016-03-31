! $Id$
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

    module user_model2

    ! ESMF Framework module
    use ESMF

    implicit none
    
    public user_neg_routine, user_setvm, user_register
        
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
!   !  The Negotiation routine
 
  subroutine user_neg_routine(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc
    type(ESMF_VM) :: vm

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "USER: Hello world: User negotiation routine (user model 2)!"

    if(.not. associated(comp%compp)) then
      print *, "ERROR: comp%compp is NOT associated"
    else
      if(comp%compp%negInfo%state == ESMF_COMP_USER_NEG_INIT) then
        ! Fill in the details
        print *, "USER: Setting info that we need accelerators"
        comp%compp%negInfo%accInfo = ESMF_COMP_MUST_ACC
      else if(comp%compp%negInfo%state == ESMF_COMP_USER_NEG_INPROGRESS) then
        if((.not. associated(comp%compp%negInfo%negPetList)) .or.&
          (size(comp%compp%negInfo%negPetList) <= 0)) then
          print *, "ERROR: Negotiation in progress but no PET list"
        else 
          ! Check if the passed petlist works for us
          print *, "USER: Got a PET List :", comp%compp%negInfo%negPetList
          print *, "USER: Reject it the first time"
          rc = ESMF_FAILURE
          return
        end if
      else if(comp%compp%negInfo%state == ESMF_COMP_USER_NEG_FINALIZE) then
        ! last chance
        if((.not. associated(comp%compp%negInfo%negPetList)) .or.&
          (size(comp%compp%negInfo%negPetList) <= 0)) then
          print *, "ERROR: Negotiation in progress but no PET list"
        else 
          ! Check if the passed petlist works for us
          print *, "USER: Got a PET List :", comp%compp%negInfo%negPetList
          print *, "USER: Accepting the provided PET list"
        end if
      else
        ! Unexpected state
        print *, "ERROR: Unexpected negotiation state"
      end if
    end if

    call ESMF_VMGetGlobal(vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail on error
#ifdef ESMF_TESTWITHTHREADS
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail on error
#endif

  end subroutine user_neg_routine
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
    if (rc/=ESMF_SUCCESS) return ! bail on error
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail on error
    if (pthreadsEnabled) then
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail on error
    endif
#endif

  end subroutine

    subroutine user_register(comp, rc)
        type(ESMF_GridComp) :: comp
        integer, intent(out) :: rc

        ! Local variables
        type(mydata), pointer :: mydatablock
        type(wrapper) :: mywrapper

        rc = ESMF_SUCCESS

        print *, "In user register routine (user model 2)"

        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, userRoutine=user_init1,&
          phase=1, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail on error    
        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, userRoutine=user_init2,&
          phase=2, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail on error    
        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, userRoutine=user_run, &
          rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail on error    
        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, userRoutine=user_final, &
          rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail on error    

        print *, "Registered Initialize, Run, and Finalize routines"

        ! Set Internal State

        allocate(mydatablock)
        mydatablock%index = 13
        mydatablock%scale_factor = 0.66
        mydatablock%flag = 25

        mywrapper%wrap => mydatablock

        call ESMF_GridCompSetInternalState(comp, mywrapper, rc)
        if (rc/=ESMF_SUCCESS) return ! bail on error    

        print *, "Registered local datablock"
        print *, " initial data =", mydatablock%index, &
                     mydatablock%scale_factor, mydatablock%flag

    end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   first Initialization routine.
 
    
    subroutine user_init1(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS

        print *, "first init routine called (user model 2)"

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

        rc = ESMF_SUCCESS

        print *, "User Comp Init 2 starting (user model 2)"

        ! This is where the model specific setup code goes.  

        call ESMF_GridCompPrint(comp, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail on error    
        call ESMF_StatePrint(exportState, options="", rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail on error    

        print *, "init, ready to call get data ptr"
        nullify(mydatablock)
        mywrapper%wrap => mydatablock
        call ESMF_GridCompGetInternalState(comp, mywrapper, rc)
        if (rc/=ESMF_SUCCESS) return ! bail on error    
        mydatablock => mywrapper%wrap
        print *, "init, back from data ptr"

        print *, "init, data = ", mydatablock%index, &
                     mydatablock%scale_factor, mydatablock%flag

        ! Add an empty "humidity" field to the export state.
        humidity = ESMF_FieldEmptyCreate(name="humidity", rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail on error    
        call ESMF_StateAdd(exportState, (/humidity/), rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail on error    
        call ESMF_StatePrint(exportState, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail on error    

        print *, "User Comp Init returning"
   
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

        type(mydata), pointer :: mydatablock
        type(wrapper) :: mywrapper

        rc = ESMF_SUCCESS

        print *, "User Comp Run starting (user model 2)"

        ! In a real application, the coupler would move the export from
        ! one component to the import before this call.  For now, copy the
        ! field from the export state to import state by hand.
        if (onetime .gt. 0) then
          call ESMF_StateGet(exportState, "humidity", humidity, rc=rc)
          if (rc/=ESMF_SUCCESS) return ! bail on error    
          call ESMF_StateAdd(importState, (/humidity/), rc=rc)
          if (rc/=ESMF_SUCCESS) return ! bail on error    
          onetime = 0
        endif

        ! Get private data block
        print *, "in run, ready to get data block"
        nullify(mydatablock)
        mywrapper%wrap => mydatablock
        call ESMF_GridCompGetInternalState(comp, mywrapper, rc)
        if (rc/=ESMF_SUCCESS) return ! bail on error    
        mydatablock => mywrapper%wrap

        print *, "run, local data =", mydatablock%index, &
                        mydatablock%scale_factor, mydatablock%flag
   
        call ESMF_StatePrint(importState, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail on error    
        call ESMF_StateGet(importState, "humidity", humidity, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail on error    
        call ESMF_FieldPrint(humidity, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail on error    

        ! This is where the model specific computation goes.

        ! Here is where the output state is updated.
        !call ESMF_StateAdd(exportState, humidity, rc=status)
        call ESMF_StatePrint(exportState, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail on error    

        print *, "User Comp Run returning"

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
        type(ESMF_Field) :: humidity

        rc = ESMF_SUCCESS

        print *, "User Comp Final starting (user model 2)"
    
        ! Query component for information.
        print *, "final, ready to call get data ptr"
        nullify(mydatablock)
        mywrapper%wrap => mydatablock
        call ESMF_GridCompGetInternalState(comp, mywrapper, rc)
        if (rc/=ESMF_SUCCESS) return ! bail on error    
        mydatablock => mywrapper%wrap
   
        print *, "final, local data =", mydatablock%index, &
                        mydatablock%scale_factor, mydatablock%flag

        ! garbage collection 
        deallocate(mydatablock)
        call ESMF_StateGet(importState, "humidity", humidity, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail on error    
        call ESMF_FieldDestroy(humidity, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail on error    
        
        print *, "User Comp Final returning"
   
    end subroutine user_final


    end module user_model2
    
!\end{verbatim}
    
