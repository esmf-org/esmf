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

    module user_model1

    ! ESMF Framework module
    use ESMF
    use user_neg_info

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

    ! This is the device list negotiated with the driver
    integer, dimension(:), allocatable :: device_list

    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
!   !   The user_setvm() callback routine is called multiple times by
!   !   the driver to negotiate PET list and device list with the component
  subroutine user_setvm(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc

    integer :: i, pet_count, pet_list_info_type, device_count, dev_list_info_type
    integer, allocatable :: pet_list(:)
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
#endif

    integer :: neg_state

    ! Initialize return code
    rc = ESMF_SUCCESS
    print *, "USER: user_setvm called (user_model1)"

    call ESMF_AttributeGet(comp, name="ESMF_COMP_USER_NEG_STATE",&
          value=neg_state, rc=rc)
    if(rc /= ESMF_SUCCESS) then
      print *, "Getting Negotiation state failed"
      return
    end if

    if(neg_state == ESMF_COMP_USER_NEG_INIT) then
      print *, "Negotiation state : INIT"
      call ESMF_AttributeSet(comp, name="ESMF_COMP_USER_NEG_ACC_INFO",&
            value=ESMF_COMP_CAN_ACC, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Setting Negotiation acc info failed"
        return
      end if
    else if(neg_state == ESMF_COMP_USER_NEG_INPROGRESS) then
      print *, "Negotiation state : INPROGRESS"
      ! Get PET list
      call ESMF_AttributeGet(comp, name="ESMF_COMP_USER_NEG_PETLIST_INFO_TYPE",&
        value=pet_list_info_type, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Getting pet list info type failed, exiting..."
        return
      end if
      if(pet_list_info_type == ESMF_COMP_USER_NEG_LIST_INFO_ENUMERATE) then
        call ESMF_AttributeGet(comp, name="ESMF_COMP_USER_NEG_PETLIST_INFO_SIZE",&
          value=pet_count, rc=rc)
        if(rc /= ESMF_SUCCESS) then
          print *, "Getting pet list info type failed, exiting..."
          return
        end if
        if(pet_count <= 0) then
          print *, "Pet list info size <= 0, exiting"
          rc = ESMF_FAILURE
          return
        end if
        allocate(pet_list(pet_count))
        call ESMF_AttributeGet(comp, name="ESMF_COMP_USER_NEG_PETLIST_INFO",&
          valueList=pet_list, rc=rc)
        if(rc /= ESMF_SUCCESS) then
          print *, "Setting pet list info failed, exiting..."
          return
        end if
        print *, "Got Petlist : ", pet_list
        print *, "Accepting the petlist passed by driver/main"
      else
        print *, "ERROR: Only LIST_INFO of type ENUMERATE is supported for now"
        return
      end if

      ! We don't need to save the PET list because as long as we return a 
      ! success the driver will recreate this components with the negotiated
      ! pet list

      ! Get device list
      call ESMF_AttributeGet(comp, name="ESMF_COMP_USER_NEG_DEVLIST_INFO_TYPE",&
        value=dev_list_info_type, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Getting pet list info type failed, exiting..."
        return
      end if
      if(dev_list_info_type == ESMF_COMP_USER_NEG_LIST_INFO_ENUMERATE) then
        call ESMF_AttributeGet(comp, name="ESMF_COMP_USER_NEG_DEVLIST_INFO_SIZE",&
          value=device_count, rc=rc)
        if(rc /= ESMF_SUCCESS) then
          print *, "Getting pet list info type failed, exiting..."
          return
        end if
        if(device_count <= 0) then
          print *, "dev list info size <= 0"
        else
          allocate(device_list(device_count))
          call ESMF_AttributeGet(comp, name="ESMF_COMP_USER_NEG_DEVLIST_INFO",&
            valueList=device_list, rc=rc)
          if(rc /= ESMF_SUCCESS) then
            print *, "Setting device list info failed, exiting..."
            return
          end if
          print *, "Got device list : ", device_list
          print *, "Accepting the device list passed by driver/main"
        end if
      else
        print *, "ERROR: Only LIST_INFO of type ENUMERATE is supported for now"
        return
      end if

      rc = ESMF_SUCCESS
    else if(neg_state == ESMF_COMP_USER_NEG_FINALIZE) then
      print *, "Negotiation state : FINALIZE"
      rc = ESMF_SUCCESS
    else
      print *, "ERROR : INVALID Negotiation state"
      rc = ESMF_FAILURE
    end if

  end subroutine

    subroutine user_register(comp, rc)
        type(ESMF_GridComp) :: comp
        integer, intent(out) :: rc

        ! Local variables
        type(mydata), pointer :: mydatablock
        type(wrapper) :: mywrapper

        rc = ESMF_SUCCESS

        print *, "In user register routine"

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

        print *, "first init routine called"

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

        print *, "User Comp Init 2 starting"

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
        use mpi
        use openacc
        implicit none
        type(ESMF_GridComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer, intent(out) :: rc

        type(ESMF_Field) :: humidity
        integer, save :: onetime=1              ! static variable

        type(mydata), pointer :: mydatablock
        type(wrapper) :: mywrapper

        integer :: i, ndevices, deviceid, rank
        integer, parameter :: INIT_VAL = 2
        integer, parameter :: N = 100
        integer, parameter :: SCALE_FACTOR = 10
        integer, dimension(:) :: vec(N), svec(N)

        rc = ESMF_SUCCESS

        print *, "User Comp Run starting"

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
        call MPI_Comm_rank(MPI_COMM_WORLD, rank, rc)
        ndevices = 0
        if(allocated(device_list)) then
          ndevices = SIZE(device_list)
        end if
        print *, "Num of devices available = ", ndevices
        deviceid = 0
        if(ndevices > 0) then
          ! Use device in the list based on the proc rank
          deviceid = device_list(mod(rank, ndevices) + 1)
          ! Set the device used by this PET
          call acc_set_device_num(deviceid, acc_device_not_host)
        end if
        vec = INIT_VAL
        print *, "Vector before scaling = ", vec

        !$acc kernels loop
        do i=1,N
          svec(i) = SCALE_FACTOR * vec(i)
        end do

        print *, "Scaled vector = ", svec
   

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

        print *, "User Comp Final starting"
    
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


    end module user_model1
    
!\end{verbatim}
    
