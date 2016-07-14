! $Id$
!
! Example/test code which shows User Negotiation calls used by main

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User negotitation modules used by main
!
!
!\begin{verbatim}

    module user_neg

    ! ESMF Framework module
    use ESMF
    use ESMF_TestMod
    use user_neg_info
    use pet_partitioner

    implicit none
    public user_neg_routine, user_comp_recreate
    public user_neg_comp_phase1, user_neg_comp_phase2
    
    contains

    subroutine user_neg_routine(vm, comps, user_setvms, rc)
      type(ESMF_VM), intent(in) :: vm
      type(ESMF_GridComp), intent(inout) :: comps(:)
      type (ESMF_SetVMInterfaceType), intent(in) :: user_setvms(:)
      integer, intent(out) :: rc

      integer, allocatable :: comp_info(:)
      type(ESMF_PetListInfo), allocatable :: comps_pet_info(:)
      integer, allocatable :: comp_pet_list(:)
      integer :: i

      if(size(comps) /= size(user_setvms)) then
        print *, "ERROR: Not enough setvm routines provided"
        rc = ESMF_FAILURE
        return
      end if

      allocate(comp_info(size(comps)))
      print *, "Performing negotiation (phase1) for ", size(comps), " components"
      rc = ESMF_SUCCESS
      do i=1,size(comps)
        print *, "Performing res neg (phase1) for comp ", i
        call user_neg_comp_phase1(vm, comps(i), comp_info(i), user_setvms(i)%pfunc, rc)
        if(rc /= ESMF_SUCCESS) then
          print *, "Resource negotiation (phase1) failed for component ", i
          return
        end if
      end do

      allocate(comps_pet_info(size(comps)))
      call partition_pet_global_list(vm, comp_info, comps_pet_info, ESMF_ACC_PET_PARTITION_CONTIG, rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "ERROR: Partitioning global pet list failed"
      end if

      print *, "Performing negotiation for (phase2) ", size(comps), " components"
      rc = ESMF_SUCCESS
      do i=1,size(comps)
        print *, "PET list for comp ", i, "=", comps_pet_info(i)%pet_list
        print *, "Performing res neg (phase2) for comp ", i
        call user_neg_comp_phase2(vm, comps(i), comps_pet_info(i), user_setvms(i)%pfunc, rc)
        if(rc /= ESMF_SUCCESS) then
          print *, "Resource negotiation (phase2) failed for component ", i
          return
        end if
      end do

      if(rc /= ESMF_SUCCESS) then
        print *, "Resource negotiation failed, exiting..."
        return
      end if
    end subroutine user_neg_routine

    ! Get acc specific info for all components
    subroutine user_neg_comp_phase1(vm, comp, comp_info, user_setvm, rc)
      type(ESMF_VM), intent(in) :: vm
      type(ESMF_GridComp), intent(inout) :: comp
      integer, intent(inout) :: comp_info
      interface
        subroutine user_setvm(gridcomp, rc)
          use ESMF_CompMod
          implicit none
          type(ESMF_GridComp)  :: gridcomp
          integer, intent(out)  :: rc
        end subroutine user_setvm
      end interface

      integer, intent(out) :: rc

      integer :: user_neg_acc_info, pet_count
      integer, allocatable :: pet_list(:)

      integer, parameter :: MAX_USER_NEG_STEPS = 2
      integer :: neg_step, i
      integer :: userrc

      rc = ESMF_SUCCESS

      ! Initialize negotiation - get negotiation info from user
      print *, "Creating attribute for state of negotiation for comp"
      call ESMF_AttributeSet(comp, name="ESMF_COMP_USER_NEG_STATE",&
        value=ESMF_COMP_USER_NEG_INIT, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Creating attribute for state of negotiation failed, exiting..."
        return
      end if
      print *, "Calling user neg routine, phase : INIT"
      call ESMF_GridCompSetVM(comp, user_setvm, userRc=userrc, rc=rc)
      if ((rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS)) then
        print *, "ESMF_GridCompSetVM failed for component"
        rc = ESMF_FAILURE
        return
      end if
      print *, "Getting info from user, phase : INIT"
      call ESMF_AttributeGet(comp, name="ESMF_COMP_USER_NEG_ACC_INFO",&
        value=user_neg_acc_info, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "WARNING: Getting ESMF_COMP_USER_NEG_ACC_INFO failed"
        print *, "WARNING: Assuming component does not support user res negotiation"
        rc = ESMF_SUCCESS
        return
      end if

      comp_info = user_neg_acc_info

      ! Perform user negotiation - with the user - final stage
      call ESMF_AttributeSet(comp, name="ESMF_COMP_USER_NEG_STATE",&
        value=ESMF_COMP_USER_NEG_FINALIZE, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Setting state of negotiation to FINALIZE failed, exiting..."
        return
      end if
      print *, "Calling user neg routine, phase : FINALIZE"
      call ESMF_GridCompSetVM(comp, user_setvm, userRc=userrc, rc=rc)
      if ((rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS)) then
        print *, "ESMF_GridCompSetVM failed for component"
        rc = ESMF_FAILURE
        return
      end if

    end subroutine user_neg_comp_phase1

        
    subroutine user_neg_comp_phase2(vm, comp, comp_pet_info, user_setvm, rc)
      type(ESMF_VM), intent(in) :: vm
      type(ESMF_GridComp), intent(inout) :: comp
      type(ESMF_PetListInfo), intent(in) :: comp_pet_info
      interface
        subroutine user_setvm(gridcomp, rc)
          use ESMF_CompMod
          implicit none
          type(ESMF_GridComp)  :: gridcomp
          integer, intent(out)  :: rc
        end subroutine user_setvm
      end interface

      integer, intent(out) :: rc

      integer :: user_neg_acc_info, pet_count
      !integer, allocatable :: pet_list(:)

      integer, parameter :: MAX_USER_NEG_STEPS = 2
      integer :: neg_step, i
      integer :: userrc

      rc = ESMF_SUCCESS

      ! Initialize negotiation - get negotiation info from user
      print *, "Creating attribute for state of negotiation for comp"
      call ESMF_AttributeSet(comp, name="ESMF_COMP_USER_NEG_STATE",&
        value=ESMF_COMP_USER_NEG_INIT, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Creating attribute for state of negotiation failed, exiting..."
        return
      end if
      print *, "Calling user neg routine, phase : INIT"
      call ESMF_GridCompSetVM(comp, user_setvm, userRc=userrc, rc=rc)
      if ((rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS)) then
        print *, "ESMF_GridCompSetVM failed for component"
        rc = ESMF_FAILURE
        return
      end if
      print *, "Getting info from user, phase : INIT"
      call ESMF_AttributeGet(comp, name="ESMF_COMP_USER_NEG_ACC_INFO",&
        value=user_neg_acc_info, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "WARNING: Getting ESMF_COMP_USER_NEG_ACC_INFO failed"
        print *, "WARNING: Assuming component does not support user res negotiation"
        rc = ESMF_SUCCESS
        return
      end if

      ! Perform user negotiation - with the user
      print *, "Setting NEG info for user"
      call ESMF_AttributeSet(comp, name="ESMF_COMP_USER_NEG_STATE",&
        value=ESMF_COMP_USER_NEG_INPROGRESS, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Setting state of negotiation to PROGRESS failed, exiting..."
        return
      end if

      ! Get Pet count from VM
      call ESMF_VMGet(vm, petCount=pet_count, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Unable to petcount from VM"
        return
      end if

      ! Create a PET list that we can use for negotiation
      !allocate(pet_list(pet_count))
      !do i=1,pet_count
      !  pet_list(i) = i-1
      !end do

      pet_count = size(comp_pet_info%pet_list)

      call ESMF_AttributeSet(comp, name="ESMF_COMP_USER_NEG_PETLIST_INFO_TYPE",&
        value=ESMF_COMP_USER_NEG_PETLIST_INFO_ENUMERATE, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Setting pet list info type failed, exiting..."
        return
      end if
      call ESMF_AttributeSet(comp, name="ESMF_COMP_USER_NEG_PETLIST_INFO_SIZE",&
        value=pet_count, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Setting pet list info type failed, exiting..."
        return
      end if
      call ESMF_AttributeSet(comp, name="ESMF_COMP_USER_NEG_PETLIST_INFO",&
        valueList=comp_pet_info%pet_list, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Setting pet list info failed, exiting..."
        return
      end if

      do i=1,MAX_USER_NEG_STEPS-1
        print *, "Calling user neg routine, phase : INPROGRESS"
        call ESMF_GridCompSetVM(comp, user_setvm, userRc=userrc, rc=rc)
        if (rc .ne. ESMF_SUCCESS) then
          print *, "ESMF_GridCompSetVM failed for component"
          rc = ESMF_FAILURE
          return
        end if
        if(userrc == ESMF_SUCCESS) then
          print *, "User negotiation step succeeded"
        else
          print *, "User negotiation step failed, will retry"
        end if
      end do

      ! Perform user negotiation - with the user - final stage
      call ESMF_AttributeSet(comp, name="ESMF_COMP_USER_NEG_STATE",&
        value=ESMF_COMP_USER_NEG_FINALIZE, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Setting state of negotiation to FINALIZE failed, exiting..."
        return
      end if
      print *, "Calling user neg routine, phase : FINALIZE"
      call ESMF_GridCompSetVM(comp, user_setvm, userRc=userrc, rc=rc)
      if ((rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS)) then
        print *, "ESMF_GridCompSetVM failed for component"
        rc = ESMF_FAILURE
        return
      end if

      call user_comp_recreate(comp, petList=comp_pet_info%pet_list, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Recreating user component failed"
        return
      end if

    end subroutine user_neg_comp_phase2

    subroutine user_comp_recreate(gridcomp, petList, rc)
  ! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout)        :: gridcomp
      integer,                 intent(in), optional       :: petList(:)
      integer,             intent(out), optional          :: rc

      character(len=ESMF_MAXSTR) :: cname
      type(ESMF_VM) :: vm

      print *, "Getting info about the gridcomp : name"
      call ESMF_GridCompGet(gridcomp, name=cname, rc=rc)
      if(rc  /= ESMF_SUCCESS) then
        print *, "Getting info about gridcomp failed"
        return
      end if

      print *, "Destroying old gridcomp"
      call ESMF_GridCompDestroy(gridcomp, rc=rc)
      if(rc  /= ESMF_SUCCESS) then
        print *, "Destroying gridcomp failed"
        return
      end if

      if(present(petList)) then
        ! Use the petList in the argument
        print *, "Recreating gridcomp : petlist = ", petList
        gridcomp = ESMF_GridCompCreate(name=cname, petList=petList, rc=rc)
      else
        print *, "Recreating gridcomp : petlist = ALL Pets"
        gridcomp = ESMF_GridCompCreate(name=cname, rc=rc)
      end if
      if(rc  /= ESMF_SUCCESS) then
        print *, "Recreating gridcomp failed"
        return
      end if

      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS

    end subroutine user_comp_recreate


    end module user_neg
    
!\end{verbatim}
    
