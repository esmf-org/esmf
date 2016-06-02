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

    implicit none
    
    public user_neg_routine, user_neg_comp, user_comp_recreate
    
    contains

    subroutine user_neg_routine(vm, comps, user_setvms, rc)
      type(ESMF_VM), intent(in) :: vm
      type(ESMF_GridComp), intent(inout) :: comps(:)
      type (ESMF_SetVMInterfaceType), intent(in) :: user_setvms(:)
      integer, intent(out) :: rc

      integer :: i

      if(size(comps) /= size(user_setvms)) then
        print *, "ERROR: Not enough setvm routines provided"
        rc = ESMF_FAILURE
        return
      end if

      print *, "Performing negotiation for ", size(comps), " components"
      rc = ESMF_SUCCESS
      do i=1,size(comps)
        print *, "Performing res neg for comp ", i
        call user_neg_comp(vm, comps(i), user_setvms(i)%pfunc, rc)
        if(rc /= ESMF_SUCCESS) then
          print *, "Resource negotiation failed for component ", i
          exit
        end if
      end do

      if(rc /= ESMF_SUCCESS) then
        print *, "Resource negotiation failed, exiting..."
        return
      end if
    end subroutine user_neg_routine
        
    subroutine user_neg_comp(vm, comp, user_setvm, rc)
      type(ESMF_VM), intent(in) :: vm
      type(ESMF_GridComp), intent(inout) :: comp
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
      allocate(pet_list(pet_count))
      do i=1,pet_count
        pet_list(i) = i-1
      end do

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
        valueList=pet_list, rc=rc)
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

      call user_comp_recreate(comp, petList=pet_list, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Recreating user component failed"
        return
      end if

    end subroutine user_neg_comp

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

      print *, "Recreating gridcomp"
      if(present(petList)) then
        ! Use the petList in the argument
        gridcomp = ESMF_GridCompCreate(name=cname, petList=petList, rc=rc)
      else
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
    
