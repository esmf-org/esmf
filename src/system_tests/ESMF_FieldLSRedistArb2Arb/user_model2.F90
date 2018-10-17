! $Id$
!
! Example/test code which shows User Component calls.

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

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
    
    public userm2_register
        
    contains

!--------------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine userm2_register(comp, rc)
        type(ESMF_GridComp) :: comp
        integer, intent(out) :: rc

        ! local variables

        rc = ESMF_SUCCESS
        print *, "In user register routine"

        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, user_init, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, user_run, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, user_final, rc=rc)
        if(rc/=ESMF_SUCCESS) return

        print *, "Registered Initialize, Run, and Finalize routines"

    end subroutine

!--------------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    subroutine user_init(comp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

!   ! Local variables
      type(ESMF_Field) :: humidity
      type(ESMF_VM) :: vm
      integer, allocatable :: indexList(:)
      type(ESMF_LocStream) :: locs
      type(ESMF_DistGrid)  :: distgrid
      type(ESMF_ArraySpec) :: arrayspec
      integer(ESMF_KIND_I4), dimension(:), pointer :: dstfptr
      integer :: npets, de_id

      rc = ESMF_SUCCESS
      ! Initially import state contains a field with a grid but no data.

      ! Query component for VM and create a layout with the right breakdown
      call ESMF_GridCompGet(comp, vm=vm, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      call ESMF_VMGet(vm, localPet=de_id, petCount=npets, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      print *, de_id, "User Comp 2 Init starting"

        ! Check for correct number of PETs
        if ( npets .ne. 2 ) then
            call ESMF_LogSetError(ESMF_RC_ARG_BAD,&
                msg="This component must run on 2 PETs.",&
                line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
        endif

        if ( de_id .eq. 0 ) then
            allocate(indexList(6))
            indexList = (/1,2,3,4,5,6/)
        elseif ( de_id .eq. 1 ) then
            allocate(indexList(10))
            indexList = (/7,8,9,10,11,12,13,14,15,16/)
        endif

      ! Add a "humidity" field to the import state.
      distgrid = ESMF_DistGridCreate(arbSeqIndexList=indexList, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      deallocate(indexList)

      locs = ESMF_LocStreamCreate(distgrid=distgrid, &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_I4, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      humidity = ESMF_FieldCreate(locs, arrayspec, &
          name="humidity", rc=rc)
      if(rc/=ESMF_SUCCESS) return

      call ESMF_FieldGet(humidity, localDe=0, farrayPtr=dstfptr, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      dstfptr = 0
  
      call ESMF_StateAdd(importState, (/humidity/), rc=rc)
      if(rc/=ESMF_SUCCESS) return
      !   call ESMF_StatePrint(importState, rc=rc)
  
      print *, de_id, "User Comp 2 Init returning"
   
    end subroutine user_init


!--------------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
    subroutine user_run(comp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

!   ! Local variables
      type(ESMF_VM) :: vm
      integer :: de_id
      type(ESMF_Field) :: humidity
      type(ESMF_LocStream) :: locs
      type(ESMF_DistGrid)  :: distgrid
      logical              :: arbIndex
      integer              :: elementCount
      integer, allocatable :: indexList(:)
      integer(ESMF_KIND_I4), dimension(:), pointer :: fptr
      integer                                      :: exlb(1), exub(1), i

      rc = ESMF_SUCCESS

      call ESMF_GridCompGet(comp, vm=vm, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      call ESMF_VMGet(vm, localPet=de_id, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      print *, de_id, "User Comp Run starting"

      ! Get information from the component.
      call ESMF_StateGet(importState, "humidity", humidity, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      call ESMF_FieldGet(humidity, locstream=locs, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      call ESMF_LocStreamGet(locs, distgrid=distgrid, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      call ESMF_DistGridGet(distgrid, localDe=0, arbSeqIndexFlag=arbIndex, &
            elementCount=elementCount, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      allocate(indexList(elementCount))

      call ESMF_DistGridGet(distgrid, localDe=0, seqIndexList=indexList, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      call ESMF_FieldGet(humidity, localDe=0, farrayPtr=fptr, &
            exclusiveLBound=exlb, exclusiveUBound=exub, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      ! Verify that the redist data in dstField(l) is correct.
      ! Before the FieldRedist op, the dst Field contains all 0. 
      ! The FieldRedist op reset the values to the index value, verify this is the case.
      !write(*, '(9I3)') l, lpe, fptr
      do i = exlb(1), exub(1)
          if(fptr(i) .ne. indexList(i)) then
            print *, de_id, "ERROR ", indexList(i), " value =", fptr(i)
            rc = ESMF_FAILURE
          endif
      enddo

      deallocate(indexList)

      print *, de_id, "User Comp Run returning"

    end subroutine user_run


!--------------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine user_final(comp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      ! Local variables
      type(ESMF_VM) :: vm
      integer :: de_id
      type(ESMF_Field) :: field
      type(ESMF_LocStream) :: locs

      rc = ESMF_SUCCESS

      call ESMF_GridCompGet(comp, vm=vm, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      call ESMF_VMGet(vm, localPet=de_id, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      print *, de_id, "User Comp Final starting"  

      ! check validity of results
      ! Get Fields from import state
      call ESMF_StateGet(importState, "humidity", field, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      call ESMF_FieldGet(field, locstream=locs, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      call ESMF_FieldDestroy(field, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      call ESMF_LocStreamDestroy(locs, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      print *, de_id, "User Comp Final returning"
   
    end subroutine user_final

    end module user_model2
    
!\end{verbatim}
    
