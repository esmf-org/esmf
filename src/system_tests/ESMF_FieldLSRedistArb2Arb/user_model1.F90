! $Id$
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Component, most recent interface revision.
!
!
!\begin{verbatim}

    module user_model1

    ! ESMF Framework module
    use ESMF

    implicit none
    
    public userm1_register
        
    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine userm1_register(comp, rc)
        type(ESMF_GridComp) :: comp
        integer, intent(out) :: rc

        ! local variables

        rc = ESMF_SUCCESS
        print *, "in user register routine"

        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, user_init, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, user_run, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, user_final, rc=rc)
        if(rc/=ESMF_SUCCESS) return

        print *, "Registered Initialize, Run, and Finalize routines"

    end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
    subroutine user_init(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer, intent(out) :: rc

       ! Local variables
        type(ESMF_Field) :: humidity
        type(ESMF_LocStream) :: locs
        integer, allocatable :: indexList(:)
        type(ESMF_DistGrid)  :: distgrid
        type(ESMF_VM) :: vm
        logical              :: arbIndex
        integer              :: elementCount
        type(ESMF_ArraySpec) :: arrayspec
        integer(ESMF_KIND_I4), dimension(:), pointer :: srcfptr
        integer :: npets, de_id
        integer :: elb(1),eub(1),i

        ! Query component for VM and create a layout with the right breakdown
        call ESMF_GridCompGet(comp, vm=vm, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_VMGet(vm, localPet=de_id, petCount=npets, rc=rc)
        if(rc/=ESMF_SUCCESS) return

        rc = ESMF_SUCCESS
        print *, de_id, "User Comp 1 Init starting"

        ! Check for correct number of PETs
        if ( npets .ne. 4 ) then
            call ESMF_LogSetError(ESMF_RC_ARG_BAD,&
                msg="This component must run on 4 PETs.",&
                line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
        endif

        if ( de_id .eq. 0 ) then
            allocate(indexList(4))
            indexList = (/1,2,3,4/)
        elseif ( de_id .eq. 1 ) then
            allocate(indexList(6))
            indexList = (/5,6,7,8,9,10/)
        elseif ( de_id .eq. 2 ) then
            allocate(indexList(0))
!            indexList = (//)
        elseif ( de_id .eq. 3 ) then
            allocate(indexList(6))
            indexList = (/11,12,13,14,15,16/)
        endif

        ! Add a "humidity" field to the export state.
        distgrid = ESMF_DistGridCreate(arbSeqIndexList=indexList, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        deallocate(indexList)

        locs = ESMF_LocStreamCreate(distgrid=distgrid, &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_I4, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        humidity = ESMF_FieldCreate(locs, arrayspec, &
            name="humidity", rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        call ESMF_DistGridGet(distgrid, localDe=0, arbSeqIndexFlag=arbIndex, &
            elementCount=elementCount, rc=rc)
        if(rc/=ESMF_SUCCESS) return

        allocate(indexList(elementCount))

        call ESMF_DistGridGet(distgrid, localDe=0, seqIndexList=indexList, rc=rc)
        if(rc/=ESMF_SUCCESS) return

        call ESMF_FieldGet(humidity, localDe=0, farrayPtr=srcfptr, &
          exclusiveLbound=elb,exclusiveUbound=eub, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        do i=elb(1),eub(1)
           srcfptr(i) = indexList(i)
        enddo

        deallocate(indexList)

        ! Set up a 2D real array
        call ESMF_ArraySpecSet(arrayspec, rank=2, &
                               typekind=ESMF_TYPEKIND_R8)
        if (rc .ne. ESMF_SUCCESS) return

        call ESMF_StateAdd(exportState, (/humidity/), rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
     !   call ESMF_StatePrint(exportState, rc=rc)

        print *, de_id, "User Comp 1 Init returning"

    end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
    subroutine user_run(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer, intent(out) :: rc

       ! Local variables

        rc = ESMF_SUCCESS
        print *, "User Comp Run starting"

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

        ! Local variables
        type(ESMF_Field) :: humidity
        type(ESMF_LocStream) :: locs

        rc = ESMF_SUCCESS
        print *, "User Comp Final starting"

        ! Get our local info
        call ESMF_StateGet(exportState, "humidity", humidity, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_FieldGet(humidity, locstream=locs, rc=rc)
        if(rc/=ESMF_SUCCESS) return

        call ESMF_FieldDestroy(humidity, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_LocStreamDestroy(locs, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        print *, "User Comp Final returning"

    end subroutine user_final


    end module user_model1
    
!\end{verbatim}
    
