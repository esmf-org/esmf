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
        type(ESMF_LocStream) :: locstream
        type(ESMF_VM) :: vm
        type(ESMF_ArraySpec) :: arrayspec
        integer :: npets, de_id
        integer :: clb(1),cub(1),i
        real(ESMF_KIND_R8), pointer :: lon(:),lat(:)
        real(ESMF_KIND_R8), pointer :: srcfptr(:)
        integer :: totalNumPoints=100

        ! Query component for VM and create a layout with the right breakdown
        call ESMF_GridCompGet(comp, vm=vm, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_VMGet(vm, localPet=de_id, petCount=npets, rc=rc)
        if(rc/=ESMF_SUCCESS) return

        rc = ESMF_SUCCESS
        print *, de_id, "User Comp 1 Init starting"

        ! Create the LocStream:  Allocate space for the LocStream object,
        ! define the number and distribution of the locations.
        locstream=ESMF_LocStreamCreate(name="Equatorial Measurements", &
             maxIndex=totalNumPoints, &
             coordSys=ESMF_COORDSYS_SPH_DEG, &
             indexFlag=ESMF_INDEX_GLOBAL, &
             rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
        
        ! Add key data (internally allocating memory).
        call ESMF_LocStreamAddKey(locstream,                 &
             keyName="ESMF:Lat",           &
             KeyTypeKind=ESMF_TYPEKIND_R8, &
             keyUnits="Degrees",           &
             keyLongName="Latitude", rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        call ESMF_LocStreamAddKey(locstream,                 &
             keyName="ESMF:Lon",           &
             KeyTypeKind=ESMF_TYPEKIND_R8, &
             keyUnits="Degrees",           &
             keyLongName="Longitude", rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        ! Get coordinate memory
        call ESMF_LocStreamGetKey(locstream,                 &
             localDE=0,                    &
             keyName="ESMF:Lat",           &
             farray=lat,                   &
             rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        call ESMF_LocStreamGetKey(locstream,                 &
             localDE=0,                    &
             keyName="ESMF:Lon",           &
             farray=lon,                   &
             rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        ! Create Field
        humidity = ESMF_FieldCreate(locstream, ESMF_TYPEKIND_R8, &
            name="humidity", rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        ! Get Field memory
        call ESMF_FieldGet(humidity, localDe=0, farrayPtr=srcfptr, &
             computationalUBound=clb, computationalLBound=cub, &
             rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        ! Set coordinate data and field data
        do i=clb(1),cub(1)
           lon(i)=(i-1)*360.0/REAL(totalNumPoints)
           lat(i)=0.0
           srcfptr(i)=lon(i)/360.0 ! Just set it to this for testing
        enddo

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
    
