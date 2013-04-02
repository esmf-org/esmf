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
        type(ESMF_VM) :: vm
        type(ESMF_Grid) :: grid1
        type(ESMF_ArraySpec) :: arrayspec
        real(ESMF_KIND_R8), dimension(:,:), pointer :: idata, coordX, coordY
        integer :: i, j, counts(2), tlb(2), tub(2)
        real(ESMF_KIND_R8) :: min(2), max(2), dx, dy
        integer :: npets, de_id

        ! Query component for VM and create a layout with the right breakdown
        call ESMF_GridCompGet(comp, vm=vm, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_VMGet(vm, localPet=de_id, petCount=npets, rc=rc)
        if(rc/=ESMF_SUCCESS) return

        rc = ESMF_SUCCESS
        print *, de_id, "User Comp 1 Init starting"

        ! Add a "humidity" field to the export state.
        counts(1) = 60
        counts(2) = 40
        min(1) = 0.0
        max(1) = 60.0
        min(2) = 0.0
        max(2) = 50.0

        dx = (max(1)-min(1))/(counts(1)-1)
        dy = (max(2)-min(2))/(counts(2)-1)

        grid1 = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=counts, &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            indexflag=ESMF_INDEX_GLOBAL, &
            regDecomp=(/1, npets/), name="source grid", rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
        call ESMF_GridAddCoord(grid1, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
        call ESMF_GridGetCoord(grid1, localDE=0, coordDim=1, &
                           farrayPtr=coordX, computationalLBound=tlb, computationalUBound=tub, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_GridGetCoord(grid1, localDE=0, coordDim=2, &
                           farrayPtr=coordY, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        do j   = tlb(2), tub(2)
          do i = tlb(1), tub(1)
            coordX(i,j) = (i-1)*dx
            coordY(i,j) = (j-1)*dy
          enddo
        enddo

        ! Set up a 2D real array
        call ESMF_ArraySpecSet(arrayspec, rank=2, &
                               typekind=ESMF_TYPEKIND_R8)
        if (rc .ne. ESMF_SUCCESS) return

        ! Create the field and have it create the array internally
        humidity = ESMF_FieldCreate(grid1, arrayspec, &
                                    totalLWidth=(/0,0/), totalUWidth=(/0,0/), &
                                    name="humidity", rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        ! Get the allocated array back and get an F90 array pointer
        call ESMF_FieldGet(humidity, farrayPtr=idata, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        ! Set initial data values over whole array to our de id
        idata = real(de_id,ESMF_KIND_R8)

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
        type(ESMF_Field) :: humidity
        type(ESMF_grid) :: grid
        real(ESMF_KIND_R8) :: pi
        real(ESMF_KIND_R8), dimension(:,:), pointer :: idata, coordX, coordY
        integer :: i, j, i1, j1, haloWidth, haloUWidth(2,1), counts(2), tlb(2), tub(2)

        rc = ESMF_SUCCESS
        print *, "User Comp Run starting"

        !!if (present(importState)) print *, "importState present"
        !!if (.not.present(importState)) print *, "importState *not* present"
        !!if (present(exportState)) print *, "exportState present"
        !!if (.not.present(exportState)) print *, "exportState *not* present"
        !!if (present(clock)) print *, "clock present"
        !!if (.not.present(clock)) print *, "clock *not* present"
        !!if (present(rc)) print *, "rc present"
        !!if (.not.present(rc)) print *, "rc *not* present"

        pi = 3.14159

        ! Get the Field and FieldBundle data from the State
        call ESMF_StateGet(exportState, "humidity", humidity, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
      
        ! get the grid and coordinates
        call ESMF_FieldGet(humidity, grid=grid, &
                           totalUWidth=haloUWidth, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
        haloWidth = haloUWidth(1,1)
        call ESMF_GridGetCoord(grid, localDE=0, coordDim=1, &
                           farrayPtr=coordX, computationalLBound=tlb, computationalUBound=tub, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_GridGetCoord(grid, localDE=0, coordDim=2, &
                           farrayPtr=coordY, rc=rc)
        if(rc/=ESMF_SUCCESS) return

        ! update field values here
        ! call ESMF_StateGetDataPointer(exportState, "humidity", idata, rc=rc)
        ! Get a pointer to the start of the data
        call ESMF_FieldGet(humidity, farrayPtr=idata, rc=rc)
        if(rc/=ESMF_SUCCESS) return

        ! increment data values in place
        do j   = tlb(2), tub(2)
          j1   = j + haloWidth
          do i = tlb(1), tub(1)
            i1 = i + haloWidth
            idata(i1,j1) = 10.0 + 5.0*sin(coordX(i,j)/60.0*pi) &
                                + 2.0*sin(coordY(i,j)/50.0*pi)
          enddo
        enddo

     !   call ESMF_StatePrint(exportState, rc=rc)
     !   call ESMF_FieldPrint(humidity, rc=rc)
     !   call ESMF_ArrayPrint(array1, "", rc=rc)
 
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
        type(ESMF_grid) :: grid

        rc = ESMF_SUCCESS
        print *, "User Comp Final starting"

        ! garbage collection   
        call ESMF_StateGet(exportState, "humidity", humidity, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
        call ESMF_FieldGet(humidity, grid=grid, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
        call ESMF_FieldDestroy(humidity, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
        call ESMF_GridDestroy(grid, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        print *, "User Comp Final returning"

    end subroutine user_final


    end module user_model1
    
!\end{verbatim}
    
