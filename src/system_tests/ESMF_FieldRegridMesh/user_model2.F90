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
      type(ESMF_Field) :: dstField
      type(ESMF_VM) :: vm
      type(ESMF_grid) :: dstGrid
      type(ESMF_ArraySpec) :: arrayspec
      integer :: localPET, petCount
      integer dst_nx, dst_ny, i1,i2
      real(ESMF_KIND_R8) :: dst_minx,dst_miny
      real(ESMF_KIND_R8) :: dst_maxx,dst_maxy
      integer :: lDE, localDECount, localrc
      real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:), farrayPtr1D(:)
      real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
      real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
      integer :: clbnd(2),cubnd(2)

      rc = ESMF_SUCCESS
      ! Initially import state contains a field with a grid but no data.

      ! Query component for VM and create a layout with the right breakdown
      call ESMF_GridCompGet(comp, vm=vm, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if(rc/=ESMF_SUCCESS) return


      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Create Destination grid
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! Establish the resolution of the grids
      dst_nx = 10
      dst_ny = 10

      ! Establish the coordinates of the grids
      dst_minx = 0.1
      dst_miny = 0.1

      dst_maxx = 1.9
      dst_maxy = 1.9

      ! Create Grid
      dstGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/), &
                coordSys=ESMF_COORDSYS_CART, &
                regDecomp=(/2,2/), indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
         rc=ESMF_FAILURE
         return
      endif

      ! Create source/destination fields
      call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

      dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dst", rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
         return
      endif

      call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
         rc=ESMF_FAILURE
         return
       endif

      ! Get number of local DEs
      call ESMF_GridGet(dstGrid, localDECount=localDECount, rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
         rc=ESMF_FAILURE
         return
      endif

      ! Get memory and set coords for dst
      do lDE=0,localDECount-1

         !! get coords
         call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                           computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
         if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
         endif

         call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
         if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
         endif


         call ESMF_FieldGet(dstField, lDE, farrayPtr, rc=localrc)
         if (localrc /=ESMF_SUCCESS) then
             rc=ESMF_FAILURE
             return
         endif

        !! set coords
        do i1=clbnd(1),cubnd(1)
        do i2=clbnd(2),cubnd(2)

           ! Set source coordinates
           farrayPtrXC(i1,i2) = ((dst_maxx-dst_minx)*REAL(i1-1)/REAL(dst_nx-1))+dst_minx
           farrayPtrYC(i1,i2) = ((dst_maxy-dst_miny)*REAL(i2-1)/REAL(dst_ny-1))+dst_miny

           ! initialize destination field
           farrayPtr(i1,i2)=0.0

        enddo
        enddo
      enddo    ! lDE

      ! Set Field Into State
      call ESMF_StateAdd(importState, (/dstField/), rc=rc)
      if(rc/=ESMF_SUCCESS) return

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
      type(ESMF_Field) :: dstField
      type(ESMF_Grid) :: dstGrid
      integer :: lDE, localDECount, localrc,i1,i2
      real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:), farrayPtr1D(:)
      real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
      real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
      integer :: clbnd(2),cubnd(2)

      rc = ESMF_SUCCESS

      ! Get information from the component.
      call ESMF_StateGet(importState, "dst", dstField, rc=rc)
      if(rc/=ESMF_SUCCESS) return


      ! Get Grid from field
      call ESMF_FieldGet(dstField, grid=dstGrid, rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
      endif

      ! Get number of local DEs
      call ESMF_GridGet(dstGrid, localDECount=localDECount, rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
      endif

      ! Check error
      do lDE=0,localDECount-1

         !! get coords
         call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                                farrayPtr=farrayPtrXC, rc=localrc)
         if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
         endif

         call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                               farrayPtr=farrayPtrYC, rc=localrc)
         if (localrc /=ESMF_SUCCESS) then
             rc=ESMF_FAILURE
             return
         endif

         call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                            computationalUBound=cubnd,  rc=localrc)
         if (localrc /=ESMF_SUCCESS) then
             rc=ESMF_FAILURE
            return
         endif

         !! check error
         do i1=clbnd(1),cubnd(1)
          do i2=clbnd(2),cubnd(2)

             !! if error is too big report an error
             if (abs(farrayPtr(i1,i2)-(20.0+farrayPtrXC(i1,i2)+farrayPtrYC(i1,i2))) > 0.0001) then
!                 write(*,*) farrayPtr(i1,i2),".ne.",(20.0+farrayPtrXC(i1,i2)+farrayPtrYC(i1,i2))
                 rc=ESMF_FAILURE
                 return
             endif      
         enddo
       enddo

       ! RESET DESTINATION BACK TO 0
       farrayPtr=0.0

      enddo    ! lDE


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
      type(ESMF_Field) :: dstField
      type(ESMF_Grid) :: dstGrid

      rc = ESMF_SUCCESS
      print *, "User Comp Final starting"

      ! check validity of results
      ! Get Fields from import state
      call ESMF_StateGet(importState, "dst", dstField, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      ! garbage collection
      call ESMF_FieldGet(dstField, grid=dstGrid, rc=rc)
      if (rc .ne. ESMF_SUCCESS) return

      call ESMF_FieldDestroy(dstField, rc=rc)
      if (rc .ne. ESMF_SUCCESS) return

      call ESMF_GridDestroy(dstGrid, rc=rc)
      if (rc .ne. ESMF_SUCCESS) return

      print *, "User Comp Final returning"

    end subroutine user_final


    end module user_model2

!\end{verbatim}

