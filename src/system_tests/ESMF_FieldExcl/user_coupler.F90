! $Id: user_coupler.F90,v 1.2 2004/10/07 16:31:22 nscollins Exp $
!
! System test of Exclusive components, user-written Coupler component.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Coupler
!
!
!\begin{verbatim}

    module user_coupler

    ! ESMF Framework module
    use ESMF_Mod
    
    implicit none
    
    public usercpl_register
        
    ! global data
    type(ESMF_RouteHandle), save :: routehandle

    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine usercpl_register(comp, rc)
      type(ESMF_CplComp) :: comp
      integer :: rc

      integer :: localrc

      print *, "in user setservices routine"

      ! Register the callback routines.
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, user_init, &
                                     ESMF_SINGLEPHASE, localrc)
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, user_run, &
                                     ESMF_SINGLEPHASE, localrc)
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, &
                                     ESMF_SINGLEPHASE, localrc)

      print *, "Registered Initialize, Run, and Finalize routines"

      ! set return code
      rc = localrc

    end subroutine

!-------------------------------------------------------------------------
!   !User Comp Component created by higher level calls, here is the
!   ! Initialization routine.
 
    
    subroutine user_init(comp, importState, exportState, clock, rc)
      type(ESMF_CplComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer :: rc

      ! Local variables
      integer :: itemcount, status
      type(ESMF_Field) :: humidity1, humidity2
      type(ESMF_Field) :: humidity1proxy, humidity2proxy
      type(ESMF_VM) :: vm
      type(ESMF_DELayout) :: cplDElayout, delayout
      type(ESMF_Array) :: array1, array2
      type(ESMF_ArraySpec) :: arrayspec
      real(ESMF_KIND_R8), dimension(:,:), pointer :: array1ptr, array1pptr
      real(ESMF_KIND_R8), dimension(:,:), pointer :: array2ptr, array2pptr
      type(ESMF_Grid) :: grid1, grid2
      real(ESMF_KIND_R8), dimension(:,:), pointer :: idata
      real(ESMF_KIND_R8) :: min(2), max(2)
      integer :: counts(ESMF_MAXGRIDDIM)
      integer :: npets, pet_id, countsPerDE1(4), countsPerDE2(2)
      logical :: i_have_comp1, i_have_comp2
      real(ESMF_KIND_R8) :: delta1(40), delta2(50)
      type(ESMF_GridHorzStagger) :: horz_stagger


      print *, "User Coupler Init starting"
      status = ESMF_FAILURE

      ! Get VM from coupler component
      call ESMF_CplCompGet(comp, vm=vm, rc=status)
      call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=status)

      ! figure out which comps exist on our PET
      if (pet_id .lt. npets/2) then
          i_have_comp1 = .TRUE.
          i_have_comp2 = .FALSE.
      else
          i_have_comp1 = .FALSE.
          i_have_comp2 = .TRUE.
      endif

      ! Since we are planning to use a communication call, we must
      ! reconcile the object lists in the State objects.
 
      ! New routine:
      ! must be called on each state which is going to be accessed from
      ! this coupler.  when the call returns, all objects which were not
      ! in existance on all PETs now have an object which represents them.
      call ESMF_StateReconcile(importState, vm, rc=status)
      call ESMF_StateReconcile(exportState, vm, rc=status)


      ! Query component for VM and create a layout with the right breakdown
      delayout = ESMF_DELayoutCreate(vm, (/ npets/2, 2 /), rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      print *, pet_id, "User Comp 1 Init proxy creation starting"

      ! Add a "humidity" field to the export state.
      countsPerDE1 = (/ 15, 15, 15, 15 /)
      countsPerDE2 = (/ 40, 0 /)

      counts(1) = 60
      counts(2) = 40
      min(1) = 0.0
      max(1) = 60.0
      min(2) = 0.0
      max(2) = 50.0
      horz_stagger = ESMF_GRID_HORZ_STAGGER_A

      grid1 = ESMF_GridCreateHorzXYUni(counts=counts, &
                                minGlobalCoordPerDim=min, &
                                maxGlobalCoordPerDim=max, &
                                horzStagger=horz_stagger, &
                                name="source grid", rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
      call ESMF_GridDistribute(grid1, delayout=delayout, &
                                 countsPerDEDim1=countsPerDE1, &
                                 countsPerDEDim2=countsPerDE2, &
                                 rc=status)

      if (status .ne. ESMF_SUCCESS) goto 10

      ! Set up a 2D real array
      call ESMF_ArraySpecSet(arrayspec, rank=2, type=ESMF_DATA_REAL, &
                              kind=ESMF_R8)
      if (status .ne. ESMF_SUCCESS) goto 10

      ! Create the field and have it create the array internally
      humidity1proxy = ESMF_FieldCreate(grid1, arrayspec, &
                                 horzRelloc=ESMF_CELL_CENTER, &
                                 haloWidth=0, name="humidity1proxy", rc=status)

      ! Get array from import state to add to this field
      if (i_have_comp1) then
        call ESMF_StateGetField(importstate, "humidity", humidity1, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_FieldGetDataPointer(humidity1, array1ptr, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_FieldGetDataPointer(humidity1proxy, array1pptr, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! point at real data where we have it
        array1pptr => array1ptr
      endif

      call ESMF_StateAddField(importState, humidity1proxy, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
  !   call ESMF_StatePrint(importState, rc=status)

      print *, pet_id, "end of proxy field 1 creation"
   

      ! Query component for VM and create a layout with the right breakdown
      print *, pet_id, "User Comp 2 Init starting"

      ! Add a "humidity" field to the import state.
      countsPerDE1 = (/ 10, 6, 12, 12 /)
      countsPerDE2 = (/ 0, 50 /)
      min(1) = 0.0
      delta1 = (/ 1.0, 1.0, 1.0, 1.1, 1.1, 1.1, 1.2, 1.2, 1.3, 1.4, &
                  1.4, 1.5, 1.6, 1.6, 1.6, 1.8, 1.8, 1.7, 1.7, 1.6, &
                  1.6, 1.6, 1.8, 1.8, 2.0, 2.0, 2.2, 2.2, 2.2, 2.2, &
                  2.0, 1.7, 1.5, 1.3, 1.2, 1.1, 1.0, 1.0, 1.0, 0.9 /)
      min(2) = 0.0
      delta2 = (/ 0.8, 0.8, 0.8, 0.8, 0.8, 0.7, 0.7, 0.6, 0.7, 0.8, &
                  0.9, 0.9, 0.9, 0.9, 1.0, 1.0, 1.0, 1.0, 0.9, 1.0, &
                  1.0, 1.0, 1.0, 1.1, 1.2, 1.3, 1.3, 1.3, 1.4, 1.4, &
                  1.4, 1.4, 1.4, 1.4, 1.4, 1.3, 1.3, 1.3, 1.2, 1.2, &
                  1.1, 1.0, 1.0, 0.9, 0.8, 0.7, 0.6, 0.6, 0.5, 0.5 /)
      min(1) = 0.0
      min(2) = 0.0
      horz_stagger = ESMF_GRID_HORZ_STAGGER_D_NE

      grid2 = ESMF_GridCreateHorzXY(minGlobalCoordPerDim=min, &
                                    delta1=delta1, delta2=delta2, &
                                    horzStagger=horz_stagger, &
                                    name="source grid", rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
      call ESMF_GridDistribute(grid2, delayout=delayout, &
                               countsPerDEDim1=countsPerDE1, &
                               countsPerDEDim2=countsPerDE2, &
                               rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10


      ! Create the field and have it create the array internally
      humidity2proxy = ESMF_FieldCreate(grid2, arrayspec, &
                                 horzRelloc=ESMF_CELL_NFACE, &
                                 haloWidth=0, name="humidity2proxy", rc=status)

      if (i_have_comp2) then
        ! Get array from import state to add to this field
        call ESMF_StateGetField(exportstate, "humidity",  humidity2, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_FieldGetDataPointer(humidity2, array2ptr, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_FieldGetDataPointer(humidity2proxy, array2pptr, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        array2pptr => array2ptr
      endif

      call ESMF_StateAddField(exportState, humidity2proxy, rc)
      if (status .ne. ESMF_SUCCESS) goto 10
      !   call ESMF_StatePrint(exportState, rc=status)
  
      print *, pet_id, "User Comp 2 Init returning"
  
      ! end of new code 

      call ESMF_StateGet(importState, itemcount=itemcount, rc=status)
      print *, "Import State contains ", itemcount, " items."
       

      ! Get input data
      call ESMF_StateGetField(importState, "humidity1proxy", humidity1, rc=status)
      ! call ESMF_FieldPrint(humidity1, rc=status)

      ! Get location of output data
      call ESMF_StateGetField(exportState, "humidity2proxy", humidity2, rc=status)
      ! call ESMF_FieldPrint(humidity2, rc=status)

      ! Create a layout with all the PETs
      cplDElayout = ESMF_DELayoutCreate(vm, rc=status)

      ! These are fields on different Grids - call RegridStore to set
      ! up the Regrid structure

      call ESMF_FieldRegridStore(humidity1, humidity2, cplDElayout, &
                                 routehandle, &
                                 regridmethod=ESMF_REGRID_METHOD_BILINEAR, &
                                 rc=status)


      print *, "User Coupler Init returning"
   
      rc = ESMF_SUCCESS
      return

      ! get here only on error exit
10  continue
      rc = status
  
    end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !
 
    subroutine user_run(comp, importState, exportState, clock, rc)
      type(ESMF_CplComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer :: rc

      ! Local variables
      type(ESMF_Field) :: humidity1, humidity2
      integer :: status

      print *, "User Coupler Run starting"

      ! Get input data
      call ESMF_StateGetField(importState, "humidity1proxy", humidity1, rc=status)
      ! call ESMF_FieldPrint(humidity1, rc=status)

      ! Get location of output data
      call ESMF_StateGetField(exportState, "humidity2proxy", humidity2, rc=status)
      ! call ESMF_FieldPrint(humidity2, rc=status)

      ! These are fields on different Grids - call Regrid to rearrange
      !  the data.   The communication pattern was computed at init,
      !  this simply has to execute the send and receive equivalents.

      call ESMF_FieldRegrid(humidity1, humidity2, routehandle, rc=status)

      ! Data is moved directly to the field in the output state, so no
      ! "put" is needed here.
 
      print *, "User Coupler Run returning"

      rc = status

    end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine user_final(comp, importState, exportState, clock, rc)
      type(ESMF_CplComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer :: rc

      ! Local variables
      integer :: status

      print *, "User Coupler Final starting"
      status = ESMF_FAILURE
   
      ! Release resources stored for the Regridding.
      call ESMF_FieldRegridRelease(routehandle, rc=status)

      print *, "User Coupler Final returning"
   
      rc = status

    end subroutine user_final


    end module user_coupler
    
!\end{verbatim}
    
