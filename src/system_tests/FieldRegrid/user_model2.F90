! $Id: user_model2.F90,v 1.3 2003/09/23 16:31:23 jwolfe Exp $
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
    use ESMF_Mod

    implicit none
    
    public userm2_register
        
    type mylocaldata
      integer :: dataoffset
    end type

    type wrapper
      type(mylocaldata), pointer :: ptr
    end type

    contains

!--------------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine userm2_register(comp, rc)
        type(ESMF_GridComp) :: comp
        integer :: rc

        ! local variables
        type(mylocaldata), pointer :: mydatablock
        type(wrapper) :: wrap

        print *, "In user register routine"

        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, &
                                        user_init, ESMF_SINGLEPHASE, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, &
                                        user_run, ESMF_SINGLEPHASE, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, &
                                        user_final, ESMF_SINGLEPHASE, rc)

        print *, "Registered Initialize, Run, and Finalize routines"

        allocate(mydatablock)

        mydatablock%dataoffset = 52

        wrap%ptr => mydatablock
        call ESMF_GridCompSetInternalState(comp, wrap, rc)

        print *, "Registered Private Data block for Internal State"

        rc = ESMF_SUCCESS

    end subroutine

!--------------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    subroutine user_init(comp, importstate, exportstate, clock, rc)
      type(ESMF_GridComp), intent(inout) :: comp
      type(ESMF_State), intent(inout) :: importstate, exportstate
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out) :: rc

!   ! Local variables
      type(ESMF_Field) :: humidity
      type(ESMF_DELayout) :: layout
      integer :: i, x, y
      type(ESMF_Grid) :: grid1
      type(ESMF_Array) :: array1
      type(ESMF_ArraySpec) :: arrayspec
      real(ESMF_KIND_R8), dimension(:,:), pointer :: idata
      real(ESMF_KIND_R8) :: min(2)
      real(ESMF_KIND_R8) :: delta1(40), delta2(50)
      integer :: countsPerDE1(3), countsPerDE2(2)
      integer :: de_id
      integer :: horz_gridtype, horz_stagger, horz_coord_system
      integer :: status, myde

      print *, "User Comp Init starting"

      ! Initially import state contains a field with a grid but no data.
      call ESMF_GridCompGet(comp, layout=layout, rc=status)

      ! Add a "humidity" field to the import state.
      countsPerDE1 = (/ 10, 18, 12 /)
      countsPerDE2 = (/ 22, 28 /)
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
      horz_gridtype = ESMF_GridType_XY
      horz_stagger = ESMF_GridStagger_D
      horz_coord_system = ESMF_CoordSystem_Cartesian

      grid1 = ESMF_GridCreate(countsPerDE1=countsPerDE1, &
                              countsPerDE2=countsPerDE2, &
                              min=min, delta1=delta1, delta2=delta2, &
                              layout=layout, &
                              horz_gridtype=horz_gridtype, &
                              horz_stagger=horz_stagger, &
                              horz_coord_system=horz_coord_system, &
                              name="source grid", rc=status)

      ! Figure out our local processor id
      call ESMF_DELayoutGetDEID(layout, de_id, rc)

      ! Set up a 2D integer array
      call ESMF_ArraySpecInit(arrayspec, rank=2, type=ESMF_DATA_INTEGER, &
                              kind=ESMF_I4)

      ! Create the field and have it create the array internally
      humidity = ESMF_FieldCreate(grid1, arrayspec, relloc=ESMF_CELL_CENTER, &
                                  name="humidity", rc=rc)

      ! Get the allocated array back and get an F90 array pointer
      call ESMF_FieldGetData(humidity, array1, rc)
      call ESMF_ArrayGetData(array1, idata, rc=rc)

      ! Set initial data values over exclusive domain to the de identifier
      idata = real(de_id)

      call ESMF_StateAddData(importstate, humidity, rc)
      call ESMF_StatePrint(importstate, rc=rc)

      print *, "User Comp Init returning"
   
      rc = ESMF_SUCCESS

    end subroutine user_init


!--------------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
    subroutine user_run(comp, importstate, exportstate, clock, rc)
      type(ESMF_GridComp), intent(inout) :: comp
      type(ESMF_State), intent(inout) :: importstate, exportstate
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out) :: rc

!   ! Local variables
      type(ESMF_Field) :: humidity
      type(ESMF_Array) :: array1
      integer :: status

      print *, "User Comp Run starting"

      ! Get information from the component.
      call ESMF_StatePrint(importstate, rc=status)
      call ESMF_StateGetData(importstate, "humidity", humidity, rc=status)
      call ESMF_FieldPrint(humidity, "", rc=status)
    
      ! This is where the model specific computation goes.
      call ESMF_FieldGetData(humidity, array1, rc=status)
      print *, "Imported Array in user model 2:"
      call ESMF_ArrayPrint(array1, "", rc)

      print *, "User Comp Run returning"

      rc = status

    end subroutine user_run


!--------------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine user_final(comp, importstate, exportstate, clock, rc)
      type(ESMF_GridComp), intent(inout) :: comp
      type(ESMF_State), intent(inout) :: importstate, exportstate
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out) :: rc

      ! Local variables
      integer :: status
      type(mylocaldata), pointer :: mydatablock
      type(wrapper) :: wrap

      print *, "User Comp Final starting"  

      ! Get our local info
      nullify(wrap%ptr)
      mydatablock => wrap%ptr
        
      call ESMF_GridCompGetInternalState(comp, wrap, status)

      mydatablock => wrap%ptr
      print *, "before deallocate, dataoffset = ", mydatablock%dataoffset
      deallocate(mydatablock, stat=status)
      print *, "deallocate returned ", status
      nullify(wrap%ptr)

      print *, "User Comp Final returning"
   
      rc = ESMF_SUCCESS

    end subroutine user_final


    end module user_model2
    
!\end{verbatim}
    
