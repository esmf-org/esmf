! $Id: ESMF_SysTest70385.F90,v 1.15 2003/04/29 14:14:35 nscollins Exp $
!
! System test code #70385

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test number 70385.
!
!
!\begin{verbatim}

    program ESMF_SysTest70385

    ! ESMF Framework module
    use ESMF_Mod
    
    implicit none
    
    ! Subroutine to set entry points.
    external setserv

    ! Local variables
    type(ESMF_AppComp) :: app
    type(ESMF_GridComp) :: comp1
    type(ESMF_DELayout) :: layout1, deflayout
    integer, dimension(12) :: delist
    character(len=ESMF_MAXSTR) :: cname
    type(ESMF_State) :: import
    integer :: ndes, rc
        
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "------------------"
    print *, "System Test #70385:"
    print *, "------------------"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
    app = ESMF_AppCompCreate(name="Application #70385", rc=rc)
    call ESMF_AppCompGet(app, layout=deflayout, rc=rc)
    call ESMF_DELayoutGetNumDEs(deflayout, ndes, rc)
    if (ndes .ne. 12) then
        print *, "This system test needs to run 12-way, current np = ", ndes
        goto 10
    endif

    
    
!   Create a DELayout for the Component
    delist = (/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 /)
    layout1 = ESMF_DELayoutCreate(delist, 2, (/ 3, 4 /), (/ 0, 0 /), rc)

    cname = "System Test #70385"
    comp1 = ESMF_GridCompCreate(name=cname, layout=layout1, rc=rc)

    print *, "Comp Create finished, name = ", trim(cname)

    call ESMF_GridCompSetServices(comp1, setserv, rc)

!
!-------------------------------------------------------------------------
!  Init section
!
    import = ESMF_StateCreate("gridded comp import", ESMF_STATEIMPORT, rc=rc)
    call ESMF_GridCompInitialize(comp1, importstate=import, rc=rc)

    print *, "Comp Init returned"

!
!-------------------------------------------------------------------------
!     Run section
!

    call ESMF_GridCompRun(comp1, importstate=import, rc=rc)

    print *, "Comp Run returned"

!
!-------------------------------------------------------------------------
!     Finalize section

    call ESMF_GridCompFinalize(comp1, importstate=import, rc=rc)

    print *, "Comp Finalize returned"

!
!-------------------------------------------------------------------------
!     Destroy section
! 

    call ESMF_DELayoutDestroy(layout1, rc)
    call ESMF_GridCompDestroy(comp1, rc)
    call ESMF_StateDestroy(import, rc)
    call ESMF_AppCompDestroy(app, rc)
    print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
10    print *, "System Test #70385 complete!"

    end program ESMF_SysTest70385
    

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Set services
!-------------------------------------------------------------------------
!
    subroutine setserv(comp, rc)
      use ESMF_Mod

      type(ESMF_GridComp) :: comp
      integer :: rc

      external myinit, myrun, myfinal
       
      call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, myinit, &
                                                          ESMF_SINGLEPHASE, rc)
      call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, myrun, &
                                                          ESMF_SINGLEPHASE, rc)
      call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, myfinal, &
                                                          ESMF_SINGLEPHASE, rc)
  
    end subroutine setserv

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!
    subroutine myinit(comp, importstate, exportstate, clock, rc)
      use ESMF_Mod

      type(ESMF_GridComp) :: comp
      type(ESMF_State) :: importstate, exportstate
      type(ESMF_Clock) :: clock
      integer :: rc

      ! Local variables
      integer :: i, j, ni, nj
      type(ESMF_DELayout) :: layout1 
      type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: index
      type(ESMF_Grid) :: grid1
      type(ESMF_Field) :: field1
      integer(ESMF_IKIND_I4), dimension(:,:), pointer :: ldata
      integer :: de_id
      integer :: i_max, j_max
      integer :: horz_gridtype, vert_gridtype, halo_width
      integer :: horz_stagger, vert_stagger
      integer :: horz_coord_system, vert_coord_system
      real :: x_min, x_max, y_min, y_max
      type(ESMF_Array) :: array1
      character(len=ESMF_MAXSTR) :: gname, fname
      integer :: status

      print *, "Entering Initialization routine"

      ! Query component for layout
      call ESMF_GridCompGet(comp, layout=layout1, rc=status)
      print *, "myinit: getting layout, rc = ", status

      ! The user creates a simple horizontal Grid internally by passing all
      ! necessary information through the CreateInternal argument list.

      i_max = 30
      j_max = 36
      x_min = 0.0
      x_max = 15.0
      y_min = 0.0
      y_max = 12.0
      horz_gridtype = ESMF_GridType_XY
      horz_stagger = ESMF_GridStagger_A
      horz_coord_system = ESMF_CoordSystem_Cartesian
      halo_width = 2
      gname = "test grid 1"

      grid1 = ESMF_GridCreate(i_max=i_max, j_max=j_max, &
                             x_min=x_min, x_max=x_max, &
                             y_min=y_min, y_max=y_max, &
                             layout=layout1, &
                             horz_gridtype=horz_gridtype, &
                             horz_stagger=horz_stagger, &
                             horz_coord_system=horz_coord_system, &
                             halo_width=halo_width, &
                             name=gname, rc=status)

      print *, "Grid Create returned"

      ! Figure out our local processor id to use as data in the Field.
      call ESMF_DELayoutGetDEID(layout1, de_id, rc)

      ! Allocate arrays.
      call ESMF_GridGetDE(grid1, lcelltot_index=index, rc=rc)
      ni = index(1)%r - index(1)%l + 1
      nj = index(2)%r - index(2)%l + 1
      print *, "allocating", ni, " by ",nj," cells on DE", de_id
      allocate(ldata(ni,nj))

      ! Set initial data values over whole array to -1
      do j=1,nj
        do i=1,ni
          ldata(i,j) = -1
        enddo
      enddo

      ! Set initial data values over exclusive domain to the de identifier
      call ESMF_GridGetDE(grid1, lcellexc_index=index, rc=rc)
      do j=index(2)%l,index(2)%r
        do i=index(1)%l,index(1)%r
          ldata(i,j) =de_id
        enddo
      enddo

      ! Create Array based on an existing, allocated F90 pointer.
      ! Data is type Integer, 2D.
      array1 = ESMF_ArrayCreate(ldata, ESMF_DATA_REF, rc)
      print *, "Array Create returned"

      ! No deallocate() is needed for idata, it will be freed when the
      ! Array is destroyed. 

      ! Create a Field using the Grid and Arrays created above
      fname = "DE id"
      field1 = ESMF_FieldCreate(grid1, array1, relloc=ESMF_CELL_CENTER, &
                                                           name=fname, rc=rc)

      call ESMF_FieldPrint(field1, "", rc)
      call ESMF_FieldValidate(field1, "", rc)
      print *, "Field Create returned"

      ! Add the field to the import state.
      call ESMF_StateAddData(importstate, field1, rc)
      call ESMF_StatePrint(importstate, "", rc)

      print *, "Exiting Initialization routine"

    end subroutine myinit

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!

    subroutine myrun(comp, importstate, exportstate, clock, rc)
      use ESMF_Mod

      type(ESMF_GridComp) :: comp
      type(ESMF_State) :: importstate, exportstate
      type(ESMF_Clock) :: clock
      integer :: rc

      ! Local variables
      type(ESMF_Field) :: field1

      print *, "Entering Run routine"

      call ESMF_StatePrint(importstate, "", rc)

      ! Get the field from the import state
      print *, "About to get field from import state"
      call ESMF_StateGetData(importstate, "DE id", field1, rc=rc);
      print *, "Returned from getting field from import state"
      call ESMF_FieldPrint(field1, "", rc)


      ! Call Field method to halo data.  This updates the data in place.
      print *, "about to call Field Halo"
      call ESMF_FieldHalo(field1, rc)
      print *, "returned from Field Halo call"

      print *, "Exiting Run routine"

      end subroutine myrun

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------


    subroutine myfinal(comp, importstate, exportstate, clock, rc)
      use ESMF_Mod

      type(ESMF_GridComp) :: comp
      type(ESMF_State) :: importstate, exportstate
      type(ESMF_Clock) :: clock
      integer :: rc

      ! Local variables
      integer :: i, j, ni, nj
      integer :: de_id
      integer :: status
      integer(ESMF_IKIND_I4), dimension(:,:), pointer :: ldata
      type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: index
      type(ESMF_DELayout) :: layout
      type(ESMF_Field) :: field1
      type(ESMF_Grid) :: grid1
      type(ESMF_Array) :: array1

      print *, "Entering Finalize routine"

      ! Get layout from component
      call ESMF_GridCompGet(comp, layout=layout, rc=rc)
      call ESMF_DELayoutGetDEID(layout, de_id, rc=rc)

      ! Get Field from import state
      call ESMF_StateGetData(importstate, "DE id", field1, rc=rc);
      call ESMF_FieldGetGrid(field1, grid=grid1, rc=rc)

      ! Get a pointer to the data Array in the Field
      call ESMF_FieldGetData(field1, array1, rc=rc)
      print *, "data back from field"

      ! Get a pointer to the start of the data
      call ESMF_ArrayGetData(array1, ldata, ESMF_DATA_REF, rc)

      ! Get size of local array
      call ESMF_GridGetDE(grid1, lcelltot_index=index, rc=rc)
      ni = index(1)%r - index(1)%l + 1
      nj = index(2)%r - index(2)%l + 1

      ! Print results
      print *, "------------------------------------------------------"
      write(*,*) 'de_id = ',de_id
      do j = nj,1,-1
        write(*,10) (ldata(i,j), i=1,ni)
 10     format(20(1x,i2))
      enddo
      print *, "------------------------------------------------------"
    
      print *, "Exiting Finalize routine"

    end subroutine myfinal

!\end{verbatim}
    
