! $Id: ESMF_SysTest62501.F90,v 1.6 2003/04/24 16:43:13 nscollins Exp $
!
! System test code #62501

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test number 62501.
!
!
!\begin{verbatim}

    program ESMF_SysTest62501

    ! ESMF Framework module
    use ESMF_Mod
    
    implicit none
    
!   ! Local variables
    integer :: nx, ny, i, j, ni, nj, rc
    integer, dimension(2) :: delist
    integer :: row_to_reduce
    integer :: timestep, rowlen, rowi, rstart, rend
    integer :: result, len, base, de_id
    integer :: i_max, j_max
    integer :: horz_gridtype, vert_gridtype
    integer :: horz_stagger, vert_stagger
    integer :: horz_coord_system, vert_coord_system
    integer :: status
    integer :: nDE_i, nDE_j
    real :: x_min, x_max, y_min, y_max
    integer(ESMF_IKIND_I4), dimension(:), pointer :: idata, idata2, rowdata
    character(len=ESMF_MAXSTR) :: cname, gname, fname
    type(ESMF_DELayout) :: layout1 
    type(ESMF_Grid) :: grid1
    type(ESMF_Array) :: array1, array2
    type(ESMF_Field) :: field1
    type(ESMF_GridComp) :: comp1
        
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "System Test #62501:"


!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

!   ! Create a DELayout for the Component
    delist = (/ 0, 1 /)
    layout1 = ESMF_DELayoutCreate(2, 1, delist, ESMF_XFAST, rc)

    cname = "Atmosphere"
    comp1 = ESMF_GridCompCreate(cname, layout1, ESMF_GRIDCOMP, &
                                       ESMF_ATM, "/usr/local", rc=rc)

    print *, "Comp Create finished, name = ", trim(cname)

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
    call ESMF_GridCompInit(comp1, istate, estate, clock, rc)
       type(ESMF_GridComp) :: comp1
       type(ESMF_State) :: istate, estate
       type(ESMF_Clock) :: clock
       integer :: rc

!   !  The user creates a simple horizontal Grid internally by passing all
!   !  necessary information through the CreateInternal argument list.

      type(ESMF_DELayout) :: layout
 
     
      call ESMF_GridCompGet(comp1, layout=layout, rc=status)

      i_max = 40
      j_max = 20
      horz_gridtype = ESMF_GridType_XY
      vert_gridtype = ESMF_GridType_Unknown
      horz_stagger = ESMF_GridStagger_A
      vert_stagger = ESMF_GridStagger_Unknown
      horz_coord_system = ESMF_CoordSystem_Cartesian
      vert_coord_system = ESMF_CoordSystem_Unknown
      x_min = 0.0
      x_max = 20.0
      y_min = 0.0
      y_max = 5.0
      gname = "test grid 1"

      grid1 = ESMF_GridCreate(i_max=i_max, j_max=j_max, &
                             x_min=x_min, x_max=x_max, &
                             y_min=y_min, y_max=y_max, &
                             layout=layout, &
                             horz_gridtype=horz_gridtype, &
                             vert_gridtype=vert_gridtype, &
                             horz_stagger=horz_stagger, &
                             vert_stagger=vert_stagger, &
                             horz_coord_system=horz_coord_system, &
                             vert_coord_system=vert_coord_system, &
                             name=gname, rc=status)

      print *, "Grid Create returned"


!   !  Create Array based on an existing, allocated F90 pointer.
!   !  Data is type Integer, 1D.

!   ! Allocate and set initial data values.  These are different on each DE.
    ni = 400
    allocate(idata(ni))

!   ! Get base according to which DE we are
    call ESMF_GridGetDE(grid1, gstart=base, rc=rc)

!   ! Generate global cell numbers, each DE has a contiguous 
!   ! chunk of numbers.
    do i=1,ni
       idata(i) = i + base
    enddo

    array1 = ESMF_ArrayCreate(idata, ESMF_DATA_REF, rc)
    print *, "Array Create returned"

    call ESMF_ArrayPrint(array1, "foo", rc);

!   ! No deallocate() is needed for idata, it will be freed when the
!   !  Array is destroyed.  TODO:  does delete need a 'delete data' flag
!   !  so the user can continue to use the f90 array after the Array is gone?


!   ! Create a Field using the Grid and Arrays created above
    fname = "relative humidity"
    field1 = ESMF_FieldCreate(grid1, array1, relloc=ESMF_CELL_CENTER, &
                                    name=fname, rc=rc)

    print *, "Field Create returned"


    print *, "Comp Init finished"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    timestep = 1
    call ESMF_GridCompRun(comp1, timestep, rc)


!   Call Reduction operator here
    row_to_reduce = 5

    ! Get a pointer to the data Array in the Field
    call ESMF_FieldGetData(field1, array2, rc=rc)
    print *, "data back from field"
    call ESMF_ArrayPrint(array2, "foo", rc)

    ! Get a pointer to the start of the data
    call ESMF_ArrayGetData(array2, idata2, ESMF_DATA_REF, rc)

    ! Get the mapping between local and global indices for this DE
    !   and count of row size
    call ESMF_GridGetDE(grid1, n_dir1_size=rowlen, rc=rc)
 
    ! Create a new Fortran array for just the part of this row on this DE
    rstart = ((row_to_reduce-1) * rowlen) + 1
    rend = (row_to_reduce) * rowlen
    print *, "rowlen = ", rowlen
    print *, "rstart, rend = ", rstart, rend
    
    ! make space for row
    allocate(rowdata(rowlen))

    ! and copy over the data row
    rowdata = idata2(rstart:rend)
    print *, "row data = ", rowdata

    ! Call the Reduce code
    call ESMF_DELayoutAllReduce(layout1, rowdata, result, rowlen, ESMF_SUM, rc)
    print *, "Row Reduction operation called"

    ! Clean up local array
    deallocate(rowdata)

    print *, "Comp Run returned"


!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Print result

    call ESMF_GridCompFinalize(comp1, rc)

    call ESMF_DELayoutGetDEID(layout1, de_id, rc)

    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"
    print *, "Row Reduction operation returned ", result, " on DE ", de_id
    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"

    print *, "Comp Finalize returned"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Clean up

    call ESMF_GridCompDestroy(comp1, rc)
    call ESMF_FieldDestroy(field1, rc)
    call ESMF_GridDestroy(grid1, rc)
    call ESMF_ArrayDestroy(array1, rc)
    call ESMF_DELayoutDestroy(layout1, rc)
    print *, "All Destroy routines done"


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    print *, "System Test #62501 complete!"

    

    end program ESMF_SysTest62501
    
!\end{verbatim}
    
