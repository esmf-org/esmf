! $Id: ESMF_SysTest69527.F90,v 1.7 2003/04/04 21:10:29 nscollins Exp $
!
! System test code #69527

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test number 69527.
!
!
!\begin{verbatim}

    program ESMF_SysTest69527

    ! ESMF Framework module
    use ESMF_Mod
    
    implicit none
    
!   ! Local variables
    integer :: nx, ny, i, j, ni, nj, rc
    integer, dimension(2) :: delist
    integer :: row_to_reduce
    integer :: timestep, rowlen, rowi, rstart, rend
    integer :: result, len, de_id
    integer :: i_max, j_max
    integer :: horz_gridtype, vert_gridtype
    integer :: horz_stagger, vert_stagger
    integer :: horz_coord_system, vert_coord_system
    integer :: status
    real :: x_min, x_max, y_min, y_max
    integer(ESMF_IKIND_I4), dimension(:), pointer :: idata, idata2, &
                                                     rowdata, ldata
    type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: index
    character(len=ESMF_MAXSTR) :: cname, gname, fname
    type(ESMF_DELayout) :: layout1 
    type(ESMF_Grid) :: grid1
    type(ESMF_Array) :: array1, array2
    type(ESMF_Field) :: field1
        
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "System Test #69527:"



!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

!   ! Create a DELayout 
    delist = (/ 0, 1 /)
    layout1 = ESMF_DELayoutCreate(delist, 2, (/ 2, 1 /), (/ 0, 0 /), rc)

    cname = "System Test #69527"

    print *, "Create section finished"
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

!   !  The user creates a simple horizontal Grid internally by passing all
!   !  necessary information through the CreateInternal argument list.

      i_max = 41
      j_max = 20
      x_min = 0.0
      x_max = 20.5
      y_min = 0.0
      y_max = 5.0
      horz_gridtype = ESMF_GridType_XY
      horz_stagger = ESMF_GridStagger_A
      horz_coord_system = ESMF_CoordSystem_Cartesian
      gname = "test grid 1"

      grid1 = ESMF_GridCreate(i_max=i_max, j_max=j_max, &
                             x_min=x_min, x_max=x_max, &
                             y_min=y_min, y_max=y_max, &
                             layout=layout1, &
                             horz_gridtype=horz_gridtype, &
                             horz_stagger=horz_stagger, &
                             horz_coord_system=horz_coord_system, &
                             name=gname, rc=status)

      print *, "Grid Create returned"


!   ! figure out our local processor id
    call ESMF_DELayoutGetDEID(layout1, de_id, rc)


!   ! Allocate and set initial data values.  These are different on each DE.
    call ESMF_GridGetDE(grid1, lcelltot_count=ni, rc=rc)
    print *, "allocating", ni, " cells on DE", de_id
    allocate(idata(ni))
    allocate(ldata(ni))

!   ! Generate global cell numbers.  First set to local number and
!   ! then translate to global index.
    do i=1,ni
       ldata(i) = i
    enddo
    call ESMF_GridLocalToGlobalIndex(grid1, ldata, idata, rc) 

!   !  Create Array based on an existing, allocated F90 pointer.
!   !  Data is type Integer, 1D.
    array1 = ESMF_ArrayCreate(idata, ESMF_NO_COPY, rc)
    print *, "Array Create returned"

    call ESMF_ArrayPrint(array1, "foo", rc);

!   ! No deallocate() is needed for idata, it will be freed when the
!   !  Array is destroyed.  TODO:  it seems delete need a 'delete data' 
!   !  option so the user can choose whether to have esmf delete the space 
!   !  or continue to use the f90 array after the Array is gone?


!   ! Create a Field using the Grid and Arrays created above
    fname = "relative humidity"
    field1 = ESMF_FieldCreate(grid1, array1, relloc=ESMF_CELL_CENTER, &
                                    name=fname, rc=rc)

    print *, "Field Create returned"


    print *, "Init section finished"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!


!   Call Reduction operator here
    row_to_reduce = 5

    ! Get a pointer to the data Array in the Field
    call ESMF_FieldGetData(field1, array2, rc=rc)
    print *, "data back from field"
    call ESMF_ArrayPrint(array2, "foo", rc)

    ! Get a pointer to the start of the data
    call ESMF_ArrayGetData(array2, idata2, ESMF_NO_COPY, rc)

    ! Get the mapping between local and global indices for this DE
    !   and count of row size
    call ESMF_GridGetDE(grid1, lcelltot_index=index, rc=rc)
 
    ! Create a new Fortran array for just the part of this row on this DE
    rowlen = index(1)%r - index(1)%l + 1
    rstart = ((row_to_reduce-1) * rowlen) + 1
    rend = (row_to_reduce) * rowlen
    
    ! make space for row
    allocate(rowdata(rowlen))

    ! and copy over the data row
    rowdata = idata2(rstart:rend)
    print *, "rowlen = ", rowlen
    print *, "rstart, rend = ", rstart, rend
    print *, "row data = ", rowdata

    ! Call the Reduce code
    call ESMF_DELayoutAllReduce(layout1, rowdata, result, rowlen, ESMF_SUM, rc)
    print *, "Row Reduction operation called"

    ! Clean up local array
    deallocate(rowdata)


    print *, "Run section finished"
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Print result

    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"
    print *, "Row Reduction operation returned ", result, " on DE ", de_id
    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"

    print *, "Finalize section finished"
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Clean up

    call ESMF_FieldDestroy(field1, rc)
    call ESMF_GridDestroy(grid1, rc)
    !call ESMF_ArrayDestroy(array1, rc)
    !call ESMF_DELayoutDestroy(layout1, rc)
    print *, "All Destroy routines done"


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    print *, "System Test #69527 complete!"

    

    end program ESMF_SysTest69527
    
!\end{verbatim}
    
