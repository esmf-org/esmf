! $Id: ESMF_RowReduceSTest.F90,v 1.19 2004/04/14 21:42:09 jwolfe Exp $
!
! System test DELayoutRowReduce
!  Description on Sourceforge under System Test #69725

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test DELayoutRowReduce.
!
!
!\begin{verbatim}

    program DELayoutRowReduce

#include <ESMF_Macros.inc>

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod
    
    implicit none
    
    ! Local variables
    integer :: nx, ny, i, j, ni, nj, rc
    integer, dimension(2) :: delist
    integer :: row_to_reduce
    integer :: timestep, rowlen, rowi, rstart, rend
    integer :: result, len, de_id, ndes, rightvalue 
    integer :: counts(2)
    type(ESMF_GridType) :: horz_gridtype, vert_gridtype
    type(ESMF_GridStagger) :: horz_stagger, vert_stagger
    type(ESMF_CoordSystem) :: horz_coord_system, vert_coord_system
    integer :: status
    real(ESMF_KIND_R8) :: min(2), max(2)
    integer(ESMF_KIND_I4), dimension(:), pointer :: idata, ldata, rowdata
    type(ESMF_AxisIndex), dimension(2) :: index
    character(len=ESMF_MAXSTR) :: cname, gname, fname
    type(ESMF_newDELayout) :: delayout1 
    type(ESMF_Grid) :: grid1
    type(ESMF_Array) :: array1, array2
    type(ESMF_Field) :: field1
    type(ESMF_VM) :: vm
        
    ! cumulative result: count failures; no failures equals "all pass"
    integer :: testresult = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message and final status msg
    character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "System Test DELayoutRowReduce:"


!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
    call ESMF_Initialize(rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! get the global VM
    call ESMF_VMGetGlobal(vm, rc)

    ! Create a default 1 x N DELayout 
    delayout1 = ESMF_newDELayoutCreate(vm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_newDELayoutGet(delayout1, deCount=ndes, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    cname = "System Test DELayoutRowReduce"

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

      counts(1) = 41
      counts(2) = 20
      min(1) = 0.0
      max(1) = 20.5
      min(2) = 0.0
      max(2) = 5.0
      horz_gridtype = ESMF_GridType_XY
      horz_stagger = ESMF_GridStagger_A
      horz_coord_system = ESMF_CoordSystem_Cartesian
      gname = "test grid 1"

      grid1 = ESMF_GridCreateLogRectUniform(2, counts=counts, &
                              minGlobalCoordPerDim=min, &
                              maxGlobalCoordPerDim=max, &
                              delayout=delayout1, &
                              horzGridType=horz_gridtype, &
                              horzStagger=horz_stagger, &
                              horzCoordSystem=horz_coord_system, &
                              name=gname, rc=status)

      print *, "Grid Create returned ", status,  "(0=SUCCESS, -1=FAILURE)"


    ! figure out our local processor id
    call ESMF_newDELayoutGet(delayout1, localDE=de_id, rc=rc)

    ! Allocate and set initial data values.  These are different on each DE.
    call ESMF_GridGetDE(grid1, localCellCount=ni, &
                        horzRelloc=ESMF_CELL_CENTER, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "allocating", ni, " cells on DE", de_id
    allocate(idata(ni))
    allocate(ldata(ni))

    ! set original values to 0; local to global call below will overwrite
    ! this with global index numbers
    idata(:) = 0
    
    ! Generate global cell numbers.  First set to local number and
    ! then translate to global index.
    do i=1,ni
       ldata(i) = i
    enddo
    print *, "size local1D", size(ldata)
    print *, "size global1D", size(idata)
    call ESMF_GridLocalToGlobalIndex(grid1, local1D=ldata, global1D=idata, &
                                     horzRelloc=ESMF_CELL_CENTER, rc=rc) 
    if (rc .ne. ESMF_SUCCESS) goto 10

    print *, "after local to global"
    print *, "ldata was = ", ldata
    print *, "idata now = ", idata
    ! Delete local cell number array, not needed anymore.
    deallocate(ldata, stat=rc)

    !  Create Array based on an existing, allocated F90 pointer.
    !  Data is type Integer, 1D.
    array1 = ESMF_ArrayCreate(idata, ESMF_DATA_REF, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! We did the allocate outside the framework, so at the end after
    ! the Array is destroyed we will need an explicit deallocate().


    ! Create a Field using the Grid and Arrays created above
    fname = "relative humidity"
    field1 = ESMF_FieldCreate(grid1, array1, horzRelloc=ESMF_CELL_CENTER, &
                                    name=fname, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_FieldPrint(field1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

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
    call ESMF_FieldGetArray(field1, array2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_ArrayValidate(array2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_ArrayPrint(array2, "foo", rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Get a pointer to the start of the data
    call ESMF_ArrayGetData(array2, ldata, ESMF_DATA_REF, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Get the mapping between local and global indices for this DE
    !   and count of row size
    call ESMF_GridGetDE(grid1, globalAIPerDim=index, &
                        horzRelloc=ESMF_CELL_CENTER, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
 
    ! Create a new Fortran array for just the part of this row on this DE
    rowlen = index(1)%max - index(1)%min + 1
    rstart = ((row_to_reduce-1) * rowlen) + 1
    rend = (row_to_reduce) * rowlen
    
    ! make space for row
    allocate(rowdata(rowlen))

    ! and copy over the data row
    rowdata = ldata(rstart:rend)
    print *, "rowlen = ", rowlen
    print *, "rstart, rend = ", rstart, rend
    print *, "row data = ", rowdata

    ! Call the Reduce code
    call ESMF_newDELayoutAllGlobalReduce(delayout1, rowdata, result, rowlen, &
                                         ESMF_newSUM, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
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
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_GridDestroy(grid1, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_ArrayDestroy(array1, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_newDELayoutDestroy(delayout1, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "All Destroy routines done"

    deallocate(idata, stat=rc)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
10     print *, "System Test DELayoutRowReduce complete!"


    ! Only print on DE 0 for success, or any DE with an error
    if ((de_id .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then
      write(failMsg, *)  "Row Reduction value incorrect"
      write(testname, *) "System Test DELayoutRowReduce: Row Reduction"

      rightvalue = 0
      if (ndes .eq. 1) rightvalue = 7585
      if (ndes .eq. 2) rightvalue = 12205
      
      call ESMF_Test((result .eq. rightvalue) .and. (rc.eq.ESMF_SUCCESS), &
                        testname, failMsg, testresult, ESMF_SRCLINE)

      ! Separate message to console for quick confirmation of success/failure
      if ((result .eq. rightvalue) .and. (rc .eq. ESMF_SUCCESS)) then
        write(finalMsg, *) "SUCCESS!! Row reduction value (", rightvalue, &
                           ") is correct"
      else
        write(finalMsg, *) "System Test did not succeed. ", &
                               "Row reduction result", result, "not equal ", &
                               rightvalue, " or error code set ", rc
      endif
      write(0, *) ""
      write(0, *) trim(testname)
      write(0, *) trim(finalMsg)
      write(0, *) ""

    endif
    
    call ESMF_Finalize(rc)

    end program DELayoutRowReduce
    
!\end{verbatim}
    
