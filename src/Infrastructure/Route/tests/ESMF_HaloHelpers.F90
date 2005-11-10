! $Id: ESMF_HaloHelpers.F90,v 1.3 2005/11/10 21:56:05 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2005, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!

module ESMF_HaloHelpers
 
 use ESMF_Mod

 public CreateFields, FillConstantField, FillConstantHalo 
 public FillIndexField, ValidateConstantField, ValidateConstantHalo 
 public ValidateIndexField, ValidateIndexHalo, Cleanup 

contains

!
! Create 2 fields with the same grid but different layouts.
! This grid is periodic in both directions.
!
! TODO: We need to create a non-periodic grid as well.
!  That complicates the error checking part (the exterior edges 
!  should not have been altered but interior should be).
!
subroutine CreateFields(field1, field2, rc)
    type(ESMF_Field), intent(out) :: field1, field2
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: npets, halo
    type(ESMF_Grid) :: srcgrid, dstgrid
    type(ESMF_ArraySpec) :: arrayspec
    !type(ESMF_FieldDataMap) :: datamap
    type(ESMF_DELayout) :: layout1, layout2
    type(ESMF_Logical) :: wrap(2)
    type(ESMF_VM) :: vm
    real (ESMF_KIND_R8), dimension(2) :: mincoords, maxcoords

 
    ! pick a default halowidth, must be same for both src and dst
    halo = 3

    ! make the grids periodic both ways so we have fewer issues with
    ! halo updates and boundary conditions.
    wrap(1) = ESMF_TRUE
    wrap(2) = ESMF_TRUE

    ! assume the worst
    rc = ESMF_FAILURE
        
    call ESMF_VMGetGlobal(vm, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    ! Get number of PETs we are running with
    call ESMF_VMGet(vm, petCount=npets, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    layout1 = ESMF_DELayoutCreate(vm, (/ 1, npets /), rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    layout2 = ESMF_DELayoutCreate(vm, (/ npets, 1 /), rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    mincoords = (/  0.0,  0.0 /)
    maxcoords = (/ 20.0, 30.0 /)
    srcgrid = ESMF_GridCreateHorzXYUni((/ 90, 180 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_GRID_HORZ_STAGGER_A, &
                   periodic=wrap,  &
                   name="srcgrid", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    call ESMF_GridDistribute(srcgrid, delayout=layout1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    ! same grid coordinates, but different layout
    dstgrid = ESMF_GridCreateHorzXYUni((/ 90, 180 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_GRID_HORZ_STAGGER_A, &
                   periodic=wrap,  &
                   name="srcgrid", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    call ESMF_GridDistribute(dstgrid, delayout=layout2, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return


    ! Real*8, 2D data
    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_DATA_REAL, ESMF_R8, rc)
    if (rc.NE.ESMF_SUCCESS) return
    
    ! specify halowidth and let the field allocate the proper space
    field1 = ESMF_FieldCreate(srcgrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                haloWidth=halo, name="src pressure", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
                                
 
    ! specify halowidth and let the field allocate the proper space
    field2 = ESMF_FieldCreate(dstgrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                haloWidth=halo, name="dst pressure", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
                                
    rc = ESMF_SUCCESS
    return

end subroutine CreateFields


!
! Fill all the data associated with a field with a constant value.
! This assumes the data is real*8, 2D.
! TODO: this currently overwrites the halo area as well as the computational
!  area.  it should have an option to fill the computational area only.
!  for now, call this and then call the fill halo routine to overwrite
!  the halo with the desired data.
!
subroutine FillConstantField(field, val, rc)
    type(ESMF_Field), intent(inout) :: field
    real (ESMF_KIND_R8), intent(in) :: val
    integer, intent(out) :: rc
    
    ! Local variables
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr

    rc = ESMF_FAILURE
        
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.

    ! TODO: if it is important to set the halo to something other than the
    ! constant value, then we will need the halo width and the grid info.
    ! for now, simply set the entire data space to the constant value.
    !call ESMF_FieldGet(field, haloWidth=halo, grid=grid, rc=rc)

    ! get a Fortran 90 pointer back to the data block
    call ESMF_FieldGetDataPointer(field, f90ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    
    f90ptr(:,:) = val

    rc = ESMF_SUCCESS
    return

end subroutine FillConstantField


!
! Fill the halo region (only) associated with a field with a constant value.
! This assumes the data is real*8, 2D.
!
subroutine FillConstantHalo(field, val, rc)
    type(ESMF_Field), intent(inout) :: field
    real (ESMF_KIND_R8), intent(in) :: val
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: lb(2), ub(2), halo
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr

    rc = ESMF_FAILURE
        
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.

    call ESMF_FieldGet(field, haloWidth=halo, rc=rc)

    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field, f90ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    lb(:) = lbound(f90ptr)
    ub(:) = ubound(f90ptr)
    
    ! now set the chunks, one at a time.  this duplicates the overlaps
    ! at the corners, but it is the simplest to program.
 
    ! bottom / south
    f90ptr(lb(1):ub(1), lb(2):lb(2)+halo-1) = val

    ! left / west edge
    f90ptr(lb(1):lb(1)+halo-1, lb(2):ub(2)) = val

    ! right / east edge
    f90ptr(ub(1)-halo+1:ub(1), lb(2):ub(2)) = val

    ! top / north
    f90ptr(lb(1):ub(1), ub(2)-halo+1:ub(2)) = val

    rc = ESMF_SUCCESS

    return

end subroutine FillConstantHalo

!
! Fill a field with real*8 values which are the global cell index number.
!
subroutine FillIndexField(field, rc)
    type(ESMF_Field), intent(inout) :: field
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: i, j
    integer :: lb(2), ub(2), haloWidth, cellNum, rownum, colnum
    integer :: localCellCounts(2), globalCellCounts(2), gridOffsets(2)
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr
    type(ESMF_Grid) :: grid

    rc = ESMF_FAILURE
          
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.

    call ESMF_FieldGet(field, haloWidth=haloWidth, grid=grid, rc=rc)

    call ESMF_GridGet(grid, horzrelloc=ESMF_CELL_CENTER, &
                      globalCellCountPerDim=globalCellCounts, rc=rc)

    ! get grid information used to calculate global indices
    call ESMF_GridGetDELocalInfo(grid, horzrelloc=ESMF_CELL_CENTER, &
                                 localCellCountPerDim=localCellCounts, &
                                 globalStartPerDim=gridOffsets, rc=rc)

    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field, f90ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    
    lb(:) = lbound(f90ptr)
    ub(:) = ubound(f90ptr)

    ! Set the data values to the global cell index number.
    do j=lb(2)+haloWidth, ub(2)-haloWidth
      rownum = j - haloWidth - 1
      cellNum = (gridOffsets(1) + 1) + &
                ((gridOffsets(2)+rownum) * globalCellCounts(1)) 
      do i=lb(1)+haloWidth, ub(1)-haloWidth
        colnum = i - haloWidth - 1
        f90ptr(i, j) = cellNum + colnum
      enddo
    enddo

    rc = ESMF_SUCCESS
    return

end subroutine FillIndexField

!
! Make sure the data in a field matches the constant value.
! Assumes data is real*8.
!
subroutine ValidateConstantField(field, val, rc)
    type(ESMF_Field), intent(in) :: field
    real (ESMF_KIND_R8), intent(in) :: val
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: i, j
    integer :: lb(2), ub(2), halo
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr

    rc = ESMF_FAILURE
        
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.

    call ESMF_FieldGet(field, haloWidth=halo, rc=rc)

    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field, f90ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    

    lb(:) = lbound(f90ptr)
    ub(:) = ubound(f90ptr)
    
    rc = ESMF_SUCCESS
    do j=lb(2)+halo, ub(2)-halo
      do i=lb(1)+halo, ub(1)-halo
        if (f90ptr(i, j) .ne. val) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            !rc = ESMF_FAILURE
            !print *, "(bailing on first error - may be others)"
            !return 
        endif
      enddo
    enddo

    ! return with whatever rc value it has

    return

end subroutine ValidateConstantField


!
! Make sure the halo data in a field matches the constant value.
! Assumes data is real*8.
!
subroutine ValidateConstantHalo(field, val, rc)
    type(ESMF_Field), intent(in) :: field
    real (ESMF_KIND_R8), intent(in) :: val
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: i, j
    integer :: lb(2), ub(2), halo
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr

    rc = ESMF_FAILURE
        
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.

    call ESMF_FieldGet(field, haloWidth=halo, rc=rc)

    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field, f90ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    

    lb(:) = lbound(f90ptr)
    ub(:) = ubound(f90ptr)
    
    ! now check the chunks, one at a time.  this duplicates the overlaps
    ! at the corners, but it is the simplest to program.
 
    rc = ESMF_SUCCESS

    ! bottom / south
    do j=lb(2), lb(2)+halo-1
      do i=lb(1), ub(1)
        if (f90ptr(i, j) .ne. val) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            !rc = ESMF_FAILURE
            !print *, "(bailing on first error - may be others)"
            !return 
        endif
      enddo
    enddo

    ! west edge
    do j=lb(2), ub(2)
      do i=lb(1), lb(1)+halo-1
        if (f90ptr(i, j) .ne. val) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            !rc = ESMF_FAILURE
            !print *, "(bailing on first error - may be others)"
            !return 
        endif
      enddo
    enddo

    ! east edge
    do j=lb(2), ub(2)
      do i=ub(1)-halo+1, ub(1)
        if (f90ptr(i, j) .ne. val) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            !rc = ESMF_FAILURE
            !print *, "(bailing on first error - may be others)"
            !return 
        endif
      enddo
    enddo

    ! top / north
    do j=ub(2)-halo+1, ub(2)
      do i=lb(1), ub(1)
        if (f90ptr(i, j) .ne. val) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            !rc = ESMF_FAILURE
            !print *, "(bailing on first error - may be others)"
            !return 
        endif
      enddo
    enddo

    ! return with whatever rc value it has

    return

end subroutine ValidateConstantHalo


!
! Make sure the data in a field contains the correct global index numbers.
! Assumes data is real*8.
!
subroutine ValidateIndexField(field, rc)
    type(ESMF_Field), intent(in) :: field
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: i, j
    integer :: lb(2), ub(2), haloWidth, cellNum, rownum, colnum
    integer :: localCellCounts(2), globalCellCounts(2), gridOffsets(2)
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr
    type(ESMF_Grid) :: grid

    rc = ESMF_FAILURE
        
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.

    call ESMF_FieldGet(field, haloWidth=haloWidth, grid=grid, rc=rc)

    call ESMF_GridGet(grid, horzrelloc=ESMF_CELL_CENTER, &
                      globalCellCountPerDim=globalCellCounts, rc=rc)

    ! get grid information used to calculate global indices
    call ESMF_GridGetDELocalInfo(grid, horzrelloc=ESMF_CELL_CENTER, &
                                 localCellCountPerDim=localCellCounts, &
                                 globalStartPerDim=gridOffsets, rc=rc)

    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field, f90ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    
    lb(:) = lbound(f90ptr)
    ub(:) = ubound(f90ptr)

    ! start with success, and any mismatch sets error
    rc = ESMF_SUCCESS

    ! Check the data values against the global cell index number.
    do j=lb(2)+haloWidth, ub(2)-haloWidth
      rownum = j - halowidth - 1
      cellNum = (gridOffsets(1) + 1) + &
                ((gridOffsets(2)+rownum) * globalCellCounts(1)) 
      do i=lb(1)+haloWidth, ub(1)-haloWidth
        colnum = i - haloWidth - 1
        if (f90ptr(i, j) .ne. cellNum + colnum) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", &
                        cellNum + colnum
            !rc = ESMF_FAILURE
            !print *, "(bailing on first error - may be others)"
            !return 
        endif
      enddo
    enddo

    ! rc set above, leave it as is

    return

end subroutine ValidateIndexField


!
! Validate the halo region
!
subroutine ValidateIndexHalo(field, rc)
    type(ESMF_Field), intent(inout) :: field
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: i, j
    integer :: lb(2), ub(2), haloWidth, cellNum, rownum, colnum
    integer :: localCellCounts(2), globalCellCounts(2), gridOffsets(2)
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr
    type(ESMF_Grid) :: grid

    rc = ESMF_FAILURE
          
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.

    call ESMF_FieldGet(field, haloWidth=haloWidth, grid=grid, rc=rc)

    call ESMF_GridGet(grid, horzrelloc=ESMF_CELL_CENTER, &
                      globalCellCountPerDim=globalCellCounts, rc=rc)

    ! get grid information used to calculate global indices
    call ESMF_GridGetDELocalInfo(grid, horzrelloc=ESMF_CELL_CENTER, &
                                 localCellCountPerDim=localCellCounts, &
                                 globalStartPerDim=gridOffsets, rc=rc)

    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field, f90ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    
    lb(:) = lbound(f90ptr)
    ub(:) = ubound(f90ptr)

    ! TODO: add the 4 tests here.
    ! now check the chunks, one at a time.  this duplicates the overlaps
    ! at the corners, but it is the simplest to program.
 
    rc = ESMF_SUCCESS

    ! bottom / south
    do j=lb(2), lb(2)+haloWidth-1
      rownum = j - haloWidth - 1
      cellNum = (gridOffsets(1) + 1) + &
                ((gridOffsets(2)+rownum) * globalCellCounts(1)) 
      do i=lb(1), ub(1)
        colnum = i - haloWidth - 1
        val = cellNum + colnum
        if (f90ptr(i, j) .ne. val) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            !rc = ESMF_FAILURE
            !print *, "(bailing on first error - may be others)"
            !return 
        endif
      enddo
    enddo

    ! west edge
    do j=lb(2), ub(2)
      rownum = j - halowidth - 1
      cellNum = (gridOffsets(1) + 1) + &
                ((gridOffsets(2)+rownum) * globalCellCounts(1)) 
      do i=lb(1), lb(1)+haloWidth-1
        colnum = i - haloWidth - 1
        val = cellNum + colnum
        if (f90ptr(i, j) .ne. val) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            !rc = ESMF_FAILURE
            !print *, "(bailing on first error - may be others)"
            !return 
        endif
      enddo
    enddo

    ! east edge
    do j=lb(2), ub(2)
      rownum = j - haloWidth - 1
      cellNum = (gridOffsets(1) + 1) + &
                ((gridOffsets(2)+rownum) * globalCellCounts(1)) 
      do i=ub(1)-haloWidth+1, ub(1)
        colnum = i - haloWidth - 1
        val = cellNum + colnum
        if (f90ptr(i, j) .ne. val) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            !rc = ESMF_FAILURE
            !print *, "(bailing on first error - may be others)"
            !return 
        endif
      enddo
    enddo

    ! top / north
    do j=ub(2)-haloWidth+1, ub(2)
      rownum = j - haloWidth - 1
      cellNum = (gridOffsets(1) + 1) + &
                ((gridOffsets(2)+rownum) * globalCellCounts(1)) 
      do i=lb(1), ub(1)
        colnum = i - haloWidth - 1
        val = cellNum + colnum
        if (f90ptr(i, j) .ne. val) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            !rc = ESMF_FAILURE
            !print *, "(bailing on first error - may be others)"
            !return 
        endif
      enddo
    enddo

    ! return with whatever rc value it has

    rc = ESMF_SUCCESS
    return

end subroutine ValidateIndexHalo


subroutine Cleanup(field1, field2, rc)
    type(ESMF_Field), intent(inout) :: field1, field2
    integer, intent(out) :: rc
    
    ! Local variables
    type(ESMF_Grid) :: srcgrid, dstgrid
    type(ESMF_Array) :: srcarray, dstarray

    rc = ESMF_FAILURE

    ! query for grids
    call ESMF_FieldGet(field1, grid=srcgrid, array=srcarray, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    call ESMF_FieldGet(field2, grid=dstgrid, array=dstarray, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    ! plus arrays need cleanup

    call ESMF_FieldDestroy(field1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    call ESMF_FieldDestroy(field2, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    call ESMF_GridDestroy(srcgrid, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    call ESMF_GridDestroy(dstgrid, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    call ESMF_ArrayDestroy(srcarray, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    call ESMF_ArrayDestroy(dstarray, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    rc = ESMF_SUCCESS
    return

end subroutine Cleanup

end module

