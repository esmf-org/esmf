! $Id: ESMF_BundleRedistHelpers.F90,v 1.1 2005/10/12 19:06:16 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2005, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!

#define ESMF_FILENAME "ESMF_BundleRedistHelpers.F90"

#include <ESMF.h>

module ESMF_BundleRedistHelpers

   use ESMF_Mod

   public CreateGrids, CreateFields, CreateBundle
   public FillConstantField, FillIndexField, FillConstantHalo
   public FillConstantR4Field, FillConstantR4Halo
   public ValidateConstantField, ValidateConstantHalo
   public ValidateConstantR4Field, ValidateConstantR4Halo
   public ValidateIndexField
   public FieldCleanup, BundleCleanup

   public CreateGrid, CreateLayout
   public CreateEmptyDataField, CreateDataField

contains

!------------------------------------------------------------------------------
!
! Create 2 grids which have identical numbers of cells and coordinates,
! but are decomposed differently.
!
! TODO: pass in optional list of layouts, list of rellocs, and
!  make grid an array of grids?
!
#undef  ESMF_METHOD
#define ESMF_METHOD "CreateGrids"
subroutine CreateGrids(grid1, grid2, rc)
    type(ESMF_Grid), intent(out) :: grid1, grid2
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: npets
    type(ESMF_DELayout) :: layout1, layout2
    type(ESMF_VM) :: vm
    real (ESMF_KIND_R8), dimension(2) :: mincoords, maxcoords

 
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
    grid1 = ESMF_GridCreateHorzXYUni((/ 90, 180 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_GRID_HORZ_STAGGER_A, &
                   name="srcgrid", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    call ESMF_GridDistribute(grid1, delayout=layout1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    ! same grid coordinates, but different layout
    grid2 = ESMF_GridCreateHorzXYUni((/ 90, 180 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_GRID_HORZ_STAGGER_A, &
                   name="dstgrid", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    call ESMF_GridDistribute(grid2, delayout=layout2, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return


    rc = ESMF_SUCCESS
    return

end subroutine CreateGrids



!------------------------------------------------------------------------------
!
! Create 2 fields with the same grid
!
! todo: make fields a list and create as many as are in the list?
!
#undef  ESMF_METHOD
#define ESMF_METHOD "CreateFields"
subroutine CreateFields(grid1, field1, field2, rc)
    type(ESMF_Grid), intent(in) :: grid1
    type(ESMF_Field), intent(out) :: field1, field2
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: halo
    type(ESMF_ArraySpec) :: arrayspec

 
    rc = ESMF_FAILURE
        

    ! pick a default halowidth
    halo = 3

    ! Real*8, 2D data
    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_DATA_REAL, ESMF_R8, rc)
    if (rc.NE.ESMF_SUCCESS) return
    
    ! let field create call allocate the proper amount of space
    field1 = ESMF_FieldCreate(grid1, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                haloWidth=halo, name="src pressure", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
                                
    ! let field create call allocate the proper amount of space
    field2 = ESMF_FieldCreate(grid1, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                haloWidth=halo, name="dst pressure", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
                                
    rc = ESMF_SUCCESS
    return

end subroutine CreateFields


!------------------------------------------------------------------------------
!
! Create bundle and add up to 5 fields to it
!
! TODO: make fields an array and add as many as are in the list
!
#undef  ESMF_METHOD
#define ESMF_METHOD "CreateBundle"
subroutine CreateBundle(bundle, field1, field2, field3, field4, field5, rc)
    type(ESMF_Bundle), intent(out) :: bundle
    type(ESMF_Field), intent(in), optional :: field1,field2,field3,field4,field5
    integer, intent(out), optional :: rc
    
    integer :: localrc

    if (present(rc)) rc = ESMF_FAILURE
        
    ! make a bundle and add fields to it
    bundle = ESMF_BundleCreate(rc=localrc)
    if (localrc.NE.ESMF_SUCCESS) return

    if (present(field1)) then
      call ESMF_BundleAddField(bundle, field1, rc=localrc)
      if (localrc.NE.ESMF_SUCCESS) return
    endif

    if (present(field2)) then
      call ESMF_BundleAddField(bundle, field2, rc=localrc)
      if (localrc.NE.ESMF_SUCCESS) return
    endif

    if (present(field3)) then
      call ESMF_BundleAddField(bundle, field3, rc=localrc)
      if (localrc.NE.ESMF_SUCCESS) return
    endif

    if (present(field4)) then
      call ESMF_BundleAddField(bundle, field4, rc=localrc)
      if (localrc.NE.ESMF_SUCCESS) return
    endif

    if (present(field5)) then
      call ESMF_BundleAddField(bundle, field5, rc=localrc)
      if (localrc.NE.ESMF_SUCCESS) return
    endif

    if (present(rc)) rc = ESMF_SUCCESS
    return

end subroutine CreateBundle


!------------------------------------------------------------------------------
!
! Fill all the data associated with a field with a constant value.
! This assumes the data is real*8, 2D.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "FillConstantField"
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

!------------------------------------------------------------------------------
!
! Fill all the data associated with a field with a constant value.
! This assumes the data is real*4, 2D.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "FillConstantR4Field"
subroutine FillConstantR4Field(field, val, rc)
    type(ESMF_Field), intent(inout) :: field
    real (ESMF_KIND_R4), intent(in) :: val
    integer, intent(out) :: rc
    
    ! Local variables
    real (ESMF_KIND_R4), dimension(:,:), pointer :: f90ptr

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

end subroutine FillConstantR4Field


!------------------------------------------------------------------------------
!
! Fill the halo region (only) associated with a field with a constant value.
! This assumes the data is real*8, 2D.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "FillConstantHalo"
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


!------------------------------------------------------------------------------
!
! Fill the halo region (only) associated with a field with a constant value.
! This assumes the data is real*4, 2D.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "FillConstantR4Halo"
subroutine FillConstantR4Halo(field, val, rc)
    type(ESMF_Field), intent(inout) :: field
    real (ESMF_KIND_R4), intent(in) :: val
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: lb(2), ub(2), halo
    real (ESMF_KIND_R4), dimension(:,:), pointer :: f90ptr

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

end subroutine FillConstantR4Halo


!------------------------------------------------------------------------------
!
! Fill a field with real*8 values which are the global cell index number.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "FillIndexField"
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
      rownum = j - halowidth - 1
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

!------------------------------------------------------------------------------
!
! Make sure the data in a field matches the constant value.
! Assumes data is real*8.  Do not check halo regions.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "ValidateConstantField"
subroutine ValidateConstantField(field, val, slop, rc)
    type(ESMF_Field), intent(in) :: field
    real (ESMF_KIND_R8), intent(in) :: val
    logical, intent(in), optional :: slop
    integer, intent(out), optional  :: rc
    
    ! Local variables
    integer :: i, j
    integer :: lb(2), ub(2), halo
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr

    if (present(rc)) rc = ESMF_FAILURE
        
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.

    call ESMF_FieldGet(field, haloWidth=halo, rc=rc)

    ! if slop specified, and true, then do not check the outer row
    ! of cells in the computational area.  this is for regrid where
    ! the boundary conditions may affect the outer rows.
    if (present(slop)) then
        if (slop) halo = halo + 1
    endif

    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field, f90ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    

    lb(:) = lbound(f90ptr)
    ub(:) = ubound(f90ptr)
    
    rc = ESMF_SUCCESS
    do j=lb(2)+halo, ub(2)-halo
      do i=lb(1)+halo, ub(1)-halo
        if (abs(f90ptr(i, j) - val) .gt. 10E-8) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! return with whatever rc value it has

    return

end subroutine ValidateConstantField


!------------------------------------------------------------------------------
!
! Make sure the data in a field matches the constant value.
! Assumes data is real*4.  Do not check halo regions.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "ValidateConstantR4Field"
subroutine ValidateConstantR4Field(field, val, slop, rc)
    type(ESMF_Field), intent(in) :: field
    real (ESMF_KIND_R4), intent(in) :: val
    logical, intent(in), optional :: slop
    integer, intent(out), optional  :: rc
    
    ! Local variables
    integer :: i, j
    integer :: lb(2), ub(2), halo
    real (ESMF_KIND_R4), dimension(:,:), pointer :: f90ptr

    if (present(rc)) rc = ESMF_FAILURE
        
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.

    call ESMF_FieldGet(field, haloWidth=halo, rc=rc)

    ! if slop specified, and true, then do not check the outer row
    ! of cells in the computational area.  this is for regrid where
    ! the boundary conditions may affect the outer rows.
    if (present(slop)) then
        if (slop) halo = halo + 1
    endif

    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field, f90ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    

    lb(:) = lbound(f90ptr)
    ub(:) = ubound(f90ptr)
    
    rc = ESMF_SUCCESS
    do j=lb(2)+halo, ub(2)-halo
      do i=lb(1)+halo, ub(1)-halo
        if (abs(f90ptr(i, j) - val) .gt. 10E-8) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! return with whatever rc value it has

    return

end subroutine ValidateConstantR4Field


!------------------------------------------------------------------------------
!
! Make sure the halo data in a field matches the constant value.
! Assumes data is real*8.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "ValidateConstantHalo"
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
        if (abs(f90ptr(i, j) - val) .gt. 10E-8) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! west edge
    do j=lb(2), ub(2)
      do i=lb(1), lb(1)+halo-1
        if (abs(f90ptr(i, j) - val) .gt. 10E-8) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! east edge
    do j=lb(2), ub(2)
      do i=ub(1)-halo+1, ub(1)
        if (abs(f90ptr(i, j) - val) .gt. 10E-8) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! top / north
    do j=ub(2)-halo+1, ub(2)
      do i=lb(1), ub(1)
        if (abs(f90ptr(i, j) - val) .gt. 10E-8) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! return with whatever rc value it has

    return

end subroutine ValidateConstantHalo


!------------------------------------------------------------------------------
!
! Make sure the halo data in a field matches the constant value.
! Assumes data is real*4.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "ValidateConstantR4Halo"
subroutine ValidateConstantR4Halo(field, val, rc)
    type(ESMF_Field), intent(in) :: field
    real (ESMF_KIND_R4), intent(in) :: val
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: i, j
    integer :: lb(2), ub(2), halo
    real (ESMF_KIND_R4), dimension(:,:), pointer :: f90ptr

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
        if (abs(f90ptr(i, j) - val) .gt. 10E-8) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! west edge
    do j=lb(2), ub(2)
      do i=lb(1), lb(1)+halo-1
        if (abs(f90ptr(i, j) - val) .gt. 10E-8) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! east edge
    do j=lb(2), ub(2)
      do i=ub(1)-halo+1, ub(1)
        if (abs(f90ptr(i, j) - val) .gt. 10E-8) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! top / north
    do j=ub(2)-halo+1, ub(2)
      do i=lb(1), ub(1)
        if (abs(f90ptr(i, j) - val) .gt. 10E-8) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! return with whatever rc value it has

    return

end subroutine ValidateConstantR4Halo


!------------------------------------------------------------------------------
!
! Make sure the data in a field contains the correct global index numbers.
! Assumes data is real*8.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "ValidateIndexField"
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
        if (abs(f90ptr(i, j) - (cellNum+colnum)) .gt. 10E-8) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", &
                        cellNum + colnum
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! rc set above, leave it as is

    return

end subroutine ValidateIndexField


!------------------------------------------------------------------------------
!
! delete all fields; this code assumes that all share the identical grid
!  and only delete it once.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "FieldCleanup"
subroutine FieldCleanup(field1, field2, field3, field4, field5, rc)
    type(ESMF_Field), intent(inout) ::field1
    type(ESMF_Field), intent(inout), optional :: field2, field3, field4, field5
    integer, intent(out), optional :: rc
    
    ! Local variables
    type(ESMF_Grid) :: grid
    type(ESMF_Array) :: array

    if (present(rc)) rc = ESMF_FAILURE

    ! query for grid and data.  field1 is required; all other fields test
    ! first to be sure it is present
    call ESMF_FieldGet(field1, grid=grid, array=array, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    call ESMF_FieldDestroy(field1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    call ESMF_ArrayDestroy(array, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    ! query for data only; grid is shared and will be deleted last.
    if (present(field2)) then
        call ESMF_FieldGet(field2, array=array, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return

        call ESMF_FieldDestroy(field2, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return

        call ESMF_ArrayDestroy(array, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return
    endif

    ! query for data only; grid is shared and will be deleted last.
    if (present(field3)) then
        call ESMF_FieldGet(field3, array=array, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return

        call ESMF_FieldDestroy(field3, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return

        call ESMF_ArrayDestroy(array, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return
    endif

    ! query for data only; grid is shared and will be deleted last.
    if (present(field4)) then
        call ESMF_FieldGet(field4, array=array, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return

        call ESMF_FieldDestroy(field4, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return

        call ESMF_ArrayDestroy(array, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return
    endif

    ! query for data only; grid is shared and will be deleted last.
    if (present(field5)) then
        call ESMF_FieldGet(field5, array=array, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return

        call ESMF_FieldDestroy(field5, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return

        call ESMF_ArrayDestroy(array, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return
    endif

    ! do this last.
    call ESMF_GridDestroy(grid, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    if (present(rc)) rc = ESMF_SUCCESS
    return

end subroutine FieldCleanup

!------------------------------------------------------------------------------
! TODO: make this recurse and delete the grid, the fields, the data inside
!  the fields - an all-in-one which assumes no sharing of any of the contents.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "BundleCleanup"
subroutine BundleCleanup(bundle, rc)
    type(ESMF_Bundle), intent(inout) :: bundle
    integer, intent(out) :: rc
    

    rc = ESMF_FAILURE

    call ESMF_BundleDestroy(bundle, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    rc = ESMF_SUCCESS
    return

end subroutine BundleCleanup


!------------------------------------------------------------------------------
!
! Create a field.  All args but the name can be defaulted.
!  Only 1 of the two data values can be specified; it sets the data type
!  as well as the data value.  If neither specified, R8 is default.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "CreateDataField"
function CreateDataField(name, grid, layout, relloc, r4value, r8value, rc)
  type(ESMF_Field) :: CreateDataField

  character(len=*), intent(in) :: name                ! field name
  type(ESMF_Grid), intent(in), optional :: grid       ! if set, grid to use
  type(ESMF_DELayout), intent(in), optional :: layout ! if set, layout to use
  type(ESMF_RelLoc), intent(in), optional :: relloc   ! if set, relloc to use
  real(ESMF_KIND_R4), intent(in), optional :: r4value ! if set, initial value
  real(ESMF_KIND_R8), intent(in), optional :: r8value ! if set, initial value
  integer, intent(out), optional :: rc                ! return code

  type(ESMF_Grid) :: thisgrid
  type(ESMF_DELayout) :: thislayout
  type(ESMF_RelLoc) :: thisrelloc
  real(ESMF_KIND_R4) :: thisr4data
  real(ESMF_KIND_R8) :: thisr8data
  type(ESMF_ArraySpec) :: as
  type(ESMF_VM) :: vm
  integer :: status
  logical :: use_r8
  real(ESMF_KIND_R4), pointer :: r4data(:,:)
  real(ESMF_KIND_R8), pointer :: r8data(:,:)

  ! TODO: what about making sure the function return is null?
  if (present(rc)) rc = ESMF_FAILURE

  ! Set defaults here based on whether the arguments are present or not.

  ! default layout
  if (present(layout)) then
      thislayout = layout
  else
      call ESMF_VMGetGlobal(vm, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      thislayout = ESMF_DELayoutCreate(vm, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10
  endif

  ! default grid
  if (.not. present(grid)) then
      thisgrid = ESMF_GridCreateHorzXYUni(counts=(/100, 200/), &
                               minGlobalCoordPerDim=(/0.0_ESMF_KIND_R8, &
                                                      0.0_ESMF_KIND_R8/), &
                               maxGlobalCoordPerDim=(/180.0_ESMF_KIND_R8, &
                                                      360.0_ESMF_KIND_R8/), &
                               horzStagger=ESMF_GRID_HORZ_STAGGER_B_NE, &
                               rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      call ESMF_GridDistribute(thisgrid, delayout=thislayout, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10
  else
      thisgrid = grid
  endif

  ! default relloc
  if (present(relloc)) then
      thisrelloc = relloc
  else
      thisrelloc = ESMF_CELL_CENTER
  endif

  ! default data - both type and value.  only one of r4value or r8value
  ! can be specified.  if neither, default is r8.
  if (present(r4value) .and. present(r8value)) then
      call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
                               "Cannot specify both *4 and *8 values", &
                                ESMF_CONTEXT, rc)
      goto 10
  else if (present(r4value)) then
      use_r8 = .false.
      thisr4data = r4value
  else if (present(r8value)) then
      use_r8 = .true.
      thisr8data = r8value
  else ! neither specified, default to r8
      use_r8 = .true.
      thisr8data = 3.14159
  endif


  ! fixed items:  2d array, data type real, halo width of 2.
  if (use_r8) then
      call ESMF_ArraySpecSet(as, rank=2, type=ESMF_DATA_REAL, &
                             kind=ESMF_R8, rc=status)
  else
      call ESMF_ArraySpecSet(as, rank=2, type=ESMF_DATA_REAL, &
                             kind=ESMF_R4, rc=status)
  endif
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  ! make the new field
  CreateDataField = ESMF_FieldCreate(grid=thisgrid, arrayspec=as, &
                                     horzRelloc=thisrelloc, haloWidth=2, &
                                     name=name, rc=status)
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  ! initialize the data field
  if (use_r8) then
      call ESMF_FieldGetDataPointer(CreateDataField, r8data, ESMF_DATA_REF, rc)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10
 
      r8data(:,:) = thisr8data
  else 
      call ESMF_FieldGetDataPointer(CreateDataField, r4data, ESMF_DATA_REF, rc)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10
 
      r4data(:,:) = thisr4data
  endif
  

10 continue
  ! if coming here because of error, rc is already set to the error code.
  ! if falling thru the code, rc is was set to success by logerr.

end function CreateDataField


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "CreateLayout"
function CreateLayout(which, rc)
  type(ESMF_DELayout) :: CreateLayout

  integer, intent(in), optional :: which         ! optional layout type
  integer, intent(out), optional :: rc           ! return code

  type(ESMF_VM) :: vm
  integer :: npets, status, thiswhich


  if (present(rc)) rc = ESMF_FAILURE

  call ESMF_VMGetGlobal(vm, rc=status)
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  call ESMF_VMGet(vm, petCount=npets, rc=status)
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  ! if running with only 1 PET, this is your only option.
  if (npets .eq. 1) then
      CreateLayout = ESMF_DELayoutCreate(vm, (/ 1, 1 /), rc=status)

  else
      ! if not specified, use the default case: npets by 1
      if (.not.present(which)) then
          thiswhich = 0
      else
          thiswhich = which
      endif

      select case (thiswhich)
      case (1)
        ! npets must be even
        if (modulo(npets, 2) .ne. 0) then
          CreateLayout = ESMF_DELayoutCreate(vm, (/ npets, 1 /), rc=status)
        else
          CreateLayout = ESMF_DELayoutCreate(vm, (/ npets/2, 2 /), rc=status)
        endif

      case (2)
        ! npets must be even
        if (modulo(npets, 2) .ne. 0) then
          CreateLayout = ESMF_DELayoutCreate(vm, (/ npets, 1 /), rc=status)
        else
          CreateLayout = ESMF_DELayoutCreate(vm, (/ 2, npets/2 /), rc=status)
        endif

      case (3)
        ! npets must be evenly divisible by 4
        if (modulo(npets, 4) .ne. 0) then
          CreateLayout = ESMF_DELayoutCreate(vm, (/ npets, 1 /), rc=status)
        else
          CreateLayout = ESMF_DELayoutCreate(vm, (/ npets/4, 4 /), rc=status)
        endif
 
      case (4)
        ! npets must be evenly divisible by 4
        if (modulo(npets, 4) .ne. 0) then
          CreateLayout = ESMF_DELayoutCreate(vm, (/ npets, 1 /), rc=status)
        else
          CreateLayout = ESMF_DELayoutCreate(vm, (/ 4, npets/4 /), rc=status)
        endif

      case (5)
          CreateLayout = ESMF_DELayoutCreate(vm, (/ 1, npets /), rc=status)

      case default
          CreateLayout = ESMF_DELayoutCreate(vm, (/ npets, 1 /), rc=status)

      end select
  endif
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

10 continue
  ! rc will have been set by the call to logerr; 
  ! just return at this point

end function CreateLayout


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "CreateGrid"
function CreateGrid(which, layout, rc)
  type(ESMF_Grid) :: CreateGrid

  integer, intent(in), optional :: which               ! optional grid type
  type(ESMF_DELayout), intent(in), optional :: layout  ! layout to use
  integer, intent(out), optional :: rc                 ! return code

  type(ESMF_VM) :: vm
  type(ESMF_DELayout) :: thislayout
  real (ESMF_KIND_R8), dimension(2) :: mincoords1, maxcoords1
  real (ESMF_KIND_R8), dimension(2) :: mincoords2, maxcoords2
  integer :: npets, status, thiswhich


  if (present(rc)) rc = ESMF_FAILURE

  if (present(layout)) then
      thislayout = layout
  else
      call ESMF_VMGetGlobal(vm, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10
    
      call ESMF_VMGet(vm, petCount=npets, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10
    
      thislayout = ESMF_DELayoutCreate(vm, (/ 1, npets /), rc=status)
   endif

   ! if not specified, use a default grid spacing
   if (.not.present(which)) then
       thiswhich = 0
   else
       thiswhich = which
   endif

   ! used below in cases 1-4
   mincoords1 = (/  0.0,  0.0 /)
   maxcoords1 = (/ 180.0, 360.0 /)

   ! used below in cases 11-14
   mincoords2 = (/  0.0,  0.0 /)
   maxcoords2 = (/ 120.0, 300.0 /)

   select case (thiswhich)
   case (1)
      CreateGrid = ESMF_GridCreateHorzXYUni((/100, 200/), &
                                     mincoords1, maxcoords1, &
                                     horzStagger=ESMF_GRID_HORZ_STAGGER_B_NE, &
                                     rc=status)

    case (2)
      CreateGrid = ESMF_GridCreateHorzXYUni((/300, 500/), &
                                     mincoords1, maxcoords1, &
                                     horzStagger=ESMF_GRID_HORZ_STAGGER_B_NE, &
                                     rc=status)

    case (3)
      CreateGrid = ESMF_GridCreateHorzXYUni((/10, 20/), &
                                     mincoords1, maxcoords1, &
                                     horzStagger=ESMF_GRID_HORZ_STAGGER_B_NE, &
                                     rc=status)

    case (4)
      CreateGrid = ESMF_GridCreateHorzXYUni((/90, 180/), &
                                     mincoords1, maxcoords1, &
                                     horzStagger=ESMF_GRID_HORZ_STAGGER_B_NE, &
                                     rc=status)

    case (11)
      CreateGrid  = ESMF_GridCreateHorzXYUni((/ 90, 180 /), &
                                        mincoords2, maxcoords2, &
                                        horzStagger=ESMF_GRID_HORZ_STAGGER_A, &
                                        rc=status)

    case (12)
      CreateGrid  = ESMF_GridCreateHorzXYUni((/ 400, 700 /), &
                                        mincoords2, maxcoords2, &
                                        horzStagger=ESMF_GRID_HORZ_STAGGER_A, &
                                        rc=status)

    case (13)
      CreateGrid  = ESMF_GridCreateHorzXYUni((/ 15, 80 /), &
                                        mincoords2, maxcoords2, &
                                        horzStagger=ESMF_GRID_HORZ_STAGGER_A, &
                                        rc=status)

    case (14)
      CreateGrid  = ESMF_GridCreateHorzXYUni((/ 900, 1800 /), &
                                        mincoords2, maxcoords2, &
                                        horzStagger=ESMF_GRID_HORZ_STAGGER_A, &
                                        rc=status)

    case default
      CreateGrid = ESMF_GridCreateHorzXYUni((/100, 200/), &
                                            mincoords1, maxcoords1, &
                                            rc=status)

  end select
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10


  ! distribute the grid across the PETs
  call ESMF_GridDistribute(CreateGrid, delayout=thislayout, rc=status)
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10
   

10 continue
  ! rc will have been set by the call to logerr; 
  ! just return at this point

end function CreateGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "CreateEmptyDataField"
function CreateEmptyDataField(name, rc)
  type(ESMF_Field) :: CreateEmptyDataField

  character(len=*), intent(in) :: name
  integer, intent(out) :: rc

  integer :: status

  rc = ESMF_FAILURE
  CreateEmptyDataField = ESMF_FieldCreateNoData(name=name, rc=status)
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

10 continue
  ! rc will have been set by the call to logerr; 
  ! just return at this point

end function CreateEmptyDataField

!------------------------------------------------------------------------------

end module
