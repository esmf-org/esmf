! $Id: ESMF_RedistHelpers.F90,v 1.9.2.2 2009/01/21 21:25:23 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      module ESMF_RedistHelpers

#include "ESMF_Macros.inc"

!------------------------------------------------------------------------------
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_RedistUTest - Data redistribution tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Redist unit tests, using the Route code.
!
!  "Redist" is sending data from one field to another, where the igrids 
!   themselves are identical, but the decompositions (which subsets of the
!   igrid are located on each processor) are different.  Redist sends data
!   from one processor to another with no interpolation.  See Regrid for
!   routines which do data interpolation from one igrid to another.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod
      implicit none

     public CreateFields, FillConstantField, FillIndexField, ValidateConstantField
     public ValidateConstantHalo, ValidateIndexField, Cleanup


      contains


!
! Create 2 fields with the same igrid but different layouts.
!
subroutine CreateFields(field1, field2, rc)
    type(ESMF_Field), intent(out) :: field1, field2
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: npets, halo
    type(ESMF_IGrid) :: srcigrid, dstigrid
    type(ESMF_ArraySpec) :: arrayspec
    !type(ESMF_FieldDataMap) :: datamap
    type(ESMF_DELayout) :: layout1, layout2
    type(ESMF_VM) :: vm
    real (ESMF_KIND_R8), dimension(2) :: mincoords, maxcoords

 
    ! pick a default halowidth, must be same for both src and dst
    halo = 3

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
    srcigrid = ESMF_IGridCreateHorzXYUni((/ 90, 180 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_IGRID_HORZ_STAGGER_A, &
                   name="srcigrid", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    call ESMF_IGridDistribute(srcigrid, delayout=layout1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    ! same igrid coordinates, but different layout
    dstigrid = ESMF_IGridCreateHorzXYUni((/ 90, 180 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_IGRID_HORZ_STAGGER_A, &
                   name="srcigrid", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    call ESMF_IGridDistribute(dstigrid, delayout=layout2, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return


    ! Real*8, 2D data
    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)
    if (rc.NE.ESMF_SUCCESS) return
    
    ! allow for a halo width of 3, let the field allocate the proper space
    field1 = ESMF_FieldCreate(srcigrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                haloWidth=halo, name="src pressure", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
                                
 
    ! allow for a halo width of 3, let the field allocate the proper space
    field2 = ESMF_FieldCreate(dstigrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                haloWidth=halo, name="dst pressure", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
                                
    rc = ESMF_SUCCESS
    return

end subroutine CreateFields


!
! Fill all the data associated with a field with a constant value.
! This assumes the data is real*8, 2D.
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
    ! constant value, then we will need the halo width and the igrid info.
    ! for now, simply set the entire data space to the constant value.
    !call ESMF_FieldGet(field, haloWidth=halo, igrid=igrid, rc=rc)

    ! get a Fortran 90 pointer back to the data block
    call ESMF_FieldGetDataPointer(field, f90ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    
    f90ptr(:,:) = val

    rc = ESMF_SUCCESS
    return

end subroutine FillConstantField

!
! Fill a field with real*8 values which are the global cell index number.
!
subroutine FillIndexField(field, rc)
    type(ESMF_Field), intent(inout) :: field
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: i, j
    integer :: lb(2), ub(2), haloWidth, cellNum, rownum, colnum
    integer :: localCellCounts(2), globalCellCounts(2), igridOffsets(2)
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr
    type(ESMF_IGrid) :: igrid

    rc = ESMF_FAILURE
          
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.

    call ESMF_FieldGet(field, haloWidth=haloWidth, igrid=igrid, rc=rc)

    call ESMF_IGridGet(igrid, horzrelloc=ESMF_CELL_CENTER, &
                      globalCellCountPerDim=globalCellCounts, rc=rc)

    ! get igrid information used to calculate global indices
    call ESMF_IGridGetDELocalInfo(igrid, horzrelloc=ESMF_CELL_CENTER, &
                                 localCellCountPerDim=localCellCounts, &
                                 globalStartPerDim=igridOffsets, rc=rc)

    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field, f90ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    
    lb(:) = lbound(f90ptr)
    ub(:) = ubound(f90ptr)

    ! Set the data values to the global cell index number.
    do j=lb(2)+haloWidth, ub(2)-haloWidth
      rownum = j - halowidth - 1
      cellNum = (igridOffsets(1) + 1) + &
                ((igridOffsets(2)+rownum) * globalCellCounts(1)) 
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
    type(ESMF_Field), intent(inout) :: field
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
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
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
    type(ESMF_Field), intent(inout) :: field
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
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! west edge
    do j=lb(2), ub(2)
      do i=lb(1), lb(1)+halo-1
        if (f90ptr(i, j) .ne. val) then
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
        if (f90ptr(i, j) .ne. val) then
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
        if (f90ptr(i, j) .ne. val) then
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


!
! Make sure the data in a field contains the correct global index numbers.
! Assumes data is real*8.
!
subroutine ValidateIndexField(field, rc)
    type(ESMF_Field), intent(inout) :: field
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: i, j
    integer :: lb(2), ub(2), haloWidth, cellNum, rownum, colnum
    integer :: localCellCounts(2), globalCellCounts(2), igridOffsets(2)
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr
    type(ESMF_IGrid) :: igrid

    rc = ESMF_FAILURE
        
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.

    call ESMF_FieldGet(field, haloWidth=haloWidth, igrid=igrid, rc=rc)

    call ESMF_IGridGet(igrid, horzrelloc=ESMF_CELL_CENTER, &
                      globalCellCountPerDim=globalCellCounts, rc=rc)

    ! get igrid information used to calculate global indices
    call ESMF_IGridGetDELocalInfo(igrid, horzrelloc=ESMF_CELL_CENTER, &
                                 localCellCountPerDim=localCellCounts, &
                                 globalStartPerDim=igridOffsets, rc=rc)

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
      cellNum = (igridOffsets(1) + 1) + &
                ((igridOffsets(2)+rownum) * globalCellCounts(1)) 
      do i=lb(1)+haloWidth, ub(1)-haloWidth
        colnum = i - haloWidth - 1
        if (f90ptr(i, j) .ne. cellNum + colnum) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", &
                        cellNum + colnum
            rc = ESMF_FAILURE
            !!print *, "(bailing on first error - may be others)"
            !!return 
        endif
      enddo
    enddo

    ! rc set above, leave it as is

    return

end subroutine ValidateIndexField



subroutine Cleanup(field1, field2, rc)
    type(ESMF_Field), intent(inout) :: field1, field2
    integer, intent(out) :: rc
    
    ! Local variables
    type(ESMF_IGrid) :: srcigrid, dstigrid

    rc = ESMF_FAILURE

    ! query for igrids
    call ESMF_FieldGet(field1, igrid=srcigrid, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    call ESMF_FieldGet(field2, igrid=dstigrid, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    ! plus arrays need cleanup

    call ESMF_FieldDestroy(field1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    call ESMF_FieldDestroy(field2, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    call ESMF_IGridDestroy(srcigrid, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    call ESMF_IGridDestroy(dstigrid, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    rc = ESMF_SUCCESS
    return

end subroutine Cleanup


      end module ESMF_RedistHelpers



