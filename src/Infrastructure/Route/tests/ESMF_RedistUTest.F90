! $Id: ESMF_RedistUTest.F90,v 1.3 2005/09/23 17:01:11 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2005, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
      program ESMF_RedistUTest

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
!  "Redist" is sending data from one grid to another, where the grids 
!   themselves are identical, but the decomposition (which subsets of the
!   grid are located on each processor) is different.  Redist sends data
!   from one processor to another, with no interpolation.  See Regrid for
!   routines which do interpolation of data from one grid to another.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_RedistUTest.F90,v 1.3 2005/09/23 17:01:11 nscollins Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR*2) :: failMsg
      character(ESMF_MAXSTR) :: validate_options = "full"
      character(ESMF_MAXSTR) :: print_options = "all"

      ! local args needed to create/construct objects
      type(ESMF_RouteHandle) :: redist_rh
      type(ESMF_Field) :: field1, field2
      type(ESMF_VM) :: vm
      real(ESMF_KIND_R8) :: val1, val2


      ! -------- beginning of executable code below here -------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! create fields for later on
      call CreateFields(field1, field2, rc)
      write(name, *) "Creating src and dest fields"
      write(failMsg, *) "Unable to create src and/or dst fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! fill source field with known data
      val1 = 1.0
      call FillConstantField(field1, val1, rc)
      write(name, *) "Filling src field with known data values"
      write(failMsg, *) "Filling src field with known data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! fill destination field with known data
      val2 = -1.0
      call FillConstantField(field2, val2, rc)
      write(name, *) "Filling dst field with known data values"
      write(failMsg, *) "Filling dst field with known data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! store
      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_FieldRedistStore(field1, field2, vm, &
                                                routehandle=redist_rh, rc=rc)
      write(name, *) "Computing route for redist"
      write(failMsg, *) "Computing route for redist"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! run
      call ESMF_FieldRedist(field1, field2, routehandle=redist_rh, rc=rc)
      write(name, *) "Executing redist"
      write(failMsg, *) "Executing redist"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! release
      call ESMF_FieldRedistRelease(redist_rh, rc=rc)
      write(name, *) "Releasing route"
      write(failMsg, *) "Releasing route"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! validate destination field
      call ValidateConstantField(field2, val1, rc)
      write(name, *) "Validating data created dest fields"
      write(failMsg, *) "Validating data created dest fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! cleanup
      call Cleanup(field1, field2, rc)
      write(name, *) "Deleting fields at cleanup time"
      write(failMsg, *) "Deleting fields at cleanup time"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


10    continue

      call ESMF_TestEnd(result, ESMF_SRCLINE)
  
      ! -------- end of unit test code ------------------------

      contains


!
! Create 2 fields with the same grid but different layouts.
!
subroutine CreateFields(field1, field2, rc)
    ! ESMF Framework module
    use ESMF_Mod
    type(ESMF_Field), intent(out) :: field1, field2
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: npets, halo
    type(ESMF_Grid) :: srcgrid, dstgrid
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
    srcgrid = ESMF_GridCreateHorzXYUni((/ 90, 180 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_GRID_HORZ_STAGGER_A, &
                   name="srcgrid", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    call ESMF_GridDistribute(srcgrid, delayout=layout1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    ! same grid coordinates, but different layout
    dstgrid = ESMF_GridCreateHorzXYUni((/ 90, 180 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_GRID_HORZ_STAGGER_A, &
                   name="srcgrid", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    call ESMF_GridDistribute(dstgrid, delayout=layout2, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return


    ! Real*8, 2D data
    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_DATA_REAL, ESMF_R8, rc)
    if (rc.NE.ESMF_SUCCESS) return
    
    ! allow for a halo width of 3, let the field allocate the proper space
    field1 = ESMF_FieldCreate(srcgrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                haloWidth=halo, name="src pressure", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
                                
 
    ! allow for a halo width of 3, let the field allocate the proper space
    field2 = ESMF_FieldCreate(dstgrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
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
    ! ESMF Framework module
    use ESMF_Mod
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
! Fill a field with real*8 values which are the global cell index number.
!
subroutine FillIndexField(field, rc)
    ! ESMF Framework module
    use ESMF_Mod
    type(ESMF_Field), intent(inout) :: field
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: i, j
    integer :: lb(2), ub(2), haloWidth
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
    

    ! offsets are number of cells before the start of this one.  add one
    ! to set the start of the global cell numbers.
    lb(1) = gridOffsets(1) + 1
    lb(2) = gridOffsets(2) + 1

    ! calculate upper bounds from lower bounds and counts
    do i = 1,2
      ub(i) = lb(i) + localCellCounts(i) - 1
    enddo


    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field, f90ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    
    ! Set the data values to the global cell index number.
    do j=lb(2)+haloWidth, ub(2)-haloWidth
      do i=lb(1)+haloWidth, ub(1)-haloWidth
        f90ptr(i, j) = j + i
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
    ! ESMF Framework module
    use ESMF_Mod
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
! Make sure the data in a field contains the correct global index numbers.
! Assumes data is real*8.
!
subroutine ValidateIndexField(field, rc)
    ! ESMF Framework module
    use ESMF_Mod
    type(ESMF_Field), intent(in) :: field
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
    
    do j=lb(2)+halo, ub(2)-halo
      do i=lb(1)+halo, ub(1)-halo
        if (f90ptr(i, j) .ne. i*1000 + j) then
            ! TODO: got to compute this based on original i,j - not ones
            ! from other layout.
            !print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", i*1000+j
            !rc = ESMF_FAILURE
            !!print *, "(bailing on first error - may be others)"
            !!return 
        endif
      enddo
    enddo

    rc = ESMF_SUCCESS
    return

end subroutine ValidateIndexField



subroutine Cleanup(field1, field2, rc)
    ! ESMF Framework module
    use ESMF_Mod
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


      end program ESMF_RedistUTest



