! $Id: ESMF_HaloHelpers.F90,v 1.14.2.2 2009/01/21 21:25:23 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!

module ESMF_HaloHelpers
 
 use ESMF_Mod

 public CreateFields, FillConstantField, FillConstantHalo 
 public FillIndexField, ValidateConstantField, ValidateConstantHalo 
 public ValidateIndexField, ValidateIndexHalo, Cleanup 

contains

!
! Create 2 fields with the same igrid but different layouts.
! This igrid is periodic in both directions.
!
! TODO: We need to create a non-periodic igrid as well.
!  That complicates the error checking part (the exterior edges 
!  should not have been altered but interior should be).
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
    type(ESMF_Logical) :: wrap(2)
    type(ESMF_VM) :: vm
    real (ESMF_KIND_R8), dimension(2) :: mincoords, maxcoords

 
    ! pick a default halowidth, must be same for both src and dst
    halo = 3

    ! the validate routines now account for both periodic and nonperiodic
    ! igrids, so this does not have to always be true.
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
    srcigrid = ESMF_IGridCreateHorzXYUni((/ 90, 180 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_IGRID_HORZ_STAGGER_A, &
                   periodic=wrap,  &
                   name="srcigrid", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    call ESMF_IGridDistribute(srcigrid, delayout=layout1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    ! same igrid coordinates, but different layout
    dstigrid = ESMF_IGridCreateHorzXYUni((/ 90, 180 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_IGRID_HORZ_STAGGER_A, &
                   periodic=wrap,  &
                   name="srcigrid", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    call ESMF_IGridDistribute(dstigrid, delayout=layout2, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return


    ! Real*8, 2D data
    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)
    if (rc.NE.ESMF_SUCCESS) return
    
    ! specify halo width; let the field allocate the proper space
    field1 = ESMF_FieldCreate(srcigrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                haloWidth=halo, name="src pressure", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
                                
 
    ! specify halo width; let the field allocate the proper space
    field2 = ESMF_FieldCreate(dstigrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
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
    integer :: lb(2), ub(2), haloWidth, localRownum, localColnum
    integer :: globalCellCounts(2), globalOffsets(2), globalRowstart
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
                                 globalStartPerDim=globalOffsets, rc=rc)

    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field, f90ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    
    lb(:) = lbound(f90ptr)
    ub(:) = ubound(f90ptr)

    ! Set the data values to the global cell index number.
    do j=lb(2)+haloWidth, ub(2)-haloWidth
      localRownum = j - haloWidth 
      globalRowstart = ((localRownum + globalOffsets(2) - 1) &
                     *   globalCellCounts(1))  + globalOffsets(1)
      do i=lb(1)+haloWidth, ub(1)-haloWidth
        localColnum = i - haloWidth 
        f90ptr(i, j) = globalRowstart + localColnum
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
    
    ! TODO: need to query overall igrid like the ValidateIndexHalo code
    !  and handle the external edges and corners differently.

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
            rc = ESMF_FAILURE
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
            rc = ESMF_FAILURE
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
            rc = ESMF_FAILURE
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
    type(ESMF_Field), intent(inout) :: field
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: i, j
    integer :: lb(2), ub(2), haloWidth, localRownum, localColnum
    integer :: globalCellCounts(2), globalOffsets(2), globalRowstart
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
                                 globalStartPerDim=globalOffsets, rc=rc)

    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field, f90ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    
    lb(:) = lbound(f90ptr)
    ub(:) = ubound(f90ptr)

    ! start with success, and any mismatch sets error
    rc = ESMF_SUCCESS

    ! Check the data values against the global cell index number.
    do j=lb(2)+haloWidth, ub(2)-haloWidth
      localRownum = j - haloWidth 
      globalRowstart = ((localRownum + globalOffsets(2) - 1) &
                     *   globalCellCounts(1))  + globalOffsets(1)
      do i=lb(1)+haloWidth, ub(1)-haloWidth
        localColnum = i - haloWidth 
        if (f90ptr(i, j) .ne. (globalRowstart + localColnum)) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", &
                        globalRowstart + localColnum
            rc = ESMF_FAILURE
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
subroutine ValidateIndexHalo(field, originalval, rc)
    type(ESMF_Field), intent(inout) :: field
    real (ESMF_KIND_R8), intent(in) :: originalval
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: i, j
    integer :: lb(2), ub(2), haloWidth, localRownum, localColnum
    integer :: globalCellCounts(2), globalOffsets(2), globalRowstart
    integer :: deid, ncount(2), pos(2)
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr
    type(ESMF_DELayout) :: delayout
    type(ESMF_Logical) :: periodic(2)
    type(ESMF_IGrid) :: igrid
    logical :: top, bottom, right, left
    logical :: xperiodic, yperiodic

    rc = ESMF_FAILURE
          
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.

    call ESMF_FieldGet(field, haloWidth=haloWidth, igrid=igrid, rc=rc)

    call ESMF_IGridGet(igrid, horzrelloc=ESMF_CELL_CENTER, &
                      globalCellCountPerDim=globalCellCounts, rc=rc)

    ! get igrid information used to calculate global indices
    call ESMF_IGridGetDELocalInfo(igrid, horzrelloc=ESMF_CELL_CENTER, &
                                 globalStartPerDim=globalOffsets, rc=rc)

    ! figure out where we are in the overall layout, and set flags in case
    ! we are on the boundaries relative to the entire igrid.
    call ESMF_IGridGet(igrid, delayout=delayout, periodic=periodic, rc=rc)
    call ESMF_DELayoutGetDeprecated(delayout, deCountPerDim=ncount, localDE=deid, rc=rc)
    call ESMF_DELayoutGetDELocalInfo(delayout, deid, coord=pos, rc=rc)
   
    xperiodic = (periodic(1) .eq. ESMF_TRUE)
    yperiodic = (periodic(2) .eq. ESMF_TRUE)

    top = .FALSE.
    bottom = .FALSE.
    right = .FALSE.
    left = .FALSE.

    ! is our chunk of the data on any of the overall igrid boundaries?
    if (pos(1) .eq. ncount(1)) right = .TRUE.
    if (pos(1) .eq. 1)         left = .TRUE. 
    if (pos(2) .eq. ncount(2)) top = .TRUE.
    if (pos(2) .eq. 1)         bottom = .TRUE.


    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field, f90ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    
    lb(:) = lbound(f90ptr)
    ub(:) = ubound(f90ptr)

    ! Debug only.
    !print *, "data is"
    !do j=lb(2), ub(2)
    !    print *, f90ptr(:,j)
    !enddo

    ! now check the chunks, one at a time.  if we are on the boundary of
    ! the entire igrid, treat the external boundaries separately.
 
    rc = ESMF_SUCCESS

    ! bottom left corner:
    do j=lb(2), lb(2)+haloWidth-1
      localRownum = j - haloWidth 
      if (bottom .and. yperiodic) then
          localRownum = localRownum + globalCellCounts(2) 
      endif
      globalRowstart = ((localRownum + globalOffsets(2) - 1) &
                     *   globalCellCounts(1))  + globalOffsets(1)
      do i=lb(1), lb(1)+haloWidth-1
        localColnum = i - haloWidth 
        if (left .and. xperiodic) then
            localColnum = localColnum + globalCellCounts(1)
        endif

        ! for those edges where halo should not have been updated, make sure
        ! the original value remained.  otherwise, compute the index value.
        ! the row and col numbers have already been updated for periodic cases.
        if ((left .and. bottom) .or. &
            (left .and. (.not. xperiodic)) .or. &
            (bottom .and. (.not. yperiodic))) then
            val = originalval
        else
            val = globalRowstart + localColnum
        endif

        if (f90ptr(i, j) .ne. val) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            !print *, "(bailing on first error - may be others)"
            !return 
        endif
      enddo
    enddo

    ! bottom (without corners):
    do j=lb(2), lb(2)+haloWidth-1
      localRownum = j - haloWidth 
      if (bottom .and. yperiodic) then
          localRownum = localRownum + globalCellCounts(2) 
      endif
      globalRowstart = ((localRownum + globalOffsets(2) - 1) &
                     *   globalCellCounts(1))  + globalOffsets(1)
      do i=lb(1)+haloWidth, ub(1)-haloWidth
        localColnum = i - haloWidth 

        ! for those edges where halo should not have been updated, make sure
        ! the original value remained.  otherwise, compute the index value.
        ! the row and col numbers have already been updated for periodic cases.
        if (bottom .and. (.not. yperiodic)) then
            val = originalval
        else
            val = globalRowstart + localColnum
        endif

        if (f90ptr(i, j) .ne. val) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            !print *, "(bailing on first error - may be others)"
            !return 
        endif
      enddo
    enddo

    ! bottom right corner:
    do j=lb(2), lb(2)+haloWidth-1
      localRownum = j - haloWidth 
      if (bottom .and. yperiodic) then
          localRownum = localRownum + globalCellCounts(2) 
      endif
      globalRowstart = ((localRownum + globalOffsets(2) - 1) &
                     *   globalCellCounts(1))  + globalOffsets(1)
      do i=ub(1)-haloWidth+1, ub(1)
        localColnum = i - haloWidth 
        if (right .and. xperiodic) then
            localColnum = localColnum - globalCellCounts(1)
        endif

        ! for those edges where halo should not have been updated, make sure
        ! the original value remained.  otherwise, compute the index value.
        ! the row and col numbers have already been updated for periodic cases.
        if ((right .and. bottom) .or. &
            (right .and. (.not. xperiodic)) .or. &
            (bottom .and. (.not. yperiodic))) then
            val = originalval
        else
            val = globalRowstart + localColnum
        endif

        if (f90ptr(i, j) .ne. val) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            !print *, "(bailing on first error - may be others)"
            !return 
        endif
      enddo
    enddo

    ! left (without corners):
    do j=lb(2)+haloWidth, ub(2)-haloWidth
      localRownum = j - haloWidth 
      globalRowstart = ((localRownum + globalOffsets(2) - 1) &
                     *   globalCellCounts(1))  + globalOffsets(1)
      do i=lb(1), lb(1)+haloWidth-1
        localColnum = i - haloWidth 
        if (left .and. xperiodic) then
            localColnum = localColnum + globalCellCounts(1)
        endif

        ! for those edges where halo should not have been updated, make sure
        ! the original value remained.  otherwise, compute the index value.
        ! the row and col numbers have already been updated for periodic cases.
        if (left .and. (.not. xperiodic)) then
            val = originalval
        else
            val = globalRowstart + localColnum
        endif

        if (f90ptr(i, j) .ne. val) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            !print *, "(bailing on first error - may be others)"
            !return 
        endif
      enddo
    enddo

    ! right (without corners):
    do j=lb(2)+haloWidth, ub(2)-haloWidth
      localRownum = j - haloWidth 
      globalRowstart = ((localRownum + globalOffsets(2) - 1) &
                     *   globalCellCounts(1))  + globalOffsets(1)
      do i=ub(1)-haloWidth+1, ub(1)
        localColnum = i - haloWidth 
        if (right .and. xperiodic) then
            localColnum = localColnum - globalCellCounts(1)
        endif

        ! for those edges where halo should not have been updated, make sure
        ! the original value remained.  otherwise, compute the index value.
        ! the row and col numbers have already been updated for periodic cases.
        if (right .and. (.not. xperiodic)) then
            val = originalval
        else
            val = globalRowstart + localColnum
        endif

        if (f90ptr(i, j) .ne. val) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            !print *, "(bailing on first error - may be others)"
            !return 
        endif
      enddo
    enddo

    ! top left corner:
    do j=ub(2)-haloWidth+1, ub(2)
      localRownum = j - haloWidth 
      if (top .and. yperiodic) then
          localRownum = localRownum - globalCellCounts(2) 
      endif
      globalRowstart = ((localRownum + globalOffsets(2) - 1) &
                     *   globalCellCounts(1))  + globalOffsets(1)
      do i=lb(1), lb(1)+haloWidth-1
        localColnum = i - haloWidth 
        if (left .and. xperiodic) then
            localColnum = localColnum + globalCellCounts(1)
        endif

        ! for those edges where halo should not have been updated, make sure
        ! the original value remained.  otherwise, compute the index value.
        ! the row and col numbers have already been updated for periodic cases.
        if ((left .and. top) .or. &
            (left .and. (.not. xperiodic)) .or. &
            (top .and. (.not. yperiodic))) then
            val = originalval
        else
            val = globalRowstart + localColnum
        endif

        if (f90ptr(i, j) .ne. val) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            !print *, "(bailing on first error - may be others)"
            !return 
        endif
      enddo
    enddo

    ! top (without corners):
    do j=ub(2)-haloWidth+1, ub(2)
      localRownum = j - haloWidth 
      if (top .and. yperiodic) then
          localRownum = localRownum - globalCellCounts(2) 
      endif
      globalRowstart = ((localRownum + globalOffsets(2) - 1) &
                     *   globalCellCounts(1))  + globalOffsets(1)
      do i=lb(1)+haloWidth, ub(1)-haloWidth
        localColnum = i - haloWidth 

        ! for those edges where halo should not have been updated, make sure
        ! the original value remained.  otherwise, compute the index value.
        ! the row and col numbers have already been updated for periodic cases.
        if (top .and. (.not. yperiodic)) then
            val = originalval
        else
            val = globalRowstart + localColnum
        endif

        if (f90ptr(i, j) .ne. val) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            !print *, "(bailing on first error - may be others)"
            !return 
        endif
      enddo
    enddo

    ! top right corner:
    do j=ub(2)-haloWidth+1, ub(2)
      localRownum = j - haloWidth 
      if (top .and. yperiodic) then
          localRownum = localRownum - globalCellCounts(2) 
      endif
      globalRowstart = ((localRownum + globalOffsets(2) - 1) &
                     *   globalCellCounts(1))  + globalOffsets(1)
      do i=ub(1)-haloWidth+1, ub(1)
        localColnum = i - haloWidth 
        if (right .and. xperiodic) then
            localColnum = localColnum - globalCellCounts(1)
        endif

        ! for those edges where halo should not have been updated, make sure
        ! the original value remained.  otherwise, compute the index value.
        ! the row and col numbers have already been updated for periodic cases.
        if ((right .and. top) .or. &
            (right .and. (.not. xperiodic)) .or. &
            (top .and. (.not. yperiodic))) then
            val = originalval
        else
            val = globalRowstart + localColnum
        endif

        if (f90ptr(i, j) .ne. val) then
            print *, "data mismatch at", i, j, f90ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
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

end module

