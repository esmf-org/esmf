! $Id: ESMF_FieldCreateEx.F90,v 1.25 2004/05/25 11:22:18 nscollins Exp $
!
! Example/test code which creates a new field.

!-------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! See the following code fragments for examples of how to create new Fields.  
! Also see the Programming Model section of this document.
!
!EOP
!BOC
!   ! Example program showing various ways to create a Field object
    program ESMF_FieldCreateEx

    ! ESMF Framework module
    use ESMF_Mod
    implicit none
    
!   ! Local variables
    integer :: x, y, rc, mycell
    type(ESMF_Grid) :: grid
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array) :: arraya, arrayb
    type(ESMF_FieldDataMap) :: datamap
    type(ESMF_DELayout) :: layout
    type(ESMF_VM) :: vm
    type(ESMF_RelLoc) :: relativelocation
    character (len = ESMF_MAXSTR) :: fname
    type(ESMF_IOSpec) :: iospec
    type(ESMF_Field) :: field1, field2, field3
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr1, f90ptr2
    real (ESMF_KIND_R8), dimension(2) :: origin
!EOC
    integer :: finalrc       
!   !Set finalrc to success
    finalrc = ESMF_SUCCESS
!BOC
!-------------------------------------------------------------------------
    call ESMF_Initialize(rc=rc)
!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  The user has already created a Grid and has Field data
!   !  stored in an Array object.  This version of create simply
!   !  associates the data with the Grid.  The data is referenced
!   !  by default.  The DataMap is created with defaults.
 
    call ESMF_VMGetGlobal(vm, rc)
    layout = ESMF_DELayoutCreate(vm, rc=rc)
    origin = (/ 0.0, 0.0 /)
    grid = ESMF_GridCreateHorz_XYUni((/ 10, 20 /), origin, name="atmgrid", rc=rc)
    call ESMF_GridDistribute(grid, delayout=layout, rc=rc)

    allocate(f90ptr1(10,20))
    arraya = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_REF, rc=rc)  
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    call ESMF_ArrayPrint(arraya, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    field1 = ESMF_FieldCreate(grid, arraya, &
                         horzRelloc=ESMF_CELL_CENTER, name="pressure", rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    call ESMF_FieldGet(field1, name=fname, rc=rc)
    print *, "Field example 1 returned, name = ", trim(fname)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
!-------------------------------------------------------------------------
!   ! Example 2:
!   !
!   !  The user creates an ArraySpec that describes the data and the
!   !  Field create call allocates the appropriate memory for it. 

    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_DATA_REAL, ESMF_R4, rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    field2 = ESMF_FieldCreate(grid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                              name="rh", rc=rc)
    print *, "Field example 2 returned"
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
!-------------------------------------------------------------------------
!   ! Example 3:
!   !
!   !  The user wishes to associate different data with the Field
!   !  created in example 1.  The get data call returns the 
!   !  pointer to the old data array; the set call passes in the 
!   !  pointer to the new array.

    call ESMF_FieldGetArray(field1, array=arraya, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    allocate(f90ptr2(30,15))
    arrayb = ESMF_ArrayCreate(f90ptr2, ESMF_DATA_REF, rc=rc)  
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    call ESMF_FieldSetArray(field1, array=arrayb, rc=rc)
    print *, "Field example 3 returned"
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
!-------------------------------------------------------------------------
!   ! Example 4:
!   !
!   !  The user creates an empty Field, and adds the Grid and 
!   !  data in later calls.

     field3 = ESMF_FieldCreateNoData("precip", rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
!
!    ! At some later time, associate a Grid with this Field
     call ESMF_FieldSetGrid(field3, grid, rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
!    ! ...and associate a data Array.
!    call ESMF_FieldAttachArray(field3, arraya, rc=rc)
     print *, "Field example 4 returned"
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
!-------------------------------------------------------------------------
!   ! Example 5:
!   !
!   ! Query a Field for number of local grid cells.
     call ESMF_FieldGetLocalGridInfo(field3, ncell=mycell, rc=rc)
     print *, "Field example 5 returned"
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
     call ESMF_FieldDestroy(field1, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
     call ESMF_FieldDestroy(field2, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
     call ESMF_FieldDestroy(field3,rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
!-------------------------------------------------------------------------
     call ESMF_Finalize(rc)
!-------------------------------------------------------------------------
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    if (finalrc.EQ.ESMF_SUCCESS) then
	print *, "PASS: ESMF_FieldCreateEx.F90"
    else
	print *, "FAIL: ESMF_FieldCreateEx.F90"
    end if
!BOC
     end program ESMF_FieldCreateEx
!EOC
    
