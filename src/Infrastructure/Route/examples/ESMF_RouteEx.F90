! $Id: ESMF_RouteEx.F90,v 1.4 2003/12/02 18:12:04 svasquez Exp $
!
! Example/test code which creates a new field.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! See the following code fragments for examples of how to call Route on 
! a Field.
! Also see the Programming Model section of this document.
!
!
!\begin{verbatim}

!   ! Example program showing various ways to create a Field object
    program ESMF_RouteUseEx

    ! ESMF Framework module
    use ESMF_Mod

    implicit none
    
!   ! Local variables
    integer :: x, y, rc, mycell, finalrc
    type(ESMF_Grid) :: grid
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array) :: arraya, arrayb
    type(ESMF_DataMap) :: datamap
    type(ESMF_RelLoc) :: relativelocation
    character (len = ESMF_MAXSTR) :: fname
    type(ESMF_IOSpec) :: iospec
    type(ESMF_Field) :: field1, field2, field3, field4
    real (selected_real_kind(6,45)), dimension(:,:), pointer :: f90ptr1, f90ptr2
    finalrc = ESMF_SUCCESS
        
!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  The user has already created a Grid and has Field data
!   !  stored in an Array object.  This version of create simply
!   !  associates the data with the Grid.  The data is referenced
!   !  by default.  The DataMap is created with defaults.
 
    grid = ESMF_GridCreate(name="atmgrid", rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    allocate(f90ptr1(10,20))
    arraya = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_REF, rc=rc)  

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    call ESMF_ArrayPrint(arraya, rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    field1 = ESMF_FieldCreate(grid, arraya, &
                         relloc=ESMF_CELL_CENTER, name="pressure", rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    call ESMF_FieldGetName(field1, fname, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    print *, "Field example 1 returned, name = ", trim(fname)

!-------------------------------------------------------------------------
!   ! Example 2:
!   !
!   !  The user creates an ArraySpec that describes the data and the
!   !  Field create call allocates the appropriate memory for it. 

!   !   arrayspec = ESMF_ArraySpecCreate()

    field2 = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_CENTER, &
                              name="rh", rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    print *, "Field example 2 returned"

!-------------------------------------------------------------------------
!   ! Example 3:
!   !
!   !  The user wishes to associate different data with the Field
!   !  created in example 1.  The detach data call returns the 
!   !  pointer to the old data array; the attach call passes in the 
!   !  pointer to the new array.

    call ESMF_FieldDetachData(field1, array=arraya, rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    allocate(f90ptr2(30,15))
    arrayb = ESMF_ArrayCreate(f90ptr2, ESMF_DATA_REF, rc=rc)  

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    call ESMF_FieldAttachData(field1, array=arrayb, rc=rc)

    print *, "Field example 3 returned"

!-------------------------------------------------------------------------
!   ! Example 4:
!   !
!   !  The user creates an empty Field, and adds the Grid and 
!   !  data in later calls.

     field3 = ESMF_FieldCreateNoData("precip", rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if
!
!    ! At some later time, associate a Grid with this Field
     call ESMF_FieldSetGrid(field3, grid, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!    ! ...and associate a data Array.
!    call ESMF_FieldAttachArray(field3, arraya, rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

     print *, "Field example 4 returned"

!-------------------------------------------------------------------------
!   ! Example 5:
!   !
!   ! Query a Field for number of local grid cells.

     call ESMF_FieldGetLocalGridInfo(field3, ncell=mycell, rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

     print *, "Field example 5 returned"

     call ESMF_FieldDestroy(field1, rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

     call ESMF_FieldDestroy(field2, rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

     call ESMF_FieldDestroy(field3, rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

     !call ESMF_FieldDestroy(field4, rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    if (finalrc.EQ.ESMF_SUCCESS) then
       print *, "PASS: ESMF_RouteUseEx.F90"
    else
       print *, "FAIL: ESMF_RouteUseEx.F90"
    end if


     end program ESMF_RouteUseEx
    
!\end{verbatim}
    
