! $Id: ESMF_FieldCreateEx.F90,v 1.8 2003/12/17 20:55:34 svasquez Exp $
!
! Example/test code which creates a new field.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! See the following code fragments for examples of how to create new Fields.  
! Also see the Programming Model section of this document.
!
!
!\begin{verbatim}

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
    type(ESMF_DataMap) :: datamap
    type(ESMF_RelLoc) :: relativelocation
    character (len = ESMF_MAXSTR) :: fname
    type(ESMF_IOSpec) :: iospec
    type(ESMF_Field) :: field1, field2, field3, field4
    real (selected_real_kind(6,45)), dimension(:,:), pointer :: f90ptr1, f90ptr2
!\end{verbatim}
!EOP

    integer :: x, y, rc, mycell, finalrc       
!   !Set finalrc to success
    finalrc = ESMF_SUCCESS

!BOP
!\begin{verbatim}
!-------------------------------------------------------------------------

    call ESMF_Initialize(rc)

!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  The user has already created a Grid and has Field data
!   !  stored in an Array object.  This version of create simply
!   !  associates the data with the Grid.  The data is referenced
!   !  by default.  The DataMap is created with defaults.
 
    grid = ESMF_GridCreate(name="atmgrid", rc=rc)

    allocate(f90ptr1(10,20))
    arraya = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_REF, rc=rc)  
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
	finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    call ESMF_ArrayPrint(arraya, rc=rc)
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
	finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    field1 = ESMF_FieldCreate(grid, arraya, &
                         relloc=ESMF_CELL_CENTER, name="pressure", rc=rc)
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
	finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    call ESMF_FieldGetName(field1, fname, rc)

    print *, "Field example 1 returned, name = ", trim(fname)
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
	finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
!-------------------------------------------------------------------------
!   ! Example 2:
!   !
!   !  The user creates an ArraySpec that describes the data and the
!   !  Field create call allocates the appropriate memory for it. 

    call ESMF_ArraySpecInit(arrayspec, 2, ESMF_DATA_REAL, ESMF_R4, rc)
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
	finalrc = ESMF_FAILURE
    end if
!BOP
!\begin{verbatim}
    field2 = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_CENTER, &
                              name="rh", rc=rc)

    print *, "Field example 2 returned"
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
	finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
!-------------------------------------------------------------------------
!   ! Example 3:
!   !
!   !  The user wishes to associate different data with the Field
!   !  created in example 1.  The detach data call returns the 
!   !  pointer to the old data array; the attach call passes in the 
!   !  pointer to the new array.

    call ESMF_FieldDetachData(field1, array=arraya, rc=rc)
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
	finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    allocate(f90ptr2(30,15))
    arrayb = ESMF_ArrayCreate(f90ptr2, ESMF_DATA_REF, rc=rc)  
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
	finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    call ESMF_FieldAttachData(field1, array=arrayb, rc=rc)

    print *, "Field example 3 returned"
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
	finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
!-------------------------------------------------------------------------
!   ! Example 4:
!   !
!   !  The user creates an empty Field, and adds the Grid and 
!   !  data in later calls.

     field3 = ESMF_FieldCreateNoData("precip", rc=rc)
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
	finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
!
!    ! At some later time, associate a Grid with this Field
     call ESMF_FieldSetGrid(field3, grid, rc)
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
	finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
!    ! ...and associate a data Array.
!    call ESMF_FieldAttachArray(field3, arraya, rc=rc)

     print *, "Field example 4 returned"
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
	finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
!-------------------------------------------------------------------------
!   ! Example 5:
!   !
!   ! Query a Field for number of local grid cells.

     call ESMF_FieldGetLocalGridInfo(field3, ncell=mycell, rc=rc)

     print *, "Field example 5 returned"
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
	finalrc = ESMF_FAILURE
    end if


!BOP
!\begin{verbatim}
     call ESMF_FieldDestroy(field1, rc=rc)
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
     call ESMF_FieldDestroy(field2, rc=rc)
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
     call ESMF_FieldDestroy(field3,rc=rc)
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
     !call ESMF_FieldDestroy(field4,rc=rc)
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
!-------------------------------------------------------------------------

     call ESMF_Finalize(rc)

!-------------------------------------------------------------------------
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if


    if (finalrc.EQ.ESMF_SUCCESS) then
	print *, "PASS: ESMF_FieldCreateEx.F90"
    else
	print *, "FAIL: ESMF_FieldCreateEx.F90"
    end if

!BOP
!\begin{verbatim}
     end program ESMF_FieldCreateEx
!\end{verbatim}
!EOP
    
