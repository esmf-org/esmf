! $Id: ESMF_FieldCreateEx.F90,v 1.3 2003/05/27 23:01:33 jwolfe Exp $
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
    call ESMF_ArrayPrint(arraya, rc=rc)

    field1 = ESMF_FieldCreate(grid, arraya, &
                         relloc=ESMF_CELL_CENTER, name="pressure", rc=rc)

    call ESMF_FieldGetName(field1, fname, rc)

    print *, "Field example 1 returned, name = ", trim(fname)

!-------------------------------------------------------------------------
!   ! Example 2:
!   !
!   !  The user creates an ArraySpec that describes the data and the
!   !  Field create call allocates the appropriate memory for it. 

!   !   arrayspec = ESMF_ArraySpecCreate()

    field2 = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_CENTER, &
                              name="rh", rc=rc)

    print *, "Field example 2 returned"

!-------------------------------------------------------------------------
!   ! Example 3:
!   !
!   !  The user wishes to associate different data with the Field
!   !  created in example 1.  The detach data call returns the 
!   !  pointer to the old data array; the attach call passes in the 
!   !  pointer to the new array.

    call ESMF_FieldDetachData(field1, array=arraya, rc=rc)

    allocate(f90ptr2(30,15))
    arrayb = ESMF_ArrayCreate(f90ptr2, ESMF_DATA_REF, rc=rc)  

    call ESMF_FieldAttachData(field1, array=arrayb, rc=rc)

    print *, "Field example 3 returned"

!-------------------------------------------------------------------------
!   ! Example 4:
!   !
!   !  The user creates an empty Field, and adds the Grid and 
!   !  data in later calls.

     field3 = ESMF_FieldCreateNoData("precip", rc=rc)
!
!    ! At some later time, associate a Grid with this Field
     call ESMF_FieldSetGrid(field3, grid, rc)

!    ! ...and associate a data Array.
!    call ESMF_FieldAttachArray(field3, arraya, rc=rc)

     print *, "Field example 4 returned"

!-------------------------------------------------------------------------
!   ! Example 5:
!   !
!   ! Query a Field for number of local grid cells.

     call ESMF_FieldGetLocalGridInfo(field3, ncell=mycell, rc=rc)

     print *, "Field example 5 returned"

     call ESMF_FieldDestroy(field1)
     call ESMF_FieldDestroy(field2)
     call ESMF_FieldDestroy(field3)
     !call ESMF_FieldDestroy(field4)

     end program ESMF_FieldCreateEx
    
!\end{verbatim}
    
