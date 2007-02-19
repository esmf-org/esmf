! $Id: ESMF_FieldCreateEx.F90,v 1.38 2007/02/19 23:44:41 rosalind Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
    program ESMF_FieldCreateEx

!------------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_FieldCreateEx - Field creation
!
! !DESCRIPTION:
!
! This program shows examples of Field initialization and manipulation
!-----------------------------------------------------------------------------

    ! ESMF Framework module
    use ESMF_Mod
    implicit none
    
    ! Local variables
    integer :: rc, mycell
    integer :: gridCount(2)
    type(ESMF_Grid) :: grid
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_InternArray) :: iarray1, iarray2
    type(ESMF_DELayout) :: layout
    type(ESMF_VM) :: vm
    !type(ESMF_RelLoc) :: relativelocation
    !type(ESMF_FieldDataMap) :: datamap
    type(ESMF_Field) :: field1, field2, field3
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr1, f90ptr2
    real (ESMF_KIND_R8), dimension(2) :: origin
    character (len = ESMF_MAXSTR) :: fname
!EOC
    integer :: finalrc       
!   !Set finalrc to success
    finalrc = ESMF_SUCCESS

    call ESMF_Initialize(rc=rc)

!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  The user has already created a Grid and has Field data
!   !  stored in an InternArray object.  This version of create simply
!   !  associates the data with the Grid.  The data is referenced
!   !  by default.  The DataMap is created with defaults.
 
    call ESMF_VMGetGlobal(vm, rc)
    layout = ESMF_DELayoutCreate(vm, rc=rc)
    origin = (/ 0.0, 0.0 /)
    grid = ESMF_GridCreateHorzXYUni((/ 10, 20 /), origin, &
                                    deltaPerDim=(/ 1.0d0, 1.0d0 /), &
                                    name="atmgrid", rc=rc)
    call ESMF_GridDistribute(grid, delayout=layout, rc=rc)
    ! InternArray size has to match the Grid (plus any halo width), so query grid for
    ! local cell counts to determine allocation
    call ESMF_GridGetDELocalInfo(grid, horzrelloc=ESMF_CELL_CENTER, &
                                 localCellCountPerDim=gridCount, rc=rc)

    allocate(f90ptr1(gridCount(1),gridCount(2)))

    iarray1 = ESMF_InternArrayCreate(f90ptr1, ESMF_DATA_REF, rc=rc)  
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_InternArrayPrint(iarray1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Field Create with Grid and InternArray}
      
!  The user has already created an {\tt ESMF\_Grid} and an
!  {\tt ESMF\_InterArray} with data.  This create associates the
!  two objects.  An {\tt ESMF\_FieldDataMap} is created with all defaults.
!EOE
      
!BOC
    field1 = ESMF_FieldCreate(grid, iarray1, &
                         horzRelloc=ESMF_CELL_CENTER, name="pressure", rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldGet(field1, name=fname, rc=rc)
    print *, "Field example 1 returned, name = ", trim(fname)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Field Create with Grid and ArraySpec}
      
!  The user has already created an {\tt ESMF\_Grid} and an
!  {\tt ESMF\_ArraySpec} which describes the data.  This version of 
!  create will create an {\tt ESMF\_Array} based on the grid size
!  and the {\tt ESMF\_ArraySpec}. 
!  An {\tt ESMF\_FieldDataMap} is created with all defaults.
!EOE
      

!-------------------------------------------------------------------------
!   ! Example 2:
!   !
!   !  The user creates an ArraySpec that describes the data and the
!   !  Field create call allocates the appropriate memory for it. 

!BOC
    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_DATA_REAL, ESMF_TYPEKIND_R4, rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    field2 = ESMF_FieldCreate(grid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                              name="rh", rc=rc)
!EOC
    print *, "Field example 2 returned"
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
!   ! Example 3:
!   !
!   !  The user wishes to associate different data with the Field
!   !  created in example 1.  The get data call returns the 
!   !  pointer to the old data array; the set call passes in the 
!   !  pointer to the new array.

    call ESMF_FieldGetInternArray(field1, array=iarray1, rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! the size of the data in the array still has to line up with the Grid
    ! and its decomposition
    allocate(f90ptr2(gridCount(1),gridCount(2)))
    iarray2 = ESMF_InternArrayCreate(f90ptr2, ESMF_DATA_REF, rc=rc)  

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldSetInternArray(field1, array=iarray2, rc=rc)
    print *, "Field example 3 returned"

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE


!BOE
!\subsubsection{Empty Field Create}

!  The user creates an empty {\tt ESMF\_Field} object.
!  The {\tt ESMF\_Grid}, {\tt ESMF\_InternArray}, and {\tt ESMF\_FieldDataMap}
!  can be added later using the set methods.
!EOE

!-------------------------------------------------------------------------
!   ! Example 4:
!   !
!   !  The user creates an empty Field, and adds the Grid and 
!   !  data in later calls.

!BOC
     field3 = ESMF_FieldCreateNoData("precip", rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!
!    ! At some later time, associate a Grid with this Field
     call ESMF_FieldSetGrid(field3, grid, rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!    ! ...and associate a data Array.
!    call ESMF_FieldAttachArray(field3, array1, rc=rc)
     print *, "Field example 4 returned"

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
!   ! Example 5:
!   !
!   ! Query a Field for number of local grid cells.
!   COMMENT THIS TEST OUT FOR NOW BECAUSE THE SUBROUTINE
!   IS UNIMPLEMENTED CAN TURN BACK ON WHEN INITMACROS ARE ON
!     call ESMF_FieldGetLocalGridInfo(field3, ncell=mycell, rc=rc)
!     print *, "Field example 5 returned"
!
!    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Destroy a Field}

!  When finished with an {\tt ESMF\_Field}, the destroy method
!  removes it.  However, the objects inside the {\tt ESMF\_Field}
!  should be deleted separately, since objects can be added to
!  more than one {\tt ESMF\_Field}, for example the same {\tt ESMF\_Grid}
!  can be used in multiple {\tt ESMF\_Field}s.
!EOE

!BOC
    call ESMF_FieldDestroy(field1, rc=rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldDestroy(field2, rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldDestroy(field3,rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
     call ESMF_Finalize(rc=rc)
!-------------------------------------------------------------------------

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    if (finalrc.EQ.ESMF_SUCCESS) then
	print *, "PASS: ESMF_FieldCreateEx.F90"
    else
	print *, "FAIL: ESMF_FieldCreateEx.F90"
    end if
!BOC
     end program ESMF_FieldCreateEx
!EOC
    
