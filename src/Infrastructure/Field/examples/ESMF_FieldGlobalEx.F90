! $Id: ESMF_FieldGlobalEx.F90,v 1.1 2004/07/27 23:03:21 jwolfe Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
    program ESMF_FieldGlobalEx

!------------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_FieldGlobalEx - Field creation using global indexing
!
! !DESCRIPTION:
!
! This program shows an example of Field creation using global indexing.
! At some point global indexing may be an option for FieldCreate routines,
! but in the meantime this is an example of one way to accomplish it.
!-----------------------------------------------------------------------------

    ! ESMF Framework module
    use ESMF_Mod
    implicit none
    
    ! Local variables
    integer :: rc, haloWidth, finalrc, myDE
    integer :: localCount(2), lbounds(2), ubounds(2)
    character (len = ESMF_MAXSTR) :: fname
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr1
    real (ESMF_KIND_R8), dimension(2) :: origin
    type(ESMF_Grid) :: grid
    type(ESMF_Array) :: array1
    type(ESMF_DELayout) :: layout
    type(ESMF_VM) :: vm
    type(ESMF_Field) :: field1
    type(ESMF_RouteHandle) :: routehandle
!EOC

!   !Set finalrc to success
    finalrc = ESMF_SUCCESS

    call ESMF_Initialize(rc=rc)

!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  The user has already created a Grid and has Field data
!   !  stored in an Array object.  This version of create simply
!   !  associates the data with the Grid.  The data is referenced
!   !  by default.  The DataMap is created with defaults.
 
    call ESMF_VMGetGlobal(vm, rc)
    layout = ESMF_DELayoutCreate(vm, (/ 3, 2 /), rc=rc)

    origin = (/ 0.0, 0.0 /)
    grid = ESMF_GridCreateHorzXYUni((/ 50, 30 /), origin, &
                                    deltaPerDim=(/ 1.0d0, 1.0d0 /), &
                                    name="atmgrid", rc=rc)
    call ESMF_GridDistribute(grid, delayout=layout, rc=rc)

    ! get grid information used to calculate global indices
    call ESMF_GridGetDELocalInfo(grid, horzrelloc=ESMF_CELL_CENTER, &
                                 myDE=myDE, localCellCountPerDim=localCount, &
                                 globalStartPerDim=lbounds, rc=rc)

    write(*,*) 'lbounds1',lbounds
    write(*,*) 'ubounds1',ubounds

    ! globalStartPerDim really should be globalOffsetPerDim, so the lower
    ! bounds should be the offset plus one
    lbounds(1) = lbounds(1) + 1
    lbounds(2) = lbounds(2) + 1

    ! calculate upper bounds from lower bounds and counts
    ubounds(1) = lbounds(1) + localCount(1) - 1
    ubounds(2) = lbounds(2) + localCount(2) - 1

    write(*,*) 'lbounds2',lbounds
    write(*,*) 'ubounds2',ubounds

    ! set the haloWidth of the Array
    haloWidth = 3

    ! modify the lower and upper bounds by the haloWidth
    lbounds(1) = lbounds(1) - haloWidth
    lbounds(2) = lbounds(2) - haloWidth
    ubounds(1) = ubounds(1) + haloWidth
    ubounds(2) = ubounds(2) + haloWidth

    write(*,*) 'lbounds3',lbounds
    write(*,*) 'ubounds3',ubounds

    allocate(f90ptr1(lbounds(1):ubounds(1),lbounds(2):ubounds(2)))
    f90ptr1 = myDE

    array1 = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_REF, haloWidth=haloWidth, rc=rc)  
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_ArrayPrint(array1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Field Create with Grid and Array}
      
!  The user has already created an {\tt ESMF\_Grid} and an
!  {\tt ESMF\_Array} with data.  This create associates the
!  two objects.  An {\tt ESMF\_FieldDataMap} is created with all defaults.
!EOE
      
!BOC
    field1 = ESMF_FieldCreate(grid, array1, &
                         horzRelloc=ESMF_CELL_CENTER, name="pressure", rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldHaloStore(field1, routehandle, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldHalo(field1, routehandle, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldPrint(field1, "", rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldDestroy(field1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
     call ESMF_Finalize(rc)
!-------------------------------------------------------------------------

    if (finalrc.EQ.ESMF_SUCCESS) then
	print *, "PASS: ESMF_FieldGlobalEx.F90"
    else
	print *, "FAIL: ESMF_FieldGlobalEx.F90"
    end if
!BOC
     end program ESMF_FieldGlobalEx
!EOC
    
