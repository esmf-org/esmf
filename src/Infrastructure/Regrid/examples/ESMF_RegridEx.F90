! $Id: ESMF_RegridEx.F90,v 1.1 2004/06/18 10:54:13 nscollins Exp $
!
! Example/test code which regrids data.

!-------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! See the following code fragments for examples of how to call
!   Regrid on a Field.
! Also see the Programming Model section of this document.
!
!
!\begin{verbatim}

!   ! Example program showing various ways to call the communications
!   ! routines on a Field object
    program ESMF_RegridUseEx

    ! ESMF Framework module
    use ESMF_Mod

    implicit none
    
!   ! Local variables
    integer :: x, y, rc, mycell, finalrc, numdes
    integer :: i, j
    type(ESMF_Grid) :: srcgrid, dstgrid
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array) :: arraya, arrayb
    type(ESMF_FieldDataMap) :: datamap
    type(ESMF_DELayout) :: layout1, layout2
    type(ESMF_VM) :: vm
    type(ESMF_RouteHandle) :: halo_rh, redist_rh, regrid_rh
    character (len = ESMF_MAXSTR) :: fname
    type(ESMF_IOSpec) :: iospec
    type(ESMF_Field) :: field1, field2, field3, field4
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr1, f90ptr2
    real (ESMF_KIND_R8), dimension(2) :: mincoords, maxcoords

    finalrc = ESMF_SUCCESS
        
!-------------------------------------------------------------------------
!   ! Setup:
!   !
!   !  Create a source and destination grid with data on it, to use
!   !  in the Regrid calls below.
 
    call ESMF_Initialize(rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_VMGetGlobal(vm, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    layout1 = ESMF_DELayoutCreate(vm, rc=rc)
    call ESMF_DELayoutGet(layout1, deCount=numdes, rc=rc)
    layout2 = ESMF_DELayoutCreate(vm, (/ numdes, 1 /), rc=rc)

    mincoords = (/  0.0,  0.0 /)
    mincoords = (/ 20.0, 30.0 /)
    srcgrid = ESMF_GridCreateHorzXYUni((/ 90, 180 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_GRID_HORZ_STAGGER_A, &
                   name="srcgrid", rc=rc)
    call ESMF_GridDistribute(srcgrid, delayout=layout1, rc=rc)

    ! same grid coordinates, but different layout
    dstgrid = ESMF_GridCreateHorzXYUni((/ 90, 180 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_GRID_HORZ_STAGGER_A, &
                   name="srcgrid", rc=rc)
    call ESMF_GridDistribute(dstgrid, delayout=layout2, rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! allow for a halo width of 3
    allocate(f90ptr1(96,186))
    do j=1, 180
      do i=1, 90
        f90ptr1(i+3, j+3) = i*1000 + j
      enddo
    enddo

    arraya = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_REF, rc=rc)  

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    field1 = ESMF_FieldCreate(srcgrid, arraya, horzRelloc=ESMF_CELL_CENTER, &
                                haloWidth=3, name="src pressure", rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_DATA_REAL, ESMF_R8, rc)

    field2 = ESMF_FieldCreate(dstgrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                                   name="dst pressure", rc=rc)

 
    ! fields all ready to go


!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  Calling Field Regrid


    regrid_rh = ESMF_RouteHandleCreate(rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldRegridStore(field1, field2, layout1, &
                               routehandle=regrid_rh, &
                               regridmethod=ESMF_REGRID_METHOD_BILINEAR, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldRegrid(field1, field2, regrid_rh, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldRegridRelease(regrid_rh, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_RouteHandleDestroy(redist_rh)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    print *, "Regrid example 3 returned"

!-------------------------------------------------------------------------
!    ! Cleanup

    call ESMF_FieldDestroy(field1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldDestroy(field2, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridDestroy(srcgrid, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridDestroy(dstgrid, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE


    if (finalrc.EQ.ESMF_SUCCESS) then
       print *, "PASS: ESMF_RegridEx.F90"
    else
       print *, "FAIL: ESMF_RegridEx.F90"
    end if

    call ESMF_Finalize(rc=rc)

    end program ESMF_RegridUseEx
    
!\end{verbatim}
    
