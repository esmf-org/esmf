! $Id: ESMF_RouteEx.F90,v 1.31 2007/06/23 04:00:42 cdeluca Exp $
!
! Example/test code which creates a new field.

!-------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! See the following code fragments for examples of how to call Redist,
!  Halo, and Regrid on a Field.
! Also see the Programming Model section of this document.
!
!
!\begin{verbatim}

!   ! Example program showing various ways to call the communications
!   ! routines on a Field object
    program ESMF_RouteUseEx

    ! ESMF Framework module
    use ESMF_Mod

    implicit none
    
!   ! Local variables
    integer :: rc, finalrc, npets
    integer :: i, j
    integer :: lb(2), ub(2), halo
    type(ESMF_IGrid) :: srcigrid, dstigrid
    type(ESMF_ArraySpec) :: arrayspec
    !type(ESMF_FieldDataMap) :: datamap
    type(ESMF_DELayout) :: layout1, layout2
    type(ESMF_VM) :: vm
    type(ESMF_RouteHandle) :: halo_rh, redist_rh, regrid_rh
    type(ESMF_Field) :: field1, field2
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr1
    real (ESMF_KIND_R8), dimension(2) :: mincoords, maxcoords

    finalrc = ESMF_SUCCESS
        
!-------------------------------------------------------------------------
!   ! Setup:
!   !
!   !  Create a source and destination igrid with data on it, to use
!   !  in the Halo, Redist, and Regrid calls below.
 
    call ESMF_Initialize(rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_VMGetGlobal(vm, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! Get number of PETs we are running with
    call ESMF_VMGet(vm, petCount=npets, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    layout1 = ESMF_DELayoutCreate(vm, (/ 1, npets /), rc=rc)
    layout2 = ESMF_DELayoutCreate(vm, (/ npets, 1 /), rc=rc)

    mincoords = (/  0.0,  0.0 /)
    maxcoords = (/ 20.0, 30.0 /)
    srcigrid = ESMF_IGridCreateHorzXYUni((/ 90, 180 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_IGRID_HORZ_STAGGER_A, &
                   name="srcigrid", rc=rc)
    call ESMF_IGridDistribute(srcigrid, delayout=layout1, rc=rc)

    ! same igrid coordinates, but different layout
    dstigrid = ESMF_IGridCreateHorzXYUni((/ 90, 180 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_IGRID_HORZ_STAGGER_A, &
                   name="srcigrid", rc=rc)
    call ESMF_IGridDistribute(dstigrid, delayout=layout2, rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! Real*8, 2D data
    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)
    
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    
    ! allow for a halo width of 3, let the field allocate the proper space
    halo = 3
    field1 = ESMF_FieldCreate(srcigrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                haloWidth=3, name="src pressure", rc=rc)
                                
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    
    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field1, f90ptr1, ESMF_DATA_REF, rc=rc)
    
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    
    lb(:) = lbound(f90ptr1)
    ub(:) = ubound(f90ptr1)
    
    f90ptr1(:,:) = 0.0
    do j=lb(2)+halo, ub(2)-halo
      do i=lb(1)+halo, ub(1)-halo
        f90ptr1(i, j) = i*1000 + j
      enddo
    enddo


    field2 = ESMF_FieldCreate(dstigrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                    haloWidth=3,  name="dst pressure", rc=rc)

 
    ! fields all ready to go

!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  Calling Field Halo



    call ESMF_FieldHaloStore(field1, routehandle=halo_rh, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldHalo(field1, halo_rh, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldHaloRelease(halo_rh, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE


    print *, "Halo example 1 returned"

!-------------------------------------------------------------------------
!   ! Example 2:
!   !
!   !  Calling Field Redist



    call ESMF_FieldRedistStore(field1, field2, vm, &
                                                routehandle=redist_rh, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldRedist(field1, field2, routehandle=redist_rh, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldRedistRelease(redist_rh, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE


    print *, "Redist example 2 returned"

!-------------------------------------------------------------------------
!   ! Example 3:
!   !
!   !  Calling Field Regrid



    call ESMF_FieldRegridStore(field1, field2, vm, &
                               routehandle=regrid_rh, &
                               regridmethod=ESMF_REGRID_METHOD_BILINEAR, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldRegrid(field1, field2, regrid_rh, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldRegridRelease(regrid_rh, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE


    print *, "Regrid example 3 returned"

!-------------------------------------------------------------------------
!    ! Cleanup

    call ESMF_FieldDestroy(field1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldDestroy(field2, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_IGridDestroy(srcigrid, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_IGridDestroy(dstigrid, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE


    if (finalrc.EQ.ESMF_SUCCESS) then
       print *, "PASS: ESMF_RouteEx.F90"
    else
       print *, "FAIL: ESMF_RouteEx.F90"
    end if

    call ESMF_Finalize(rc=rc)

    end program ESMF_RouteUseEx
    
!\end{verbatim}
    
