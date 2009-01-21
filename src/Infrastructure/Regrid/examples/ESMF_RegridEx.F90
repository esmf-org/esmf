! $Id: ESMF_RegridEx.F90,v 1.15.2.3 2009/01/21 21:25:23 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
    program ESMF_RegridEx

!------------------------------------------------------------------------------
!ESMF_EXremoveAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
!
! !PROGRAM: ESMF_RegridEx - Using the Regridding methods
!
! !DESCRIPTION:
!
! This program shows examples of using Regrid on Field data
!-----------------------------------------------------------------------------

    ! ESMF Framework module
    use ESMF_Mod

    implicit none
    
    ! Local variables to be used in the Regrid method calls.
    ! The code creating and filling these variables is not included in the
    ! example documentation because those interfaces are not specific to
    ! Regrid.
    type(ESMF_Field) :: field1, field2
    type(ESMF_IGrid) :: srcigrid, dstigrid
    type(ESMF_RouteHandle) :: regrid_rh
    type(ESMF_DELayout) :: layout1, layout2
    integer :: rc

!EOC

    integer :: finalrc, npets
    integer :: i, j, lb(2), ub(2), halo
    type(ESMF_ArraySpec) :: arrayspec
    !type(ESMF_FieldDataMap) :: datamap
    type(ESMF_VM) :: vm
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr1
    real (ESMF_KIND_R8), dimension(2) :: mincoords, maxcoords

    finalrc = ESMF_SUCCESS
        
!-------------------------------------------------------------------------
!   ! Setup:
!   !
!   !  Create a source and destination igrid with data on it, to use
!   !  in the Regrid calls below.
 
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

    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    
    ! allow for a halo width of 3, let field create data space
    halo = 3
    field1 = ESMF_FieldCreate(srcigrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                haloWidth=3, name="src pressure", rc=rc)
                                
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    
    ! get a fortran pointer to the data spacd
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
                                                   name="dst pressure", rc=rc)

 
    ! fields all ready to go


!BOE
      
!  The user has already created an {\tt ESMF\_IGrid}, an
!  {\tt ESMF\_Array} with data, and put them together in an {\tt ESMF\_Field}.
!  An {\tt ESMF\_RouteHandle} is created by the regrid store call 
!  and the data movement needed to
!  execute the regrid is stored with that handle by the store method. 
!  To actually execute the operation, the source and destination data
!  objects must be supplied, along with the same {\tt ESMF\_RouteHandle}.
!EOE
      

!BOC
    call ESMF_FieldRegridStore(field1, field2, vm, &
                               routehandle=regrid_rh, &
                               regridmethod=ESMF_REGRID_METHOD_BILINEAR, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
    call ESMF_FieldRegrid(field1, field2, regrid_rh, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
    call ESMF_FieldRegridRelease(regrid_rh, rc=rc)
!EOC
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
       print *, "PASS: ESMF_RegridEx.F90"
    else
       print *, "FAIL: ESMF_RegridEx.F90"
    end if

    call ESMF_Finalize(rc=rc)

    end program ESMF_RegridEx

    
    
