! $Id: ESMF_FieldCommEx.F90,v 1.21.2.3 2009/01/21 21:25:21 cdeluca Exp $
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
    program ESMF_FieldCommEx

!------------------------------------------------------------------------------
!ESMF_EXremoveAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_FieldCommEx - Field level communication routines
!
! !DESCRIPTION:
!
! Example/test code which does communication operations on Fields,
! including examples of using Redist, Halo, and Regrid on a Field.
! Also see the Programming Model section of this document.
!-----------------------------------------------------------------------------

#include "ESMF.h" 

    ! ESMF Framework module
    use ESMF_Mod

    implicit none
    
    ! Local variables
    integer :: rc, finalrc, npets
    integer :: i, j
    integer :: lb(2), ub(2), halo
    type(ESMF_IGrid) :: srcigrid, dstigrid
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_DELayout) :: layout1, layout2
    type(ESMF_VM) :: vm
    type(ESMF_RouteHandle) :: halo_rh, redist_rh, regrid_rh
    type(ESMF_Field) :: field1, field2
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr1
    real (ESMF_KIND_R8), dimension(2) :: mincoords, maxcoords
!EOC

    finalrc = ESMF_SUCCESS
        
!-------------------------------------------------------------------------
!   ! Setup:
!   !
!   !  Create a source and destination igrid with data on it, to use
!   !  in the Halo, Redist, and Regrid calls below.
!   !  Note Regrid call has been removed.  cmd 8/07.
 
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
                   name="dstigrid", rc=rc)
    call ESMF_IGridDistribute(dstigrid, delayout=layout2, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! allow for a halo width of 3
    halo = 3
    field1 = ESMF_FieldCreate(srcigrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                              haloWidth=halo, name="src pressure", rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

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
                              haloWidth=halo, name="dst pressure", rc=rc)

 
    ! fields all ready to go

!------------------------------------------------------------------------------
!BOE
!\subsubsection{Field Halo operation}
      
!  The user has already created an {\tt ESMF\_IGrid}, an
!  {\tt ESMF\_Array} with data, and used them to create an {\tt ESMF\_Field}.
!
!  Now that field is used in the store call which precomputes the
!  data movement which is required.  The {\tt ESMF_FieldHalo()} call
!  actually performs the operation.
!EOE


!BOC
    call ESMF_FieldHaloStore(field1, routehandle=halo_rh, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
    call ESMF_FieldHalo(field1, halo_rh, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
    call ESMF_FieldHaloRelease(halo_rh, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    print *, "Halo example 1 returned"


!------------------------------------------------------------------------------
!BOE
!\subsubsection{Field Data Redistribution operation}
      
!  The user has already created an {\tt ESMF\_IGrid}, an 
!  {\tt ESMF\_Array} with data, and used them to create an {\tt ESMF\_Field}.
!
!  The data redistribution operation does not do interpolation; it 
!  is used when the same igrid is decomposed differently - perhaps along
!  different indices, or on different numbers of PETs.
!
!  The store call will precompute the data movement necessary, and the
!  call to {\tt ESMF\_FieldRedist()} will perform the actual movement.
!EOE


!BOC
    call ESMF_FieldRedistStore(field1, field2, vm, &
                               routehandle=redist_rh, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
    call ESMF_FieldRedist(field1, field2, routehandle=redist_rh, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
    call ESMF_FieldRedistRelease(redist_rh, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE


    print *, "Redist example 2 returned"

!-------------------------------------------------------------------------
!BremoveOE
!\subsubsection{Field Regridding operation}
!      
!  The user has already created an {\tt ESMF\_IGrid}, an
!  {\tt ESMF\_Array} with data, and used them to create an {\tt ESMF\_Field}.
!
!  The regridding operation can interpolate data values to move data
!  from one igrid to another.
!
!  The store call will precompute the data movement necessary, and the
!  call to {\tt ESMF\_FieldRegrid()} will perform the actual movement.
!EremoveOE
!
!
!
!BremoveOC
!    call ESMF_FieldRegridStore(field1, field2, vm, &
!                               routehandle=regrid_rh, &
!                               regridmethod=ESMF_REGRID_METHOD_BILINEAR, rc=rc)
!EremoveOC
!    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!
!BremoveOC
!    call ESMF_FieldRegrid(field1, field2, regrid_rh, rc=rc)
!EremoveOC
!    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!
!BremoveOC
!    call ESMF_FieldRegridRelease(regrid_rh, rc=rc)
!EremoveOC
!    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!
!    print *, "Regrid example 3 returned"

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
       print *, "PASS: ESMF_FieldCommEx.F90"
    else
       print *, "FAIL: ESMF_FieldCommEx.F90"
    end if

    call ESMF_Finalize(rc=rc)

    end program ESMF_FieldCommEx
    
!\end{verbatim}
    
