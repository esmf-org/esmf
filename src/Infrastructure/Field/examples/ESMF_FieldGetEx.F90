! $Id: ESMF_FieldGetEx.F90,v 1.3 2008/02/01 00:50:00 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
    program ESMF_FieldGetEx

!------------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
! !PROGRAM: ESMF_FieldGetEx - Field Get Examples
!
! !DESCRIPTION:
!
! This program shows examples of Field get data pointer methods
!-----------------------------------------------------------------------------

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_FieldGetMod
    implicit none
    
    ! Local variables
    integer :: rc

    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farray
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farray1
    type(ESMF_Field)  :: f8
    type(ESMF_Grid)   :: grid8
    type(ESMF_DistGrid) :: distgrid8
    type(ESMF_Array)  :: array8, array
    integer           :: xdim, ydim, zdim

    integer :: gridCompLBnd(ESMF_MAXDIM), gridCompUBnd(ESMF_MAXDIM)
    integer :: gridExclLBnd(ESMF_MAXDIM), gridExclUBnd(ESMF_MAXDIM)
    integer :: gridTotaLBnd(ESMF_MAXDIM), gridTotaUBnd(ESMF_MAXDIM)

    integer :: comp_count(ESMF_MAXDIM)
    integer :: excl_count(ESMF_MAXDIM)
    integer :: total_count(ESMF_MAXDIM)

    integer :: finalrc       
!   !Set finalrc to success
    finalrc = ESMF_SUCCESS

    call ESMF_Initialize(rc=rc)

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Get Fortran data pointer, Bounds, and Counts information from a Field}
!
!  User can get various bounds and counts information from a {\tt ESMF\_Field}
!  through the ESMF\_FieldGetDataPtr interface in addition to the intrinsic
!  Fortran data pointer contained in the internal {\tt ESMF\_Array} object
!  of a {\tt ESMF\_Field}
!
!EOE
!-------------------------------------------------------------------------
!   ! 
!   ! User can get various bounds and counts information from a Field
!   ! through the ESMF_FieldGetDataPtr interface.
    xdim = 12
    ydim = 22
    zdim = 31

    grid8 = ESMF_GridCreateShapeTile(minIndex=(/1,1,1/), maxIndex=(/4*xdim-1,ydim-1,zdim-1/), &
                              regDecomp=(/4,1,1/), name="grid", rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    allocate(farray(xdim,ydim,zdim))
    call ESMF_GridGet(grid8, distgrid=distgrid8, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    array8 = ESMF_ArrayCreate(farray, distgrid=distgrid8, &
        staggerloc=0, computationalEdgeLWidth=(/0,0,0/), computationalEdgeUWidth=(/-1,-1,-1/), rc=rc) 
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    f8 = ESMF_FieldCreate(grid8, array8, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
  
!BOC
    call ESMF_FieldGetDataPtr(f8, farray1, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldGetDataPtr (f8, localDE=0, &
        computationalLBound=gridCompLBnd, computationalUBound=gridCompUBnd, &
        exclusiveLBound=gridExclLBnd, exclusiveUBound=gridExclUBnd, &
        totalLBound=gridTotaLBnd, totalUBound=gridTotaUBnd, &
        computationalCount=comp_count, &
        exclusiveCount=excl_count, &
        totalCount=total_count, &
        rc=rc)   
!EOC

    call ESMF_FieldDestroy(f8, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_GridDestroy(grid8, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_ArrayDestroy(array8, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    deallocate(farray)
    print *, "Field Get Data Pointer example returned"
!-------------------------------------------------------------------------
    call ESMF_Finalize(rc=rc)
!-------------------------------------------------------------------------

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    if (finalrc.EQ.ESMF_SUCCESS) then
	print *, "PASS: ESMF_FieldGetEx.F90"
    else
	print *, "FAIL: ESMF_FieldGetEx.F90"
    end if
end program ESMF_FieldGetEx
