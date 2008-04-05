! $Id: ESMF_FieldGetEx.F90,v 1.4.2.5 2008/04/05 03:12:39 cdeluca Exp $
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
    program ESMF_FieldGetEx

!------------------------------------------------------------------------------
!ESMF_removeEXAMPLE        String used by test script to count examples.
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
    type(ESMF_Grid)   :: grid8, grid
    type(ESMF_DistGrid) :: distgrid8
    type(ESMF_Array)  :: array8, array
    integer           :: xdim, ydim, zdim

    integer :: gridCompLBnd(ESMF_MAXDIM), gridCompUBnd(ESMF_MAXDIM)
    integer :: gridExclLBnd(ESMF_MAXDIM), gridExclUBnd(ESMF_MAXDIM)
    integer :: gridTotaLBnd(ESMF_MAXDIM), gridTotaUBnd(ESMF_MAXDIM)

    integer :: comp_count(ESMF_MAXDIM)
    integer :: excl_count(ESMF_MAXDIM)
    integer :: total_count(ESMF_MAXDIM)

    type(ESMF_TypeKind)        :: typekind
    integer                    :: rank
    type(ESMF_StaggerLoc)      :: staggerloc 
    integer                    :: gridToFieldMap(ESMF_MAXDIM)    
    integer                    :: ungriddedLBound(ESMF_MAXDIM)
    integer                    :: ungriddedUBound(ESMF_MAXDIM)
    integer                    :: maxHaloLWidth(ESMF_MAXDIM)
    integer                    :: maxHaloUWidth(ESMF_MAXDIM)
    integer                    :: gcc(3), gec(3), fa_shape(3)
    character(len=32)          :: name

    integer :: finalrc       
!   !Set finalrc to success
    finalrc = ESMF_SUCCESS

    call ESMF_Initialize(rc=rc)

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BremoveOE
!\subsubsection{Get Fortran data pointer, Bounds, and Counts information from a Field}
!
!  User can get various bounds and counts information from a {\tt ESMF\_Field}
!  through the ESMF\_FieldGetDataPtr interface in addition to the intrinsic
!  Fortran data pointer contained in the internal {\tt ESMF\_Array} object
!  of a {\tt ESMF\_Field}. The bounds and counts information are DE specific
!  for the associated Fortran data pointer.
!
!EremoveOE
!-------------------------------------------------------------------------
!   ! 
!   ! User can get various bounds and counts information from a Field
!   ! through the ESMF_FieldGetDataPtr interface.
    xdim = 12
    ydim = 22
    zdim = 31

    grid8 = ESMF_GridCreateShapeTile(minIndex=(/1,1,1/), maxIndex=(/4*xdim,ydim,zdim/), &
                              regDecomp=(/4,1,1/), name="grid", rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridGet(grid8, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
        computationalCount=gcc, exclusiveCount=gec, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    do i = 1, 3
        fa_shape(i) = max(gcc(3), gec(3))
    enddo
    allocate(farray(fa_shape(1), fa_shape(2), fa_shape(3)) )

    call ESMF_GridGet(grid8, distgrid=distgrid8, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    array8 = ESMF_ArrayCreate(farray, distgrid=distgrid8, &
        staggerloc=0, computationalEdgeLWidth=(/0,0,0/), computationalEdgeUWidth=(/-1,-1,-1/), rc=rc) 
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    f8 = ESMF_FieldCreate(grid8, array8, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
  
!BremoveOC
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
!EremoveOC
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    print *, "Field Get Data Pointer example returned"

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BremoveOE
!\subsubsection{Get ESMF\_Grid and ESMF\_Array from a Field}
!
!  User can get the internal {\tt ESMF\_Grid} and {\tt ESMF\_Array} 
!  from a {\tt ESMF\_Field}. User should not issue any destroy command
!  on the retrieved grid or array object since they are referenced
!  from the {\tt ESMF\_Field}. The retrieved objects should be used
!  in a read-only fashion to query additional information not directly
!  available through FieldGet interface.
!
!EremoveOE

!BremoveOC
    call ESMF_FieldGet(f8, grid=grid, array=array, &
        typekind=typekind, rank=rank, staggerloc=staggerloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
        maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
        name=name, &
        rc=rc)
!EremoveOC
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    print *, "Field Get Grid and Array example returned"

    call ESMF_FieldDestroy(f8, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_GridDestroy(grid8, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_ArrayDestroy(array8, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    deallocate(farray)
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
