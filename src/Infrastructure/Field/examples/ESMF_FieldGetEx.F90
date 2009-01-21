! $Id: ESMF_FieldGetEx.F90,v 1.4.2.10 2009/01/21 21:25:20 cdeluca Exp $
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
    program ESMF_FieldGetEx

!------------------------------------------------------------------------------
!EremoveSMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
! !PROGRAM: ESMF_FieldGetEx - Field Get Examples
!
! !DESCRIPTION:
!
! This program shows examples of Field get data pointer methods
!-----------------------------------------------------------------------------

    ! ESMF Framework module
    use ESMF_Mod
    implicit none
    
    ! Local variables
    integer :: rc

    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farray
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farray1
    type(ESMF_Field)  :: field
    type(ESMF_Grid)   :: grid3d, grid
    type(ESMF_DistGrid) :: distgrid3d
    type(ESMF_Array)  :: array3d, array
    integer           :: xdim, ydim, zdim

    integer :: compLBnd(1:3), compUBnd(1:3)
    integer :: exclLBnd(1:3), exclUBnd(1:3)
    integer :: totalLBnd(1:3), totalUBnd(1:3)

    integer :: comp_count(1:3)
    integer :: excl_count(1:3)
    integer :: total_count(1:3)

    type(ESMF_TypeKind)        :: typekind
    integer                    :: rank
    type(ESMF_StaggerLoc)      :: staggerloc 
    integer                    :: gridToFieldMap(ESMF_MAXDIM)    
    integer                    :: ungriddedLBound(ESMF_MAXDIM)
    integer                    :: ungriddedUBound(ESMF_MAXDIM)
    integer                    :: maxHaloLWidth(ESMF_MAXDIM)
    integer                    :: maxHaloUWidth(ESMF_MAXDIM)
    integer                    :: fa_shape(3)
    character(len=32)          :: name

    real(4) :: PI=3.14159265
    integer :: finalrc, i, j, k
!   !Set finalrc to success
    finalrc = ESMF_SUCCESS

    call ESMF_Initialize(rc=rc)

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Get Fortran data pointer, Bounds, and Counts information from a Field}
!\label{sec:field:usage:field_get_dataptr}
!
!  User can get various bounds and counts information from a {\tt ESMF\_Field}
!  through the {\tt ESMF\_FieldGet} interface in addition to the intrinsic
!  Fortran data pointer contained in the internal {\tt ESMF\_Array} object
!  of a {\tt ESMF\_Field}. The bounds and counts information are DE specific
!  for the associated Fortran data pointer.
!
!  In this example, we first create a 3D Field based on a 3D Grid and Array.
!  Then we use the {\tt ESMF\_FieldGet} interface to retrieve the data pointer,
!  potentially update or verify its values. We also retrieve the bounds and counts
!  information of the 3D Field to assist data element iteration.
!
!EOE
!BOC
    xdim = 180
    ydim = 90
    zdim = 50

    ! create a 3D data Field from Grid and Array
    ! create a Grid 
    grid3d = ESMF_GridCreateShapeTile(minIndex=(/1,1,1/), maxIndex=(/xdim,ydim,zdim/), &
                              regDecomp=(/2,2,1/), name="grid", rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridGet(grid3d, distgrid=distgrid3d, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldGet(grid3d, localDe=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
        totalCount=fa_shape, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    allocate(farray(fa_shape(1), fa_shape(2), fa_shape(3)) )

    ! create an Array 
    array3d = ESMF_ArrayCreate(farray, distgrid=distgrid3d, &
        staggerloc=0, computationalEdgeLWidth=(/0,0,0/), &
        computationalEdgeUWidth=(/-1,-1,-1/), rc=rc) 
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! create a Field
    field = ESMF_FieldCreate(grid3d, array3d, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
  
    ! retrieve the Fortran data pointer from the Field
    call ESMF_FieldGet(field, 0, farray1, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! retrieve the Fortran data pointer from the Field
    call ESMF_FieldGet(field, 0, farray1, &
        computationalLBound=compLBnd, computationalUBound=compUBnd, &
        exclusiveLBound=exclLBnd, exclusiveUBound=exclUBnd, &
        totalLBound=totalLBnd, totalUBound=totalUBnd, &
        computationalCount=comp_count, &
        exclusiveCount=excl_count, &
        totalCount=total_count, &
        rc=rc)   

    do k = totalLBnd(3), totalUBnd(3)
        do j = totalLBnd(2), totalUBnd(2)
            do i = totalLBnd(1), totalUBnd(1)
                farray1(i, j, k) = sin(2*i/total_count(1)*PI) + &
                    sin(4*j/total_count(2)*PI) + &
                    sin(8*k/total_count(2)*PI)
            enddo
        enddo
    enddo
!EOC
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    print *, "Field Get Data Pointer example returned"

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Get Grid and Array and other information from a Field}
!\label{sec:field:usage:field_get_default}
!
!  User can get the internal {\tt ESMF\_Grid} and {\tt ESMF\_Array} 
!  from a {\tt ESMF\_Field}. User should not issue any destroy command
!  on the retrieved grid or array object since they are referenced
!  from the {\tt ESMF\_Field}. The retrieved objects should be used
!  in a read-only fashion to query additional information not directly
!  available through FieldGet interface.
!
!EOE

!BOC
    call ESMF_FieldGet(field, grid=grid, array=array, &
        typekind=typekind, dimCount=rank, staggerloc=staggerloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
        maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
        name=name, &
        rc=rc)
!EOC
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    print *, "Field Get Grid and Array example returned"

    call ESMF_FieldDestroy(field, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_GridDestroy(grid3d, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_ArrayDestroy(array3d, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    deallocate(farray)

!-------------------------------------------------------------------------
    call ESMF_Finalize(rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!-------------------------------------------------------------------------

    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_FieldGetEx.F90"
    else
        print *, "FAIL: ESMF_FieldGetEx.F90"
    end if
end program ESMF_FieldGetEx
