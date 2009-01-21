! $Id: ESMF_FieldSetEx.F90,v 1.3.2.7 2009/01/21 21:25:20 cdeluca Exp $
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
    program ESMF_FieldSetEx

!------------------------------------------------------------------------------
!ESMF_removeEXAMPLE        String used by test script to count examples.
!==============================================================================
! !PROGRAM: ESMF_FieldSetEx - Field Set Examples
!
! !DESCRIPTION:
!
! This program shows examples of Field get data pointer methods
!-----------------------------------------------------------------------------

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_FieldSetMod
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
    integer             :: gec(3), gcc(3)
    integer             :: fa_shape(3)

    integer :: finalrc       
!   !Set finalrc to success
    finalrc = ESMF_SUCCESS

    call ESMF_Initialize(rc=rc)

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BremoeOE
!\subsubsection{Set the Fortran data pointer in a Field}
!
!  Through the {\tt ESMF\_FieldSetDataPtr} interface user can reset the intrinsic
!  Fortran data pointer contained in the internal {\tt ESMF\_Array} object
!  of a {\tt ESMF\_Field}.
!  {\tt ESMF\_FieldSetDataPtr} is an overloaded interface based on the type,
!  kind, and rank of the input fortran pointer argument. In this example,
!  a rank 3 ESMF\_KIND\_R8 fortran data pointer is used.
!  This method internally creates a ESMF\_Array inside the
!  field. The previous ESMF\_Array will be deleted if it was internally
!  created by the field, otherwise it's simply replaced by the newly created
!  array. 
! 
!  The shape of the Fortran array is computed using the same approach explained
!  in the sections on Field creation from a Fortran data array.
!
!EremoeOE
!BremoeOC
    xdim = 12
    ydim = 22
    zdim = 31

    grid8 = ESMF_GridCreateShapeTile(minIndex=(/1,1,1/), maxIndex=(/4*xdim,ydim,zdim/), &
                              regDecomp=(/4,1,1/), name="grid", rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE


    call ESMF_GridGet(grid, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
        computationalCount=gcc, exclusiveCount=gec, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    do i = 1, 3
        fa_shape(i) = max(gec(i), gcc(i))
    end do

    allocate(farray1(fa_shape(1), fa_shape(2), fa_shape(3)))
    call ESMF_GridGet(grid8, distgrid=distgrid8, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    array8 = ESMF_ArrayCreate(farray, distgrid=distgrid8, &
        staggerloc=0, computationalEdgeLWidth=(/0,0,0/), computationalEdgeUWidth=(/-1,-1,-1/), rc=rc) 
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    f8 = ESMF_FieldCreate(grid8, array8, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
  
    allocate(farray(fa_shape(1), fa_shape(2), fa_shape(3)))
    call ESMF_FieldSetDataPtr(f8, farray, rc=rc)
!EremoeOC
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    print *, "Field Set Data Pointer example returned"

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BremoeOE
!\subsubsection{Set the Grid in a Field}
!
!  User can reset the internal {\tt ESMF\_Grid} object of a {\tt ESMF\_Field}
!  through ESMF\_FieldSetGrid interface. Invalid Grid that has different computational
!  or exclusive bounds from the Array in the Field will be rejected. These
!  are expensive operations that should be avoided for performance critical portion
!  of a model.
!
!EremoveOE
!BremoveOC
    grid = ESMF_GridCreateShapeTile(minIndex=(/1,1,1/), maxIndex=(/4*xdim,ydim,zdim/), &
                              regDecomp=(/4,1,1/), name="grid", rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    
    call ESMF_FieldSetGrid(f8, grid=grid, rc=rc)
!EremoveOC
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    print *, "Field Set Grid example returned"

    call ESMF_FieldDestroy(f8, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_GridDestroy(grid8, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_ArrayDestroy(array8, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    deallocate(farray)
    deallocate(farray1)
!-------------------------------------------------------------------------
    call ESMF_Finalize(rc=rc)
!-------------------------------------------------------------------------

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    if (finalrc.EQ.ESMF_SUCCESS) then
	print *, "PASS: ESMF_FieldSetEx.F90"
    else
	print *, "FAIL: ESMF_FieldSetEx.F90"
    end if
end program ESMF_FieldSetEx
