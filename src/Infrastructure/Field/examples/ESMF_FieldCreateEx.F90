! $Id: ESMF_FieldCreateEx.F90,v 1.61 2008/01/31 20:43:05 feiliu Exp $
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
    program ESMF_FieldCreateEx

!------------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
! !PROGRAM: ESMF_FieldCreateEx - Field creation
!
! !DESCRIPTION:
!
! This program shows examples of Field initialization and manipulation
!-----------------------------------------------------------------------------

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_FieldGetMod
    implicit none
    
    ! Local variables
    integer :: rc
    integer :: mycell
    type(ESMF_Grid) :: grid
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array) :: array1, array2, array3
    type(ESMF_DELayout) :: layout
    type(ESMF_VM) :: vm
    !type(ESMF_RelLoc) :: relativelocation
    !type(ESMF_FieldDataMap) :: datamap
    type(ESMF_Field) :: field1, field2, field3
    real (ESMF_KIND_R8), dimension(2) :: origin
    character (len = ESMF_MAXSTR) :: fname

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

!BOE
!\subsubsection{Field Create with Grid and Arrayspec}
!  The user has already created an {\tt ESMF\_Grid} and an
!  {\tt ESMF\_Arrayspec} with data.  This create associates the
!  two objects.  
!EOE
!-------------------------------------------------------------------------
!   !
!   !  We first create a Grid with a regular distribution that is
!   !  10x20 DEs.  This version of create simply
!   !  associates the data with the Grid.  The data is referenced
!   !  explicitly on a regular 2x2 uniform grid. 
!   !  Then we create an arrayspec. With grid and arrayspec,
!   !  we then create a field.

!BOC
    grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="atmgrid", rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R4, rc)

    field1 = ESMF_FieldCreate(grid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="pressure", rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldGet(field1, name=fname, rc=rc)
    print *, "Field creation from Grid and Arrayspec returned, name = ", trim(fname)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Empty Field Create}

!  The user creates an empty {\tt ESMF\_Field} object.
!  Then the user can add the Grid and data in later calls
!EOE
!-------------------------------------------------------------------------
!   !
!   !  The user creates an empty Field, and adds the Grid and 
!   !  data in later calls.

!BOC
     field3 = ESMF_FieldCreateEmpty("precip", rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!
!    ! At some later time, associate a Grid with this Field
     call ESMF_FieldSetGrid(field3, grid, rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    print *, "Field Empty Field Creation example returned"

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Replace the ESMF_Array inside a Field}
!  It's often necessary to replace the internal data array within a field.
!  The following examples demonstrate different ways of creating {\tt ESMF\_Array}
!  and replace the existing {\tt ESMF\_Array} of a {\tt ESMF\_Field}
!EOE

!BOE
!\subsubsection{Use ESMF_ArrayCreateFromGrid to replace Field internal Array}
!  User can replace the {\tt ESMF\_Array} inside an existing Field by construct a proper
!  shape {\tt ESMF\_Array} by calling {\tt ESMF\_ArrayCreateFromGrid}
!EOE
!-------------------------------------------------------------------------
!   !
!   !  The user wishes to associate different data with the Field
!   !  created in example 1.  The get data call returns the 
!   !  pointer to the old data array; the set call passes in the 
!   !  pointer to the new array.

    call ESMF_FieldGetArray(field1, array=array1, rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! The size of the data in the array still has to line up with the Grid
    ! and its decomposition.
!BOC
    array2 = ESMF_ArrayCreateFromGrid(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
        rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldSetArray(field1, array2, rc=rc)
!EOC
    print *, "Field replace Field internal array returned"

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE


!BOE
!\subsubsection{Use ESMF_ArrayCreate to to replace Field internal Array}
!  User can replace the {\tt ESMF\_Array} inside an existing Field by construct a proper
!  shape {\tt ESMF\_Array}
!EOE
!-------------------------------------------------------------------------
!   !
!   !  The user can substitute another array created by ArrayCreate in field1.
!   ! This example demonstrates some of the topology nature of a field
!   ! default created through FieldCreate which internally calls
!   ! ArrayCreateFromGrid which is done in example 2. This example
!   ! makes it clear that field1's array has a computational region smaller
!   ! than its exclusive region.
!BOC
    array3 = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, staggerLoc=0, &
            computationalEdgeLWidth=(/0,0/), computationalEdgeUWidth=(/-1,-1/), rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldSetArray(field1, array3, rc=rc)
!EOC
    print *, "Field replace internal array returned"

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Destroy a Field}

!  When finished with an {\tt ESMF\_Field}, the destroy method
!  removes it.  However, the objects inside the {\tt ESMF\_Field}
!  that has external reference should be deleted separately, 
!  since objects can be added to
!  more than one {\tt ESMF\_Field}, for example the same {\tt ESMF\_Grid}
!  can be used in multiple {\tt ESMF\_Field}s.
!EOE
!-------------------------------------------------------------------------
!   !
!   ! Query a Field for number of local grid cells.
!   COMMENT THIS TEST OUT FOR NOW BECAUSE THE SUBROUTINE
!   IS UNIMPLEMENTED CAN TURN BACK ON WHEN INITMACROS ARE ON
!     call ESMF_FieldGetLocalGridInfo(field3, ncell=mycell, rc=rc)
!     print *, "Field example 5 returned"
!
!    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
    call ESMF_FieldDestroy(field1, rc=rc)
!EOC

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
end program ESMF_FieldCreateEx
    
