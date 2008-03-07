! $Id: ESMF_FieldCreateEx.F90,v 1.55.2.6 2008/03/07 16:39:23 feiliu Exp $
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
    integer :: rc, localPet, petCount
    type(ESMF_Grid) :: grid
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array) :: array1, array2
    type(ESMF_DELayout) :: layout
    type(ESMF_VM) :: vm

    type(ESMF_Field) :: field1, field2, field3, field4
    real (ESMF_KIND_R8), dimension(2) :: origin
    character (len = ESMF_MAXSTR) :: fname

    real(ESMF_KIND_R8), dimension(:,:), pointer :: farray
    real(ESMF_KIND_R8), dimension(:,:), pointer :: farray1
    integer               :: xdim, ydim, zdim
    integer, dimension(2) :: compEdgeLWdith
    integer, dimension(2) :: compEdgeUWdith
    integer, dimension(ESMF_MAXDIM) :: gcc, gec

    integer :: finalrc       
!   !Set finalrc to success
    finalrc = ESMF_SUCCESS

    call ESMF_Initialize(vm=vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create Field with Grid and Arrayspec}
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
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    field1 = ESMF_FieldCreate(grid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="pressure", rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EOC

    print *, "Field creation from Grid and Arrayspec returned, name = ", trim(fname)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BremoveOE
!\subsubsection{Create Empty Field and Finish an empty Field 
!  with FieldSetCommit}

!  The user creates an empty {\tt ESMF\_Field} object.
!  Then the user can finalize a {\tt ESMF\_Field} from a {\tt ESMF\_Grid} and a intrinsic 
!  Fortran data pointer. This interface is overloaded for type, kind, rank of
!  of the fortran data pointer.
!EremoveOE

!BremoveOC
    field3 = ESMF_FieldCreateEmpty("precip", rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridGet(grid, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
        computationalCount=gcc, exclusiveCount=gec, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    allocate(farray(max(gec(1), gcc(1)), max(gec(2), gcc(2))) )
    call ESMF_FieldSetCommit(field3, grid, farray, rc=rc)
!EremoveOC
    print *, "Finish a Field created by ESMF_FieldCreateEmpty returned"
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Use ESMF\_ArrayCreate to reset Field internal Array}
!  It's often necessary to reset the data array contained within a field.
!  The following examples demonstrate different ways of creating {\tt ESMF\_Array}
!  and reset the existing {\tt ESMF\_Array} of a {\tt ESMF\_Field}.
!  User can reset the {\tt ESMF\_Array} inside an existing Field by construct a proper
!  shape {\tt ESMF\_Array} 
!  arrayspec, distgrid are objects created from previous examples.
!EOE
!-------------------------------------------------------------------------
!   !
!   !  The user can substitute another array created by ArrayCreate in field1.
!   ! This example
!   ! makes it clear that field1's array has a computational region smaller
!   ! than its exclusive region.
!BOC
    call ESMF_GridGet(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
        computationalEdgeLWidth=compEdgeLWdith, &
        computationalEdgeUWidth=compEdgeUWdith, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    array2 = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, staggerLoc=0, &
            computationalEdgeLWidth=compEdgeLWdith, &
            computationalEdgeUWidth=compEdgeUWdith, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldSetArray(field1, array2, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EOC
    print *, "Field reset internal array through ArrayCreate returned"

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BremoveOE
!\subsubsection{Create Field with Grid and Array}
!  User can create a {\tt ESMF\_Field} from a {\tt ESMF\_Grid} and a 
!  {\tt ESMF\_Array}. grid and array2 are objects created in previous examples.
!EremoveOE

!BremoveOC
    field4 = ESMF_FieldCreate(grid, array2, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EremoveOC
    print *, "Field Create from a Grid and a ESMF_Array returned"
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BremoveOE
!\subsubsection{Create Field with Grid and Fortran data pointer}
!  User can create a {\tt ESMF\_Field} from a {\tt ESMF\_Grid} and a intrinsic 
!  Fortran data pointer. This interface is overloaded for type, kind, rank of
!  of the fortran data pointer. grid and farray are created in previous examples.
!EremoveOE

!BremoveOC
    field2 = ESMF_FieldCreate(grid, farray, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EremoveOC
    print *, "Field Create from a Grid and a Fortran data pointer returned"
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
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

!BOC
    call ESMF_FieldDestroy(field1, rc=rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

! Destroy objects
    call ESMF_FieldDestroy(field4,rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_FieldDestroy(field3,rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_FieldDestroy(field2,rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_ArrayDestroy(array2, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    deallocate(farray)

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
