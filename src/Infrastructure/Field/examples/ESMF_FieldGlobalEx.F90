! $Id: ESMF_FieldGlobalEx.F90,v 1.11 2007/06/22 23:21:29 cdeluca Exp $
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
    program ESMF_FieldGlobalEx

!------------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_FieldGlobalEx - Field creation using global indexing
!
! !DESCRIPTION:
!
! This program shows an example of Field creation using global indexing.
! At some point global indexing may be an option for FieldCreate routines,
! but in the meantime this is an example of one way to accomplish it.
! This example will show how to create a 3D Field and set its relationship to
! a corresponding 2D InternGrid.
!-----------------------------------------------------------------------------

    ! ESMF Framework module
    use ESMF_Mod
    implicit none
    
    ! Local variables
    integer :: finalrc, haloWidth, i, rc
    integer :: interngridCount(2), interngridStart(2)
    integer :: dataIndexList(3), lbounds(3), localCount(3), ubounds(3)
    real (ESMF_KIND_R8), dimension(:,:,:), pointer :: f90ptr1
    real (ESMF_KIND_R8), dimension(2) :: origin
    type(ESMF_InternArray)        :: iarray1
    type(ESMF_DELayout)     :: layout
    type(ESMF_Field)        :: field1
    type(ESMF_FieldDataMap) :: datamap
    type(ESMF_InternGrid)         :: interngrid
    type(ESMF_VM)           :: vm
!EOC

!   !Set finalrc to success
    finalrc = ESMF_SUCCESS

    call ESMF_Initialize(rc=rc)

!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  The user has already created a InternGrid and has Field data
!   !  stored in an Array object.  This version of create simply
!   !  associates the data with the InternGrid.  The data is referenced
!   !  by default.
 
    call ESMF_VMGetGlobal(vm, rc)
    layout = ESMF_DELayoutCreate(vm, (/ 3, 2 /), rc=rc)

    origin = (/ 0.0, 0.0 /)
    interngrid = ESMF_InternGridCreateHorzXYUni((/ 50, 30 /), origin, &
                                    deltaPerDim=(/ 1.0d0, 1.0d0 /), &
                                    name="atminterngrid", rc=rc)
    call ESMF_InternGridDistribute(interngrid, delayout=layout, rc=rc)

    ! get interngrid information used to calculate global indices
    call ESMF_InternGridGetDELocalInfo(interngrid, horzrelloc=ESMF_CELL_CENTER, &
                                 localCellCountPerDim=interngridCount, &
                                 globalStartPerDim=interngridStart, rc=rc)

    ! globalStartPerDim really should be called something more descriptive like
    ! globalOffsetPerDim because it refers to the amount that must be added to a
    ! local index to translate it to a global index.  So the globalStart referring
    ! to an index instead of an offset should be this value plus one
    interngridStart(1) = interngridStart(1) + 1
    interngridStart(2) = interngridStart(2) + 1

    ! set local counts for the Field.  For this example, the second dimension of the
    ! data will correspond to the first InternGrid dimension and the third data dimension
    ! will correspond to the second InternGrid dimension
    dataIndexList(1) = 0
    dataIndexList(2) = 1
    dataIndexList(3) = 2
    lbounds(2)    = interngridStart(1)
    localCount(2) = interngridCount(1)
    lbounds(3)    = interngridStart(2)
    localCount(3) = interngridCount(2)

    ! the first data dimension is unrelated to the interngrid, so it has a user-specified
    ! count and its local index is assumed to start at one, although that is not
    ! necessarily true
    lbounds(1)    = 1
    localCount(1) = 32

    ! calculate upper bounds from lower bounds and counts
    do i = 1,3
      ubounds(i) = lbounds(i) + localCount(i) - 1
    enddo

    ! set the haloWidth of the Array
    haloWidth = 3

    ! modify the lower and upper bounds by the haloWidth
    ! Currently ESMF requires that all dimensions include the halo width, even if
    ! they are not related to the interngrid.  Hopefully that will no longer be
    ! required soon.
    do i = 1,3
      lbounds(i) = lbounds(i) - haloWidth
      ubounds(i) = ubounds(i) + haloWidth
    enddo

    ! allocate the F90 pointer with the calculated bounds
    allocate(f90ptr1(lbounds(1):ubounds(1), &
                     lbounds(2):ubounds(2), &
                     lbounds(3):ubounds(3)))

    ! create the array from the F90 pointer and haloWidth
    iarray1 = ESMF_InternArrayCreate(f90ptr1, ESMF_DATA_REF, haloWidth=haloWidth, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_InternArrayPrint(iarray1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Field Create with InternGrid, DataMap and InternArray}

!  The user has already created an {\tt ESMF\_InternGrid} and an
!  {\tt ESMF\_InternArray} with data.  The user creates a FieldDataMap, and then this
!  create associates the two objects.
!EOE

!BOC
    call ESMF_FieldDataMapSetDefault(datamap, dataRank=3, &
                                     dataIndexList=dataIndexList, &
                                     counts=localCount(1:1), rc=rc)
    field1 = ESMF_FieldCreate(interngrid, iarray1, horzRelloc=ESMF_CELL_CENTER, &
                              datamap=datamap, name="pressure", rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldPrint(field1, "", rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldDestroy(field1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
     call ESMF_Finalize(rc=rc)
!-------------------------------------------------------------------------

    if (finalrc.EQ.ESMF_SUCCESS) then
	print *, "PASS: ESMF_FieldGlobalEx.F90"
    else
	print *, "FAIL: ESMF_FieldGlobalEx.F90"
    end if
!BOC
     end program ESMF_FieldGlobalEx
!EOC
    
