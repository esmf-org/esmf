! $Id: ESMF_FieldFromUserEx.F90,v 1.10.2.4 2009/01/21 21:25:20 cdeluca Exp $
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
    program ESMF_FieldFromUserEx

!------------------------------------------------------------------------------
!ESMF_EXremoveAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_FieldFromUserEx - Field creation from a user's allocated array
!
!
! !DESCRIPTION:
!
! This program shows an example of Field creation (and the underlying IGrid
! creation) that might occur for a user whose code already manages the
! data allocation and distribution, but needs ESMF to correctly interpret
! or recognize the code's fields for other reasons (for example, regridding).
! In this example, we have created simple subroutines to return necessary
! parameters that might otherwise come from queries of the user code.  Their
! names have a "User" prefix and they are, for the most part, dummy routines.
! For the sake of convenience and simplicity, we will assume a one-to-one
! correspondence between the ESMF concepts of PEs, PETs, and DEs in the
! documentation of this example.  Users should be aware that the relationship
! between these can be more complicated if desired.
!-----------------------------------------------------------------------------

    ! ESMF Framework module
    use ESMF_Mod
    use UserCodeMod
    implicit none
    
    ! Local variables
    integer :: finalrc, rc
    integer :: halo
    integer :: decompX, decompY, myPE, myLocation(2)
    integer, dimension(3) :: dataIndexList
    integer, dimension(:), pointer :: distX, distY
    real(ESMF_KIND_R8), dimension(:), pointer :: coordX, coordY
    real(ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr1
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: f90ptr2

    type(ESMF_DELayout) :: layout
    type(ESMF_Field) :: field1, field2
    type(ESMF_FieldDataMap) :: datamap
    type(ESMF_IGrid) :: igrid
    type(ESMF_VM) :: vm
!EOC

    ! Set finalrc to success
    finalrc = ESMF_SUCCESS

    ! necessary call to get ESMF going
    call ESMF_Initialize(rc=rc)

!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  The user has field data from their own code that has already been
!   !  allocated and stored in a Fortran array, but needs ESMF to know about
!   !  it.  The size of this array might be different on different processors.
!   !  It will be important that the information used to create the equivalent
!   !  ESMF structures exactly match the user's code.
 
    ! this is a generic but necessary call
    call ESMF_VMGetGlobal(vm, rc)

    ! This is the first ESMF call that must match the user's implementation.
    ! In this case, decompX and decompY represent the layout of processors
    ! (or PEs) used in the domain decomposition of the user code.  An optional
    ! argument to the following ESMF_DELayoutCreate call, petList, allows
    ! the user to specify a subset or ordering of the processors in case
    ! the user code has a given processor topology.

    call UserGetPEDecomposition(decompX, decompY)
    layout = ESMF_DELayoutCreate(vm, (/ decompX, decompY /), rc=rc)

    ! Once we have created the layout, we can query it to get the location of
    ! this PE (or DE since we are assuming a one-to-one correspondence) in the
    ! layout.  We will use this information later.

    call ESMF_DELayoutGetDeprecated(layout, localDe=myPE, rc=rc)
    call ESMF_DELayoutGetDELocalInfo(layout, de=myPE, coord=myLocation, rc=rc)

    ! If any of the user fields are to be used for ESMF regridding or any
    ! other high-level communication, the ESMF_IGrid must exactly match the
    ! user's igrid, both in a physical sense (coordinates) as well as a
    ! decomposition (or local distribution) sense.  Also, the user must call
    ! the explicit interface for the appropriate IGridType (for example,
    ! LatLon for a latitude-longitude IGrid).  In the future, ESMF should have
    ! the capability to read in a user igrid, but for now the user must specify
    ! an ESMF_IGrid through a Create call.  Please note that the coordiantes
    ! in this call refer to all the igrid coordinates, not just the local (on
    ! any given processor) ones.

    call UserGetIGridCoords(coordX, coordY)
    igrid = ESMF_IGridCreateHorzXY(coord1=coordX, coord2=coordY, &
                                 horzstagger=ESMF_IGRID_HORZ_STAGGER_C_SW, &
                                 periodic=(/ ESMF_TRUE, ESMF_FALSE /), &
                                 name="atmigrid", rc=rc)

    ! Again, the data distribution ESMF will apply must match the user's.  In
    ! this call, distX and distY are arrays containing the number of igrid cells
    ! stored locally on the PEs described by the delayout.

    call UserGetIGridDistribution(distX, distY)
    call ESMF_IGridDistribute(igrid, delayout=layout, &
                             countsPerDEDim1=distX, &
                             countsPerDEDim2=distY, rc=rc)


    ! Create the Field from the F90 pointer and halo width.  The haloWidth given
    ! to ESMF must match the actual user's halo width.  Please note that currently
    ! ESMF requires that the halo width for any given Field be the same in all
    ! directions.  This example of FieldCreate is relatively simple because we
    ! have assumed the Field axes are completely aligned with the IGrid's.

    call UserGetPointer2D(f90ptr1, myLocation(1), myLocation(2))
    call UserGetHalo(halo)
    field1 = ESMF_FieldCreate(igrid, f90ptr1, ESMF_DATA_REF, &
                              horzrelloc=ESMF_CELL_CENTER, &
                              haloWidth=halo, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! This example of FieldCreate is a bit more complicated.  Here we have a 3D
    ! Field on a 2D IGrid, so two of the Field axes can be aligned with the IGrid
    ! but one is not.  ESMF must know which of the Field axes are aligned with 
    ! the IGrid's.  This is done with a FieldDataMap, which is set and then passed
    ! in to the FieldCreate routine.  In this case, we will assume the
    ! second Field axis corresponds to the first IGrid axis and the third Field
    ! axis corresponds to the second IGrid axis.

    call UserGetPointer3D(f90ptr2, myLocation(1), myLocation(2))
    dataIndexList(1) = 0    
    dataIndexList(2) = 1    
    dataIndexList(3) = 2
    call ESMF_FieldDataMapSetDefault(datamap, dataRank=3, &
                                     dataIndexList=dataIndexList, rc=rc)
    field2 = ESMF_FieldCreate(igrid, f90ptr2, ESMF_DATA_REF, &
                              horzRelloc=ESMF_CELL_CENTER, &
                              datamap=datamap, name="concentration", &
                              haloWidth=halo, rc=rc)

    call ESMF_FieldPrint(field1, "", rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldDestroy(field1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldDestroy(field2, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
     call ESMF_Finalize(rc=rc)
!-------------------------------------------------------------------------

    if (finalrc.EQ.ESMF_SUCCESS) then
	print *, "PASS: ESMF_FieldFromUserEx.F90"
    else
	print *, "FAIL: ESMF_FieldFromUserEx.F90"
    endif

!BOC
     end program ESMF_FieldFromUserEx
!EOC
