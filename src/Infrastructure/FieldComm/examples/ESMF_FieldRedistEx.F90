! $Id: ESMF_FieldRedistEx.F90,v 1.16.2.3 2009/01/21 21:25:21 cdeluca Exp $
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

     program FieldRedistEx

!-------------------------------------------------------------------------
!ESMF_EXremoveAMPLE        String used by test script to count examples.
!==============================================================================
!
! !PROGRAM: ESMF_FieldRedistEx - Field Redistribution
!     
! !DESCRIPTION:
!     
! This program shows examples of Field interfaces for redistribution of data.
!-----------------------------------------------------------------------------

     ! ESMF Framework module
     use ESMF_Mod
     implicit none

     ! instantiate two igrids, two fields, and two arrays
     type(ESMF_IGrid)  ::  igrid1,  igrid2
     type(ESMF_Field) :: field1, field2

     ! Local variables
     integer :: finalrc, rc
     integer :: i, j, j1, add
     integer :: counts(2), localCounts(2)
     integer :: npets, myDE
     integer, dimension(:,:), allocatable :: myIndices
     real(ESMF_KIND_R8) :: min(2), max(2)
     real(ESMF_KIND_R8) :: pi = 3.1416d0
     real(ESMF_KIND_R8), dimension(:,:), pointer :: coordX, coordY
     real(ESMF_KIND_R8), dimension(:,:), pointer :: srcdata
     type(ESMF_ArraySpec) :: arrayspec1D, arrayspec2D
     type(ESMF_DELayout) :: delayout1, delayout2
     type(ESMF_IGridHorzStagger) :: horz_stagger
     type(ESMF_RouteHandle) :: rh12
     type(ESMF_VM) :: vm

     finalrc = ESMF_SUCCESS

     ! Initialize the framework and get back the default global VM
     call ESMF_Initialize(vm=vm, rc=rc)
     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE

     ! Get the PET count
     call ESMF_VMGet(vm, petCount=npets, rc=rc)
     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE

     if (npets .eq. 1) then
       print *, "This test must run with > 1 processor"
       finalrc = ESMF_FAILURE
     endif

!BOE
!\subsubsection{Field Redistribution example}

! This example illustrates the use of Field interfaces for redistribution of
! data.
!
! Basically redistribution works on two Fields that are on the same IGrid except
! that the IGrid is distributed differently.  In this example, two IGrids are created
! from the same underlying 2D horizontal IGrid, but one is distributed as logical
! blocks and the other is distributed as arbitrary vectors.
!EOE

!BOC
     ! First create two layouts, one for a 2D block distribution and a 1D layout
     ! for vector distribution:

     delayout1 = ESMF_DELayoutCreate(vm, (/ 2, npets/2 /), rc=rc)
     delayout2 = ESMF_DELayoutCreate(vm, (/ npets, 1 /), rc=rc)
!EOC

     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
     ! Next create the IGrids with exactly the same underlying parameters:

     counts(1) = 60
     counts(2) = 40
     min(1)    =  0.0
     max(1)    = 60.0
     min(2)    =  0.0
     max(2)    = 50.0
     horz_stagger = ESMF_IGRID_HORZ_STAGGER_A

     igrid1 = ESMF_IGridCreateHorzXYUni(counts=counts, &
                             minGlobalCoordPerDim=min, &
                             maxGlobalCoordPerDim=max, &
                             horzStagger=horz_stagger, &
                             name="source igrid", rc=rc)
!EOC

     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
     igrid2 = ESMF_IGridCreateHorzXYUni(counts=counts, &
                             minGlobalCoordPerDim=min, &
                             maxGlobalCoordPerDim=max, &
                             horzStagger=horz_stagger, &
                             name="source igrid", rc=rc)
!EOC

     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
     ! With two identical IGrids, distribute one in the normal block style:

     call ESMF_IGridDistribute(igrid1, delayout=delayout1, rc=rc)
!EOC

     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE

     ! and get our local DE number on the second layout
     call ESMF_DELayoutGetDeprecated(delayout2, localDE=myDE, rc=rc)
     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
     ! The second IGrid is distributed in arbitrary vectors.  The following code
     ! fragment calculates the vectors of index pairs in {\tt myIndices}, based
     ! on the local DE number.  This is just a simple algorithm to create a
     ! semi-regular distribution of points to the PETs.

     i = int((counts(1)*counts(2) + npets -1)/npets)
     allocate (myIndices(i,2))

     j1  = 1 + myDE
     add = 0
     do i = 1,counts(1)
       do j = j1,counts(2),npets
         add = add + 1
         myIndices(add,1) = i
         myIndices(add,2) = j
       enddo
       j1 = j - counts(2)
     enddo

     call ESMF_IGridDistribute(igrid2, delayout=delayout2, myCount=add, &
                              myIndices=myIndices, rc=rc)
!EOC

     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE

     ! Set up a 1D (for the vector Field) and a 2D real array
     call ESMF_ArraySpecSet(arrayspec2D, rank=2, &
                            typekind=ESMF_TYPEKIND_R8, rc=rc)
     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE
     call ESMF_ArraySpecSet(arrayspec1D, rank=1, &
                            typekind=ESMF_TYPEKIND_R8, rc=rc)
     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
     ! Create Fields for each of the IGrids:
     field1 = ESMF_FieldCreate(igrid1, arrayspec2D, &
                               horzRelloc=ESMF_CELL_CENTER, &
                               haloWidth=0, name="humidity1", rc=rc)

     field2 = ESMF_FieldCreate(igrid2, arrayspec1D, &
                               horzRelloc=ESMF_CELL_CENTER, &
                               haloWidth=0, name="humidity2", rc=rc)
!EOC

     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
     ! Then precompute the communication pattern to
     ! move data from the regularly distributed Field1 
     ! to the arbitrarily stored Field2:
     call ESMF_FieldRedistStore(field1, field2, vm, routehandle=rh12, rc=rc)
!EOC

     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE

     ! get coordinate arrays available for setting the source data array
     call ESMF_IGridGetCoord(igrid1, dim=1, horzRelloc=ESMF_CELL_CENTER, &
                            centerCoord=coordX, localCounts=localCounts, rc=rc)
     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE
     call ESMF_IGridGetCoord(igrid1, dim=2, horzRelloc=ESMF_CELL_CENTER, &
                            centerCoord=coordY, rc=rc)
     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE

     ! Get pointers to the data and set it up
     call ESMF_FieldGetDataPointer(field1, srcdata, ESMF_DATA_REF, rc=rc)
     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE

     ! initialize data arrays
     srcdata = 0.0

     ! set data array to a function of coordinates (in the computational part
     ! of the array only, not the halo region)
     do j   = 1,localCounts(2)
       do i = 1,localCounts(1)
         srcdata(i,j) = 10.0 + 5.0*sin(coordX(i,j)/60.0*pi) &
                             + 2.0*sin(coordY(i,j)/50.0*pi) 
       enddo
     enddo

     ! No deallocate() is needed for array data, it will be freed when the
     ! Array is destroyed. 

!BOC
     ! After the data in Field1 has been filled, simply call the 
     ! redistribution method here to move the data to Field2:
     call ESMF_FieldRedist(field1, field2, rh12, rc=rc)
!EOC

     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE

     ! Clean up

     deallocate(myIndices)

!BOC
     ! Once the Route is no longer needed, it is up to the user to release
     ! it, since the user created it:
     call ESMF_FieldRedistRelease(rh12, rc)
!EOC


     ! Second example showing one fuction call
!BOE
!\subsubsection{Field Redistribution example using a single call}

! This example illustrates the use of Field interfaces for redistribution of
! data with a single call.  Using the data structures from the previous example,
! this example illustrates the capability to perform a redistribution in a single
! call to FieldRedist rather than the three separate calls to FieldRedistStore,
! FieldRedist, and FieldRedistRelease.  Please note that in this case the calling
! argument list does not include a RouteHandle and one is not returned to the
! user for reuse.  However, this interface can be useful for some applications
! where there is no future use of the communication patterns.
!
!EOE

!BOC
     ! Note that this call looks similar to the previous one applying the
     ! precomputed RouteHandle with the exception of requiring the VM in
     ! the calling list.

     call ESMF_FieldRedist(field1, field2, parentVM=vm, rc=rc)

!EOC

     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE
     call ESMF_FieldDestroy(field1, rc)
     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE
     call ESMF_FieldDestroy(field2, rc)
     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE
     call ESMF_IGridDestroy(igrid1, rc)
     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE
     call ESMF_IGridDestroy(igrid2, rc)
     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE
     call ESMF_DELayoutDestroy(delayout1, rc)
     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE
     call ESMF_DELayoutDestroy(delayout2, rc)
     if (rc.ne.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

     call ESMF_Finalize(rc=rc)

     if (rc.NE.ESMF_SUCCESS) then
       finalrc = ESMF_FAILURE
     end if

     if (finalrc.EQ.ESMF_SUCCESS) then
       print *, "PASS: ESMF_FieldRedistEx.F90"
     else
       print *, "FAIL: ESMF_FieldRedistEx.F90"
     end if

    end program FieldRedistEx
