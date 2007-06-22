! $Id: ESMF_InternGridCreateEx.F90,v 1.1 2007/06/22 23:21:35 cdeluca Exp $
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
      program ESMF_InternGridCreateEx

!-------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_InternGridCreateEx - InternGrid creation
!
! !DESCRIPTION:
!
! This program shows examples of different methods to create 2D and 3D interngrids
!-----------------------------------------------------------------------------

      ! ESMF Framework module
      use ESMF_Mod
      implicit none
    
      ! instantiate two interngrids
      type(ESMF_InternGrid) :: interngrid1, interngrid2, interngrid3

      ! instantiate horizontal and vertical interngrid staggerings
      type(ESMF_InternGridHorzStagger) :: horz_stagger
      type(ESMF_InternGridVertStagger) :: vert_stagger

      ! local variables for Create routines
      integer :: counts(2), countsPerDE1(2), countsPerDE2(2)
      integer :: myCount
      integer, dimension(:,:), allocatable :: myIndices
      character(len=ESMF_MAXSTR) :: name
      real(ESMF_KIND_R8), dimension(2) :: min, max
      real(ESMF_KIND_R8) :: delta1(40), delta2(50), delta3(10)
      type(ESMF_ArraySpec):: arrayspec1
      type(ESMF_Field):: humidity1

      ! return code
      integer :: rc
!EOC

      ! Local variables
      type(ESMF_DELayout) :: layout
      type(ESMF_VM) :: vm

      integer :: i, j, j1, myDE, n, npets
      integer :: finalrc
      finalrc = ESMF_SUCCESS

!BOC
      ! initialize ESMF framework
      call ESMF_Initialize(rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
      call ESMF_VMGetGlobal(vm, rc)
      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
      call ESMF_VMGet(vm, petCount=npets, rc=rc)

!BOE
!\subsubsection{Uniform 2D InternGrid Creation}

! This example shows how to create a simple uniform horizontal {\tt ESMF\_InternGrid}.
!EOE
    
!BOC

      ! set the global number of computational cells in each direction
      counts(1)    = 10
      counts(2)    = 12

      ! set the global coordinate extrema
      min(1)       = 0.0
      max(1)       = 10.0
      min(2)       = 0.0
      max(2)       = 12.0

      ! set the staggering for the horizontal interngrid
      horz_stagger = ESMF_IGRID_HORZ_STAGGER_A

      ! and add a name to the interngrid
      name         = "test interngrid 1"
 
      ! create a 2 x 2 layout for the InternGrid
      layout = ESMF_DELayoutCreate(vm, (/ 2, 2 /), rc=rc)
!EOC
 
      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize a simple uniform horizontal interngrid with the above values
      interngrid1 = ESMF_InternGridCreateHorzXYUni(counts=counts, &
                                       minGlobalCoordPerDim=min, &
                                       maxGlobalCoordPerDim=max, &
                                       horzstagger=horz_stagger, &
                                       name=name, rc=rc)

      ! distribute the interngrid
      call ESMF_InternGridDistribute(interngrid1, delayout=layout, rc=rc)

!EOC
 
      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "InternGrid example 1 returned"

      call ESMF_InternGridDestroy(interngrid1, rc)

      print *, "InternGrid example 1 destroyed"
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{3D InternGrid Creation}

! This example shows how to create a 3D {\tt ESMF\_InternGrid} with specified,
! non-uniform spacing.
!EOE

!BOC
      ! set the global coordinate minima for the horizontal interngrid
      ! note: the vertical interngrid does not need a coordinate minimum
      !       because the specific call to InternGridAddVertHeight infers
      !       a minimum at 0.0.
      min(1)       = 0.0
      min(2)       = 0.0

      ! set up arrays of coordinate spacing for the horizontal interngrid
      delta1 = (/ 1.0, 1.0, 1.0, 1.1, 1.1, 1.1, 1.2, 1.2, 1.3, 1.4, &
                  1.4, 1.5, 1.6, 1.6, 1.6, 1.8, 1.8, 1.7, 1.7, 1.6, &
                  1.6, 1.6, 1.8, 1.8, 2.0, 2.0, 2.2, 2.2, 2.2, 2.2, &
                  2.0, 1.7, 1.5, 1.3, 1.2, 1.1, 1.0, 1.0, 1.0, 0.9 /)
      delta2 = (/ 0.8, 0.8, 0.8, 0.8, 0.8, 0.7, 0.7, 0.6, 0.7, 0.8, &
                  0.9, 0.9, 0.9, 0.9, 1.0, 1.0, 1.0, 1.0, 0.9, 1.0, &
                  1.0, 1.0, 1.0, 1.1, 1.2, 1.3, 1.3, 1.3, 1.4, 1.4, &
                  1.4, 1.4, 1.4, 1.4, 1.4, 1.3, 1.3, 1.3, 1.2, 1.2, &
                  1.1, 1.0, 1.0, 0.9, 0.8, 0.7, 0.6, 0.6, 0.5, 0.5 /)

      ! set array of coordinate spacing for the vertical interngrid
      delta3 = (/ 1.0, 1.0, 1.0, 0.5, 0.5, 0.6, 0.8, 1.0, 1.0, 1.0 /)

      ! set the staggerings for the horizontal and vertical interngrids
      horz_stagger = ESMF_IGRID_HORZ_STAGGER_D_NE
      vert_stagger = ESMF_IGRID_VERT_STAGGER_CENTER

      ! and add a name to the interngrid
      name         = "test interngrid 2"

      ! set specified number of computational cells per DE for each
      ! decomposition direction
      countsPerDE1 = (/ 26, 14 /)
      countsPerDE2 = (/ 22, 28 /)
!EOC
 
      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize the interngrid with the above values
      interngrid2 = ESMF_InternGridCreateHorzLatLon(minGlobalCoordPerDim=min, &
                                        delta1=delta1, delta2=delta2, &
                                        horzstagger=horz_stagger, &
                                        name=name, rc=rc)

      ! add a vertical subinterngrid to the horizontal interngrid
      ! note: the vertical subinterngrid must be added before the interngrid is
      !      distributed
      call ESMF_InternGridAddVertHeight(interngrid2, delta3, vertstagger=vert_stagger, &
                                  rc=rc)
!EOC
 
      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC

      ! distribute the interngrid using the same layout as from the first example
      ! but specifying the decomposition of computational cells
      call ESMF_InternGridDistribute(interngrid2, delayout=layout, &
                               countsPerDEDim1=countsPerDE1, &
                               countsPerDEDim2=countsPerDE2, &
                               rc=rc)

!EOC
 
      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

      print *, "InternGrid example 2 returned"

      call ESMF_InternGridDestroy(interngrid2, rc)

      print *, "InternGrid example 2 destroyed"

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOE
!\subsubsection{3D InternGrid Creation with Arbitrary Distribution}

! This example shows how to create the same non-uniform 3D {\tt ESMF\_InternGrid} as
! from the previous example but distributed in an arbitrary fashion as one might
! for a column model.  Different from a block distributed ESMF\_InternGrid, the Field 
! associated with an arbitrary InternGrid has to be one dimension less than the InternGrid 
! itself.  A Field associated with a 2D horizontal InternGrid is stored as an 1D
! array. In this example, the Field has to be a 2D Field for the 3D ESMF\_InternGrid
! with the 2 horizontal dimensions collapsed into the first dimension of the
! Field and the vertical dimension stored in the second dimension of the 
! Field.
!EOE

!BOC
      ! Use the same parameters to create the InternGrid as before
      min(1)       = 0.0
      min(2)       = 0.0

      delta1 = (/ 1.0, 1.0, 1.0, 1.1, 1.1, 1.1, 1.2, 1.2, 1.3, 1.4, &
                  1.4, 1.5, 1.6, 1.6, 1.6, 1.8, 1.8, 1.7, 1.7, 1.6, &
                  1.6, 1.6, 1.8, 1.8, 2.0, 2.0, 2.2, 2.2, 2.2, 2.2, &
                  2.0, 1.7, 1.5, 1.3, 1.2, 1.1, 1.0, 1.0, 1.0, 0.9 /)
      delta2 = (/ 0.8, 0.8, 0.8, 0.8, 0.8, 0.7, 0.7, 0.6, 0.7, 0.8, &
                  0.9, 0.9, 0.9, 0.9, 1.0, 1.0, 1.0, 1.0, 0.9, 1.0, &
                  1.0, 1.0, 1.0, 1.1, 1.2, 1.3, 1.3, 1.3, 1.4, 1.4, &
                  1.4, 1.4, 1.4, 1.4, 1.4, 1.3, 1.3, 1.3, 1.2, 1.2, &
                  1.1, 1.0, 1.0, 0.9, 0.8, 0.7, 0.6, 0.6, 0.5, 0.5 /)
      delta3 = (/ 1.0, 1.0, 1.0, 0.5, 0.5, 0.6, 0.8, 1.0, 1.0, 1.0 /)

      horz_stagger = ESMF_IGRID_HORZ_STAGGER_D_NE
      vert_stagger = ESMF_IGRID_VERT_STAGGER_CENTER

      name         = "test interngrid 3"

      ! initialize the interngrid with the above values
      interngrid3 = ESMF_InternGridCreateHorzLatLon(minGlobalCoordPerDim=min, &
                                        delta1=delta1, delta2=delta2, &
                                        horzstagger=horz_stagger, &
                                        name=name, rc=rc)

!EOC
 
      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! as before, add a vertical subinterngrid to the horizontal interngrid
      call ESMF_InternGridAddVertHeight(interngrid3, delta3, vertstagger=vert_stagger, &
                                  rc=rc)
!EOC
 
      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

      ! allocate myIndices to an appropriate size
      counts(1) = size(delta1)
      counts(2) = size(delta2)
      i = int((counts(1)*counts(2) + npets -1)/npets)
      allocate (myIndices(i,2))

     ! and get our local DE number from the layout
     call ESMF_DELayoutGetDeprecated(layout, localDE=myDE, rc=rc)

!BOC

      ! Calculate myIndices based on DE number.
      ! This is just a simple algorithm to create a semi-regular distribution
      ! of points to the pets.   It starts at point (1,1+myDE) and go up in the
      ! j-direction first, and creates a 2D array of point indices that looks
      ! like:  for n = 1, myCount
      !        myIndices(n,1) = global i-index of the nth local point
      !        myIndices(n,2) = global j-index of the nth local point
      j1 = 1 + myDE
      n  = 0
      do i   =  1,counts(1)
        do j = j1,counts(2),npets
          n = n + 1
          myIndices(n,1) = i
          myIndices(n,2) = j
        enddo
        j1 = j - counts(2)
      enddo
      myCount = n

      ! The distribute call is similar to the block distribute but with
      ! a couple of different arguments
      call ESMF_InternGridDistribute(interngrid3, delayout=layout, myCount=myCount, &
                               myIndices=myIndices, rc=rc)

      ! The Field for an arbitrary InternGrid has to be one dimension less, i.e., 2D
      ! for a 3D InternGrid.
      ! Set up 2D Array for the Field
      call ESMF_ArraySpecSet(arrayspec1, rank=2, &
           typekind=ESMF_TYPEKIND_R8)

      ! Create the Field 
      humidity1 = ESMF_FieldCreate(interngrid3, arrayspec1, &
		horzRelloc=ESMF_CELL_CENTER, vertRelloc=ESMF_CELL_CENTER, &
		haloWidth=0, name="humidity1", rc=rc)
!EOC
 
      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

      print *, "InternGrid example 3 returned"

      deallocate(myIndices)
      call ESMF_InternGridDestroy(interngrid3, rc)

      print *, "InternGrid example 3 destroyed"

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

      call ESMF_Finalize(rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

     if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_InternGridCreateEx.F90"
     else
        print *, "FAIL: ESMF_InternGridCreateEx.F90"
     end if

      end program ESMF_InternGridCreateEx
