! $Id: ESMF_GridCreateEx.F90,v 1.27 2005/01/05 17:06:55 jwolfe Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
      program ESMF_GridCreateEx

!-------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_GridCreateEx - Grid creation
!
! !DESCRIPTION:
!
! This program shows examples of different methods to create 2D and 3D grids
!-----------------------------------------------------------------------------

      ! ESMF Framework module
      use ESMF_Mod
      implicit none
    
      ! instantiate two grids
      type(ESMF_Grid) :: grid1, grid2

      ! instantiate horizontal and vertical grid staggerings
      type(ESMF_GridHorzStagger) :: horz_stagger
      type(ESMF_GridVertStagger) :: vert_stagger

      ! local variables for Create routines
      integer :: counts(2), countsPerDE1(2), countsPerDE2(2)
      character(len=ESMF_MAXSTR) :: name
      real(ESMF_KIND_R8), dimension(2) :: min, max
      real(ESMF_KIND_R8) :: delta1(40), delta2(50), delta3(10)

      ! return code
      integer :: rc
!EOC

      ! Local variables
      type(ESMF_DELayout) :: layout
      type(ESMF_VM) :: vm

      integer :: finalrc
      finalrc = ESMF_SUCCESS

!BOC
      ! initialize ESMF framework
      call ESMF_Initialize(rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
      call ESMF_VMGetGlobal(vm, rc)
      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Uniform 2D Grid Creation}

! This example shows how to create a simple uniform horizontal {\tt ESMF\_Grid}.
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

      ! set the staggering for the horizontal grid
      horz_stagger = ESMF_GRID_HORZ_STAGGER_A

      ! and add a name to the grid
      name         = "test grid 1"
 
      ! create a 2 x 2 layout for the Grid
      layout = ESMF_DELayoutCreate(vm, (/ 2, 2 /), rc=rc)
!EOC
 
      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize a simple uniform horizontal grid with the above values
      grid1 = ESMF_GridCreateHorzXYUni(counts=counts, &
                                       minGlobalCoordPerDim=min, &
                                       maxGlobalCoordPerDim=max, &
                                       horzstagger=horz_stagger, &
                                       name=name, rc=rc)

      ! distribute the grid
      call ESMF_GridDistribute(grid1, delayout=layout, rc=rc)

!EOC
 
      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Grid example 1 returned"

      call ESMF_GridDestroy(grid1, rc)

      print *, "Grid example 1 destroyed"
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{3D Grid Creation}

! This example shows how to create a 3D {\tt ESMF\_Grid} with specified,
! non-uniform spacing.
!EOE

!BOC
      ! set the global coordinate minima for the horizontal grid
      ! note: the vertical grid does not need a coordinate minimum
      !       because the specific call to GridAddVertHeight infers
      !       a minimum at 0.0.
      min(1)       = 0.0
      min(2)       = 0.0

      ! set up arrays of coordinate spacing for the horizontal grid
      delta1 = (/ 1.0, 1.0, 1.0, 1.1, 1.1, 1.1, 1.2, 1.2, 1.3, 1.4, &
                  1.4, 1.5, 1.6, 1.6, 1.6, 1.8, 1.8, 1.7, 1.7, 1.6, &
                  1.6, 1.6, 1.8, 1.8, 2.0, 2.0, 2.2, 2.2, 2.2, 2.2, &
                  2.0, 1.7, 1.5, 1.3, 1.2, 1.1, 1.0, 1.0, 1.0, 0.9 /)
      delta2 = (/ 0.8, 0.8, 0.8, 0.8, 0.8, 0.7, 0.7, 0.6, 0.7, 0.8, &
                  0.9, 0.9, 0.9, 0.9, 1.0, 1.0, 1.0, 1.0, 0.9, 1.0, &
                  1.0, 1.0, 1.0, 1.1, 1.2, 1.3, 1.3, 1.3, 1.4, 1.4, &
                  1.4, 1.4, 1.4, 1.4, 1.4, 1.3, 1.3, 1.3, 1.2, 1.2, &
                  1.1, 1.0, 1.0, 0.9, 0.8, 0.7, 0.6, 0.6, 0.5, 0.5 /)

      ! set array of coordinate spacing for the vertical grid
      delta3 = (/ 1.0, 1.0, 1.0, 0.5, 0.5, 0.6, 0.8, 1.0, 1.0, 1.0 /)

      ! set the staggerings for the horizontal and vertical grids
      horz_stagger = ESMF_GRID_HORZ_STAGGER_D_NE
      vert_stagger = ESMF_GRID_VERT_STAGGER_CENTER

      ! and add a name to the grid
      name         = "test grid 2"

      ! set specified number of computational cells per DE for each
      ! decomposition direction
      countsPerDE1 = (/ 26, 14 /)
      countsPerDE2 = (/ 22, 28 /)
!EOC
 
      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize the grid with the above values
      grid2 = ESMF_GridCreateHorzLatLon(minGlobalCoordPerDim=min, &
                                        delta1=delta1, delta2=delta2, &
                                        horzstagger=horz_stagger, &
                                        name=name, rc=rc)

      ! add a vertical subgrid to the horizontal grid
      ! note: the vertical subgrid must be added before the grid is
      !      distributed
      call ESMF_GridAddVertHeight(grid2, delta3, vertstagger=vert_stagger, &
                                  rc=rc)
!EOC
 
      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC

      ! distribute the grid using the same layout as from the first example
      ! but specifying the decomposition of computational cells
      call ESMF_GridDistribute(grid2, delayout=layout, &
                               countsPerDEDim1=countsPerDE1, &
                               countsPerDEDim2=countsPerDE2, &
                               rc=rc)

!EOC
 
      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

      print *, "Grid example 2 returned"

      call ESMF_GridDestroy(grid2, rc)

      print *, "Grid example 2 destroyed"

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

      call ESMF_Finalize(rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

     if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_GridCreateEx.F90"
     else
        print *, "FAIL: ESMF_GridCreateEx.F90"
     end if

      end program ESMF_GridCreateEx
