!
! Example/test code which creates a new grid.

!-------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! See the following code fragments for examples of how to create new Grids.  
! Also see the Programming Model section of this document.
!
!
!EOP
!BOC

!     ! Example program showing various ways to create a Grid object
      program ESMF_GridCreateEx

      ! ESMF Framework module
      use ESMF_Mod
    
      implicit none
    
!     ! Local variables
      integer :: status, rc
      integer :: delist(4)
      type(ESMF_GridType) :: horz_gridtype
      type(ESMF_GridStagger) :: horz_stagger
      integer, dimension(2) :: counts
      real(ESMF_KIND_R8), dimension(2) :: min, max
      type(ESMF_CoordSystem) :: horz_coord_system
      type(ESMF_newDELayout) :: layout
      type(ESMF_Grid) :: grid
      type(ESMF_VM) :: vm
      character (len = ESMF_MAXSTR) :: name
!EOC

      integer :: finalrc
      finalrc = ESMF_SUCCESS

!BOC
      call ESMF_Initialize(rc=rc)
!EOC
      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_VMGetGlobal(vm, rc)
!EOC
      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    
!BOC
!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  The user creates a simple horizontal Grid internally by passing all
!   !  necessary information through the CreateInternal argument list.

      counts(1) = 10
      counts(2) = 12
      horz_gridtype = ESMF_GridType_XY
      horz_stagger = ESMF_GridStagger_A
      horz_coord_system = ESMF_CoordSystem_Cartesian
      min(1) = 0.0
      max(1) = 10.0
      min(2) = 0.0
      max(2) = 12.0
      name = "test grid 1"
 
      ! Create a 2 x 2 layout for the Grid
      !delist = (/ 0, 1, 2, 3 /)
      !layout = ESMF_newDELayoutCreate(delist, 2, (/ 2, 2 /), (/ 0, 0 /), rc=status)
      layout = ESMF_newDELayoutCreate(vm, (/ 2, 2 /), rc=rc)
!EOC
 
      if (status.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      grid = ESMF_GridCreateLogRectUniform(dimCount=2, counts=counts, &
                              minGlobalCoordPerDim=min, &
                              maxGlobalCoordPerDim=max, &
                              delayout=layout, &
                              horzGridType=horz_gridtype, &
                              horzStagger=horz_stagger, &
                              horzCoordSystem=horz_coord_system, &
                              name=name, rc=status)
!EOC
 
      if (status.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Grid example 1 returned"

      call ESMF_GridDestroy(grid, rc)

      print *, "Grid example 1 destroyed"
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      call ESMF_Finalize(rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

     if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_GridCreateEx.F90"
     else
        print *, "FAIL: ESMF_GridCreateEx.F90"
     end if

!BOC
      end program ESMF_GridCreateEx
!EOC   
