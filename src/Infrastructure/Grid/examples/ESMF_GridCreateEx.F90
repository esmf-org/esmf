!
! Example/test code which creates a new grid.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! See the following code fragments for examples of how to create new Grids.  
! Also see the Programming Model section of this document.
!
!
!\begin{verbatim}

!     ! Example program showing various ways to create a Grid object
      program ESMF_GridCreateEx

      ! ESMF Framework module
      use ESMF_Mod
    
      implicit none
    
!     ! Local variables
      integer :: status, rc
      integer :: delist(4)
      integer :: horz_gridtype, horz_stagger
      integer, dimension(2) :: counts
      real(ESMF_KIND_R8), dimension(2) :: min, max
      type(ESMF_CoordSystem) :: horz_coord_system
      type(ESMF_DELayout) :: layout
      type(ESMF_Grid) :: grid
      character (len = ESMF_MAXSTR) :: name
        
      call ESMF_Initialize(rc)
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
      delist = (/ 0, 1, 2, 3 /)
      layout = ESMF_DELayoutCreate(delist, 2, (/ 2, 2 /), (/ 0, 0 /), rc=status)

      grid = ESMF_GridCreate(numDims=2, counts=counts, min=min, max=max, &
                             layout=layout, horz_gridtype=horz_gridtype, &
                             horz_stagger=horz_stagger, &
                             horz_coord_system=horz_coord_system, &
                             name=name, rc=status)

      print *, "Grid example 1 returned"

      call ESMF_GridDestroy(grid, status)

      print *, "Grid example 1 destroyed"

      call ESMF_Finalize(rc)

      end program ESMF_GridCreateEx
    
!\end{verbatim}
    
