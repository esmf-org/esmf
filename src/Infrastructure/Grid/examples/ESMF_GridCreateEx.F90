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

!   ! Example program showing various ways to create a Grid object
    program ESMF_GridCreateEx

    ! ESMF Framework module
    use ESMF_Mod
    
    implicit none
    
!   ! Local variables
    integer :: i_max, j_max
    integer :: delist(4)
    integer :: horz_gridtype, vert_gridtype
    integer :: horz_stagger, vert_stagger
    integer :: horz_coord_system, vert_coord_system
    integer :: status
    real :: x_min, x_max, y_min, y_max
    type(ESMF_Grid) :: grid
    type(ESMF_DELayout) :: layout
    character (len = ESMF_MAXSTR) :: name
        
!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  The user creates a simple horizontal Grid internally by passing all
!   !  necessary information through the CreateInternal argument list.

      i_max = 10
      j_max = 12
      horz_gridtype = ESMF_GridType_XY
      vert_gridtype = ESMF_GridType_Unknown
      horz_stagger = ESMF_GridStagger_A
      vert_stagger = ESMF_GridStagger_Unknown
      horz_coord_system = ESMF_CoordSystem_Cartesian
      vert_coord_system = ESMF_CoordSystem_Unknown
      x_min = 0.0
      x_max = 10.0
      y_min = 0.0
      y_max = 12.0
      name = "test grid 1"
 
      ! Create a 2 x 2 layout for the Grid
      delist = (/ 0, 1, 2, 3 /)
      layout = ESMF_DELayoutCreate(delist, 2, (/ 2, 2 /), (/ 0, 0 /), rc=status)

      grid = ESMF_GridCreate(i_max=i_max, j_max=j_max, &
                             layout=layout, &
                             horz_gridtype=horz_gridtype, &
                             vert_gridtype=vert_gridtype, &
                             horz_stagger=horz_stagger, &
                             vert_stagger=vert_stagger, &
                             horz_coord_system=horz_coord_system, &
                             vert_coord_system=vert_coord_system, &
                             x_min=x_min, x_max=x_max, &
                             y_min=y_min, y_max=y_max, &
                             name=name, rc=status)

      print *, "Grid example 1 returned"

     call ESMF_GridDestroy(grid, status)

     print *, "Grid example 1 destroyed"

     end program ESMF_GridCreateEx
    
!\end{verbatim}
    
