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
    
!   ! Some common definitions.  This requires the C preprocessor.
#include "ESMF.h"

!   ! Other ESMF modules which are needed by Grids
    use ESMF_IOMod
    use ESMF_DataMapMod
    use ESMF_GridMod
    
    implicit none
    
!   ! Local variables
    integer :: i_max, j_max
    integer :: horz_gridtype, vert_gridtype
    integer :: horz_stagger, vert_stagger
    integer :: horz_coord_system, vert_coord_system
    integer :: status
    real :: x_min, x_max, y_min, y_max
    type(ESMF_Grid) :: grid
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
 
      grid = ESMF_GridCreate(i_max, j_max, horz_gridtype, vert_gridtype, &
                             horz_stagger, vert_stagger, &
                             horz_coord_system, vert_coord_system, &
                             x_min, x_max, y_min, y_max, name, status)

      print *, "Grid example 1 returned"

     end program ESMF_GridCreateEx
    
!\end{verbatim}
    
