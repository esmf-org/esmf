! $Id: ESMF_SysTest63029.F90,v 1.2 2003/02/27 23:03:27 nscollins Exp $
!
! System test code #63029

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test number 63029.
!
!
!\begin{verbatim}

    program ESMF_SysTest63029

#include "ESMF.h"

!   ! Modules needed
!   TODO: (these will be collapsed into a single ESMF_Mod soon)
    use ESMF_BaseMod
    use ESMF_IOMod
    use ESMF_LayoutMod
    use ESMF_ArrayMod
    use ESMF_GridMod
    use ESMF_DataMapMod
    use ESMF_FieldMod
    use ESMF_CompMod
    
    use user_model

    implicit none
    
!   Local variables
    integer :: nx, ny, i, j, ni, nj, rc
    integer, dimension(04) :: delist
    integer :: timestep
    integer :: de_id
    integer :: i_max, j_max
    integer :: horz_gridtype, vert_gridtype, halo_width
    integer :: horz_stagger, vert_stagger
    integer :: horz_coord_system, vert_coord_system
    integer :: status
    real :: x_min, x_max, y_min, y_max
    integer(ESMF_IKIND_I4), dimension(:,:), pointer :: idata, idata2, &
                                                       ldata
    character(len=ESMF_MAXSTR) :: cname, gname, fname
    type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: index
    type(ESMF_Layout) :: layout1 
    type(ESMF_Grid) :: grid1
    type(ESMF_Array) :: array1, array2
    type(ESMF_Field) :: field1
    type(ESMF_Comp) :: comp1
        
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "System Test #63029:"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

!   Create a Layout for the Component
!   delist = (/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 /)
    delist = (/ 0, 1, 2, 3 /)
    layout1 = ESMF_LayoutCreate(2, 2, delist, ESMF_XFAST, rc)

    cname = "System Test #63029"
    comp1 = ESMF_CompCreate(cname, layout=layout1, ctype=ESMF_GRIDCOMP, &
                      mtype=ESMF_ATM, filepath="/usr/local", rc=rc)

!   Figure out our local processor id
    call ESMF_LayoutGetDEId(layout1, de_id, rc)

    print *, "Comp Create finished, name = ", trim(cname)


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      call ESMF_CompRegister(comp1, user_register, rc)
      print *, "Comp Register finished, rc= ", rc

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
      call ESMF_CompInitialize(comp1, rc)
      print *, "Comp Initialize finished"
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      call ESMF_CompRun(comp1, rc)
      print *, "Comp Run returned first time"

      call ESMF_CompRun(comp1, rc)
      print *, "Comp Run returned second time"
 
      call ESMF_CompRun(comp1, rc)
      print *, "Comp Run returned third time"
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Print result

      call ESMF_CompFinalize(comp1, rc)

      print *, "-----------------------------------------------------------------"
      print *, "-----------------------------------------------------------------"
      print *, "Test finished, de_id = ", de_id
      print *, "-----------------------------------------------------------------"
      print *, "-----------------------------------------------------------------"

      print *, "Comp Finalize returned"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Clean up

      call ESMF_CompDestroy(comp1, rc)
      call ESMF_LayoutDestroy(layout1, rc)
      print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      print *, "System Test #63029 complete!"

      end program ESMF_SysTest63029
    
!\end{verbatim}
    
