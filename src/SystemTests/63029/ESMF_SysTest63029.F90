! $Id: ESMF_SysTest63029.F90,v 1.5 2003/03/10 03:23:14 cdeluca Exp $
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
    use ESMF_DELayoutMod
    use ESMF_ArrayMod
    use ESMF_GridMod
    use ESMF_DataMapMod
    use ESMF_FieldMod
    use ESMF_CompMod
    
    use user_model

    implicit none
    
!   Local variables
    integer, dimension(4) :: delist
    integer :: de_id, rc
    character(len=ESMF_MAXSTR) :: cname
    type(ESMF_DELayout) :: layout1 
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

!   Create a DELayout for the Component
    delist = (/ 0, 1, 2, 3 /)
    layout1 = ESMF_DELayoutCreate(2, 2, delist, ESMF_XFAST, rc)

    cname = "System Test #63029"
    comp1 = ESMF_CompCreate(cname, layout=layout1, ctype=ESMF_GRIDCOMP, &
                      mtype=ESMF_ATM, filepath="/usr/local", rc=rc)

!   Figure out our local processor id
    call ESMF_DELayoutGetDEId(layout1, de_id, rc)

    call ESMF_CompPrint(comp1)

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
      call ESMF_DELayoutDestroy(layout1, rc)
      print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      print *, "System Test #63029 complete!"

      end program ESMF_SysTest63029
    
!\end{verbatim}
    
