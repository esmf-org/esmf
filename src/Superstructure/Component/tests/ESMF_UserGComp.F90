! $Id: ESMF_UserGComp.F90,v 1.1 2003/04/01 23:51:19 nscollins Exp $
!
! Test code which supplies a user-written component.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  A skeletal user-written component for testing framework.
!
!
!\begin{verbatim}

    module UserGridCompMod
    
!   ! Some common definitions.  This requires the C preprocessor.
#include "ESMF.h"

!   ! Other ESMF modules which are needed by Comps
    use ESMF_BaseMod
    use ESMF_DELayoutMod
    use ESMF_ClockMod
    use ESMF_GridMod
    use ESMF_StateMod
    use ESMF_CompMod
    
    implicit none
    private
    
    public User_SetServices

    type mydata
        integer :: per_instance_data
    end type

    type datawrapper
        type(mydata), pointer :: wrap
    end type

    contains

    subroutine User_SetServices(gcomp, rc)
       type(ESMF_GridComp) :: gcomp
       integer :: rc
       type(mydata), pointer :: privatedata
       type(datawrapper) :: wrapper

       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, my_init, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, my_run, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, my_final, &
                                                     ESMF_SINGLEPHASE, rc)

       allocate(privatedata)
       wrapper%wrap => privatedata

       call ESMF_GridCompSetInternalState(gcomp, wrapper, rc)

    end subroutine User_SetServices


    subroutine my_init(gcomp, importstate, exportstate, externalclock, &
                                         compname, layout, grid, config, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importstate
      type(ESMF_State) :: exportstate
      type(ESMF_Clock) :: externalclock
      character(len=*) :: compname
      type(ESMF_DELayout) :: layout
      type(ESMF_Grid) :: grid
      type(ESMF_Config) :: config
      integer :: rc
     

    end subroutine my_init


    subroutine my_run(gcomp, importstate, exportstate, externalclock, &
                                         compname, layout, grid, config, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importstate
      type(ESMF_State) :: exportstate
      type(ESMF_Clock) :: externalclock
      character(len=*) :: compname
      type(ESMF_DELayout) :: layout
      type(ESMF_Grid) :: grid
      type(ESMF_Config) :: config
      integer :: rc
     

    end subroutine my_run


    subroutine my_final(gcomp, importstate, exportstate, externalclock, &
                                         compname, layout, grid, config, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importstate
      type(ESMF_State) :: exportstate
      type(ESMF_Clock) :: externalclock
      character(len=*) :: compname
      type(ESMF_DELayout) :: layout
      type(ESMF_Grid) :: grid
      type(ESMF_Config) :: config
      integer :: rc
     

    end subroutine my_final


    end module UserGridCompMod
    
!\end{verbatim}
    
