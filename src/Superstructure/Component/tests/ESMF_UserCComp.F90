! $Id: ESMF_UserCComp.F90,v 1.1 2003/04/01 23:51:19 nscollins Exp $
!
! Test code which supplies a user-written coupler component.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  A skeletal user-written component for testing framework.
!
!
!\begin{verbatim}

    module UserCplCompMod
    
!   ! Some common definitions.  This requires the C preprocessor.
#include "ESMF.h"

!   ! Other ESMF modules which are needed by Comps
    use ESMF_BaseMod
    use ESMF_DELayoutMod
    use ESMF_ClockMod
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

    subroutine User_SetServices(ccomp, rc)
       type(ESMF_CplComp) :: ccomp
       integer :: rc
       type(mydata), pointer :: privatedata
       type(datawrapper) :: wrapper

       call ESMF_CplCompSetEntryPoint(ccomp, ESMF_SETINIT, my_init, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_CplCompSetEntryPoint(ccomp, ESMF_SETRUN, my_run, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_CplCompSetEntryPoint(ccomp, ESMF_SETFINAL, my_final, &
                                                     ESMF_SINGLEPHASE, rc)

       allocate(privatedata)
       wrapper%wrap => privatedata

       call ESMF_CplCompSetInternalState(ccomp, wrapper, rc)

    end subroutine User_SetServices


    subroutine my_init(ccomp, statelist, externalclock, &
                                         compname, layout, config, rc)
      type(ESMF_CplComp) :: ccomp
      type(ESMF_State) :: statelist(:)
      type(ESMF_Clock) :: externalclock
      character(len=*) :: compname
      type(ESMF_DELayout) :: layout
      type(ESMF_Config) :: config
      integer :: rc
     

    end subroutine my_init


    subroutine my_run(ccomp, statelist, externalclock, &
                                         compname, layout, config, rc)
      type(ESMF_CplComp) :: ccomp
      type(ESMF_State) :: statelist(:)
      type(ESMF_Clock) :: externalclock
      character(len=*) :: compname
      type(ESMF_DELayout) :: layout
      type(ESMF_Config) :: config
      integer :: rc
     

    end subroutine my_run


    subroutine my_final(ccomp, statelist, externalclock, &
                                         compname, layout, config, rc)
      type(ESMF_CplComp) :: ccomp
      type(ESMF_State) :: statelist(:)
      type(ESMF_Clock) :: externalclock
      character(len=*) :: compname
      type(ESMF_DELayout) :: layout
      type(ESMF_Config) :: config
      integer :: rc
     

    end subroutine my_final


    end module UserCplCompMod
    
!\end{verbatim}
    
