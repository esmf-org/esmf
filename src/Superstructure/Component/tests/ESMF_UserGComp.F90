! $Id: ESMF_UserGComp.F90,v 1.8 2007/06/23 07:00:54 cdeluca Exp $
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
    
!   ! ESMF Framework module
    use ESMF_Mod
    
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


    subroutine my_init(gcomp, importState, exportState, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: externalclock
      integer :: rc
     
      print *, "User initialize routine called"

    end subroutine my_init


    subroutine my_run(gcomp, importState, exportState, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: externalclock
      integer :: rc
     
      print *, "User run routine called"

    end subroutine my_run


    subroutine my_final(gcomp, importState, exportState, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: externalclock
      integer :: rc
     
      print *, "User finalize routine called"

    end subroutine my_final

    end module UserGridCompMod

!\end{verbatim}
    
