! $Id: CouplerComponentTemplate.F90,v 1.2 2003/11/06 23:56:46 nscollins Exp $
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
    
!   ! ESMF Framework module
    use ESMF_Mod
    
    implicit none
    private
    
    public UserCpl_SetServices

    contains

    subroutine UserCpl_SetServices(ccomp, rc)
       type(ESMF_CplComp) :: ccomp
       integer :: rc

       call ESMF_CplCompSetEntryPoint(ccomp, ESMF_SETINIT, my_init, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_CplCompSetEntryPoint(ccomp, ESMF_SETRUN, my_run, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_CplCompSetEntryPoint(ccomp, ESMF_SETFINAL, my_final, &
                                                     ESMF_SINGLEPHASE, rc)

    end subroutine UserCpl_SetServices


    subroutine my_init(ccomp, statelist, externalclock, rc)
      type(ESMF_CplComp) :: ccomp
      type(ESMF_State) :: statelist
      type(ESMF_Clock) :: externalclock
      integer :: rc
     
      type(ESMF_State) :: state1, state2
      call ESMF_StateGetData(statelist,  "statename1", state1, rc)
      call ESMF_StateGetData(statelist,  "statename2", state2, rc)

    end subroutine my_init


    subroutine my_run(ccomp, statelist, externalclock, rc)
      type(ESMF_CplComp) :: ccomp
      type(ESMF_State) :: statelist
      type(ESMF_Clock) :: externalclock
      integer :: rc
     

    end subroutine my_run


    subroutine my_final(ccomp, statelist, externalclock, rc)
      type(ESMF_CplComp) :: ccomp
      type(ESMF_State) :: statelist
      type(ESMF_Clock) :: externalclock
      integer :: rc
     

    end subroutine my_final


    end module UserCplCompMod
    
!\end{verbatim}
    
