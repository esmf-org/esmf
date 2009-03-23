! $Id: CplCompTemplate.F90,v 1.6 2009/03/23 20:40:47 theurich Exp $
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

       call ESMF_CplCompSetEntryPoint(ccomp, ESMF_SETINIT, my_init, rc=rc)
       call ESMF_CplCompSetEntryPoint(ccomp, ESMF_SETRUN, my_run, rc=rc)
       call ESMF_CplCompSetEntryPoint(ccomp, ESMF_SETFINAL, my_final, rc=rc)

    end subroutine UserCpl_SetServices


    subroutine my_init(ccomp, importstate, exportstate, externalclock, rc)
      type(ESMF_CplComp) :: ccomp
      type(ESMF_State) :: importstate, exportstate
      type(ESMF_Clock) :: externalclock
      integer :: rc
     
      type(ESMF_State) :: state1, state2

      call ESMF_LogWrite("Coupler Initialize routine called", ESMF_LOG_INFO)

      call ESMF_StateGet(importstate,  "GComp1 Import", state1, rc)
      call ESMF_StateGet(importstate,  "GComp2 Import", state2, rc)

      call ESMF_LogWrite("Coupler Initialize routine returning", ESMF_LOG_INFO)
      rc=ESMF_SUCCESS

    end subroutine my_init


    subroutine my_run(ccomp, importstate, exportstate, externalclock, rc)
      type(ESMF_CplComp) :: ccomp
      type(ESMF_State) :: importstate, exportstate
      type(ESMF_Clock) :: externalclock
      integer :: rc
     
      call ESMF_LogWrite("Coupler Run routine called", ESMF_LOG_INFO)

      call ESMF_LogWrite("Coupler Run routine returning", ESMF_LOG_INFO)
      rc=ESMF_SUCCESS

    end subroutine my_run


    subroutine my_final(ccomp, importstate, exportstate, externalclock, rc)
      type(ESMF_CplComp) :: ccomp
      type(ESMF_State) :: importstate, exportstate
      type(ESMF_Clock) :: externalclock
      integer :: rc
     
      call ESMF_LogWrite("Coupler Finalize routine called", ESMF_LOG_INFO)

      call ESMF_LogWrite("Coupler Finalize routine returning", ESMF_LOG_INFO)
      rc=ESMF_SUCCESS

    end subroutine my_final


    end module UserCplCompMod
    
!\end{verbatim}
    
