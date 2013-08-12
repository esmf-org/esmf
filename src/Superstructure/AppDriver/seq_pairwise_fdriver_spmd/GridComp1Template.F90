! $Id$
!
! Template code for a child Grid Component, which has no subcomponents
!  below it.  This is where the bulk of the computation is expected to be
!  placed.  There are three phases where code is run:  Initialization,
!  the Run routine, and Finalization.  Init and Finalize are expected to
!  be called only once.  Run may be called once or many times from the
!  higher level components in the application.   Data which is needed from
!  other components or is produced here for other components must be
!  placed in an ESMF State object.


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  A Template for a User-Written Gridded Component.
!
!
!\begin{verbatim}

    module UserGridComp1Mod
    
    ! ESMF Framework module
    use ESMF
    
    implicit none
    private
    
    public UserGrid_SetServices

    contains

    subroutine UserGrid_SetServices(gcomp, rc)
       type(ESMF_GridComp) :: gcomp
       integer, intent(out) :: rc

       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, my_init, rc=rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, my_run, rc=rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, my_final, rc=rc)

    end subroutine UserGrid_SetServices


    subroutine my_init(gcomp, importState, exportState, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: externalclock
      integer, intent(out) :: rc
     
      call ESMF_LogWrite("User initialize routine called", ESMF_LOGMSG_INFO)
      rc = ESMF_SUCCESS

    end subroutine my_init


    subroutine my_run(gcomp, importState, exportState, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: externalclock
      integer, intent(out) :: rc
     
      call ESMF_LogWrite("User run routine called", ESMF_LOGMSG_INFO)
      rc = ESMF_SUCCESS

    end subroutine my_run


    subroutine my_final(gcomp, importState, exportState, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: externalclock
      integer, intent(out) :: rc
     
      call ESMF_LogWrite("User finalize routine called", ESMF_LOGMSG_INFO)
      rc = ESMF_SUCCESS

    end subroutine my_final

    end module UserGridComp1Mod

!\end{verbatim}
    
