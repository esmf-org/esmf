! $Id: ParentGridComponentTemplate.F90,v 1.2 2003/11/06 21:05:51 nscollins Exp $
!
! Template code for a Gridded Component which creates 3 child Components:
!  two Gridded Components which perform a computation and a Coupler component
!  which mediates the data exchange between them.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  A template for a user-written Gridded Component which creates 3 child
!  Components:
!  two Gridded Components which perform a computation and a Coupler component
!  which mediates the data exchange between them.
!
!
!\begin{verbatim}

    module UserParentGridCompMod
    
    ! ESMF Framework module
    use ESMF_Mod
    
    implicit none
    private
    
    public UserPComp_SetServices

    contains

    subroutine UserPComp_SetServices(gcomp, rc)
       type(ESMF_GridComp) :: gcomp
       integer :: rc

       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, my_init, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, my_run, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, my_final, &
                                                     ESMF_SINGLEPHASE, rc)

    end subroutine UserPComp_SetServices


    subroutine my_init(gcomp, importstate, exportstate, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importstate
      type(ESMF_State) :: exportstate
      type(ESMF_Clock) :: externalclock
      integer :: rc
     
      print *, "User initialize routine called"

    end subroutine my_init


    subroutine my_run(gcomp, importstate, exportstate, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importstate
      type(ESMF_State) :: exportstate
      type(ESMF_Clock) :: externalclock
      integer :: rc
     
      print *, "User run routine called"

    end subroutine my_run


    subroutine my_final(gcomp, importstate, exportstate, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importstate
      type(ESMF_State) :: exportstate
      type(ESMF_Clock) :: externalclock
      integer :: rc
     
      print *, "User finalize routine called"

    end subroutine my_final

    end module UserParentGridCompMod

!\end{verbatim}
    
