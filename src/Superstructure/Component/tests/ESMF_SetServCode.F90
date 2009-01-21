! $Id: ESMF_SetServCode.F90,v 1.7.2.4 2009/01/21 21:25:24 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      module SetServCode

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !MODULE: SetServCode - Supporting code for ESMF_CompSetServUTest
!
! !DESCRIPTION:
!   Test replacing an already registered service routine with another
!   and actually having it take effect.
!
!-------------------------------------------------------------------------
!
! !USES:

    use ESMF_Mod
    implicit none
    
    public SetServ1, SetServ2

contains

    subroutine SetServ1(gcomp, rc)
       type(ESMF_GridComp) :: gcomp
       integer, intent(out) :: rc

       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, my_init1, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, my_run1, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, my_final1, &
                                                     ESMF_SINGLEPHASE, rc)

    end subroutine SetServ1

    subroutine SetServ2(gcomp, rc)
       type(ESMF_GridComp) :: gcomp
       integer, intent(out) :: rc

       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, my_init2, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, my_run2, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, my_final2, &
                                                     ESMF_SINGLEPHASE, rc)

    end subroutine SetServ2


    subroutine my_init1(gcomp, importState, exportState, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: externalclock
      integer, intent(out) :: rc
     
      print *, "User initialize 1 routine called"
      rc = ESMF_SUCCESS

    end subroutine my_init1


    subroutine my_run1(gcomp, importState, exportState, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: externalclock
      integer, intent(out) :: rc
     
      print *, "User run routine 1 called"
      rc = ESMF_SUCCESS

    end subroutine my_run1


    subroutine my_final1(gcomp, importState, exportState, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: externalclock
      integer, intent(out) :: rc
     
      print *, "User finalize 1 routine called"
      rc = ESMF_SUCCESS

    end subroutine my_final1


    subroutine my_init2(gcomp, importState, exportState, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: externalclock
      integer, intent(out) :: rc
     
      print *, "User initialize 2 routine called"
      rc = ESMF_SUCCESS

    end subroutine my_init2


    subroutine my_run2(gcomp, importState, exportState, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: externalclock
      integer, intent(out) :: rc
     
      print *, "User run routine 2 called"
      rc = ESMF_SUCCESS

    end subroutine my_run2


    subroutine my_final2(gcomp, importState, exportState, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: externalclock
      integer, intent(out) :: rc
     
      print *, "User finalize 2 routine called"
      rc = ESMF_SUCCESS

    end subroutine my_final2


    
    end module
    
