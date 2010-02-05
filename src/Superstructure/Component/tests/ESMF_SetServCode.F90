! $Id: ESMF_SetServCode.F90,v 1.16.2.1 2010/02/05 20:04:30 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
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
    
    public SetVM
    public SetServ1, SetServ2

contains

    subroutine SetVM(gcomp, rc)
       type(ESMF_GridComp) :: gcomp
       integer, intent(out) :: rc
#ifdef ESMF_TESTWITHTHREADS
       type(ESMF_VM) :: vm
       logical :: pthreadsEnabled
#endif
       ! Initialize return code
       rc = ESMF_SUCCESS
#ifdef ESMF_TESTWITHTHREADS
       ! The following call will turn on ESMF-threading (single threaded)
       ! for this component. If you are using this file as a template for
       ! your own code development you probably don't want to include the
       ! following call unless you are interested in exploring ESMF's
       ! threading features.

       ! First test whether ESMF-threading is supported on this machine
       call ESMF_VMGetGlobal(vm, rc=rc)
       call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
       if (pthreadsEnabled) then
         call ESMF_GridCompSetVMMinThreads(gcomp, rc=rc)
       endif
#endif
    end subroutine SetVM


    subroutine SetServ1(gcomp, rc)
       type(ESMF_GridComp) :: gcomp
       integer, intent(out) :: rc
       
       ! Initialize return code
       rc = ESMF_SUCCESS

       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, userRoutine=my_init1, &
         rc=rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, userRoutine=my_run1, &
         rc=rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, userRoutine=my_final1, &
         rc=rc)
                                                     
    end subroutine SetServ1


    subroutine SetServ2(gcomp, rc)
       type(ESMF_GridComp) :: gcomp
       integer, intent(out) :: rc

       ! Initialize return code
       rc = ESMF_SUCCESS

       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, userRoutine=my_init2, &
         rc=rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, userRoutine=my_run2, &
         rc=rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, userRoutine=my_final2, &
         rc=rc)

    end subroutine SetServ2


    subroutine my_init1(gcomp, importState, exportState, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: externalclock
      integer, intent(out) :: rc
      type(ESMF_Method):: currentMethod
      integer          :: currentPhase
     
      print *, "User initialize 1 routine called"
      
      ! set user return code according to correctness of method and phase test

      call ESMF_GridCompGet(gcomp, currentMethod=currentMethod, &
        currentPhase=currentPhase)
        
      if (currentMethod/=ESMF_SETINIT) then
        rc = ESMF_FAILURE
        return
      endif

      if (currentPhase/=1) then
        rc = ESMF_FAILURE
        return
      endif

      ! return successfully
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

      ! test returning a specific user return code      
      rc = 123456

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
    
