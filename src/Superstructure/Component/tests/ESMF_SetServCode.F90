! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
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

  use ESMF
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
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    if (pthreadsEnabled) then
      call ESMF_GridCompSetVMMinThreads(gcomp, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
    endif
#endif
  end subroutine SetVM


  subroutine SetServ0(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc
       
    ! Initialize return code
    rc = ESMF_SUCCESS

  end subroutine SetServ0


  subroutine SetServ1(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc
       
    ! Initialize return code
    rc = ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=my_init1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
      
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=my_run1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
      
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=my_final1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
                                                     
  end subroutine SetServ1


  subroutine SetServ2(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=my_init2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
      
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=my_run2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
      
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=my_final2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine SetServ2


  subroutine my_init1(gcomp, importState, exportState, externalclock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: externalclock
    integer, intent(out)  :: rc

    ! local variables
    type(ESMF_Method_Flag)  :: currentMethod
    integer                 :: currentPhase
     
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User initialize 1 routine called"
      
    ! set user return code according to correctness of method and phase test

    call ESMF_GridCompGet(gcomp, currentMethod=currentMethod, &
      currentPhase=currentPhase, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
        
    if (currentMethod/=ESMF_METHOD_INITIALIZE) then
      rc = ESMF_FAILURE
      return  ! bail out
    endif

    if (currentPhase/=1) then
      rc = ESMF_FAILURE
      return  ! bail out
    endif

  end subroutine my_init1


  subroutine my_run1(gcomp, importState, exportState, externalclock, rc)
    type(ESMF_GridComp) :: gcomp
    type(ESMF_State) :: importState
    type(ESMF_State) :: exportState
    type(ESMF_Clock) :: externalclock
    integer, intent(out) :: rc
   
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User run routine 1 called"

  end subroutine my_run1


  subroutine my_final1(gcomp, importState, exportState, externalclock, rc)
    type(ESMF_GridComp) :: gcomp
    type(ESMF_State) :: importState
    type(ESMF_State) :: exportState
    type(ESMF_Clock) :: externalclock
    integer, intent(out) :: rc
   
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User finalize 1 routine called"

  end subroutine my_final1


  subroutine my_init2(gcomp, importState, exportState, externalclock, rc)
    type(ESMF_GridComp) :: gcomp
    type(ESMF_State) :: importState
    type(ESMF_State) :: exportState
    type(ESMF_Clock) :: externalclock
    integer, intent(out) :: rc
   
    ! Initialize return code
    rc = ESMF_SUCCESS

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
   
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User run routine 2 called"

  end subroutine my_run2


  subroutine my_final2(gcomp, importState, exportState, externalclock, rc)
    type(ESMF_GridComp) :: gcomp
    type(ESMF_State) :: importState
    type(ESMF_State) :: exportState
    type(ESMF_Clock) :: externalclock
    integer, intent(out) :: rc
   
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User finalize 2 routine called"

  end subroutine my_final2

end module
    
