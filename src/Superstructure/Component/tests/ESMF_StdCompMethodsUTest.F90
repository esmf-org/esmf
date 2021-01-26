! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================


module ESMF_StdCompMethods_mod

  ! modules
  use ESMF
  
  implicit none
  
  private
  
  public gcomp_setvm, gcomp_register
  public cplcomp_setvm1, cplcomp_setvm2, cplcomp_register
    
  contains !--------------------------------------------------------------------

  subroutine gcomp_setvm(gcomp, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
#endif
    
    ! Initialize
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

  end subroutine !--------------------------------------------------------------

  subroutine gcomp_register(gcomp, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
    
    ! Initialize
    rc = ESMF_SUCCESS

    ! register INIT method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=gcomp_init, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! register RUN method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=gcomp_run, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! register FINAL method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=gcomp_final, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! register WRITERESTART method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_WRITERESTART, &
      userRoutine=gcomp_writerestart, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! register READESTART method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_READRESTART, &
      userRoutine=gcomp_readrestart, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine !--------------------------------------------------------------
  
  recursive subroutine gcomp_init(gcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc

    ! Initialize
    rc = ESMF_SUCCESS

  end subroutine !--------------------------------------------------------------
  
  recursive subroutine gcomp_run(gcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc
    
    ! Initialize
    rc = ESMF_SUCCESS

  end subroutine !--------------------------------------------------------------

  recursive subroutine gcomp_final(gcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc

    ! Initialize
    rc = ESMF_SUCCESS

  end subroutine !--------------------------------------------------------------

  recursive subroutine gcomp_writerestart(gcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc

    ! Initialize
    rc = ESMF_SUCCESS

  end subroutine !--------------------------------------------------------------

  recursive subroutine gcomp_readrestart(cplcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_GridComp):: cplcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc

    ! Initialize
    rc = ESMF_SUCCESS

  end subroutine !--------------------------------------------------------------


  subroutine cplcomp_setvm1(cplcomp, rc)
    ! arguments
    type(ESMF_CplComp):: cplcomp
    integer, intent(out):: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
#endif
    
    ! Initialize
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
      call ESMF_CplCompSetVMMaxThreads(cplcomp, maxPetCountPerVas=1, rc=rc)
    endif
#endif

  end subroutine !--------------------------------------------------------------

  subroutine cplcomp_setvm2(cplcomp, rc)
    ! arguments
    type(ESMF_CplComp):: cplcomp
    integer, intent(out):: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
#endif
    
    ! Initialize
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
      call ESMF_CplCompSetVMMaxPEs(cplcomp, maxPeCountPerPet=1, rc=rc)
    endif
#endif

  end subroutine !--------------------------------------------------------------

  subroutine cplcomp_register(cplcomp, rc)
    ! arguments
    type(ESMF_CplComp):: cplcomp
    integer, intent(out):: rc
    
    ! Initialize
    rc = ESMF_SUCCESS

    ! register INIT method
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=cplcomp_init, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! register RUN method
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_RUN, &
      userRoutine=cplcomp_run, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! register FINAL method
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=cplcomp_final, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! register WRITERESTART method
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_WRITERESTART, &
      userRoutine=cplcomp_writerestart, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! register READESTART method
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_READRESTART, &
      userRoutine=cplcomp_readrestart, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine !--------------------------------------------------------------
  
  recursive subroutine cplcomp_init(cplcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_CplComp):: cplcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc

    ! Initialize
    rc = ESMF_SUCCESS

  end subroutine !--------------------------------------------------------------
  
  recursive subroutine cplcomp_run(cplcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_CplComp):: cplcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc
    
    ! Initialize
    rc = ESMF_SUCCESS

  end subroutine !--------------------------------------------------------------

  recursive subroutine cplcomp_final(cplcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_CplComp):: cplcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc

    ! Initialize
    rc = ESMF_SUCCESS

  end subroutine !--------------------------------------------------------------

  recursive subroutine cplcomp_writerestart(cplcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_CplComp):: cplcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc

    ! Initialize
    rc = ESMF_SUCCESS

  end subroutine !--------------------------------------------------------------

  recursive subroutine cplcomp_readrestart(cplcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_CplComp):: cplcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc

    ! Initialize
    rc = ESMF_SUCCESS

  end subroutine !--------------------------------------------------------------


end module

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

program ESMF_StdCompMethodsUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_VMComponentUTest - Unit test for VM/Component interaction
!
! !DESCRIPTION:
!
! The code in this file drives the F90 VM/Component test.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF
  use ESMF_StdCompMethods_mod

  implicit none

!------------------------------------------------------------------------------
  ! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  ! local variables
  integer:: i, j, rc
  type(ESMF_VM):: vm
  type(ESMF_GridComp):: gcomp
  type(ESMF_CplComp):: cplcomp
  
!------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------

  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Get global VM and print info
  call ESMF_VMGetGlobal(vm, rc=rc)
  call ESMF_VMPrint(vm)
  
  
  ! - Gridded Component Tests -----------------------------------------------
  
  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Gridded Component Create() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  gcomp = ESMF_GridCompCreate(name='My gridded component', rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Gridded Component SetVM() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridCompSetVM(gcomp, userRoutine=gcomp_setvm, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Gridded Component SetServices() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridCompSetServices(gcomp, userRoutine=gcomp_register, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Gridded Component Initialize() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridCompInitialize(gcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Gridded Component Initialize() Test - with timeout"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_GridCompInitialize(gcomp, timeout=10, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Gridded Component Run() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridCompRun(gcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Gridded Component Run() Test - with timeout"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_GridCompRun(gcomp, timeout=10, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Gridded Component Finalize() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridCompFinalize(gcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Gridded Component Finalize() Test - with timeout"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_GridCompFinalize(gcomp, timeout=10, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Gridded Component WriteRestart() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridCompWriteRestart(gcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Gridded Component WriteRestart() Test - with timeout"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_GridCompWriteRestart(gcomp, timeout=10, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Gridded Component ReadRestart() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridCompReadRestart(gcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Gridded Component ReadRestart() Test - with timeout"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_GridCompReadRestart(gcomp, timeout=10, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Gridded Component Destroy() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridCompDestroy(gcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      

  ! - Coupler Component Tests w/ cplcomp_setvm1 -----------------------------

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component Create() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  cplcomp = ESMF_CplCompCreate(name='My coupler component', rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component SetVM() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_CplCompSetVM(cplcomp, userRoutine=cplcomp_setvm1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component SetServices() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_CplCompSetServices(cplcomp, userRoutine=cplcomp_register, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component Initialize() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_CplCompInitialize(cplcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component Initialize() Test - with timeout"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_CplCompInitialize(cplcomp, timeout=10, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component Run() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_CplCompRun(cplcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component Run() Test - with timeout"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_CplCompRun(cplcomp, timeout=10, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component Finalize() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_CplCompFinalize(cplcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component Finalize() Test - with timeout"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_CplCompFinalize(cplcomp, timeout=10, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component WriteRestart() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_CplCompWriteRestart(cplcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component WriteRestart() Test - with timeout"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_CplCompWriteRestart(cplcomp, timeout=10, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component ReadRestart() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_CplCompReadRestart(cplcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component ReadRestart() Test - with timeout"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_CplCompReadRestart(cplcomp, timeout=10, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component Destroy() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_CplCompDestroy(cplcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  ! - Coupler Component Tests w/ cplcomp_setvm2 -----------------------------

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component Create() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  cplcomp = ESMF_CplCompCreate(name='My coupler component', rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component SetVM() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_CplCompSetVM(cplcomp, userRoutine=cplcomp_setvm2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component SetServices() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_CplCompSetServices(cplcomp, userRoutine=cplcomp_register, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component Initialize() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_CplCompInitialize(cplcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component Run() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_CplCompRun(cplcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component Finalize() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_CplCompFinalize(cplcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component WriteRestart() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_CplCompWriteRestart(cplcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component ReadRestart() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_CplCompReadRestart(cplcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Coupler Component Destroy() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_CplCompDestroy(cplcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  call ESMF_TestEnd(ESMF_SRCLINE)
  
  continue

end program ESMF_StdCompMethodsUTest
