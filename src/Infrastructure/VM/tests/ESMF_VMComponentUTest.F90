! $Id: ESMF_VMComponentUTest.F90,v 1.26.2.1 2010/02/05 20:02:07 svasquez Exp $
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


module ESMF_VMComponentUTest_gcomp_mod

  ! modules
  use ESMF_Mod
  
  implicit none
  
  private
  
  public mygcomp_setvm, mygcomp_register_nexh, mygcomp_register_exh
    
  contains !--------------------------------------------------------------------

  subroutine mygcomp_setvm(gcomp, rc)
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

  subroutine mygcomp_register_nexh(gcomp, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
    
    ! Initialize
    rc = ESMF_SUCCESS

    ! register INIT method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, userRoutine=mygcomp_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! register RUN method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, userRoutine=mygcomp_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! register FINAL method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, userRoutine=mygcomp_final, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine !--------------------------------------------------------------
  
  subroutine mygcomp_register_exh(gcomp, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
    
    ! Initialize
    rc = ESMF_SUCCESS

    ! register INIT method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, userRoutine=mygcomp_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! register RUN method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, userRoutine=mygcomp_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! register FINAL method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, userRoutine=mygcomp_final, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine !--------------------------------------------------------------
  
  recursive subroutine mygcomp_init(gcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc

    ! Initialize
    rc = ESMF_SUCCESS

  end subroutine !--------------------------------------------------------------
  
  recursive subroutine mygcomp_run(gcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc
    
    ! Initialize
    rc = ESMF_SUCCESS

  end subroutine !--------------------------------------------------------------

  recursive subroutine mygcomp_final(gcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
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

program ESMF_VMComponentUTest

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
  use ESMF_Mod
  use ESMF_VMComponentUTest_gcomp_mod

  implicit none

!------------------------------------------------------------------------------
  ! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_VMComponentUTest.F90,v 1.26.2.1 2010/02/05 20:02:07 svasquez Exp $'
!------------------------------------------------------------------------------
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  ! local variables
  integer:: i, j, rc, loop_rc
  type(ESMF_VM):: vm
  type(ESMF_GridComp):: gcomp(1000)
  
!------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------

  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

  ! Get global VM and print info
  call ESMF_VMGetGlobal(vm, rc=rc)
  call ESMF_VMPrint(vm)
  
  ! Prepare for non-exhaustive test section
  loop_rc=ESMF_SUCCESS 

  do j=1, 2
    do i=1, 100

      gcomp(i) = ESMF_GridCompCreate(name='My gridded component', rc=loop_rc)
      if (loop_rc /= ESMF_SUCCESS) goto 10
  
      call ESMF_GridCompSetVM(gcomp(i), userRoutine=mygcomp_setvm, rc=loop_rc)
      if (loop_rc /= ESMF_SUCCESS) goto 10

      call ESMF_GridCompSetServices(gcomp(i), userRoutine=mygcomp_register_nexh, &
        rc=loop_rc)
      if (loop_rc /= ESMF_SUCCESS) goto 10

    enddo
    do i=1, 100

      call ESMF_GridCompDestroy(gcomp(i), rc=loop_rc)
      if (loop_rc /= ESMF_SUCCESS) goto 10
      
    enddo
  enddo
  
10 continue
  !----------------------------------------------------------------
  !Verify loop test results
  !NEX_UTest
  write(name, *) "Component Create/SetServices/Destroy Test"
  write(failMsg, *) "Failure codes returned!"
  call ESMF_Test((loop_rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


#ifdef ESMF_TESTEXHAUSTIVE

  ! Prepare for exhaustive test section
  loop_rc=ESMF_SUCCESS 

  do j=1, 20
    do i=1, 200

      gcomp(i) = ESMF_GridCompCreate(name='My gridded component', rc=loop_rc)
      if (loop_rc /= ESMF_SUCCESS) goto 20
  
      call ESMF_GridCompSetVM(gcomp(i), userRoutine=mygcomp_setvm, rc=loop_rc)
      if (loop_rc /= ESMF_SUCCESS) goto 10

      call ESMF_GridCompSetServices(gcomp(i), userRoutine=mygcomp_register_exh, &
        rc=loop_rc)
      if (loop_rc /= ESMF_SUCCESS) goto 20

    enddo
    do i=1, 200

      call ESMF_GridCompDestroy(gcomp(i), rc=loop_rc)
      if (loop_rc /= ESMF_SUCCESS) goto 20
      
    enddo
  enddo
  
20 continue
  !----------------------------------------------------------------
  !Verify loop test results
  !EX_UTest
  write(name, *) "Exhaustive Component Create/SetServices/Destroy Test"
  write(failMsg, *) "Failure codes returned!"
  call ESMF_Test((loop_rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif


  call ESMF_TestEnd(result, ESMF_SRCLINE)
  
  continue

end program ESMF_VMComponentUTest
