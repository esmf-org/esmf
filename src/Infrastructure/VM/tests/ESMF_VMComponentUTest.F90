! $Id: ESMF_VMComponentUTest.F90,v 1.2 2007/02/27 04:52:29 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
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
  
  public mygcomp_register
    
  contains !--------------------------------------------------------------------

  subroutine mygcomp_register(gcomp, rc)
    ! arguments
    type(ESMF_GridComp), intent(inout):: gcomp
    integer, intent(out):: rc
    
    print *, '*** hi from mygcomp_register ***'
    
    ! register INIT method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, mygcomp_init, &
      ESMF_SINGLEPHASE, rc)
    ! register RUN method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, mygcomp_run, &
      ESMF_SINGLEPHASE, rc)
    ! register FINAL method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, mygcomp_final, &
      ESMF_SINGLEPHASE, rc)

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for 
    ! your own code development you probably don't want to include the 
    ! following call unless you are interested in exploring ESMF's 
    ! threading features.
    !call ESMF_GridCompSetVMMinThreads(gcomp, rc=rc)
    ! TODO: Many systems are not able to run this test in ESMF-threaded mode
    ! because it will spawn 1000 concurrent Pthreads. This is *not* an ESMF
    ! problem but a system issue that originates from the stacklimit being 
    ! too small as to allow 1000 additional threads within the same VAS.
    ! On some systems the default stacklimit can be set to unlimited in which
    ! case this test _will_ run, but there are some systems out there where
    ! the admin has set the hardlimit of the stacksize too small as to allow
    ! this test to run successful in ESMF-threaded mode.
    ! Alternatively one could lower the number of components created in this
    ! test to get below the typical stacksize limit, but the whole point of
    ! this test is to stress test ESMF's VM/Component implementation.
#endif

  end subroutine !--------------------------------------------------------------
  
  recursive subroutine mygcomp_init(gcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_GridComp), intent(inout):: gcomp
    type(ESMF_State), intent(in):: istate, estate
    type(ESMF_Clock), intent(in):: clock
    integer, intent(out):: rc

    ! local variables
    type(ESMF_VM):: vm
    
    print *, '*** hi from mygcomp_init ***'
    
    ! get this component's vm    
    call ESMF_GridCompGet(gcomp, vm=vm)

    call ESMF_VMPrint(vm, rc)
    
    rc = 0
  end subroutine !--------------------------------------------------------------
  
  recursive subroutine mygcomp_run(gcomp, istate, estate, clock, rc)
    ! like mygcomp_init...
    ! arguments
    type(ESMF_GridComp), intent(inout):: gcomp
    type(ESMF_State), intent(in):: istate, estate
    type(ESMF_Clock), intent(in):: clock
    integer, intent(out):: rc
    
    ! local variables
    type(ESMF_VM):: vm

    print *, '*** hi from mygcomp_run ***'
    
    ! get this component's vm    
    call ESMF_GridCompGet(gcomp, vm=vm)

    call ESMF_VMPrint(vm, rc)
    
    rc = 0
  end subroutine !--------------------------------------------------------------

  recursive subroutine mygcomp_final(gcomp, istate, estate, clock, rc)
    ! like mygcomp_init...
    ! arguments
    type(ESMF_GridComp), intent(inout):: gcomp
    type(ESMF_State), intent(in):: istate, estate
    type(ESMF_Clock), intent(in):: clock
    integer, intent(out):: rc

    ! local variables
    type(ESMF_VM):: vm

    print *, '*** hi from mygcomp_final ***'

    ! get this component's vm    
    call ESMF_GridCompGet(gcomp, vm=vm)

    call ESMF_VMPrint(vm, rc)
    
    rc = 0
  end subroutine !--------------------------------------------------------------

end module

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

program ESMF_VMComponentUTest

!------------------------------------------------------------------------------
 
#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_VMBarrierUTest - Unit test for VM Barrier Function
!
! !DESCRIPTION:
!
! The code in this file drives the F90 VM Barrier test.  The VM
!   Barrier function is complex enough to require a separate test file.
!   It runs on multiple processors.
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
    '$Id: ESMF_VMComponentUTest.F90,v 1.2 2007/02/27 04:52:29 theurich Exp $'
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
  
  loop_rc=ESMF_SUCCESS 

  do j=1, 20
    do i=1, 1000

      gcomp(i) = ESMF_GridCompCreate(name='My gridded component', rc=loop_rc)
      if (loop_rc /= ESMF_SUCCESS) goto 10
  
      call ESMF_GridCompSetServices(gcomp(i), mygcomp_register, loop_rc)
      if (loop_rc /= ESMF_SUCCESS) goto 10

    enddo
    do i=1, 1000

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

  call ESMF_TestEnd(result, ESMF_SRCLINE)
  
  continue

end program ESMF_VMComponentUTest
