! $Id$
!
!-------------------------------------------------------------------------
!ESMF_disable_MULTI_PROC_SYSTEM_TEST   String used by test script to count system tests.
!=========================================================================

!-------------------------------------------------------------------------
!
! !DESCRIPTION:
! System test ESMF_FaultToleranceMPMD.
!    Testing fault tolerance implementation
!
!-------------------------------------------------------------------------
!\begin{verbatim}

program ESMF_FaultToleranceSTest
#define ESMF_METHOD "program ESMF_FaultToleranceSTest"

#include "ESMF.h"

  ! ESMF Framework module
  use ESMF
  use ESMF_TestMod

  implicit none

  ! Local variables
  integer             :: localPet, petCount, userRc, localrc, rc, i
  type(ESMF_VM)       :: vm
  type(ESMF_GridComp) :: dualComp

  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg, finalMsg
  
  integer :: result = 0
  
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(testname, *) "System Test ESMF_FaultTolerance STest"
  write(failMsg, *) "System Test failure"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  print *, "--------------------------------------- "
  print *, "Start of ", trim(testname)
  print *, "--------------------------------------- "

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Initialize ESMF
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

  ! Initialize framework and get back default global VM
  call ESMF_Initialize(vm=vm, defaultlogfilename="FaultToleranceSTest.Log", &
                        logkindflag=ESMF_LOGKIND_MULTI, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_LogSet (flush=.true.)

  ! Get number of PETs we are running with
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  ! - Sanity test the socket based connection 
  ! - TODO: remove

!  if (localPet == petCount-1) then
!    call c_ESMCI_vmkSocketClient(localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) &
!      call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
!  endif
  
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  ! - This is the top level access layer to an actual component running
  ! - remotely as an independent executable through a local dual component.
  
  ! - Create the dual component and connect with the actual component by
  ! - calling a special SetServices routine.
  
  dualComp = ESMF_GridCompCreate(name="dual component A", rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(dualComp, port=60000, server="fudge", &
    rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompInitialize(dualComp, userRc=userRc, rc=localrc)
  if (ESMF_LogFoundError(userRc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

10 continue

  ! Normal ESMF Test output
  print *, testname, " complete."

  ! Separate message to console, for quick confirmation of success/failure
  write(finalMsg, *) "SUCCESS: ",trim(testname)," finished correctly."
  write(0, *) ""
  write(0, *) trim(testname)
  write(0, *) trim(finalMsg)
  write(0, *) ""

  print *, "------------------------------------------------------------"
  print *, "------------------------------------------------------------"
  print *, "Test finished, localPet = ", localPet
  print *, "------------------------------------------------------------"
  print *, "------------------------------------------------------------"

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest(.true., testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize()

end program ESMF_FaultToleranceSTest

!\end{verbatim}
