! $Id: ESMF_FaultToleranceSTest.F90,v 1.1 2011/12/01 18:58:25 theurich Exp $
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
  integer :: localPet, petCount, userrc, localrc, rc, i
  type(ESMF_VM):: vm

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

  ! Get number of PETs we are running with
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  if (localPet == petCount-1) then
    call c_ESMCI_vmkSocketClient(localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  endif
  
  ! test MPI function after call into socket code
  do i=1, 10
    call ESMF_VMBarrier(vm)
    print *, "returned from user VMBarrier()"
!    call MPI_Barrier(MPI_COMM_WORLD, localrc)
!    print *, "returned from MPI_Barrier()"
    call ESMF_VMWTimeDelay(1.d0)
  enddo  
  

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
