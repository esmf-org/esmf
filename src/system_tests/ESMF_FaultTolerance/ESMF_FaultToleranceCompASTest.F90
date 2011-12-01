! $Id: ESMF_FaultToleranceCompASTest.F90,v 1.1 2011/12/01 18:58:25 theurich Exp $
!
!=========================================================================

!-------------------------------------------------------------------------
!
! !DESCRIPTION:
! System test ESMF_FaultTolerance.
!    Testing fault tolerance implementation Component A
!
!-------------------------------------------------------------------------
!\begin{verbatim}

program ESMF_FaultToleranceCompASTest
#define ESMF_METHOD "program ESMF_FaultToleranceCompASTest"

#include "ESMF.h"

  ! ESMF Framework module
  use ESMF
  use ESMF_TestMod

  implicit none

  ! Local variables
  integer :: localPet, petCount, userrc, localrc, rc
  type(ESMF_VM):: vm

  character(ESMF_MAXSTR) :: testname, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(testname, *) "System Test ESMF_FaultToleranceCompA STest"

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
  call ESMF_Initialize(vm=vm, defaultlogfilename="FaultToleranceCompASTest.Log", &
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
    call c_ESMCI_vmkSocketServer(localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  endif
  
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

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

  call ESMF_Finalize()

end program ESMF_FaultToleranceCompASTest

!\end{verbatim}
