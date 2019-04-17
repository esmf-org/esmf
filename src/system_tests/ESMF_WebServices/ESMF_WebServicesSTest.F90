! $Id$
!
!-------------------------------------------------------------------------
!ESMF_disable_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!-------------------------------------------------------------------------
!
! !DESCRIPTION:
! System test WebServices.
!    Component Service startup.
!
!-------------------------------------------------------------------------
!\begin{verbatim}

module ESMF_WebServUserModel

  ! ESMF Framework module
  use ESMF
  use ESMF_TestMod
  use ESMF_IO_NetCDFMod

  implicit none
    
  public ESMF_WebServUserModelRegister
        
  contains


  !-------------------------------------------------------------------------
  !   !  The Register routine sets the subroutines to be called
  !   !   as the init, run, and finalize routines.  Note that these are
  !   !   private to the module.

  subroutine ESMF_WebServUserModelRegister(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Register starting"

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, &
                                    userRoutine=user_init, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, &
                                    userRoutine=user_run, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, &
                                    userRoutine=user_final, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "Registered Initialize, Run, and Finalize routines"
    print *, "User Comp1 Register returning"
    
  end subroutine

  !-------------------------------------------------------------------------
  !   !  User Comp Component created by higher level calls, here is the
  !   !   Initialization routine.
    
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: comp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)         :: vm
    integer               :: petCount

    ! local test variables
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
    integer                :: result = 0

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Init starting: "

    print *, "User Comp1 Init returning: "

  end subroutine user_init


  !-------------------------------------------------------------------------
  !   !  The Run routine where data is computed.
  !   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: comp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! Local variables
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Run starting"

    print *, "User Comp1 Run returning"

  end subroutine user_run


  !-------------------------------------------------------------------------
  !   !  The Finalization routine where things are deleted and cleaned up.
  !   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: comp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! Local variables
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Final starting"

    ! call ESMF_StatePrint(exportState)

    print *, "User Comp1 Final returning"

  end subroutine user_final


end module ESMF_WebServUserModel


program ESMF_WebServicesSTest
#define ESMF_METHOD "program ESMF_WebServicesSTest"

#include "ESMF.h"

  ! ESMF Framework module
  use ESMF
  use ESMF_TestMod
  use ESMF_WebServMod
  use ESMF_WebServUserModel

  implicit none

  ! Local variables
  integer                    :: rc = ESMF_SUCCESS
  character(len=ESMF_MAXSTR) :: cname1
  type(ESMF_VM)              :: vm
  type(ESMF_GridComp)        :: comp1
  integer                    :: portNum
  integer                    :: localrc

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test name
  character(ESMF_MAXSTR) :: testname

  ! individual test failure message, and final status msg
  character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(testname, *) "System Test ESMF_WebServicesSTest"
  write(failMsg, *) "System Test failure"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  print *, "--------------------------------------- "
  print *, "Start of ", trim(testname)
  print *, "--------------------------------------- "

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
  ! Initialize framework and get back default global VM
  call ESMF_Initialize(vm=vm, defaultlogfilename="WebServicesSTest.Log", &
                        logkindflag=ESMF_LOGKIND_MULTI, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_LogSet (flush=.true.)

  ! Create the model component
  cname1 = "user model 1"
  comp1 = ESMF_GridCompCreate(name=cname1, rc=localrc)
  print *, "Created component ", trim(cname1), "rc =", localrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  print *, "Comp Create finished"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompSetServices(comp1, &
    userRoutine=ESMF_WebServUserModelRegister, rc=localrc)
  print *, "Comp1 SetServices finished, rc= ", localrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Start Loop section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  rc = ESMF_SUCCESS
  portNum = 27060
  call ESMF_WebServicesLoop(comp1, portNum, rc=rc)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompDestroy(comp1, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

10 continue

  ! Normal ESMF Test output
  print *, testname, " complete."

  if (rc .eq. ESMF_SUCCESS) then
    ! Separate message to console, for quick confirmation of success/failure
    write(finalMsg, *) "SUCCESS: ",trim(testname)," finished correctly."
    write(0, *) ""
    write(0, *) trim(testname)
    write(0, *) trim(finalMsg)
    write(0, *) ""
  endif

  print *, "------------------------------------------------------------"
  print *, "------------------------------------------------------------"
!  print *, "Test finished, localPet = ", localPet
  print *, "Test finished, rc =  ", rc
  print *, "------------------------------------------------------------"
  print *, "------------------------------------------------------------"

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((rc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize()

end program ESMF_WebServicesSTest

!\end{verbatim}
