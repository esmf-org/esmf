! $Id: ESMF_RecursiveComponentSTest.F90,v 1.1.2.2 2008/02/28 01:42:04 theurich Exp $
!
!-------------------------------------------------------------------------
!ESMF_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!-------------------------------------------------------------------------
!
! !DESCRIPTION:
! System test RecursiveComponent.  
!
!     Component hierarchy:
!
!                        Driver (PET 0,1,2,3,4,5)
!                          |
!                      component (PET 0,1,2,3,4,5)
!                          |
!       +---------------------------------------+
!       |                                       |
!   component (PET 0,1,2)                   component (PET 3,4,5)
!       |                                       |
! +------------------------+              +------------------------+
! component (Pet 0)  component (PET 1,2)  component (Pet 3)  component (PET 4,5)
!-------------------------------------------------------------------------
!\begin{verbatim}

program ESMF_RecursiveComponentSTest
#define ESMF_METHOD "program ESMF_RecursiveComponentSTest"

#include "ESMF.h"

  ! ESMF Framework module
  use ESMF_Mod
  use ESMF_TestMod

  ! Application components
  use componentMod,  only : componentReg

  implicit none
    
  ! Local variables
  integer :: localPet, petCount, localrc, rc=ESMF_SUCCESS
  type(ESMF_VM):: vm
  type(ESMF_GridComp) :: component
  type(ESMF_State) :: import, export

  ! Test variables
  integer :: testresult = 0     ! all pass
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "System Test failure"
  write(testname, *) "System Test ESMF_RecursiveComponentTest"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  print *, "--------------------------------------- "
  print *, "Start of ", trim(testname)
  print *, "--------------------------------------- "

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Startup ESMF
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  ! Initialize framework and get back default global VM
  call ESMF_Initialize(vm=vm, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Get number of PETs and local PET this driver is running on
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  if (petCount .lt. 6) then
    print *, "This system test needs to run at least 6-way, petCount = ", &
      petCount
    goto 10 ! bail out
  endif

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Create Component
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  ! component on all PETs, i.e. 0,1,2,3,4,5
  component = ESMF_GridCompCreate(name="component012345", rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! SetServices for Component
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompSetServices(component, componentReg, localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Create States and initialize Component
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
  import = ESMF_StateCreate("import", ESMF_STATE_IMPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  export = ESMF_StateCreate("export", ESMF_STATE_EXPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_GridCompInitialize(component, importState=import, &
    exportState=export, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Run component
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompRun(component, importState=import, &
    exportState=export, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Finalize Components
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompFinalize(component, importState=import, &
    exportState=export, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Destroy Component and States
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompDestroy(component, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_StateDestroy(import, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
    
  call ESMF_StateDestroy(export, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

10 continue

  ! Normal ESMF Test output
  print *, testname, " complete."

  ! IMPORTANT: TestGlobal() prints the PASS: string that the scripts grep for.
  call ESMF_TestGlobal((rc.eq.ESMF_SUCCESS), testname, failMsg, testresult, &
    ESMF_SRCLINE)

  if ((localPet .eq. 0) .and. (rc .eq. ESMF_SUCCESS)) then
    ! Separate message to console, for quick confirmation of success/failure
    write(finalMsg, *) "SUCCESS: ",trim(testname)," finished correctly."
    write(0, *) ""
    write(0, *) trim(testname)
    write(0, *) trim(finalMsg)
    write(0, *) ""
  endif
  
  print *, "------------------------------------------------------------"
  print *, "------------------------------------------------------------"
  print *, "Test finished, localPet = ", localPet
  print *, "------------------------------------------------------------"
  print *, "------------------------------------------------------------"

  call ESMF_Finalize()

end program ESMF_RecursiveComponentSTest
    
!\end{verbatim}
