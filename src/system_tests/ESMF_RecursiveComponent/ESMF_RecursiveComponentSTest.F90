! $Id: ESMF_RecursiveComponentSTest.F90,v 1.1.2.4 2008/04/18 23:12:53 svasquez Exp $
!
!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!-------------------------------------------------------------------------
!
! !DESCRIPTION:
! System test RecursiveComponent.  
!
!   A single component is created by the driver code and its SetServices() 
!   routine is invoked. Consequently the component's Initialize(), Run(), 
!   Finalize() methods are called in sequence. The component Initialize()
!   method creates two recursive subcomponents by invoking its own 
!   SetServices() routine on two exclusive sets of PETs. It then calls the
!   Initialize() routine for these two subcomponents which results in two more
!   recursive subcomponents for each call. The recursion stops at a recursion
!   level of 2. After the Initialize() method has returned through the recursion
!   stack for all subcomponents, the driver calls into the component's Run()
!   method which again starts a recursive call tree into the second level
!   before it returns. Finally the Finalize() call recursively destroys all
!   subcomponents and returns to the driver code.
!
!   Recursive component hierarchy:
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
