!-------------------------------------------------------------------------
!ESMF_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test Attribute.  
!     
!\begin{verbatim}

program ESMF_AttributeSTest
#define ESMF_METHOD "program ESMF_AttributeSTest"

#include "ESMF.h"
#include <ESMF_Macros.inc>

  ! ESMF Framework module
  use ESMF_Mod
  use ESMF_TestMod

  use user_model1, only : userm1_register
  use user_model2, only : userm2_register
  use user_coupler, only : usercpl_register

  implicit none
      
  ! Local variables
  integer :: localPet, petCount, rc
  character(len=ESMF_MAXSTR) :: cname1, cname2, cplname
  type(ESMF_VM):: vm
  type(ESMF_State) :: c1imp, c1exp, c2imp, c2exp, MyState, physics
  type(ESMF_GridComp) :: comp1
  type(ESMF_GridComp) :: comp2
  type(ESMF_CplComp) :: cplcomp
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: testresult = 0

  ! individual test name
  character(ESMF_MAXSTR) :: testname

  ! individual test failure message, and final status msg
  character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "System Test failure"
  write(testname, *) "System Test ESMF_AttributeSTest"

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
  call ESMF_Initialize(vm=vm, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  ! Get number of PETs we are running with
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  if (petCount .lt. 6) then
    ! Create the 2 model components and coupler
    cname1 = "user model 1"
    ! use petList to define comp1 on PET 0,1,2,3
    comp1 = ESMF_GridCompCreate(name=cname1, petList=(/0/), rc=rc)
    !print  *, "Created component ", trim(cname1), "rc =", rc
    if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    cname2 = "user model 2"
    ! use petList to define comp1 on PET 0,1,2,3
    comp2 = ESMF_GridCompCreate(name=cname2, petList=(/0/), rc=rc)
    !print  *, "Created component ", trim(cname1), "rc =", rc
    if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    cplname = "user coupler"
    ! use petList to define comp1 on PET 0,1,2,3
    cplcomp = ESMF_CplCompCreate(name=cplname, petList=(/0/), rc=rc)
    !print  *, "Created component ", trim(cname1), "rc =", rc
    if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  else
    ! Create the 2 model components and coupler
    cname1 = "user model 1"
    ! use petList to define comp1 on PET 0,1,2,3
    comp1 = ESMF_GridCompCreate(name=cname1, petList=(/0,1,2,3,4,5/), rc=rc)
    !print  *, "Created component ", trim(cname1), "rc =", rc
    if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    cname2 = "user model 2"
    ! use petList to define comp1 on PET 0,1,2,3
    comp2 = ESMF_GridCompCreate(name=cname2, petList=(/0,1,2,3,4,5/), rc=rc)
    !print  *, "Created component ", trim(cname1), "rc =", rc
    if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    cplname = "user coupler"
    ! use petList to define comp1 on PET 0,1,2,3
    cplcomp = ESMF_CplCompCreate(name=cplname, petList=(/0,1,2,3,4,5/), rc=rc)
    !print  *, "Created component ", trim(cname1), "rc =", rc
    if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  
  endif

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompSetServices(comp1, userm1_register, rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_GridCompSetServices(comp2, userm2_register, rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_CplCompSetServices(cplcomp, usercpl_register, rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
! print *, '-------------------------------Initialize----------------------------------'
 
  c1imp = ESMF_StateCreate("Comp1 importState", ESMF_STATE_IMPORT, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  c1exp = ESMF_StateCreate("Comp1 exportState", ESMF_STATE_EXPORT, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_GridCompInitialize(comp1, importState=c1imp, exportState=c1exp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
   
  c2imp = ESMF_StateCreate("Comp2 importState", ESMF_STATE_IMPORT, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  c2exp = ESMF_StateCreate("Comp2 exportState", ESMF_STATE_EXPORT, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_GridCompInitialize(comp2, importState=c2imp, exportState=c2exp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_CplCompInitialize(cplcomp, importState=c1exp, exportState=c2imp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

! print *, '----------------------------------Run--------------------------------------'

  call ESMF_GridCompRun(comp1, importState=c1imp, exportState=c1exp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_CplCompRun(cplcomp, importState=c1exp, exportState=c2imp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_GridCompRun(comp2, importState=c2imp, exportState=c2exp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

! print *, '--------------------------------Finalize------------------------------------'

  call ESMF_GridCompFinalize(comp1, importState=c1imp, exportState=c1exp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_GridCompFinalize(comp2, importState=c2imp, exportState=c2exp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_CplCompFinalize(cplcomp, importState=c1exp, exportState=c2imp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

! print *, '---------------------------------Destroy------------------------------------'

  call ESMF_StateDestroy(c1imp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_StateDestroy(c1exp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_StateDestroy(c2imp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_StateDestroy(c2exp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_GridCompDestroy(comp1, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_GridCompDestroy(comp2, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_CplCompDestroy(cplcomp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

10 continue
  
  ! Normal ESMF Test output
  print *, testname, " complete."

  call ESMF_TestGlobal((rc.eq.ESMF_SUCCESS), testname, failMsg, testresult, &
    ESMF_SRCLINE)

  if ((localPet .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then
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

end program ESMF_AttributeSTest
    
!\end{verbatim}
    
