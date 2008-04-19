! $Id: ESMF_DirectCouplingSTest.F90,v 1.1.2.4 2008/04/19 01:40:48 svasquez Exp $
!
!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!-------------------------------------------------------------------------
!
! !DESCRIPTION:
! System test DirectCoupling.  
!
!     Component hierarchy:
!
!                Driver (PET 0,1,2,3,4,5)
!                   |
!         --------------------------------------------------
!         |                |                               |
!     ioComp (PET 0)      cplComp (PET 0,1,2,3,4,5)    modelComp (PET 1,2,3,4,5)
!                                                          |
!                                                -------------------
!                                                |                 |
!                                modelAComp (PET 1,4)     modelBComp (PET 2,3,5)
!
!
!     Direct coupling stucture: 
!                               ioComp
!                                /  ^ 
!                               /    \
!                              v      \
!                     modelAComp ---> modelBComp
!
!
!     The ioComp runs on a single PET and during Initialize() creates a 2D
!     source Array "ioComp.arraySrc" as well as a destination array 
!     "ioComp.arrayDst" on a 100x150 DistGrid with a single DE. The modelAComp
!     creates a 100x150 Array "modelA.array" with 2 DEs across its 2 PETs during
!     Initialize(). Finally modelBComp also creates a 100x150 Array 
!     "modelB.array" with 3 DEs across the the PETs it is defined on.
!
!     The cplComp has a 2 phase Initialize(). During the first phase an
!     ArrayRegrid() between "ioComp.arraySrc" and "modelA.array" is precomputed.
!     In the second phase an ArrayRegrid() between "modelB.array" and 
!     "ioComp.arrayDst" is precomputed. The associated RouteHandles are added
!     to the respective States and thus become accessible to ioComp, modelAComp
!     and modelBComp.
!
!     For setting up of the coupling between modelAComp and modelBComp no extra 
!     coupler component is used, instead the ArrayRedist() between
!     "modelA.array" and "modelB.array" is precomputed during modelComp 
!     Initialize(). Again the RouteHandle is added to the appropriate States.
!
!     After all Initialize() methods have executed the ioComp, modelAComp and
!     modelBComp are set up for direct coupling. The Run() section on the 
!     main driver level starts up ioComp and modelComp concurrently. The
!     modelComp Run() branches out into concurrent exection of modelAComp and
!     modelBComp. Once running ioComp, modelAComp and modelBComp do not return
!     to an upper level to exchange data but call directly into the precomputed
!     ArrayRedist() methods.
!
!     In order to test the correctness of the application the "ioComp.arraySrc"
!     is initialized to
!
!       10.0 + 5.0*sin((I/Imax)*pi) + 2.0*sin((J/Jmax)*pi)
!
!     where I = [1,..,Imax=100] and J = [1,..,Jmax=150]. The "ioCOmp.arrayDst"
!     is initialized to 0.
!
!     During each iteration a full coupling cycle is completed:
!     * ioComp sends "ioComp.arraySrc" data to modelAComp via ArrayRedist()
!     * modelAComp receives data from ioComp via ArrayRedist() into 
!       "modelA.array"
!     * modelAComp sends "modelA.array" data to modelBComp via ArrayRedist(),
!       which was precomputed with factor = -2
!     * modelBComp receives modelAComp data via ArrayRedist() into 
!       "modelB.array" which is -2 x "modelA.array" data
!     * modelBComp sends "modelB.array" data to ioComp via ArrayRedist()
!     * ioComp receives modelBComp data into "ioComp.arrayDst" via ArrayRedist()
!     * ioComp copies "ioComp.arrayDst" data into "ioComp.arraySrc" for the next
!       iteration.
!
!     The above cycle is repeated for 3 iterations. The final result in
!     "ioComp.arrayDst" is then -8 x the initial value of "ioComp.arraySrc".
!     The ioComp Run() method validates the results before returning to the 
!     main driver.
!
!     As an added feature the Arrays in ioComp are of type/kind R4 while
!     modelAComp and modelBComp Arrays are of type/kind R8. The ArrayRedist()
!     method automatically handles typecasting of the exchanged data.
!
!     The Finalize() methods are responsible for destroying all of the 
!     explicitly created objects in the Component context. RouteHandles must
!     be released in the Component that precomputed them. All of the
!     objects created by ESMF_StateReconcile() will be automatically destroyed 
!     when the State is destroyed.
!
!-------------------------------------------------------------------------
!\begin{verbatim}

program ESMF_DirectCouplingSTest
#define ESMF_METHOD "program ESMF_DirectCouplingSTest"

#include "ESMF.h"

  ! ESMF Framework module
  use ESMF_Mod
  use ESMF_TestMod

  ! Application components
  use ioCompMod,     only : ioCompReg
  use modelCompMod,  only : modelCompReg
  use cplCompMod,    only : cplCompReg

  implicit none
    
  ! Local variables
  integer :: localPet, petCount, localrc, rc=ESMF_SUCCESS
  type(ESMF_VM):: vm
  type(ESMF_GridComp) :: ioComp, modelComp
  type(ESMF_CplComp) :: cplComp
  type(ESMF_State) :: ioExp, ioImp, modelExp, modelImp

  ! Test variables
  integer :: testresult = 0     ! all pass
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "System Test failure"
  write(testname, *) "System Test ESMF_DirectCouplingTest"

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
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  ! Get number of PETs and local PET this driver is running on
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Create Components
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  ! ioComp on PET 0
  ioComp = ESMF_GridCompCreate(name="ioComp", petList=(/0/), rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  ! modelComp on PET 1,2,3,4,5
  modelComp = ESMF_GridCompCreate(name="modelComp", petList=(/1,2,3,4,5/), &
    rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  ! cplComp on all PETs
  cplComp = ESMF_CplCompCreate(name="cplComp", rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! SetServices for Components
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompSetServices(ioComp, ioCompReg, localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_GridCompSetServices(modelComp, modelCompReg, localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_CplCompSetServices(cplComp, cplCompReg, localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Create States and initialize Components
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
  ! ioComp
  ioImp = ESMF_StateCreate("ioComp import", ESMF_STATE_IMPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  ioExp = ESMF_StateCreate("ioComp export", ESMF_STATE_EXPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_GridCompInitialize(ioComp, importState=ioImp, exportState=ioExp, &
    rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
 
  ! modelComp
  modelImp = ESMF_StateCreate("modelComp import", ESMF_STATE_IMPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  modelExp = ESMF_StateCreate("modelComp export", ESMF_STATE_EXPORT, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_GridCompInitialize(modelComp, importState=modelImp, &
    exportState=modelExp, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
 
  ! ioComp->modelComp coupling
  call ESMF_CplCompInitialize(cplComp, importState=ioExp, &
    exportState=modelImp, phase=1, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  ! modelComp->ioComp coupling
  call ESMF_CplCompInitialize(cplComp, importState=modelExp, &
    exportState=ioImp, phase=2, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Run io and model Components concurrently -> direct coupling between Components
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  ! ioComp
  call ESMF_GridCompRun(ioComp, importState=ioImp, exportState=ioExp, &
    rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  ! modelComp
  call ESMF_GridCompRun(modelComp, importState=modelImp, exportState=modelExp, &
    rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Finalize Components
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  ! ioComp->modelComp coupling
  call ESMF_CplCompFinalize(cplComp, importState=ioExp, &
    exportState=modelImp, phase=1, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  ! modelComp->ioComp coupling
  call ESMF_CplCompFinalize(cplComp, importState=modelExp, &
    exportState=ioImp, phase=2, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  ! ioComp
  call ESMF_GridCompFinalize(ioComp, importState=ioImp, exportState=ioExp, &
    rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  ! modelComp
  call ESMF_GridCompFinalize(modelComp, importState=modelImp, &
    exportState=modelExp, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Destroy Components and States
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompDestroy(ioComp, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_GridCompDestroy(modelComp, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_CplCompDestroy(cplComp, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_StateDestroy(ioExp, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    
  call ESMF_StateDestroy(ioImp, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    
  call ESMF_StateDestroy(modelExp, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    
  call ESMF_StateDestroy(modelImp, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    
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

end program ESMF_DirectCouplingSTest
    
!\end{verbatim}
