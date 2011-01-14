!-------------------------------------------------------------------------
!ESMF_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test AttributeCIM.
!
!\begin{verbatim}

program ESMF_AttributeCIMSTest
#define ESMF_METHOD "program ESMF_AttributeCIMSTest"

#include "ESMF.h"

  ! ESMF Framework module
  use ESMF_Mod
  use ESMF_TestMod

  use user_model1, only : userm1_setvm, userm1_register
  use user_model2, only : userm2_setvm, userm2_register
  use user_coupler, only : usercpl_setvm, usercpl_register

  implicit none

  ! Local variables
  integer :: localPet, petCount, localrc, rc, userrc
  character(len=ESMF_MAXSTR) :: cname1, cname2, cplname
  type(ESMF_VM):: vm
  type(ESMF_State) :: c1imp, c1exp, c2imp, c2exp
  type(ESMF_GridComp) :: comp1
  type(ESMF_GridComp) :: comp2
  type(ESMF_CplComp) :: cplcomp
  character(ESMF_MAXSTR) :: conv, purp, attrVal

  ! Cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! Individual test name
  character(ESMF_MAXSTR) :: testname

  ! Individual test failure message, and final status msg
  character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "System Test failure"
  write(testname, *) "System Test ESMF_AttributeCIMSTest"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

  ! Initialize framework and get back default global VM
  call ESMF_Initialize(vm=vm, defaultlogfilename="AttributeCIMSTest.Log", &
                        defaultlogtype=ESMF_LOG_MULTI, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  ! Get number of PETs we are running with
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  if (localPet .eq. 0) then
      print *, "--------------------------------------- "
      print *, "Start of ", trim(testname)
      print *, "--------------------------------------- "
  endif

  if (petCount .lt. 6) then
    ! Create the 2 model components and coupler on a single PET
    cname1 = "user model 1"
    ! use petList to define comp1 on PET 0
    comp1 = ESMF_GridCompCreate(name=cname1, petList=(/0/), rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

    cname2 = "user model 2"
    ! use petList to define comp2 on PET 0
    comp2 = ESMF_GridCompCreate(name=cname2, petList=(/0/), rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

    cplname = "user coupler"
    ! use petList to define cplcomp on PET 0
    cplcomp = ESMF_CplCompCreate(name=cplname, petList=(/0/), rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  else
    ! Create the 2 model components and coupler on 6 PETs
    cname1 = "user model 1"
    ! use petList to define comp1 on PETs 0,1,2
    comp1 = ESMF_GridCompCreate(name=cname1, petList=(/0,1,2/), rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

    cname2 = "user model 2"
    ! use petList to define comp2 on PETs 3,4,5
    comp2 = ESMF_GridCompCreate(name=cname2, petList=(/3,4,5/), rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

    cplname = "user coupler"
    ! default petList defines cplcomp on all PETs 0,1,2,3,4,5
    cplcomp = ESMF_CplCompCreate(name=cplname, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  endif

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  ! Gridded Component 1 register
  call ESMF_GridCompSetVM(comp1, userRoutine=userm1_setvm, userRc=userrc, &
    rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridCompSetServices(comp1, userRoutine=userm1_register, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Gridded Component 2 register
  call ESMF_GridCompSetVM(comp2, userRoutine=userm2_setvm, userRc=userrc, &
    rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridCompSetServices(comp2, userRoutine=userm2_register, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Coupler Component register
  call ESMF_CplCompSetVM(cplcomp, userRoutine=usercpl_setvm, userRc=userrc, &
    rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_CplCompSetServices(cplcomp, userRoutine=usercpl_register, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

! print *, '-------------------------------Initialize----------------------------------'

  ! Gridded Component 1 initialize
  c1imp = ESMF_StateCreate(stateName="Comp1 importState",  &
                           stateType=ESMF_STATE_IMPORT, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  c1exp = ESMF_StateCreate(stateName="Comp1 exportState",  &
                           stateType=ESMF_STATE_EXPORT, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_GridCompInitialize(comp1, importState=c1imp, exportState=c1exp, &
    userRc=userrc, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Gridded Component 2 initialize
  c2imp = ESMF_StateCreate(stateName="Comp2 importState",  &
                           stateType=ESMF_STATE_IMPORT, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  c2exp = ESMF_StateCreate(stateName="Comp2 exportState",  &
                           stateType=ESMF_STATE_EXPORT, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_GridCompInitialize(comp2, importState=c2imp, exportState=c2exp, &
    userRc=userrc, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Coupler Component initialize, comp1 -> comp2
  call ESMF_CplCompInitialize(cplcomp, importState=c1exp, exportState=c2imp, &
    userRc=userrc, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! reconcile comp2's export State to get comp2's Field Attributes onto
  ! PETs 0,1,2, from PETs 3,4,5, since Coupler is setup one way,
  ! comp1 -> comp2, rather than vice versa, comp2 -> comp1. So cannot put in
  ! Coupler initialize, must put here in driver
  call ESMF_StateReconcile(c2exp, vm, ESMF_ATTRECONCILE_ON, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  ! link the Coupler Component Attribute hierarchy to the Gridded Components'
  ! (must occur here in driver, rather than coupler initialize, since driver has
  ! access to cplcomp, comp1 and comp2)
  call ESMF_AttributeLink(cplcomp, comp1, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_AttributeLink(cplcomp, comp2, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  ! link comp1's and comp2's Attribute hierarchies to their respective
  ! export States (must occur here in driver, rather than component initialize,
  ! or coupler initialize, since driver has access to both comps and their
  ! export states, and to create the links on all PETs)
  call ESMF_AttributeLink(comp1, c1exp, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_AttributeLink(comp2, c2exp, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  if (petCount .eq. 6) then
    ! call AttributeUpdate to get a VM wide view of the
    ! metadata set on comp1 in comp1initialize
    call ESMF_AttributeUpdate(comp1, vm, rootList=(/0,1,2/), rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

    ! now call AttributeUpdate to get a VM wide view of the
    ! metadata set on comp2 in comp1initialize
    call ESMF_AttributeUpdate(comp2, vm, rootList=(/3,4,5/), rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  !else
    ! uni-processor mode -- ESMF_AttributeUpdate() not necessary
  endif

  conv = 'CIM 1.0'
  purp = 'Model Component Simulation Description'
#if 1
  ! write the Attribute info in CIM XML format for the Coupler, both Components,
  ! and their Fields
  if (localPet .eq. 0) then
    call ESMF_AttributeWrite(cplcomp, conv, purp, &
                             attwriteflag=ESMF_ATTWRITE_XML, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) then
      if (rc .ne. ESMF_RC_LIB_NOT_PRESENT) then
        call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
      endif
    endif
  endif
#endif
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

! print *, '----------------------------------Run--------------------------------------'

  ! run comp1
  call ESMF_GridCompRun(comp1, importState=c1imp, exportState=c1exp, &
    userRc=userrc, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

#if 0
  if (petCount .eq. 6) then
    call ESMF_AttributeUpdate(comp1, vm, rootList=(/0,1,2/), rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

    call ESMF_AttributeGet(comp1, name="IndividualName", value=attrVal, &
      convention=conv, purpose=purp, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    print *, "IndividualName = ", attrVal
  endif
#endif

  ! run the coupler
  call ESMF_CplCompRun(cplcomp, importState=c1exp, exportState=c2imp, &
    userRc=userrc, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! run comp2
  call ESMF_GridCompRun(comp2, importState=c2imp, exportState=c2exp, &
    userRc=userrc, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

! print *, '--------------------------------Finalize------------------------------------'

  ! finalize comp1
  call ESMF_GridCompFinalize(comp1, importState=c1imp, exportState=c1exp, &
    userRc=userrc, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! finalize comp2
  call ESMF_GridCompFinalize(comp2, importState=c2imp, exportState=c2exp, &
    userRc=userrc, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! finalize the coupler
  call ESMF_CplCompFinalize(cplcomp, importState=c1exp, exportState=c2imp, &
    userRc=userrc, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

! print *, '---------------------------------Destroy------------------------------------'

  ! destroy components
  call ESMF_GridCompDestroy(comp1, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_GridCompDestroy(comp2, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_CplCompDestroy(cplcomp, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  ! destroy states
  call ESMF_StateDestroy(c1imp, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_StateDestroy(c1exp, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_StateDestroy(c2imp, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  call ESMF_StateDestroy(c2exp, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  ! Normal ESMF Test output
  if (localPet .eq. 0) then
      print *, "--------------------------------------- "
      print *, "End of ", trim(testname)
      print *, "--------------------------------------- "
  endif

  if (rc .eq. ESMF_SUCCESS) then
    ! Separate message to console, for quick confirmation of success/failure
    write(finalMsg, *) "SUCCESS: ",trim(testname)," finished correctly."
    write(0, *) ""
    write(0, *) trim(testname)
    write(0, *) trim(finalMsg)
    write(0, *) ""
  endif

!  print *, "------------------------------------------------------------"
!  print *, "------------------------------------------------------------"
!  print *, "Test finished, localPet = ", localPet
!  print *, "------------------------------------------------------------"
!  print *, "------------------------------------------------------------"


  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors
  ! into the Log file that the scripts grep for.
  call ESMF_STest((rc.eq.ESMF_SUCCESS), testname, failMsg, result, &
  __FILE__, &
  __LINE__)

  call ESMF_Finalize()

end program ESMF_AttributeCIMSTest

!\end{verbatim}
