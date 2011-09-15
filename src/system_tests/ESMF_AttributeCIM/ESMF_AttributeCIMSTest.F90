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
  use ESMF
  use ESMF_TestMod

  use user_model1, only : userm1_setvm, userm1_register
  use user_model2, only : userm2_setvm, userm2_register
  use user_model3, only : userm3_setvm, userm3_register
  use user_model4, only : userm4_setvm, userm4_register
  use user_coupler, only : usercpl_setvm, usercpl_register

  implicit none

  ! Local variables
  integer :: localPet, petCount, userrc, localrc, rc=ESMF_SUCCESS
  character(len=ESMF_MAXSTR) :: cname1, cname2, cname3, cname4, cplname
  type(ESMF_VM):: vm
  type(ESMF_State) :: c1imp, c1exp, c2imp, c2exp, c3imp, c3exp, c4imp, c4exp
  type(ESMF_GridComp) :: comp1, comp2, comp3, comp4
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
                        logkindflag=ESMF_LOGKIND_MULTI, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Get number of PETs we are running with
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  if (localPet .eq. 0) then
      print *, "--------------------------------------- "
      print *, "Start of ", trim(testname)
      print *, "--------------------------------------- "
  endif

  if (petCount .lt. 8) then
    ! Create the 4 model components and coupler on a single PET
    cname1 = "user model 1"
    ! use petList to define comp1 on PET 0
    comp1 = ESMF_GridCompCreate(name=cname1, petList=(/0/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    cname2 = "user model 2"
    ! use petList to define comp2 on PET 0
    comp2 = ESMF_GridCompCreate(name=cname2, petList=(/0/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    cname3 = "user model 3"
    ! use petList to define comp3 on PET 0
    comp3 = ESMF_GridCompCreate(name=cname3, petList=(/0/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    cname4 = "user model 4"
    ! use petList to define comp4 on PET 0
    comp4 = ESMF_GridCompCreate(name=cname4, petList=(/0/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    cplname = "user coupler"
    ! use petList to define cplcomp on PET 0
    cplcomp = ESMF_CplCompCreate(name=cplname, petList=(/0/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

  else
    ! Create the 4 model components and coupler on 8 PETs
    cname1 = "user model 1"
    ! use petList to define comp1 on PETs 0,1
    comp1 = ESMF_GridCompCreate(name=cname1, petList=(/0,1/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    cname2 = "user model 2"
    ! use petList to define comp2 on PETs 2,3
    comp2 = ESMF_GridCompCreate(name=cname2, petList=(/2,3/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    cname3 = "user model 3"
    ! use petList to define comp3 on PETs 4,5
    comp3 = ESMF_GridCompCreate(name=cname3, petList=(/4,5/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    cname4 = "user model 4"
    ! use petList to define comp4 on PETs 6,7
    comp4 = ESMF_GridCompCreate(name=cname4, petList=(/6,7/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    cplname = "user coupler"
    ! default petList defines cplcomp on all PETs 0,1,2,3,4,5,6,7
    cplcomp = ESMF_CplCompCreate(name=cplname, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

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
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(comp1, userRoutine=userm1_register, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Gridded Component 2 register
  call ESMF_GridCompSetVM(comp2, userRoutine=userm2_setvm, userRc=userrc, &
    rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(comp2, userRoutine=userm2_register, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Gridded Component 3 register
  call ESMF_GridCompSetVM(comp3, userRoutine=userm3_setvm, userRc=userrc, &
    rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(comp3, userRoutine=userm3_register, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Gridded Component 4 register
  call ESMF_GridCompSetVM(comp4, userRoutine=userm4_setvm, userRc=userrc, &
    rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(comp4, userRoutine=userm4_register, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Coupler Component register
  call ESMF_CplCompSetVM(cplcomp, userRoutine=usercpl_setvm, userRc=userrc, &
    rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_CplCompSetServices(cplcomp, userRoutine=usercpl_register, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

! print *, '-------------------------------Initialize----------------------------------'

  ! Gridded Component 1 initialize
  c1imp = ESMF_StateCreate(name="Comp1 importState",  &
                           stateintent=ESMF_STATEINTENT_IMPORT, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  c1exp = ESMF_StateCreate(name="Comp1 exportState",  &
                           stateintent=ESMF_STATEINTENT_EXPORT, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompInitialize(comp1, importState=c1imp, exportState=c1exp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Gridded Component 2 initialize
  c2imp = ESMF_StateCreate(name="Comp2 importState",  &
                           stateintent=ESMF_STATEINTENT_IMPORT, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  c2exp = ESMF_StateCreate(name="Comp2 exportState",  &
                           stateintent=ESMF_STATEINTENT_EXPORT, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompInitialize(comp2, importState=c2imp, exportState=c2exp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Gridded Component 3 initialize
  c3imp = ESMF_StateCreate(name="Comp3 importState",  &
                           stateintent=ESMF_STATEINTENT_IMPORT, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  c3exp = ESMF_StateCreate(name="Comp3 exportState",  &
                           stateintent=ESMF_STATEINTENT_EXPORT, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompInitialize(comp3, importState=c3imp, exportState=c3exp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Gridded Component 4 initialize
  c4imp = ESMF_StateCreate(name="Comp4 importState",  &
                           stateintent=ESMF_STATEINTENT_IMPORT, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  c4exp = ESMF_StateCreate(name="Comp4 exportState",  &
                           stateintent=ESMF_STATEINTENT_EXPORT, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompInitialize(comp4, importState=c4imp, exportState=c4exp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Coupler Component initialize, comp1 -> comp2
  call ESMF_CplCompInitialize(cplcomp, importState=c1exp, exportState=c2imp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Reconcile comp2's export State to get comp2's Field Attributes onto
  ! all PETs 0-7 from PETs 2,3 since Coupler is setup one way,
  ! comp1 -> comp2, rather than vice versa, comp2 -> comp1 (Coupler only has
  ! access to c1exp).  So cannot put in Coupler initialize (c2exp not visible),
  ! must put here in driver
  call ESMF_StateReconcile(c2exp, vm=vm, attreconflag=ESMF_ATTRECONCILE_ON, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Similarly, reconcile comp3 and comp4's export States here, since they are
  ! not visible from Coupler initialize
  call ESMF_StateReconcile(c3exp, vm=vm, attreconflag=ESMF_ATTRECONCILE_ON, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateReconcile(c4exp, vm=vm, attreconflag=ESMF_ATTRECONCILE_ON, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Link gridded comp's Attribute hierarchies to their respective
  ! export States (must occur here in driver, rather than component initialize,
  ! or coupler initialize, since driver has access to both comps and their
  ! export states, and to create the links on all PETs)
  call ESMF_AttributeLink(comp1, c1exp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeLink(comp2, c2exp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeLink(comp3, c3exp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeLink(comp4, c4exp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  if (petCount .eq. 8) then
    ! call AttributeUpdate to get a VM wide view of the
    ! metadata set on comp1 in comp1initialize
    call ESMF_AttributeUpdate(comp1, vm, rootList=(/0,1/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Call AttributeUpdate to get a VM wide view of the
    ! metadata set on comp2 in comp1initialize
    call ESMF_AttributeUpdate(comp2, vm, rootList=(/2,3/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Call AttributeUpdate to get a VM wide view of the
    ! metadata set on comp3 in comp1initialize
    call ESMF_AttributeUpdate(comp3, vm, rootList=(/4,5/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Call AttributeUpdate to get a VM wide view of the
    ! metadata set on comp4 in comp1initialize
    call ESMF_AttributeUpdate(comp4, vm, rootList=(/6,7/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !else
    ! uni-processor mode -- ESMF_AttributeUpdate() not necessary
  endif

  ! Link the Coupler Component Attribute hierarchy to the Gridded Components'
  ! (must occur here in driver, rather than coupler initialize, since driver has
  ! access to cplcomp, comp1, comp2, comp3, and comp4)
  call ESMF_AttributeLink(cplcomp, comp1, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeLink(cplcomp, comp2, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeLink(comp1, comp3, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeLink(comp2, comp4, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  conv = 'CIM'
  purp = 'Model Component Simulation Description'
#if 1
  ! write the Attribute info in CIM XML format for the Coupler, both Components,
  ! and their Fields
  if (localPet .eq. 0) then
    call ESMF_AttributeWrite(cplcomp, conv, purp, &
                             attwriteflag=ESMF_ATTWRITE_XML, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) then
      if (rc .ne. ESMF_RC_LIB_NOT_PRESENT) then
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
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
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! run comp3, chid of comp1
  call ESMF_GridCompRun(comp3, importState=c3imp, exportState=c3exp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

#if 0
  if (petCount .eq. 8) then
    call ESMF_AttributeUpdate(comp1, vm, rootList=(/0,1/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(comp1, name="IndividualName", value=attrVal, &
      convention=conv, purpose=purp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    print *, "IndividualName = ", attrVal
  endif
#endif

  ! run the coupler
  call ESMF_CplCompRun(cplcomp, importState=c1exp, exportState=c2imp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! run comp2
  call ESMF_GridCompRun(comp2, importState=c2imp, exportState=c2exp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! run comp4, child of comp2
  call ESMF_GridCompRun(comp4, importState=c4imp, exportState=c4exp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

! print *, '--------------------------------Finalize------------------------------------'

  ! finalize comp1
  call ESMF_GridCompFinalize(comp1, importState=c1imp, exportState=c1exp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! finalize comp2
  call ESMF_GridCompFinalize(comp2, importState=c2imp, exportState=c2exp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! finalize comp3
  call ESMF_GridCompFinalize(comp3, importState=c3imp, exportState=c3exp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! finalize comp4
  call ESMF_GridCompFinalize(comp4, importState=c4imp, exportState=c4exp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! finalize the coupler
  call ESMF_CplCompFinalize(cplcomp, importState=c1exp, exportState=c2imp, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

! print *, '---------------------------------Destroy------------------------------------'

  ! destroy components
  call ESMF_GridCompDestroy(comp1, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompDestroy(comp2, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompDestroy(comp3, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompDestroy(comp4, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_CplCompDestroy(cplcomp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! destroy states
  call ESMF_StateDestroy(c1imp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateDestroy(c1exp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateDestroy(c2imp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateDestroy(c2exp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateDestroy(c3imp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateDestroy(c3exp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateDestroy(c4imp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateDestroy(c4exp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

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
