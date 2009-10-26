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
#include "ESMF_Macros.inc"

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
  character(ESMF_MAXSTR) :: conv,purp
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

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
! Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
  
  ! Initialize framework and get back default global VM
  call ESMF_Initialize(vm=vm, defaultlogfilename="AttributeSTest.Log", &
                        defaultlogtype=ESMF_LOG_MULTI, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  ! Get number of PETs we are running with
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  if (localPet .eq. 0) then
      print *, "--------------------------------------- "
      print *, "Start of ", trim(testname)
      print *, "--------------------------------------- "
  endif


  if (petCount .lt. 6) then
    ! Create the 2 model components and coupler
    cname1 = "user model 1"
    ! use petList to define comp1 on PET 0,1,2,3
    comp1 = ESMF_GridCompCreate(name=cname1, petList=(/0/), rc=rc)
    if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    cname2 = "Finite_Volume_Dynamical_Core"
    ! use petList to define comp1 on PET 0,1,2,3
    comp2 = ESMF_GridCompCreate(name=cname2, petList=(/0/), rc=rc)
    if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    cplname = "user coupler"
    ! use petList to define comp1 on PET 0,1,2,3
    cplcomp = ESMF_CplCompCreate(name=cplname, petList=(/0/), rc=rc)
    if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  else
    ! Create the 2 model components and coupler
    cname1 = "user model 1"
    ! use petList to define comp1 on PET 0,1,2,3
    comp1 = ESMF_GridCompCreate(name=cname1, petList=(/0,1,2/), rc=rc)
    if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    cname2 = "Finite_Volume_Dynamical_Core"
    ! use petList to define comp1 on PET 0,1,2,3
    comp2 = ESMF_GridCompCreate(name=cname2, petList=(/3,4,5/), rc=rc)
    if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    cplname = "user coupler"
    ! use petList to define comp1 on PET 0,1,2,3
    cplcomp = ESMF_CplCompCreate(name=cplname, rc=rc)
    if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  
  endif

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompSetVM(comp1, userRoutine=userm1_setvm, userRc=userrc, &
    rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_GridCompSetServices(comp1, userRoutine=userm1_register, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridCompSetVM(comp2, userRoutine=userm2_setvm, userRc=userrc, &
    rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_GridCompSetServices(comp2, userRoutine=userm2_register, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_CplCompSetVM(cplcomp, userRoutine=usercpl_setvm, userRc=userrc, &
    rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_CplCompSetServices(cplcomp, userRoutine=usercpl_register, &
    userRc=userrc, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
! print *, '-------------------------------Initialize----------------------------------'
 
  ! create two states for comp1
  c1imp = ESMF_StateCreate("Comp1 importState", ESMF_STATE_IMPORT, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  c1exp = ESMF_StateCreate("Comp1 exportState", ESMF_STATE_EXPORT, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_GridCompInitialize(comp1, importState=c1imp, exportState=c1exp, &
    userRc=userrc, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! create two states for comp2
  c2imp = ESMF_StateCreate("Comp2 importState", ESMF_STATE_IMPORT, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  c2exp = ESMF_StateCreate("Comp2 exportState", ESMF_STATE_EXPORT, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  call ESMF_GridCompInitialize(comp2, importState=c2imp, exportState=c2exp, &
    userRc=userrc, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! create the coupler with appropriate import and export states
  call ESMF_CplCompInitialize(cplcomp, importState=c1exp, exportState=c2imp, &
    userRc=userrc, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
    
  ! link the Component Attribute hierarchy to State
!  call ESMF_AttributeLink(comp1, c1exp, rc=rc)
!  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
!    ESMF_CONTEXT, rcToReturn=rc)) &
!    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  
  ! now call AttributeUpdate to get a VM wide view of the
  ! metadata set on comp1 in comp1initialize
!  call ESMF_AttributeUpdate(comp1, vm, rootList=(/0,1,2/), rc=rc)
!  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
!    ESMF_CONTEXT, rcToReturn=rc)) &
!    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    
  ! now call AttributeCopy to get info from comp1 to comp2
  ! careful, though, this data only exists on PETs 0,1,2
  ! you must call AttributeUpdate to make this data 
  ! available VM wide (above)
!  call ESMF_AttributeCopy(comp1, comp2, &
!      ESMF_ATTCOPY_HYBRID, ESMF_ATTTREE_ON, rc=rc)
!  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
!    ESMF_CONTEXT, rcToReturn=rc)) &
!    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

! print *, '----------------------------------Run--------------------------------------'

  ! run comp1
  call ESMF_GridCompRun(comp1, importState=c1imp, exportState=c1exp, &
    userRc=userrc, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  ! run the coupler
  call ESMF_CplCompRun(cplcomp, importState=c1exp, exportState=c2imp, &
    userRc=userrc, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  ! run comp2
  call ESMF_GridCompRun(comp2, importState=c2imp, exportState=c2exp, &
    userRc=userrc, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Write the Attribute info in tab-delimited and .xml format for both Components
  ! The XML format will also send the output to an .xml in this directory
  conv = 'ESMF'
  purp = 'General'
  ! AttributeUpdate is not called so we are only printing from PET0 for now
  if (localPet .eq. 0) then
!    call ESMF_AttributeWrite(comp1,conv,purp,rc=rc)
!    call ESMF_AttributeWrite(comp2,conv,purp,rc=rc)
!    call ESMF_AttributeWrite(comp1,conv,purp,attwriteflag=ESMF_ATTWRITE_XML,rc=rc)
!    call ESMF_AttributeWrite(comp2,conv,purp,attwriteflag=ESMF_ATTWRITE_XML,rc=rc)
    if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)  
  endif
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

! print *, '--------------------------------Finalize------------------------------------'

  ! finalize comp1
  call ESMF_GridCompFinalize(comp1, importState=c1imp, exportState=c1exp, &
    userRc=userrc, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  ! finalize comp2
  call ESMF_GridCompFinalize(comp2, importState=c2imp, exportState=c2exp, &
    userRc=userrc, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  ! finalize the coupler
  call ESMF_CplCompFinalize(cplcomp, importState=c1exp, exportState=c2imp, &
    userRc=userrc, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  if (ESMF_LogMsgFoundError(userrc, ESMF_ERR_PASSTHRU, &
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
  
  ! destroy states
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

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((rc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize() 

end program ESMF_AttributeSTest
    
!\end{verbatim}
    
