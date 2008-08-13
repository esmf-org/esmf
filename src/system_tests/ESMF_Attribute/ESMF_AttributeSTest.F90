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
  type(ESMF_State) :: c1imp, c1exp, c2imp, c2exp
  type(ESMF_GridComp) :: comp1
  type(ESMF_GridComp) :: comp2
  type(ESMF_CplComp) :: cplcomp
  character(ESMF_MAXSTR) :: name1,name2,name3,name4,name5,name6,name7, &
                            name8, name9, name10, value1,value2,value3, &
                            value4,value5,value6,value7,value8,value9, &
                            value10,conv,purp
    
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
    !print  *, "Created component ", trim(cname1), "rc =", rc
    if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    cname2 = "Finite_Volume_Dynamical_Core"
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
    comp1 = ESMF_GridCompCreate(name=cname1, petList=(/0,1,2,3/), rc=rc)
    !print  *, "Created component ", trim(cname1), "rc =", rc
    if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    cname2 = "Finite_Volume_Dynamical_Core"
    ! use petList to define comp1 on PET 0,1,2,3
    comp2 = ESMF_GridCompCreate(name=cname2, petList=(/4,5/), rc=rc)
    !print  *, "Created component ", trim(cname1), "rc =", rc
    if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    cplname = "user coupler"
    ! use petList to define comp1 on PET 0,1,2,3
    cplcomp = ESMF_CplCompCreate(name=cplname, rc=rc)
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
 
  ! create two states for comp1
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

  ! create two states for comp2
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

  ! create the coupler with appropriate import and export states
  call ESMF_CplCompInitialize(cplcomp, importState=c1exp, exportState=c2imp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    
  ! add Attribute packages to the Gridded Components and link the States
  
  ! initialize variables
  conv = 'CF'
  purp = 'basic'
  name1 = 'discipline'
  name2 = 'physical_domain'
  name3 = 'agency'
  name4 = 'institution'
  name5 = 'author'
  name6 = 'coding_language'
  name7 = 'model_component_framework'
  name8 = 'name'
  name9 = 'full_name'
  name10 = 'version'
  value1 = 'Atmosphere'
  value2 = 'Earth system'
  value3 = 'NASA'
  value4 = 'Global Modeling and Assimilation Office (GMAO)'
  value5 = 'Max Suarez'
  value6 = 'Fortran 90'
  value7 = 'ESMF (Earth System Modeling Framework)'
  value8 = 'FV dycore'
  value9 = 'Finite Volume Dynamical Core'
  value10 = 'GEOSagcm-EROS-beta7p12'
  
  ! Add the Attribute package to comp2
  call ESMF_AttributeAdd(comp2, convention=conv, purpose=purp, rc=rc)
  call ESMF_AttributeSet(comp2, name1, value1, convention=conv, purpose=purp, rc=rc)
  call ESMF_AttributeSet(comp2, name2, value2, convention=conv, purpose=purp, rc=rc)
  call ESMF_AttributeSet(comp2, name3, value3, convention=conv, purpose=purp, rc=rc)
  call ESMF_AttributeSet(comp2, name4, value4, convention=conv, purpose=purp, rc=rc)
  call ESMF_AttributeSet(comp2, name5, value5, convention=conv, purpose=purp, rc=rc)
  call ESMF_AttributeSet(comp2, name6, value6, convention=conv, purpose=purp, rc=rc)
  call ESMF_AttributeSet(comp2, name7, value7, convention=conv, purpose=purp, rc=rc)
  call ESMF_AttributeSet(comp2, name8, value8, convention=conv, purpose=purp, rc=rc)
  call ESMF_AttributeSet(comp2, name9, value9, convention=conv, purpose=purp, rc=rc)
  call ESMF_AttributeSet(comp2, name10, value10, convention=conv, purpose=purp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  
    ! link the Component Attribute hierarchy to State
  call ESMF_AttributeSet(comp2, c2imp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

! print *, '----------------------------------Run--------------------------------------'

  ! run comp1
  call ESMF_GridCompRun(comp1, importState=c1imp, exportState=c1exp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  ! run the coupler
  call ESMF_CplCompRun(cplcomp, importState=c1exp, exportState=c2imp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  ! run comp2
  call ESMF_GridCompRun(comp2, importState=c2imp, exportState=c2exp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

  ! Write the Attribute info to esmf/test/testg/<platform>/ESMF_AttributeSTest.stdout
  ! The XML format will also send the output to an .xml in this directory
  if (localPet .eq. 0) then
    call ESMF_AttributeWrite(comp2,conv,purp,attwriteflag=ESMF_ATTWRITE_XML,rc=rc)
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
  call ESMF_GridCompFinalize(comp1, importState=c1imp, exportState=c1exp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  ! finalize comp2
  call ESMF_GridCompFinalize(comp2, importState=c2imp, exportState=c2exp, rc=rc)
  if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  ! finalize the coupler
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
  
!  print *, "------------------------------------------------------------"
!  print *, "------------------------------------------------------------"
!  print *, "Test finished, localPet = ", localPet
!  print *, "------------------------------------------------------------"
!  print *, "------------------------------------------------------------"

  call ESMF_Finalize() 

end program ESMF_AttributeSTest
    
!\end{verbatim}
    
