! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_CompSetServUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_CompSetServUTest - Unit test for Components.
!
! !DESCRIPTION:
!   Test replacing an already registered service routine with another
!   and actually having it take effect.
!
!-------------------------------------------------------------------------
!
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF
    use SetServCode   ! "user" code
    implicit none
    
!   ! Local variables
    integer :: rc, userRc
    character(ESMF_MAXSTR) :: cname
    type(ESMF_GridComp) :: comp1
    type(ESMF_VM) :: vm
    integer:: localPet, petCount, i
    integer, allocatable:: petList(:)
    logical :: pthreadsEnabledFlag

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
    integer :: result = 0

    ! Internal State Variables
    type testData
    sequence
        integer :: testNumber
    end type

    type dataWrapper
    sequence
        type(testData), pointer :: p
    end type

#ifdef ESMF_TESTEXHAUSTIVE
    type (dataWrapper) :: wrap1, wrap2
    type(testData), target :: data1, data2
#endif

!-------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   to OFF, then only the sanity unit tests.
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------
        
    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

! - construct petList according to petCount
    call ESMF_VMGetGlobal(vm, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, &
      pthreadsEnabledFlag=pthreadsEnabledFlag, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    allocate(petList((petCount+1)/2))
    petList=(/(2*i, i=0,(petCount+1)/2-1)/)
    
!-------------------------------------------------------------------------
!   !
    !NEX_UTest
!   !  Create a Component
    write(name, *) "Creating a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    cname = "Atmosphere"
    comp1 = ESMF_GridCompCreate(name=cname, petList=petList, &
      configFile="comp.rc", rc=rc)  

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !NEX_UTest
!   !  Call initialize before SetServices
    write(name, *) "Calling Component Initialize before SetServices"
    write(failMsg, *) "Did return ESMF_SUCCESS"

    call ESMF_GridCompInitialize(comp1, rc=rc)

    if (ESMF_GridCompIsPetLocal(comp1)) then  
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    else
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    endif

!-------------------------------------------------------------------------
!   !
    !NEX_UTest
!   !  Call run before SetServices
    write(name, *) "Calling Component Run before SetServices"
    write(failMsg, *) "Did return ESMF_SUCCESS"

    call ESMF_GridCompRun(comp1, rc=rc)

    if (ESMF_GridCompIsPetLocal(comp1)) then  
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    else
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    endif

!-------------------------------------------------------------------------
!   !
    !NEX_UTest
!   !  Call finalize before SetServices
    write(name, *) "Calling Component Finalize before SetServices"
    write(failMsg, *) "Did return ESMF_SUCCESS"

    call ESMF_GridCompFinalize(comp1, rc=rc)

    if (ESMF_GridCompIsPetLocal(comp1)) then  
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    else
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    endif

!-------------------------------------------------------------------------
!   !
    !NEX_UTest
!   !  Destroying a component
    write(name, *) "Destroying a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompDestroy(comp1, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !NEX_UTest
!   !  Create a Component
    write(name, *) "Creating a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    cname = "Atmosphere - child in parent VM context"
    comp1 = ESMF_GridCompCreate(name=cname, configFile="comp.rc", &
      contextflag=ESMF_CONTEXT_PARENT_VM, rc=rc)  

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !NEX_UTest
!   !  Set Services
    write(name, *) "Setting Component Services - SetServ0"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompSetServices(comp1, userRoutine=SetServ0, userRc=userRc, &
      rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !NEX_UTest
    write(failMsg, *) "userRc not ESMF_SUCCESS"
    call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !NEX_UTest
!   !  Destroying a component
    write(name, *) "Destroying a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompDestroy(comp1, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !NEX_UTest
!   !  Create a Component
    write(name, *) "Creating a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    cname = "Atmosphere - child in parent VM context"
    comp1 = ESMF_GridCompCreate(name=cname, configFile="comp.rc", &
      contextflag=ESMF_CONTEXT_PARENT_VM, rc=rc)  

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !NEX_UTest
!   !  Set VM
    write(name, *) "Setting Component VM"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompSetVM(comp1, SetVM, userRc=userRc, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !NEX_UTest
#if (defined ESMF_TESTWITHTHREADS && ! defined ESMF_NO_PTHREADS)
    ! The user SetVM() routine will not return ESMF_SUCCESS because it cannot
    ! make the Component threaded due to the fact that it was created with
    ! ESMF_CONTEXT_PARENT_VM.
    if (pthreadsEnabledFlag) then
      ! ESMF Component Threading is enabled -> this test will work as expected
      write(failMsg, *) "userRc ESMF_SUCCESS"
      call ESMF_Test((userRc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    else
      ! ESMF Component Threading is NOT enabled -> SetVM will not even try to thread
      write(failMsg, *) "userRc not ESMF_SUCCESS"
      call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    endif
#else    
    write(failMsg, *) "userRc not ESMF_SUCCESS"
    call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif

!-------------------------------------------------------------------------
!   !
    !NEX_UTest
!   !  Set Services
    write(name, *) "Setting Component Services - SetServ0"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompSetServices(comp1, userRoutine=SetServ0, userRc=userRc, &
      rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !NEX_UTest
    write(failMsg, *) "userRc not ESMF_SUCCESS"
    call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !NEX_UTest
!   !  Destroying a component
    write(name, *) "Destroying a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompDestroy(comp1, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !NEX_UTest
!   !  Create a Component
    write(name, *) "Creating a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    cname = "Atmosphere - in its own context"
    comp1 = ESMF_GridCompCreate(name=cname, petList=petList, &
      configFile="comp.rc", rc=rc)  

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !NEX_UTest
!   !  Set VM
    write(name, *) "Setting Component VM"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompSetVM(comp1, SetVM, userRc=userRc, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !NEX_UTest
    write(failMsg, *) "userRc not ESMF_SUCCESS"
    call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !NEX_UTest
!   !  Set Services
    write(name, *) "Setting Component Services - SetServ1"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompSetServices(comp1, userRoutine=SetServ1, userRc=userRc, &
      rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !NEX_UTest
    write(failMsg, *) "userRc not ESMF_SUCCESS"
    call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !NEX_UTest
!   !  Call init
    write(name, *) "Calling Component Init"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompInitialize(comp1, userRc=userRc, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !NEX_UTest
    write(name, *) "Calling Component Init"
    write(failMsg, *) "userRc not ESMF_SUCCESS"
    call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Re-Set Services
    write(name, *) "Setting Component Services - SetServ2"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompSetServices(comp1, userRoutine=SetServ2, userRc=userRc, &
      rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    write(failMsg, *) "userRc not ESMF_SUCCESS"
    call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Call new init
    write(name, *) "Calling Component Init"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompInitialize(comp1, userRc=userRc, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    write(failMsg, *) "userRc does not contain expected code"
    if ((localPet/2)*2 == localPet) then
      call ESMF_Test((userRc.eq.123456), name, failMsg, result, ESMF_SRCLINE)
    else
      call ESMF_Test(userRc.eq.ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
    endif

!-------------------------------------------------------------------------
!   !  Set Internal State
    !EX_UTest
    write(name, *) "Set Internal State Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    data1%testnumber=4567
    wrap1%p=>data1

    call ESMF_GridCompSetInternalState(comp1, wrap1, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Get Internal State
    !EX_UTest
    write(name, *) "Get Internal State Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompGetInternalState(comp1, wrap2, rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Verify Internal State
    !EX_UTest
    write(name, *) "Verify Internal State Test"
    write(failMsg, *) "Did not return correct data"

    data2 = wrap2%p 

    call ESMF_Test((data2%testnumber.eq.4567), name, failMsg, result, ESMF_SRCLINE)
    print *, "data2%testnumber = ", data2%testnumber

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Destroying a component
    write(name, *) "Destroying a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompDestroy(comp1, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Create a Component
    write(name, *) "Creating a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    cname = "Atmosphere - child in parent VM context"
    comp1 = ESMF_GridCompCreate(name=cname, &
      configFile="comp.rc", contextflag=ESMF_CONTEXT_PARENT_VM, rc=rc)  

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Set Services
    write(name, *) "Setting Component Services - SetServ1"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompSetServices(comp1, userRoutine=SetServ1, userRc=userRc, &
      rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    write(failMsg, *) "userRc not ESMF_SUCCESS"
    call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Call init
    write(name, *) "Calling Component Init"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompInitialize(comp1, userRc=userRc, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    write(failMsg, *) "userRc not ESMF_SUCCESS"
    call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Destroying a component
    write(name, *) "Destroying a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompDestroy(comp1, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Create a Component
    write(name, *) "Creating a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    cname = "Atmosphere - in its own context"
    comp1 = ESMF_GridCompCreate(name=cname, rc=rc)  

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Set VM
    write(name, *) "Setting Component VM"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompSetVM(comp1, SetVM, userRc=userRc, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    write(failMsg, *) "userRc not ESMF_SUCCESS"
    call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Set VM
    write(name, *) "Setting Component VM - 2nd time (but before SetServices)"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompSetVM(comp1, SetVM, userRc=userRc, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    write(failMsg, *) "userRc not ESMF_SUCCESS"
    call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Set Services
    write(name, *) "Setting Component Services - SetServ1"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompSetServices(comp1, userRoutine=SetServ1, userRc=userRc, &
      rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    write(failMsg, *) "userRc not ESMF_SUCCESS"
    call ESMF_Test((userRc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Set VM
    write(name, *) "Setting Component VM - 3rd time (but after SetServices)"
    write(failMsg, *) "Did return ESMF_SUCCESS"

    call ESMF_GridCompSetVM(comp1, SetVM, userRc=userRc, rc=rc)
!     NOTE: The userRc is irrelevant when RC/=ESMF_SUCCESS

    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Set VM directly
    write(name, *) "Directly setting Component VM properties after SetServices"
    write(failMsg, *) "Did return ESMF_SUCCESS"

    call ESMF_GridCompSetVMMinThreads(comp1, rc=rc)

    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

!-------------------------------------------------------------------------
!   !
    !NEX_UTest
!   !  Destroying a component
    write(name, *) "Destroying a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    call ESMF_GridCompDestroy(comp1, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    deallocate(petList)

    call ESMF_TestEnd(ESMF_SRCLINE)

end program ESMF_CompSetServUTest
    
