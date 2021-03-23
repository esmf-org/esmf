! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
module ESMF_AttributeUpdateUTestMod

  use ESMF

  implicit none

  private

  public userm1_setvm, userm1_register
  public userm2_setvm, userm2_register
  public usercpl_setvm, usercpl_register

  contains

  !-------------------------------------------------------------------------
!   !  The SetVM Register routines for Gridcomp1
 
  subroutine userm1_setvm(comp, rc) 
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for 
    ! your own code development you probably don't want to include the 
    ! following call unless you are interested in exploring ESMF's 
    ! threading features.
    
    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetGlobal(vm, rc=rc)
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (pthreadsEnabled) then
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    endif
#endif

  end subroutine userm1_setvm

  subroutine userm1_register(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, userm1_init, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, userm1_run, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, userm1_final, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine userm1_register

!-------------------------------------------------------------------------
!   !  The SetVM Register routines for Gridcomp2

  subroutine userm2_setvm(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for 
    ! your own code development you probably don't want to include the 
    ! following call unless you are interested in exploring ESMF's 
    ! threading features.

    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetGlobal(vm, rc=rc)
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (pthreadsEnabled) then
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    endif
#endif

  end subroutine userm2_setvm

  subroutine userm2_register(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, userm2_init, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, userm2_run, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, userm2_final, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine userm2_register

!-------------------------------------------------------------------------
!   !  The SetVM Register routines for cplcomp

  subroutine usercpl_setvm(comp, rc)
    type(ESMF_CplComp) :: comp
    integer, intent(out) :: rc

#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for 
    ! your own code development you probably don't want to include the 
    ! following call unless you are interested in exploring ESMF's 
    ! threading features.

    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetGlobal(vm, rc=rc)
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (pthreadsEnabled) then
      call ESMF_CplCompSetVMMinThreads(comp, rc=rc)
    endif
#endif

  end subroutine usercpl_setvm

  subroutine usercpl_register(comp, rc)
    type(ESMF_CplComp) :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Register the callback routines.
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, usercpl_init, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_RUN, usercpl_run, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, usercpl_final, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine usercpl_register

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.


  subroutine userm1_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    type(ESMF_AttPack)                   :: attpack
    type(ESMF_VM)                        :: vm
    integer                              :: petCount, status, myPet
    character(ESMF_MAXSTR), dimension(3) :: attrList

    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) return
    call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    attrList(1) = "name1"
    attrList(2) = "name2"
    attrList(3) = "name3"

    call ESMF_AttributeAdd(comp, convention="Comp", purpose="Top", &
      attrList=attrList, attpack=attpack, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    call ESMF_AttributeSet(comp, "name1", attpack=attpack, value="value1", &
      rc=status)
    call ESMF_AttributeSet(comp, "name2", attpack=attpack, value="value2", &
      rc=status)
    call ESMF_AttributeSet(comp, "name3", attpack=attpack, value="value3", &
      rc=status)
    if (status .ne. ESMF_SUCCESS) return

  end subroutine userm1_init

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.


  subroutine userm2_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

   ! Initialize return code
    rc = ESMF_SUCCESS

  end subroutine userm2_init

!-------------------------------------------------------------------------
!   !User Comp Component created by higher level calls, here is the
!   ! Initialization routine.

  subroutine usercpl_init(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    type(ESMF_VM)         :: vm

    rc = ESMF_SUCCESS

    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    ! call ESMF_StateReconcile(importState, vm=vm, &
    !   attreconflag=ESMF_ATTRECONCILE_ON, rc=rc)
    ! if (rc/=ESMF_SUCCESS) return
    ! call ESMF_StateReconcile(exportState, vm=vm, &
    !   attreconflag=ESMF_ATTRECONCILE_ON, rc=rc)
    ! if (rc/=ESMF_SUCCESS) return

  end subroutine usercpl_init

!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !

  subroutine userm1_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    type(ESMF_AttPack)        :: attpack
    type(ESMF_VM)             :: vm
    integer                   :: petCount, status, myPet
    character(ESMF_MAXSTR)    :: outVal

    rc = ESMF_SUCCESS

    ! add a single Attribute alongside the Attribute packages
    call ESMF_AttributeSet(comp, "Lone Attribute", value="Lone Attribute", &
                           rc=status)

  end subroutine userm1_run

!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !

  subroutine usercpl_run(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    type(ESMF_VM)               :: vm
    integer                     :: myPet

    integer, dimension(2)       :: rootList

    rc = ESMF_SUCCESS

    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_VMGet(vm, localPet=myPet, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    call ESMF_StateGet(importState, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_StateGet(exportState, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    ! rootList = (/0,1/)
    ! call ESMF_AttributeUpdate(importState, vm, rootList=rootList, rc=rc)
    ! if (rc/=ESMF_SUCCESS) return
!
    ! call ESMF_AttributeCopy(importState, exportState, &
    !   attcopy=ESMF_ATTCOPY_REFERENCE, rc=rc)
    ! if (rc/=ESMF_SUCCESS) return

  end subroutine usercpl_run

!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !

  subroutine userm2_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    type(ESMF_VM)               :: vm
    integer                     :: petCount, status, myPet

    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) return
    call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=status)
    if (status .ne. ESMF_SUCCESS) return


  end subroutine userm2_run

!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !

  subroutine userm1_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

  end subroutine userm1_final

!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !

  subroutine userm2_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

  end subroutine userm2_final

!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !

  subroutine usercpl_final(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

  end subroutine usercpl_final
 
end module

program ESMF_AttributeUpdateUTest

#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_AttributeUpdateUTest - Attribute Update Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Attribute Update unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF
  use ESMF_TestMod
  use ESMF_AttributeUpdateUTestMod, only : userm1_setvm, userm1_register, &
  userm2_setvm, userm2_register, usercpl_setvm, usercpl_register


  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------


    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(2*ESMF_MAXSTR) :: name

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc = ESMF_SUCCESS

    ! local variables
    integer                 :: petCount, localPet
    integer                 :: petList1(4), petList2(4), petList3(8)
    type(ESMF_VM)           :: vm
    type(ESMF_State)        :: c1exp, c2imp
    type(ESMF_GridComp)     :: gridcomp1
    type(ESMF_GridComp)     :: gridcomp2
    type(ESMF_CplComp)      :: cplcomp

    type(ESMF_AttPack)      :: attpack
    character(ESMF_MAXSTR)  :: outVal

!-------------------------------------------------------------------------------
!  The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!  always run. When the environment variable, EXHAUSTIVE, is set to ON then
!  the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!  to OFF, then only the sanity unit tests.
!  Special strings (Non-exhaustive and exhaustive) have been
!  added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

    !-----------------------------------------------------------------------------
    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    !-----------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE
    call ESMF_VMGetCurrent(vm, rc=rc) 
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    petList1 = (/2,3,0,1/)
    petList2 = (/6,7,4,5/)
    petList3(1:4) = petList1
    petList3(5:8) = petList2

#if 0
    petList1 = (/20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,36,37,38,39/)
    petList2 = (/79,78,77,76,75,74,73,72,71,70,69,68,67,66,65,64,63,62,61,60,59,58,57,56,55,54,53,52,51,50,49,48,47,46,45,44,43,42,41,40/)
    petList2 = (/40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79/)
    petList2 = (/50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,40,41,42,43,44,45,46,47,48,49,70,71,72,73,74,75,76,77,78,79/)
    petList3(1:40) = petList1
    petList3(41:80) = petList2
#endif

    gridcomp1 = ESMF_GridCompCreate(name="gridcomp1", &
      petList=petList2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    gridcomp2 = ESMF_GridCompCreate(name="gridcomp2", &
      petList=petList1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    cplcomp = ESMF_CplCompCreate(name="cplcomp", &
      petList=petList3, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    c1exp = ESMF_StateCreate(name="Comp1 exportState", &
                             stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    c2imp = ESMF_StateCreate(name="Comp2 importState", &
                             stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetVM(gridcomp1, userm1_setvm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetVM(gridcomp2, userm2_setvm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetVM(cplcomp, usercpl_setvm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetServices(gridcomp1, userRoutine=userm1_register, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(gridcomp2, userRoutine=userm2_register, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetServices(cplcomp, userRoutine=usercpl_register, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompInitialize(gridcomp1, exportState=c1exp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(gridcomp2, importState=c2imp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompInitialize(cplcomp, importState=c1exp, &
      exportState=c2imp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompRun(gridcomp1, exportState=c1exp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompRun(cplcomp, importState=c1exp, &
      exportState=c2imp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompRun(gridcomp2, importState=c2imp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !!!!!!!!!!!!!!!!!!!!! Attribute Update on Component !!!!!!!!!!!!!!!!
    call ESMF_AttributeUpdate(gridcomp1, vm, rootList=petList2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    !!!!!!!!!!!!!!!!!!!!! Attribute Update on Component !!!!!!!!!!!!!!!!

    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGetAttPack(gridcomp1, convention="Comp", purpose="Top", &
            attpack=attpack, rc=rc)
    call ESMF_AttributeGet(gridcomp1, "name1", value=outVal, attpack=attpack, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "PET ", localPet, ": Getting an updated Attribute value from a Component test: value = ", trim(outVal)
    call ESMF_Test((rc==ESMF_SUCCESS).and.(outVal=="value1"), &
                    name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGet(gridcomp1, "name2", value=outVal, attpack=attpack, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "PET ", localPet, ": Getting an updated Attribute value from a Component test: value = ", trim(outVal)
    call ESMF_Test((rc==ESMF_SUCCESS).and.(outVal=="value2"), &
                    name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGet(gridcomp1, "name3", value=outVal, attpack=attpack, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "PET ", localPet, ": Getting an updated Attribute value from a Component test: value = ", trim(outVal)
    call ESMF_Test((rc==ESMF_SUCCESS).and.(outVal=="value3"), &
                    name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGet(gridcomp1, "Lone Attribute", value=outVal, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "PET ", localPet, ": Getting a lone Attribute from a Component test: value = ", trim(outVal)
    call ESMF_Test((rc==ESMF_SUCCESS) .and. outVal=="Lone Attribute", &
                    name, failMsg, result, ESMF_SRCLINE)

    ! Now back to finalizing the model run
    call ESMF_GridCompFinalize(gridcomp1, exportState=c1exp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompFinalize(gridcomp2, importState=c2imp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompFinalize(cplcomp, importState=c1exp, &
      exportState=c2imp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompDestroy(gridcomp1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(gridcomp2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompDestroy(cplcomp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_StateDestroy(c1exp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_StateDestroy(c2imp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#endif
    !-----------------------------------------------------------------------------
    call ESMF_TestEnd(ESMF_SRCLINE)
    !-----------------------------------------------------------------------------

end program ESMF_AttributeUpdateUTest
