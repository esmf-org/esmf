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
module ESMF_AttributeUpdateCIMRespPartyUTestMod

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

    ! Local variables
          type(ESMF_AttPack) :: attpack, attpack1, attpack2
    integer                     :: nameCount
    character(ESMF_MAXSTR)      :: convCIM, purpComp
    character(ESMF_MAXSTR)      :: convISO, purpRP, purpCitation
    character(ESMF_MAXSTR), dimension(2) :: nestConv, nestPurp
    character(ESMF_MAXSTR), dimension(5) :: nestAttPackName


    ! Initialize return code
    rc = ESMF_SUCCESS

    !
    !  CIM child component attributes, set on this comp, child of the coupler
    !
    convCIM = 'CIM 1.5'
    purpComp = 'ModelComp'

    convISO = 'ISO 19115'
    purpRP = 'RespParty'
    purpCitation = 'Citation'

    nestConv(1) = convISO
    nestPurp(1) = purpRP
    nestConv(2) = convISO
    nestPurp(2) = purpCitation

    nameCount = 0
    call ESMF_AttributeAdd(comp, convention=convCIM, purpose=purpComp, &
      nestConvention=nestConv, nestPurpose=nestPurp, &
      nestAttPackInstanceCountList=(/2,1/), &
      nestAttPackInstanceNameList=nestAttPackName, &
      nestCount=2, nestAttPackInstanceNameCount=nameCount, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_AttributeSet(comp, 'ShortName', &
      'EarthSys_Atmos', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'LongName', &
      'Atmosphere component of the EarthSys model', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'ReleaseDate', &
      '2009-02-02T02:03:04Z', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'ModelType', &
      'Atmosphere', &
        convention=convCIM, purpose=purpComp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_AttributeGetAttPack(comp, convISO, purpRP, &
      attPackInstanceName=nestAttPackName(1), attpack=attpack1, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    ! Responsible party attributes (for Principal Investigator)
    call ESMF_AttributeSet(comp, 'Name', &
      'John Doe', attpack=attpack1, rc=rc)
    call ESMF_AttributeSet(comp, 'PhysicalAddress', &
     'Department of Meteorology, University of ABC',&
        attpack=attpack1, rc=rc)
    call ESMF_AttributeSet(comp, 'EmailAddress', &
      'john.doe@earthsys.org', attpack=attpack1, rc=rc)
    call ESMF_AttributeSet(comp, 'ResponsiblePartyRole', &
      'PI', attpack=attpack1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_AttributeGetAttPack(comp, convISO, purpRP, &
      attPackInstanceName=nestAttPackName(2), attpack=attpack2, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    ! Responsible party attributes (for Center)
    call ESMF_AttributeSet(comp, 'Name', &
     'Department of Meteorology, University of ABC', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(2),rc=rc)
    call ESMF_AttributeSet(comp, 'PhysicalAddress', &
     'Colorado, USA', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(2),rc=rc)
    call ESMF_AttributeSet(comp, 'EmailAddress', &
     'info@earthsys.org', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(2),rc=rc)
    call ESMF_AttributeSet(comp, 'ResponsiblePartyRole', &
     'Center', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(2),rc=rc)
    call ESMF_AttributeSet(comp, 'URL', &
     'www.earthsys.org', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(2),rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

  end subroutine userm1_init

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.

  subroutine userm2_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

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

    rc = ESMF_SUCCESS

  end subroutine usercpl_init

!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !

  subroutine userm1_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

          type(ESMF_AttPack) :: attpack1, attpack2, attpack_nested
    character(ESMF_MAXSTR) :: convCIM, purpComp, convISO, purpRP, purpExt
    character(ESMF_MAXSTR),dimension(2) :: attrList, valueList, attPackInstNames
    integer attPackInstCount

    convCIM = 'CIM 1.5'
    purpComp = 'ModelComp'
    purpExt = 'Extended'

    attrList(1) = 'Coordinates'
    attrList(2) = 'Mask'
    valueList(1) = "Latlon"
    valueList(2) = "Yes"

    convISO = 'ISO 19115'
    purpRP = 'RespParty'

    call ESMF_AttributeGet(comp, convention=convISO, purpose=purpRP, &
      attPackInstanceNameList=attPackInstNames, &
      attPackInstanceNameCount=attPackInstCount, rc=rc)

    call ESMF_AttributeGetAttPack(comp, convISO, purpRP, attpack=attpack1, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_AttributeGetAttPack(comp, convISO, purpRP, &
      attPackInstanceName=attPackInstNames(2), attpack=attpack2, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    call ESMF_AttributeSet(comp, 'Name', &
     'University of CBA', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=attPackInstNames(2), rc=rc)
    if (rc/=ESMF_SUCCESS) return

    call ESMF_AttributeAdd(comp, convention=convCIM, purpose=purpExt, &
      attrList=attrList, nestConvention=convCIM, nestPurpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_AttributeSet(comp, attrList(1), valueList(1), &
      convention=convCIM, purpose=purpExt, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_AttributeSet(comp, attrList(2), valueList(2), &
      convention=convCIM, purpose=purpExt, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    call ESMF_AttributeRemove(comp, name='PhysicalAddress', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    call ESMF_AttributeRemove(comp, name='EmailAddress', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=attPackInstNames(2), rc=rc)
    if (rc/=ESMF_SUCCESS) return

  end subroutine userm1_run

!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !

  subroutine usercpl_run(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

  end subroutine usercpl_run

!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !

  subroutine userm2_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

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

program ESMF_AttributeUpdateCIMRespPartyUTest

#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_AttributeUpdateCIMRespPartyUTest - Attribute Update Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Attribute Update unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF
  use ESMF_TestMod
  use ESMF_AttributeUpdateCIMRespPartyUTestMod, only : userm1_setvm, userm1_register, &
    userm2_setvm, userm2_register, usercpl_setvm, usercpl_register


  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------


    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc = ESMF_SUCCESS

    ! local variables
    integer                 :: petCount, localPet, attPackInstCount
    type(ESMF_VM)           :: vm
    type(ESMF_GridComp)     :: gridcomp1
    type(ESMF_GridComp)     :: gridcomp2
    type(ESMF_CplComp)      :: cplcomp
    type(ESMF_AttPack)      :: attpack
    character(ESMF_MAXSTR)  :: convCIM, purpExt, convISO, purpRP, outVal
    character(ESMF_MAXSTR)  :: attPackInstNames(2)

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

    ! run components on interleaving and overlapping Pets
    gridcomp1 = ESMF_GridCompCreate(name="gridcomp1", &
      petList=(/0,2,4,5/), rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    gridcomp2 = ESMF_GridCompCreate(name="gridcomp2", &
      petList=(/1,3,4,5/), rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    cplcomp = ESMF_CplCompCreate(name="cplcomp", &
      petList=(/0,1,2,3,4,5/), rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetVM(gridcomp1, userm1_setvm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetVM(gridcomp2, userm2_setvm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetVM(cplcomp, usercpl_setvm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetServices(gridcomp1,  &
        userRoutine=userm1_register, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(gridcomp2,  &
        userRoutine=userm2_register, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetServices(cplcomp,  &
        userRoutine=usercpl_register, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompInitialize(gridcomp1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(gridcomp2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompInitialize(cplcomp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompRun(gridcomp1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompRun(cplcomp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompRun(gridcomp2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! call AttributeUpdate to get a VM wide view of the
    ! metadata set on comp1 in comp1initialize
    call ESMF_AttributeUpdate(gridcomp1, vm, &
      rootList=(/0,2,4,5/), rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    convCIM = 'CIM 1.5'
    purpExt = 'Extended'

    convISO = 'ISO 19115'
!    purpRP = 'Responsible Party Description'
    purpRP = 'RespParty'

    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGet(gridcomp1, convention=convISO, purpose=purpRP, &
      attPackInstanceNameList=attPackInstNames, &
      attPackInstanceNameCount=attPackInstCount, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "Getting AttPack instance names from a GridComp test"
    call ESMF_Test((rc==ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)
    !print *, "attPackInstNames(1)=", attPackInstNames(1)
    !print *, "attPackInstNames(2)=", attPackInstNames(2)

    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGetAttPack(gridcomp1, &
                       convention=convISO, purpose=purpRP, &
                       attPackInstanceName=attPackInstNames(2), &
                       attpack=attpack, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "Get Attribute package ["!, convISO, ", ", purpRP, ", ",&
                    !attPackInstNames(2), "]"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGet(gridcomp1, 'Name', value=outVal, &
      attpack=attpack, rc=rc)  ! in 2nd RP instance
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "Getting an updated Attribute value from a GridComp test"
    call ESMF_Test((rc==ESMF_SUCCESS).and.(outVal=='University of CBA'), &
                    name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGetAttPack(gridcomp1, &
                       convention=convISO, purpose=purpRP, &
                       attPackInstanceName=attPackInstNames(1), &
                       attpack=attpack, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "Get Attribute package ["!, convISO, ", ", purpRP, ", ",&
                    !attPackInstNames(1), "]"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGet(gridcomp1, 'Name', value=outVal, &
      attpack=attpack, rc=rc)  ! in 1st RP instance
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "Getting an updated Attribute value from a GridComp test"
    call ESMF_Test((rc==ESMF_SUCCESS).and.(outVal=='John Doe'), &
                    name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGetAttPack(gridcomp1, &
                       convention=convCIM, purpose=purpExt, &
                       attpack=attpack, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "Get Attribute package ["!, convISO, ", ", purpRP, "]"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGet(gridcomp1, 'Coordinates', value=outVal, &
      convention=convCIM, purpose=purpExt, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "Getting an updated Attribute package Attribute value from a GridComp test"
    call ESMF_Test((rc==ESMF_SUCCESS).and.(outVal=='Latlon'), &
                    name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGet(gridcomp1, 'Mask', value=outVal, &
      convention=convCIM, purpose=purpExt, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "Getting an updated Attribute package Attribute value from a GridComp test"
    call ESMF_Test((rc==ESMF_SUCCESS).and.(outVal=='Yes'), &
                    name, failMsg, result, ESMF_SRCLINE)

#if 0
    if (localPet == 0) then
      call ESMF_GridCompPrint(gridcomp1)
    endif
#endif

    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGetAttPack(gridcomp1, &
                       convention=convISO, purpose=purpRP, &
                       attPackInstanceName=attPackInstNames(2), &
                       attpack=attpack, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "Get Attribute package ["!, convISO, ", ", purpRP, ", ",&
                    !attPackInstNames(2), "]"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGet(gridcomp1, 'EmailAddress', value=outVal, &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=attPackInstNames(2), rc=rc)  ! in 2nd RP instance
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "Getting an updated deleted Attribute value from a GridComp test"
    call ESMF_Test((rc/=ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGetAttPack(gridcomp1, &
                       convention=convISO, purpose=purpRP, &
                       attpack=attpack, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "Get Attribute package ["!, convISO, ", ", purpExt, "]"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGet(gridcomp1, 'PhysicalAddress', value=outVal, &
      convention=convISO, purpose=purpRP, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "Getting an updated deleted Attribute value from a GridComp test"
    call ESMF_Test((rc/=ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    ! Now back to finalizing the model run
    call ESMF_GridCompFinalize(gridcomp1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompFinalize(gridcomp2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompFinalize(cplcomp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompDestroy(gridcomp1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(gridcomp2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompDestroy(cplcomp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#endif
    !-----------------------------------------------------------------------------
    call ESMF_TestEnd(ESMF_SRCLINE)
    !-----------------------------------------------------------------------------

end program ESMF_AttributeUpdateCIMRespPartyUTest
