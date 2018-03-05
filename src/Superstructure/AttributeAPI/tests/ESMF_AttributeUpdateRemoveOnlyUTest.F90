! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
module ESMF_AttributeUpdateRemoveOnlyUTestMod

!  THIS unit test is to show the behavior of a known bug documented for the
!  510, 520, 520p1, and 520r releases.  When an Attribute
!  hierarchy is updated after ONLY removals have been performed, the newly
!  removed Attributes are not removed on all PETs by the ESMF_AttributeUpdate()
!
!  The reason for this bug is that the routine inside of AttributeUpdate which
!  unpackes the serialized Attribute hierarchy that is sent from the rootPets
!  is received on the non-root PETs.  If only removals have happened then the
!  missing Attributes will not be present on the non root PETs where the
!  unpacking routine runs.  The routine is recursive, so it depends on an object
!  hierarchy to guide the program flow.  If there is no Attribute there, it will
!  not be able to detect that fact.

!  One solution would be to use the entire buffer that is passed from the root

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

    type(ESMF_AttPack)        :: attpack
    type(ESMF_VM)               :: vm
    integer                     :: petCount, status, myPet
    character(ESMF_MAXSTR)      :: name1,name2,name3,name4,value1,value2, &
                                   value3,value4,convESMF,purpGen
    type(ESMF_ArraySpec)        :: arrayspec
    type(ESMF_Grid)             :: grid
    type(ESMF_Field)            :: field
    type(ESMF_FieldBundle)      :: fieldbundle
    character(ESMF_MAXSTR),dimension(2)   :: attrList

    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) return
    call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/100,150/), &
      regDecomp=(/1,petCount/), &
      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    convESMF = 'ESMF'
    purpGen = 'General'
    name1 = 'ShortName'
    name2 = 'StandardName'
    name3 = 'LongName'
    name4 = 'Units'

    value1 = 'field'
    value2 = 'tendency_of_air_pressure'
    value3 = 'Edge pressure tendency'
    value4 = 'Pa s-1'

    field = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, name="field", rc=status)
    call ESMF_AttributeAdd(field, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(field, name1, value1, &
      convention=convESMF,  purpose=purpGen, rc=status)
    call ESMF_AttributeSet(field, name2, value2, &
      convention=convESMF, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(field, name3, value3, &
      convention=convESMF, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(field, name4, value4, &
      convention=convESMF, purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    fieldbundle = ESMF_FieldBundleCreate(name="fieldbundle", rc=status)
    if (status .ne. ESMF_SUCCESS) return

    call ESMF_FieldBundleAdd(fieldbundle, (/ field /), rc=status)
    if (status .ne. ESMF_SUCCESS) return

    call ESMF_StateAdd(exportState, fieldbundleList=(/fieldbundle/), rc=status)
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
    call ESMF_StateReconcile(importState, vm=vm, attreconflag=ESMF_ATTRECONCILE_ON, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_StateReconcile(exportState, vm=vm, attreconflag=ESMF_ATTRECONCILE_ON, rc=rc)
    if (rc/=ESMF_SUCCESS) return

  end subroutine usercpl_init

!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !

  subroutine userm1_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    type(ESMF_VM)               :: vm
    integer                     :: petCount, status, myPet
    character(ESMF_MAXSTR)      :: name3, name2, value32, convESMF, purpGen
    character(ESMF_MAXSTR)      :: name_to_add, value_to_add
    type(ESMF_Field)            :: field
    type(ESMF_FieldBundle)      :: fieldbundle
    type(ESMF_AttPack)        :: attpack

    rc = ESMF_SUCCESS

    convESMF = 'ESMF'
    purpGen = 'General'
    name2 = 'StandardName'
    name3 = 'LongName'
    value32 = 'CHANGED!!'
    name_to_add = "new_attribute"
    value_to_add = "new_att_value"


    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) return
    call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    call ESMF_StateGet(exportState, "fieldbundle", fieldbundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_FieldBundleGet(fieldbundle, fieldname="field", field=field, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    ! it's actually the CF/Extended (nested) package that contains "StandardName"
    call ESMF_AttributeGetAttPack(field, "CF", "Extended", attpack=attpack, rc=status)
    if (rc/=ESMF_SUCCESS) return

    call ESMF_AttributeRemove(field, name=name2, attpack=attpack, rc=status)
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

    rootList = (/0,1/)
    call ESMF_AttributeUpdate(importState, vm, rootList=rootList, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    call ESMF_AttributeCopy(importState, exportState, &
      attcopy=ESMF_ATTCOPY_REFERENCE, rc=rc)
    if (rc/=ESMF_SUCCESS) return

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

    type(ESMF_Field)            :: field
    type(ESMF_FieldBundle)      :: fieldbundle
    type(ESMF_Grid)             :: grid

    ! Initialize return code
    rc = ESMF_SUCCESS

    call ESMF_StateGet(exportState, "fieldbundle", fieldbundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldBundleGet(fieldbundle, fieldname="field", field=field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldDestroy(field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldBundleDestroy(fieldbundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridDestroy(grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

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

program ESMF_AttributeUpdateRemoveOnlyUTest

#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_AttributeUpdateRemoveOnlyUTest - Attribute Update Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Attribute Update unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF
  use ESMF_TestMod
  use ESMF_AttributeUpdateRemoveOnlyUTestMod, &
    only : userm1_setvm, userm1_register, &
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
      integer                 :: petCount, localPet
      type(ESMF_VM)           :: vm
      type(ESMF_State)        :: c1exp, c2imp
      type(ESMF_GridComp)     :: gridcomp1
      type(ESMF_GridComp)     :: gridcomp2
      type(ESMF_CplComp)      :: cplcomp
      character(ESMF_MAXSTR)  :: convESMF,purpGen

          type(ESMF_AttPack)    :: attpack
      type(ESMF_Field)        :: field
      type(ESMF_FieldBundle)  :: fieldbundle

      character(ESMF_MAXSTR)  :: name3, name2, value3, value2, value_out
      character(ESMF_MAXSTR)  :: name_to_add, value_to_add, value_out_added
      logical                 :: isPresent

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

    gridcomp1 = ESMF_GridCompCreate(name="gridcomp1", &
      petList=(/0,1/), rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    gridcomp2 = ESMF_GridCompCreate(name="gridcomp2", &
      petList=(/2,3/), rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    cplcomp = ESMF_CplCompCreate(name="cplcomp", &
      petList=(/0,1,2,3/), rc=rc)
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

    call ESMF_GridCompSetServices(gridcomp1,  &
        userRoutine=userm1_register, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(gridcomp2,  &
        userRoutine=userm2_register, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetServices(cplcomp,  &
        userRoutine=usercpl_register, rc=rc)
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

    ! Now we can start doing some testing
    convESMF = 'ESMF'
    purpGen = 'General'
    name3 = 'LongName'
    name2 = 'StandardName'
    value3 = 'CHANGED!!'
    value_out = ''
    value_out_added = ''

    name_to_add = "new_attribute"
    value_to_add = "new_att_value"

    call ESMF_StateGet(c1exp, "fieldbundle", fieldbundle, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldBundleGet(fieldbundle, fieldname="field", field=field, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !EX_UTest_Multi_Proc_Only
    ! This Attribute actually lives in the CF/Extended (nested) Attpack
    call ESMF_AttributeGetAttPack(field, convention="CF", &
                         purpose="Extended", attpack=attpack, rc=rc)
    call ESMF_AttributeGet(field, name2, attpack=attpack, &
                                           isPresent=isPresent, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "Getting an updated deleted Attribute value from a Field test"
    call ESMF_Test((rc==ESMF_SUCCESS).and.(isPresent.eqv..false.), &
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

end program ESMF_AttributeUpdateRemoveOnlyUTest
