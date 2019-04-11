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


module ESMF_StateReconcileUTest_Mod
use ESMF
implicit none

contains

subroutine comp_dummy(gcomp, rc)
   type(ESMF_GridComp) :: gcomp
   integer, intent(out) :: rc

   rc = ESMF_SUCCESS
end subroutine comp_dummy
    

! Initialize routine which creates "field1" on PETs 0 and 1
subroutine comp1_init(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: istate, ostate
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Field) :: field1, field1nest
    type(ESMF_State) :: neststate

    print *, "i am comp1_init"

    rc = ESMF_FAILURE
    field1 = ESMF_FieldEmptyCreate(name="Comp1 Field", rc=rc)
    if (rc .ne. ESMF_SUCCESS) return
  
    call ESMF_StateAdd(istate, (/field1/), rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    neststate = ESMF_StateCreate(name="Nested State", rc=rc)
    if (rc .ne. ESMF_SUCCESS) return
    
    call ESMF_StateAdd(istate, (/neststate/), rc=rc)
    if (rc .ne. ESMF_SUCCESS) return
    
    field1nest = ESMF_FieldEmptyCreate(name="Comp1 Field in nested State", rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_StateAdd(neststate, (/field1nest/), rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

end subroutine comp1_init

! Initialize routine which creates "field2" on PETs 2 and 3
subroutine comp2_init(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: istate, ostate
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Field) :: field2

    print *, "i am comp2_init"

    rc = ESMF_FAILURE
    field2 = ESMF_FieldEmptyCreate(name="Comp2 Field", rc=rc)
    if (rc .ne. ESMF_SUCCESS) return
    
    call ESMF_StateAdd(istate, (/field2/), rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

end subroutine comp2_init

! Finalize routine which destroys "field1" on PETs 0 and 1
subroutine comp1_final(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: istate, ostate
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Field) :: field1, field1nest
    type(ESMF_State) :: neststate

    print *, "i am comp1_final"

    call ESMF_StateGet(istate, "Comp1 Field", field1,  rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_FieldDestroy(field1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_StateGet(istate, "Nested State", neststate,  rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_StateGet(neststate, "Comp1 Field in nested State", field1nest,  rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_FieldDestroy(field1nest, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_StateDestroy(neststate, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

end subroutine comp1_final

! Finalize routine which destroys "field2" on PETs 2 and 3
subroutine comp2_final(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: istate, ostate
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Field) :: field2

    print *, "i am comp2_final"

    call ESMF_StateGet(istate, "Comp2 Field", field2,  rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_FieldDestroy(field2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

end subroutine comp2_final
    

! Initialize routine which creates "field1"and "field2" - sharing a Grid
subroutine comp1_sg_init(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: istate, ostate
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Grid)  :: grid1
    type(ESMF_Field) :: field1, field2

    print *, "comp1_sg_init: entered"

    rc = ESMF_FAILURE

print *, 'comp1_sg_init: creating grid1'
    grid1 = ESMF_GridCreateNoPeriDim(  &
        minIndex=(/1,1/), maxIndex=(/10,20/),  &
        regDecomp=(/1,2/), name="shared Grid", rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

print *, 'comp1_sg_init: creating field1'
    field1 = ESMF_FieldCreate(grid1, typekind=ESMF_TYPEKIND_R4, &
        indexflag=ESMF_INDEX_DELOCAL, &
        staggerloc=ESMF_STAGGERLOC_CENTER, name="Field_sg1", rc=rc)
!    field1 = ESMF_FieldEmptyCreate(name="Field_sg1", rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

print *, 'comp1_sg_init: creating field2'
    field2 = ESMF_FieldCreate(grid1, typekind=ESMF_TYPEKIND_R4, &
        indexflag=ESMF_INDEX_DELOCAL, &
        staggerloc=ESMF_STAGGERLOC_CENTER, name="Field_sg2", rc=rc)
!    field2 = ESMF_FieldEmptyCreate(name="Field_sg2", rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

print *, 'comp1_sg_init: adding fields to istate'
    call ESMF_StateAdd(istate, (/field1, field2/), rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

end subroutine comp1_sg_init

! Initialize routine for shared Grid test
subroutine comp2_sg_init(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: istate, ostate
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    print *, "comp2_sg_init: entered"

    rc = ESMF_SUCCESS

end subroutine comp2_sg_init

! Finalize routine which destroys "field1" and "field2"
subroutine comp1_sg_final(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: istate, ostate
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Field) :: field1, field2

    print *, "i am comp1_sg_final"

    call ESMF_StateGet(istate, "Field_sg1", field1,  rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_FieldDestroy(field1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_StateGet(istate, "Field_sg2", field2,  rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_FieldDestroy(field2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

end subroutine comp1_sg_final

! Finalize routine
subroutine comp2_sg_final(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: istate, ostate
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    print *, "i am comp2_sg_final"

    rc = ESMF_SUCCESS

end subroutine comp2_sg_final

end module ESMF_StateReconcileUTest_Mod


program ESMF_StateReconcileUTest

!------------------------------------------------------------------------------
!==============================================================================
! !PROGRAM: ESMF_StateReconcileUTest - State reconciliation
!
! !DESCRIPTION:
!
! This program tests using the State Reconcile function
!  both concurrently and sequentially.
!
!-----------------------------------------------------------------------------
#include "ESMF_Macros.inc"

    ! ESMF Framework module
    use ESMF
    use ESMF_TestMod
    use ESMF_StateReconcileUTest_Mod
    implicit none


    !-------------------------------------------------------------------------
    ! Local variables
    integer :: rc
    type(ESMF_State) :: state1, state2, state3, state_nested
    type(ESMF_State) :: state_attr
    type(ESMF_State) :: state_sgrid
    type(ESMF_GridComp) :: comp1, comp2
    type(ESMF_GridComp) :: comp1_sg, comp2_sg
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array)     :: array1, array1_alternate, array2
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_Field)     :: field_nested, field_dummy
    type(ESMF_Field)     :: field_attr(5)
    type(ESMF_Field)     :: field_attr_new(size (field_attr))
    type(ESMF_Field)     :: field_sg1, field_sg2
    type(ESMF_FieldBundle) :: fb_attr, fb_attr_new
    type(ESMF_Grid)      :: grid_shared, grid_sg1, grid_sg2
    type(ESMF_VM) :: vm
    character(len=ESMF_MAXSTR) :: comp1name, comp2name, statename, fieldname
    character(len=ESMF_MAXSTR) :: array1name
    character(len=ESMF_MAXSTR) :: fb_name
    character(4) :: localpet_str, temppet_str
    integer :: attr_val(1)
    integer :: i
    logical :: reconcile_needed, recneeded_expected

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
    integer :: result = 0, localPet, petCount

    !-------------------------------------------------------------------------

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize()
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) goto 10

    ! Get the global VM for this job.
    call ESMF_VMGetGlobal(vm=vm, rc=rc)
    
    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
    write (localpet_str, '(i4)') localPet
    localpet_str = adjustl (localpet_str)

    !-------------------------------------------------------------------------
    ! exclusive component test section
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    comp1name = "Atmosphere"
    comp1 = ESMF_GridCompCreate(name=comp1name, petList=(/ 0, 1 /), rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    comp2name = "Ocean"
    comp2 = ESMF_GridCompCreate(name=comp2name, petList=(/ 2, 3 /), rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    statename = "Ocn2Atm"
    state1 = ESMF_StateCreate(name=statename, rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a State"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    reconcile_needed = ESMF_StateIsReconcileNeeded (state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Testing empty State for reconcile needed"
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return .false. for empty State"
    call ESMF_Test(.not. reconcile_needed, name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    reconcile_needed = ESMF_StateIsReconcileNeeded (state1,  &
        vm=vm, collectiveflag=.true., rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Testing empty State for reconcile needed (collective)"
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return .false. for empty State"
    call ESMF_Test(.not. reconcile_needed, name, failMsg, result, ESMF_SRCLINE)

    ! In SetServices() the VM for each component is initialized.
    ! Normally you would call SetEntryPoint inside set services,
    ! but to make this test very short, they are called inline below.

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp1, userRoutine=comp_dummy, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp2, userRoutine=comp_dummy, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp1, ESMF_METHOD_INITIALIZE, &
      userRoutine=comp1_init, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp2, ESMF_METHOD_INITIALIZE, &
      userRoutine=comp2_init, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp1, ESMF_METHOD_FINALIZE, &
      userRoutine=comp1_final, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp2, ESMF_METHOD_FINALIZE, &
      userRoutine=comp2_final, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompInitialize(comp1, importState=state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompInitialize(comp2, importState=state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    reconcile_needed = ESMF_StateIsReconcileNeeded (state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Testing modified State for reconcile needed"
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return .true. for modified State"
    call ESMF_Test(reconcile_needed, name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    reconcile_needed = ESMF_StateIsReconcileNeeded (state1,  &
        vm=vm, collectiveflag=.true., rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Testing modified State for reconcile needed (collective)"
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return .true. for modified State"
    call ESMF_Test(reconcile_needed, name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateReconcile(state1, vm=vm, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateReconcile in concurrent mode"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    reconcile_needed = ESMF_StateIsReconcileNeeded (state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Testing reconciled State for reconcile needed"
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! Note that even though the top level State should have its reconcileneeded
    ! flag cleared, the two nested States still should have their flags set.
    recneeded_expected = localpet == 0 .or. localpet == 1
    write(failMsg, *) "Did not return correct result for reconciled State",  &
        localpet, reconcile_needed, recneeded_expected
    call ESMF_Test(reconcile_needed .eqv. recneeded_expected,  &
        name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    reconcile_needed = ESMF_StateIsReconcileNeeded (state1,  &
        vm=vm, collectiveflag=.true., rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Testing reconciled State for reconcile needed (collective)"
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! Note that even though the top level State should have its reconcileneeded
    ! flag cleared, two nested States still should have their flags set.  So the
    ! collective ESMF_StateIsReconcileNeeded function should return .true..
    write(failMsg, *) "Did not return correct collective result for reconciled State",  &
        localpet, reconcile_needed, recneeded_expected
    call ESMF_Test(reconcile_needed,  &
        name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! Test redundant reconcile
    call ESMF_StateReconcile(state1, vm=vm, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling 2nd StateReconcile in concurrent mode"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate on 2nd StateReconcile"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompFinalize(comp1, importState=state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompFinalize"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompFinalize(comp2, importState=state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompFinalize"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompDestroy(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompDestroy"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompDestroy(comp2, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompDestroy"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateDestroy(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateDestroy"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    ! sequential component test section
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    comp1name = "Atmosphere"
    comp1 = ESMF_GridCompCreate(name=comp1name, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    comp2name = "Ocean"
    comp2 = ESMF_GridCompCreate(name=comp2name, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    statename = "Ocn2Atm"
    state1 = ESMF_StateCreate(name=statename, rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a State"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    ! In SetServices() the VM for each component is initialized.
    ! Normally you would call SetEntryPoint inside set services,
    ! but to make this test very short, they are called inline below.

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp1, userRoutine=comp_dummy, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp2, userRoutine=comp_dummy, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp1, ESMF_METHOD_INITIALIZE, &
      userRoutine=comp1_init, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp2, ESMF_METHOD_INITIALIZE, &
      userRoutine=comp2_init, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp1, ESMF_METHOD_FINALIZE, &
      userRoutine=comp1_final, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp2, ESMF_METHOD_FINALIZE, &
      userRoutine=comp2_final, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompInitialize(comp1, importState=state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompInitialize(comp2, importState=state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateReconcile(state1, vm=vm, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateReconcile in sequential mode"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! Test redundant reconcile
    call ESMF_StateReconcile(state1, vm=vm, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling 2nd StateReconcile in sequential mode"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate on 2nd StateReconcile"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompFinalize(comp1, importState=state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompFinalize"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompFinalize(comp2, importState=state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompFinalize"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompDestroy(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompDestroy"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompDestroy(comp2, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompDestroy"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateDestroy(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateDestroy"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------


    !-------------------------------------------------------------------------
    ! run-in-parent-context component test section
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    comp1name = "Atmosphere"
    comp1 = ESMF_GridCompCreate(name=comp1name, &
      contextflag=ESMF_CONTEXT_PARENT_VM, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    comp2name = "Ocean"
    comp2 = ESMF_GridCompCreate(name=comp2name, &
      contextflag=ESMF_CONTEXT_PARENT_VM, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    statename = "Ocn2Atm"
    state1 = ESMF_StateCreate(name=statename, rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a State"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    ! In SetServices() the VM for each component is initialized.
    ! Normally you would call SetEntryPoint inside set services,
    ! but to make this test very short, they are called inline below.

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp1, userRoutine=comp_dummy, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp2, userRoutine=comp_dummy, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp1, ESMF_METHOD_INITIALIZE, &
      userRoutine=comp1_init, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp2, ESMF_METHOD_INITIALIZE, &
      userRoutine=comp2_init, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp1, ESMF_METHOD_FINALIZE, &
      userRoutine=comp1_final, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp2, ESMF_METHOD_FINALIZE, &
      userRoutine=comp2_final, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompInitialize(comp1, importState=state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompInitialize(comp2, importState=state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateReconcile(state1, vm=vm, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateReconcile in run-in-parent mode"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! Test redundant reconcile
    call ESMF_StateReconcile(state1, vm=vm, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling 2nd StateReconcile in run-in-parent mode"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate on 2nd StateReconcile"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompFinalize(comp1, importState=state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompFinalize"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompFinalize(comp2, importState=state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompFinalize"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompDestroy(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompDestroy"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompDestroy(comp2, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompDestroy"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateDestroy(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateDestroy"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------


    !-------------------------------------------------------------------------
    ! Simple re-reconcile test section
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    state2 = ESMF_StateCreate ()
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateCreate for rereconcile tests"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    rc = ESMF_SUCCESS
    array1name = 'Array_for_reconciling'
    if (localPet == 0) then
      call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, &
          rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/15,23/), &
          regDecomp=(/2,2/), rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      array1 = ESMF_ArrayCreate(arrayspec=arrayspec, name=array1name,  &
          distgrid=distgrid, &
          indexflag=ESMF_INDEX_GLOBAL, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_StateAdd (state2, (/array1/), rc=rc)
    end if
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating PET 0 Array for rereconcile tests"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateReconcile (state2, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling initial reconcile for rereconcile tests"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateGet (state2, itemName=array1name,  &
        array=array1_alternate, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "PET", localpet, ": Calling StateGet to access proxies"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    if (localPet == 0) then
      call ESMF_StateRemove (state2, itemNameList=(/array1name/), rc=rc)
    end if
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Removing non-proxy item test"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateReconcile (state2, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Re-reconciling State test"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateGet (state2, itemName=array1name,  &
        array=array2, rc=rc)
    write(failMsg, *) "Returned ESMF_SUCCESS by mistake"
    write(name, *) "Checking for empty State tests"
    call ESMF_Test((rc /= ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------


    !-------------------------------------------------------------------------
    ! Test accessing nested State items after reconcile
    !-------------------------------------------------------------------------

    ! Nested State items will be created on PET 0.  After reconciliation,
    ! the other PETs will have a proxy nested State.

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    state3 = ESMF_StateCreate (name='state3', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateCreate for top-level State tests"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    if (localPET /= 0) then
      rc = ESMF_SUCCESS
    end if
    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    if (localPET == 0) then
      state_nested = ESMF_StateCreate (name='state_nested', rc=rc)
    end if
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateCreate for nested State tests"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    if (localPET == 0) then
      field_nested = ESMF_FieldEmptyCreate (name='nested Field', rc=rc)
    end if
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling FieldEmptyCreate for nested State test"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    if (localPET == 0) then
      call ESMF_StateAdd (state_nested, fieldList=(/ field_nested /), rc=rc)
    end if
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateAdd of nested Field test"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    if (localPET == 0) then
      call ESMF_StateAdd (state3, nestedStateList=(/ state_nested /), rc=rc)
      ! call ESMF_StatePrint (state3, nestedFlag=.true.)
    end if
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateAdd of nested State test"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateReconcile (state=state3, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateReconcile of nested State test"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

   ! PETs 1-n should now be able to access the nested Field proxy item

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateGet (state=state3,  &
        itemname='state_nested/nested Field',  &
        field=field_dummy,  &
        rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateGet of nested Field test"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateGet (state=state3,  &
        itemname='state_nested/nested Field_badname',  &
        field=field_dummy,  &
        rc=rc)
    write(failMsg, *) "Incorrectly returned ESMF_SUCCESS"
    write(name, *) "Calling StateGet of nested Field with wrong name test"
    call ESMF_Test((rc /= ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    ! Test with attribute reconcile turned on
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    statename = 'state with attributes'
    state_attr = ESMF_StateCreate (name=statename, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a State for attributes"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Reconciling state with Attribute reconciling turned on"
    call ESMF_StateReconcile (state_attr, attreconflag=ESMF_ATTRECONCILE_ON, rc=rc)
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Adding an Attribute to the Base test"
    write (localpet_str, '(i4)') localPet
    call ESMF_AttributeSet (state_attr,  &
        name='Base PET ' // trim (adjustl (localpet_str)),  &
        valueList=(/ localPet /),  &
        rc=rc)
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Reconciling state with Base attribute test"
    call ESMF_StateReconcile (state_attr, attreconflag=ESMF_ATTRECONCILE_ON, rc=rc)
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Verifying reconciled Base attributes test"
    write (temppet_str, '(i4)') localPet
    call ESMF_AttributeGet (state_attr,  &
      name='Base PET ' // trim (adjustl (temppet_str)),  &
      valueList=attr_val,  &
      rc=rc)
    if (attr_val(1) /= localPet) rc = ESMF_FAILURE
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Adding Field to State on PET 0 test"
    rc = ESMF_SUCCESS
    if (localPet == 0) then
      do, i=1, size (field_attr)
        write (fieldname, '(a,i4)') 'PET 0 Field', i
        field_attr(i) = ESMF_FieldEmptyCreate (name=fieldname, rc=rc)
        if (rc /= ESMF_SUCCESS) go to 50

        call ESMF_AttributeSet (field_attr(i),  &
            name=trim (fieldname) // ' attribute',  &
            valueList=(/ i /),  &
            rc=rc)
        if (rc /= ESMF_SUCCESS) go to 50
      end do

      call ESMF_StateAdd (state_attr, field_attr, rc=rc)
      if (rc /= ESMF_SUCCESS) go to 50
    end if

50 continue
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Reconciling state with Field and Attribute test"
    call ESMF_StateReconcile (state_attr, attreconflag=ESMF_ATTRECONCILE_ON, rc=rc)
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Accessing reconciled Fields test"
    do, i=1, size (field_attr_new)
      write (fieldname, '(a,i4)') 'PET 0 Field', i
      call ESMF_StateGet (state_attr,  &
          itemName=fieldname, field=field_attr_new(i),  &
          rc=rc)
      if (rc /= ESMF_SUCCESS) exit
    end do
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Accessing reconciled Field Attributes test"
    do, i=1, size (field_attr_new)
      write (fieldname, '(a,i4)') 'PET 0 Field', i
      call ESMF_AttributeGet (field_attr_new(i),  &
          name=trim (fieldname) // ' attribute',  &
          value=attr_val(1),  &
          rc=rc)
      if (rc /= ESMF_SUCCESS) exit
      if (attr_val(1) /= i) then
        rc = ESMF_FAILURE
        exit
      end if
    end do
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Create a FieldBundle with Attributes on PET 0 test"
    fb_name = 'my fields'
    if (localPet == 0) then
      fb_attr = ESMF_FieldBundleCreate (  &
          fieldList=(/field_attr/),  &
          name=fb_name,  &
          rc=rc)
      if (rc /= ESMF_SUCCESS) go to 55

      call ESMF_StateAdd (state_attr, (/ fb_attr /), rc=rc)
      if (rc /= ESMF_SUCCESS) go to 55
    else
      rc = ESMF_SUCCESS
    end if

 55 continue
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Reconciling state with FieldBundle and Attributes test"
    call ESMF_StateReconcile (state_attr, attreconflag=ESMF_ATTRECONCILE_ON, rc=rc)
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Access reconciled FieldBundle test"
    call ESMF_StateGet (state_attr,  &
        itemName=fb_name,  &
        fieldBundle=fb_attr_new,  &
        rc=rc)
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!-------------------------------------------------------------------------
!   Fields with shared Grids
!-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    comp1name = "Atmosphere"
    comp1_sg = ESMF_GridCompCreate(name=comp1name, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component for shared grid tests"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    comp2name = "Ocean"
    comp2_sg = ESMF_GridCompCreate(name=comp2name, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component for shared grid tests"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Create State for shared Grid test"
    state_sgrid = ESMF_StateCreate(name="Atm-Ocn", rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    ! In SetServices() the VM for each component is initialized.
    ! Normally you would call SetEntryPoint inside set services,
    ! but to make this test very short, they are called inline below.

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp1_sg, userRoutine=comp_dummy, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices for shared grid tests"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp2_sg, userRoutine=comp_dummy, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices for shared grid tests"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp1_sg, ESMF_METHOD_INITIALIZE, &
      userRoutine=comp1_sg_init, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint for shared grid tests"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp2_sg, ESMF_METHOD_INITIALIZE, &
      userRoutine=comp2_sg_init, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint for shared grid tests"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp1_sg, ESMF_METHOD_FINALIZE, &
      userRoutine=comp1_sg_final, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint for shared grid tests"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp2_sg, ESMF_METHOD_FINALIZE, &
      userRoutine=comp2_sg_final, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint for shared grid tests"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompInitialize(comp1_sg, importState=state_sgrid, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize for shared grid tests"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompInitialize(comp2_sg, importState=state_sgrid, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize for shared grid tests"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Reconcile State for shared Grid test"
    call ESMF_StateReconcile (state_sgrid, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Access Proxy Field 1 test"
!    if (any (localPet == [2,3])) then
      call ESMF_StateGet (state_sgrid, 'Field_sg1', field=field_sg1, rc=rc)
!    else
!      rc = ESMF_SUCCESS
!    end if
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Access Proxy Field 2 test"
!    if (any (localPet == [2,3])) then
      call ESMF_StateGet (state_sgrid, 'Field_sg2', field=field_sg2, rc=rc)
!    else
!      rc = ESMF_SUCCESS
!    end if
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Access shared Grid 1 test"
    call ESMF_FieldGet (field_sg1, grid=grid_sg1, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Access shared Grid 2 test"
    call ESMF_FieldGet (field_sg2, grid=grid_sg2, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Compare shared grids test"
    rc = merge (ESMF_SUCCESS, ESMF_FAILURE, grid_sg1 == grid_sg2)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Re-reconcile State for shared Grid test"
    call ESMF_StateReconcile (state_sgrid, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompFinalize(comp1_sg, importState=state_sgrid, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompFinalize for shared Grid test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompFinalize(comp2_sg, importState=state_sgrid, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompFinalize for shared Grid test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompDestroy(comp1_sg, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompDestroy for shared Grid test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompDestroy(comp2_sg, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompDestroy for shared Grid test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateDestroy(state_sgrid, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateDestroy for shared Grid test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
10  continue

    call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize()

end program ESMF_StateReconcileUTest
