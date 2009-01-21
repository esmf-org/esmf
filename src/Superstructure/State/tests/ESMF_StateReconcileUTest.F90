! $Id: ESMF_StateReconcileUTest.F90,v 1.16.2.3 2009/01/21 21:25:25 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!============================================================================== 
!


module ESMF_StateReconcileUTest_Mod
use ESMF_Mod

contains

! Initialize routine which creates "field1" on PETs 0 and 1
subroutine comp1_init(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp), intent(inout) :: gcomp
    type(ESMF_State), intent(inout) :: istate, ostate
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    type(ESMF_Field) :: field1
    integer :: localrc

    print *, "i am comp1_init"

    field1 = ESMF_FieldCreateNoData(name="Comp1 Field", rc=localrc)
  
    call ESMF_StateAdd(istate, field1, rc=localrc)
    
    rc = localrc

end subroutine comp1_init

! Initialize routine which creates "field2" on PETs 2 and 3
subroutine comp2_init(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp), intent(inout) :: gcomp
    type(ESMF_State), intent(inout) :: istate, ostate
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    type(ESMF_Field) :: field2
    integer :: localrc

    print *, "i am comp2_init"

    field2 = ESMF_FieldCreateNoData(name="Comp2 Field", rc=localrc)
    
    call ESMF_StateAdd(istate, field2, rc=localrc)

    rc = localrc

end subroutine comp2_init

subroutine comp_dummy(gcomp, rc)
   type(ESMF_GridComp), intent(inout) :: gcomp
   integer, intent(out) :: rc

   rc = ESMF_SUCCESS
end subroutine comp_dummy
    
subroutine StateDestroyAll(state, rc)
   type(ESMF_State), intent(inout) :: state
   integer, intent(out) :: rc

   type(ESMF_Field) :: field(100)
   integer :: i, itemCount
   character(len=ESMF_MAXSTR) :: nameList(100)

   ! assumes all items are empty fields.  if that changes, this code has
   ! to get the igrid and delete that as well (after deleting the field) and
   ! get and delete the array after, too.
   call ESMF_StateGet(state, itemCount=itemCount, itemNameList=nameList, rc=rc)
   if (rc .ne. ESMF_SUCCESS) return

   do i=1, itemCount
       call ESMF_StateGet(state, nameList(i), field(i),  rc=rc)
       if (rc .ne. ESMF_SUCCESS) return
   enddo

   call ESMF_StateDestroy(state, rc=rc)
   if (rc .ne. ESMF_SUCCESS) return
  
   do i=1, itemCount
       call ESMF_FieldDestroy(field(i),  rc=rc)
       if (rc .ne. ESMF_SUCCESS) return
   enddo

   rc = ESMF_SUCCESS
   return
end subroutine StateDestroyAll

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
    use ESMF_Mod
    use ESMF_TestMod
    use ESMF_StateReconcileUTest_Mod
    implicit none


    !-------------------------------------------------------------------------
    ! Local variables
    integer :: rc
    type(ESMF_State) :: state1
    type(ESMF_GridComp) :: comp1, comp2
    type(ESMF_VM) :: vm
    character(len=ESMF_MAXSTR) :: comp1name, comp2name, statename

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
    integer :: result = 0, localPet

    !-------------------------------------------------------------------------

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize()

    if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) goto 10


    ! Get the global VM for this job.
    call ESMF_VMGetGlobal(vm=vm, rc=rc)
    
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)

    !-------------------------------------------------------------------------
    ! exclusive component test section
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    comp1name = "Atmosphere"
    comp1 = ESMF_GridCompCreate(name=comp1name, petList=(/ 0, 1 /), rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    comp2name = "Ocean"
    comp2 = ESMF_GridCompCreate(name=comp2name, petList=(/ 2, 3 /), rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    statename = "Ocn2Atm"
    state1 = ESMF_StateCreate(statename, rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a State"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    ! In SetServices() the VM for each component is initialized.
    ! Normally you would call SetEntryPoint inside set services,
    ! but to make this test very short, they are called inline below.

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp1, comp_dummy, rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp2, comp_dummy, rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp1, ESMF_SETINIT, &
                                        comp1_init, ESMF_SINGLEPHASE, rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp2, ESMF_SETINIT, &
                                        comp2_init, ESMF_SINGLEPHASE, rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompInitialize(comp1, state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompInitialize(comp2, state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_StateReconcile(state1, vm, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateReconcile in concurrent mode"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompDestroy(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompDestroy"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompDestroy(comp2, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompDestroy"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call StateDestroyAll(state1, rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateDestroy"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    ! sequential component test section
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    comp1name = "Atmosphere"
    comp1 = ESMF_GridCompCreate(name=comp1name, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    comp2name = "Ocean"
    comp2 = ESMF_GridCompCreate(name=comp2name, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    statename = "Ocn2Atm"
    state1 = ESMF_StateCreate(statename, rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a State"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    ! In SetServices() the VM for each component is initialized.
    ! Normally you would call SetEntryPoint inside set services,
    ! but to make this test very short, they are called inline below.

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp1, comp_dummy, rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp2, comp_dummy, rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp1, ESMF_SETINIT, &
                                        comp1_init, ESMF_SINGLEPHASE, rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp2, ESMF_SETINIT, &
                                        comp2_init, ESMF_SINGLEPHASE, rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompInitialize(comp1, state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompInitialize(comp2, state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_StateReconcile(state1, vm, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateReconcile in concurrent mode"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompDestroy(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompDestroy"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompDestroy(comp2, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompDestroy"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call StateDestroyAll(state1, rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateDestroy"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------


    !-------------------------------------------------------------------------
    ! run-in-parent-context component test section
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    comp1name = "Atmosphere"
    ! TODO: add the parent VM flag here
    comp1 = ESMF_GridCompCreate(name=comp1name, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    comp2name = "Ocean"
    ! TODO: add the parent VM flag here
    comp2 = ESMF_GridCompCreate(name=comp2name, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    statename = "Ocn2Atm"
    state1 = ESMF_StateCreate(statename, rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a State"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    ! In SetServices() the VM for each component is initialized.
    ! Normally you would call SetEntryPoint inside set services,
    ! but to make this test very short, they are called inline below.

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp1, comp_dummy, rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp2, comp_dummy, rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp1, ESMF_SETINIT, &
                                        comp1_init, ESMF_SINGLEPHASE, rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp2, ESMF_SETINIT, &
                                        comp2_init, ESMF_SINGLEPHASE, rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompInitialize(comp1, state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompInitialize(comp2, state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_StateReconcile(state1, vm, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateReconcile in concurrent mode"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompDestroy(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompDestroy"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call ESMF_GridCompDestroy(comp2, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompDestroy"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
    !-------------------------------------------------------------------------
    !NEX_removeUTest_Multi_Proc_Only
    call StateDestroyAll(state1, rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateDestroy"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!-------------------------------------------------------------------------
10  continue

    call ESMF_TestEnd(result, ESMF_SRCLINE) ! calls ESMF_Finalize()

end program ESMF_StateReconcileUTest
