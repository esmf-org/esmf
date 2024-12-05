! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!============================================================================== 
!
#define FILENAME "src/Superstructure/StateReconcile/tests/ESMF_StateReconcileUTest.F90"

#define FB_MEMBER_COUNT 3

module ESMF_StateReconcileUTest_Mod
use ESMF
implicit none

contains

subroutine comp_dummy(gcomp, rc)
   type(ESMF_GridComp) :: gcomp
   integer, intent(out) :: rc

   rc = ESMF_SUCCESS
end subroutine comp_dummy


! Initialize routine that sets "istate" on PETs 0 and 1
subroutine comp1_init(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: istate, ostate
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Field)      :: field
    type(ESMF_State)      :: neststate
    type(ESMF_FieldBundle):: fb
    type(ESMF_Grid)       :: grid
    integer               :: i
    character(40)         :: fieldName

    print *, "i am comp1_init"

    rc = ESMF_SUCCESS

    field = ESMF_FieldEmptyCreate(name="Comp1 Field", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_StateAdd(istate, (/field/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    neststate = ESMF_StateCreate(name="Comp1 Nested State", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_StateAdd(istate, (/neststate/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    field = ESMF_FieldEmptyCreate(name="Comp1 Field in Nested State", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_StateAdd(neststate, (/field/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    fb = ESMF_FieldBundleCreate(name="Comp1 FieldBundle", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    grid = ESMF_GridCreateNoPeriDim(name="Comp1 Grid", regDecomp=(/2,1/), &
      minIndex=(/1,1/), maxIndex=(/10,20/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    do i=1,FB_MEMBER_COUNT
      write(fieldName,'("Comp1 Field ", I2, " in FB")') i

      field = ESMF_FieldCreate(name=trim(fieldName), grid=grid, &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      call ESMF_FieldBundleAdd(fb, (/field/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    enddo

    call ESMF_StateAdd(istate, (/fb/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! store the grid inside gcomp to provide access during run method for check
    call ESMF_GridCompSet(gcomp, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

end subroutine comp1_init

! Initialize routine that sets "istate" on PETs 2 and 3
subroutine comp2_init(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: istate, ostate
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Field)      :: field
    type(ESMF_FieldBundle):: fb
    type(ESMF_Grid)       :: grid
    integer               :: i
    character(40)         :: fieldName

    print *, "i am comp2_init"

    rc = ESMF_SUCCESS

    field = ESMF_FieldEmptyCreate(name="Comp2 Field", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_StateAdd(istate, (/field/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    fb = ESMF_FieldBundleCreate(name="Comp2 FieldBundle", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    grid = ESMF_GridCreateNoPeriDim(name="Comp2 Grid", regDecomp=(/1,2/), &
      minIndex=(/1,1/), maxIndex=(/20,10/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    do i=1,FB_MEMBER_COUNT
      write(fieldName,'("Comp2 Field ", I2, " in FB")') i

      field = ESMF_FieldCreate(name=trim(fieldName), grid=grid, &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      call ESMF_FieldBundleAdd(fb, (/field/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    enddo

    call ESMF_StateAdd(istate, (/fb/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! store the grid inside gcomp to provide access during run method for check
    call ESMF_GridCompSet(gcomp, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

end subroutine comp2_init

! Run routine to tests "istate" on PETs 0 and 1
subroutine comp1_run(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: istate, ostate
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Field)      :: field
    type(ESMF_FieldBundle):: fb
    type(ESMF_Grid)       :: grid, gridCheck
    integer               :: i
    character(40)         :: fieldName

    print *, "i am comp1_run"

    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(gcomp, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_StateGet(istate, "Comp1 FieldBundle", fb, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    do i=1,FB_MEMBER_COUNT
      write(fieldName,'("Comp1 Field ", I2, " in FB")') i

      call ESMF_FieldBundleGet(fb, fieldName=trim(fieldName), field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      call ESMF_FieldGet(field, grid=gridCheck, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      if (gridCheck /= grid) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, &
          msg="The actual grid object has been changed!", &
          line=__LINE__, file=FILENAME, &
          rcToReturn=rc)
        return
      endif
    enddo

end subroutine comp1_run

! Run routine to tests "istate" on PETs 2 and 3
subroutine comp2_run(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: istate, ostate
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Field)      :: field
    type(ESMF_FieldBundle):: fb
    type(ESMF_Grid)       :: grid, gridCheck
    integer               :: i
    character(40)         :: fieldName

    print *, "i am comp2_run"

    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(gcomp, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_StateGet(istate, "Comp2 FieldBundle", fb, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    do i=1,FB_MEMBER_COUNT
      write(fieldName,'("Comp2 Field ", I2, " in FB")') i

      call ESMF_FieldBundleGet(fb, fieldName=trim(fieldName), field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      call ESMF_FieldGet(field, grid=gridCheck, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      if (gridCheck /= grid) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, &
          msg="The actual grid object has been changed!", &
          line=__LINE__, file=FILENAME, &
          rcToReturn=rc)
        return
      endif
    enddo

end subroutine comp2_run

! Initialize routine that destroys members of "istate" on PETs 0 and 1
subroutine comp1_final(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: istate, ostate
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Field) :: field
    type(ESMF_State) :: neststate
    type(ESMF_FieldBundle):: fb
    type(ESMF_Grid)       :: grid

    print *, "i am comp1_final"

    call ESMF_StateGet(istate, "Comp1 Field", field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_FieldDestroy(field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_StateGet(istate, "Comp1 Nested State", neststate, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_StateGet(neststate, "Comp1 Field in Nested State", field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_FieldDestroy(field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_StateDestroy(neststate, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_StateGet(istate, "Comp1 FieldBundle", fb, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! For now do not worry about cleaning up fields inside FB

    call ESMF_FieldBundleDestroy(fb, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

end subroutine comp1_final

! Initialize routine that destroys members of "istate" on PETs 0 and 1
subroutine comp2_final(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: istate, ostate
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Field) :: field
    type(ESMF_FieldBundle):: fb
    type(ESMF_Grid)       :: grid

    print *, "i am comp2_final"

    call ESMF_StateGet(istate, "Comp2 Field", field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_FieldDestroy(field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_StateGet(istate, "Comp2 FieldBundle", fb, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! For now do not worry about cleaning up fields inside FB

    call ESMF_FieldBundleDestroy(fb, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

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

    rc = ESMF_SUCCESS

print *, 'comp1_sg_init: creating grid1'
    grid1 = ESMF_GridCreateNoPeriDim(  &
        minIndex=(/1,1/), maxIndex=(/10,20/),  &
        regDecomp=(/1,2/), name="shared Grid", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

print *, 'comp1_sg_init: creating field1'
    field1 = ESMF_FieldCreate(grid1, typekind=ESMF_TYPEKIND_R4, &
        indexflag=ESMF_INDEX_DELOCAL, &
        staggerloc=ESMF_STAGGERLOC_CENTER, name="Field_sg1", rc=rc)
!    field1 = ESMF_FieldEmptyCreate(name="Field_sg1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

print *, 'comp1_sg_init: creating field2'
    field2 = ESMF_FieldCreate(grid1, typekind=ESMF_TYPEKIND_R4, &
        indexflag=ESMF_INDEX_DELOCAL, &
        staggerloc=ESMF_STAGGERLOC_CENTER, name="Field_sg2", rc=rc)
!    field2 = ESMF_FieldEmptyCreate(name="Field_sg2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

print *, 'comp1_sg_init: adding fields to istate'
    call ESMF_StateAdd(istate, (/field1, field2/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

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

    call ESMF_StateGet(istate, "Field_sg1", field1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_FieldDestroy(field1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_StateGet(istate, "Field_sg2", field2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_FieldDestroy(field2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

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

!------------------------------------------------------------------------------

! Array added to exportState

subroutine comp1_init_simple(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: istate, ostate
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Distgrid)  :: distgrid
    type(ESMF_Array)     :: array

    rc = ESMF_SUCCESS

    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/15,23/), &
      regDecomp=(/2,2/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    array = ESMF_ArrayCreate(arrayspec=arrayspec, &
      name="Array_for_reconciling",  distgrid=distgrid, &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_StateAdd (ostate, [array], rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

end subroutine comp1_init_simple

!------------------------------------------------------------------------------

! Nested State with Field added to exportState

subroutine comp1_init_nested(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: istate, ostate
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_State)     :: state_nested
    type(ESMF_Field)     :: field_nested

    rc = ESMF_SUCCESS

    state_nested = ESMF_StateCreate(name='state_nested', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    field_nested = ESMF_FieldEmptyCreate(name='nested Field', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_StateAdd(state_nested, fieldList=[field_nested], rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_StateAdd(ostate, nestedStateList=[state_nested], rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

end subroutine comp1_init_nested

!------------------------------------------------------------------------------

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
    integer :: rc, urc
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
    type(ESMF_Grid)      :: grid_shared, grid_sg1, grid_sg2
    type(ESMF_VM)        :: vm, vm0
    character(len=ESMF_MAXSTR) :: comp1name, comp2name, statename, fieldname
    character(len=ESMF_MAXSTR) :: array1name
    character(len=ESMF_MAXSTR) :: fb_name
    character(4) :: localpet_str, temppet_str
    integer :: attr_val(1)
    integer :: i

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
    integer :: result = 0, localPet, petCount, testPet

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

    ! In SetServices() the VM for each component is initialized.
    ! Normally you would call SetEntryPoint inside set services,
    ! but to make this test very short, they are called inline below.

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp1, userRoutine=comp_dummy, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp2, userRoutine=comp_dummy, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

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
    call ESMF_GridCompSetEntryPoint(comp1, ESMF_METHOD_RUN, &
      userRoutine=comp1_run, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp2, ESMF_METHOD_RUN, &
      userRoutine=comp2_run, rc=rc)
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

call ESMF_StateLog(state1, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompInitialize(comp1, importState=state1, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

call ESMF_StateLog(state1, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompInitialize(comp2, importState=state1, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

call ESMF_StateLog(state1, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateReconcile(state1, vm=vm, checkflag=.true., rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateReconcile in concurrent mode"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

call ESMF_StateLog(state1, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompRun(comp1, importState=state1, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompRun"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompRun(comp2, importState=state1, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompRun"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! Test redundant reconcile
    call ESMF_StateReconcile(state1, vm=vm, checkflag=.true., rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling 2nd StateReconcile in concurrent mode"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

call ESMF_StateLog(state1, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompRun(comp1, importState=state1, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompRun"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompRun(comp2, importState=state1, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompRun"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate on 2nd StateReconcile"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompFinalize(comp1, importState=state1, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompFinalize"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompFinalize(comp2, importState=state1, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompFinalize"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

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
    call ESMF_GridCompSetServices(comp1, userRoutine=comp_dummy, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp2, userRoutine=comp_dummy, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

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
    call ESMF_GridCompInitialize(comp1, importState=state1, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompInitialize(comp2, importState=state1, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateReconcile(state1, vm=vm, checkflag=.true., rc=rc)
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
    call ESMF_StateReconcile(state1, vm=vm, checkflag=.true., rc=rc)
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
    call ESMF_GridCompFinalize(comp1, importState=state1, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompFinalize"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompFinalize(comp2, importState=state1, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompFinalize"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

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
    call ESMF_GridCompSetServices(comp1, userRoutine=comp_dummy, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp2, userRoutine=comp_dummy, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

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
    call ESMF_GridCompInitialize(comp1, importState=state1, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompInitialize(comp2, importState=state1, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateValidate(state1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateValidate"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateReconcile(state1, vm=vm, checkflag=.true., rc=rc)
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
    call ESMF_StateReconcile(state1, vm=vm, checkflag=.true., rc=rc)
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
    call ESMF_GridCompFinalize(comp1, importState=state1, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompFinalize"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompFinalize(comp2, importState=state1, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompFinalize"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

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
    state2 = ESMF_StateCreate()
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateCreate for rereconcile tests"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! component on PET 0
    comp1 = ESMF_GridCompCreate(petList=[0], rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    ! In SetServices() the VM for each component is initialized.
    ! Normally you would call SetEntryPoint inside set services,
    ! but to make this test very short, they are called inline below.

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp1, userRoutine=comp_dummy, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp1, ESMF_METHOD_INITIALIZE, &
      userRoutine=comp1_init_simple, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! creating and adding array to the exportState on PET 0
    call ESMF_GridCompInitialize(comp1, exportState=state2, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

   !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateReconcile(state2, checkflag=.true., rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling initial reconcile for rereconcile tests"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    array1name = "Array_for_reconciling"
    call ESMF_StateGet(state2, itemName=array1name, array=array1_alternate, &
      rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "PET", localpet, ": Calling StateGet to access proxies"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    if (localPet == 0) then
      call ESMF_StateRemove(state2, itemNameList=[array1name], rc=rc)
    end if
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Removing non-proxy item test"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateReconcile(state2, checkflag=.true., rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Re-reconciling State test"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateGet(state2, itemName=array1name, array=array2, rc=rc)
    write(failMsg, *) "Returned ESMF_SUCCESS by mistake"
    write(name, *) "Checking for empty State tests"
    call ESMF_Test((rc /= ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateDestroy(state2, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "StateDestroy test"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompDestroy(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "GridCompDestroy test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------


    !-------------------------------------------------------------------------
    ! Test accessing nested State items after reconcile
    !-------------------------------------------------------------------------

    ! Nested State items will be created on PET 0.  After reconciliation,
    ! the other PETs will have a proxy nested State.

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    state3 = ESMF_StateCreate(name='state3', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateCreate for rereconcile tests"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! component on PET 0
    comp1 = ESMF_GridCompCreate(petList=[0], rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    ! In SetServices() the VM for each component is initialized.
    ! Normally you would call SetEntryPoint inside set services,
    ! but to make this test very short, they are called inline below.

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp1, userRoutine=comp_dummy, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetEntryPoint(comp1, ESMF_METHOD_INITIALIZE, &
      userRoutine=comp1_init_nested, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetEntryPoint"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! creating and adding nested state with field to the exportState on PET 0
    call ESMF_GridCompInitialize(comp1, exportState=state3, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateReconcile(state=state3, checkflag=.true., rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateReconcile of nested State test"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    ! PETs 1-n should now be able to access the nested Field proxy item

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateGet(state=state3,  &
        itemname='state_nested/nested Field',  &
        field=field_dummy,  &
        rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling StateGet of nested Field test"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateGet(state=state3,  &
        itemname='state_nested/nested Field_badname',  &
        field=field_dummy,  &
        rc=rc)
    write(failMsg, *) "Incorrectly returned ESMF_SUCCESS"
    write(name, *) "Calling StateGet of nested Field with wrong name test"
    call ESMF_Test((rc /= ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_StateDestroy(state3, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "StateDestroy test"
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompDestroy(comp1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "GridCompDestroy test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    ! Test attribute reconcililiation
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! component on PET 0 -> needed to create Field below on PET 0
    comp1 = ESMF_GridCompCreate(petList=[0], rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Gridded Component"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    ! In SetServices() the VM for each component is initialized.

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp1, userRoutine=comp_dummy, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Access the VM that has one PET on 0"
    call ESMF_GridCompGet(comp1, vm=vm0, rc=rc)
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

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
    write(name, *) "Reconciling state before adding Attributes"
    call ESMF_StateReconcile(state_attr, checkflag=.true., rc=rc)
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
    ! use explicit checkflag=.false, because state Info values will not be
    ! consistent across PETs... and in case we force default .true. for
    ! development, make sure this is explicitly set to .false. here
    call ESMF_StateReconcile (state_attr, checkflag=.false., rc=rc)
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Verifying StateReconcile does not mess with Base attributes"
    write (temppet_str, '(i4)') localPet
    call ESMF_AttributeGet (state_attr,  &
      name='Base PET ' // trim (adjustl (temppet_str)),  &
      valueList=attr_val, rc=rc)
    call ESMF_Test((attr_val(1)==localPet), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "ReconcileExchgAttributes to exchange State attributes test"
    call ESMF_ReconcileExchgAttributes(state_attr, vm, rc=rc)
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Verifying exchange reconciled Base attributes test"
    testPet = mod(localPet+1, petCount)
    write (temppet_str, '(i4)') testPet
    call ESMF_AttributeGet (state_attr,  &
      name='Base PET ' // trim (adjustl (temppet_str)),  &
      valueList=attr_val,  &
      rc=rc)
    if (attr_val(1) /= testPet) rc = ESMF_FAILURE
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Adding Field to State on PET 0 test"
    rc = ESMF_SUCCESS
    if (localPet == 0) then
      do, i=1, size (field_attr)
        write (fieldname, '(a,i4)') 'PET 0 Field', i
        ! create on vm0 to not violate ESMF unison rule for Create() methods
        field_attr(i) = ESMF_FieldEmptyCreate(name=fieldname, vm=vm0, rc=rc)
        if (rc /= ESMF_SUCCESS) goto 50

        call ESMF_AttributeSet (field_attr(i),  &
            name=trim (fieldname) // ' attribute',  &
            valueList=(/ i /),  &
            rc=rc)
        if (rc /= ESMF_SUCCESS) goto 50
      end do

      call ESMF_StateAdd (state_attr, field_attr, rc=rc)
      if (rc /= ESMF_SUCCESS) goto 50
    end if

50 continue
    call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Reconciling state with Field and Attribute test"
    call ESMF_StateReconcile (state_attr, checkflag=.true., rc=rc)
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
          valueList=attr_val,  &
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
    call ESMF_GridCompDestroy(comp1, rc=rc) ! also shuts down vm0
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "GridCompDestroy test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

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
    call ESMF_GridCompSetServices(comp1_sg, userRoutine=comp_dummy, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompSetServices(comp2_sg, userRoutine=comp_dummy, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompSetServices"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

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
    call ESMF_GridCompInitialize(comp1_sg, importState=state_sgrid, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompInitialize(comp2_sg, importState=state_sgrid, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompInitialize"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Reconcile State for shared Grid test"
    call ESMF_StateReconcile (state_sgrid, checkflag=.true., rc=rc)
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
    call ESMF_StateReconcile (state_sgrid, checkflag=.true., rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompFinalize(comp1_sg, importState=state_sgrid, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompFinalize"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    call ESMF_GridCompFinalize(comp2_sg, importState=state_sgrid, userrc=urc, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Calling GridCompFinalize"
    call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(urc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

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
