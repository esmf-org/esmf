! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2020, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================


module subcomp_mod

  ! modules
  use ESMF
  
  implicit none
  
  private
  
  public SetServices
    
  contains !--------------------------------------------------------------------

  subroutine SetServices(gcomp, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
    
    ! Initialize
    rc = ESMF_SUCCESS

    ! register Initialize method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=init, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! register Finaliuze method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=final, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine !--------------------------------------------------------------
  
  recursive subroutine init(gcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc
    
    ! local variables
    type(ESMF_DistGrid)     :: dg
    type(ESMF_Array)        :: array
    type(ESMF_Grid)         :: grid
    type(ESMF_Field)        :: field
    type(ESMF_FieldBundle)  :: fb
    
    ! Initialize
    rc = ESMF_SUCCESS

    dg = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/100,200/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    array = ESMF_ArrayCreate(dg, ESMF_TYPEKIND_R8, name="array1", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateAdd(estate, (/array/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    array = ESMF_ArrayCreate(dg, ESMF_TYPEKIND_R8, name="array2", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_StateAdd(estate, (/array/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    grid = ESMF_GridCreate(dg, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    field = ESMF_FieldCreate(grid=grid, typekind=ESMF_TYPEKIND_R8, &
      name="field1", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateAdd(estate, (/field/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    field = ESMF_FieldCreate(grid=grid, typekind=ESMF_TYPEKIND_R8, &
      name="field2", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateAdd(estate, (/field/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    fb = ESMF_FieldBundleCreate(name="fb", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_StateAdd(estate, (/fb/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine !--------------------------------------------------------------
  
  recursive subroutine final(gcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc

    ! local variables
    type(ESMF_DistGrid) :: dg
    type(ESMF_Array)    :: array
    
    ! Initialize
    rc = ESMF_SUCCESS

    call ESMF_StateGet(estate, "array1", array=array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_ArrayDestroy(array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateGet(estate, "array2", array=array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_ArrayGet(array, distgrid=dg, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_ArrayDestroy(array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_DistGridDestroy(dg, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine !--------------------------------------------------------------
  
end module

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

program ESMF_StateReconcileProxyUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_StateReconcileProxyUTest - Unit test 
!
! !DESCRIPTION:
!
! Test correct properties and behavior of proxy objects created during
! StateReconcile.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF
  use subcomp_mod

  implicit none

!------------------------------------------------------------------------------
  ! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  ! local variables
  integer:: i, j, rc
  integer:: petCount
  integer, allocatable  :: petList(:)
  type(ESMF_VM)         :: vm
  type(ESMF_GridComp)   :: subcomp
  type(ESMF_State)      :: exportState
  type(ESMF_Array)      :: array
  type(ESMF_DistGrid)   :: dg1, dg2
  type(ESMF_Field)      :: field, fieldRe
  type(ESMF_Grid)       :: g1, g2
  type(ESMF_FieldBundle):: fb, fbRe
  
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Create State
  exportState = ESMF_StateCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)  

  ! Get global VM info
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Construct reduced petList for sub component
  allocate(petList(petCount/2))
  j = 0
  do i=1, petCount/2
    petList(i) = j
    j = j + 2
  enddo
  
  ! Create sub component on reduced petList
  subcomp = ESMF_GridCompCreate(name="sub-component", petList=petList, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Sub component SetServices
  call ESMF_GridCompSetServices(subcomp, userRoutine=SetServices, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Sub component Initialize
  call ESMF_GridCompInitialize(subcomp, exportState=exportState, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Reconcile the State
  !NEX_UTest_Multi_Proc_Only
  call ESMF_StateReconcile(exportState, rc=rc)
  write(name, *) "Reconciling a State"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! Extract the DistGrid from the two Array objects in the State
  call ESMF_StateGet(exportState, "array1", array=array, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ArrayGet(array, distgrid=dg1, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-
  call ESMF_StateGet(exportState, "array2", array=array, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ArrayGet(array, distgrid=dg2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! NOTE: This test is disabled until DistGrid aliasing is implemented in
  ! StateReconcile.
!  ! Test whether dg1 and dg2 are aliases to the same DistGrid in memory
!  !NEX_disabled_UTest_Multi_Proc_Only
!  write(name, *) "Ensure dg1 and dg2 are aliases to the same DistGrid Test"
!  write(failMsg, *) "Found non-aliased DistGrid objects!"
!  call ESMF_Test((dg1==dg2), name, failMsg, result, ESMF_SRCLINE)

  ! Extract the Grid from the two Field objects in the State
  call ESMF_StateGet(exportState, "field1", field=field, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldGet(field, grid=g1, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-
  call ESMF_StateGet(exportState, "field2", field=field, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldGet(field, grid=g2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Test whether g1 and g2 are aliases to the same Grid in memory
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Ensure g1 and g2 are aliases to the same Grid (proxy) Test"
  write(failMsg, *) "Found non-aliased Grid (proxy) objects!"
  call ESMF_Test((g1==g2), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_StateGet(exportState, "fb", fieldbundle=fb, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Re-Reconcile the State
  !NEX_UTest_Multi_Proc_Only
  call ESMF_StateReconcile(exportState, rc=rc)
  write(name, *) "Re-Reconciling a State"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_StateGet(exportState, "field2", field=fieldRe, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Test whether field and fieldRe are aliases
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Ensure Re-Reconcile Field (proxy) persistence Test"
  write(failMsg, *) "Found non-persistent Field (proxy) objects!"
  call ESMF_Test((field==fieldRe), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_StateGet(exportState, "fb", fieldbundle=fbRe, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Test whether fb and fbRe are aliases
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Ensure Re-Reconcile FieldBundle (proxy) persistence Test"
  write(failMsg, *) "Found non-persistent FieldBundle (proxy) objects!"
  call ESMF_Test((fb==fbRe), name, failMsg, result, ESMF_SRCLINE)

  ! Sub component Finalize
  call ESMF_GridCompFinalize(subcomp, exportState=exportState, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! clean-up
  deallocate(petList)

  ! Destroy sub component
  call ESMF_GridCompDestroy(subcomp, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Destroy State
  call ESMF_StateDestroy(exportState, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !----------------------------------------------------------------

  call ESMF_TestEnd(ESMF_SRCLINE)
  
end program ESMF_StateReconcileProxyUTest
