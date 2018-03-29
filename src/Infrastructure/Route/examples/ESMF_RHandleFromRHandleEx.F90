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

!==============================================================================
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================

module compAmod
  use ESMF
  implicit none
  private
  public SetServices
  
 contains
 
  subroutine SetServices(comp, rc)
    type(ESMF_GridComp)   :: comp
    integer, intent(out)  :: rc
    !
    rc = ESMF_SUCCESS
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, &
      userRoutine=Initialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  
  subroutine Initialize(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    ! local variables
    integer               :: petCount
    type(ESMF_Grid)       :: grid
    type(ESMF_Field)      :: field
    !
    rc = ESMF_SUCCESS
    call ESMF_GridCompGet(comp, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    grid = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/180, 160/), &
      minCornerCoord=(/0._ESMF_KIND_R8, -80._ESMF_KIND_R8/), &
      maxCornerCoord=(/360._ESMF_KIND_R8, 80._ESMF_KIND_R8/), &
      staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
      regDecomp=(/petCount,1/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name="fieldA", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateAdd(exportState, (/field/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldFill(field, dataFillScheme="one", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#if 0
    call ESMF_FieldWrite(field, fileName="srcField.nc", &
      status=ESMF_FILESTATUS_REPLACE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
  end subroutine
  
  subroutine Finalize(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    ! local variables
    integer               :: petCount
    type(ESMF_Grid)       :: grid
    type(ESMF_Field)      :: field
    !
    rc = ESMF_SUCCESS
    call ESMF_StateGet(exportState, field=field, itemName="fieldA", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(field, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldDestroy(field, noGarbage=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridDestroy(grid, noGarbage=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine

end module compAmod

!-------------------------------------------------------------------------------

module compBmod
  use ESMF
  implicit none
  private
  public SetServices
  
 contains
 
  subroutine SetServices(comp, rc)
    type(ESMF_GridComp)   :: comp
    integer, intent(out)  :: rc
    !
    rc = ESMF_SUCCESS
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, &
      userRoutine=Initialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  
  subroutine Initialize(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    ! local variables
    integer               :: petCount
    type(ESMF_Grid)       :: grid
    type(ESMF_Field)      :: field
    !
    rc = ESMF_SUCCESS
    call ESMF_GridCompGet(comp, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    grid = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/180, 160/), &
      minCornerCoord=(/0._ESMF_KIND_R8, -80._ESMF_KIND_R8/), &
      maxCornerCoord=(/360._ESMF_KIND_R8, 80._ESMF_KIND_R8/), &
      staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
      regDecomp=(/petCount,1/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name="fieldB", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateAdd(importState, (/field/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  
  subroutine Finalize(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    ! local variables
    integer               :: petCount
    type(ESMF_Grid)       :: grid
    type(ESMF_Field)      :: field
    !
    rc = ESMF_SUCCESS
    call ESMF_StateGet(importState, field=field, itemName="fieldB", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(field, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#if 0
    call ESMF_FieldWrite(field, fileName="dstField.nc", &
      status=ESMF_FILESTATUS_REPLACE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
    call ESMF_FieldDestroy(field, noGarbage=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridDestroy(grid, noGarbage=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine

end module compBmod


!-------------------------------------------------------------------------------


program ESMF_RHandleFromRHandleEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  use compAmod, only: ssA => SetServices
  use compBmod, only: ssB => SetServices
  
  implicit none
  
  ! local variables
  integer                 :: rc, urc
  type(ESMF_VM)           :: vm
  integer                 :: i
  integer                 :: petCount, petCountR, petCountA, petCountB1
  integer, allocatable    :: petListA(:), petListB1(:), petListB2(:)
  integer, allocatable    :: originPetList(:), targetPetList(:)
  type(ESMF_State)        :: stateAB1, stateAB2
  type(ESMF_GridComp)     :: compA, compB1, compB2
  type(ESMF_Field)        :: fieldA, fieldB1, fieldB2
  type(ESMF_RouteHandle)  :: rh1, rh2
  
  integer(ESMF_KIND_I4), pointer :: testArray(:)


  ! result code
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg
  
  
  finalrc = ESMF_SUCCESS

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_RHandleFromRHandleEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_Initialize(vm=vm, defaultlogfilename="RHandleFromRHandleEx.Log", &
    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (petCount < 4) then
    finalrc = ESMF_FAILURE
    goto 10
  endif
  
!BOE
! \subsubsection{Creating a RouteHandle from an existing RouteHandle -- 
! Transfer to a different set of PETs}
! \label{RH:RHfromRH}
!
! \begin{sloppypar}
! Typically a RouteHandle object is created indirectly, i.e. without explicitly
! calling the {\tt ESMF\_RouteHandleCreate()} method. The RouteHandle
! object is a byproduct of calling communication Store() methods like 
! {\tt ESMF\_FieldRegridStore()}. 
! \end{sloppypar}
!
! The exception to this rule is when creating a duplicate RouteHandle from an
! existing RouteHandle object. In this case the {\tt ESMF\_RouteHandleCreate()}
! method is used explicitly. While this method allows to create a duplicate 
! RouteHandle on the exact same set of PETs as the original RouteHandle, the 
! real purpose of duplication is that of being able to transfer a precomputed
! RouteHandle to a different set of PETs. This is an efficient way to reduce
! the time spent in Store() calls, if the same communication pattern repeats
! for multiple components.
!
! This example demonstrates the transfer of a RouteHandle from one set of PETs
! to another by first introducing three components. Component A is defined
! on the first half of available PETs.
!EOE

  stateAB1 = ESMF_StateCreate(rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  stateAB2 = ESMF_StateCreate(rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  petCountA = petCount/2  ! component A gets half the PETs

  allocate(petListA(petCountA))
  do i=1, petCountA
    petListA(i) = i-1 ! PETs are base 0
  enddo
  
  compA = ESMF_GridCompCreate(petList=petListA, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC

!BOE
! The other two components, B1 and B2, split the remaining PETs evenly.
!EOE

!BOC
  petCountR = petCount - petCountA
  petCountB1 = petCountR / 2
  
  allocate(petListB1(petCountB1))
  do i=1, petCountB1
    petListB1(i) = petCountA + i-1 ! PETs are base 0
  enddo

  allocate(petListB2(petCountR-petCountB1))
  do i=1, petCountR-petCountB1
    petListB2(i) = petCountA + petCountB1 + i-1 ! PETs are base 0
  enddo

  compB1 = ESMF_GridCompCreate(petList=petListB1, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  compB2 = ESMF_GridCompCreate(petList=petListB2, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC

  call ESMF_GridCompSetServices(compA, ssA, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(compB1, ssB, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(compB2, ssB, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompInitialize(compA, exportState=stateAB1, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompInitialize(compB1, importState=stateAB1, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_GridCompInitialize(compB2, importState=stateAB2, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_StateReconcile(stateAB1, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateReconcile(stateAB2, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateGet(stateAB1, field=fieldA, itemName="fieldA", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_StateGet(stateAB1, field=fieldB1, itemName="fieldB", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateGet(stateAB2, field=fieldB2, itemName="fieldB", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Skipping all of the standard superstructure code, assume that {\tt fieldA}
! has been created by component A, has been reconciled across all PETs via
! a StateReconcile() call, and accessed via a StateGet(). The same is true for
! {\tt fieldB1} and {\tt fieldB2} from components B1 and B2, respectively.
!
! Now the RouteHandle {\tt rh1} for a Redist operation is precomputed between 
! {\tt fieldA} and {\tt fieldB1}.
!EOE

!BOC
  call ESMF_FieldRedistStore(srcField=fieldA, dstField=fieldB1, &
    routehandle=rh1, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC

!BOE
! The communication pattern stored in {\tt rh1} is between the PETs associated
! with component A and those associated with component B1. Now component B2 is
! simply a second instance of the same component code as B1, but on a different
! set of PETs. The {\tt ESMF\_RouteHandleCreate()} method can be used to 
! transfer {\tt rh1} to the set of PETs that is consistent with fieldA to 
! fieldB2 communication.
!
! In order to transfer a RouteHandle to a different set of PETs, the 
! {\tt originPetList} and {\tt targetPetList} must be constructed. The
! {\tt originPetList} is the union of source and destination PETs (in that
! order) for which {\tt rh1} was explicitly computed via the Store() call:
!EOE

!BOC
  allocate(originPetList(size(petListA)+size(petListB1)))
  originPetList(1:size(petListA)) = petListA(:)
  originPetList(size(petListA)+1:) = petListB1(:)
!EOC

!BOE
! The {\tt targetPetList} is the union of source and destination PETs (in that
! order) for which the target RouteHandle (i.e. {\tt rh2}) will be defined:
!EOE

!BOC
  allocate(targetPetList(size(petListA)+size(petListB2)))
  targetPetList(1:size(petListA)) = petListA(:)
  targetPetList(size(petListA)+1:) = petListB2(:)
!EOC

!BOE
! Now the new RouteHandle {\tt rh2} can be created easily from the exising 
! RouteHandle {\tt rh1}, suppling the origin and target petLists.
!EOE

!BOC
  rh2 = ESMF_RouteHandleCreate(rh1, originPetList=originPetList, &
    targetPetList=targetPetList, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC
  deallocate(originPetList, targetPetList)

  call ESMF_FieldFill(fieldA, dataFillScheme="sincos", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldFill(fieldB1, dataFillScheme="one", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldFill(fieldB2, dataFillScheme="one", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The new RouteHandle {\tt rh2} is completely independent of the original
! RouteHandle. In fact, it is perfectly fine to destroy (or release) {\tt rh1} 
! while holding on to {\tt rh2}.
!EOE

!BOC
  call ESMF_RouteHandleDestroy(rh1, noGarbage=.true., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC

!BOE
! Finally the {\tt rh2} object can be used to redistribute data from 
! {\tt fieldA} to {\tt fieldB2}. 
!EOE

!BOC
  call ESMF_FieldRedist(srcField=fieldA, dstField=fieldB2, &
    routehandle=rh2, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC

!BOE
! The communication pattern held by {\tt rh2}
! is idential to what whould have been created by an explicit 
! {\tt ESMF\_FieldRedistStore()} call. However, the 
! {\tt ESMF\_RouteHandleCreate()} call used to create {\tt rh2} from {\tt rh1}
! is much faster than the full RedistStore() operation.
!EOE

  call ESMF_RouteHandleDestroy(rh2, noGarbage=.true., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompFinalize(compA, exportState=stateAB1, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompFinalize(compB1, importState=stateAB1, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  call ESMF_GridCompFinalize(compB2, importState=stateAB2, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompDestroy(compA, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompDestroy(compB1, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompDestroy(compB2, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateDestroy(stateAB1, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateDestroy(stateAB2, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  deallocate(petListA)
  deallocate(petListB1)
  deallocate(petListB2)

10 continue

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_RHandleFromRHandleEx.F90"
  else
    print *, "FAIL: ESMF_RHandleFromRHandleEx.F90"
  endif

end program
