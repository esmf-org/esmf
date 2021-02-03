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

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

program ESMF_RHandleReusabilityEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  
  implicit none
  
  ! local variables
  integer                     :: rc
  type(ESMF_VM)               :: vm
  integer                     :: petCount, localPet
  type(ESMF_RouteHandle)      :: routehandle
  type(ESMF_Grid)             :: srcGrid, dstGrid
  type(ESMF_Field)            :: srcField, dstField
  type(ESMF_Field)            :: srcField2, dstField2
  type(ESMF_Field)            :: srcField3, dstField3
  type(ESMF_Field)            :: srcField4, dstField4
  type(ESMF_Field)            :: srcField5, dstField5
  type(ESMF_Field)            :: srcField6, dstField6
  type(ESMF_FieldBundle)      :: srcFieldBundle, dstFieldBundle
  type(ESMF_DistGrid)         :: srcDistGrid, dstDistGrid
  type(ESMF_Array)            :: srcArray, dstArray
  integer                     :: i
  
  ! result code
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg
  
  finalrc = ESMF_SUCCESS

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_RHandleReusabilityEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_Initialize(vm=vm, defaultlogfilename="RHandleReusabilityEx.Log", &
    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOE
! \subsubsection{Reusablity of RouteHandles and interleaved distributed 
! and undistributed dimensions}
! \label{RH:Reusability}
!
! A RouteHandle object is typically created during a communication {\tt Store()}
! call, e.g. an {\tt ESMF\_FieldRegridStore()}. Other communication methods
! with {\tt Store()} are {\tt Halo}, {\tt Redist}, and {\tt SMM}. The primary
! input objects of a {\tt Store()} call are either Fields, Arrays,
! FieldBundles, or ArrayBundles. There will be an object for the source side,
! and another object for the destination side. Both objects must be of the
! same type.
!EOE

  ! create srcGrid
  srcGrid = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/36,16/), &
    minCornerCoord=(/0._ESMF_KIND_R8, -80._ESMF_KIND_R8/), &
    maxCornerCoord=(/360._ESMF_KIND_R8, 80._ESMF_KIND_R8/), &
    staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
    regDecomp=(/petCount,1/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOC
  srcField = ESMF_FieldCreate(srcGrid, ESMF_TYPEKIND_R8, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! fill srcField with some data
  call ESMF_FieldFill(srcField, dataFillScheme="sincos", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! create dstGrid
  dstGrid = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/36, 16/), &
    minCornerCoord=(/90.5_ESMF_KIND_R8, -80._ESMF_KIND_R8/), &
    maxCornerCoord=(/450.5_ESMF_KIND_R8, 80._ESMF_KIND_R8/), &
    staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
    regDecomp=(/1,petCount/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOC
  dstField = ESMF_FieldCreate(dstGrid, ESMF_TYPEKIND_R8, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
    routehandle=routehandle, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The purpose of the explicit {\tt Store()} call is to separate out the 
! expensive part of creating the RouteHandle object for a specific
! communication patter, from the less expensive part of applying it.
! Applying the RouteHandle results in data movement between 
! the source and destination objects. Once a RouteHandle is available, it is
! reusable. This means it can be applied over and over again to communicate
! data from the source to the destination object.
!EOE

!BOC
  do i=1, 10
    ! repeatedly applying the routehandle
    call ESMF_FieldRegrid(srcField=srcField, dstField=dstField, &
      routehandle=routehandle, rc=rc)
  enddo
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Reusability of a RouteHandle object extends beyond re-applying it to the same
! source/destination object pair that was used during {\tt Store()}. The same
! RouteHandle can be applied to a different object pair, as long as these
! criterial are met:
! \begin{itemize}
! \item The new pair matches the original pair with respect to {\em type}, 
!   and {\em kind}.
! \item The memory layout of the {\em distributed} (i.e. {\em gridded}) 
!   dimensions of the new pair is congruent with the original pair. This means
!   the {\em DistGrid}s must match, as well as any extra padding on the 
!   distributed/gridded dimensions.
! \item Size, number, and position (i.e. index order) of potentially present
!   {\em undistributed} (i.e. {\em ungridded}) dimensions does not affect the
!   reusability of a RouteHandle.
! \end{itemize}
! The following examples will discuss in detail what this means in practice.
!
! First consider the case where a second pair of source and destination Fields
! is created identical to the first set. The precomputed RouteHandle is 
! immediatly reusable for this new Field pair to carry out the regrid operation.
!EOE
  
!BOC
  srcField2 = ESMF_FieldCreate(srcGrid, ESMF_TYPEKIND_R8, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  dstField2 = ESMF_FieldCreate(dstGrid, ESMF_TYPEKIND_R8, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  ! applying the same routehandle to a different pair of fields
  call ESMF_FieldRegrid(srcField=srcField2, dstField=dstField2, &
    routehandle=routehandle, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The same RouteHandle stays re-usable even for a Field pair where source and 
! destination have one or more additional undistributed dimensions. Here a
! single undistributed dimension is added. By default all undistributed 
! dimensions will be ordered {\em after} the distributed dimensions provided
! by the Grid object. 
!EOE

!BOC
  srcField3 = ESMF_FieldCreate(srcGrid, ESMF_TYPEKIND_R8, &
    ungriddedLBound=(/1/), ungriddedUBound=(/10/), &  ! undistributed dim last
    rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  dstField3 = ESMF_FieldCreate(dstGrid, ESMF_TYPEKIND_R8, &
    ungriddedLBound=(/1/), ungriddedUBound=(/10/), & ! undistributed dim last
    rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  ! applying the same routehandle to a different pair of fields
  call ESMF_FieldRegrid(srcField=srcField3, dstField=dstField3, &
    routehandle=routehandle, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The undistributed dimension can also be moved into the first position,
! and the same RouteHandle can still be re-used. Specifying the order
! of dimensions in a Field is accomplished by providing the
! {\tt gridToFieldMap}. Here the Grid dimensions are mapped to 2nd and 3rd
! Field dimensions, moving the undistributed dimension into the leading
! position.
!EOE

!BOC
  srcField4 = ESMF_FieldCreate(srcGrid, ESMF_TYPEKIND_R8, &
    ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
    gridToFieldMap=(/2,3/), rc=rc)  ! undistributed dim 1st 
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  dstField4 = ESMF_FieldCreate(dstGrid, ESMF_TYPEKIND_R8, &
    ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
    gridToFieldMap=(/2,3/), rc=rc)  ! undistributed dim 1st 
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  ! applying the same routehandle to a different pair of fields
  call ESMF_FieldRegrid(srcField=srcField4, dstField=dstField4, &
    routehandle=routehandle, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! It is not necessary that the undistributed dimension is in the same position
! on the source and destination Field. The only criteria that needs to be
! satisfied is that both source and destination have the {\em same number} of 
! undistributed elements. Here the RouteHandle is re-used for a 
! Field pair where the destination Field interleaves the undistributed dimension
! between the two distributed dimensions. At the same time the source Field
! keeps the undistributed dimension in leading position.
!EOE

!BOC
  srcField5 = ESMF_FieldCreate(srcGrid, ESMF_TYPEKIND_R8, &
    ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
    gridToFieldMap=(/2,3/), rc=rc)  ! undistributed dim 1st 
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  dstField5 = ESMF_FieldCreate(dstGrid, ESMF_TYPEKIND_R8, &
    ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
    gridToFieldMap=(/1,3/), rc=rc)  ! undistributed dim 2nd
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  ! applying the same routehandle to a different pair of fields
  call ESMF_FieldRegrid(srcField=srcField5, dstField=dstField5, &
    routehandle=routehandle, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! In the following example the undistributed elements on the source side are
! spread across two undistributed dimensions. Of course the product of the two
! dimension sizes must equal the number of undistributed elements on the 
! destination side, in order to fulfil the element count criteria. Here this 
! number is 10. At two undistributed dimension on the source side are placed
! in first and fourth position using the {\tt gridToFieldMap}. The same
! RouteHandle is applied to this Field pair, resulting in the desired 
! regrid operation.
!EOE

!BOC
  srcField6 = ESMF_FieldCreate(srcGrid, ESMF_TYPEKIND_R8, &
    ungriddedLBound=(/1,1/), ungriddedUBound=(/2,5/), &
    gridToFieldMap=(/2,3/), rc=rc)  ! undistributed dims 1st and 4th
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  dstField6 = ESMF_FieldCreate(dstGrid, ESMF_TYPEKIND_R8, &
    ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
    gridToFieldMap=(/1,3/), rc=rc)  ! undistributed dim 2nd
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  ! applying the same routehandle to a different pair of fields
  call ESMF_FieldRegrid(srcField=srcField6, dstField=dstField6, &
    routehandle=routehandle, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! While the RouteHandle was precomputed using a specific source/destination
! Field pair, we have seen how it can be re-used as long as the memory layout 
! associated with the distributed (i.e. gridded) dimensions does not change.
! A natural extension of this feature is to allow the same RouteHandle to be 
! re-used when source and destination are FieldBundles instead of Fields. The
! only requirement here is that both sides contain the same number of elements,
! and that
! each pair constructed from the source and destination side is compatible with
! the original pair used as shown in the examples above. Here this criteria is
! simply met by constructing the source and destination FieldBundles from the
! exact Fields used in the previous examples.
!EOE

!BOC
  srcFieldBundle = ESMF_FieldBundleCreate(fieldList=(/srcField, &
    srcField2, srcField3, srcField4, srcField5, srcField6/), rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  dstFieldBundle = ESMF_FieldBundleCreate(fieldList=(/dstField, &
    dstField2, dstField3, dstField4, dstField5, dstField6/), rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  ! applying the same routehandle to a pair of FieldBundles
  call ESMF_FieldBundleRegrid(srcFieldBundle, dstFieldBundle, &
    routehandle=routehandle, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! On a fundamental level, RouteHandles are re-usable across objects that
! have the same memory layout for their distributed dimensions. Since ESMF
! Fields are built on top of ESMF Arrays, it is 
! possible to re-use the same RouteHandle that was precomputed for a Field
! pair and apply it to a matching Array pair. 
!
! For this example, the easiest way to create Arrays with the same memory 
! layout in the distributed dimensions is to query the source and destination
! Grid objects for their DistGrids. Then source and destination Arrays can be
! easily constructed.
!EOE

!BOC
  call ESMF_GridGet(srcGrid, distgrid=srcDistGrid, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  call ESMF_GridGet(dstGrid, distgrid=dstDistGrid, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  srcArray = ESMF_ArrayCreate(srcDistGrid, ESMF_TYPEKIND_R8, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  dstArray = ESMF_ArrayCreate(dstDistGrid, ESMF_TYPEKIND_R8, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  ! applying the same routehandle to an Array pair
  call ESMF_ArraySMM(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Finally the resources associated with the RouteHandle object are released.
! The recommended way to do this is by calling into the {\tt Release()} method
! associated with the {\tt Store()} method used to create the RouteHandle.
!EOE

!BOC
  call ESMF_FieldRegridRelease(routehandle, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

! ----------------------------------------------------------------------

  call ESMF_ArrayDestroy(srcArray, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArrayDestroy(dstArray, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldBundleDestroy(srcFieldBundle, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldBundleDestroy(dstFieldBundle, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(srcField, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(dstField, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(srcField2, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(dstField2, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(srcField3, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(dstField3, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(srcField4, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(dstField4, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(srcField5, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(dstField5, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(srcField6, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(dstField6, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridDestroy(srcGrid, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridDestroy(dstGrid, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)


! ----------------------------------------------------------------------

10 continue

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_RHandleReusabilityEx.F90"
  else
    print *, "FAIL: ESMF_RHandleReusabilityEx.F90"
  endif

end program
