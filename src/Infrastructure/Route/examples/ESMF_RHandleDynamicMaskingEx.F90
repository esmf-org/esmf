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

module ESMF_RHandleDynamicMaskingMod
  
  use ESMF

  implicit none

  public simpleDynMaskProc, simpleHandleAllProc
#ifndef ESMF_NO_DYNMASKOVERLOAD
  public simpleHandleAllProcV
#endif
  public simpleDynMaskProcR4R8R4

 contains !-------------------------------------
 
  subroutine simpleDynMaskProc(dynamicMaskList, dynamicSrcMaskValue, &
    dynamicDstMaskValue, rc)
    type(ESMF_DynamicMaskElementR8R8R8), pointer        :: dynamicMaskList(:)
    real(ESMF_KIND_R8),            intent(in), optional :: dynamicSrcMaskValue
    real(ESMF_KIND_R8),            intent(in), optional :: dynamicDstMaskValue
    integer,                       intent(out)          :: rc
    integer :: i, j
    real(ESMF_KIND_R8)  :: renorm
    if (associated(dynamicMaskList)) then
      do i=1, size(dynamicMaskList)
        if (matchR8(dynamicDstMaskValue,dynamicMaskList(i)%dstElement)) then
          ! dstElement was masked -> just set to a specific value
          dynamicMaskList(i)%dstElement = 50.d0
        else
          ! there must be srcElements masked 
          ! -> don't use masked srcElements, but renormalize all other factors
          dynamicMaskList(i)%dstElement = 0.d0 ! set to zero
          renorm = 0.d0 ! reset
          do j=1, size(dynamicMaskList(i)%factor)
            if (.not. &
              matchR8(dynamicSrcMaskValue,dynamicMaskList(i)%srcElement(j))) then
              dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement &
                + dynamicMaskList(i)%factor(j) &
                * dynamicMaskList(i)%srcElement(j)
              renorm = renorm + dynamicMaskList(i)%factor(j)
            endif
          enddo
          if (renorm > 0.d0) then
            dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement / renorm
          endif
        endif
      enddo
    endif
    ! return successfully
    rc = ESMF_SUCCESS
  end subroutine  
  
  !-----------
  
  subroutine simpleHandleAllProc(dynamicMaskList, dynamicSrcMaskValue, &
    dynamicDstMaskValue, rc)
    type(ESMF_DynamicMaskElementR8R8R8), pointer        :: dynamicMaskList(:)
    real(ESMF_KIND_R8),            intent(in), optional :: dynamicSrcMaskValue
    real(ESMF_KIND_R8),            intent(in), optional :: dynamicDstMaskValue
    integer,                       intent(out)          :: rc
    integer :: i, j
    real(ESMF_KIND_R8)  :: renorm
    if (associated(dynamicMaskList)) then
      do i=1, size(dynamicMaskList)
        ! -> don't use masked srcElements, but renormalize all other factors
        dynamicMaskList(i)%dstElement = 0.d0 ! set to zero
        renorm = 0.d0 ! reset
        do j=1, size(dynamicMaskList(i)%factor)
          if (.not. &
            matchR8(dynamicSrcMaskValue,dynamicMaskList(i)%srcElement(j))) then
            dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement &
              + dynamicMaskList(i)%factor(j) &
              * dynamicMaskList(i)%srcElement(j)
            renorm = renorm + dynamicMaskList(i)%factor(j)
          endif
        enddo
        if (renorm > 0.d0) then
          dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement / renorm
        endif
        ! here customize interpolation by setting everything destination point
        ! that is above 0.5 to the dynamicDstMaskValue 
        if (dynamicMaskList(i)%dstElement > 0.5d0) then
          dynamicMaskList(i)%dstElement = dynamicDstMaskValue
        endif
      enddo
    endif
    ! return successfully
    rc = ESMF_SUCCESS
  end subroutine  
  
  !-----------
#ifndef ESMF_NO_DYNMASKOVERLOAD
  subroutine simpleHandleAllProcV(dynamicMaskList, dynamicSrcMaskValue, &
    dynamicDstMaskValue, rc)
    type(ESMF_DynamicMaskElementR8R8R8V), pointer       :: dynamicMaskList(:)
    real(ESMF_KIND_R8),            intent(in), optional :: dynamicSrcMaskValue
    real(ESMF_KIND_R8),            intent(in), optional :: dynamicDstMaskValue
    integer,                       intent(out)          :: rc
    integer :: i, j, v, vSize
    real(ESMF_KIND_R8)  :: renorm
    if (associated(dynamicMaskList)) then
      do i=1, size(dynamicMaskList)
        vSize = size(dynamicMaskList(i)%dstElement) ! vector size
        ! -> don't use masked srcElements, but renormalize all other factors
        renorm = 0.d0 ! reset
        do v=1, vSize
          dynamicMaskList(i)%dstElement(v) = 0.d0 ! set to zero
        enddo
        do j=1, size(dynamicMaskList(i)%factor)
          if (.not. matchR8(dynamicSrcMaskValue,&
            dynamicMaskList(i)%srcElement(j)%ptr(1))) then
            renorm = renorm + dynamicMaskList(i)%factor(j)
            do v=1, vSize
              dynamicMaskList(i)%dstElement(v) = &
                dynamicMaskList(i)%dstElement(v) &
                + dynamicMaskList(i)%factor(j) &
                * dynamicMaskList(i)%srcElement(j)%ptr(v)
            enddo
          endif
        enddo
        do v=1, vSize
          if (renorm > 0.d0) then
            dynamicMaskList(i)%dstElement(v) = &
              dynamicMaskList(i)%dstElement(v) / renorm
          endif
          ! here customize interpolation by setting everything destination point
          ! that is above 0.5 to the dynamicDstMaskValue 
          if (dynamicMaskList(i)%dstElement(v) > 0.5d0) then
            dynamicMaskList(i)%dstElement(v) = dynamicDstMaskValue
          endif
        enddo
      enddo
    endif
    ! return successfully
    rc = ESMF_SUCCESS
  end subroutine  
#endif
  !-----------
  
  subroutine simpleDynMaskProcR4R8R4(dynamicMaskList, dynamicSrcMaskValue, &
    dynamicDstMaskValue, rc)
    type(ESMF_DynamicMaskElementR4R8R4), pointer        :: dynamicMaskList(:)
    real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
    real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
    integer,                       intent(out)          :: rc
    integer :: i, j
    real(ESMF_KIND_R8)  :: renorm
    if (associated(dynamicMaskList)) then
      do i=1, size(dynamicMaskList)
        if (matchR4(dynamicDstMaskValue,dynamicMaskList(i)%dstElement)) then
          ! dstElement was masked -> just set to a specific value
          dynamicMaskList(i)%dstElement = 50.d0
        else
          ! there must be srcElements masked 
          ! -> don't use masked srcElements, but renormalize all other factors
          dynamicMaskList(i)%dstElement = 0. ! set to zero
          renorm = 0.d0 ! reset
          do j=1, size(dynamicMaskList(i)%factor)
            if (.not. &
              matchR4(dynamicSrcMaskValue,dynamicMaskList(i)%srcElement(j))) then
              dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement &
                + real(dynamicMaskList(i)%factor(j) &
                     * dynamicMaskList(i)%srcElement(j), &
                  ESMF_KIND_R4)
              renorm = renorm + dynamicMaskList(i)%factor(j)
            endif
          enddo
          if (renorm > 0.d0) then
            dynamicMaskList(i)%dstElement = &
              real(dynamicMaskList(i)%dstElement / renorm, ESMF_KIND_R4)
          endif
        endif
      enddo
    endif
    ! return successfully
    rc = ESMF_SUCCESS
  end subroutine  
  
  !-----------
  
  function matchR8(val1, val2)
    ! ability to safely compare optional arguments
    logical :: matchR8
    real(ESMF_KIND_R8), optional  :: val1
    real(ESMF_KIND_R8), optional  :: val2
    matchR8 = .false.
    if (.not.present(val1)) return
    if (.not.present(val2)) return
    matchR8 = (val1 .eq. val2)
  end function
    
  function matchR4(val1, val2)
    ! ability to safely compare optional arguments
    logical :: matchR4
    real(ESMF_KIND_R4), optional  :: val1
    real(ESMF_KIND_R4), optional  :: val2
    matchR4 = .false.
    if (.not.present(val1)) return
    if (.not.present(val2)) return
    matchR4 = (val1 .eq. val2)
  end function
    
end module ESMF_RHandleDynamicMaskingMod


program ESMF_RHandleDynamicMaskingEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  
  use ESMF_RHandleDynamicMaskingMod
  
  implicit none
  
  ! local variables
  integer                     :: rc
  type(ESMF_VM)               :: vm
  integer                     :: i, petCount, localPet
  type(ESMF_Grid)             :: srcGrid, dstGrid
  type(ESMF_Field)            :: srcField, dstField
  type(ESMF_RouteHandle)      :: routehandle
  integer                     :: srcTermProcessing
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr3d(:,:,:)
  real(ESMF_KIND_R8)          :: srcMaskValue=-777.d0
  real(ESMF_KIND_R8)          :: dstMaskValue=-888.d0
  real(ESMF_KIND_R4), pointer :: farrayPtrR4(:,:)
  real(ESMF_KIND_R4)          :: srcMaskValueR4=-777.d0
  real(ESMF_KIND_R4)          :: dstMaskValueR4=-888.d0
  type(ESMF_DynamicMask)      :: dynamicMask
  
  ! result code
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg
  
  
  finalrc = ESMF_SUCCESS

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_RHandleDynamicMaskingEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_Initialize(vm=vm, defaultlogfilename="RHandleDynamicMaskingEx.Log", &
    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOE
! \subsubsection{Dynamic Masking}
! \label{RH:DynMask}
!
! When a RouteHandle object is created during an {\tt ESMF\_FieldRegridStore()}
! call, masking information can be provided by the user. This type of masking
! is said to be {\em static}, and is described in section \ref{regrid:masking}.
! It is static, because the masks set the maximum limits of the regrid 
! operation, which cannot be changed later. All subsequent executions of the
! same RouteHandle can only use elements - source or destination - 
! that were not masked during the Store() call.
!
! Once a RouteHandle object is available, whether it was created with or without 
! static masking, the associated regrid operation can further be masking 
! during RouteHandle execution . This is called {\em dynamic} masking, because
! it can dynamically change between subsequent RouteHandle executions. The 
! RouteHandle itself remains unchange during this process. The dynamic
! masking information is processed on the fly as the RouteHandle is applied.
!
! The following example demonstrates dynamic masking for a regrid operation
! between two Field objects. Although it is supported, here
! the regrid operation between {\tt srcField} and {\tt dstField} is computed
! without static masking.
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
    
  ! create srcField
  srcField = ESMF_FieldCreate(srcGrid, ESMF_TYPEKIND_R8, rc=rc)
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
    
  ! create srcField
  dstField = ESMF_FieldCreate(dstGrid, ESMF_TYPEKIND_R8, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Note that since the intention is to later use the generated RouteHandle for
! dynamic masking, it is important to provide the {\tt srcTermProcessing} 
! argument, which must be set equal to 0. Doing this ensures that all
! of the multiplying with interpolation weights, and summing of terms, is
! carried out on the destination side. This is critical for dynamic masking.
!EOE

!BOC
  srcTermProcessing=0

  call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
    srcTermProcessing=srcTermProcessing, routehandle=routehandle, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Now that {\tt routehandle} is available, it can be used to execute the 
! regrid operation over and over during the course of the simualtion run.
!EOE

!BOC
  call ESMF_FieldRegrid(srcField=srcField, dstField=dstField, &
    routehandle=routehandle, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Assume that during the course of the simulation the {\tt srcField} becomes
! partially masked. This masking may be dynamically changing, as would be the
! case for the ice cover over the arctic ocean. Then the regrid operation 
! represented by {\tt routehandle} should dynamically adjust to only use
! unmasked source elements.
!
! The dynamic masking behavior can be achieved in ESMF by setting {\tt srcField}
! elements to a special value.
!EOE

!BOC
  call ESMF_FieldGet(srcField, farrayPtr=farrayPtr, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! setting an arbitrary local source element to special value 'srcMaskValue'
  farrayPtr(lbound(farrayPtr,1)+3,lbound(farrayPtr,2)+3) = srcMaskValue
!EOC

!BOE
! Then set up an {\tt ESMF\_DynamicMask} object that holds information about
! the special mask value. The dynamic mask object
! further holds a pointer to the routine that will be called in order to handle
! dynamically masked elements.
!EOE

!BOC
  call ESMF_DynamicMaskSetR8R8R8(dynamicMask, &
    dynamicSrcMaskValue=srcMaskValue, &
    dynamicMaskRoutine=simpleDynMaskProc, &
    rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOE
! The names of the specific {\tt DynamicMaskSet} methods all carry a 
! typekind-triplet suffix. Here the suffix is {\tt R8R8R8}. 
! This indicates that the {\tt dynamicMaskRoutine} argument
! provided is expected to deal with {\tt real(ESMF\_KIND\_R8)} destination data
! (first R8 typekind), {\tt real(ESMF\_KIND\_R8)} factors (second R8 typekind),
! and {\tt real(ESMF\_KIND\_R8)} source data (third R8 typekind).
!
! Now when the {\tt routehandle} is executed, and the {\tt dynamicMask} object
! is passed into the {\tt ESMF\_FieldRegrid()} call,
!EOE

!BOC
  call ESMF_FieldRegrid(srcField=srcField, dstField=dstField, &
    routehandle=routehandle, dynamicMask=dynamicMask, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE 
! ESMF will scan the {\tt srcField} for elements that have data equal to
! that set by {\tt dynamicSrcMaskValue}. If any are found, they
! are passed into the routine provided via the {\tt dynamicMaskRoutine}
! argument.
!EOE

#if 0
  call ESMF_FieldWrite(dstField, fileName="dstFieldR8_onlySrcMask.nc", &
    status=ESMF_FILESTATUS_REPLACE, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

!BOE
! The procedure passed through the {\tt dynamicMaskRoutine} argument must 
! satisfy exactly the following predefined interface:
!
! \begin{verbatim}
!  interface
!    subroutine ESMF_DynamicMaskRoutineR8R8R8(dynMaskList, &
!      dynamicSrcMaskValue, dynamicDstMaskValue, rc)
!      use ESMF_UtilTypesMod
!      implicit none
!      type(ESMF_DynamicMaskElementR8R8R8), pointer        :: dynMaskList(:)
!      real(ESMF_KIND_R8),            intent(in), optional :: dynamicSrcMaskValue
!      real(ESMF_KIND_R8),            intent(in), optional :: dynamicDstMaskValue
!      integer,                       intent(out)  :: rc
!    end subroutine
!  end interface
! \end{verbatim}
!
! The first argument accepted according to this interface is an array of type
! {\tt ESMF\_DynamicMaskElement}. Each element of this array corresponds to a
! single element in the {\tt dstField} that is affected by dynamic masking. 
! For each such {\tt dstElement} the complete interpolation stencile is
! provided by the {\tt ESMF\_DynamicMaskElement} derived type:
!
! \begin{verbatim}
!  type ESMF_DynamicMaskElementR8R8R8
!    real(ESMF_KIND_R8), pointer       :: dstElement
!    real(ESMF_KIND_R8), allocatable   :: factor(:)
!    real(ESMF_KIND_R8), allocatable   :: srcElement(:)
!  end type
! \end{verbatim}
!
! Here the {\tt dstElement} is a pointer to the actual element in the 
! {\tt dstField}. Thus, assigning {\tt dstElement} to a value, immediately
! results in a value change of the element inside the {\tt dstField} object.
! Further, the size of the {\tt factor(:)} and {\tt srcElement(:)} arrays is
! identical to each other and corresponds to the number of source elements in
! the interpolation stencile. Without dynamic masking, the {\tt dstElement}
! would simply be calculated as the scalar product of {\tt factor(:)} and 
! {\tt srcElement(:)}.
!
! By providing the {\tt dynamicMaskRoutine}, the user has full control as to
! what exactly happens to destination elements that are affected by dynamic
! masking. For the current example, where some source elements may be marked by
! a special masking value, a simple scheme could be to only use non-masked
! source elements to calculate destination elements. The result then needs to
! be renormalized in order to account for the missing source elements. This
! could be implemented similar to the following subroutine:
!
! \begin{verbatim}
!  subroutine simpleDynMaskProc(dynamicMaskList, dynamicSrcMaskValue, &
!    dynamicDstMaskValue, rc)
!    type(ESMF_DynamicMaskElementR8R8R8), pointer        :: dynamicMaskList(:)
!    real(ESMF_KIND_R8),            intent(in), optional :: dynamicSrcMaskValue
!    real(ESMF_KIND_R8),            intent(in), optional :: dynamicDstMaskValue
!    integer,                       intent(out)          :: rc
!    integer :: i, j
!    real(ESMF_KIND_R8)  :: renorm
!    if (associated(dynamicMaskList)) then
!      do i=1, size(dynamicMaskList)
!        dynamicMaskList(i)%dstElement = 0.d0 ! set to zero
!        renorm = 0.d0 ! reset
!        do j=1, size(dynamicMaskList(i)%factor)
!          if (.not. &
!            match(dynamicSrcMaskValue,dynamicMaskList(i)%srcElement(j))) then
!            dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement &
!              + dynamicMaskList(i)%factor(j) &
!              * dynamicMaskList(i)%srcElement(j)
!            renorm = renorm + dynamicMaskList(i)%factor(j)
!          endif
!        enddo
!        if (renorm > 0.d0) then
!          dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement / renorm
!        else if (present(dynamicSrcMaskValue)) then
!          dynamicMaskList(i)%dstElement = dynamicSrcMaskValue
!        else
!          rc = ESMF_RC_ARG_BAD  ! error detected
!          return
!        endif
!      enddo
!    endif
!    ! return successfully
!    rc = ESMF_SUCCESS
!  end subroutine
! \end{verbatim}
!
! So far in the example only the {\tt srcField} had been dynamically masked.
! However, elements in the {\tt dstField} can be masked as well, following 
! exactly the same manner.
!
! First ensure that the {\tt dstField} is in a well defined condition. This can
! be achived by reseting it, e.g. to zero, using the {\tt ESMF\_FieldFill()}
! method.
!EOE

!BOC
  call ESMF_FieldFill(dstField, dataFillScheme="const", const1=0.d0, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Now some of the destination elements are set to a defined masking value.
!EOE

!BOC
  call ESMF_FieldGet(dstField, farrayPtr=farrayPtr, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! setting an arbitrary local destination element to special value 'dstMaskValue'
  farrayPtr(lbound(farrayPtr,1)+1,lbound(farrayPtr,2)+1) = dstMaskValue
!EOC

!BOE
! The {\tt dynamicMask} is reset using the same {\tt DynamicMaskSet} method as
! before, but in addition to the previous arguments, {\tt dynamicDstMaskValue}
! is also specified.
!EOE

!BOC
  call ESMF_DynamicMaskSetR8R8R8(dynamicMask, &
    dynamicSrcMaskValue=srcMaskValue, &
    dynamicDstMaskValue=dstMaskValue, &
    dynamicMaskRoutine=simpleDynMaskProc, &
    rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Passing the reset {\tt dynamicMask} object into {\tt ESMF\_FieldRegrid()} 
! causes ESMF to not only look for source elements that match
! {\tt dynamicSrcMaskValue}, but also destination elements that
! match {\tt dynamicDstMaskValue}.
!EOE

!BOC
  call ESMF_FieldRegrid(srcField=srcField, dstField=dstField, &
    routehandle=routehandle, zeroregion=ESMF_REGION_EMPTY, &
    dynamicMask=dynamicMask, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
#if 0
  call ESMF_FieldWrite(dstField, fileName="dstFieldR8.nc", &
    status=ESMF_FILESTATUS_REPLACE, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

!BOE
! Again an adequate procedure is supplied through 
! {\tt dynamicMaskRoutine}. For the current case, however, a suitable procedure
! would be inspecting the {\tt dstElement} as well as all the {\tt dstElement}s
! provided via the {\tt dynMaskList} argument.
!
! Notice the {\tt zeroregion = ESMF\_REGION\_EMPTY} specification in the 
! {\tt ESMF\_FieldRegrid()} call. This setting ensures that values in the
! {\tt dstField} remain unchanged until they are checked for
! {\tt dynamicDstMaskValue}. 
!EOE

! ----------------------------------------------------------------------

!BOE
! The {\tt DynamicMaskSet} methods provide an argument of {\tt logical} type, 
! called {\tt handleAllElements}. By default it is set to {\tt .false.}, 
! which means that only elements affected by dynamic masking -- as described
! above -- are passed to the {\tt dynamicMaskRoutine}. However, when
! {\tt handleAllElements} is set to {\tt .true.}, {\em all} local
! elements on each PET are made available to the {\tt dynamicMaskRoutine}.
! This allows the user supplied procedure to implement fully customized
! handling of the interpolation from source to destination, using the 
! information supplied by ESMF.
!
! To demonstrate this, a custom routine {\tt simpleHandleAllProc()} is 
! passed in as {\tt dynamicMaskRoutine}, and {\tt handleAllElements} is
! set to {\tt .true.}. All other aspects of the user interface remain unchanged.
!EOE

!BOC
  call ESMF_DynamicMaskSetR8R8R8(dynamicMask, &
    dynamicSrcMaskValue=srcMaskValue, &
    dynamicDstMaskValue=-2.d0, &
    dynamicMaskRoutine=simpleHandleAllProc, &
    handleAllElements=.true., &
    rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  call ESMF_FieldRegrid(srcField=srcField, dstField=dstField, &
    routehandle=routehandle, zeroregion=ESMF_REGION_EMPTY, &
    dynamicMask=dynamicMask, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
#if 0
  call ESMF_FieldWrite(dstField, fileName="dstFieldR8_handleAll.nc", &
    status=ESMF_FILESTATUS_REPLACE, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

  call ESMF_FieldRegridRelease(routehandle, rc=rc)
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

! ----------------------------------------------------------------------

!BOE
! Dynamic masking is also available for source and destination fields that
! contain leading undistributed dimensions. When ESMF applies the regridding
! weights, it interprets the product space of leading undistributed dimensions
! of a Field or Array as the elements of a vector. In this approach the 
! interpolation becomes a vector operation.  When applying the concept
! of dynamic masking to such a vector operation, without making further 
! assumptions, it must be assumed that different vector elements may be 
! affected differently by the dynamic mask. ESMF therefore unrolls the vector
! dimension when constructing the information passed to the
! {\tt dynamicMaskRoutine}. As a consequence of this, masking routines
! do not generally have to consider vectorization explicitly.
!
! The concept is demonstrated by creating source and destination fields
! with one leading undistributed dimension.
!EOE

  ! create srcField
!BOC
  srcField = ESMF_FieldCreate(srcGrid, ESMF_TYPEKIND_R8, &
    gridToFieldMap=(/2,3/), ungriddedLBound=(/1/), ungriddedUBound=(/20/), &
    rc=rc)
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

  call ESMF_FieldGet(srcField, farrayPtr=farrayPtr3d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! setting an arbitrary local source element to special value 'srcMaskValue'
  farrayPtr3d(lbound(farrayPtr3d,1)+3,lbound(farrayPtr3d,2)+3,&
    lbound(farrayPtr3d,3)+3) = srcMaskValue

  ! create srcField
!BOC
  dstField = ESMF_FieldCreate(dstGrid, ESMF_TYPEKIND_R8, &
    gridToFieldMap=(/2,3/), ungriddedLBound=(/1/), ungriddedUBound=(/20/), &
    rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! A regrid operation is computed in the usual manner. In order to make the
! resulting RouteHandle object suitable for dynamic masking, computations are
! pushed completely onto the destination PETs, as in previous examples, by
! setting the {\tt srcTermProcessing} argument to zero.
!EOE

!BOC
  srcTermProcessing=0

  call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
    srcTermProcessing=srcTermProcessing, routehandle=routehandle, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldFill(dstField, dataFillScheme="const", const1=0.d0, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The same {\tt dynamicMaskRoutine} as before can be used when setting up
! the {\tt ESMF\_DynamicMask} object. However, the source and destination
! Fields now contain 20 undistributed elements at each distributed location,
! and the dynamic mask routine will handle all elements that are affected
! by the dynamic mask conditions.
!EOE
  
!BOC
  call ESMF_DynamicMaskSetR8R8R8(dynamicMask, &
    dynamicSrcMaskValue=srcMaskValue, &
    dynamicDstMaskValue=dstMaskValue, &
    dynamicMaskRoutine=simpleDynMaskProc, &
    rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  call ESMF_FieldRegrid(srcField=srcField, dstField=dstField, &
    routehandle=routehandle, zeroregion=ESMF_REGION_EMPTY, &
    dynamicMask=dynamicMask, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

#if 0
  call ESMF_FieldWrite(dstField, fileName="dstFieldR8_vect.nc", &
    status=ESMF_FILESTATUS_REPLACE, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

!BOE
! Setting the {\tt handleAllElements} to {\tt .true.} will pass all elements
! to the {\tt dynamicMaskRoutine}. There are 20 times as many elements
! on the source and destination side, and therefore the dynamic masking routine
! will handle exactly 20 times as many elements compared to the case without
! undistributed dimension.
!EOE

!BOC
  call ESMF_DynamicMaskSetR8R8R8(dynamicMask, &
    dynamicSrcMaskValue=srcMaskValue, &
    dynamicDstMaskValue=-2.d0, &
    dynamicMaskRoutine=simpleHandleAllProc, &
    handleAllElements=.true., &
    rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  call ESMF_FieldRegrid(srcField=srcField, dstField=dstField, &
    routehandle=routehandle, zeroregion=ESMF_REGION_EMPTY, &
    dynamicMask=dynamicMask, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

#if 0
  call ESMF_FieldWrite(dstField, fileName="dstFieldR8_vectHandleAll.nc", &
    status=ESMF_FILESTATUS_REPLACE, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

!BOE
! For the case with {\tt handleAllElements=.true.}, where the entire
! vector of undistributed elements is passed to {\tt dynamicMaskRoutine} at
! every distributed location, an alternative implementation option exists for
! the dynamic masking routine. In some cases this alternative may result in
! more efficient code because it allows to vectorize over the undistributed
! elements when summing up the interpolation terms. The alternative interface
! for {\tt dynamicMaskRoutine} is:
!
! \begin{verbatim}
!  interface
!    subroutine ESMF_DynamicMaskRoutineR8R8R8V(dynMaskList, &
!      dynamicSrcMaskValue, dynamicDstMaskValue, rc)
!      use ESMF_UtilTypesMod
!      implicit none
!      type(ESMF_DynamicMaskElementR8R8R8V), pointer       :: dynMaskList(:)
!      real(ESMF_KIND_R8),            intent(in), optional :: dynamicSrcMaskValue
!      real(ESMF_KIND_R8),            intent(in), optional :: dynamicDstMaskValue
!      integer,                       intent(out)  :: rc
!    end subroutine
!  end interface
! \end{verbatim}
!
! The difference compared to the previously used interface is that the first
! argument now is of type {\tt ESMF\_DynamicMaskElementR8R8R8V}. This type is
! declared as follows:
!
! \begin{verbatim}
!  type ESMF_DynamicMaskElementR8R8R8V
!    real(ESMF_KIND_R8), pointer       :: dstElement(:)
!    real(ESMF_KIND_R8), allocatable   :: factor(:)
!    type(ESMF_PtrR8D1), allocatable   :: srcElement(:)
!  end type
! \end{verbatim}
!
! Here {\tt size(dstElement)} for every element in {\tt dynMaskList} is
! identical to the vector size, i.e. the number of undistributed elements to 
! be handled. The same is true for {\tt size(srcElement(j)\%ptr))}, for every
! element {\tt j} of the interpolation stencile.
!EOE

#ifndef ESMF_NO_DYNMASKOVERLOAD

!BOC
  call ESMF_DynamicMaskSetR8R8R8V(dynamicMask, &
    dynamicSrcMaskValue=srcMaskValue, &
    dynamicDstMaskValue=-2.d0, &
    dynamicMaskRoutine=simpleHandleAllProcV, &
    handleAllElements=.true., &
    rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  call ESMF_FieldRegrid(srcField=srcField, dstField=dstField, &
    routehandle=routehandle, zeroregion=ESMF_REGION_EMPTY, &
    dynamicMask=dynamicMask, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

#if 0
  call ESMF_FieldWrite(dstField, fileName="dstFieldR8_vectHandleAllV.nc", &
    status=ESMF_FILESTATUS_REPLACE, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

  call ESMF_FieldRegridRelease(routehandle, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
#endif

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

! ----------------------------------------------------------------------

!BOE
! Applying dynamic masking to source and destination fields of other typekind
! than R8 only requires that the correct {\tt DynamicMaskSet} method is chosen.
! Here we create {\tt real(ESMF\_KIND\_R4)} source and destination fields.
!EOE

  ! create srcField
!BOC
  srcField = ESMF_FieldCreate(srcGrid, ESMF_TYPEKIND_R4, rc=rc)
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

  ! create srcField
!BOC
  dstField = ESMF_FieldCreate(dstGrid, ESMF_TYPEKIND_R4, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Computing a suitable RouteHandle is unchanged.
!EOE

!BOC
  srcTermProcessing=0

  call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
    srcTermProcessing=srcTermProcessing, routehandle=routehandle, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Now setting some source and destination elements to defined special values
! of the correct typekind.
!EOE

!BOC
  call ESMF_FieldGet(srcField, farrayPtr=farrayPtrR4, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  farrayPtrR4(lbound(farrayPtrR4,1)+3,lbound(farrayPtrR4,2)+3) = srcMaskValueR4
!EOC

!BOC
  call ESMF_FieldFill(dstField, dataFillScheme="const", const1=0.d0, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOC
  call ESMF_FieldGet(dstField, farrayPtr=farrayPtrR4, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  farrayPtrR4(lbound(farrayPtrR4,1)+1,lbound(farrayPtrR4,2)+1) = dstMaskValueR4
!EOC

!BOE
! Setting up the {\tt ESMF\_DynamicMask} object is practically the same as 
! before, just that the correct typekind-triplet suffix for the 
! {\tt DynamicMaskSet} method must be selected, indicating that the 
! destination data is of typekind R4, the factors are still of typekind R8,
! and the source data is of typekind R4.
!EOE

#ifndef ESMF_NO_DYNMASKOVERLOAD

!BOC
  call ESMF_DynamicMaskSetR4R8R4(dynamicMask, &
    dynamicSrcMaskValue=srcMaskValueR4, &
    dynamicDstMaskValue=dstMaskValueR4, &
    dynamicMaskRoutine=simpleDynMaskProcR4R8R4, &
    rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Finally calling into {\tt ESMF\_FieldRegrid()} with the {\tt dynamicMask}
! object is unchanged.
!EOE

!BOC
  call ESMF_FieldRegrid(srcField=srcField, dstField=dstField, &
    routehandle=routehandle, zeroregion=ESMF_REGION_EMPTY, &
    dynamicMask=dynamicMask, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

#if 0
  call ESMF_FieldWrite(dstField, fileName="dstFieldR4.nc", &
    status=ESMF_FILESTATUS_REPLACE, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

  call ESMF_FieldRegridRelease(routehandle, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
#endif

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
    print *, "PASS: ESMF_RHandleDynamicMaskingEx.F90"
  else
    print *, "FAIL: ESMF_RHandleDynamicMaskingEx.F90"
  endif

end program
