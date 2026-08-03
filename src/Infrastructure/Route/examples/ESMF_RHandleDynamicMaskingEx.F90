! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2026, University Corporation for Atmospheric Research,
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
! {\bf Construct a RouteHandle compatible with Dynamic Masking}
!
! Note that since the intention is to later use the generated RouteHandle for
! dynamic masking, it is important to provide the {\tt srcTermProcessing}
! argument, which {\bf must be set equal to 0}. Doing this ensures that all
! of the multiplying with interpolation weights and summing of terms is
! carried out on the destination side. This is critical for correct functioning
! of dynamic masking!
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
! regrid operation over and over during the course of the simulation run, by
! calling the {\tt ESMF\_FieldRegrid()} method.
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
! {\bf Source Side Dynamic Masking}
!
! Assume that during the course of the simulation the {\tt srcField} becomes
! partially masked. This masking may be dynamically changing, as would be the
! case e.g. for the changing ice cover over the arctic ocean. Then the regrid
! operation represented by {\tt routehandle} should dynamically adjust to
! only use unmasked source elements at the time of operation.
!
! This dynamic masking behavior can be achieved in ESMF by setting the
! {\tt srcField} elements to a special value, and constructing an appropriate
! {\tt ESMF\_DynamicMask} object.
!
! Obtain a Fortran pointer to the local array segment.
!EOE

!BOC
  call ESMF_FieldGet(srcField, farrayPtr=farrayPtr, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Then set the desired local source element(s) to a custom value, here
! {\tt srcMaskValue}.
!EOE
!BOC
  farrayPtr(lbound(farrayPtr,1)+3,lbound(farrayPtr,2)+3) = srcMaskValue
!EOC

!BOE
! Finally set up an {\tt ESMF\_DynamicMask} object that holds information about
! the special mask value, and what action to take for elements that hold this
! special value during RouteHandle execution. Both pieces of information are
! set using one of the available {\tt ESMF\_DynamicMaskSet*()} methods.
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
! typekind-triplet suffix. Here the suffix is {\tt R8R8R8}, which
! indicates that the {\tt dynamicMaskRoutine} argument
! provided is expected to deal with
! \begin{itemize}
! \item {\tt real(ESMF\_KIND\_R8)} destination data (the first R8 typekind),
! \item {\tt real(ESMF\_KIND\_R8)} interpolation weights (the second R8 typekind), and
! \item {\tt real(ESMF\_KIND\_R8)} source data (the third R8 typekind).
! \end{itemize}
! Now when the {\tt routehandle} is executed again, and the {\tt dynamicMask}
! object is passed into the {\tt ESMF\_FieldRegrid()} call,
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
! that set by {\tt dynamicSrcMaskValue = srcMaskValue}. If any matching elements
! are found, they are passed into the routine that was provided via the
! {\tt dynamicMaskRoutine} argument, i.e. {\tt simpleDynMaskProc} in this
! example.
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
! The procedure provided through the {\tt dynamicMaskRoutine} argument must
! {\em exactly} satisfy the following prescribed interface:
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
! The first argument, per the interface, is an array of type
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
! Here the {\tt dstElement} is a pointer to the actual element in
! {\tt dstField}. Thus, assigning {\tt dstElement} to a value, immediately
! results in a value change of the element inside the {\tt dstField} object.
! Further, the {\tt factor(:)} and {\tt srcElement(:)} arrays have the same
! number of elements, corresponding to the number of source elements in
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
! could be implemented similar to the following subroutine, which satisfies the
! prescribed interface from above:
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
!EOE


!BOE
! {\bf Destination Side Dynamic Masking}
!
! So far only special values in {\tt srcField} have been
! considered for dynamic masking. However, elements in the {\tt dstField}
! can also be considered for dynamic masking when setting up the
! {\tt ESMF\_DynamicMask} object.
!
! First ensure that the {\tt dstField} is in a well defined state. This can
! be achived, for example, by using the {\tt ESMF\_FieldFill()} method. Here set the
! entire {\tt dstField} to zero:
!EOE

!BOC
  call ESMF_FieldFill(dstField, dataFillScheme="const", const1=0.d0, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Now some of the destination elements are set to a custom masking value
! of {\tt dstMaskValue}.
!EOE

!BOC
  ! obtain pointer to field data
  call ESMF_FieldGet(dstField, farrayPtr=farrayPtr, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! set an element to custom value 'dstMaskValue'
  farrayPtr(lbound(farrayPtr,1)+1,lbound(farrayPtr,2)+1) = dstMaskValue
!EOC

!BOE
! Now reset the {\tt dynamicMask} object using the same
! {\tt ESMF\_DynamicMaskSetR8R8R8()} method as before, but in addition to
! the previous arguments also specify {\tt dynamicDstMaskValue = dstMaskValue}.
! Again an adequate procedure, matching the prescribed interface, must be
! supplied through the {\tt dynamicMaskRoutine} argument.
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
! When using this {\tt dynamicMask} object in a {\tt ESMF\_FieldRegrid()}
! call, ESMF not only looks for source elements that match
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
! Notice the {\tt zeroregion = ESMF\_REGION\_EMPTY} specification in the
! {\tt ESMF\_FieldRegrid()} call! This setting ensures that values in the
! {\tt dstField} remain unchanged until they are checked for
! {\tt dynamicDstMaskValue}. This is critical for correct behavior of
! destination side dynamic masking.
!
! During the execution of {\tt ESMF\_FieldRegrid()}, the supplied procedure
! {\tt simpleDynMaskProc} is called with a {\tt dynMaskList} that contains
! all of the elements affected by source or destination side masking. A
! suitable procedure would be inspecting the {\tt dstElement} as well as
! all of the elements of {\tt srcElement(:)}, for each element in the
! provided {\tt dynMaskList(:)} argument, comparing them against
! {\tt dynamicDstMaskValue} and {\tt dynamicSrcMaskValue}, respectively.
! Then take the appropriate action to implement the desired masked iterpolation.
!EOE

! ----------------------------------------------------------------------

!BOE
! {\bf Handle all Elements}
!
! The {\tt ESMF\_DynamicMaskSet*()} methods provide an optional argument,
! {\tt handleAllElements}, of {\tt logical} type. By default it is
! {\tt .false.}, which means that only elements affected by source or
! destination side dynamic masking, as described previously, are passed to
! the supplied {\tt dynamicMaskRoutine}. However, when set to {\tt .true.},
! {\em all} local elements on each PET are made available to the
! {\tt dynamicMaskRoutine}. This allows implemention of fully customized
! handling of the interpolation from source to destination, using the
! information supplied by ESMF.
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
! {\bf Leading Undistributed Dimensions - Scalar Handling}
!
! Dynamic masking is also available for source and destination fields that
! contain leading undistributed dimensions. When ESMF applies the regridding
! weights, it interprets the product space of leading undistributed dimensions
! of a Field or Array as the elements of a vector. In this approach the
! interpolation becomes a vector operation.
!
! When applying the concept of dynamic masking to such a vector operation,
! without making further assumptions, it is likely that different vector
! elements are affected differently by the dynamic mask. ESMF therefore unrolls
! the vector dimension when constructing the information passed to the
! {\tt dynamicMaskRoutine}. As a consequence of this, masking routines
! do not generally have to consider vectorization explicitly, but can be used
! directly in their scalar form for the unrolled vector case.
!
! The concept is demonstrated by creating source and destination fields
! with one leading undistributed dimension. Each of 20 elements.
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
! A regrid operation is computed in the usual manner with
! {\tt srcTermProcessing = 0}.
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
! by the dynamic mask conditions individually.
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
! Again {\tt handleAllElements} can be set to {\tt .true.} in order to handle
! all of the elements via {\tt dynamicMaskRoutine} regardless of data values.
! There are now 20 times as many elements on the source and destination side,
! therefore leading to exactly 20 times as many elements to be handled.
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
! {\bf Leading Undistributed Dimensions - Vector Handling}
!
! For the case with {\tt handleAllElements=.true.}, where the entire
! vector of undistributed elements is passed to {\tt dynamicMaskRoutine} at
! every distributed location regardless of data values, an alternative
! implementation option exists for the dynamic masking routine. In some cases
! this alternative may result in more efficient code because it allows to
! vectorize over the undistributed elements when summing up the interpolation
! terms.
!
! The vector version of the {\tt dynamicMaskRoutine} interfaces have a trailing
! {\tt V} in the name and look like this:
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
! The difference compared to the previously discussed scalar interface is that
! the first argument of the vector version is of type
! {\tt ESMF\_DynamicMaskElementR8R8R8V} - again notice the trailing {\tt V} in
! the type name. The vector element type is declared as follows:
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
!
! Providing a suitable vector routine through the {\tt dynamicMaskRoutine}
! argument, the {\tt ESMF\_DynamicMask} object is set using one of the
! {\tt ESMF\_DynamicMaskSet*V()} methods:
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
! {\bf Source and Destination TypeKind}
!
! Applying dynamic masking to source and destination fields of other typekinds
! than R8 requires that the correct {\tt DynamicMaskSet} method is chosen.
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
! {\tt ESMF\_DynamicMaskSet*()} method must be selected, indicating that the
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

#endif

! ----------------------------------------------------------------------

!BOE
! {\bf Predefined Dynamic Masking Routines}
!
! ESMF currently provides three flavors of predefined public dynamic masking
! routines that can be used for dynamic masking:
! \begin{itemize}
! \item TODO: Need a description of DynamicMaskProcDst here!!!
!   \begin{itemize}
!   \item {\tt ESMF\_DynamicMaskProcDstR8R8R8}
!   \item {\tt ESMF\_DynamicMaskProcDstR4R8R4}
!   \item {\tt ESMF\_DynamicMaskProcDstR8R8R8V}
!   \item {\tt ESMF\_DynamicMaskProcDstR4R8R4V}
!   \end{itemize}
! \item TODO: Need a description of DynamicMaskProcSrc here!!!
!   \begin{itemize}
!   \item {\tt ESMF\_DynamicMaskProcSrcR8R8R8}
!   \item {\tt ESMF\_DynamicMaskProcSrcR4R8R4}
!   \item {\tt ESMF\_DynamicMaskProcSrcR8R8R8V}
!   \item {\tt ESMF\_DynamicMaskProcSrcR4R8R4V}
!   \end{itemize}
! \item TODO: Need a description of DynamicMaskProcVote here!!!
!   \begin{itemize}
!   \item {\tt ESMF\_DynamicMaskProcVoteR8R8R8}
!   \item {\tt ESMF\_DynamicMaskProcVoteR4R8R4}
!   \item {\tt ESMF\_DynamicMaskProcVoteR8R8R8V}
!   \item {\tt ESMF\_DynamicMaskProcVoteR4R8R4V}
!   \end{itemize}
! \end{itemize}
!
! One way to utilize the predefined public routines is by passing them into
! the usual {\tt ESMF\_DynamicMaskSet*()} methods through the
! {\tt dynamicMaskRoutine} argument:
!EOE

#ifndef ESMF_NO_DYNMASKOVERLOAD

!BOC
  call ESMF_DynamicMaskSetR4R8R4(dynamicMask, &
    dynamicSrcMaskValue=srcMaskValueR4, &
    dynamicDstMaskValue=dstMaskValueR4, &
    dynamicMaskRoutine=ESMF_DynamicMaskProcVoteR4R8R4, &
    rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! This dynamic mask can then be used with the {\tt srcField}, {\tt dstField},
! and {\tt routehandle} objects from the previous section when calling into
! {\tt ESMF\_FieldRegrid()}:
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

!BOE
! There exists an alternative set of {\tt ESMF\_DynamicMaskSetPredef*()}
! methods for convenience that take arument {\tt predefFlag} instead of
! the {\tt dynamicMaskRoutine} agument. The {\tt predefFlag} argument is of
! {\tt type ESMF\_DynamicMaskPredef\_Flag} with the following named constant
! values:
! \begin{itemize}
! \item {\tt ESMF\_DYNAMICMASKPREDEF\_DST}
! \item {\tt ESMF\_DYNAMICMASKPREDEF\_SRC}
! \item {\tt ESMF\_DYNAMICMASKPREDEF\_VOTE}
! \end{itemize}
!
! Using this approach, the same {\tt ESMF\_DynamicMask} object can be set by
! making the following call:
!EOE

!BOC
  call ESMF_DynamicMaskSetPredefR4R8R4(dynamicMask, &
    dynamicSrcMaskValue=srcMaskValueR4, &
    dynamicDstMaskValue=dstMaskValueR4, &
    predefFlag=ESMF_DYNAMICMASKPREDEF_VOTE, &
    rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Which then is used in the same manner with {\tt ESMF\_FieldRegrid()}:
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

! --- cleanup ---

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
