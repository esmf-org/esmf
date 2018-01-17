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

module ESMF_RHandleDynamicMaskingMod
  
  use ESMF

  implicit none

  public simpleDynMaskProc, simpleDynMaskProcR4R8R4

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
          dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement / renorm
        endif
      enddo
    endif
    ! return successfully
    rc = ESMF_SUCCESS
  end subroutine  
  
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
                + dynamicMaskList(i)%factor(j) &
                * dynamicMaskList(i)%srcElement(j)
              renorm = renorm + dynamicMaskList(i)%factor(j)
            endif
          enddo
          dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement / renorm
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
  integer                     :: i, petCount
  type(ESMF_Grid)             :: srcGrid, dstGrid
  type(ESMF_Field)            :: srcField, dstField
  type(ESMF_RouteHandle)      :: routehandle
  integer                     :: srcTermProcessing
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)
  real(ESMF_KIND_R8)          :: srcMaskValue=-777.d0
  real(ESMF_KIND_R8)          :: dstMaskValue=-888.d0
  real(ESMF_KIND_R4), pointer :: farrayPtrR4(:,:)
  real(ESMF_KIND_R4)          :: srcMaskValueR4=-777.d0
  real(ESMF_KIND_R4)          :: dstMaskValueR4=-888.d0

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
  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (petCount < 4) then
    finalrc = ESMF_FAILURE
    goto 10
  endif
  
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
! elements to a special value. Then let the {\tt EMSF\_FieldRegrid()} method
! know about the special value via the {\tt dynamicSrcMaskValue} argument. 
!EOE

!BOC
  call ESMF_FieldGet(srcField, farrayPtr=farrayPtr, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  farrayPtr(lbound(farrayPtr,1)+3,lbound(farrayPtr,2)+3) = srcMaskValue
!EOC

!BOE
! Now when the {\tt routehandle} is executed, ESMF will scan the {\tt srcField}
! for elements that are set to {\tt dynamicSrcMaskValue}. If any are found, they
! are passed into the routine provided via the {\tt dynamicMaskRoutine}
! argument.
!EOE

!BOC
  call ESMF_FieldRegrid(srcField=srcField, dstField=dstField, &
    routehandle=routehandle, termorderflag=ESMF_TERMORDER_SRCSEQ, &
    dynamicSrcMaskValue=srcMaskValue, dynamicMaskRoutine=simpleDynMaskProc, &
    rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The procedure passed through the {\tt dynamicMaskRoutine} argument must 
! satisfy exactly the following predefined interface:
!
! \begin{verbatim}
!  interface
!    subroutine ESMF_DynamicMaskRoutine(dynMaskList, dynamicSrcMaskValue, &
!      dynamicDstMaskValue, rc)
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
! Going back to the example of regridding between the {\tt srcField} and
! {\tt dstField}, notice that beside providing the {\tt dynamicSrcMaskValue}
! and {\tt dynamicMaskRoutine} arguments, there was also 
! {\tt termorderflag = ESMF\_TERMORDER\_SRCSEQ} specified in the
! {\tt ESMF\_FieldRegrid()} call.
! The {\tt ESMF\_TERMORDER\_SRCSEQ} option ensures that the destination side
! waits for {\em all} of the source elements before summing into the destination
! element. This is critical for handling the dynamically masked 
! source and destination objects.
!
! So far in the example, only the {\tt srcField} had been dynamically masked.
! However, elements in the {\tt dstField} can be masked following exactly the
! same manner.
!
! In order to ensure that the {\tt dstField} is in a well defined condition, it
! is advisable to first reset it, e.g. to zero.
!EOE

!BOC
  call ESMF_FieldFill(dstField, dataFillScheme="const", const1=0.d0, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Now some of the destination elements can be set to a defined masking value.
!EOE

!BOC
  call ESMF_FieldGet(dstField, farrayPtr=farrayPtr, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  farrayPtr(lbound(farrayPtr,1)+1,lbound(farrayPtr,2)+1) = dstMaskValue
!EOC

!BOE
! The same {\tt routehandle} can now be executed, but with both
! {\tt dynamicSrcMaskValue}, and {\tt dynamicDstMaskValue} arguments specified.
!EOE

!BOC
  call ESMF_FieldRegrid(srcField=srcField, dstField=dstField, &
    routehandle=routehandle, termorderflag=ESMF_TERMORDER_SRCSEQ, &
    zeroregion=ESMF_REGION_EMPTY, &
    dynamicSrcMaskValue=srcMaskValue, &
    dynamicDstMaskValue=dstMaskValue, &
    dynamicMaskRoutine=simpleDynMaskProc, &
    rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
#if 1
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
! The above code demonstrated dynamic masking for source and destination Fields
! that contain double precision, i.e. {\tt real(ESMF\_KIND\_R8)} data. Switching
! to single precision, {\tt real(ESMF\_KIND\_R4)} data is straight forward.
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

!BOC
  srcTermProcessing=0

  call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
    srcTermProcessing=srcTermProcessing, routehandle=routehandle, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

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
! The dynamic mask values, as well as the dynamic mask routine is passed into
! the Field Regrid method as before. However, because of overload restrictions
! of most current compilers, we are not currently able to offer dynamic masking
! of different typekinds through a simple overloaded {\tt ESMF\_FieldRegrid()} 
! interface. Instead, the current prototype introduces typekind specific calls like
! {\tt ESMF\_FieldRegridR4R8R4()}. Here the three-part typekind signature at the end of the 
! method name indicates that the {\tt dynamicMaskRoutine} argument is expected
! to deal with {\tt real(ESMF\_KIND\_R4)} destination data (first typekind), 
! {\tt real(ESMF\_KIND\_R8)} factors (second typekind), and 
! {\tt real(ESMF\_KIND\_R4)} source data (third typekind).
!EOE

!BOC
  call ESMF_FieldRegridR4R8R4(srcField=srcField, dstField=dstField, &
    routehandle=routehandle, termorderflag=ESMF_TERMORDER_SRCSEQ, &
    zeroregion=ESMF_REGION_EMPTY, &
    dynamicSrcMaskValue=srcMaskValueR4, &
    dynamicDstMaskValue=dstMaskValueR4, &
    dynamicMaskRoutine=simpleDynMaskProcR4R8R4, &
    rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

#if 1
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
