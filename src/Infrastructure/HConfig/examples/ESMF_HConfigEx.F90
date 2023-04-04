! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2023, University Corporation for Atmospheric Research,
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

program ESMF_HConfigEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod

  implicit none

  ! local variables
  integer                         :: rc, size, i
  type(ESMF_HConfig)              :: hconfig, hconfigNode, hconfigNode2
  type(ESMF_HConfig)              :: hconfigIter, hconfigIterEnd
  type(ESMF_Config)               :: config
  logical                         :: asOkay, valueL, isDefined
  logical                         :: isNull, isScalar, isSequence, isMap
  character(len=:), allocatable   :: string, stringKey, tag
  integer(ESMF_KIND_I4)           :: valueI4
  integer(ESMF_KIND_I8)           :: valueI8
  real(ESMF_KIND_R4)              :: valueR4
  real(ESMF_KIND_R8)              :: valueR8
  character(5)                    :: keyList(3)
  character(160)                  :: msgString, filename
  ! result code
  integer                     :: finalrc, result
  character(ESMF_MAXSTR)      :: testname
  character(ESMF_MAXSTR)      :: failMsg


  finalrc = ESMF_SUCCESS

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_HConfigEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_Initialize(defaultlogfilename="HConfigEx.Log", &
    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------------
!BOE
! \subsubsection{Create an empty HConfig object}
!
! By default, {\tt ESMF\_HConfigCreate()} creates an empty HConfig object.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfig
  hconfig = ESMF_HConfigCreate(rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------------
!BOE
! \subsubsection{Set HConfig from string using YAML syntax}
!
! An empty HConfig object can be set directly from a string using YAML
! syntax.
!EOE
!BOC
  call ESMF_HConfigSet(hconfig, content="[1, 2, 3, abc, b, TRUE]", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The syntax used for the {\tt content} argument is YAML. The above case
! creates a {\em list} of six scalar members.
!EOE

!-------------------------------------------------------------------------------
!BOE
! \subsubsection{Iterator based HConfig list parsing}
!
! An easy way to parse the elements contained in {\tt hconfig} is to iterate
! through them. Two additional HConfig objects, acting as iterators, are needed.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfigIter, hconfigIterEnd
  hconfigIter = ESMF_HConfigIterBegin(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  hconfigIterEnd = ESMF_HConfigIterEnd(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Both {\tt hconfigIter} and {\tt hconfigIterEnd} are merely references, not
! associated with their own deep allocation. In other words, neither was
! created by an assignment that had a {\tt Create()} call on the righ hand side.
! As such, neither {\tt hconfigIter} nor {\tt hconfigIterEnd} need to be
! explicitly destroyed when done.
!
! Iterating over {\tt hconfig} is straight forward:
!EOE
!BOC
  do while (hconfigIter /= hconfigIterEnd)

    ! Check whether the current element is a scalar.
    ! logical :: isScalar
    isScalar = ESMF_HConfigIsScalar(hconfigIter, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    if (isScalar) then

      ! Any scalar can be accessed as a string.
      ! character(len=:), allocatable :: string
      string = ESMF_HConfigAsString(hconfigIter, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_LogWrite("AsString: "//string, ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! The attempt can be made to interpret the scalar as any of the other
      ! supported data types. By default, if the scalar cannot be interpreted
      ! as the requested data type, rc /= ESMF_SUCCESS is returned. To prevent
      ! such error condition, the optional, intent(out) argument "asOkay" can
      ! be provided. If asOkay == .true. is returned, the interpretation was
      ! successful. Otherwise asOkay == .false. is returned.
      ! logical :: asOkay

      ! integer(ESMF_KIND_I4) :: valueI4
      valueI4 = ESMF_HConfigAsI4(hconfigIter, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, '("asOkay: ", l2, " valueI4: ", i10)') asOkay, valueI4
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! integer(ESMF_KIND_I8) :: valueI8
      valueI8 = ESMF_HConfigAsI8(hconfigIter, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, '("asOkay: ", l2, " valueI8: ", i10)') asOkay, valueI8
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! real(ESMF_KIND_R4) :: valueR4
      valueR4 = ESMF_HConfigAsR4(hconfigIter, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, '("asOkay: ", l2, " valueR4: ", f10.6)') asOkay, valueR4
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! real(ESMF_KIND_R8) :: valueR8
      valueR8 = ESMF_HConfigAsR8(hconfigIter, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, '("asOkay: ", l2, " valueR8: ", f10.6)') asOkay, valueR8
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! logical :: valueL
      valueL = ESMF_HConfigAsLogical(hconfigIter, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, '("asOkay: ", l2, " valueL:  ", l10)') asOkay, valueL
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    else
      ! Possible recursive iteration over the current hconfigIter element.
    endif

    ! Next iteration step.
    call ESMF_HConfigIterNext(hconfigIter, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  enddo
!EOC

!-------------------------------------------------------------------------------
!BOE
! \subsubsection{Index based random access HConfig list parsing}
!
! An alternative way to loop over the elements contained in {\tt hconfig},
! and parsing them, is to use an {\tt index} variable. For this approach the
! size of {\tt hconfig} is queried.
!EOE
!BOC
  ! integer :: size
  size = ESMF_HConfigGetSize(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Then looping over the elements is done with a simple do loop. Index based
! access allows random order of access, versus the iterator approach that
! only supports begin to end iteration. This is demonstrated here by writing
! the do loop in reverse order.
!EOE
!BOC
  ! integer :: i
  do i=size, 1, -1

    ! Check whether the current element is a scalar.
    ! logical :: isScalar
    isScalar = ESMF_HConfigIsScalar(hconfig, index=i, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    if (isScalar) then

      ! Any scalar can be accessed as a string.
      ! character(len=:), allocatable :: string
      string = ESMF_HConfigAsString(hconfig, index=i, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_LogWrite("AsString: "//string, ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! The attempt can be made to interpret the scalar as any of the other
      ! supported data types. By default, if the scalar cannot be interpreted
      ! as the requested data type, rc /= ESMF_SUCCESS is returned. To prevent
      ! such error condition, the optional, intent(out) argument "asOkay" can
      ! be provided. If asOkay == .true. is returned, the interpretation was
      ! successful. Otherwise asOkay == .false. is returned.
      ! logical :: asOkay

      ! integer(ESMF_KIND_I4) :: valueI4
      valueI4 = ESMF_HConfigAsI4(hconfig, index=i, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, '("asOkay: ", l2, " valueI4: ", i10)') asOkay, valueI4
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! integer(ESMF_KIND_I8) :: valueI8
      valueI8 = ESMF_HConfigAsI8(hconfig, index=i, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, '("asOkay: ", l2, " valueI8: ", i10)') asOkay, valueI8
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! real(ESMF_KIND_R4) :: valueR4
      valueR4 = ESMF_HConfigAsR4(hconfig, index=i, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, '("asOkay: ", l2, " valueR4: ", f10.6)') asOkay, valueR4
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! real(ESMF_KIND_R8) :: valueR8
      valueR8 = ESMF_HConfigAsR8(hconfig, index=i, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, '("asOkay: ", l2, " valueR8: ", f10.6)') asOkay, valueR8
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! logical :: valueL
      valueL = ESMF_HConfigAsLogical(hconfig, index=i, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, '("asOkay: ", l2, " valueL:  ", l10)') asOkay, valueL
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    else
      ! Possible recursive iteration over the current index=i element.
    endif
  enddo
!EOC
!BOE
! The above loop is safe with respect to {\tt index} potentially being specified
! with an out-of-range value. This is because {\tt ESMF\_HConfigIsScalar()}
! returns {\tt .false.} in this case. There are only four valid options of what
! {\tt type} a valid HConfig element can be. Each has an associated {\tt Is}
! method:
! \begin{itemize}
! \item Null: {\tt ESMF\_HConfigIsNull()}
! \item Scalar: {\tt ESMF\_HConfigIsScalar()}
! \item Sequence: {\tt ESMF\_HConfigIsSequence()}
! \item Map: {\tt ESMF\_HConfigIsMap()}
! \end{itemize}
! The general check to see whether an index points to a valid element is
! provided by {\tt ESMF\_HConfigIsDefined()}.
!EOE
!BOC
  ! logical :: isDefined
  isDefined = ESMF_HConfigIsDefined(hconfig, index=10, rc=rc)
!BOE
! This returns {\tt isDefined == .false.} because for {\tt hconfig} a value of
! {\tt index=10} is out of range.
!EOE

!-------------------------------------------------------------------------------
!BOE
! \subsubsection{Destroy an HConfig object}
!
! When done with {\tt hconfig}, it should be destroyed in the usual manner.
!EOE
!BOC
  call ESMF_HConfigDestroy(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------------
!BOE
! \subsubsection{Create an HConfig object directly loading from YAML string}
!
! The {\tt ESMF\_HConfigCreate()} method supports loading contents from
! string using YAML syntax directly via the optional {\tt content} argument.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfig
  hconfig = ESMF_HConfigCreate(content="{car: red, bike: 22, plane: TRUE}", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Here a map is created. In this case, all of the keys are scalars (car,
! bike, plane), as are all of the associated values (red, 22, TRUE).
!EOE

!-------------------------------------------------------------------------------
!BOE
! \subsubsection{Iterator based HConfig map parsing}
!
! The elements of the {\em map} contained in {\tt hconfig} can be iterated over
! similar to the the {\em list} case demonstrated earlier. Again two iterator
! variables are employed.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfigIter, hconfigIterEnd
  hconfigIter = ESMF_HConfigIterBegin(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  hconfigIterEnd = ESMF_HConfigIterEnd(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Then iterate over {\tt hconfig} using the iterator variables as before.
! Notice, however, in the code below, compared to the {\em list} case, all 
! of the {\tt As} access methods now are either of the form {\tt As*MapKey} or
! {\tt As*MapVal} to selectively access the {\em map key} or {\em map value},
! respectively.
!EOE
!BOC

  do while (hconfigIter /= hconfigIterEnd)

    ! Check whether the current element is a scalar both for the map key
    ! and the map value.
    ! logical :: isScalar
    isScalar = ESMF_HConfigIsScalarMapKey(hconfigIter, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    isScalar = isScalar .and. ESMF_HConfigIsScalarMapVal(hconfigIter, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    if (isScalar) then

      ! Any scalar can be accessed as a string. Use this for the map key.
      ! character(len=:), allocatable :: stringKey
      stringKey = ESMF_HConfigAsStringMapKey(hconfigIter, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! Now access the map value. Again first access as a string, which
      ! always works.
      ! character(len=:), allocatable :: string
      string = ESMF_HConfigAsStringMapVal(hconfigIter, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_LogWrite("map key="//stringKey//" map value: AsString: "// &
        string, ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! The attempt can be made to interpret the scalar as any of the other
      ! supported data types. By default, if the scalar cannot be interpreted
      ! as the requested data type, rc /= ESMF_SUCCESS is returned. To prevent
      ! such error condition, the optional, intent(out) argument "asOkay" can
      ! be provided. If asOkay == .true. is returned, the interpretation was
      ! successful. Otherwise asOkay == .false. is returned.
      ! logical :: asOkay

      ! integer(ESMF_KIND_I4) :: valueI4
      valueI4 = ESMF_HConfigAsI4MapVal(hconfigIter, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, &
        '("map key=", A10, " map value: asOkay: ", l2, " valueI4: ", i10)') &
        stringKey, asOkay, valueI4
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! integer(ESMF_KIND_I8) :: valueI8
      valueI8 = ESMF_HConfigAsI8MapVal(hconfigIter, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, &
        '("map key=", A10, " map value: asOkay: ", l2, " valueI8: ", i10)') &
        stringKey, asOkay, valueI8
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! real(ESMF_KIND_R4) :: valueR4
      valueR4 = ESMF_HConfigAsR4MapVal(hconfigIter, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, &
        '("map key=", A10, " map value: asOkay: ", l2, " valueR4: ", f10.6)') &
        stringKey, asOkay, valueR4
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! real(ESMF_KIND_R8) :: valueR8
      valueR8 = ESMF_HConfigAsR8MapVal(hconfigIter, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, &
        '("map key=", A10, " map value: asOkay: ", l2, " valueR8: ", f10.6)') &
        stringKey, asOkay, valueR8
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! logical :: valueL
      valueL = ESMF_HConfigAsLogicalMapVal(hconfigIter, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, &
        '("map key=", A10, " map value: asOkay: ", l2, " valueL: ", l10)') &
        stringKey, asOkay, valueL
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    else
      ! Deal with case where either key or value are not scalars themselves.
    endif

    ! Next iteration step.
    call ESMF_HConfigIterNext(hconfigIter, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  enddo
!EOC

!-------------------------------------------------------------------------------
!BOE
! \subsubsection{Key based random access HConfig map parsing}
!
! The {\em map values} stored in {\tt hconfig} can be accessed
! in random order providing the {\em map key}.
! 
! To demonstrate this, a temporary array holding keys in random order is defined.
!EOE
!BOC
  ! character(5) :: keyList(3)
  keyList = ["bike ", "plane", "car  "]
!EOC
!BOE
! Then loop over the elements of {\tt keyList} and use them as {\em map key}
! to access the {\em map values} in {\tt hconfig}.
!EOE
!BOC
  ! integer :: i
  do i=1,3

    ! Ensure that all white space padding is removed.
    ! character :: stringKey
    stringKey = trim(keyList(i))

    ! Check whether the accessed map value is a scalar.
    ! logical :: isScalar
    isScalar = ESMF_HConfigIsScalar(hconfig, keyString=stringKey, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    if (isScalar) then

      ! Access as a string always works.
      ! character(len=:), allocatable :: string
      string = ESMF_HConfigAsString(hconfig, keyString=stringKey, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_LogWrite("map key="//stringKey//" map value: AsString: "// &
        string, ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! The attempt can be made to interpret the scalar as any of the other
      ! supported data types. By default, if the scalar cannot be interpreted
      ! as the requested data type, rc /= ESMF_SUCCESS is returned. To prevent
      ! such error condition, the optional, intent(out) argument "asOkay" can
      ! be provided. If asOkay == .true. is returned, the interpretation was
      ! successful. Otherwise asOkay == .false. is returned.
      ! logical :: asOkay

      ! integer(ESMF_KIND_I4) :: valueI4
      valueI4 = ESMF_HConfigAsI4(hconfig, keyString=stringKey, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, &
        '("map key=", A10, " map value: asOkay: ", l2, " valueI4: ", i10)') &
        stringKey, asOkay, valueI4
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! integer(ESMF_KIND_I8) :: valueI8
      valueI8 = ESMF_HConfigAsI8(hconfig, keyString=stringKey, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, &
        '("map key=", A10, " map value: asOkay: ", l2, " valueI8: ", i10)') &
        stringKey, asOkay, valueI8
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! real(ESMF_KIND_R4) :: valueR4
      valueR4 = ESMF_HConfigAsR4(hconfig, keyString=stringKey, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, &
        '("map key=", A10, " map value: asOkay: ", l2, " valueR4: ", f10.6)') &
        stringKey, asOkay, valueR4
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! real(ESMF_KIND_R8) :: valueR8
      valueR8 = ESMF_HConfigAsR8(hconfig, keyString=stringKey, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, &
        '("map key=", A10, " map value: asOkay: ", l2, " valueR8: ", f10.6)') &
        stringKey, asOkay, valueR8
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! logical :: valueL
      valueL = ESMF_HConfigAsLogical(hconfig, keyString=stringKey, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, &
        '("map key=", A10, " map value: asOkay: ", l2, " valueL: ", l10)') &
        stringKey, asOkay, valueL
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    else
      ! Deal with case where either key or value are not scalars themselves.
    endif

  enddo
!EOC
!BOE
! The above loop is safe with respect to {\tt stringKey} potentially
! specifying a value that is not a valid {\em map key}. This is because
! {\tt ESMF\_HConfigIsScalar()} returns {\tt .false.} in this case.
!
! The general check to see whether a {\em map key} refers to a valid element
! is provided by {\tt ESMF\_HConfigIsDefined()}.
!EOE
!BOC
  ! logical :: isDefined
  isDefined = ESMF_HConfigIsDefined(hconfig, keyString="bad-key", rc=rc)
!BOE
! This returns {\tt isDefined == .false.} because {\tt hconfig} does not
! contain {\tt "bad-key"} as one of its valid {\em map keys}.
!
! Finally destroy {\tt hconfig} when done.
!EOE
!BOC
  call ESMF_HConfigDestroy(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------------
!BOE
! \subsubsection{Access HConfig from Config}
!
! The {\tt ESMF\_Config} class can be queried for an HConfig object. This allows
! the use of the HConfig API to access information contained in a Config object.
!EOE
  config = ESMF_ConfigCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ConfigLoadFile(config, filename="example.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! type(ESMF_Config) :: config
  ! type(ESMF_HConfig) :: hconfig
  call ESMF_ConfigGet(config, hconfig=hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The {\tt hconfig} obtained this way is indistinguishable from an explicitly
! created HConfig instance. E.g. it can be queried for its {\em type} using the
! {\tt Is} methods:
!BOC
  ! logical :: isDefined
  isDefined = ESMF_HConfigIsDefined(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("HConfig from Config IsDefined: ", l2)') isDefined
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! logical :: isNull
  isNull = ESMF_HConfigIsNull(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("HConfig from Config IsNull: ", l2)') isNull
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! logical :: isSequence
  isSequence = ESMF_HConfigIsSequence(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("HConfig from Config IsSequence: ", l2)') isSequence
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! logical :: isMap
  isMap = ESMF_HConfigIsMap(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("HConfig from Config IsMap: ", l2)') isMap
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Once done with {\tt hconfig} it must {\em not} be destroyed explicitly by
! the user. The {\tt hconfig} is still owned by the {\tt config} object, and
! will be destroyed automatically when the {\tt config} object is destroyed.
! This follows the simple rule that a user only owns those objects created
! explicitly by calling a {\tt Create()} method.
!EOE
  call ESMF_ConfigDestroy(config, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------------
!BOE
! \subsubsection{Load HConfig from YAML file}
!
! One option to load a YAML file is to first create an empty HConfig object,
! followed by calling {\tt ESMF\_HConfigLoadFile()}.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfig
  hconfig = ESMF_HConfigCreate(rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_HConfigLoadFile(hconfig, filename="example.yaml", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! When done destroy as usual.
  call ESMF_HConfigDestroy(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The alternative option is to create and load the HConfig object in a single
! call to {\tt ESMF\_HConfigCreate()} using the optional {\tt filename}
! argument to specify the YAML file.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfig
  hconfig = ESMF_HConfigCreate(filename="example.yaml", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! And again destroy hconfig when done with it.
  call ESMF_HConfigDestroy(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------------
!BOE
! \subsubsection{Save HConfig to YAML file}
!
! An HConfig object can be saved to a YAML file by calling the
! {\tt ESMF\_HConfigSaveFile()} method. To demonstrate this, a YAML file
! containing:
! \begin{verbatim}
! # An example of YAML configuration file
!
! simple_list: [1, 2, 3, abc, b, TRUE]
! simple_map: {car: red, [bike, {p1: 10, p2: 20}]: [bmx, mountain, street], plane: [TRUE, FALSE]}
! \end{verbatim}
!
! is loaded to create the {\tt hconfig} object:
!EOE
!BOC
  hconfig = ESMF_HConfigCreate(filename="example.yaml", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Now the {\tt hconfig} object can be saved to file using the
! {\tt ESMF\_HConfigSaveFile()} method.
!EOE
!BOC
  call ESMF_HConfigSaveFile(hconfig, filename="saveMe.yml", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Notice that the resulting contents of file {\tt saveMe.yml} does {\em not}
! contain the comments of the original file. The YAML structure is saved.
! \begin{verbatim}
! simple_list: [1, 2, 3, abc, b, TRUE]
! simple_map: {car: red, [bike, {p1: 10, p2: 20}]: [bmx, mountain, street], plane: [TRUE, FALSE]}
! \end{verbatim}
!
! The object specified in {\tt ESMF\_HConfigSaveFile()} can be a regular node
! (of any type) or a {\em sequence} iterator. In either case the file written
! represents the YAML hierarchy with the specified object as the root node.
!
! In the case of a {\em map} iterator, it is necessary to first create an
! appropriate root node utilizing the appropriate {\tt CreateAt} method. This
! allows to either save the {\em map key} or {\em map value} node at the
! current iterator. This is demonstrated below.
!
! In the current example, where {\tt hconfig} is a map with two elements, a
! map iterator can be set to the beginning using the following call.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfigIter
  hconfigIter = ESMF_HConfigIterBegin(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Here {\tt hconfigIter} cannot be be saved to file directly. To write the
! {\em key} node, first create a HConfig object for it using method
! {\tt ESMF\_HConfigCreateAtMapKey()}.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfigNode
  hconfigNode = ESMF_HConfigCreateAtMapKey(hconfigIter, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Then save it.
!EOE
!BOC
  call ESMF_HConfigSaveFile(hconfigNode, filename="mapKeyBegin.yaml", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! And finally destroy {\tt hconfigNode} again.
!EOE
!BOC
  call ESMF_HConfigDestroy(hconfigNode, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Similarily, to write the {\em value} node to file, first create a HConfig
! object for it using method {\tt ESMF\_HConfigCreateAtMapVal()}.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfigNode
  hconfigNode = ESMF_HConfigCreateAtMapVal(hconfigIter, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Then save it.
!EOE
!BOC
  call ESMF_HConfigSaveFile(hconfigNode, filename="mapValBegin.yaml", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! And destroy it.
!EOE
!BOC
  call ESMF_HConfigDestroy(hconfigNode, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Since {\tt hconfig} is a {\em map} node, it is also possible to directly
! create a {\em value} node by calling {\tt ESMF\_HConfigCreateAt()}
! on it, using the desired {\em key}.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfigNode
  hconfigNode = ESMF_HConfigCreateAt(hconfig, keyString="simple_map", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Now {\tt hconfigNode} points to the {\em value} node, that is
! associated with the "simple\_map" key, which is in turn a map:
! \begin{verbatim}
! {car: red, [bike, {p1: 10, p2: 20}]: [bmx, mountain, street], plane: [TRUE, FALSE]}
! \end{verbatim}
! It can be saved to file as usual.
!EOE
!BOC
  call ESMF_HConfigSaveFile(hconfigNode, filename="mapValAtKey.yaml", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Any of the {\em value} nodes of {\tt hconfigNode} can be accessed through
! recursive usage of the {\tt ESMF\_HConfigCreateAt()} method.
! For example, the following call accesses the {\em value} node that is
! associated with {\tt keyString="[bike, {p1: 10, p2: 20}]"}. Here the
! {\tt keyString} is interpreted as YAML syntax, for which an internal HConfig
! representation is created, and finally the map held by {\tt hconfigNode} is
! searched for a matching key.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfigNode2
  hconfigNode2 = ESMF_HConfigCreateAt(hconfigNode, &
    keyString="[bike, {p1: 10, p2: 20}]", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Now {\tt hconfigNode2} points to the {\em list node} with contents
! {\tt [bmx, mountain, street]}. It, too, can be saved to file.
!EOE
!BOC
  call ESMF_HConfigSaveFile(hconfigNode2, filename="mapValRecursive.yaml", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Finally {\tt hconfigNode2}, {\tt hconfigNode} and {\tt hconfig} should be destroyed.
!EOE
!BOC
  call ESMF_HConfigDestroy(hconfigNode2, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_HConfigDestroy(hconfigNode, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_HConfigDestroy(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


!-------------------------------------------------------------------------------
!BOE
! \subsubsection{Tags and Schemas}
!
! The HConfig class implements tags to identify a node's data type according to
! the YAML standard. The combination of a set of defined tags and a mechansim
! to resolve non-specific tags is called a schema under YAML. Currently the
! HConfig class implements the JSON schema. This setting can not be changed.
!
! This example starts with an empty HConfig object.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfig
  hconfig = ESMF_HConfigCreate(rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Method {\tt ESMF\_HConfigGetTag()} is used to query the tag.
!EOE
!BOC
  ! character(len=:), allocatable :: tag
  tag = ESMF_HConfigGetTag(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("Empty HConfig tag:       ", A30)') tag
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! \paragraph{Null}
! The {\tt hconfig} is an empty object, in other words it is associated with
! NULL. The JSON schema tag for this situation is
! {\tt{\bf tag:yaml.org,2002:null}}.
!
! Next, file {\tt exampleWithTags.yaml} is loaded.
!BOC
  call ESMF_HConfigLoadFile(hconfig, filename="exampleWithTags.yaml", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The file contains the following YAML:
! \begin{verbatim}
! value_one:    {word1: this, word2: is, word3: a, word4: map}
! value_two:    [this, is, a, list]
! value_three:            123
! value_four:   !!float   123
! value_five:             2.5
! value_six:    !!str     2.5
! value_seven:            False
! value_eight:  !!str     true
! value_nine:             0x234
! value_ten:              Null
! value_eleven:
! value_twelve:  !myStuff xyz
! \end{verbatim}
!
! The value associated with {\em map key} "value\_ten" is explicitly set to
! {\tt Null}. The associated tag for this node can be obtained
! directly by supplying the {\tt keyString} argument.
!EOE
!BOC
  tag = ESMF_HConfigGetTag(hconfig, keyString="value_ten", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("value_ten HConfig tag:  ", A30)') tag
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The resolved JSON schema tag is again {\tt{\bf tag:yaml.org,2002:null}}. There
! are four special values that resolve to this tag: {\tt null}, {\tt Null},
! {\tt NULL}, and {\tt $\sim$}. In addition to those special values, an {\em empty}
! value, as demonstrated by {\em key} "value\_eleven", also automatically
! resolves to {\tt{\bf tag:yaml.org,2002:null}}.
!EOE
!BOC
  tag = ESMF_HConfigGetTag(hconfig, keyString="value_eleven", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("value_eleven HConfig tag:  ", A30)') tag
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! \paragraph{Map}
! On the top level, after loading the YAML file, {\tt hconfig} is a map. 
! Querying again for the tag of {\tt hconfig},
!EOE
!BOC
  tag = ESMF_HConfigGetTag(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("Top level HConfig tag: ", A30)') tag
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! results in the JSON schema tag of {\tt{\bf tag:yaml.org,2002:map}}.
!
! \paragraph{List}
! The value associated with {\em map key} "value\_two" in the current
! {\tt hconfig} object is a list. The tag for this node can be obtained
! directly by supplying the {\tt keyString} argument.
!EOE
!BOC
  tag = ESMF_HConfigGetTag(hconfig, keyString="value_two", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("value_two HConfig tag:  ", A30)') tag
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The resolved JSON schema tag for a list is {\tt{\bf tag:yaml.org,2002:seq}}.
!
! \paragraph{String}
! All of the {\em keys} of the currently loaded {\tt hconfig} object are
! strings. To obtain the tag that is associated with the first key node,
! an iterator is used to access the map nodes individually.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfigIter
  hconfigIter = ESMF_HConfigIterBegin(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Now the {\tt ESMF\_HConfigGetTagMapKey()} method can be used to obtain
! the tag for the first key node.
!EOE
!BOC
  tag = ESMF_HConfigGetTagMapKey(hconfigIter, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("first key HConfig tag:  ", A30)') tag
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Here the JSON schema tag resolves to {\tt{\bf tag:yaml.org,2002:str}}.
!
! \paragraph{Integer}
! The value associated with {\em map key} "value\_three" in the current
! {\tt hconfig} object is an integer number. The tag for this node can be
! obtained as before by directly supplying the {\tt keyString} argument.
!EOE
!BOC
  tag = ESMF_HConfigGetTag(hconfig, keyString="value_three", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("value_three HConfig tag:  ", A30)') tag
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The JSON schema tag resolves to {\tt{\bf tag:yaml.org,2002:int}}.
!
! The value associated with {\em map key} "value\_nine" in the current
! {\tt hconfig} object is an integer number in hex. The tag for this node can be
! obtained as before by directly supplying the {\tt keyString} argument.
!EOE
!BOC
  tag = ESMF_HConfigGetTag(hconfig, keyString="value_nine", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("value_nine HConfig tag:  ", A30)') tag
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The JSON schema tag resolves to {\tt{\bf tag:yaml.org,2002:int}}.
!
! \paragraph{Floating Point}
! The value associated with {\em map key} "value\_five" in the current
! {\tt hconfig} object is a floating point number. The tag for this node can be
! obtained as before by directly supplying the {\tt keyString} argument.
!EOE
!BOC
  tag = ESMF_HConfigGetTag(hconfig, keyString="value_five", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("value_five HConfig tag:    ", A30)') tag
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The JSON schema tag resolves to {\tt{\bf tag:yaml.org,2002:float}}.
!
! \paragraph{Boolean}
! The value associated with {\em map key} "value\_seven" in the current
! {\tt hconfig} object is a boolean. The tag for this node can be
! obtained as before by directly supplying the {\tt keyString} argument.
!EOE
!BOC
  tag = ESMF_HConfigGetTag(hconfig, keyString="value_seven", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("value_seven HConfig tag:    ", A30)') tag
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The JSON schema tag resolves to {\tt{\bf tag:yaml.org,2002:bool}}. The
! supported boolean values are {\tt true}, {\tt True}, {\tt TRUE},
! {\tt false}, {\tt False}, and {\tt FALSE}.
!
! \paragraph{Explicit standard tags}
! Standard short-hand tags can be specified to change the default resolution.
! This is demonstrated for {\em map keys} "value\_four", "value\_six", and
! "value\_eight".
!EOE
!BOC
  tag = ESMF_HConfigGetTag(hconfig, keyString="value_four", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("value_four HConfig tag:    ", A30)') tag
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  tag = ESMF_HConfigGetTag(hconfig, keyString="value_six", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("value_six HConfig tag:    ", A30)') tag
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  tag = ESMF_HConfigGetTag(hconfig, keyString="value_eight", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("value_eight HConfig tag:    ", A30)') tag
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The default resolution of these three keys would be
! {\tt tag:yaml.org,2002:int}, {\tt tag:yaml.org,2002:float}, and
! {\tt tag:yaml.org,2002:bool}, respectively. However, with the explict tags
! in place, they are resolved to {\tt tag:yaml.org,2002:float},
! {\tt tag:yaml.org,2002:str}, {\tt tag:yaml.org,2002:str}, instead.
!
! \paragraph{Explicit custom tags}
! The HConfig class supports application-specific local tags as per the YAML
! standard. These are tags that are not known by the JSON schema. If such a
! tag is encountered on a node, it is preserved and no further automatic
! tag resolution is performed.
!
! The value associated with {\em map key} "value\_twelve" in the current
! {\tt hconfig} object has a custom tag. The tag for this node can be
! obtained as before by directly supplying the {\tt keyString} argument.
!EOE
!BOC
  tag = ESMF_HConfigGetTag(hconfig, keyString="value_twelve", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("value_twelve HConfig tag:    ", A30)') tag
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The returned tag is {\tt{\bf !myStuff}}.
!
! Finally clean up {\tt hconfig}.
!EOE
!BOC
  ! Destroy hconfig when done with it.
  call ESMF_HConfigDestroy(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------------
!BOE
! \subsubsection{Building and Editing HConfig structures}
!
! Here describe how to do it...
!EOE

  hconfig = ESMF_HConfigCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigSaveFile(hconfig, filename="debug0.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigAdd(hconfig, "first added item", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigSaveFile(hconfig, filename="debug0.yaml.1", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigAdd(hconfig, 12.57_ESMF_KIND_R8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigSaveFile(hconfig, filename="debug0.yaml.2", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigLoadFile(hconfig, filename="exampleWithTags.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigSaveFile(hconfig, filename="debug1.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigAdd(hconfig, "first added item", &
    addKeyString="[test_key1, test_key2, list]", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigSaveFile(hconfig, filename="debug1.yaml.1", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  hconfigNode = ESMF_HConfigCreateAt(hconfig, keyString="value_nine", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigSaveFile(hconfigNode, filename="debug2.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!  call ESMF_HConfigLoadFile(hconfigNode, filename="example.yaml", rc=rc)
!  call ESMF_HConfigSet(hconfigNode, content="test value", rc=rc)
  call ESMF_HConfigSet(hconfig, content=9876, keyString="value_nine", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigSaveFile(hconfigNode, filename="debug3.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigDestroy(hconfigNode, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  hconfigIter = ESMF_HConfigIterBegin(hconfig, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_HConfigSetMapVal(hconfigIter, content=12.d6, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigSaveFile(hconfig, filename="debug4.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  hconfigIter = ESMF_HConfigIterBegin(hconfig, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigIterNext(hconfigIter, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigAddMapVal(hconfigIter, "[aa, bb, cc]", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigAddMapVal(hconfigIter, "aaa", index=5, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  hconfigNode = ESMF_HConfigCreateAtMapKey(hconfigIter, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigLoadFile(hconfigNode, filename="example.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigSaveFile(hconfig, filename="debug5.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  hconfigIter = ESMF_HConfigCreate(hconfig, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigSaveFile(hconfigIter, filename="debugIter5.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigSet(hconfigIter, content="NULL", rc=rc)  ! wipe out all, single scalar
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigSaveFile(hconfig, filename="debug6.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigSaveFile(hconfigIter, filename="debugIter6.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigDestroy(hconfig, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigAdd(hconfigIter, "(5, 3)", addKeyString="myKey1", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigAdd(hconfigIter, "here a string", addKeyString="myKey2", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigSaveFile(hconfigIter, filename="debugIter7.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_HConfigEx.F90"
  else
    print *, "FAIL: ESMF_HConfigEx.F90"
  endif

end program
