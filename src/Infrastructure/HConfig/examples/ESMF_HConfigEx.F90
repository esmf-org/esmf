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
  type(ESMF_HConfig)              :: hconfig
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
  character(160)                  :: msgString
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

!BOE
! \subsubsection{Load HConfig from string using YAML syntax}
!
! An empty HConfig object can be loaded directly from a string using YAML
! syntax.
!EOE
!BOC
  call ESMF_HConfigLoad(hconfig, content="[1, 2, 3, abc, b, TRUE]", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The syntax used for the {\tt content} argument is YAML. The above case
! creates a {\em list} of six scalar members.
!EOE

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

!BOE
! \subsubsection{Index based random access HConfig parsing}
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

!BOE
! \subsubsection{Destroy an HConfig object}
!
! When done with {\tt hconfig}, it should be destroyed in the usual manner.
!EOE
!BOC
  call ESMF_HConfigDestroy(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! \subsubsection{Create an HConfig object directly loading from YAML string}
!
! The {\tt ESMF\_HConfigCreate()} method supports loading contents from
! string using YAML syntax directly via the optional {\tt content} argument.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfig
  hconfig = ESMF_HConfigCreate(content="{car: red, bike: 2, plane: TRUE}", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Here a map is created. In this case, all of the keys are scalars (car,
! bike, plane), as are all of the associated values (red, 2, TRUE).
!EOE

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
! of the {\tt As} access methods now are either of the form {\tt AsMapKey} or
! {\tt AsMapVal} to selectively access the {\em map key} or {\em map value},
! respectively.
!EOE
!BOC
  do while (hconfigIter /= hconfigIterEnd)

    ! Check whether the current element is a scalar both for the map key
    ! and the map value.
    ! logical :: isScalar
    isScalar = ESMF_HConfigIsMapKeyScalar(hconfigIter, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    isScalar = isScalar .and. ESMF_HConfigIsMapValScalar(hconfigIter, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    if (isScalar) then

      ! Any scalar can be accessed as a string. Use this for the {\em map key}.
      ! character(len=:), allocatable :: stringKey
      stringKey = ESMF_HConfigAsMapKeyString(hconfigIter, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! Now access the {\em map value}. Again first access as a string, which
      ! always works.
      ! character(len=:), allocatable :: string
      string = ESMF_HConfigAsMapValString(hconfigIter, rc=rc)
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
      valueI4 = ESMF_HConfigAsMapValI4(hconfigIter, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, &
        '("map key=", A10, " map value: asOkay: ", l2, " valueI4: ", i10)') &
        stringKey, asOkay, valueI4
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! integer(ESMF_KIND_I8) :: valueI8
      valueI8 = ESMF_HConfigAsMapValI8(hconfigIter, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, &
        '("map key=", A10, " map value: asOkay: ", l2, " valueI8: ", i10)') &
        stringKey, asOkay, valueI8
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! real(ESMF_KIND_R4) :: valueR4
      valueR4 = ESMF_HConfigAsMapValR4(hconfigIter, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, &
        '("map key=", A10, " map value: asOkay: ", l2, " valueR4: ", f10.6)') &
        stringKey, asOkay, valueR4
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! real(ESMF_KIND_R8) :: valueR8
      valueR8 = ESMF_HConfigAsMapValR8(hconfigIter, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, &
        '("map key=", A10, " map value: asOkay: ", l2, " valueR8: ", f10.6)') &
        stringKey, asOkay, valueR8
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! logical :: valueL
      valueL = ESMF_HConfigAsMapValLogical(hconfigIter, asOkay=asOkay, rc=rc)
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

!BOE
! \subsubsection{Key based random access HConfig map parsing}
!
! The {\em map values} stored in {\tt hconfig} can be accessed
! in random order providing the {\em map key}.
! 
! To demonstrate this, an array holding keys in random order is defined.
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
    isScalar = ESMF_HConfigIsScalar(hconfig, key=stringKey, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    if (isScalar) then

      ! Access as a string always works.
      ! character(len=:), allocatable :: string
      string = ESMF_HConfigAsString(hconfig, key=stringKey, rc=rc)
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
      valueI4 = ESMF_HConfigAsI4(hconfig, key=stringKey, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, &
        '("map key=", A10, " map value: asOkay: ", l2, " valueI4: ", i10)') &
        stringKey, asOkay, valueI4
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! integer(ESMF_KIND_I8) :: valueI8
      valueI8 = ESMF_HConfigAsI8(hconfig, key=stringKey, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, &
        '("map key=", A10, " map value: asOkay: ", l2, " valueI8: ", i10)') &
        stringKey, asOkay, valueI8
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! real(ESMF_KIND_R4) :: valueR4
      valueR4 = ESMF_HConfigAsR4(hconfig, key=stringKey, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, &
        '("map key=", A10, " map value: asOkay: ", l2, " valueR4: ", f10.6)') &
        stringKey, asOkay, valueR4
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! real(ESMF_KIND_R8) :: valueR8
      valueR8 = ESMF_HConfigAsR8(hconfig, key=stringKey, asOkay=asOkay, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write (msgString, &
        '("map key=", A10, " map value: asOkay: ", l2, " valueR8: ", f10.6)') &
        stringKey, asOkay, valueR8
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      ! logical :: valueL
      valueL = ESMF_HConfigAsLogical(hconfig, key=stringKey, asOkay=asOkay, rc=rc)
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
  isDefined = ESMF_HConfigIsDefined(hconfig, key="bad-key", rc=rc)
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

!BOE
! \subsubsection{Load HConfig from YAML file}
!
! One option is to first create an empty HConfig object, followed by calling
! {\tt ESMF\_HConfigLoadFile()} to load the content of the specified YAML file.
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
