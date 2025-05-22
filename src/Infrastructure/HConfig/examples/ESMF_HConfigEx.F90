! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
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
  integer                         :: rc, size, i, docCount
  type(ESMF_HConfig)              :: hconfig, hconfigTemp, hconfigTemp2
  type(ESMF_HConfigIter)          :: hconfigIter, hconfigIterBegin, hconfigIterEnd
  type(ESMF_Config)               :: config
  type(ESMF_HConfigMatch_Flag)    :: match
  logical                         :: asOkay, valueL, isDefined
  logical                         :: isNull, isScalar, isSequence, isMap
  logical                         :: isAlias, isNotAlias, isExact, isMatch
  logical                         :: isNone
  logical, allocatable            :: valueLSeq(:)
  character(len=:), allocatable   :: string, stringKey, tag
  character(len=:), allocatable   :: valueSSeq(:)
  integer(ESMF_KIND_I4)           :: valueI4
  integer(ESMF_KIND_I4), allocatable :: valueI4Seq(:)
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
! This sets {\tt hconfig} as a {\em sequence} of six scalar members.
!EOE

!-------------------------------------------------------------------------------
!BOE
! \subsubsection{Iterator based HConfig sequence parsing}
!
! One way to parse the elements contained in {\tt hconfig} is to use the
! iterator pattern known from laguages such as C++ or Python. HConfig
! iterators are implemented as {\tt type(ESMF\_HConfigIter)} objects that
! are initialized using one of the {\tt HConfigIter*()} methods. An iterator
! can then be used to traverse the elements in a {\em sequence} or {\em map}
! by calling the {\tt ESMF\_HConfigIterNext()} method, taking one step forward
! each time the method is called.
!
! Being a HConfig object, an iterator can be passed into any of the usual
! HConfig methods. The operation is applied to the element that the iterator is
! currently referencing. 
!
! Notice that iterators are merely references, not associated with their own
! deep allocation. This is reflected in the fact that iterators are
! {\em not} created by an assignment that has a {\tt Create()} call on the right
! hand side. As such, HConfig iterators need {\em not} be destroyed
! explicitly when done.
!
! Two special HConfig iterators are defined, referencing the beginning and
! the end of a HConfig sequence or map object.
!EOE
!BOC
  ! type(ESMF_HConfigIter) :: hconfigIterBegin, hconfigIterEnd
  hconfigIterBegin = ESMF_HConfigIterBegin(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  hconfigIterEnd = ESMF_HConfigIterEnd(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! In analogy to the C++ iterator pattern, {\tt hconfigIterBegin} points to the
! first element in {\tt hconfig}, while {\tt hconfigIterEnd} points one step
! beyond the last element. Using these elements together, an iterator loop
! can be written in the following intuitive way, using {\tt hconfigIter} as the
! loop variable.
!EOE
!BOC
  ! type(ESMF_HConfigIter) :: hconfigIter
  hconfigIter = hconfigIterBegin
  do while (hconfigIter /= hconfigIterEnd)

    ! Code here that uses hconfigIter
    ! to access the currently referenced
    ! element in hconfig.  .......

    call ESMF_HConfigIterNext(hconfigIter, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  enddo
!EOC
!BOE
! One {\em major} concern with the above iterator loop implementation is when
! Fortran {\tt cycle} statements are introduced. In order to make the above loop
! {\tt cycle}--safe, each such {\tt cycle} statement needs to be matched with
! its own call to {\tt ESMF\_HConfigIterNext()}. This needs to be done to
! prevent endless-loop conditions, where the exit condition of the
! {\tt do while} is never reached.
!
! The {\tt cycle}--safe alternative implementation of the iterator loop
! leverages the {\tt ESMF\_HConfigIterLoop()} function instead of 
! {\tt ESMF\_HConfigIterNext()}. This approach is more akin to the
! C++
! \begin{verbatim}
!   for (element : container){
!     ...
!   }
! \end{verbatim}
! or the Python
! \begin{verbatim}
!   for element in container:
!     ...
! \end{verbatim}
! approach. It is the preferable way to write HConfig iterator loops due to its
! simplicity and inherent {\tt cycle}--safety.
!
! The {\tt ESMF\_HConfigIterLoop()} function takes three required arguments.
! The first is the loop iterator, followed by the begin and end iterators. The
! loop iterator must enter equal to the begin iterator at the start of the loop.
! Each time the {\tt ESMF\_HConfigIterLoop()} function is called, the loop
! iterator is stepped forward as appropriate, and the exit condition of having
! reached the end iterator is checked. Having both the stepping and exit logic
! in one place provided by the HConfig API simplifies the usage. In addition,
! the approach is {\tt cycle}--safe: no matter where a {\tt cycle} statement is
! inserted in the loop body, it always brings the execution back to the top of
! the while loop, which in turn calls the {\tt ESMF\_HConfigIterLoop()}
! function.
!EOE
!BOC
  ! type(ESMF_HConfigIter) :: hconfigIter
  hconfigIter = hconfigIterBegin
  do while (ESMF_HConfigIterLoop(hconfigIter, hconfigIterBegin, hconfigIterEnd, rc=rc))
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
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

  enddo
!EOC

!-------------------------------------------------------------------------------
!BOE
! \subsubsection{Index based random access HConfig sequence parsing}
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
! \subsubsection{Destroy a HConfig object}
!
! When done with {\tt hconfig}, it should be destroyed in the usual manner.
!EOE
!BOC
  call ESMF_HConfigDestroy(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------------
!BOE
! \subsubsection{Create a HConfig object directly loading from YAML string}
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
! analogous to the {\em sequence} case demonstrated earlier. Again the begin
! and end iterator variables are defined.
!EOE
!BOC
  ! type(ESMF_HConfigIter) :: hconfigIterBegin, hconfigIterEnd
  hconfigIterBegin = ESMF_HConfigIterBegin(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  hconfigIterEnd = ESMF_HConfigIterEnd(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Then iterate over the elements in {\tt hconfig} using an iterator loop
! variable as before.
!
! The difference of the code below, compared to the {\em sequence} case, is
! that all the {\tt As} access methods here are either of the form
! {\tt As*MapKey} or {\tt As*MapVal}. This is necessary to selectively access
! the {\em map key} or {\em map value}, respectively.
!EOE
!BOC
  ! type(ESMF_HConfigIter) :: hconfigIter
  hconfigIter = hconfigIterBegin
  do while (ESMF_HConfigIterLoop(hconfigIter, hconfigIterBegin, hconfigIterEnd, rc=rc))
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
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
! The {\tt ESMF\_Config} class can be queried for a HConfig object. This allows
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
! followed by calling {\tt ESMF\_HConfigFileLoad()}.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfig
  hconfig = ESMF_HConfigCreate(rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_HConfigFileLoad(hconfig, filename="example.yaml", rc=rc)
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
! A HConfig object can be saved to a YAML file by calling the
! {\tt ESMF\_HConfigFileSave()} method. To demonstrate this, a YAML file
! containing:
! \begin{verbatim}
! # An example of YAML configuration file
!
! simple_list: [1, 2, 3, abc, b, TRUE]
! simple_map:
!   car: red
!   [bike, {p1: 10, p2: 20}]: [bmx, mountain, street]
!   plane: [TRUE, FALSE]
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
! {\tt ESMF\_HConfigFileSave()} method.
!EOE
!BOC
  call ESMF_HConfigFileSave(hconfig, filename="saveMe.yml", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Notice that the resulting contents of file {\tt saveMe.yml} does {\em not}
! contain the comments of the original file. The YAML structure is saved.
! \begin{verbatim}
! simple_list: [1, 2, 3, abc, b, TRUE]
! simple_map:
!   car: red
!   [bike, {p1: 10, p2: 20}]: [bmx, mountain, street]
!   plane: [TRUE, FALSE]
! \end{verbatim}
!
! The object specified in {\tt ESMF\_HConfigFileSave()} can be a regular node
! (of any type) or a {\em sequence} iterator. In either case the file written
! represents the YAML hierarchy with the specified object as the root node.
!
! In the case of a {\em map} iterator, it is necessary to first create an
! appropriate root node utilizing the appropriate {\tt CreateAt} method. This
! allows saving either the {\em map key} or {\em map value} node at the
! current iterator. This is demonstrated below.
!
! In the current example, where {\tt hconfig} is a map with two elements, a
! map iterator can be set to the beginning using the following call.
!EOE
!BOC
  ! type(ESMF_HConfigIter) :: hconfigIter
  hconfigIter = ESMF_HConfigIterBegin(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Here {\tt hconfigIter} cannot be saved to file directly. To write the
! {\em key} node, first create a HConfig object for it using method
! {\tt ESMF\_HConfigCreateAtMapKey()}.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfigTemp
  hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfigIter, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Then save it.
!EOE
!BOC
  call ESMF_HConfigFileSave(hconfigTemp, filename="mapKeyBegin.yaml", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! And finally destroy {\tt hconfigTemp} again.
!EOE
!BOC
  call ESMF_HConfigDestroy(hconfigTemp, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Similarly, to write the {\em value} node to file, first create a HConfig
! object for it using method {\tt ESMF\_HConfigCreateAtMapVal()}.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfigTemp
  hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfigIter, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Then save it.
!EOE
!BOC
  call ESMF_HConfigFileSave(hconfigTemp, filename="mapValBegin.yaml", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! And destroy it.
!EOE
!BOC
  call ESMF_HConfigDestroy(hconfigTemp, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Since {\tt hconfig} is a {\em map} node, it is also possible to directly
! create a {\em value} node by calling {\tt ESMF\_HConfigCreateAt()}
! on it, using the desired {\em key}.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfigTemp
  hconfigTemp = ESMF_HConfigCreateAt(hconfig, keyString="simple_map", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Now {\tt hconfigTemp} points to the {\em value} node, that is
! associated with the "simple\_map" key, which is in turn a map:
! \begin{verbatim}
! car: red
! [bike, {p1: 10, p2: 20}]: [bmx, mountain, street]
! plane: [TRUE, FALSE]
! \end{verbatim}
! It can be saved to file as usual.
!EOE
!BOC
  call ESMF_HConfigFileSave(hconfigTemp, filename="mapValAtKey.yaml", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Any of the {\em value} nodes of {\tt hconfigTemp} can be accessed through
! recursive usage of the {\tt ESMF\_HConfigCreateAt()} method.
! For example, the following call accesses the {\em value} node that is
! associated with {\tt keyString="[bike, {p1: 10, p2: 20}]"}. Here the
! {\tt keyString} is interpreted as YAML syntax, for which an internal HConfig
! representation is created, and finally the map held by {\tt hconfigTemp} is
! searched for a matching key.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfigTemp2
  hconfigTemp2 = ESMF_HConfigCreateAt(hconfigTemp, &
    keyString="[bike, {p1: 10, p2: 20}]", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Now {\tt hconfigTemp2} points to the {\em sequence node} with contents
! {\tt [bmx, mountain, street]}. It, too, can be saved to file.
!EOE
!BOC
  call ESMF_HConfigFileSave(hconfigTemp2, filename="mapValRecursive.yaml", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Finally {\tt hconfigTemp2}, {\tt hconfigTemp} and {\tt hconfig} should be destroyed.
!EOE
!BOC
  call ESMF_HConfigDestroy(hconfigTemp2, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_HConfigDestroy(hconfigTemp, rc=rc)
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
! the YAML standard. The combination of a set of defined tags and a mechanism
! to resolve non-specific tags is called a schema under YAML. The HConfig class
! implements the YAML Core schema, which is an extension of the JSON schema.
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
! NULL. The Core schema tag for this situation is
! {\tt{\bf tag:yaml.org,2002:null}}.
!
! Next, file {\tt exampleWithTags.yaml} is loaded.
!BOC
  call ESMF_HConfigFileLoad(hconfig, filename="exampleWithTags.yaml", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_HConfigLog(hconfig, prefix="WithTagsStart: ", rc=rc)
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
! value_thirteen:         NO
! value_fourteen:         "NO"
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
! The resolved Core schema tag is again {\tt{\bf tag:yaml.org,2002:null}}. There
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
! results in the Core schema tag of {\tt{\bf tag:yaml.org,2002:map}}.
!
! \paragraph{Sequence}
! The value associated with {\em map key} "value\_two" in the current
! {\tt hconfig} object is a sequence. The tag for this node can be obtained
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
! The resolved Core schema tag for a sequence is {\tt{\bf tag:yaml.org,2002:seq}}.
!
! \paragraph{String}
! All of the {\em keys} of the currently loaded {\tt hconfig} object are
! strings. To obtain the tag that is associated with the first key node,
! an iterator is used to access the map nodes individually.
!EOE
!BOC
  ! type(ESMF_HConfigIter) :: hconfigIter
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
! Here the Core schema tag resolves to {\tt{\bf tag:yaml.org,2002:str}}.
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
! The Core schema tag resolves to {\tt{\bf tag:yaml.org,2002:int}}.
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
! The Core schema tag resolves to {\tt{\bf tag:yaml.org,2002:int}}.
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
! The Core schema tag resolves to {\tt{\bf tag:yaml.org,2002:float}}.
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
! The Core schema tag resolves to {\tt{\bf tag:yaml.org,2002:bool}}. The
! supported boolean values are:
! \begin{verbatim}
!      true   |   false
!      True   |   False
!      TRUE   |   FALSE
! \end{verbatim}
!
! \paragraph{Additional Boolean values and the "Norway problem"}
! The YAMLCPP backend used by {\tt ESMF\_HConfig} interprets all of the values
! recognized as such under  \htmladdnormallink{YAML 1.1}
! {https://yaml.org/type/bool.html} as boolean. This extends the above list with
! additional options:
! \begin{verbatim}
!      yes    |   no
!      Yes    |   No
!      YES    |   NO
!      y      |   n
!      Y      |   N
!      on     |   off
!      On     |   Off
!      ON     |   OFF
! \end{verbatim}
! The interpretation of value {\tt NO} as a boolean, instead of a literal
! string, can be problematic. It leads to the so-called {\em "Norway problem"},
! because the same string is often used as country code instead. The underlying
! problem is the misinterpretation of values by YAML.
!EOE
  tag = ESMF_HConfigGetTag(hconfig, keyString="value_thirteen", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("value_thirteen HConfig tag:    ", A30)') tag
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Strictly speaking this is not a YAML problem, but instead a schema specific
! issue. Fortunately there are two simple solutions to ensure the correct and
! intended interpretation of values by {\tt ESMF\_HConfig}:
!
! \begin{enumerate}
! \item Explicit quotation of strings: See for instance the {\em map value}
! for {\tt value\_fourteen} in the current example. Using explicit quotes for
! {\tt "NO"}, the entry is safely interpreted as a literal string, and if
! queried for its tag, will return {\tt tag:yaml.org,2002:str}.
!
! \item Explicit standard tags: This option allows explicit specificaiton of any
! tag, e.g. the standard short-hand tag {\tt !str} for literal strings. This
! approach is discussed in more detal below.
! \end{enumerate}
!EOE
  tag = ESMF_HConfigGetTag(hconfig, keyString="value_fourteen", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("value_fourteen HConfig tag:    ", A30)') tag
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigAdd(hconfig, addKeyString="value_added",content="'NO'",rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  tag = ESMF_HConfigGetTag(hconfig, keyString="value_added", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("value_added HConfig tag:    ", A30)') tag
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigLog(hconfig, prefix="WithTagsFinish: ", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
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
! standard. These are tags that are not known by the Core schema. If such a
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
! \subsubsection{Comparing HConfig objects}
!
! The HConfig class follows the standard behavior of ESMF deep classes as
! described in section \ref{assignment_equality_copy_compare}. To demonstrate
! the operations of assignment, equality, and comparison based on content, we
! start by creating a simple HConfig object.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfig
  hconfig = ESMF_HConfigCreate(content="{car: red, bike: 22, plane: TRUE}", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! A simple assignment results in an alias to the same deep HConfig object.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfigTemp
  hconfigTemp = hconfig
!EOC
!BOE
! The equality {\tt (==)} and inequality {\tt (/=)} operators are
! overloaded to check for the alias condition when used between two
! HConfig objects.
!EOE
!BOC
  ! logical :: isAlias
  isAlias = (hconfigTemp == hconfig)
!EOC
  write (msgString, '("isAlias: ", l2)') isAlias
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! logical :: isNotAlias
  isNotAlias = (hconfigTemp /= hconfig)
!EOC
  write (msgString, '("isNotAlias: ", l2)') isNotAlias
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Alias equality can also be tested by using the {\tt ESMF\_HConfigMatch()}
! function. The return value of this function is of type
! {\tt ESMF\_HConfigMatch\_Flag}, which allows for a wider range of possible
! comparison results. See section~\ref{const:hconfigmatch} for all the
! implemented return values.
!EOE
!BOC
  ! type(ESMF_HConfigMatch_Flag)  :: match
  match = ESMF_HConfigMatch(hconfig, hconfigTemp, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! For the case of an alias match, the value of {\tt ESMF\_HCONFIGMATCH\_ALIAS}
! is returned.
!EOE
!BOC
  isAlias = (match == ESMF_HCONFIGMATCH_ALIAS)
!EOC
  write (msgString, '("isAlias from match: ", l2)') isAlias
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! To demonstrate content matching for HConfig objects that are not aliases, we
! create a separate object with the same content as {\tt hconfig}.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfigTemp
  hconfigTemp = ESMF_HConfigCreate(content="{car: red, bike: 22, plane: TRUE}", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The simple alias check now returns {\tt .false.}.
!EOE
!BOC
  ! logical :: isAlias
  isAlias = (hconfigTemp == hconfig)
!EOC
  write (msgString, '("isAlias for diff objects: ", l2)') isAlias
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! However, for two separate HConfig objects that have exactly matching
! content, as is the case for {\tt hconfig} and {\tt hconfigTemp}, function
! {\tt ESMF\_HConfigMatch()} returns value {\tt ESMF\_HCONFIGMATCH\_EXACT}.
!EOE
!BOC
  ! type(ESMF_HConfigMatch_Flag)  :: match
  match = ESMF_HConfigMatch(hconfig, hconfigTemp, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! logical :: isExact
  isExact = (match == ESMF_HCONFIGMATCH_EXACT)
!EOC
  write (msgString, '("isExact: ", l2)') isExact
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The values returned by {\tt ESMF\_HConfigMatch()} are constructed in
! a monotonically increasing manner to simplify general comparisons that
! ensure two objects are either aliases of each other {\em or} their content
! matches exactly. This common case is demonstrated in the following code that
! sets {\tt isMatch} to {\tt .true.} for the alias or exact match condition,
! and {\tt .false.} otherwise, using {\tt (>=)} logic.
!EOE
!BOC
  ! logical :: isMatch
  isMatch = (match >= ESMF_HCONFIGMATCH_EXACT)
!EOC
  write (msgString, '("isMatch: ", l2)') isMatch
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! While there is an exact match of the content of {\tt hconfig} and
! {\tt hconfigTemp}, they are distinct objects in memory, which can be modified
! independent of each other. E.g. another key-value pair can be added to
! {\tt hconfigTemp} without affecting the content of {\tt hconfig}.
!EOE
!BOC
  call ESMF_HConfigAdd(hconfigTemp, addKeyString="kNew", content=7, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Now that the content of {\tt hconfig} and {\tt hconfigTemp} differs, function
! {\tt ESMF\_HConfigMatch()} returns value {\tt ESMF\_HCONFIGMATCH\_NONE}.
!EOE
!BOC
  ! type(ESMF_HConfigMatch_Flag)  :: match
  match = ESMF_HConfigMatch(hconfig, hconfigTemp, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! logical :: isNone
  isNone = (match == ESMF_HCONFIGMATCH_NONE)
!EOC
  write (msgString, '("isNone: ", l2)') isNone
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Finally clean up both HConfig objects.
!EOE
!BOC
  ! Destroy hconfig when done with it.
  call ESMF_HConfigDestroy(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! Destroy hconfigTemp when done with it.
  call ESMF_HConfigDestroy(hconfigTemp, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------------
!BOE
! \subsubsection{Adding, Setting, and Removing elements from HConfig object}
!
! After creating a HConfig object without specifying {\tt content} or
! {\tt filename}, it is empty.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfig
  hconfig = ESMF_HConfigCreate(rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigFileSave(hconfig, filename="build_and_edit_00.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Now the {\tt ESMF\_HConfigAdd()} method can be used to add new elements to
! an existing HConfig object. The {\tt Add()} interfaces are heavily overloaded,
! each specific entry point featuring a number of optional arguments. The two
! fundamentally different ways of using {\tt Add()} are: (1) adding an element
! at the end of a {\em sequence} or (2) adding an element to a {\em map}.
! Here, where {\tt hconfig} is empty, either option is possible. The way the
! first element is added determines whether {\tt hconfig} is a sequence or
! a map.
!
! The following call adds an element to {\tt hconfig} without specifying the
! {\tt addKey} or {\tt addKeyString} argument. This indicates that a sequence
! element is added to the end, and as a consequence rendering {\tt hconfig}
! a {\em sequence}.
!EOE
!BOC
  call ESMF_HConfigAdd(hconfig, "first added item", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigFileSave(hconfig, filename="build_and_edit_01.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Additional elements can be added at the end of {\tt hconfig}.
!EOE
!BOC
  call ESMF_HConfigAdd(hconfig, 12.57_ESMF_KIND_R8, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigFileSave(hconfig, filename="build_and_edit_02.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! At this point, the content of {\tt hconfig} is a sequence with two elements.
! \begin{verbatim}
! - first added item
! - 12.5700000000
! \end{verbatim}
!
! It is also possible to add an entire HConfig structure as an item to the
! existing sequence. One way to do this is to use standar YAML syntax when
! adding the element. Here a {\em map} is added to the end of {\tt hconfig}.
!EOE
!BOC
  call ESMF_HConfigAdd(hconfig, "{k1: 7, k2: 25}", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigFileSave(hconfig, filename="build_and_edit_03.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! This results in the following content, where the third element of the sequence
! is the map that was just added.
! \begin{verbatim}
! - first added item
! - 12.5700000000
! - {k1: 7, k2: 25}
! \end{verbatim}
!
! A HConfig structure can even be loaded from file and added to the end of 
! {\tt hconfig}. This requires a temporary HConfig object.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfigTemp
  hconfigTemp = ESMF_HConfigCreate(filename="example.yaml", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_HConfigAdd(hconfig, hconfigTemp, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_HConfigDestroy(hconfigTemp, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigFileSave(hconfig, filename="build_and_edit_04.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The result is the following content for {\tt hconfig}.
! \begin{verbatim}
! - first added item
! - 12.5700000000
! - {k1: 7, k2: 25}
! - simple_list: [1, 2, 3, abc, b, TRUE]
!   simple_map:
!     car: red
!     [bike, {p1: 10, p2: 20}]: [bmx, mountain, street]
!     plane: [TRUE, FALSE]
! \end{verbatim}
!
! Using the {\tt CreateAt()} method, it is easy to gain access to any specific
! element in {\tt hconfig}. Since {\tt hconfig} is a {\em sequence}, the proper
! access is by {\em index}.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfigTemp
  hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=3, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigFileSave(hconfigTemp, filename="build_and_edit_05.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! This creates a temporary HConfig object that {\em references} the 3rd element
! of the sequence stored by {\tt hconfig}. If {\tt hconfigTemp} were to be
! saved to file, it would have the following content.
! \begin{verbatim}
! {k1: 7, k2: 25}
! \end{verbatim}
!
! Using the {\tt Set()} methods, contents in {\tt hconfigTemp}, and thus in
! the 3rd element of {\tt hconfig} can be modified. The content of
! {\tt hconfigTemp} is a {\em map}, and the proper access is by {\em map key}.
! Here key "k2" is being modified.
!EOE
!BOC
  call ESMF_HConfigSet(hconfigTemp, 12.5, keyString="k2", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The {\tt hconfigTemp} is a reference to a {\em map}, and new elements can be
! added using the {\tt addKeyString} argument.
!EOE
!BOC
  call ESMF_HConfigAdd(hconfigTemp, .true., addKeyString="k3", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_HConfigDestroy(hconfigTemp, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_HConfigFileSave(hconfig, filename="build_and_edit_06.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! After these operations, the content of {\tt hconfig} has changed to
! \begin{verbatim}
! - first added item
! - 12.5700000000
! - {k1: 7, k2: 12.5000000000, k3: True}
! - simple_list: [1, 2, 3, abc, b, TRUE]
!   simple_map:
!     car: red
!     [bike, {p1: 10, p2: 20}]: [bmx, mountain, street]
!     plane: [TRUE, FALSE]
! \end{verbatim}
! Notice that while {\tt hconfigTemp} should be destroyed explicitly, as in the
! example above, doing so does {\em not} affect the referenced node inside the
! {\tt hconfig} object. In other words, {\tt hconfigTemp} was a reference, and
! {\em not} a deep copy of the node! There is some allocated memory associated
! with the {\tt hconfigTemp} reference that gets cleaned up with the
! {\tt Destroy()} call, but it does not affect the reference itself.
!
! The {\tt Set()} method can also be used to edit the element referenced
! itself. Here the 4th element in the {\tt hconfig} sequence is set to be a
! simple scalar string value using this approach.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfigTemp
  hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=4, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_HConfigSet(hconfigTemp, "Simple scalar string value", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_HConfigDestroy(hconfigTemp, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigFileSave(hconfig, filename="build_and_edit_07.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The content of {\tt hconfig} has been updated as below.
! \begin{verbatim}
! - first added item
! - 12.5700000000
! - {k1: 7, k2: 12.5000000000, k3: True}
! - Simple scalar string value
! \end{verbatim}
!
! There is a simpler alternative for {\em direct} element editing in an
! HConfig object via the {\tt Set()} method. Using the {\tt index} or
! {\tt keyString} argument, a sequence or map element, respectively, can
! be edited directly. For instance,
!EOE
!BOC
  call ESMF_HConfigSet(hconfig, "[a, b, c]", index=4, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigFileSave(hconfig, filename="build_and_edit_08.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! sets the 4th element of {\tt hconfig} directly, without the need of a
! temporary HConfig variable. This updates the content to:
! \begin{verbatim}
! - first added item
! - 12.5700000000
! - {k1: 7, k2: 12.5000000000, k3: True}
! - [a, b, c]
! \end{verbatim}
!
! Elements can be deleted from a HConfig object holding a sequence or map
! using the {\tt Remove()} method, specifying the {\em index} or
! {\em map key}, respectively. Here the 2nd element of the sequence held by
! {\tt hconfig} is removed.
!EOE
!BOC
  call ESMF_HConfigRemove(hconfig, index=2, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigFileSave(hconfig, filename="build_and_edit_09.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The result is a sequence with only three remaining elements.
! \begin{verbatim}
! - first added item
! - {k1: 7, k2: 12.5000000000, k3: True}
! - [a, b, c]
! \end{verbatim}
!
! To demonstrate removal of an element from a {\em map}, the second
! {\tt hconfig} element is referenced by a temporary HConfig object. The element
! with key "k2" is then removed using the respective {\tt Remove()} method.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfigTemp
  hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=2, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_HConfigRemove(hconfigTemp, keyString="k2", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_HConfigDestroy(hconfigTemp, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigFileSave(hconfig, filename="build_and_edit_10.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The resulting {\tt hconfig} content is as expected.
! \begin{verbatim}
! - first added item
! - {k1: 7, k3: True}
! - [a, b, c]
! \end{verbatim}
!
! Finally the entire contents of {\tt hconfig} can be deleted by setting the
! node itself to one of the special NULL values.
!EOE
!BOC
  call ESMF_HConfigSet(hconfig, "NULL", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigFileSave(hconfig, filename="build_and_edit_11.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! If saved to file, the contents of {\tt hconfig} shows up as a simple tilde
! character, indicating its NULL value.
! \begin{verbatim}
! ~
! \end{verbatim}
!
! At this point {\tt hconfig} is neither a {\em sequence} nor a {\em map}. It is
! NULL. Adding a map element, i.e. an element with a {\em key}, turns
! {\tt hconfig} into a map.
!EOE
!BOC
  call ESMF_HConfigAdd(hconfig, "first added item", addKeyString="item1", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigFileSave(hconfig, filename="build_and_edit_12.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The contents of {\tt hconfig} now is a map with a single entry:
! character, indicating its NULL value.
! \begin{verbatim}
! item1: first added item
! \end{verbatim}
!
! As in other contexts before, the content as well as the specified
! {\tt addKeyString} can be of any legal YAML syntax. This is demonstrated
! in the following {\tt Add()} calls.
!EOE
!BOC
  ! Add YAML sequence content with simple scalar key.
  call ESMF_HConfigAdd(hconfig, "[2, added, item]", addKeyString="item2", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! Add simple scalar content with a YAML map as key.
  call ESMF_HConfigAdd(hconfig, "third added item", addKeyString="{item: 3}", &
    rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! Add complex YAML content with YAML sequence as key.
  call ESMF_HConfigAdd(hconfig, "{4th: item, 5th: [true, false]}", &
    addKeyString="[1, 2, 3, 4]", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_HConfigFileSave(hconfig, filename="build_and_edit_13.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Resulting in the final contents of {\tt hconfig}:
! \begin{verbatim}
! item1: first added item
! item2: [2, added, item]
! {item: 3}: third added item
! [1, 2, 3, 4]: {4th: item, 5th: [true, false]}
! \end{verbatim}
! Finally clean up {\tt hconfig}.
!EOE
!BOC
  ! Destroy hconfig when done with it.
  call ESMF_HConfigDestroy(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------------
!BOE
! \subsubsection{Working with multiple YAML documents}
!
! The YAML standard supports multiple documents in a single file by separating
! each document with a line containing three dashes (-{}-{}-). Optionally the
! end of each document may be indicated by three periods (...). For example,
! the following YAML file contains three documents (notice the optional usage
! of the document end marker):
! \begin{verbatim}
! ---
! - This
! - is
! - the
! - first document.
! ...
! ---
! - And
! - a second document.
! ---
! - And
! - finally a
! - third document.
! \end{verbatim}
! All of the documents contained in a YAML file can be loaded into a single
! HConfig object all at once.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfig
  hconfig = ESMF_HConfigCreate(filename="multiDoc.yaml", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The number of documents held by {\tt hconfig} can be queried.
!EOE
!BOC
  ! integer :: docCount
  docCount = ESMF_HConfigGetDocCount(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("docCount: ", i8)') docCount
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! When saving {\tt hconfig}, a multi-document YAML file will be written.
!EOE
!BOC
  call ESMF_HConfigFileSave(hconfig, filename="multi_00.yaml", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The {\tt ESMF\_HConfigFileSave()} method implements strict usage of both
! document markers when saving a multi-document HConfig object.
! \begin{verbatim}
! ---
! - This
! - is
! - the
! - first document.
! ...
! ---
! - And
! - a second document.
! ...
! ---
! - And
! - finally a
! - third document.
! ...
! \end{verbatim}
!
! The content of the {\tt hconfig} object can be written to the ESMF log file
! as usual.
!EOE
!BOC
  call ESMF_HConfigLog(hconfig, prefix="my-multi-doc: ", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The optional {\tt doc} argument can be specified to save or log a specific
! document of the multi-document {\tt hconfig} object.
!EOE
!BOC
  call ESMF_HConfigFileSave(hconfig, filename="multi_01.yaml", doc=2, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! This operation results in a single document file:
! \begin{verbatim}
! - And
! - a second document.
! \end{verbatim}
! The {\tt ESMF\_HConfigFileLoad()} method also accepts the optional {\tt doc}
! argument. When specified, the result is a single-document {\tt hconfig}
! object, holding the content of the indicated document within the loaded file.
!EOE
!BOC
  call ESMF_HConfigFileLoad(hconfig, filename="multiDoc.yaml", doc=3, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Saving {\tt hconfig} to file shows the expected situation.
!EOE
!BOC
  call ESMF_HConfigFileSave(hconfig, filename="multi_02.yaml", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Resulting in:
! \begin{verbatim}
! - And
! - finally a
! - third document.
! \end{verbatim}
!
! Most HConfig methods provide the optional {\tt doc} argument. If present,
! the method applies to the specified document. The default for when the
! {\tt doc} argument is not present, for most methods is to use the first
! document in the object. The exceptions to this rule are the
! {\tt ESMF\_HConfigFileSave()} and {\tt ESMF\_HConfigFileLoad()} methods.
! Here the default is to apply the operation to {\em all} documents.
!
! When done, clean up {\tt hconfig} as usual.
!EOE
!BOC
  ! Destroy hconfig when done with it.
  call ESMF_HConfigDestroy(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------------
!BOE
! \subsubsection{Sequence shortcuts for: Create, As, Add, and Set}
!
! The HConfig class offers shortcut methods for the sake of convenience when
! working with sequences where all elements are of the same typekind. In these
! cases a sequence can be represented as a one-dimensional Fortran array. The
! interfaces are overloaded for one-dimensional string, logical, I4, I8, R4,
! and R8 typekinds.
!
! Using a Fortran array constructor for the actual argument, a sequence of I4
! data is created.
!EOE
!BOC
  ! type(ESMF_HConfig) :: hconfig
  hconfig = ESMF_HConfigCreate([1,2,3], rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_HConfigFileSave(hconfig, filename="shortcut_00.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The content of {\tt hconfig} can be accessed in the usual manner, via
! iterators or indexed access. Alternatively, the sequence of I4 elements
! can be retrieved in a single call using a one-dimensional allocatable
! Fortran array of the appropriate typekind.
!EOE
!BOC
  ! integer(ESMF_KIND_I4), allocatable :: valueI4Seq(:)
  valueI4Seq = ESMF_HConfigAsI4Seq(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("valueI4Seq: ", 3i8)') valueI4Seq
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The optional, intent(out) argument {\tt asOkay} is available as in the scalar
! access methods. If specified, errors triggered by unsupported typekind
! conversion exceptions are suppressed, and instead {\tt asOkay == .false.} is
! returned by the call.
!
! Here an attempt is made to access the content of {\tt hconfig} as a sequence
! of logicals. This is not supported, and will be flagged in the return value
! of {\tt asOkay}.
!EOE
!BOC
  ! logical, allocatable :: valueLSeq(:)
  valueLSeq = ESMF_HConfigAsLogicalSeq(hconfig, asOkay=asOkay, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("asOkay for Logical: ", l2)') asOkay
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Finally the content of {\tt hconfig} is accessed as a sequence of strings.
! This is always supported since every typekind can be represented in string
! form.
!EOE
!BOC
  ! character(len=:), allocatable :: valueSSeq(:)
  valueSSeq = ESMF_HConfigAsStringSeq(hconfig, stringLen=10, asOkay=asOkay, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("asOkay for String: ", l2)') asOkay
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msgString, '("valueSSeq: ", 3A10)') valueSSeq
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Next {\tt hconfig} is cleaned up before re-creating it as an empty HConfig
! object.
!EOE
!BOC
  ! Clean up hconfig.
  call ESMF_HConfigDestroy(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! type(ESMF_HConfig) :: hconfig
  hconfig = ESMF_HConfigCreate(rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Sequences can be added to {\tt hconfig} conveniently using the overloaded
! {\tt Add()} interfaces that accept one-dimensional Fortran arrays. Here a
! sequence of strings is added as the value of a map entry with key string "k1".
!EOE
!BOC
  call ESMF_HConfigAdd(hconfig, ["aaa","bbb","ccc"], addKeyString="k1", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_HConfigFileSave(hconfig, filename="shortcut_01.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Next a sequence of R4 values is added to the map held by {\tt hconfig},
! under key string "k2".
!EOE
!BOC
  call ESMF_HConfigAdd(hconfig, [1.0,1.25,1.5], addKeyString="k2", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_HConfigFileSave(hconfig, filename="shortcut_02.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! At this point {\tt hconfig} contains the following information:
! \begin{verbatim}
! k1:
!   - aaa
!   - bbb
!   - ccc
! k2:
!   - 1
!   - 1.25
!   - 1.5
! \end{verbatim}
!
! The {\tt Set()} interfaces are also overloaded to accept one-dimensional
! Fortran arrays as input. This makes it easy to set any node to a sequence
! that is available as Fortran array. Here the value associated with key "k1"
! is changed to a list of two logicals.
!EOE
!BOC
  call ESMF_HConfigSet(hconfig, [.true.,.false.], keyString="k1", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_HConfigFileSave(hconfig, filename="shortcut_03.yaml", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! This changes the content of {\tt hconfig} as expected.
! \begin{verbatim}
! k1:
!   - True
!   - False
! k2:
!   - 1
!   - 1.25
!   - 1.5
! \end{verbatim}
!
! Finally clean up {\tt hconfig} as usual.
!EOE
!BOC
  ! Destroy hconfig when done with it.
  call ESMF_HConfigDestroy(hconfig, rc=rc)
!EOC
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
