! $Id: NUOPC_RunSequenceDef.F90,v 1.13 2012/11/16 23:33:09 theurich Exp $

#define FILENAME "src/addon/NUOPC/NUOPC_RunSequenceDef.F90"

module NUOPC_RunSequenceDef

  use ESMF
  use NUOPC_Base

  implicit none
  
  private
  
  public NUOPC_RunElement, NUOPC_RunSequence
  public NUOPC_RunElementAdd, NUOPC_RunSequenceAdd
  public NUOPC_RunElementPrint, NUOPC_RunSequencePrint
  public NUOPC_RunSequenceSet
  public NUOPC_RunSequenceDeallocate
  public NUOPC_RunSequenceIterate
  
!==============================================================================
! 
! DERIVED TYPES
!
!==============================================================================
  
  type NUOPC_RunElement
    integer :: i  ! i >= 0 -> model comp. index, or src model index if connector
                  ! i <  0 -> link or enddo element (depend on runSeq)
    integer :: j  ! j >= 0 -> connector component: i->j
                  ! j <  0 -> model component: i
    integer :: phase  ! run phase
    type(NUOPC_RunSequence), pointer:: runSeq ! point back to RunSequence
    type(NUOPC_RunElement), pointer :: next   ! next RunElement in linked list
  end type

  type NUOPC_RunSequence
    type(ESMF_Clock)                :: clock  ! time loop information
    type(NUOPC_RunElement), pointer :: first  ! first element of sequence
    type(NUOPC_RunElement), pointer :: stack  ! run-time stack element pointer
  end type
  
!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

  interface NUOPC_RunSequencePrint
    module procedure NUOPC_RunSequenceSinglePrint
    module procedure NUOPC_RunSequenceArrayPrint
  end interface
  
  interface NUOPC_RunSequenceDeallocate
    module procedure NUOPC_RunSequenceSingleDeall
    module procedure NUOPC_RunSequenceArrayDeall
  end interface

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_RunElementAdd - Add a RunElement to the end of a RunSequence
! !INTERFACE:
  subroutine NUOPC_RunElementAdd(runSeq, i, j, phase, rc)
! !ARGUMENTS:
    type(NUOPC_RunSequence), intent(inout), target :: runSeq
    integer,                 intent(in)            :: i, j, phase
    integer, optional,       intent(out)           :: rc
! !DESCRIPTION:
!   Add a new RunElement at the end of a RunSequence. The RunElement is set to
!   the values provided for {\tt i}, {\tt j}, {\tt phase}.
!EOP
  !-----------------------------------------------------------------------------
    integer                                :: stat
    type(NUOPC_RunElement), pointer        :: runElement
    type(NUOPC_RunElement), pointer        :: searchElement, prevElement
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! sanity check
    if (.not.associated(runSeq%first)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="first must be associated",&
        line=__LINE__, file=FILENAME, rcToReturn=rc)
      return  ! bail out
    endif
    
    ! allocate a new run element
    allocate(runElement, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of RunElement in NUOPC_RunElementAdd.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    ! initialize the new run element
    runElement%i = i
    runElement%j = j
    runElement%phase = phase
    runElement%runSeq => runSeq   ! associate for back reference
    ! append the new run element to the run sequence (but before ENDDO)
    if (.not.associated(runSeq%first%next)) then
      runElement%next => runSeq%first
      runSeq%first => runElement
    else
      searchElement => runSeq%first
      do while (associated(searchElement%next))
        prevElement => searchElement
        searchElement => searchElement%next
      enddo
      runElement%next => searchElement
      prevElement%next => runElement
    endif

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_RunElementPrint - Print info about a RunElement object
! !INTERFACE:
  subroutine NUOPC_RunElementPrint(runElement, rc)
! !ARGUMENTS:
    type(NUOPC_RunElement),  intent(in)  :: runElement
    integer, optional,       intent(out) :: rc
! !DESCRIPTION:
!   Write information about {\tt runElement} into the default log file.
!EOP
  !-----------------------------------------------------------------------------
    character(ESMF_MAXSTR)    :: msgString
    
    if (present(rc)) rc = ESMF_SUCCESS

    write (msgString, *) "runElementPrint: ", &
      runElement%i, runElement%j, runElement%phase
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_RunSequenceAdd - Add more RunSequences to a RunSequence vector
! !INTERFACE:
  subroutine NUOPC_RunSequenceAdd(runSeq, addCount, rc)
! !ARGUMENTS:
    type(NUOPC_RunSequence), pointer     :: runSeq(:)
    integer,                 intent(in)  :: addCount
    integer, optional,       intent(out) :: rc
! !DESCRIPTION:
!   The incoming RunSequence vector {\tt runSeq} is extended by {\tt addCount}
!   more RunSequence objects. The existing RunSequence objects are copied to the
!   front of the new vector before the old vector is deallocated.
!EOP
  !-----------------------------------------------------------------------------
    integer :: i, sizeIn, stat
    type(NUOPC_RunSequence), pointer :: runSeqNew(:)
    type(NUOPC_RunElement),  pointer :: runElement
    
    if (present(rc)) rc = ESMF_SUCCESS

    ! size of incoming runSeq
    if (associated(runSeq)) then
      sizeIn = size(runSeq)
    else
      sizeIn = 0
    endif
    
    ! allocate the new run sequence
    allocate(runSeqNew(sizeIn+addCount))
    
    ! copy the contents of runSeq over to new one
    if (associated(runSeq)) then
      do i=1, sizeIn
        runSeqNew(i) = runSeq(i)
      enddo
    endif
    
    ! initialize the newly added elements
    do i=sizeIn+1, sizeIn+addCount
      ! allocate a new run element
      allocate(runElement, stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of RunElement in NUOPC_RunElementAdd.", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      ! initialize the new run element to be "ENDDO"
      runElement%i = -i
      runElement%j = -1
      runElement%phase = 0
      nullify(runElement%next)  ! terminal element
      ! hook up runElement to runSeqNew
      runSeqNew(i)%first => runElement
      ! initialize stack member
      nullify(runSeqNew(i)%stack)
    enddo
    
    ! deallocate the incoming runSeq
    if (associated(runSeq)) then
      deallocate(runSeq)
    endif
    
    ! point to the newly allocated runSeqNew
    runSeq => runSeqNew
    
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_RunSequenceDeallocate - Deallocate an entire RunSequence vector
! !INTERFACE:
  ! Private name; call using NUOPC_RunSequenceDeallocate()
  subroutine NUOPC_RunSequenceArrayDeall(runSeq, rc)
! !ARGUMENTS:
    type(NUOPC_RunSequence), pointer     :: runSeq(:)
    integer, optional,       intent(out) :: rc
! !DESCRIPTION:
!   Deallocate all of the RunElements in all of the RunSequence defined in the
!   {\tt runSeq} vector.
!EOP
  !-----------------------------------------------------------------------------
    integer :: i
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    if (associated(runSeq)) then
    
      ! deallocate the individual run sequences
      do i=1, size(runSeq)
        call NUOPC_RunSequenceSingleDeall(runSeq(i))
      enddo
    
      deallocate(runSeq)  ! finally deallocate the actual runSeq array
      nullify(runSeq)     ! ensure recognizable condition
      
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_RunSequenceDeallocate - Deallocate a single RunSequence object
! !INTERFACE:
  ! Private name; call using NUOPC_RunSequenceDeallocate()
  subroutine NUOPC_RunSequenceSingleDeall(runSeq, rc)
! !ARGUMENTS:
    type(NUOPC_RunSequence), intent(inout)  :: runSeq
    integer, optional,       intent(out) :: rc
! !DESCRIPTION:
!   Deallocate all of the RunElements in the RunSequence defined by {\tt runSeq}.
!EOP
  !-----------------------------------------------------------------------------
    type(NUOPC_RunElement), pointer :: searchElement

    if (present(rc)) rc = ESMF_SUCCESS
    
    do while (associated(runSeq%first))
      searchElement => runSeq%first
      runSeq%first => searchElement%next
      deallocate(searchElement)
      nullify(searchElement)    ! ensure recognizable condition
    enddo

  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_RunSequenceIterate - Iterate through a RunSequence
! !INTERFACE:
  logical function NUOPC_RunSequenceIterate(runSeq, runSeqIndex, runElement, &
                                            rc)
! !ARGUMENTS:
    type(NUOPC_RunSequence), pointer     :: runSeq(:)
    integer,                 intent(in)  :: runSeqIndex
    type(NUOPC_RunElement),  pointer     :: runElement
    integer, optional,       intent(out) :: rc
! !DESCRIPTION:
!   Iterate through the RunSequence that is in position {\tt runSeqIndex} in the
!   {\tt runSeq} vector. If {\tt runElement} comes in {\em unassociated}, the
!   iteration starts from the beginning. Otherwise this call takes one forward
!   step relative to the incoming {\tt runElement}, returning the next
!   RunElement in {\tt runElement}. In either case, 
!   the logical function return value is {\tt .true.} if the end of iteration
!   has not been reached by the forward step, and {\tt .false.} if the end of 
!   iteration has been
!   reached. The returned {\tt runElement} is only valid for a function return
!   value of {\tt .true.}.
!EOP
  !-----------------------------------------------------------------------------
    type(ESMF_Clock)  :: clock
    logical           :: clockIsStopTime
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    NUOPC_RunSequenceIterate = .false. ! initialize to safe return value
    
    if (.not.associated(runElement)) then
      ! start from the beginning
      if (.not.associated(runSeq)) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="runSeq must be associated",&
          line=__LINE__, file=FILENAME, rcToReturn=rc)
        return  ! bail out
      endif
      if (size(runSeq)<runSeqIndex) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="runSeq must have at "// &
          "least 'runSeqIndex' number of elements", line=__LINE__, &
          file=FILENAME, rcToReturn=rc)
        return  ! bail out
      endif
      if (.not.associated(runSeq(runSeqIndex)%first)) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="run element must "// &
          "be associated", line=__LINE__, file=FILENAME, rcToReturn=rc)
        return  ! bail out
      endif
      ! check the clock
      clock = runSeq(runSeqIndex)%clock
      clockIsStopTime = ESMF_ClockIsStopTime(clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      NUOPC_RunSequenceIterate = .not.clockIsStopTime ! return value
      if (clockIsStopTime) return ! no reason to continue here
      ! finally set runElement
      runElement => runSeq(runSeqIndex)%first
      ! deal with potential "ENDDO" marker
      if (.not.associated(runElement%next)) then
        if (runElement%i == -runSeqIndex) then
          ! first run element happens to be an "ENDDO" marker
          do while (.not. ESMF_ClockIsStopTime(clock, rc=rc))
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=FILENAME)) return  ! bail out
            ! advance to next time step
!print *, "silly time loop"
            call ESMF_ClockAdvance(clock, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          enddo
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) return  ! bail out
        else
          ! invalid element
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="invalid runElement",&
            line=__LINE__, file=FILENAME, rcToReturn=rc)
        endif
        NUOPC_RunSequenceIterate = .false.  ! set safe return value 
        return  ! bail out
      endif
    else
      ! iterate to the next element
      runElement => runElement%next
    endif
    
    ! runElement may be a control element (either LINK or ENDDO)
    NUOPC_RunSequenceIterate = NUOPC_RunSequenceCtrl(runSeq, runElement, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_RunSequencePrint - Print info about a single RunSequence object
! !INTERFACE:
  ! Private name; call using NUOPC_RunSequencePrint()
  subroutine NUOPC_RunSequenceSinglePrint(runSeq, rc)
! !ARGUMENTS:
    type(NUOPC_RunSequence), intent(in)  :: runSeq
    integer, optional,       intent(out) :: rc
! !DESCRIPTION:
!   Write information about {\tt runSeq} into the default log file.
!EOP
  !-----------------------------------------------------------------------------
    type(NUOPC_RunElement), pointer :: searchElement
    character(ESMF_MAXSTR)          :: msgString

    if (present(rc)) rc = ESMF_SUCCESS
    
    if (.not.associated(runSeq%first)) then
      write (msgString, *) "NUOPC_RunSequenceSinglePrint: no runElements"
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    else
      write (msgString, *) "NUOPC_RunSequenceSinglePrint:"
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      searchElement => runSeq%first
      do while (associated(searchElement%next))
        call NUOPC_RunElementPrint(searchElement)
        searchElement => searchElement%next
      enddo
      call NUOPC_RunElementPrint(searchElement)
    endif

  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_RunSequencePrint - Print info about a RunSequence vector
! !INTERFACE:
  ! Private name; call using NUOPC_RunSequencePrint()
  subroutine NUOPC_RunSequenceArrayPrint(runSeq, rc)
! !ARGUMENTS:
    type(NUOPC_RunSequence), pointer     :: runSeq(:)
    integer, optional,       intent(out) :: rc
! !DESCRIPTION:
!   Write information about the whole {\tt runSeq} vector into the default log 
!   file.
!EOP
  !-----------------------------------------------------------------------------
    integer :: i
    character(ESMF_MAXSTR)          :: msgString
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    do i=1, size(runSeq)
      write (msgString, *) "NUOPC_RunSequenceArrayPrint: element", i, &
        " out of ", size(runSeq)
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      call NUOPC_RunSequenceSinglePrint(runSeq(i))
    enddo
    
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_RunSequenceSet - Set values inside a RunSequence object
! !INTERFACE:
  subroutine NUOPC_RunSequenceSet(runSeq, clock, rc)
! !ARGUMENTS:
    type(NUOPC_RunSequence), intent(inout) :: runSeq
    type(ESMF_Clock),        intent(in)    :: clock
    integer, optional,       intent(out)   :: rc
! !DESCRIPTION:
!   Set the Clock member in {\tt runSeq}.
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS
    
    runSeq%clock = clock  ! set the clock (is alias)

  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_RunSequenceCtrl - Recursive iterator through a RunSequence
! !INTERFACE:
  recursive logical function NUOPC_RunSequenceCtrl(runSeq, runElement, rc) &
! !ARGUMENTS:
    result(NUOPC_RunSequenceCtrlResult)
    type(NUOPC_RunSequence), pointer     :: runSeq(:)
    type(NUOPC_RunElement),  pointer     :: runElement
    integer, optional,       intent(out) :: rc
! !DESCRIPTION:
!EOP
  !-----------------------------------------------------------------------------
    type(ESMF_Clock)  :: clock
    logical           :: clockIsStopTime
    integer           :: i
    
    NUOPC_RunSequenceCtrlResult = .false. ! initialize to safe return value

    ! sanity checks
    if (.not.associated(runSeq)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="runSeq must be associated",&
        line=__LINE__, file=FILENAME, rcToReturn=rc)
      return  ! bail out
    endif
    if (.not.associated(runElement)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="runElement must be associated",&
        line=__LINE__, file=FILENAME, rcToReturn=rc)
      return  ! bail out
    endif
    
    ! deal with simple cases
    if (runElement%i >= 0 ) then
      if (associated(runElement%next)) then
        ! simple component element
        NUOPC_RunSequenceCtrlResult = .true.
        return
      else
        ! invalid element
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="invalid runElement",&
          line=__LINE__, file=FILENAME, rcToReturn=rc)
        return
      endif
    endif
    
    ! remaining cases are control elements (runElement%i < 0)
    
    i = -(runElement%i)
    if (i > size(runSeq)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="invalid reference into "//&
        "runSeq", line=__LINE__, file=FILENAME, rcToReturn=rc)
      return
    endif
    clock = runSeq(i)%clock
    
    if (.not.associated(runElement%next)) then
      ! "ENDDO" element
!print *, "found ENDDO element"
      ! advance the clock and check for stop time
      call ESMF_ClockAdvance(clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      clockIsStopTime = ESMF_ClockIsStopTime(clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      NUOPC_RunSequenceCtrlResult = .not.clockIsStopTime ! return value
      if (clockIsStopTime) then
        if (.not.associated(runSeq(i)%stack)) then
          ! reached the end of top level loop
          return  ! break out
        else
          ! reached the end of a nested run sequence loop -> move up the stack
          runElement => runSeq(i)%stack
          nullify(runSeq(i)%stack)  ! for recursive link detection
        endif
      else
        ! start back at the top of sequence
        runElement => runSeq(i)%first  ! first element in next iteration
      endif
    else
      ! "LINK" element
!print *, "found LINK element"
      if (associated(runSeq(i)%stack)) then
        ! detected recursive link
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="recursive link detected",&
          line=__LINE__, file=FILENAME, rcToReturn=rc)
        return
      endif
      ! check and set time keeping conditions between clocks
      call NUOPC_ClockCheckSetClock(setClock=clock, &
        checkClock=runElement%runSeq%clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      ! follow the link
      runSeq(i)%stack => runElement%next  ! set stack pointer for return
      ! start at the top of sequence
      runElement => runSeq(i)%first  ! first element in next iteration
    endif
    
    ! recursive call...
    NUOPC_RunSequenceCtrlResult = NUOPC_RunSequenceCtrl(runSeq, runElement, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
  
  end function
  !-----------------------------------------------------------------------------

end module NUOPC_RunSequenceDef
