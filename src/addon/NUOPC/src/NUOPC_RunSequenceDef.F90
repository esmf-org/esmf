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
#define FILENAME "src/addon/NUOPC/src/NUOPC_RunSequenceDef.F90"
!==============================================================================

module NUOPC_RunSequenceDef

  use ESMF
  use NUOPC_Base

  implicit none
  
  private
  
  public NUOPC_RunElement
  public NUOPC_RunElementAdd, NUOPC_RunElementAddComp, NUOPC_RunElementAddLink
  public NUOPC_RunElementPrint
  public NUOPC_RunSequence
  public NUOPC_RunSequenceAdd, NUOPC_RunSequenceSet, NUOPC_RunSequencePrint
  public NUOPC_RunSequenceDeallocate
  public NUOPC_RunSequenceIterate
  
!==============================================================================
! 
! DERIVED TYPES
!
!==============================================================================
  
  type NUOPC_RunElement
    ! - new style members
!    type(ESMF_GridComp), pointer    :: gcomp  !gjt: not yet used
!    type(ESMF_CplComp), pointer     :: ccomp  !gjt: not yet used
    ! - old style members
    integer :: i  ! i >= 0 -> model comp. index, or src model index if connector
                  ! i <  0 -> link or enddo element (depend on runSeq)
    integer :: j  ! j >= 0 -> connector component: i->j
                  ! j <  0 -> model component: i
    ! - common members
    integer :: phase  ! run phase
    type(NUOPC_RunSequence), pointer:: runSeq ! point back to RunSequence
    type(NUOPC_RunElement), pointer :: next   ! next RunElement in linked list
  end type

  type NUOPC_RunSequence
    type(ESMF_Clock)                :: clock  ! time loop information
    type(NUOPC_RunElement), pointer :: first  ! first element of sequence
    ! - run-time members
    type(NUOPC_RunElement), pointer :: stack  ! run-time stack element pointer
    integer                         :: loopLevel
    integer                         :: loopIteration
    integer                         :: levelMember
    integer                         :: levelChildren
    type(ESMF_Clock)                :: prevMemberClock
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
!BOPI
! !IROUTINE: NUOPC_RunElementAdd - Add a RunElement to the end of a RunSequence
! !INTERFACE:
  subroutine NUOPC_RunElementAdd(runSeq, i, j, phase, rc)
! !ARGUMENTS:
    type(NUOPC_RunSequence), intent(inout), target :: runSeq
    integer,                 intent(in)            :: i, j, phase
    integer, optional,       intent(out)           :: rc
! !DESCRIPTION:
!   Add a new RunElement at the end of an existing RunSequence. The RunElement
!   is set to the values provided for {\tt i}, {\tt j}, {\tt phase}.
!EOPI
  !-----------------------------------------------------------------------------
    integer                                :: stat
    type(NUOPC_RunElement), pointer        :: runElement
    type(NUOPC_RunElement), pointer        :: searchElement, prevElement
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! sanity check
    if (.not.associated(runSeq%first)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="runSeq must exist",&
        line=__LINE__, file=FILENAME, rcToReturn=rc)
      return  ! bail out
    endif
    
    ! allocate a new run element
    allocate(runElement, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of RunElement in NUOPC_RunElementAdd.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    ! initialize the new run element
    ! - new style members
!    runElement%gcomp => NULL()  !gjt: not yet used
!    runElement%ccomp => NULL()  !gjt: not yet used
    ! - old style members
    runElement%i = i
    runElement%j = j
    ! - common members
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
!BOPI
! !IROUTINE: NUOPC_RunElementAddComp - Add a RunElement for a Component to the end of a RunSequence
! !INTERFACE:
  subroutine NUOPC_RunElementAddComp(runSeq, i, j, phase, rc)
! !ARGUMENTS:
    type(NUOPC_RunSequence), intent(inout), target :: runSeq
    integer,                 intent(in)            :: i
    integer,                 intent(in),  optional :: j
    integer,                 intent(in),  optional :: phase
    integer, optional,       intent(out)           :: rc
! !DESCRIPTION:
!   Add a new RunElement for a Component to the end of an existing RunSequence.
!   The RunElement is set to the values provided for {\tt i}, {\tt j}, 
!   {\tt phase}, or as determined by their defaults.
!
!   The arguments are:
!   \begin{description}
!   \item[runSeq]
!     An existing {\tt NUOPC\_RunSequence} object.
!   \item[i]
!     Element {\tt i} index. This index must be > 0. Corresponds to the Model 
!     or Mediator component index if {\tt j} < 0. Corresponds to src side of a
!     Connector if {\tt j} >= 0.
!   \item[{[j]}]
!     Element {\tt j} index. Defaults to -1.
!   \item[{[phase]}]
!     Element {\tt phase} index. Defaults to 1.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
  !-----------------------------------------------------------------------------
    integer                                :: localrc
    integer                                :: jLocal, pLocal
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! error checking
    if (i<0) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="i must not be < 0.",&
        line=__LINE__, file=FILENAME, rcToReturn=rc)
      return  ! bail out
    endif
    
    ! handle defaults
    if (present(j)) then
      jLocal = j
    else
      jLocal = -1
    endif
    if (present(phase)) then
      pLocal = phase
    else
      pLocal = 1
    endif
    
    ! call into the more generic method
    call NUOPC_RunElementAdd(runSeq, i=i, j=jLocal, phase=pLocal, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_RunElementAddLink - Add a RunElement for a Link to the end of a RunSequence
! !INTERFACE:
  subroutine NUOPC_RunElementAddLink(runSeq, slot, rc)
! !ARGUMENTS:
    type(NUOPC_RunSequence), intent(inout), target :: runSeq
    integer,                 intent(in)            :: slot
    integer, optional,       intent(out)           :: rc
! !DESCRIPTION:
!   Add a new RunElement for a link to the end of an existing RunSequence.
!
!   The arguments are:
!   \begin{description}
!   \item[runSeq]
!     An existing {\tt NUOPC\_RunSequence} object.
!   \item[slot]
!     Run sequence slot to be linked to. Must be > 0.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
  !-----------------------------------------------------------------------------
    integer :: localrc

    if (present(rc)) rc = ESMF_SUCCESS
    
    ! error checking
    if (slot<=0) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="slot must be > 0.",&
        line=__LINE__, file=FILENAME, rcToReturn=rc)
      return  ! bail out
    endif
    
    ! call into the more generic method
    call NUOPC_RunElementAdd(runSeq, i=-slot, j=0, phase=0, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_RunElementPrint - Print info about a RunElement object
! !INTERFACE:
  subroutine NUOPC_RunElementPrint(runElement, logflag, rc)
! !ARGUMENTS:
    type(NUOPC_RunElement),  intent(in)  :: runElement
    logical, optional,       intent(in)  :: logflag
    integer, optional,       intent(out) :: rc
! !DESCRIPTION:
!   Write information about {\tt runElement}. If {\tt logflag} is set to 
!   {\tt .true.}, the output goes to the default log file. By default the 
!   output goes to stdout.
!EOPI
  !-----------------------------------------------------------------------------
    integer                   :: localrc
    character(ESMF_MAXSTR)    :: msgString
    logical                   :: logflagL
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    logflagL = .false.  ! default
    if (present(logflag)) logflagL = logflag

    write (msgString,"(A, I6, I6, I6)") "runElementPrint: ", &
      runElement%i, runElement%j, runElement%phase
    if (logflagL) then
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    else
      print *, trim(msgString)
    endif

  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOPI
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
!EOPI
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
      ! initialize run-time members
      nullify(runSeqNew(i)%stack)
      runSeqNew(i)%loopLevel = -1
      runSeqNew(i)%loopIteration = -1
      runSeqNew(i)%levelMember = -1
      runSeqNew(i)%levelChildren = -1
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
!BOPI
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
!EOPI
  !-----------------------------------------------------------------------------
    integer :: localrc
    integer :: i
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    if (associated(runSeq)) then
    
      ! deallocate the individual run sequences
      do i=1, size(runSeq)
        call NUOPC_RunSequenceSingleDeall(runSeq(i), rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      enddo
    
      deallocate(runSeq)  ! finally deallocate the actual runSeq array
      nullify(runSeq)     ! ensure recognizable condition
      
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_RunSequenceDeallocate - Deallocate a single RunSequence object
! !INTERFACE:
  ! Private name; call using NUOPC_RunSequenceDeallocate()
  subroutine NUOPC_RunSequenceSingleDeall(runSeq, rc)
! !ARGUMENTS:
    type(NUOPC_RunSequence), intent(inout)  :: runSeq
    integer, optional,       intent(out) :: rc
! !DESCRIPTION:
!   Deallocate all of the RunElements in the RunSequence defined by {\tt runSeq}.
!EOPI
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
!BOPI
! !IROUTINE: NUOPC_RunSequenceIterate - Iterate through a RunSequence
! !INTERFACE:
  function NUOPC_RunSequenceIterate(runSeq, runSeqIndex, runElement, rc)
! !RETURN VALUE:
    logical :: NUOPC_RunSequenceIterate
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
!   RunElement in {\tt runElement}. 
!
!   In either case the logical function return value is {\tt .true.} if the end
!   of iteration has not been reached by the forward step, and {\tt .false.} if
!   the end of iteration has been reached. 
!
!   The returned {\tt runElement} is only valid for a function return value of 
!   {\tt .true.}.
!EOPI
  !-----------------------------------------------------------------------------
    integer           :: localrc
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
      ! set the loop and level members for top level
      runSeq(runSeqIndex)%loopLevel=1
      runSeq(runSeqIndex)%loopIteration=1
      runSeq(runSeqIndex)%levelMember=1
      runSeq(runSeqIndex)%levelChildren=0
      ! check the clock
      clock = runSeq(runSeqIndex)%clock
      clockIsStopTime = ESMF_ClockIsStopTime(clock, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      NUOPC_RunSequenceIterate = .not.clockIsStopTime ! return value
      if (clockIsStopTime) return ! no reason to continue here
      ! finally set runElement
      runElement => runSeq(runSeqIndex)%first
      ! deal with potential "ENDDO" marker
      if (.not.associated(runElement%next)) then
        if (runElement%i == -runSeqIndex) then
          ! first run element happens to be an "ENDDO" marker
          do while (clockIsStopTime)
            ! advance to next time step
            call ESMF_ClockAdvance(clock, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
            ! check whether to stop at next time step
            clockIsStopTime = ESMF_ClockIsStopTime(clock, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          enddo
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
    NUOPC_RunSequenceIterate = NUOPC_RunSequenceCtrl(runSeq, runElement, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_RunSequencePrint - Print info about a single RunSequence object
! !INTERFACE:
  ! Private name; call using NUOPC_RunSequencePrint()
  subroutine NUOPC_RunSequenceSinglePrint(runSeq, logflag, rc)
! !ARGUMENTS:
    type(NUOPC_RunSequence), intent(in)  :: runSeq
    logical, optional,       intent(in)  :: logflag
    integer, optional,       intent(out) :: rc
! !DESCRIPTION:
!   Write information about {\tt runSeq}. If {\tt logflag} is set to 
!   {\tt .true.}, the output goes to the default log file. By default the 
!   output goes to stdout.
!EOPI
  !-----------------------------------------------------------------------------
    integer                         :: localrc
    character(ESMF_MAXSTR)          :: msgString
    logical                         :: logflagL
    type(NUOPC_RunElement), pointer :: searchElement

    if (present(rc)) rc = ESMF_SUCCESS
    
    logflagL = .false.  ! default
    if (present(logflag)) logflagL = logflag

    if (.not.associated(runSeq%first)) then
      write (msgString,"(A)") "runSeq::"
      if (logflagL) then
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      else
        write (*,"(A)") trim(msgString)
      endif
      write (msgString,"(A)") "::"
      if (logflagL) then
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      else
        write (*,"(A)") trim(msgString)
      endif
    else
      write (msgString,"(A)") "runSeq::"
      if (logflagL) then
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      else
        write (*,"(A)") trim(msgString)
      endif
      searchElement => runSeq%first
      do while (associated(searchElement%next))
        call NUOPC_RunElementPrint(searchElement, logflag, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        searchElement => searchElement%next
      enddo
      call NUOPC_RunElementPrint(searchElement, logflag, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      write (msgString,"(A)") "::"
      if (logflagL) then
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      else
        write (*,"(A)") trim(msgString)
      endif
    endif

  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_RunSequencePrint - Print info about a RunSequence vector
! !INTERFACE:
  ! Private name; call using NUOPC_RunSequencePrint()
  subroutine NUOPC_RunSequenceArrayPrint(runSeq, logflag, rc)
! !ARGUMENTS:
    type(NUOPC_RunSequence), pointer     :: runSeq(:)
    logical, optional,       intent(in)  :: logflag
    integer, optional,       intent(out) :: rc
! !DESCRIPTION:
!   Write information about the whole {\tt runSeq} vector. If {\tt logflag} is
!   set to {\tt .true.}, the output goes to the default log file. By default
!   the output goes to stdout.
!EOPI
  !-----------------------------------------------------------------------------
    integer                         :: localrc
    character(ESMF_MAXSTR)          :: msgString
    logical                         :: logflagL
    integer                         :: i
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    logflagL = .false.  ! default
    if (present(logflag)) logflagL = logflag
    
    do i=1, size(runSeq)
      write (msgString,"(A, I6, A, I6)") &
        "NUOPC_RunSequenceArrayPrint: element", i, " out of ", size(runSeq)
      if (logflagL) then
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      else
        write (*,"(A)") trim(msgString)
      endif
      call NUOPC_RunSequenceSinglePrint(runSeq(i), logflag, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    enddo
    
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_RunSequenceSet - Set values inside a RunSequence object
! !INTERFACE:
  subroutine NUOPC_RunSequenceSet(runSeq, clock, rc)
! !ARGUMENTS:
    type(NUOPC_RunSequence), intent(inout) :: runSeq
    type(ESMF_Clock),        intent(in)    :: clock
    integer, optional,       intent(out)   :: rc
! !DESCRIPTION:
!   Set the Clock member in {\tt runSeq}.
!EOPI
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
    type(NUOPC_RunSequence), pointer       :: runSeq(:)
    type(NUOPC_RunElement),  pointer       :: runElement
    integer, optional,       intent(out)   :: rc
! !DESCRIPTION:
!EOP
  !-----------------------------------------------------------------------------
    integer           :: localrc
    type(ESMF_Clock)  :: clock
    logical           :: clockIsStopTime
    integer           :: i
    type(ESMF_Time)   :: currTime
    
    if (present(rc)) rc = ESMF_SUCCESS
    NUOPC_RunSequenceCtrlResult = .false. ! initialize to safe return value

    ! sanity checks
    if (.not.associated(runSeq)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="runSeq must be associated", &
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
    
    ! reference the clock of the correct RunSequence slot
    ! for ENDDO this does not change clock on left hand side, for LINK it does
    clock = runSeq(i)%clock
    
    if (.not.associated(runElement%next)) then
      ! "ENDDO" element
#if 0
      print *, "found ENDDO element"
#endif
      ! advance the clock and check for stop time
      call ESMF_ClockAdvance(clock, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      clockIsStopTime = ESMF_ClockIsStopTime(clock, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
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
        ! loop back to start of same run sequence slot
        runElement => runSeq(i)%first  ! first element in next iteration
        ! increment the iteration counter
        runSeq(i)%loopIteration = runSeq(i)%loopIteration + 1
        ! reset the children counter
        runSeq(i)%levelChildren = 0
      endif
    else
      ! "LINK" element
#if 0
      print *, "found LINK element"
#endif
      if (associated(runSeq(i)%stack)) then
        ! detected recursive link
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="recursive link detected",&
          line=__LINE__, file=FILENAME, rcToReturn=rc)
        return
      endif
      ! set the linked RunSequence level index one higher than current one
      runSeq(i)%loopLevel = runElement%runSeq%loopLevel + 1
      ! set the linked RunSequence member index
      runElement%runSeq%levelChildren = runElement%runSeq%levelChildren + 1
      runSeq(i)%levelMember = runElement%runSeq%levelChildren
      if (runSeq(i)%levelMember==1) then
        ! first level member checksets Clock, forcing to upper level currTime
        call NUOPC_CheckSetClock(setClock=clock, &
          checkClock=runElement%runSeq%clock, setStartTimeToCurrent=.true., &
          forceCurrTime=.true., rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      else
        ! follow-on members checkset Clocks, forcing to previous member currTime
        call ESMF_ClockGet(runElement%runSeq%prevMemberClock, &
          currTime=currTime, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        call NUOPC_CheckSetClock(setClock=clock, &
          checkClock=runElement%runSeq%clock, setStartTimeToCurrent=.true., &
          currTime=currTime, forceCurrTime=.true., rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      endif
      ! set the prevMemberClock in case more level members follow-on
      runElement%runSeq%prevMemberClock = runSeq(i)%clock
      ! reset the linked RunSequence iteration and levelChildren counters
      runSeq(i)%loopIteration = 1
      runSeq(i)%levelChildren = 0
      ! put the next element in the current level onto the new levels stack
      runSeq(i)%stack => runElement%next  ! set stack pointer for return
      ! follow the link: start at the top of linked sequence
      runElement => runSeq(i)%first  ! first element in next iteration
    endif
    
    ! recursive call...
    NUOPC_RunSequenceCtrlResult = NUOPC_RunSequenceCtrl(runSeq, runElement, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
  
  end function
  !-----------------------------------------------------------------------------

end module NUOPC_RunSequenceDef
