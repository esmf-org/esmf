! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2017, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_MapperRunSeqUtil.F90"
!==============================================================================
!
!     ESMF Mapper Run Sequence utils module
module ESMF_MapperRunSeqUtilMod
!
!==============================================================================
!
! This file contains the utils to read and process a NUOPC run sequence
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_MapperRunSeqUtilMod - Mapper Run sequence util class
!
! !DESCRIPTION:
! The code in this file implements the utils for processing a NUOPC run sequence
!
! This type is implemented in Fortran 90.
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_LogErrMod
  use ESMF_StateTypesMod
  use ESMF_CompMod
  use ESMF_GridCompMod
  use ESMF_CplCompMod
  use ESMF_VMMod
  use ESMF_InitMacrosMod


  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

  ! Token types
  integer, parameter :: ESMF_TIMELOOP_BEGIN_TOKEN = 1,&
                        ESMF_TIMELOOP_END_TOKEN=2,&
                        ESMF_COMP_PHASE_TOKEN=3,&
                        ESMF_CONN_TOKEN=4,&
                        ESMF_RUNSEQ_BEGIN_TOKEN=5,&
                        ESMF_RUNSEQ_END_TOKEN=6,&
                        ESMF_OTHER_TOKEN=7

  ! We currently only care about two tokens
  integer, parameter :: ESMF_RUNSEQ_MAXTOKENS = 2
  integer, parameter :: ESMF_RUNSEQ_TOK1=1, ESMF_RUNSEQ_TOK2=2
  integer, parameter :: ESMF_RUNSEQ_MAXTOKENSTR = 128
  type ESMF_RunSeqTokenizedLine
    character(len=ESMF_RUNSEQ_MAXTOKENSTR) :: tok(ESMF_RUNSEQ_MAXTOKENS)
    integer :: tok_type
    integer :: lnum
  end type
  type ESMF_RunSeqTokenizedCode
    type(ESMF_RunSeqTokenizedLine), dimension(:), allocatable :: line
  end type

  type ESMF_RunSeqTimeLoopInfo
    integer :: timeLoopCount
    real :: timeLoopTime
    integer :: runSeqLoopLineNum
  end type

  type ESMF_RunSeqTimeLoopStack
    integer :: runSeqTimeLoopInfoIdx
  end type

  type ESMF_MapperRunSeqDepGraphNodePtr
    type(ESMF_MapperRunSeqDepGraphNode), pointer :: ptr => null()
  end type

  type ESMF_MapperRunSeqDepGraphNode
    type(ESMF_RunSeqTokenizedLine) :: line
    type(ESMF_MapperRunSeqDepGraphNodePtr), dimension(:), allocatable :: children
    integer :: nchildren
  end type

  type ESMF_MapperRunSeqDepGraph
    type(ESMF_MapperRunSeqDepGraphNode), dimension(:), pointer :: nodes => null()
  end type


!------------------------------------------------------------------------------
! !PUBLIC TYPES:

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:

  public ESMF_MapperProcessRunSeq ! Read and process the NUOPC run sequence
  public ESMF_MapperRunSeqDepGraph ! The run sequence dependency graph

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id$'


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

! Trims spaces at the begin and end of a string
function ESMF_FullStrTrim(str) result(ostr)
  character(len=*), intent(in) :: str

  character(len=ESMF_MAXSTR) :: ostr
  character, parameter :: SPACE = ' '
  integer :: slen, i

  ostr = trim(str)
  slen = len(trim(str))
  if(slen > ESMF_MAXSTR) then
    print *, "ERROR: Truncating string !"
    slen = ESMF_MAXSTR
  end if

  do i=1,slen
    if(str(i:i) /= SPACE) then
      ostr = trim(str(i:))
      exit
    end if
  end do
  
  !print *, """", trim(str), """"
  !print *, """", trim(ostr), """"

end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperTokenizeRunSeq
!BOPI
! !IROUTINE: ESMF_MapperTokenizeRunSeq - Tokenize the run sequence

! !INTERFACE:
  subroutine ESMF_MapperTokenizeRunSeq(runSeqCode, tokRunSeq, rc)
!
!
! !ARGUMENTS:
    character(len=ESMF_MAXSTR), dimension(:), intent(in) :: runSeqCode
    type(ESMF_RunSeqTokenizedCode), intent(out) :: tokRunSeq
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Collects all info required by the ESMF\_Mapper from a component
!
! The arguments are:
!   \begin{description}
!   \item[{[runSeqCode]}]
!     The run sequence (code)
!   \item[{[tokRunSeq]}]
!     Tokenized run sequence
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
  !-----------------------------------------------------------------------------    
    character(len=*), parameter :: COMMENT_TOKENSTR = "#"
    character(len=*), parameter :: MISC_EOL_TOKENSTR = "#:"
    character(len=*), parameter :: SPACE_TOKENSTR = " "
    character(len=*), parameter :: RUNSEQ_BEGIN_TOKENSTR = "runSeq::"
    character(len=*), parameter :: RUNSEQ_END_TOKENSTR = "::"
    character(len=*), parameter :: TIMELOOP_TOKENSTR = "@"
    character(len=*), parameter :: CONN_TOKENSTR = "->"
    integer :: pos, lpos, i, nlines
    character(len=ESMF_RUNSEQ_MAXTOKENSTR) :: tmp_tok
    
    if (present(rc)) rc = ESMF_SUCCESS

    nlines = size(runSeqCode)
    allocate(tokRunSeq%line(nlines))
    do i=1,nlines
      tokRunSeq%line(i)%tok(ESMF_RUNSEQ_TOK1) = ""
      tokRunSeq%line(i)%tok(ESMF_RUNSEQ_TOK2) = ""
      tokRunSeq%line(i)%lnum = i
      ! A blank line
      if(len(trim(runSeqCode(i))) == 0) then
        tokRunSeq%line(i)%tok_type = ESMF_OTHER_TOKEN
        cycle  
      end if
      ! Entire line is a comment
      pos = INDEX(runSeqCode(i), COMMENT_TOKENSTR)
      if(pos == 1) then
        tokRunSeq%line(i)%tok_type = ESMF_OTHER_TOKEN
        cycle  
      end if
      if((pos /= 0) .and. (len(trim(runSeqCode(i)(:pos-1))) == 0)) then
        tokRunSeq%line(i)%tok_type = ESMF_OTHER_TOKEN
        cycle  
      end if
      pos = INDEX(runSeqCode(i), RUNSEQ_BEGIN_TOKENSTR)
      if(pos /= 0) then
        tokRunSeq%line(i)%tok_type = ESMF_RUNSEQ_BEGIN_TOKEN
        cycle  
      end if
      pos = INDEX(runSeqCode(i), RUNSEQ_END_TOKENSTR)
      if(pos /= 0) then
        tokRunSeq%line(i)%tok_type = ESMF_RUNSEQ_END_TOKEN
        cycle  
      end if
      pos = INDEX(runSeqCode(i), TIMELOOP_TOKENSTR)
      if(pos /= 0) then
        if(len(trim(runSeqCode(i)(pos:))) == 1) then
          tokRunSeq%line(i)%tok_type = ESMF_TIMELOOP_END_TOKEN
        else
          tokRunSeq%line(i)%tok_type = ESMF_TIMELOOP_BEGIN_TOKEN
          tokRunSeq%line(i)%tok(ESMF_RUNSEQ_TOK1) = trim(runSeqCode(i)(pos+1:))
        end if
      else
        ! Ignore misc tokens (comments/remapMethods etc)
        ! at the end of the line
        lpos = SCAN(runSeqCode(i), MISC_EOL_TOKENSTR)
        if(lpos == 1) then
          ! We don't expect the tokens to be the first char in the line
          ! Full line comments are handled above
          print *, "ERROR: Parse error !"
          if(present(rc)) then
            rc = ESMF_FAILURE
          end if
          exit
        else if(lpos == 0) then
          lpos = len(trim(runSeqCode(i)))
        else
          ! Ignore the misc token
          lpos = lpos - 1
        end if
        pos = INDEX(runSeqCode(i)(:lpos), CONN_TOKENSTR)
        if(pos /= 0) then
          tokRunSeq%line(i)%tok_type = ESMF_CONN_TOKEN
          tokRunSeq%line(i)%tok(ESMF_RUNSEQ_TOK1) =&
            ESMF_FullStrTrim(runSeqCode(i)(:pos-1))
          tokRunSeq%line(i)%tok(ESMF_RUNSEQ_TOK2) =&
            ESMF_FullStrTrim(runSeqCode(i)(pos+2:lpos))
        else
          tokRunSeq%line(i)%tok_type = ESMF_COMP_PHASE_TOKEN
          tmp_tok = ESMF_FullStrTrim(runSeqCode(i)(:lpos))
          print *, " tmp_tok = ", """", trim(tmp_tok), """"
          pos = INDEX(tmp_tok, SPACE_TOKENSTR)
          if(pos == 0)then
            tokRunSeq%line(i)%tok(ESMF_RUNSEQ_TOK1) =&
              ESMF_FullStrTrim(tmp_tok)
          else if(len(trim(tmp_tok(pos:))) == 0) then
            tokRunSeq%line(i)%tok(ESMF_RUNSEQ_TOK1) =&
              ESMF_FullStrTrim(tmp_tok(:pos))
          else
            tokRunSeq%line(i)%tok(ESMF_RUNSEQ_TOK1) =&
              ESMF_FullStrTrim(tmp_tok(:pos))
            tokRunSeq%line(i)%tok(ESMF_RUNSEQ_TOK2) =&
              ESMF_FullStrTrim(tmp_tok(pos+1:lpos))
          end if
        end if
      end if
    end do 

  end subroutine
!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "GetSeqCodeDbgInfo
!BOPI
! !IROUTINE: GetSeqCodeDbgInfo - Get debug info on the runsequence

! !INTERFACE:
subroutine GetSeqCodeDbgInfo(runSeqCode, tokRunSeq, curLine, dbgStr, rc)
!
!
! !ARGUMENTS:
    character(len=ESMF_MAXSTR), dimension(:), intent(in) :: runSeqCode
    type(ESMF_RunSeqTokenizedCode), intent(in) :: tokRunSeq
    integer, intent(in) :: curLine
    character(len=ESMF_MAXSTR), intent(out) :: dbgStr
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Gets debug information about the run sequence
!
! The arguments are:
!   \begin{description}
!   \item[{[runSeqCode]}]
!     The run sequence (code)
!   \item[{[tokRunSeq]}]
!     Tokenized run sequence
!   \item[{[curLine]}]
!     Current line number in the run sequence
!   \item[{[dbgStr]}]
!     The debug information will be returned in this string
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

  dbgStr = "Running : " // trim(runSeqCode(curLine))
  if(tokRunSeq%line(curLine)%tok_type == ESMF_TIMELOOP_BEGIN_TOKEN) then
    dbgStr = trim(dbgStr) // " : Timeloop (begin)" 
  else if(tokRunSeq%line(curLine)%tok_type == ESMF_TIMELOOP_END_TOKEN) then
    dbgStr = trim(dbgStr) // " : Timeloop (end)" 
  else if(tokRunSeq%line(curLine)%tok_type == ESMF_COMP_PHASE_TOKEN) then
    dbgStr = trim(dbgStr) // " : comp phase :" 
    dbgStr = trim(dbgStr) // " comp = "
    dbgStr = trim(dbgStr) // tokRunSeq%line(curLine)%tok(ESMF_RUNSEQ_TOK1)
    if(len(trim(tokRunSeq%line(curLine)%tok(ESMF_RUNSEQ_TOK2))) /= 0) then
      dbgStr = trim(dbgStr) // " phase = "
      dbgStr = trim(dbgStr) // trim(tokRunSeq%line(curLine)%tok(ESMF_RUNSEQ_TOK2))
    end if
  else if(tokRunSeq%line(curLine)%tok_type == ESMF_CONN_TOKEN) then
    dbgStr = trim(dbgStr) // " : conn from : "
    dbgStr = trim(dbgStr) // trim(tokRunSeq%line(curLine)%tok(ESMF_RUNSEQ_TOK1))
    dbgStr = trim(dbgStr) // " - to - "
    dbgStr = trim(dbgStr) // trim(tokRunSeq%line(curLine)%tok(ESMF_RUNSEQ_TOK2))
  else if(tokRunSeq%line(curLine)%tok_type == ESMF_RUNSEQ_BEGIN_TOKEN) then
    dbgStr = trim(dbgStr) // " : runseq (begin)" 
  else if(tokRunSeq%line(curLine)%tok_type == ESMF_RUNSEQ_END_TOKEN) then
    dbgStr = trim(dbgStr) // " : runseq (end)" 
  else
    dbgStr = trim(dbgStr) // " : misc" 
  end if

  if(present(rc)) then
    rc = ESMF_SUCCESS
  end if

end subroutine

! Get information about the run sequence
! This function return the number of timeloops and number of lines of 
! execution (at runtime) for the tokenized run sequence
subroutine ESMF_GetRunSeqInfo(runSeqCode, tokRunSeq, nTimeLoops, nExecLines, rc)
  character(len=ESMF_MAXSTR), dimension(:), intent(in) :: runSeqCode
  type(ESMF_RunSeqTokenizedCode), intent(in) :: tokRunSeq
  integer, intent(out) :: nTimeLoops
  integer, intent(out) :: nExecLines
  integer, intent(out), optional :: rc

  integer :: i, j, nlines
  type LoopInfo
    integer :: loopCount
    real :: loopTime
    integer :: nlinesInLoop
  end type

  type(LoopInfo), dimension(:), allocatable :: loopInfoStack
  integer :: loopInfoStackTop

  nTimeLoops = 0
  nlines = size(tokRunSeq%line)
  do i=1,nlines
    if(tokRunSeq%line(i)%tok_type == ESMF_TIMELOOP_BEGIN_TOKEN) then
      nTimeLoops = nTimeLoops + 1
    end if
  end do 

  allocate(LoopInfoStack(nTimeLoops))

  loopInfoStackTop = 0
  do i=1,nlines
    if(tokRunSeq%line(i)%tok_type == ESMF_TIMELOOP_BEGIN_TOKEN) then
      loopInfoStackTop = loopInfoStackTop + 1
      if(loopInfoStackTop == 1) then
        read(tokRunSeq%line(i)%tok(ESMF_RUNSEQ_TOK1), *)&
          loopInfoStack(loopInfoStackTop)%loopTime
        loopInfoStack(loopInfoStackTop)%loopCount = 1
        loopInfoStack(loopInfoStackTop)%nlinesInLoop = 0
      else
        read(tokRunSeq%line(i)%tok(ESMF_RUNSEQ_TOK1), *)&
          loopInfoStack(loopInfoStackTop)%loopTime
        loopInfoStack(loopInfoStackTop)%loopCount =&
          INT(loopInfoStack(loopInfoStackTop-1)%loopTime/&
              loopInfoStack(loopInfoStackTop)%loopTime)
        loopInfoStack(loopInfoStackTop)%nlinesInLoop = 0
      end if
      cycle
    else if(tokRunSeq%line(i)%tok_type == ESMF_TIMELOOP_END_TOKEN) then
      if(loopInfoStackTop /= 1) then
        do j=loopInfoStackTop-1,1,-1
          if(loopInfoStack(j)%loopCount /= 0) then
            loopInfoStack(loopInfoStackTop)%loopCount =&
              loopInfoStack(loopInfoStackTop)%loopCount *&
              loopInfoStack(j)%loopCount
          end if
        end do
      end if
      loopInfoStackTop = loopInfoStackTop - 1
      cycle
    else if(tokRunSeq%line(i)%tok_type == ESMF_COMP_PHASE_TOKEN) then
      if(loopInfoStackTop == 0) then
        print *, "ERROR: COMP PHASE outside timeloop !"
        print *, "ERROR : ", tokRunSeq%line(i)%tok(ESMF_RUNSEQ_TOK1)
      else
        loopInfoStack(loopInfoStackTop)%nlinesInLoop =&
          loopInfoStack(loopInfoStackTop)%nlinesInLoop + 1
        !print *, "Counting : ", trim(runSeqCode(i)), loopInfoStack(loopInfoStackTop)%nlinesInLoop, loopInfoStack(loopInfoStackTop)%loopCount
      end if
    else if(tokRunSeq%line(i)%tok_type == ESMF_CONN_TOKEN) then
      if(loopInfoStackTop == 0) then
        print *, "ERROR: CONN outside timeloop !"
      else
        loopInfoStack(loopInfoStackTop)%nlinesInLoop =&
          loopInfoStack(loopInfoStackTop)%nlinesInLoop + 1
        !print *, "Counting : ", trim(runSeqCode(i)), loopInfoStack(loopInfoStackTop)%nlinesInLoop, loopInfoStack(loopInfoStackTop)%loopCount
      end if
    else
      ! ignore these tokens
    end if
  end do 

  nExecLines = 0
  do i=1,nTimeLoops
    nExecLines = nExecLines + loopInfoStack(i)%nlinesInLoop *&
      loopInfoStack(i)%loopCount
  end do

end subroutine

! Print the run sequence dependency graph node
subroutine ESMF_MapperPrintDepGraphNode(node, rc)
  type(ESMF_MapperRunSeqDepGraphNode), intent(inout) :: node
  integer, optional, intent(out) :: rc

  integer :: j

  if(node%line%tok_type == ESMF_COMP_PHASE_TOKEN) then
    print *, "Node :",&
      trim(node%line%tok(ESMF_RUNSEQ_TOK1)),&
      "(", node%line%lnum, ")",&
      " : ",&
      trim(node%line%tok(ESMF_RUNSEQ_TOK2)),&
      " : Has :", node%nchildren,&
      " children "
    do j=1,node%nchildren
      print *, "          : ",&
        trim(node%children(j)%ptr%line%tok(ESMF_RUNSEQ_TOK1)),& 
        " : ",&
        trim(node%children(j)%ptr%line%tok(ESMF_RUNSEQ_TOK2)), &
        node%children(j)%ptr%line%lnum
    end do
  else if(node%line%tok_type == ESMF_CONN_TOKEN) then
    print *, "Node :",&
      trim(node%line%tok(ESMF_RUNSEQ_TOK1)),&
      " --> ",&
      trim(node%line%tok(ESMF_RUNSEQ_TOK2))
  end if

  if(present(rc)) then
    rc = ESMF_SUCCESS
  end if
end subroutine

! Print the run sequence dependency graph
subroutine ESMF_MapperPrintDepGraph(runSeqDepGraph, rc)
  type(ESMF_MapperRunSeqDepGraph), intent(inout) :: runSeqDepGraph
  integer, optional, intent(out) :: rc

  integer :: localrc
  integer :: nnodes, i, j

  nnodes = size(runSeqDepGraph%nodes)
  do i=1,nnodes
    call ESMF_MapperPrintDepGraphNode(runSeqDepGraph%nodes(i), rc=localrc)
  end do

end subroutine

! Create the complete run sequence dependency graph
! The run sequence dependency graph is already populated with all the nodes.
! This function adds the parent-child connection between the nodes
subroutine ESMF_MapperCreateDepGraph(runSeqDepGraph, rc)
  type(ESMF_MapperRunSeqDepGraph), intent(inout) :: runSeqDepGraph
  integer, optional, intent(out) :: rc

  integer :: localrc
  integer :: nnodes, i, j, k, ichild
  logical :: ischild
  integer, dimension(:), allocatable :: distGraphNodeChildrenTop


  ! FIXME : Add comparison op for graph nodes
  nnodes = size(runSeqDepGraph%nodes)
  do i=1,nnodes
    if(runSeqDepGraph%nodes(i)%line%tok_type == ESMF_COMP_PHASE_TOKEN) then
      do j=i-1,1,-1
        if(runSeqDepGraph%nodes(j)%line%tok_type == ESMF_COMP_PHASE_TOKEN) then
          if(trim(runSeqDepGraph%nodes(j)%line%tok(ESMF_RUNSEQ_TOK1)) ==&
              trim(runSeqDepGraph%nodes(i)%line%tok(ESMF_RUNSEQ_TOK1))) then
            runSeqDepGraph%nodes(j)%nchildren =&
              runSeqDepGraph%nodes(j)%nchildren + 1
            exit
          end if
        else if(runSeqDepGraph%nodes(j)%line%tok_type == ESMF_CONN_TOKEN) then
          if(& 
              (&
              trim(runSeqDepGraph%nodes(j)%line%tok(ESMF_RUNSEQ_TOK1)) ==&
              trim(runSeqDepGraph%nodes(i)%line%tok(ESMF_RUNSEQ_TOK1))&
              )&
              .or.&
              (&
              trim(runSeqDepGraph%nodes(j)%line%tok(ESMF_RUNSEQ_TOK2)) ==&
              trim(runSeqDepGraph%nodes(i)%line%tok(ESMF_RUNSEQ_TOK1))&
              )&
            ) then
            do k=j-1,1,-1
              if( trim(runSeqDepGraph%nodes(j)%line%tok(ESMF_RUNSEQ_TOK1)) ==&
                  trim(runSeqDepGraph%nodes(k)%line%tok(ESMF_RUNSEQ_TOK1)) ) then
                runSeqDepGraph%nodes(k)%nchildren =&
                  runSeqDepGraph%nodes(k)%nchildren + 1
                exit
              end if
            end do
            do k=j-1,1,-1
              if( trim(runSeqDepGraph%nodes(j)%line%tok(ESMF_RUNSEQ_TOK2)) ==&
                  trim(runSeqDepGraph%nodes(k)%line%tok(ESMF_RUNSEQ_TOK1)) ) then
                runSeqDepGraph%nodes(k)%nchildren =&
                  runSeqDepGraph%nodes(k)%nchildren + 1
                exit
              end if
            end do
            !exit
          end if
        end if
      end do
    end if
  end do

  allocate(distGraphNodeChildrenTop(nnodes))
  do i=1,nnodes
    distGraphNodeChildrenTop(i) = 1
    allocate(runSeqDepGraph%nodes(i)%children(runSeqDepGraph%nodes(i)%nchildren))
    print *, "Node : ", i, " has ", runSeqDepGraph%nodes(i)%nchildren, " children"
  end do

  do i=1,nnodes
    if(runSeqDepGraph%nodes(i)%line%tok_type == ESMF_COMP_PHASE_TOKEN) then
      do j=i-1,1,-1
        if(runSeqDepGraph%nodes(j)%line%tok_type == ESMF_COMP_PHASE_TOKEN) then
          if(trim(runSeqDepGraph%nodes(j)%line%tok(ESMF_RUNSEQ_TOK1)) ==&
              trim(runSeqDepGraph%nodes(i)%line%tok(ESMF_RUNSEQ_TOK1))) then
            ischild = .false.
            do ichild=1,distGraphNodeChildrenTop(j)-1
              if(runSeqDepGraph%nodes(j)%children(ichild)%ptr%line%tok(ESMF_RUNSEQ_TOK1) ==&
                  runSeqDepGraph%nodes(i)%line%tok(ESMF_RUNSEQ_TOK1)) then
                ischild = .true.
                exit
              end if
            end do
            if(.not. ischild) then
              runSeqDepGraph%nodes(j)%children(distGraphNodeChildrenTop(j))%ptr =>&
                runSeqDepGraph%nodes(i)
              distGraphNodeChildrenTop(j) = distGraphNodeChildrenTop(j) + 1
            else
              runSeqDepGraph%nodes(j)%nchildren = runSeqDepGraph%nodes(j)%nchildren - 1
            end if
            exit
          end if
        else if(runSeqDepGraph%nodes(j)%line%tok_type == ESMF_CONN_TOKEN) then
          !if(&
          !    (trim(runSeqDepGraph%nodes(j)%line%tok(ESMF_RUNSEQ_TOK1)) ==&
          !    trim(runSeqDepGraph%nodes(i)%line%tok(ESMF_RUNSEQ_TOK1))) .or.&
          !    (trim(runSeqDepGraph%nodes(j)%line%tok(ESMF_RUNSEQ_TOK2)) ==&
          !    trim(runSeqDepGraph%nodes(i)%line%tok(ESMF_RUNSEQ_TOK1)))&
          !  ) then
          !  runSeqDepGraph%nodes(j)%children(distGraphNodeChildrenTop(j)) =&
          !    runSeqDepGraph%nodes(i)
          !  exit
          !end if
          if(& 
              (&
              trim(runSeqDepGraph%nodes(j)%line%tok(ESMF_RUNSEQ_TOK1)) ==&
              trim(runSeqDepGraph%nodes(i)%line%tok(ESMF_RUNSEQ_TOK1))&
              )&
              .or.&
              (&
              trim(runSeqDepGraph%nodes(j)%line%tok(ESMF_RUNSEQ_TOK2)) ==&
              trim(runSeqDepGraph%nodes(i)%line%tok(ESMF_RUNSEQ_TOK1))&
              )&
            ) then
            do k=j-1,1,-1
              if( trim(runSeqDepGraph%nodes(j)%line%tok(ESMF_RUNSEQ_TOK1)) ==&
                  trim(runSeqDepGraph%nodes(k)%line%tok(ESMF_RUNSEQ_TOK1)) ) then
                ischild = .false.
                do ichild=1,distGraphNodeChildrenTop(k) - 1
                  if(runSeqDepGraph%nodes(k)%children(ichild)%ptr%line%tok(ESMF_RUNSEQ_TOK1)&
                      == runSeqDepGraph%nodes(i)%line%tok(ESMF_RUNSEQ_TOK1)) then
                    ischild = .true.
                    exit
                  end if
                end do
                if(.not. ischild) then
                  runSeqDepGraph%nodes(k)%children(distGraphNodeChildrenTop(k))%ptr =>&
                    runSeqDepGraph%nodes(i)
                  distGraphNodeChildrenTop(k) = distGraphNodeChildrenTop(k) + 1
                else
                  runSeqDepGraph%nodes(k)%nchildren = runSeqDepGraph%nodes(k)%nchildren-1
                end if
                exit
              end if
            end do
            do k=j-1,1,-1
              if( trim(runSeqDepGraph%nodes(j)%line%tok(ESMF_RUNSEQ_TOK2)) ==&
                  trim(runSeqDepGraph%nodes(k)%line%tok(ESMF_RUNSEQ_TOK1)) ) then
                ischild = .false.
                do ichild=1,distGraphNodeChildrenTop(k) -1
                  if(runSeqDepGraph%nodes(k)%children(ichild)%ptr%line%tok(ESMF_RUNSEQ_TOK1)&
                      == runSeqDepGraph%nodes(i)%line%tok(ESMF_RUNSEQ_TOK1)) then
                    ischild = .true.
                    exit
                  end if
                end do
                if(.not. ischild) then
                  runSeqDepGraph%nodes(k)%children(distGraphNodeChildrenTop(k))%ptr =>&
                    runSeqDepGraph%nodes(i)
                  distGraphNodeChildrenTop(k) = distGraphNodeChildrenTop(k) + 1
                else
                  runSeqDepGraph%nodes(k)%nchildren = runSeqDepGraph%nodes(k)%nchildren-1
                end if
                exit
              end if
            end do
            !exit
          end if
        end if
      end do
    end if
  end do

end subroutine

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperProcessRunSeq
!BOPI
! !IROUTINE: ESMF_MapperProcessRunSeq - Process the run sequence

! !INTERFACE:
  subroutine ESMF_MapperProcessRunSeq(runSeqCode, runSeqDepGraph, rc)
!
!
! !ARGUMENTS:
    character(len=ESMF_MAXSTR), dimension(:), intent(in) :: runSeqCode
    type(ESMF_MapperRunSeqDepGraph), intent(out) :: runSeqDepGraph
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Collects all info required by the ESMF\_Mapper from a component
!
! The arguments are:
!   \begin{description}
!   \item[{[runSeqCode]}]
!     The run sequence (code)
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
  !-----------------------------------------------------------------------------    
  ! Token types
!  integer, parameter :: ESMF_TIMELOOP_TOKEN = 1,&
!                        ESMF_COMP_PHASE_TOKEN=2,&
!                        ESMF_CONN_TOKEN=3,&
!                        ESMF_OTHER_TOKEN=4

!  integer, parameter :: ESMF_RUNSEQ_MAXTOKENSTR = 128
!  type ESMF_RunSeqTokenizedLine
!    character(len=ESMF_RUNSEQ_MAXTOKENSTR), dimension(:), allocatable :: tok
!    integer :: tok_type
!  end type
!  type ESMF_RunSeqTokenizedCode
!    type(ESMF_RunSeqTokenizedLine), dimension(:), allocatable :: line
!  end type

!  type ESMF_RunSeqTimeLoopInfo
!    integer :: timeLoopCount
!    integer :: runSeqLoopLineNum
!  end type
!  type ESMF_RunSeqTimeLoopStack
!    integer :: runSeqTimeLoopInfoIdx
!  end type

    integer :: nTimeLoops, nlines, nExecLines, i, inode
    integer :: curLine
    type(ESMF_RunSeqTimeLoopInfo), dimension(:), allocatable :: timeLoopInfoStack
    integer :: timeLoopInfoStackTop = 0
    type(ESMF_RunSeqTokenizedCode) :: tokRunSeq
    character(len=ESMF_MAXSTR) :: dbgStr
    integer :: localrc
    

    call ESMF_MapperTokenizeRunSeq(runSeqCode, tokRunSeq, rc=localrc)
    !if (ESMF_LogFoundError(localrc, &
    !  ESMF_ERR_PASSTHRU, &
    !  ESMF_CONTEXT, rcToReturn=localrc)) return

    nTimeLoops = 0
    nlines = size(tokRunSeq%line)
    !do i=1,nlines
    !  if(tokRunSeq%line(i)%tok_type == ESMF_TIMELOOP_BEGIN_TOKEN) then
    !    nTimeLoops = nTimeLoops + 1
    !  end if
    !end do 
    call ESMF_GetRunSeqInfo(runSeqCode, tokRunSeq, nTimeLoops, nExecLines, rc=localrc)
    !if (ESMF_LogFoundError(localrc, &
    !  ESMF_ERR_PASSTHRU, &
    !  ESMF_CONTEXT, rcToReturn=localrc)) return

    print *, "nTimeLoops = ", nTimeLoops, ", nExecLines=", nExecLines

    allocate(runSeqDepGraph%nodes(nExecLines))
    inode = 1
    allocate(timeLoopInfoStack(nTimeLoops))
    curLine = 1
    do while(curLine <= nlines)
      if(tokRunSeq%line(curLine)%tok_type == ESMF_RUNSEQ_END_TOKEN) then
        exit
      end if
      if(tokRunSeq%line(curLine)%tok_type == ESMF_TIMELOOP_BEGIN_TOKEN) then
        ! Push a new time loop into stack
        timeLoopInfoStackTop = timeLoopInfoStackTop + 1
        print *, "Timeloop tok = ", tokRunSeq%line(curLine)%tok(1)
        if(timeLoopInfoStackTop == 1) then
          ! First time loop
          read(tokRunSeq%line(curLine)%tok(ESMF_RUNSEQ_TOK1), *)&
            timeLoopInfoStack(timeLoopInfoStackTop)%timeLoopTime
          timeLoopInfoStack(timeLoopInfoStackTop)%timeLoopCount = 1
          timeLoopInfoStack(timeLoopInfoStackTop)%runSeqLoopLineNum =&
            curLine 
        else
          ! We asssume that outer loops are multiples of inner loops
          read(tokRunSeq%line(curLine)%tok(ESMF_RUNSEQ_TOK1), *)&
            timeLoopInfoStack(timeLoopInfoStackTop)%timeLoopTime
          timeLoopInfoStack(timeLoopInfoStackTop)%timeLoopCount =&
            INT(timeLoopInfoStack(timeLoopInfoStackTop-1)%timeLoopTime/&
              timeLoopInfoStack(timeLoopInfoStackTop)%timeLoopTime)
          timeLoopInfoStack(timeLoopInfoStackTop)%runSeqLoopLineNum =&
            curLine 
        end if
        curLine = curLine + 1
        cycle
      else if(tokRunSeq%line(curLine)%tok_type == ESMF_TIMELOOP_END_TOKEN) then
        ! Check the top of the time loop stack to modify loop
        ! If loop is done, pop
        timeLoopInfoStack(timeLoopInfoStackTop)%timeLoopCount =&
          timeLoopInfoStack(timeLoopInfoStackTop)%timeLoopCount - 1

        if(timeLoopInfoStack(timeLoopInfoStackTop)%timeLoopCount > 0) then
          curLine = timeLoopInfoStack(timeLoopInfoStackTop)%runSeqLoopLineNum + 1
        else
          ! Loop done, pop the latest stack, move to next line
          timeLoopInfoStackTop = timeLoopInfoStackTop - 1
          curLine = curLine + 1
        end if
        cycle
      else if(tokRunSeq%line(curLine)%tok_type == ESMF_COMP_PHASE_TOKEN) then
        runSeqDepGraph%nodes(inode)%line = tokRunSeq%line(curLine)
        runSeqDepGraph%nodes(inode)%nchildren = 0
        inode = inode + 1
      else if(tokRunSeq%line(curLine)%tok_type == ESMF_CONN_TOKEN) then
        runSeqDepGraph%nodes(inode)%line = tokRunSeq%line(curLine)
        runSeqDepGraph%nodes(inode)%nchildren = 0
        inode = inode + 1
      end if
      call GetSeqCodeDbgInfo(runSeqCode, tokRunSeq, curLine, dbgStr, rc=localrc)
      !if (ESMF_LogFoundError(localrc, &
      !  ESMF_ERR_PASSTHRU, &
      !  ESMF_CONTEXT, rcToReturn=localrc)) return
      !print *, "Running : ", trim(runSeqCode(curLine)), " # ", curLine
      print *, trim(dbgStr)
      curLine = curLine + 1
    end do 

    print *, "Number of time loops = ", nTimeLoops

    call ESMF_MapperCreateDepGraph(runSeqDepGraph, rc=localrc)
    !if (ESMF_LogFoundError(localrc, &
    !  ESMF_ERR_PASSTHRU, &
    !  ESMF_CONTEXT, rcToReturn=localrc)) return

    call ESMF_MapperPrintDepGraph(runSeqDepGraph, rc=localrc)
    !if (ESMF_LogFoundError(localrc, &
    !  ESMF_ERR_PASSTHRU, &
    !  ESMF_CONTEXT, rcToReturn=localrc)) return

    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------


end module ESMF_MapperRunSeqUtilMod
