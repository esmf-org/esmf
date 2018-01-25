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

  ! We currently only care about the first three tokens
  integer, parameter :: ESMF_MAXNUMTOKENS = 3
  integer, parameter :: ESMF_MAXTOKENSTR = 128
  type ESMF_RunSeqTokenizedLine
    character(len=ESMF_MAXTOKENSTR) :: tok(ESMF_MAXNUMTOKENS)
    integer :: tok_type
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

!------------------------------------------------------------------------------
! !PUBLIC TYPES:

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:

  public ESMF_MapperProcessRunSeq ! Read and process the NUOPC run sequence

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id$'


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperTokenizeRunSeq
!BOP
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
!EOP
  !-----------------------------------------------------------------------------    
  ! Token types
!  integer, parameter :: ESMF_TIMELOOP_TOKEN = 1,&
!                        ESMF_COMP_PHASE_TOKEN=2,&
!                        ESMF_CONN_TOKEN=3,&
!                        ESMF_OTHER_TOKEN=4

!  integer, parameter :: ESMF_MAXTOKENSTR = 128
!  type ESMF_RunSeqTokenizedLine
!    character(len=ESMF_MAXTOKENSTR), dimension(:), allocatable :: tok
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

    character(len=*), parameter :: COMMENT_TOKENSTR = "#"
    character(len=*), parameter :: RUNSEQ_BEGIN_TOKENSTR = "runSeq::"
    character(len=*), parameter :: RUNSEQ_END_TOKENSTR = "::"
    character(len=*), parameter :: TIMELOOP_TOKENSTR = "@"
    character(len=*), parameter :: CONN_TOKENSTR = "->"
    integer :: pos, i, nlines
    
    nlines = size(runSeqCode)
    allocate(tokRunSeq%line(nlines))
    do i=1,nlines
      pos = INDEX(runSeqCode(i), COMMENT_TOKENSTR)
      if((pos /= 0) .and. (len(trim(runSeqCode(i)(:pos))) == 0)) then
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
          tokRunSeq%line(i)%tok(1) = trim(runSeqCode(i)(pos+1:))
        end if
      else
        pos = INDEX(runSeqCode(i), CONN_TOKENSTR)
        if(pos /= 0) then
          tokRunSeq%line(i)%tok_type = ESMF_CONN_TOKEN
        else
          tokRunSeq%line(i)%tok_type = ESMF_COMP_PHASE_TOKEN
        end if
      end if
    end do 

    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperProcessRunSeq
!BOP
! !IROUTINE: ESMF_MapperProcessRunSeq - Process the run sequence

! !INTERFACE:
  subroutine ESMF_MapperProcessRunSeq(runSeqCode, rc)
!
!
! !ARGUMENTS:
    character(len=ESMF_MAXSTR), dimension(:), intent(in) :: runSeqCode
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
!EOP
  !-----------------------------------------------------------------------------    
  ! Token types
!  integer, parameter :: ESMF_TIMELOOP_TOKEN = 1,&
!                        ESMF_COMP_PHASE_TOKEN=2,&
!                        ESMF_CONN_TOKEN=3,&
!                        ESMF_OTHER_TOKEN=4

!  integer, parameter :: ESMF_MAXTOKENSTR = 128
!  type ESMF_RunSeqTokenizedLine
!    character(len=ESMF_MAXTOKENSTR), dimension(:), allocatable :: tok
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

    integer :: nTimeLoops, nlines, i
    integer :: curLine
    type(ESMF_RunSeqTimeLoopInfo), dimension(:), allocatable :: timeLoopInfoStack
    integer :: timeLoopInfoStackTop = 0
    type(ESMF_RunSeqTokenizedCode) :: tokRunSeq
    

    call ESMF_MapperTokenizeRunSeq(runSeqCode, tokRunSeq, rc)
    !if (ESMF_LogFoundError(localrc, &
    !  ESMF_ERR_PASSTHRU, &
    !  ESMF_CONTEXT, rcToReturn=rc)) return

    nTimeLoops = 0
    nlines = size(tokRunSeq%line)
    do i=1,nlines
      if(tokRunSeq%line(i)%tok_type == ESMF_TIMELOOP_BEGIN_TOKEN) then
        nTimeLoops = nTimeLoops + 1
      end if
    end do 
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
          read(tokRunSeq%line(curLine)%tok(1), *)&
            timeLoopInfoStack(timeLoopInfoStackTop)%timeLoopTime
          timeLoopInfoStack(timeLoopInfoStackTop)%timeLoopCount = 1
          timeLoopInfoStack(timeLoopInfoStackTop)%runSeqLoopLineNum =&
            curLine 
        else
          ! We asssume that outer loops are multiples of inner loops
          read(tokRunSeq%line(curLine)%tok(1), *)&
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
      end if
      print *, "Running : ", trim(runSeqCode(curLine)), " # ", curLine
      curLine = curLine + 1
    end do 

    print *, "Number of time loops = ", nTimeLoops

    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------


end module ESMF_MapperRunSeqUtilMod
