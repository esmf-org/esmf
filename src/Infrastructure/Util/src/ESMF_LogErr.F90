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
#define ESMF_FILENAME "ESMF_LogErr.F90"
!==============================================================================
!
!     ESMF LogErr module
module ESMF_LogErrMod
!
!==============================================================================
!
! This file contains the LogErr class definition and all LogErr class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!! this should be including ESMF.h, but since it also includes the cover
!! log macros, it can't be included here.  so just include each file
!! individually.  If we add files to ESMF.h they also need to be added here.
#include "ESMF_LogMacros.inc"
#include "ESMF_Macros.inc"
#include "ESMF_Conf.inc"
#include "ESMF_InitMacros.inc"
#include "ESMF_LogConstants.inc"
#include "ESMF_ErrReturnCodes.inc"

!BOPI
!============================================================================
! !MODULE: Fortran Interface to Log class.
!
! !DESCRIPTION:
!
! The Fortran interface to the {\tt ESMF\_Log} class is written in both
! Fortran and C/C++.
! This file contains the interface code written in Fortran.  It also contains
! some utility functions used by the {\tt ESMF\_Log} class.
!
!------------------------------------------------------------------------------
! !USES:
    ! inherit from ESMF base class
      use ESMF_IOUtilMod
      use ESMF_UtilStringMod
      use ESMF_UtilTypesMod
 !!  use ESMF_InitMacrosMod Commented out to prevent circular dependency
 !!                         this is possible because since all the checks
 !!                         in this module are shallow - Bob 1/9/2007.

implicit none
private

!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!------------------------------------------------------------------------------

!     ! ESMF_LogMsg_Flag
type ESMF_LogMsg_Flag
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
    integer      :: mtype
end type

!     ! Msg Types -  keep in sync with ESMC_LogMsgType_Flag
type(ESMF_LogMsg_Flag), parameter           :: &
    ESMF_LOGMSG_INFO  =   ESMF_LogMsg_Flag(1), &
    ESMF_LOGMSG_WARNING = ESMF_LogMsg_Flag(2), &
    ESMF_LOGMSG_ERROR =   ESMF_LogMsg_Flag(3), &
    ESMF_LOGMSG_TRACE =   ESMF_LogMsg_Flag(4), &
    ESMF_LOGMSG_DEBUG =   ESMF_LogMsg_Flag(5), &
    ESMF_LOGMSG_JSON  =   ESMF_LogMsg_Flag(6)

character(8), parameter ::  &
    ESMF_LogMsgString(6) = (/ &
      'INFO    ', &
      'WARNING ', &
      'ERROR   ', &
      'TRACE   ', &
      'DEBUG   ', &
      'JSON    '  &
    /)

type(ESMF_LogMsg_Flag), parameter :: &
    ESMF_LOGMSG_ALL(6) = (/ &
      ESMF_LOGMSG_INFO,     &
      ESMF_LOGMSG_WARNING,  &
      ESMF_LOGMSG_ERROR,    &
      ESMF_LOGMSG_TRACE,    &
      ESMF_LOGMSG_DEBUG,    &
      ESMF_LOGMSG_JSON      &
    /)

#if !defined (ESMF_PGI_NAMEDCONSTANT_BUG)
integer, private :: i_ac
type(ESMF_LogMsg_Flag), parameter :: &
    ESMF_LOGMSG_NONE(0) = (/ (ESMF_LogMsg_Flag(0), i_ac=1,0) /)
#else
type(ESMF_LogMsg_Flag) :: &
    ESMF_LOGMSG_NONE(0)
#endif

type(ESMF_LogMsg_Flag), parameter :: &
   ESMF_LOGMSG_NOTRACE(5) = (/ &
      ESMF_LOGMSG_INFO,     &
      ESMF_LOGMSG_WARNING,  &
      ESMF_LOGMSG_ERROR,    &
      ESMF_LOGMSG_DEBUG,    &
      ESMF_LOGMSG_JSON      &
    /)

!     ! ESMF_LogKind_Flag
type ESMF_LogKind_Flag
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
    integer      :: ftype
end type

!     ! Log Types - keep in sync with ESMC_LogKind_Flag
type(ESMF_LogKind_Flag), parameter :: &
    ESMF_LOGKIND_SINGLE = ESMF_LogKind_Flag(1), &
    ESMF_LOGKIND_MULTI = ESMF_LogKind_Flag(2),  &
    ESMF_LOGKIND_MULTI_ON_ERROR = ESMF_LogKind_Flag(3),  &
    ESMF_LOGKIND_NONE = ESMF_LogKind_Flag(4)

!     ! Log Entry
type ESMF_LogEntry
    private
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
    real(ESMF_KIND_R8)  ::  highResTimestamp
    logical             ::  highResTimestampFlag
    integer             ::  indentCount
    integer             ::  h,m,s,ms
    integer             ::  line
    logical             ::  noprefix
    logical             ::  methodflag,lineflag,fileflag
    character, pointer  ::  msg(:)
    character(len=ESMF_MAXPATHLEN) ::  file
    character(len=32)   ::  method
    character(len=8)    ::  d
    character(len=16)   ::  lt
    ESMF_INIT_DECLARE
end type ESMF_LogEntry

type ESMF_Log
    private
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
#ifndef ESMF_NO_INITIALIZERS
    integer                                         ::  logTableIndex = 0
#else
    integer                                         ::  logTableIndex
#endif
    ESMF_INIT_DECLARE
end type ESMF_Log

type ESMF_LogPrivate
    private
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
    integer                                         ::  maxElements
    integer                                         ::  fIndex
    integer                                         ::  unitNumber
    integer                                         ::  petNumber
    logical                                         ::  stopprogram
    logical                                         ::  pad ! memory alignment
    type(ESMF_Logical)                              ::  flushImmediately
    type(ESMF_Logical)                              ::  flushed
    type(ESMF_Logical)                              ::  dirty
    type(ESMF_LogKind_Flag)                         ::  logkindflag
#ifndef ESMF_NO_INITIALIZERS
    type(ESMF_LogEntry), dimension(:),pointer       ::  LOG_ENTRY   => null ()
    type(ESMF_Logical)                              ::  FileIsOpen  = ESMF_FALSE
    integer                                         ::  errorMaskCount= 0
    integer, pointer                                ::  errorMask(:)  => null ()
    type(ESMF_LogMsg_Flag), pointer                 ::  logmsgList(:) => null ()
    type(ESMF_LogMsg_Flag), pointer                 ::  logmsgAbort(:)=> null ()
    logical                                         ::  traceFlag = .false.
    logical                                         ::  highResTimestampFlag = .false.
    logical                                         ::  appendFlag = .true.
    integer                                         ::  indentCount = 0
    logical                                         ::  deferredOpenFlag = .false.
    logical                                         ::  noprefix = .false.
#else
    type(ESMF_LogEntry), dimension(:),pointer       ::  LOG_ENTRY
    type(ESMF_Logical)                              ::  FileIsOpen
    integer                                         ::  errorMaskCount
    integer, dimension(:), pointer                  ::  errorMask(:)
    type(ESMF_LogMsg_Flag), pointer                 ::  logmsgList(:)
    type(ESMF_LogMsg_Flag), pointer                 ::  logmsgAbort(:)
    logical                                         ::  traceFlag
    logical                                         ::  highResTimestampFlag
    integer                                         ::  indentCount
    logical                                         ::  appendflag
    logical                                         ::  deferredOpenFlag
    logical                                         ::  noprefix
#endif
    character(len=ESMF_MAXPATHLEN)                  ::  nameLogErrFile
    character(len=ESMF_MAXSTR)                      ::  petNumLabel
    ESMF_INIT_DECLARE
end type ESMF_LogPrivate

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
    public ESMF_LogKind_Flag

    public ESMF_LOGMSG_INFO
    public ESMF_LOGMSG_WARNING
    public ESMF_LOGMSG_ERROR
    public ESMF_LOGMSG_TRACE
    public ESMF_LOGMSG_DEBUG
    public ESMF_LOGMSG_JSON
    public ESMF_LOGMSG_ALL
    public ESMF_LOGMSG_NONE
    public ESMF_LOGMSG_NOTRACE
    public ESMF_LOGKIND_SINGLE
    public ESMF_LOGKIND_MULTI
    public ESMF_LOGKIND_MULTI_ON_ERROR
    public ESMF_LOGKIND_NONE

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
   public ESMF_Log
   public ESMF_LogInit
   public ESMF_LogGetInit
   public ESMF_LogValidate
   public ESMF_LogClose
   public ESMF_LogFinalize
   public ESMF_LogFlush
   public ESMF_LogFoundAllocError
   public ESMF_LogFoundDeallocError
   public ESMF_LogFoundError
   public ESMF_LogFoundNetCDFError
   public ESMF_LogGet
   public ESMF_LogInitialize
   public ESMF_LogOpen
   public ESMF_LogRc2Msg
   public ESMF_LogSet
   public ESMF_LogSetError
   public ESMF_LogWrite
   public ESMF_LogMsg_Flag

!  Overloaded = operator functions
   public :: operator(==), operator(/=), operator(>)

! overload == and > with additional derived types so you can compare
!  them as if they were simple integers.


interface operator (==)
   module procedure ESMF_LogEQ
   module procedure ESMF_lmteq
   module procedure ESMF_llteq
end interface

interface operator (/=)
   module procedure ESMF_LogNE
   module procedure ESMF_lltne
end interface

interface operator (>)
   module procedure ESMF_lmtgt
end interface

! Additional overloaded methods

interface ESMF_LogOpen
  module procedure ESMF_LogOpen
  module procedure ESMF_LogOpenDefault
end interface
!EOPI

type(ESMF_Log),SAVE,target::ESMF_LogDefault
integer, parameter :: ESMF_LogTableMax=1000            ! Max # of files allowed to open
type(ESMF_LogPrivate),SAVE,target :: ESMF_LogTable(ESMF_LogTableMax) ! Users files
integer,SAVE :: ESMF_LogTableCount=0                   ! count users' number of files


!----------------------------------------------------------------------------

contains


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_LogAssignment(=) - Log assignment
!
! !INTERFACE:
!   interface assignment(=)
!   log1 = log2
!
! !ARGUMENTS:
!   type(ESMF_Log) :: log1
!   type(ESMF_Log) :: log2
!
!
! !DESCRIPTION:
!   Assign {\tt log1} as an alias to the same {\tt ESMF\_Log} object in memory
!   as {\tt log2}. If {\tt log2} is invalid, then {\tt log1} will be
!   equally invalid after the assignment.
!
!   The arguments are:
!   \begin{description}
!   \item[log1]
!     The {\tt ESMF\_Log} object on the left hand side of the assignment.
!   \item[log2]
!     The {\tt ESMF\_Log} object on the right hand side of the assignment.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------

! IMPLEMENTATION NOTE:
! Use the default Fortran assignment

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_LogOperator(==) - Test if Log 1 is equivalent to Log 2
!
! !INTERFACE:
!     interface operator(==)
!     if (log1 == log2) then ... endif
!                  OR
!     result = (log1 == log2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_Log), intent(in) :: log1
!     type(ESMF_Log), intent(in) :: log2
!
!
! !DESCRIPTION:
!     Overloads the (==) operator for the {\tt ESMF\_Log} class.
!     Compare two logs for equality; return {\tt .true.} if equal,
!     {\tt .false.} otherwise. Comparison is based on whether the objects
!     are distinct, as with two newly created logs, or are simply aliases
!     to the same log as would be the case when assignment was involved.
!
!     The arguments are:
!     \begin{description}
!     \item[log1]
!          The {\tt ESMF\_Log} object on the left hand side of the equality
!          operation.
!     \item[log2]
!          The {\tt ESMF\_Log} object on the right hand side of the equality
!          operation.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_LogOperator(/=) - Test if Log 1 is not equivalent to Log 2
!
! !INTERFACE:
!     interface operator(/=)
!     if (log1 /= log2) then ... endif
!                  OR
!     result = (log1 /= log2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_Log), intent(in) :: log1
!     type(ESMF_Log), intent(in) :: log2
!
!
! !DESCRIPTION:
!     Overloads the (/=) operator for the {\tt ESMF\_Log} class.
!     Compare two logs for inequality; return {\tt .true.} if equal,
!     {\tt .false.} otherwise.  Comparison is based on whether the objects
!     are distinct, as with two newly created logs, or are simply aliases
!     to the same log as would be the case when assignment was involved.
!
!     The arguments are:
!     \begin{description}
!     \item[log1]
!          The {\tt ESMF\_Log} object on the left hand side of the non-equality
!          operation.
!     \item[log2]
!          The {\tt ESMF\_Log} object on the right hand side of the non-equality
!          operation.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogGetInit()"
!BOPI
! !IROUTINE:  ESMF_LogGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_LogGetInit(s)
!
! !RETURN VALUE:
       ESMF_INIT_TYPE :: ESMF_LogGetInit
!
! !ARGUMENTS:
       type(ESMF_Log), intent(in), optional :: s
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt log}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Log} from which to retrieve status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_LogGetInit = ESMF_INIT_GET(s)
       else
         ESMF_LogGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_LogGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogInit()"
!BOPI
! !IROUTINE:  ESMF_LogInit - Initialize Log

! !INTERFACE:
    subroutine ESMF_LogInit(s)
!
! !ARGUMENTS:
       type(ESMF_Log) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt log}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Log} of which being initialized.
!     \end{description}
!
!EOPI
       s%logTableIndex = 0
       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_LogInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogValidate()"
!BOPI
! !IROUTINE:  ESMF_LogValidate - Check validity of a Log

! !INTERFACE:
    subroutine ESMF_LogValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_Log), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt Log} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Log} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt localfield}
!           is valid.
!     \end{description}
!
!EOPI
    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,s)

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when implementing this method
    if (s%logTableIndex==s%logTableIndex) continue

    ! return success
    if(present(rc)) then
      rc = ESMF_SUCCESS
    endif
  end subroutine ESMF_LogValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogPrivateGetInit()"
!BOPI
! !IROUTINE:  ESMF_LogPrivateGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_LogPrivateGetInit(s)
!
! !RETURN VALUE:
       ESMF_INIT_TYPE :: ESMF_LogPrivateGetInit
!
! !ARGUMENTS:
       type(ESMF_LogPrivate), intent(in), optional :: s
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt logprivate}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LogPrivate} from which to retrieve status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_LogPrivateGetInit = ESMF_INIT_GET(s)
       else
         ESMF_LogPrivateGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_LogPrivateGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogPrivateInit()"
!BOPI
! !IROUTINE:  ESMF_LogPrivateInit - Initialize Log

! !INTERFACE:
    subroutine ESMF_LogPrivateInit(s)
!
! !ARGUMENTS:
       type(ESMF_LogPrivate) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt LogPrivate}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LogPrivate} of which being initialized.
!     \end{description}
!
!EOPI
       nullify(s%LOG_ENTRY)
       s%FileIsOpen=ESMF_False
!       s%errorMask(:)=>Null()
       nullify(s%errorMask)
       s%errorMaskCount=0
       s%logmsgList => null ()
       s%traceFlag = .false.
       s%deferredOpenFlag = .false.
       s%appendFlag = .true.
       s%noprefix = .false.
       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_LogPrivateInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogPrivateValidate()"
!BOPI
! !IROUTINE:  ESMF_LogPrivateValidate - Check validity of a LogPrivate

! !INTERFACE:
    subroutine ESMF_LogPrivateValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_LogPrivate), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt LogPrivate} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LogPrivate} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt localfield}
!           is valid.
!     \end{description}
!
!EOPI
    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogPrivateGetInit,ESMF_LogPrivateInit,s)

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when implementing this method
    if (s%maxElements==s%maxElements) continue

    ! return success
    if(present(rc)) then
      rc = ESMF_SUCCESS
    endif
  end subroutine ESMF_LogPrivateValidate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogEntryGetInit()"
!BOPI
! !IROUTINE:  ESMF_LogEntryGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_LogEntryGetInit(s)
!
! !RETURN VALUE:
       ESMF_INIT_TYPE :: ESMF_LogEntryGetInit
!
! !ARGUMENTS:
       type(ESMF_LogEntry), intent(in), optional :: s
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt LogEntry}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LogEntry} from which to retrieve status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_LogEntryGetInit = ESMF_INIT_GET(s)
       else
         ESMF_LogEntryGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_LogEntryGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogEntryInit()"
!BOPI
! !IROUTINE:  ESMF_LogEntryInit - Initialize LogEntry

! !INTERFACE:
    subroutine ESMF_LogEntryInit(s)
!
! !ARGUMENTS:
       type(ESMF_LogEntry) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt logentry}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LogEntry} of which being initialized.
!     \end{description}
!
!EOPI
       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_LogEntryInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogEntryValidate()"
!BOPI
! !IROUTINE:  ESMF_LogEntryValidate - Check validity of a LogEntry

! !INTERFACE:
    subroutine ESMF_LogEntryValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_LogEntry), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt LogEntry} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LogEntry} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt logentry}
!           is valid.
!     \end{description}
!
!EOPI
    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogEntryGetInit,ESMF_LogEntryInit,s)

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when implementing this method
    if (s%h==s%h) continue

    ! return success
    if(present(rc)) then
      rc = ESMF_SUCCESS
    endif
  end subroutine ESMF_LogEntryValidate

!------------------------------------------------------------------------------
! functions to compare two types to see if they're the same or not

function ESMF_lmteq(mt1, mt2)
  logical ESMF_lmteq
  type(ESMF_LogMsg_Flag), intent(in) :: mt1,mt2

    ESMF_lmteq = (mt1%mtype == mt2%mtype)
end function

function ESMF_llteq(lt1, lt2)
  logical ESMF_llteq
  type(ESMF_LogKind_Flag), intent(in) :: lt1,lt2

    ESMF_llteq = (lt1%ftype == lt2%ftype)
end function

function ESMF_lltne(lt1, lt2)
  logical ESMF_lltne
  type(ESMF_LogKind_Flag), intent(in) :: lt1,lt2

    ESMF_lltne = (lt1%ftype /= lt2%ftype)
end function

function ESMF_lmtgt(mt1, mt2)
  logical ESMF_lmtgt
  type(ESMF_LogMsg_Flag), intent(in) :: mt1,mt2

    ESMF_lmtgt = (mt1%mtype > mt2%mtype)
end function


!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogClose()"
!BOP
! !IROUTINE: ESMF_LogClose - Close Log file(s)

! !INTERFACE:
      subroutine ESMF_LogClose(log, keywordEnforcer, rc)
!
! !ARGUMENTS:
      type(ESMF_Log), intent(inout), optional :: log
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,        intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!      This routine closes the log file(s) associated with {\tt log}.
!      If the log is not explicitly closed, it will be closed by
!      {\tt ESMF\_Finalize}.
!
!      The arguments are:
!      \begin{description}
!
!      \item [{[log]}]
!            An {\tt ESMF\_Log} object.  If not specified, the default log is closed.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!EOP

    integer::rc2,status
    type(ESMF_LogPrivate),pointer     :: alog

    ! Initialize return code; assume routine not implemented
    if (present(rc)) then
      rc=ESMF_FAILURE
    endif

    alog => null ()
    if (present(log)) then
      if (log%logTableIndex > 0) then
         alog => ESMF_LogTable(log%logTableIndex)
      endif
    else
      if (ESMF_LogDefault%logTableIndex > 0) then
        alog => ESMF_LogTable(ESMF_LogDefault%logTableIndex)
      end if
    endif

    ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)

    if(associated (alog)) then
      ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogPrivateGetInit,ESMF_LogPrivateInit,alog)

      if (alog%logkindflag /= ESMF_LOGKIND_NONE) then
        if (alog%FileIsOpen == ESMF_TRUE) then
          call ESMF_LogFlush(log,rc=rc2)
          CLOSE (UNIT=alog%unitNumber)
          alog%FileIsOpen=ESMF_FALSE
          deallocate (alog%LOG_ENTRY,stat=status)
        endif
      endif

      if (alog%errorMaskCount > 0) then
        deallocate(alog%errorMask)
      endif
      if (associated (alog%logmsgList)) then
        deallocate (alog%logmsgList)
      end if
      if (associated (alog%logmsgAbort)) then
        deallocate (alog%logmsgAbort)
      end if
    endif

    if (present(rc)) then
      rc=ESMF_SUCCESS
    endif

end subroutine ESMF_LogClose

!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogFinalize()"
!BOPI
! !IROUTINE: ESMF_LogFinalize - Finalize Log file(s)

! !INTERFACE:
      subroutine ESMF_LogFinalize(rc)
!
! !ARGUMENTS:
      integer, intent(out), optional  :: rc

! !DESCRIPTION:
!      This routine finalizes the global Log.  The default Log will be flushed
!      and closed.  All user logs will also be closed.
!
!      The arguments are:
!      \begin{description}
!
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!EOPI

        integer :: rc2,k
        type(ESMF_Log)                                    :: log

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)

        ! Loop through all ESMF_LogTable(*) and close the files
        do k = 1,ESMF_LogTableCount
          log%logTableIndex = k
          call ESMF_LogClose(log, rc=rc)
        enddo

        call c_ESMC_LogFinalize(rc2)

end subroutine ESMF_LogFinalize

!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogFlush()"
!BOP
! !IROUTINE: ESMF_LogFlush - Flush the Log file(s)

! !INTERFACE:
      subroutine ESMF_LogFlush(log, keywordEnforcer, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Log), intent(inout), optional :: log
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,        intent(out),   optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!      This subroutine flushes the file buffer associated with {\tt log}.
!
!      The arguments are:
!      \begin{description}
!
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!            of the default Log.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!EOP
    integer                         :: j
    type(ESMF_LogPrivate),pointer   :: alog
    integer                         :: localrc, localrc2
    integer                         :: memstat
    logical                         :: spaceflag

    ! Initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) then
      rc=localrc
    endif

    ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)

    nullify(alog) ! ensure that the association status is well defined

    if (present(log)) then
      if (log%logTableIndex > 0) then
         alog => ESMF_LogTable(log%logTableIndex)
      else
         localrc = ESMF_RC_OBJ_NOT_CREATED
      endif
    else
      if (ESMF_LogDefault%logTableIndex > 0) then
        alog => ESMF_LogTable(ESMF_LogDefault%logTableIndex)
      end if
    endif

    if (associated(alog)) then

      ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogPrivateGetInit,ESMF_LogPrivateInit,alog)

      if (alog%FileIsOpen /= ESMF_TRUE) then
        if (present (log)) then
          write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
              ": ESMF_Log not open -- cannot ESMF_LogFlush()."
          call ESMF_UtilIOUnitFlush(ESMF_UtilIOStderr, rc=rc)
          if (present (rc))  &
            rc=ESMF_RC_FILE_OPEN
        else
          if (present (rc))  &
            rc=ESMF_SUCCESS
        end if
        return
      endif
      if ((alog%FileIsOpen == ESMF_TRUE) .AND. &
          (alog%flushed == ESMF_FALSE) .AND. &
          (alog%dirty == ESMF_TRUE))  then
        do j=1, alog%fIndex-1
          if (.not. alog%LOG_ENTRY(j)%noprefix) then
            write (alog%unitNumber, '(2a,3i2.2,a,i3.3,5a)', advance='no')  &
                alog%LOG_ENTRY(j)%d, " ", &
                alog%LOG_ENTRY(j)%h, &
                alog%LOG_ENTRY(j)%m, &
                alog%LOG_ENTRY(j)%s, ".", &
                alog%LOG_ENTRY(j)%ms, " ", &
                alog%LOG_ENTRY(j)%lt, " ", &
                trim(alog%petNumLabel), " "

            if (alog%LOG_ENTRY(j)%highResTimestampFlag) then
              write (alog%unitNumber, '(f18.6,1x)', advance='no') alog%LOG_ENTRY(j)%highResTimestamp
            end if
          end if ! noprefix

          spaceflag = .false.
          if (alog%LOG_ENTRY(j)%fileflag) then
            write (alog%unitNumber, '(a)',  advance='no')  &
                trim(alog%LOG_ENTRY(j)%file)
            spaceflag = .true.
          end if
          if (alog%LOG_ENTRY(j)%lineflag) then
            write (alog%unitNumber, '(a,i0)',  advance='no')  &
                ':', alog%LOG_ENTRY(j)%line
            spaceflag = .true.
          end if
          if (alog%LOG_ENTRY(j)%methodflag) then
            write (alog%unitNumber, '(1x,a)',  advance='no')  &
                trim(alog%LOG_ENTRY(j)%method)
            spaceflag = .true.
          end if
          if (spaceflag) then
            write (alog%unitNumber, '(a)',  advance='no') ' '
          end if

          write (alog%unitNumber, '(a)',  advance='no')  &
              trim (ESMF_UtilArray2String (alog%LOG_ENTRY(j)%msg))
          deallocate (alog%LOG_ENTRY(j)%msg, stat=memstat)
          if (memstat /= 0) then
            write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
                ": Deallocation error."
            call ESMF_UtilIOUnitFlush(ESMF_UtilIOStderr, rc=rc)
            localrc = ESMF_RC_MEM_DEALLOCATE
            if (present (rc)) then
              rc = localrc
            end if
            return
          end if

          write (alog%unitNumber, *)

        end do
      end if
      localrc = ESMF_SUCCESS

      alog%fIndex = 1

      call ESMF_UtilIOUnitFlush (alog%unitNumber, rc=localrc2)
      if (localrc2 /= ESMF_SUCCESS) then
        write (ESMF_UtilIOStderr,*) 'unit flush failed, rc =', localrc2
        call ESMF_UtilIOUnitFlush(ESMF_UtilIOStderr, rc=rc)
        localrc = localrc2
      end if

      alog%flushed = ESMF_TRUE
      alog%dirty = ESMF_FALSE

    endif

    if (present (rc)) then
      rc = localrc
    end if

end subroutine ESMF_LogFlush

!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogFoundAllocError()"
!BOP
! !IROUTINE: ESMF_LogFoundAllocError - Check Fortran allocation status error and write message

! !INTERFACE:
      function ESMF_LogFoundAllocError(statusToCheck, keywordEnforcer,  &
                                       msg,line,file, &
                                       method,rcToReturn,log)
!
! !RETURN VALUE:
      logical                                    :: ESMF_LogFoundAllocError
!
! !ARGUMENTS:
!
      integer,          intent(in)              :: statusToCheck
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character(len=*), intent(in),    optional :: msg
      integer,          intent(in),    optional :: line
      character(len=*), intent(in),    optional :: file
      character(len=*), intent(in),    optional :: method
      integer,          intent(inout), optional :: rcToReturn
      type(ESMF_Log),   intent(inout), optional :: log

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!      This function returns {\tt .true.} when {\tt statusToCheck} indicates
!      an allocation error, otherwise it returns {\tt .false.}.  The status
!      value is typically returned from a Fortran ALLOCATE statement.
!      If an error is indicated, a ESMF memory allocation error message
!      will be written to the {\tt ESMF\_Log} along with a user added {\tt msg},
!      {\tt line}, {\tt file} and {\tt method}.
!
!      The arguments are:
!      \begin{description}
!
!      \item [statusToCheck]
!            Fortran allocation status to check.  Fortran specifies
!            that a status of 0 (zero) indicates success.
!      \item [{[msg]}]
!            User-provided message string.
!      \item [{[line]}]
!            Integer source line number.  Expected to be set by
!            using the preprocessor {\tt \_\_LINE\_\_} macro.
!      \item [{[file]}]
!            User-provided source file name.
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[rcToReturn]}]
!            If specified, when the allocation status indicates an error,
!            set the {\tt rcToReturn} value to {\tt ESMF\_RC\_MEM}.  Otherwise,
!            {\tt rcToReturn} is not modified.
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!            of the default Log.
!
!      \end{description}
!
!EOP
    character(len=ESMF_MAXSTR)::allocmsg
    integer::msglen
    type(ESMF_LogPrivate), pointer  :: alog

    ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)
    ESMF_LogFoundAllocError=.FALSE.

    nullify(alog) ! ensure that the association status is well defined

    if (present(log)) then
      if (log%logTableIndex > 0) then
         alog => ESMF_LogTable(log%logTableIndex)
      endif
    else
      if (ESMF_LogDefault%logTableIndex > 0) then
        alog => ESMF_LogTable(ESMF_LogDefault%logTableIndex)
      end if
    endif

    if (alog%traceFlag) then
      call ESMF_LogWrite ('called: ' // ESMF_METHOD, ESMF_LOGMSG_TRACE,  &
        line=line, file=file, method=method, log=log)
    end if

!   The Fortran Standard requires that a successful allocate return a stat value
!   of 0.  Any other value indicates a processor-defined error.
    if (statusToCheck .NE. 0) then
        call ESMF_Breakpoint()  ! no-op to assist debugging
        call ESMF_LogRc2Msg (ESMF_RC_MEM_ALLOCATE, allocmsg, msglen)
        if (present(rcToReturn)) then
            rcToReturn=ESMF_RC_MEM_ALLOCATE
        endif
        if (present(msg)) then
          allocmsg = allocmsg(:msglen) // " - " // msg // ' (status = '
          write (allocmsg,'(a,i5,a)') trim (allocmsg), statusToCheck, ')'
          msglen = len_trim (allocmsg)
        end if
        call ESMF_LogWrite(allocmsg(:msglen), ESMF_LOGMSG_ERROR,  &
            line=line, file=file, method=method, log=log)
        ESMF_LogFoundAllocError=.TRUE.
    endif

end function ESMF_LogFoundAllocError

!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogFoundDeallocError()"
!BOP
! !IROUTINE: ESMF_LogFoundDeallocError - Check Fortran deallocation status error and write message

! !INTERFACE:
      function ESMF_LogFoundDeallocError(statusToCheck, keywordEnforcer,  &
                                         msg,line,file, &
                                         method,rcToReturn,log)
!
! !RETURN VALUE:
      logical ::ESMF_LogFoundDeallocError
!
! !ARGUMENTS:
!
      integer,          intent(in)              :: statusToCheck
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character(len=*), intent(in),    optional :: msg
      integer,          intent(in),    optional :: line
      character(len=*), intent(in),    optional :: file
      character(len=*), intent(in),    optional :: method
      integer,          intent(inout), optional :: rcToReturn
      type(ESMF_Log),   intent(inout), optional :: log

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!      This function returns {\tt .true.} when {\tt statusToCheck} indicates
!      a deallocation error, otherwise it returns {\tt .false.}.  The status
!      value is typically returned from a Fortran DEALLOCATE statement.
!      If an error is indicated, a ESMF memory allocation error message
!      will be written to the {\tt ESMF\_Log} along with a user added {\tt msg},
!      {\tt line}, {\tt file} and {\tt method}.
!
!      The arguments are:
!      \begin{description}
!
!      \item [statusToCheck]
!            Fortran deallocation status to check.  Fortran specifies
!            that a status of 0 (zero) indicates success.
!      \item [{[msg]}]
!            User-provided message string.
!      \item [{[line]}]
!            Integer source line number.  Expected to be set by
!            using the preprocessor {\tt \_\_LINE\_\_} macro.
!      \item [{[file]}]
!            User-provided source file name.
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[rcToReturn]}]
!            If specified, when the deallocation status indicates an error,
!            set the {\tt rcToReturn} value to {\tt ESMF\_RC\_MEM}.  Otherwise,
!            {\tt rcToReturn} is not modified.
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!            of the default Log.
!
!      \end{description}
!
!EOP
    character(len=ESMF_MAXSTR)::allocmsg
    integer::msglen
    type(ESMF_LogPrivate), pointer  :: alog

    ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)
    ESMF_LogFoundDeallocError=.FALSE.

    nullify(alog) ! ensure that the association status is well defined

    if (present(log)) then
      if (log%logTableIndex > 0) then
         alog => ESMF_LogTable(log%logTableIndex)
      endif
    else
      if (ESMF_LogDefault%logTableIndex > 0) then
        alog => ESMF_LogTable(ESMF_LogDefault%logTableIndex)
      end if
    endif

    if (alog%traceFlag) then
      call ESMF_LogWrite ('called: ' // ESMF_METHOD, ESMF_LOGMSG_TRACE,  &
        line=line, file=file, method=method, log=log)
    end if

!   The Fortran Standard requires that a successful deallocate return a stat value
!   of 0.  Any other value indicates a processor-defined error.
    if (statusToCheck .NE. 0) then
        call ESMF_Breakpoint()  ! no-op to assist debugging
        call ESMF_LogRc2Msg (ESMF_RC_MEM_DEALLOCATE, allocmsg, msglen)
        if (present(rcToReturn)) then
            rcToReturn=ESMF_RC_MEM_DEALLOCATE
        endif
        if (present(msg)) then
          allocmsg = allocmsg(:msglen) // " - " // msg
          msglen = len_trim (allocmsg)
        end if
        call ESMF_LogWrite(allocmsg(:msglen), ESMF_LOGMSG_ERROR,  &
            line=line, file=file, method=method, log=log)
        ESMF_LogFoundDeallocError=.TRUE.
    endif

end function ESMF_LogFoundDeallocError

!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogFoundError()"
!BOP
! !IROUTINE: ESMF_LogFoundError - Check ESMF return code for error and write message

! !INTERFACE:
  recursive function ESMF_LogFoundError(rcToCheck,  keywordEnforcer,  &
                                  msg, line, file, method, &
                                  rcToReturn, log) result (LogFoundError)
!
! !RETURN VALUE:
      logical :: LogFoundError
!
! !ARGUMENTS:
!
      integer,          intent(in),    optional :: rcToCheck
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character(len=*), intent(in),    optional :: msg
      integer,          intent(in),    optional :: line
      character(len=*), intent(in),    optional :: file
      character(len=*), intent(in),    optional :: method
      integer,          intent(inout), optional :: rcToReturn
      type(ESMF_Log),   intent(inout), optional :: log

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!      This function returns {\tt .true.} when {\tt rcToCheck} indicates
!      an return code other than {\tt ESMF\_SUCCESS}, otherwise it returns
!      {\tt .false.}.
!      If an error is indicated, a ESMF predefined error message
!      will be written to the {\tt ESMF\_Log} along with a user added {\tt msg},
!      {\tt line}, {\tt file} and {\tt method}.
!
!      The arguments are:
!      \begin{description}
!
!      \item [{[rcToCheck]}]
!            Return code to check. Default is {\tt ESMF\_SUCCESS}.
!      \item [{[msg]}]
!            User-provided message string.
!      \item [{[line]}]
!            Integer source line number.  Expected to be set by
!            using the preprocessor {\tt \_\_LINE\_\_} macro.
!      \item [{[file]}]
!            User-provided source file name.
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[rcToReturn]}]
!            If specified, when {\tt rcToCheck} indicates an error,
!            set the {\tt rcToReturn} to the value of {\tt rcToCheck}.
!            Otherwise, {\tt rcToReturn} is not modified.
!            This is not the return code for this function; it allows
!            the calling code to do an assignment of the error code
!            at the same time it is testing the value.
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!            of the default Log.
!
!      \end{description}
!
!EOP

    integer:: rcToCheckInternal
    integer:: i
    logical:: masked
    type(ESMF_LogPrivate), pointer          :: alog
    character(len=ESMF_MAXSTR) :: errmsg
    integer :: msglen

    ! set default return
    LogFoundError = .FALSE.

    if (.not.present(rcToCheck)) then
      rcToCheckInternal = ESMF_SUCCESS
    else
      rcToCheckInternal = rcToCheck
    endif

    ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)

    nullify(alog) ! ensure that the association status is well defined

    if (present(log)) then
      if (log%logTableIndex > 0) then
         alog => ESMF_LogTable(log%logTableIndex)
      endif
    else
      if (ESMF_LogDefault%logTableIndex > 0) then
        alog => ESMF_LogTable(ESMF_LogDefault%logTableIndex)
      end if
    endif

    if (associated(alog)) then

      ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogPrivateGetInit,ESMF_LogPrivateInit,alog)

      if (alog%traceFlag) then
        call ESMF_LogWrite ('called: ' // ESMF_METHOD, ESMF_LOGMSG_TRACE,  &
          line=line, file=file, method=method, log=log)
      end if

      ! check the error code
      if (rcToCheckInternal /= ESMF_SUCCESS) then
        masked = .false.
        do i=1, alog%errorMaskCount
          if (alog%errorMask(i) == rcToCheckInternal) masked = .true.
        enddo
        if (.not.masked) then
          call ESMF_Breakpoint()  ! no-op to assist debugging
          call ESMF_LogRc2Msg (rcToCheckInternal, errmsg, msglen)
          if (present(msg)) then
            errmsg = errmsg(:msglen) // " - " // msg
            msglen = len_trim (errmsg)
          end if
          call ESMF_LogWrite(errmsg(:msglen), ESMF_LOGMSG_ERROR,  &
              line=line, file=file, method=method, log=log)
          LogFoundError=.TRUE.
          if (present(rcToReturn)) rcToReturn = rcToCheckInternal
        endif
      endif
    else
      if (rcToCheckInternal /= ESMF_SUCCESS) then
        LogFoundError=.TRUE.
        if (present(rcToReturn)) rcToReturn = rcToCheckInternal
      end if
    endif

end function ESMF_LogFoundError

!-------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogFoundNetCDFError()"
!BOP
! !IROUTINE: ESMF_LogFoundNetCDFError - Check NetCDF status code for success or log the associated NetCDF error message.

! !INTERFACE:
function ESMF_LogFoundNetCDFError(ncerrToCheck, keywordEnforcer, msg, line, &
                                  file, method, rcToReturn, log)

#if defined ESMF_NETCDF
  use netcdf
#elif defined ESMF_PNETCDF
  use pnetcdf
#endif

!
! !RETURN VALUE:
  logical :: ESMF_LogFoundNetCDFError
!
! !ARGUMENTS:
!
  integer,          intent(in)              :: ncerrToCheck
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  character(len=*), intent(in),    optional :: msg
  integer,          intent(in),    optional :: line
  character(len=*), intent(in),    optional :: file
  character(len=*), intent(in),    optional :: method
  integer,          intent(inout), optional :: rcToReturn
  type(ESMF_Log),   intent(inout), optional :: log
!
! !DESCRIPTION:
!      This function returns {\tt .true.} when {\tt ncerrToCheck} indicates
!      an return code other than {\tt 0} (the success code from NetCDF Fortran)
!      or {\tt NF\_NOERR} (the success code for PNetCDF). Otherwise it returns
!      {\tt .false.}.
!      If an error is indicated, a predefined ESMF error message
!      will be written to the {\tt ESMF\_Log} along with a user added {\tt msg},
!      {\tt line}, {\tt file} and {\tt method}. The NetCDF string error
!      representation will also be logged.
!
!      The arguments are:
!      \begin{description}
!
!      \item [{[ncerrToCheck]}]
!            NetCDF error code to check.
!      \item [{[msg]}]
!            User-provided message string.
!      \item [{[line]}]
!            Integer source line number.  Expected to be set by using the
!            preprocessor {\tt \_\_LINE\_\_} macro.
!      \item [{[file]}]
!            User-provided source file name.
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[rcToReturn]}]
!            If specified, when {\tt ncerrToCheck} indicates an error,
!            set {\tt rcToReturn} to {\tt ESMF\_RC\_NETCDF\_ERROR}. The string
!            representation for the error code will be retrieved from the NetCDF
!            Fortran library and logged alongside any user-provided message
!            string.
!            Otherwise, {\tt rcToReturn} is not modified.
!            This is not the return code for this function; it allows the
!            calling code to do an assignment of the error code at the same time
!            it is testing the value.
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!            of the default Log.
!
!      \end{description}
!
!EOP

  character(len=80) :: ncMsg
  character(len=ESMF_MAXSTR) :: localMsg
  logical :: ele
  character(len=ESMF_MAXSTR) :: ncerrToCheckChar
  integer :: localRc

#ifdef ESMF_NETCDF
  integer, parameter :: ncNoError = 0
#endif

  ! ----------------------------------------------------------------------------

#if (defined ESMF_NETCDF || defined ESMF_PNETCDF)
  ! Check the NetCDF status code for an error designation. Set the return value
  ! for the found error flag.
#ifdef ESMF_NETCDF
  if (ncerrToCheck .eq. ncNoError) then
#else
  if (ncerrToCheck .eq. NF_NOERR) then
#endif
    ESMF_LogFoundNetCDFError = .false.
  else
    ESMF_LogFoundNetCDFError = .true.

    ! Retrieve the string error from the NetCDF library. Handle NetCDF and
    ! PNetCDF error strings.
#ifdef ESMF_NETCDF
    ncMsg = nf90_strerror(ncerrToCheck)
#else
    ncMsg = nfmpi_strerror(ncerrToCheck)
#endif

    ! Convert the NetCDF error code from integer to string.
    write(ncerrToCheckChar, "(I0)") ncerrToCheck

    ! Pick the string to log based on the presence of a user message.
    if (present(msg)) then
      ! The string to be logged to file if a user provided a message.
      localMsg = trim(msg) // ' (strerr=' // trim(ncMsg) // ', ncerrToCheck=' // trim(ncerrToCheckChar) // ')'
    else
      ! The default message with no user-provided message.
      localMsg = trim(ncMsg) // ' (ncerrToCheck=' // trim(ncerrToCheckChar) // ')'
    endif

    localRc = ESMF_RC_NETCDF_ERROR
  endif
#else
  ! If NetCDF is not compiled with ESMF, set the library not present flag.
  ESMF_LogFoundNetCDFError = .true.
  localRc = ESMF_RC_LIB_NOT_PRESENT
  localMsg = 'ESMF_NETCDF or ESMF_PNETCDF not defined when lib was compiled'
#endif

  ! ----------------------------------------------------------------------------

  ! Log the error to file if an error was found.
  if (ESMF_LogFoundNetCDFError) then
    ele = ESMF_LogFoundError(localRC, msg=trim(localMsg), line=line, &
      file=file, method=method, log=log, rcToReturn=rcToReturn)
    ! Update the return code if provided.
    if (present(rcToReturn)) then
      rcToReturn = localRc
    endif
  endif

end function ESMF_LogFoundNetCDFError

!-------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogGet()"
!BOP
! !IROUTINE: ESMF_LogGet - Return information about a log object

! !INTERFACE:
      subroutine ESMF_LogGet(log, keywordEnforcer,  &
                             flush,    &
                             logmsgAbort, logkindflag, &
                             maxElements, trace, fileName,  &
                             highResTimestampFlag, indentCount,  &
                             noPrefix, rc)
!
! !ARGUMENTS:
!
      type(ESMF_Log),          intent(in),  optional :: log
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      logical,                 intent(out), optional :: flush
      type(ESMF_LogMsg_Flag),  pointer,     optional :: logmsgAbort(:)
      type(ESMF_LogKind_Flag), intent(out), optional :: logkindflag
      integer,                 intent(out), optional :: maxElements
      logical,                 intent(out), optional :: trace
      character(*),            intent(out), optional :: fileName
      logical,                 intent(out), optional :: highResTimestampFlag
      integer,                 intent(out), optional :: indentCount
      logical,                 intent(out), optional :: noPrefix
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!      This subroutine returns properties about a Log object.
!
!      The arguments are:
!      \begin{description}
!
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!            of the default Log.
!      \item [{[flush]}]
!            Flush flag.
!      \item [{[logmsgAbort]}]
!            Returns an array containing current message halt settings.
!            If the array is not pre-allocated, {\tt ESMF\_LogGet} will
!            allocate an array of the correct size.  If no message types
!            are defined, an array of length zero is returned.  It is the
!            callers responsibility to deallocate the array.
!      \item [{[logkindflag]}]
!            Defines either single or multilog.
!      \item [{[maxElements]}]
!            Maximum number of elements in the Log.
!      \item [{[trace]}]
!            Current setting of the Log call tracing flag.
!      \item [{[fileName]}]
!            Current file name.  When the log has been opened with
!            {\tt ESMF\_LOGKIND\_MULTI}, the filename has a PET number
!            prefix.
!      \item [{[highResTimestampFlag]}]
!            Current setting of the extended elapsed timestamp flag.
!      \item [{[indentCount]}]
!            Current setting of the leading white space padding.
!      \item [{[noPrefix]}]
!            Current setting of the message prefix enable/disable flag.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!EOP

        type(ESMF_LogPrivate),pointer          :: alog
        integer :: localrc
        integer :: memstat
        integer :: lma_size

        ! Initialize return code; assume routine not implemented
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) then
          rc=localrc
        endif

        ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)

    nullify(alog) ! ensure that the association status is well defined

    if (present(log)) then
      if (log%logTableIndex > 0) then
         alog => ESMF_LogTable(log%logTableIndex)
      endif
    else
      if (ESMF_LogDefault%logTableIndex > 0) then
        alog => ESMF_LogTable(ESMF_LogDefault%logTableIndex)
      end if
    endif

    if (associated(alog)) then

      ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogPrivateGetInit,ESMF_LogPrivateInit,alog)

        if (present(flush)) then
          flush=alog%flushImmediately
        endif
        if (present(logkindflag)) then
          logkindflag=alog%logkindflag
        endif
        if (present(maxElements)) then
          maxElements=alog%maxElements
        endif
        if (present(trace)) then
          trace=alog%traceFlag
        endif
        if (present(fileName)) then
          fileName=alog%nameLogErrFile
          if (len (fileName) < len_trim (alog%nameLogErrFile)) then
            if (ESMF_LogFoundError (ESMF_RC_LONG_STR,   &
                msg='fileName argument string too short', &
                ESMF_CONTEXT, rcToReturn=rc)) return
          end if
        endif
        if (present (highResTimestampFlag)) then
          highResTimestampFlag = alog%highResTimestampFlag
        endif
        if (present (indentCount)) then
          indentCount = alog%indentCount
        endif
        if (present (noPrefix)) then
          noPrefix = alog%noPrefix
        endif

      ! Return an array with the current values.  If the user has not
      ! pre-allocated an array, do the allocation here.
        if (present(logmsgAbort)) then
          if (associated (alog%logmsgAbort)) then
            lma_size = size (alog%logmsgAbort)
          else
            lma_size = 0
          end if

          if (associated (logmsgAbort)) then
            if (size (logmsgAbort) < lma_size) then
              if (ESMF_LogFoundError (ESMF_RC_ARG_SIZE,   &
                  msg='logmsgAbort array size too small', &
                  ESMF_CONTEXT, rcToReturn=rc)) return
            end if
          else
            allocate (logmsgAbort(lma_size), stat=memstat)
              if (ESMF_LogFoundAllocError (memstat,   &
                  msg='allocating logmsgAbort array', &
                  ESMF_CONTEXT, rcToReturn=rc)) return
          end if

          if (associated (alog%logmsgAbort)) then
            logmsgAbort = alog%logmsgAbort(:lma_size)
          end if
        endif
    endif

    localrc = ESMF_SUCCESS
    if (present(rc)) then
      rc=localrc
    endif

end subroutine ESMF_LogGet

!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogInitialize()"
!BOPI
! !IROUTINE: ESMF_LogInitialize - Initialize Log file(s)

! !INTERFACE:
      subroutine ESMF_LogInitialize(filename, logappendflag, logkindflag, rc)
!
! !ARGUMENTS:
      character(len=*),        intent(in)           :: filename
      logical,                 intent(in), optional :: logappendflag
      type(ESMF_LogKind_Flag), intent(in), optional :: logkindflag
      integer,                 intent(out),optional :: rc

! !DESCRIPTION:
!      This routine initializes the global default {\tt ESMF\_Log}.
!      The default {\tt ESMF\_Log} is assigned the {\tt filename} and
!      is associated with an open Fortran unit number.
!
!      The arguments are:
!      \begin{description}
!
!      \item [filename]
!            Name of file.  Maximum length 58 characters to allow for
!            the PET number to be added and keep the total file name
!            length under 64 characters.
!     \item [{[logappendflag]}]
!           If the log file already exists, a value of {\tt .false.}
!           will set the file position to the beginning of the file.  A value
!           of [\tt .true.} sets the position to the end of the file.
!           If not specified, defaults to {\tt .true.}.
!      \item [{[logkindflag]}]
!            Specifies {\tt ESMF\_LOGKIND\_SINGLE}, {\tt ESMF\_LOGKIND\_MULTI} or
!            {\tt ESMF\_LOGKIND\_NONE}.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!EOPI
    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    call ESMF_LogOpen(ESMF_LogDefault, filename,  &
        appendflag=logappendflag, logkindflag=logkindflag, rc=rc)

end subroutine ESMF_LogInitialize

!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogOpen()"
!BOP
! !IROUTINE: ESMF_LogOpen - Open Log file(s)

! !INTERFACE:
    subroutine ESMF_LogOpen(log, filename, keywordEnforcer,  &
        appendflag, logkindflag, noPrefix, rc)
!
! !ARGUMENTS:
    type(ESMF_Log),          intent(inout)         :: log
    character(len=*),        intent(in)            :: filename
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                 intent(in),  optional :: appendFlag
    type(ESMF_LogKind_Flag), intent(in),  optional :: logkindFlag
    logical,                 intent(in),  optional :: noPrefix
    integer,                 intent(out), optional :: rc

!
! !DESCRIPTION:
!      This routine opens a file named {\tt filename} and associates
!      it with the {\tt ESMF\_Log}.  When {\tt logkindflag} is set to
!      {\tt ESMF\_LOGKIND\_MULTI} or {\tt ESMF\_LOGKIND\_MULTI\_ON\_ERROR}
!      the file name is prepended with PET number identification.  If the
!      incoming log is already open, an error is returned.
!
!      The arguments are:
!      \begin{description}
!      \item [log]
!            An {\tt ESMF\_Log} object.
!      \item [filename]
!            Name of log file to be opened.
!      \item [{[appendFlag]}]
!            If the log file exists, setting to {\tt .false.} will set the file position
!            to the beginning of the file.  Otherwise, new records will be appended to the
!            end of the file.  If not specified, defaults to {\tt .true.}.
!      \item [{[logkindFlag]}]
!            Set the logkindflag. See section \ref{const:logkindflag} for a list of
!            valid options.  When the {\tt ESMF\_LOGKIND\_MULTI\_ON\_ERROR} is selected,
!            the log opening is deferred until a {\tt ESMF\_LogWrite} with log message of
!            type {\tt ESMF\_LOGMSG\_ERROR} is written.
!            If not specified, defaults to {\tt ESMF\_LOGKIND\_MULTI}.
!      \item [{[noPrefix]}]
!            Set the noPrefix flag.  If set to {\tt .false.}, log messages are prefixed
!            with time stamps, message type, and PET number.  If set to {\tt .true.} the
!            messages will be written without prefixes.  If not specified, defaults to
!            {\tt .false.}.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!EOP
    interface
      subroutine f_ESMF_VMGlobalGet(localPet, petCount)
        integer, intent(out), optional  :: localPet
        integer, intent(out), optional  :: petCount
      end subroutine f_ESMF_VMGlobalGet
    end interface

    integer :: localrc, rc2
    integer :: iostat, memstat
    integer                                                :: i
    type(ESMF_LogEntry), dimension(:), pointer             :: localbuf
    character(len=ESMF_MAXPATHLEN)                         :: fname
    character(ESMF_MAXSTR)                                 :: petNumChar
    character(8)                                           :: position
    integer                                                :: petCount
    integer                                                :: digits
    character(len=10)                                      :: formatString

    type(ESMF_LogPrivate),pointer     :: alog

    ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)

    ! Initialize return code; assume routine not implemented
    if (present(rc)) then
        rc=ESMF_FAILURE
    endif

    if (log%logTableIndex > 0) then
      alog => ESMF_LogTable(log%logTableIndex)
    else
      ESMF_LogTableCount = ESMF_LogTableCount + 1   ! counting number of files
      log%logTableIndex = ESMF_LogTableCount                ! Assign log
      alog => ESMF_LogTable(log%logTableIndex)
    endif

    ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogPrivateGetInit,ESMF_LogPrivateInit,alog)

    alog%appendFlag = .true.
    if (present (appendflag)) then
      alog%appendFlag = appendflag
    end if

    ! Test if it is open or closed
    if (alog%FileIsOpen == ESMF_TRUE) then
      write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
          ": This ESMF_Log is already open with file '", &
          trim(ESMF_LogTable(log%logTableIndex)%nameLogErrFile), "'"
      call ESMF_UtilIOUnitFlush(ESMF_UtilIOStderr, rc=rc)
      if (present(rc)) then
        rc=ESMF_RC_FILE_OPEN
      endif
      return
    endif

    alog%maxElements = 10
    alog%fIndex = 1

    call f_ESMF_VMGlobalGet(alog%petNumber, petCount)
    ! Convert PET to contiguous character label
    if (petCount>1) then
      digits = int (log10(real(petCount-1))+1)
    else
      digits = 1
    endif
    write(formatString, "('(i',i1,'.',i1,')')") digits, digits
    write(petNumChar, formatString) alog%petNumber
    alog%petNumLabel = "PET" // trim(adjustl(petNumChar))

    alog%stopprogram = .false.
    alog%flushImmediately = ESMF_FALSE
    alog%flushed = ESMF_FALSE
    alog%dirty = ESMF_FALSE
    alog%FileIsOpen=ESMF_FALSE
    alog%logmsgAbort => null ()
    nullify(alog%errorMask)
    alog%errorMaskCount=0
    if (present(logkindflag)) then
      alog%logkindflag=logkindflag
    else
      alog%logkindflag=ESMF_LOGKIND_MULTI
    endif
    alog%traceFlag = .false.
    alog%highResTimestampFlag = .false.
    alog%indentCount = 0
    alog%noPrefix = .false.

  if(alog%logkindflag /= ESMF_LOGKIND_NONE) then

    if (alog%logkindflag == ESMF_LOGKIND_SINGLE) then
        if (len_trim (filename) > ESMF_MAXPATHLEN-4) then
            write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
                ": Filename exceeded", ESMF_MAXPATHLEN, " characters."
            call ESMF_UtilIOUnitFlush(ESMF_UtilIOStderr, rc=rc)
            if (present(rc)) then
                rc = ESMF_RC_LONG_STR
            endif
            return
        endif
        alog%nameLogErrFile=trim(filename)
    else
        if (len_trim (filename) > ESMF_MAXPATHLEN-4) then
            write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
                ": Filename exceeded", ESMF_MAXPATHLEN, " characters."
            call ESMF_UtilIOUnitFlush(ESMF_UtilIOStderr, rc=rc)
            if (present(rc)) then
                rc = ESMF_RC_LONG_STR
            endif
            return
        endif
        fname = trim(alog%petNumLabel) // "." // trim(filename)
        alog%nameLogErrFile=fname
    endif

    ! Actually open the file
    if(alog%logkindflag /= ESMF_LOGKIND_MULTI_ON_ERROR) then
      call ESMF_LogOpenFile (alog, rc=localrc)
      if (present(rc)) then
        rc=localrc
      endif
    else
      alog%deferredOpenFlag = .true.
    end if

  endif

  !TODO: this is really strange because every time ESMF_LogOpen() is called
  !TODO: the _default_ Log on the C side is initialized, odd, isn't it? *gjt*
  
  !TODO: an attempt to at least only call the initialize for default log...
  
  if (log%logTableIndex == ESMF_LogDefault%logTableIndex) then
    ! this is the default log
    call c_ESMC_LogInitialize(filename,alog%petNumber,alog%logkindflag,rc2)
    !TODO: this is so messed up, why is rc2 not looked at, or passed back???
  endif

  if (present(rc)) rc=ESMF_SUCCESS
  
end subroutine ESMF_LogOpen

!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogOpenDefault()"
!BOP
! !IROUTINE: ESMF_LogOpen - Open Default Log file(s)

! !INTERFACE:
  ! Private name; call using ESMF_LogOpen ()
    subroutine ESMF_LogOpenDefault (filename, keywordEnforcer,  &
        appendflag, logkindflag, rc)
!
! !ARGUMENTS:
    character(len=*),        intent(in)            :: filename
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                 intent(in),  optional :: appendflag
    type(ESMF_LogKind_Flag), intent(in),  optional :: logkindflag
    integer,                 intent(out), optional :: rc

!
! !DESCRIPTION:
!      This routine opens a file named {\tt filename} and associates
!      it with the default log.  When {\tt logkindflag} is set to
!      {\tt ESMF\_LOGKIND\_MULTI} the file name is prepended with PET
!      number identification.  If the incoming default log is already open,
!      an error is returned.
!
!      The arguments are:
!      \begin{description}
!      \item [filename]
!            Name of DEFAULT log file to be opened.
!      \item [{[appendflag]}]
!            If the log file exists, setting to {\tt .false.} will set the file position
!            to the beginning of the file.  Otherwise, new records will be appended to the
!            end of the file.  If not specified, defaults to {\tt .true.}.
!      \item [{[logkindflag]}]
!            Set the logkindflag. See section \ref{const:logkindflag} for a list of
!            valid options.
!            If not specified, defaults to {\tt ESMF\_LOGKIND\_MULTI}.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!EOP

  integer                :: localrc
  character(ESMF_MAXSTR) :: errmsg
  integer                :: errmsg_len

  ! Initialize return code; assume routine not implemented
  if (present (rc)) then
    rc=ESMF_FAILURE
  endif

  call ESMF_LogOpen (ESMF_LogDefault, filename, appendflag=appendflag, &
    logkindflag=logkindflag, rc=localrc)
  if (localrc /= ESMF_SUCCESS) then
    call ESMF_LogRc2Msg (localrc, msg=errmsg, msglen=errmsg_len)
    write (ESMF_UtilIOStderr,*) ESMF_METHOD, ': ', errmsg(:errmsg_len)
    call ESMF_UtilIOUnitFlush(ESMF_UtilIOStderr, rc=rc)
  end if

  if (present (rc)) then
    rc = localrc
  end if

end subroutine ESMF_LogOpenDefault

!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogOpenFile()"
!BOPI
! !IROUTINE: ESMF_LogOpenFile - Open the log file and allocate buffer

! !INTERFACE:
  subroutine ESMF_LogOpenFile (alog, rc)
!
! !ARGUMENTS:
!
    type(ESMF_LogPrivate), intent(inout) :: alog
    integer,               intent(out)   :: rc
!
! !DESCRIPTION:
!     This subroutine opens the log file and allocates the log buffer.
!
!     The arguments are:
!     \begin{description}
!
!     \item [alog]
!       Internal Log object.
!     \item [rc]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    type(ESMF_LogEntry), pointer :: localbuf(:)
    character(8) :: position
    integer :: i
    integer :: memstat, iostat
    integer :: localrc

    ! Initialize return code; assume routine not implemented
    rc=ESMF_RC_NOT_IMPL

    ! find an available unit number
    call ESMF_UtilIOUnitGet (alog%unitNumber, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
      rc=ESMF_RC_CANNOT_GET
    endif

    position = merge ("append", "rewind", alog%appendFlag)

    ! open the file, with retries
    do i=1, ESMF_LOG_MAXTRYOPEN
#if !defined (ESMF_OS_MinGW)
        OPEN(UNIT=alog%unitNumber,File=alog%nameLogErrFile,&
             POSITION=position, ACTION="WRITE", STATUS="UNKNOWN", IOSTAT=iostat)
#else
#if defined (__INTEL_COMPILER)
        OPEN(UNIT=alog%unitNumber,File=alog%nameLogErrFile,&
             POSITION=position, ACTION="WRITE", STATUS="UNKNOWN", &
             SHARE="DENYNONE", IOSTAT=iostat)
#else
        OPEN(UNIT=alog%unitNumber,File=alog%nameLogErrFile,&
             POSITION=position, ACTION="WRITE", STATUS="UNKNOWN", IOSTAT=iostat)
#endif
#endif
        if (iostat == 0) then
            alog%FileIsOpen = ESMF_TRUE
            exit
        endif
    enddo

    ! if unable to open file then error out
    if (alog%FileIsOpen /= ESMF_TRUE) then
      write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
          ': error opening file: ', trim (alog%nameLogErrFile),  &
          ', iostat =', iostat
      call ESMF_UtilIOUnitFlush(ESMF_UtilIOStderr, rc=rc)
      rc=ESMF_RC_FILE_UNEXPECTED
      return
    endif

    ! BEWARE:  absoft 8.0 compiler bug - if you try to allocate directly
    ! you get an error.  if you allocate a local buffer and then point the
    ! derived type buffer at it, it works.  go figure.

    allocate(localbuf(alog%maxElements), stat=memstat)
    if (memstat /= 0) then
      write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
          ': Allocation of buffer failed.'
      call ESMF_UtilIOUnitFlush(ESMF_UtilIOStderr, rc=rc)
      rc = ESMF_RC_MEM_ALLOCATE
      return
    endif
    alog%LOG_ENTRY => localbuf

    rc = ESMF_SUCCESS

  end subroutine ESMF_LogOpenFile

!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogRc2Msg()"
!BOPI
! !IROUTINE: ESMF_LogRc2Msg - Convert rc to message string

! !INTERFACE:
    subroutine ESMF_LogRc2Msg (rcToCheck, msg, msglen)
!
! !ARGUMENTS:
!
      integer,      intent(in)  :: rcToCheck
      character(*), intent(out) :: msg
      integer,      intent(out) :: msglen
!
! !DESCRIPTION:
!     This subroutine converts an integer rc to a message string.
!
!     The arguments are:
!     \begin{description}
!
!     \item [rcToCheck]
!       An ESMF return code.
!     \item [msg]
!       A character string for the message
!     \item [msglen]
!       Message length exclusive of trailing blanks.
!     \end{description}
!
!EOPI

      call c_esmc_loggeterrormsg (rcToCheck, msg, msglen)

end subroutine ESMF_LogRc2Msg

!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogSet()"
!BOP
! !IROUTINE: ESMF_LogSet - Set Log parameters

! !INTERFACE:
    subroutine ESMF_LogSet(log, keywordEnforcer,  &
        flush,  &
        logmsgAbort, maxElements, logmsgList,  &
        errorMask, trace, highResTimestampFlag, indentCount,  &
        noPrefix, rc)
!
! !ARGUMENTS:
!
      type(ESMF_Log),         intent(inout), optional :: log
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      logical,                intent(in),    optional :: flush
      type(ESMF_LogMsg_Flag), intent(in),    optional :: logmsgAbort(:)
      integer,                intent(in),    optional :: maxElements
      type(ESMF_LogMsg_Flag), intent(in),    optional :: logmsgList(:)
      integer,                intent(in),    optional :: errorMask(:)
      logical,                intent(in),    optional :: trace
      logical,                intent(in),    optional :: highResTimestampFlag
      integer,                intent(in),    optional :: indentCount
      logical,                intent(in),    optional :: noPrefix
      integer,                intent(out),   optional :: rc

!
! !DESCRIPTION:
!      This subroutine sets the properties for the Log object.
!
!      The arguments are:
!      \begin{description}
!
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object.  The default is to use the
!            default log that was opened at {\tt ESMF\_Initialize} time.
!      \item [{[flush]}]
!            If set to {\tt .true.}, flush log messages immediately, rather
!            than buffering them.  Default is to flush after {\tt maxElements}
!            messages.
!      \item [{[logmsgAbort]}]
!            Sets the condition on which ESMF aborts.  The array
!            can contain any combination of {\tt ESMF\_LOGMSG} named constants.  These
!            named constants are described in section \ref{const:logmsgflag}.
!            Default is to always continue processing.
!      \item [{[maxElements]}]
!            Maximum number of elements in the Log buffer before flushing occurs.
!            Default is to flush when 10 messages have been accumulated.
!      \item [{[logmsgList]}]
!            An array of message types that will be logged.  Log write requests
!            not matching the list will be ignored.  If an empty array is
!            provided, no messages will be logged.
!            See section \ref{const:logmsgflag} for a list of
!            valid message types.  By default, all non-trace messages will be
!            logged.
!      \item [{[errorMask]}]
!            List of error codes that will {\em not} be logged as errors.
!            Default is to log all error codes.
!      \item [{[trace]}]
!            \begin{sloppypar}
!            If set to {\tt .true.}, calls such as {\tt ESMF\_LogFoundError()},
!            {\tt ESMF\_LogFoundAllocError()}, and
!            {\tt ESMF\_LogFoundDeallocError()}
!            will be logged in the default log files.  This option is intended
!            to be used as a tool for debugging and program flow tracing
!            within the ESMF library. Voluminous output may appear in the log,
!            with a consequent slowdown in performance.  Therefore, it is
!            recommended that this option only be enabled before a problematic
!            call to a ESMF method, and disabled afterwards. Default is to
!            not trace these calls.
!           \end{sloppypar}
!      \item [{[highResTimestampFlag]}]
!            Sets the extended elapsed timestamp flag.  If set to {\tt .true.}, a timestamp
!            from {\tt ESMF\_VMWtime} will be included in each log message.  Default is
!            to not add the additional timestamps.
!      \item [{[indentCount]}]
!            Number of leading white spaces.
!      \item [{[noPrefix]}]
!            If set to {\tt .false.}, log messages are prefixed with time stamps,
!            message type and PET number.  If set to {\tt .true.} the messages will be
!            written without the prefixes.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!EOP
    integer :: i, status, status2
    logical :: isDefault
    type(ESMF_LogPrivate), pointer          :: alog
    type(ESMF_LogEntry), dimension(:), pointer :: localbuf
    type(ESMF_Logical) :: traceFlag_c

    ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)

    nullify(alog) ! ensure that the association status is well defined

    isDefault = .false.
    if (present(log)) then
      if (log%logTableIndex > 0) then
         alog => ESMF_LogTable(log%logTableIndex)
      endif
    else
      if (ESMF_LogDefault%logTableIndex > 0) then
        alog => ESMF_LogTable(ESMF_LogDefault%logTableIndex)
        isDefault = .true.
      end if
    endif

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    if (associated(alog)) then

      ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogPrivateGetInit,ESMF_LogPrivateInit,alog)

      if (alog%FileIsOpen /= ESMF_TRUE .and. .not. isDefault) then
        write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
            ": ESMF_Log not open -- cannot ESMF_LogSet()."
        call ESMF_UtilIOUnitFlush(ESMF_UtilIOStderr, rc=rc)
        if (present (rc)) rc = ESMF_RC_CANNOT_SET
        return
      endif

      if (present(flush)) then
        alog%flushImmediately=flush
      endif
      if (present(logmsgAbort)) then
        if (associated (alog%logmsgAbort)) deallocate (alog%logmsgAbort)
        allocate (alog%logmsgAbort(size (logmsgAbort)))
        alog%logmsgAbort = logmsgAbort
      endif
      if (present(maxElements)) then
        if (maxElements>0 .AND. alog%maxElements/=maxElements) then
          allocate(localbuf(maxElements), stat=status)

          ! if the current number of log entries is greater than the new
          ! maxElements, then call flush.  Otherwise copy old contents over.
          if (alog%fIndex.ge.maxElements) then
            call ESMF_LogFlush(log,rc=status)
          else
            do i = 1,alog%fIndex
              call ESMF_LogEntryCopy(alog%LOG_ENTRY(i), localbuf(i), rc=status)
            enddo
          endif
          deallocate(alog%LOG_ENTRY,stat=status)
          alog%LOG_ENTRY => localbuf
          alog%maxElements=maxElements
        endif
      endif
      if (present(errorMask)) then
        if (alog%errorMaskCount > 0) then
          deallocate(alog%errorMask)
        endif
        alog%errorMaskCount = size(errorMask)
        allocate(alog%errorMask(alog%errorMaskCount))
        alog%errorMask = errorMask  ! copy the content of the errorMask argument
      endif

      ! currently the connection between F90 and C++ side of LogErr is only well
      ! defined for the default Log, so only then call the C++ side LogSet().
      if (isDefault) then
        !TODO: I am only implementing this to get the errorMask into the C++ side
        !TODO: LogErr needs major help anyway, so someday this may get sorted out
        !TODO: to work for more general cases. *gjt*
        if (present(errorMask)) then
          call c_ESMC_LogSet(alog%errorMask(1),alog%errorMaskCount,status2)
        endif
      endif

      if (present (logmsgList)) then
        if (associated (alog%logmsgList))  &
          deallocate (alog%logmsgList)
          allocate (alog%logmsgList(size (logmsgList)))
          alog%logmsgList = logmsgList
      end if

      if (present (trace)) then
        alog%traceFlag = trace
        if (isDefault) then
          !TODO: Should work with user logs as well.  wws...
          traceFlag_c = trace
          call c_ESMC_LogSetTrace (traceFlag_c, status2)
        end if
        if (trace) then
          call ESMF_LogWrite ('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',  &
              ESMF_LOGMSG_TRACE, method=ESMF_METHOD, log=log)
          call ESMF_LogWrite ('!!!        TRACING IS ENABLED         !!!',  &
              ESMF_LOGMSG_TRACE, method=ESMF_METHOD, log=log)
          call ESMF_LogWrite ('!!! MAY CAUSE SLOWDOWN IN PERFORMANCE !!!',  &
              ESMF_LOGMSG_TRACE, method=ESMF_METHOD, log=log)
          call ESMF_LogWrite ('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',  &
              ESMF_LOGMSG_TRACE, method=ESMF_METHOD, log=log)
        else
          call ESMF_LogWrite ('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',  &
              ESMF_LOGMSG_TRACE, method=ESMF_METHOD, log=log)
          call ESMF_LogWrite ('!!!       TRACING is disabled         !!!',  &
              ESMF_LOGMSG_TRACE, method=ESMF_METHOD, log=log)
          call ESMF_LogWrite ('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',  &
              ESMF_LOGMSG_TRACE, method=ESMF_METHOD, log=log)
        end if
      end if

      if (present (highResTimestampFlag)) then
        alog%highResTimestampFlag = highResTimestampFlag
      end if

      if (present (indentCount)) then
        alog%indentCount = indentCount
      end if

      if (present (noPrefix)) then
        if (noPrefix .and. .not. isDefault) then
          alog%noPrefix = noPrefix
        else
          call ESMF_LogWrite ('Can not set noPrefix on default Log', method=ESMF_METHOD, log=log)
          if (present (rc)) rc = ESMF_RC_CANNOT_SET
          return
        end if
      end if

      if (present(rc)) then
        rc=ESMF_SUCCESS
      endif
    endif

end subroutine ESMF_LogSet


!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogSetError()"
!BOP
! !IROUTINE: ESMF_LogSetError - Set ESMF return code for error and write msg

! !INTERFACE:
      subroutine ESMF_LogSetError(rcToCheck, keywordEnforcer,  &
                                  msg, line, file, method, &
                                  rcToReturn, log)

! !ARGUMENTS:
!
      integer,          intent(in)              :: rcToCheck
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character(len=*), intent(in),    optional :: msg
      integer,          intent(in),    optional :: line
      character(len=*), intent(in),    optional :: file
      character(len=*), intent(in),    optional :: method
      integer,          intent(out),   optional :: rcToReturn
      type(ESMF_Log),   intent(inout), optional :: log

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!      This subroutine sets the {\tt rcToReturn} value to {\tt rcToCheck} if
!      {\tt rcToReturn} is present and writes this error code to the {\tt ESMF\_Log}
!      if an error is generated.  A predefined error message will added to the
!      {\tt ESMF\_Log} along with a user added {\tt msg}, {\tt line}, {\tt file}
!      and {\tt method}.
!
!      The arguments are:
!      \begin{description}
!
!      \item [rcToCheck]
!            rc value for set
!      \item [{[msg]}]
!            User-provided message string.
!      \item [{[line]}]
!            Integer source line number.  Expected to be set by
!            using the preprocessor macro {\tt \_\_LINE\_\_} macro.
!      \item [{[file]}]
!            User-provided source file name.
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[rcToReturn]}]
!            If specified, copy the {\tt rcToCheck} value to {\tt rcToreturn}.
!            This is not the return code for this function; it allows
!            the calling code to do an assignment of the error code
!            at the same time it is testing the value.
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!            of the default Log.
!
!      \end{description}
!
!EOP

    integer:: i
    logical:: masked
    type(ESMF_LogPrivate), pointer          :: alog
    character(len=ESMF_MAXSTR) :: errmsg
    integer :: msglen

    ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)

    nullify(alog) ! ensure that the association status is well defined

    if (present(log)) then
      if (log%logTableIndex > 0) then
         alog => ESMF_LogTable(log%logTableIndex)
      endif
    else
      if (ESMF_LogDefault%logTableIndex > 0) then
        alog => ESMF_LogTable(ESMF_LogDefault%logTableIndex)
      end if
    endif

    if (associated(alog)) then

      ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogPrivateGetInit,ESMF_LogPrivateInit,alog)

      ! set default returns
      if (present(rcToReturn)) rcToReturn = ESMF_SUCCESS

      ! check the error code
      if (rcToCheck .NE. ESMF_SUCCESS) then
        masked = .false.
        do i=1, alog%errorMaskCount
          if (alog%errorMask(i) == rcToCheck) masked = .true.
        enddo
        if (.not.masked) then
          call ESMF_Breakpoint()  ! no-op to assist debugging
          call ESMF_LogRc2Msg (rcToCheck, errmsg, msglen)
          if (present(msg)) then
            errmsg = errmsg(:msglen) // " - " // msg
            msglen = len_trim (errmsg)
          end if
          call ESMF_LogWrite(errmsg(:msglen), ESMF_LOGMSG_ERROR,  &
            line=line, file=file, method=method, log=log)
          if (present(rcToReturn)) rcToReturn = rcToCheck
        endif
      endif
    else
      if (present(rcToReturn)) rcToReturn = ESMF_SUCCESS
    endif

end subroutine ESMF_LogSetError


!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogWrite()"
!BOP
! !IROUTINE: ESMF_LogWrite - Write to Log file(s)

! !INTERFACE:
      recursive subroutine ESMF_LogWrite(msg, logmsgFlag, &
                        logmsgList,      & ! DEPRECATED ARGUMENT
                        keywordEnforcer, line, file, method, log, rc)
!
!
! !ARGUMENTS:
      character(len=*),      intent(in)             :: msg
      type(ESMF_LogMsg_Flag),intent(in),optional    :: logmsgFlag
      type(ESMF_LogMsg_Flag),intent(in),optional::logmsgList ! DEPRECATED ARG
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,               intent(in),   optional :: line
      character(len=*),      intent(in),   optional :: file
      character(len=*),      intent(in),   optional :: method
      type(ESMF_Log),        intent(inout),optional :: log
      integer,               intent(out),  optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[5.2.0rp1] Added argument {\tt logmsgFlag}.
!                 Started to deprecate argument {\tt logmsgList}.
!                 This corrects inconsistent use of the {\tt List} suffix on
!                 the argument name. In ESMF this suffix indicates
!                 one--dimensional array arguments.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!      This subroutine writes to the file associated with an {\tt ESMF\_Log}.
!      A message is passed in along with the {\tt logmsgFlag}, {\tt line},
!      {\tt file} and {\tt method}.  If the write to the {\tt ESMF\_Log}
!      is successful, the function will return a logical {\tt true}.  This
!      function is the base function used by all the other {\tt ESMF\_Log}
!      writing methods.
!
!      The arguments are:
!      \begin{description}
!
!      \item [msg]
!            User-provided message string.
!      \item [{[logmsgFlag]}]
!            The type of message.  See Section~\ref{const:logmsgflag} for
!            possible values.  If not specified, the default is {\tt ESMF\_LOGMSG\_INFO}.
!      \item [{[logmsgList]}]
!            \apiDeprecatedArgWithReplacement{logmsgFlag}
!      \item [{[line]}]
!            Integer source line number.  Expected to be set by
!            using the preprocessor macro {\tt \_\_LINE\_\_} macro.
!      \item [{[file]}]
!            User-provided source file name.
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!            of the default Log.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!EOP
    interface
      subroutine f_ESMF_VMAbort(rc)
        integer, intent(out), optional :: rc
      end subroutine f_ESMF_VMAbort
    end interface

    integer                         :: argcase
    type(ESMF_LogMsg_Flag)          :: local_logmsgflag

    character(len=10)               :: t
    character(len=8)                :: d
    integer                         :: timevals(8)

    !character(len=7)               :: lt
    character(len=32)               :: tmethod
    character(len=ESMF_MAXPATHLEN)  :: tfile
    integer                         :: tline
    integer                         :: i
    integer                         :: localrc
    integer                         :: memstat
    integer                         :: rc2, index, lenTotal, indentCount
    type(ESMF_LogPrivate), pointer  :: alog

    ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)

    ! Initialize return code; assume routine not implemented
    if (present(rc)) then
      rc=ESMF_RC_NOT_IMPL
    endif

    nullify(alog) ! ensure that the association status is well defined

    localrc = ESMF_SUCCESS
    if (present(log)) then
      if (log%logTableIndex > 0) then
         alog => ESMF_LogTable(log%logTableIndex)
      else
        localrc = ESMF_RC_OBJ_INIT
      endif
    else
      if (ESMF_LogDefault%logTableIndex > 0) then
        alog => ESMF_LogTable(ESMF_LogDefault%logTableIndex)
      else
        localrc = ESMF_RC_OBJ_INIT
      end if
    endif

    ! Check argument sanity

    argcase = 0
    argcase = argcase + merge (1, 0, present (logmsgFlag))
    argcase = argcase + merge (2, 0, present (logmsgList))
    select case (argcase)
    case (0)
      local_logmsgflag = ESMF_LOGMSG_INFO

    case (1)
      local_logmsgflag = logmsgFlag

    case (2)
      write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
          ": Deprecated: Use logmsgFlag instead of logmsgList."
      call ESMF_UtilIOUnitFlush(ESMF_UtilIOStderr, rc=rc)
      local_logmsgflag = logmsgList

    case (3)
      write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
          ": Do not specify both logmsgFlag and logmsgList.  Use logmsgFlag."
      call ESMF_UtilIOUnitFlush(ESMF_UtilIOStderr, rc=rc)
      if (present(rc)) then
        rc=ESMF_RC_ARG_INCOMP
      end if
      return

    end select

    if (associated(alog)) then

      ! Open the log file if necessary
      if (alog%logkindflag /= ESMF_LOGKIND_NONE) then

        if (alog%deferredOpenFlag) then
          if (local_logmsgflag == ESMF_LOGMSG_ERROR) then
            call ESMF_LogOpenFile (alog, rc=localrc)
            if (localrc /= ESMF_SUCCESS) then
              if (present (rc)) then
                rc = localrc
              end if
              return
            end if
            alog%deferredOpenFlag = .false.
          else
            if (present (rc)) then
              rc = ESMF_SUCCESS
            end if
            return
          end if
        end if

        if (alog%FileIsOpen /= ESMF_TRUE) then
          write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
              ": ESMF_Log not open -- cannot ESMF_LogWrite().  Log message = ", trim (msg)
          call ESMF_UtilIOUnitFlush(ESMF_UtilIOStderr, rc=rc)
          if (present(rc)) rc=ESMF_FAILURE
          return
        endif

        if (associated (alog%logmsgList)) then
          do, i=1, size (alog%logmsgList)
             if (local_logmsgflag == alog%logmsgList(i)) then
               exit
             end if
          end do

          if (i > size (alog%logmsgList)) then
            if (present (rc)) rc=ESMF_SUCCESS
            return
          end if
        end if

        ! Add the message to the message queue awaiting flushing

        index = alog%fIndex

        alog%dirty = ESMF_TRUE
        call DATE_AND_TIME(date=d, time=t, values=timevals)
        if (alog%highResTimestampFlag) then
          call c_ESMC_VMWtime (alog%LOG_ENTRY(index)%highResTimestamp, localrc)
          if (localrc /= ESMF_SUCCESS) then
            if (present (rc)) rc = localrc
            return
          end if
        end if
        alog%LOG_ENTRY(index)%noPrefix = alog%noPrefix
        alog%LOG_ENTRY(index)%highResTimestampFlag = alog%highResTimestampFlag
        alog%LOG_ENTRY(index)%indentCount = alog%indentCount
        alog%LOG_ENTRY(index)%methodflag = .FALSE.
        alog%LOG_ENTRY(index)%lineflag = .FALSE.
        alog%LOG_ENTRY(index)%fileflag = .FALSE.
        if (present(method)) then
            tmethod=adjustl(method)
            alog%LOG_ENTRY(index)%methodflag=.TRUE.
            alog%LOG_ENTRY(index)%method = tmethod
        endif
        if (present(line)) then
            tline=line
            alog%LOG_ENTRY(index)%lineflag = .TRUE.
            alog%LOG_ENTRY(index)%line = tline
        endif
        if (present(file)) then
            tfile=adjustl(file)
            alog%LOG_ENTRY(index)%fileflag = .TRUE.
            alog%LOG_ENTRY(index)%file = tfile
        endif
        select case (local_logmsgflag%mtype)
        case (:0, size (ESMF_LogMsgString)+1:)
            alog%LOG_ENTRY(index)%lt="INTERNAL ERROR"
        case default
            alog%LOG_ENTRY(index)%lt= ESMF_LogMsgString(local_logmsgflag%mtype)
        end select
        alog%LOG_ENTRY(alog%fIndex)%d  = d
        alog%LOG_ENTRY(alog%fIndex)%h  = timevals(5)
        alog%LOG_ENTRY(alog%fIndex)%m  = timevals(6)
        alog%LOG_ENTRY(alog%fIndex)%s  = timevals(7)
        alog%LOG_ENTRY(alog%fIndex)%ms = timevals(8)
        indentCount = alog%LOG_ENTRY(index)%indentCount
        lenTotal = len_trim(msg) + indentCount
        allocate (alog%LOG_ENTRY(alog%fIndex)%msg(lenTotal), stat=memstat)
        if (indentCount > 0) then
          ! insert leading white spaces
          alog%LOG_ENTRY(alog%fIndex)%msg(1:indentCount) = " "
        endif
        alog%LOG_ENTRY(alog%fIndex)%msg(1+indentCount:) = &
          ESMF_UtilString2Array(trim(msg))
        alog%flushed = ESMF_FALSE

        if (associated (alog%logmsgAbort)) then
          do, i=1, size (alog%logmsgAbort)
            if (local_logmsgflag%mtype == alog%logmsgAbort(i)%mtype) then
              alog%stopprogram=.true.
              alog%fIndex = alog%fIndex + 1
              call ESMF_LogFlush(log,rc=rc2)
              call ESMF_LogClose(ESMF_LogDefault, rc=rc2)
              exit
            end if
          end do
        end if

        if (alog%fIndex == alog%maxElements .or. &
            alog%flushImmediately == ESMF_TRUE .or.  &
            local_logmsgflag == ESMF_LOGMSG_ERROR) then
                alog%fIndex = alog%fIndex + 1
                call ESMF_LogFlush(log,rc=rc2)
                alog%fIndex = 1
        else
            alog%fIndex = alog%fIndex + 1
        endif
      endif
      ! if requested, halt the program right now.
      if (alog%stopprogram) call f_ESMF_VMAbort()
      if (present(rc)) then
        rc=ESMF_SUCCESS
      endif
    else
      write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
          ": ESMF_Log not open -- cannot ESMF_LogWrite().  Log message = ", trim (msg)
      call ESMF_UtilIOUnitFlush(ESMF_UtilIOStderr, rc=rc)
      if (present (rc)) then
        rc = localrc
      end if
    endif

end subroutine ESMF_LogWrite

!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogEntryCopy()"
!BOPI
! !IROUTINE: ESMF_LogEntryCopy - Copy a Log entry

! !INTERFACE:
      subroutine ESMF_LogEntryCopy(logEntryIn, logEntryOut, rc)
!
! !ARGUMENTS:
        type(ESMF_LogEntry), intent(inout)         :: logEntryIn
        type(ESMF_LogEntry), intent(out)           :: logEntryOut
        integer,             intent(out), optional :: rc

! !DESCRIPTION:
!      This routine copies the internals from one log entry to another.
!
!      The arguments are:
!      \begin{description}
!
!      \item [logEntryIn]
!            Log entry to copy from.
!      \item [logEntryOut]
!            Log entry to copy into.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!EOPI

    integer :: memstat

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LogEntryGetInit,ESMF_LogEntryInit,logEntryIn)

    logEntryOut%h    = logEntryIn%h
    logEntryOut%m    = logEntryIn%m
    logEntryOut%s    = logEntryIn%s
    logEntryOut%ms   = logEntryIn%ms
    logEntryOut%line = logEntryIn%line

    logEntryOut%noPrefix = logEntryIn%noPrefix
    logEntryOut%methodflag = logEntryIn%methodflag
    logEntryOut%lineflag   = logEntryIn%lineflag
    logEntryOut%fileflag   = logEntryIn%fileflag

    allocate (logEntryOut%msg(size (logEntryIn%msg)), stat=memstat)
    logEntryOut%msg    = logEntryIn%msg
    logEntryOut%file   = logEntryIn%file
    logEntryOut%method = logEntryIn%method
    logEntryOut%d      = logEntryIn%d
    logEntryOut%lt     = logEntryIn%lt
    logEntryOut%highResTimestamp = logEntryIn%highResTimestamp

    if (present(rc)) then
        rc=ESMF_SUCCESS
    endif

end subroutine ESMF_LogEntryCopy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogEQ()"
!BOPI
! !IROUTINE:  ESMF_LogEQ - Compare two Logs for equality
!
! !INTERFACE:
  function ESMF_LogEQ(log1, log2)
!
! !RETURN VALUE:
    logical :: ESMF_LogEQ

! !ARGUMENTS:
    type(ESMF_Log), intent(in) :: log1
    type(ESMF_Log), intent(in) :: log2

! !DESCRIPTION:
!     This method overloads the (==) operator for the {\tt ESMF\_Log}
!     class.  See "interface operator(==)" above for complete description.
!
!EOPI

    ESMF_LogEQ = log1%logTableIndex == log2%logTableIndex

  end function ESMF_LogEQ

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogNE()"
!BOPI
! !IROUTINE:  ESMF_LogEQ - Compare two Logs for inequality
!
! !INTERFACE:
  function ESMF_LogNE(log1, log2)
!
! !RETURN VALUE:
    logical :: ESMF_LogNE

! !ARGUMENTS:
    type(ESMF_Log), intent(in) :: log1
    type(ESMF_Log), intent(in) :: log2

! !DESCRIPTION:
!     This method overloads the (/=) operator for the {\tt ESMF\_Log}
!     class.  See "interface operator(==)" above for complete description.
!
!EOPI

    ESMF_LogNE = log1%logTableIndex /= log2%logTableIndex

  end function ESMF_LogNE

end module ESMF_LogErrMod

!-------------------------------------------------------------------------------

subroutine ESMF_Breakpoint()
  ! This no-op routine is called when an error condition is detected inside of
  ! the ESMF library. By setting a breakpoint on this routine in a debugger it
  ! is possible to inspect the complete path through user and ESMF code that
  ! lead to the error condition.
  continue
end subroutine
