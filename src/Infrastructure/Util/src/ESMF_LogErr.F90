! $Id: ESMF_LogErr.F90,v 1.45.2.3 2009/01/21 21:25:24 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
!     ESMF LogErr Module
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
#include "ESMF_Macros.inc"
#include "ESMF_Conf.inc"
#include "ESMF_InitMacros.inc"
#include "ESMF_LogConstants.inc"
#include "ESMF_ErrReturnCodes.inc"

#define ESMF_SUCCESS_DEFAULT_OFF

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
    use ESMF_UtilTypesMod
 !!  use ESMF_InitMacrosMod Commented out to prevent circular dependency
 !!                         this is possible because since all the checks
 !!                         in this module are shallow - Bob 1/9/2007.

implicit none

!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!------------------------------------------------------------------------------

!     ! ESMF_MsgType
type ESMF_MsgType
    sequence
    integer      :: mtype
end type

!     ! Msg Types
type(ESMF_MsgType), parameter           :: &
    ESMF_LOG_INFO  =   ESMF_MsgType(1), &
    ESMF_LOG_WARNING = ESMF_MsgType(2), &
    ESMF_LOG_ERROR =   ESMF_MsgType(3)

!     ! ESMF_Halt
type ESMF_HaltType
    sequence
    integer      :: htype
end type

!     ! Halt Types
type(ESMF_HaltType), parameter           :: &
    ESMF_LOG_HALTNEVER  =  ESMF_HaltType(1), &
    ESMF_LOG_HALTWARNING = ESMF_HaltType(2), &
    ESMF_LOG_HALTERROR =   ESMF_HaltType(3)
    
!     ! ESMF_LogType
type ESMF_LogType
    sequence
    integer      :: ftype
end type
    
!     ! Log Types
type(ESMF_LogType), parameter		:: &
    ESMF_LOG_SINGLE = ESMF_LogType(1), &
    ESMF_LOG_MULTI = ESMF_LogType(2),  &
    ESMF_LOG_NONE = ESMF_LogType(3)
    
!     ! Log Entry                            
type ESMF_LogEntry
    private
    sequence  					
    integer		::  h,m,s,ms
    integer		::  line
    logical             ::  methodflag,lineflag,fileflag
    character(len=2*ESMF_MAXSTR) ::  msg
    character(len=32) 	::  file,method
    character(len=8) 	::  d
    character(len=8)	::  lt  			
    ESMF_INIT_DECLARE    
end type ESMF_LogEntry

type ESMF_Log
    private
    sequence
#ifndef ESMF_NO_INITIALIZERS
    integer                                         ::  logTableIndex = 0
#else
    integer                                         ::  logTableIndex
#endif
    ESMF_INIT_DECLARE
end type ESMF_Log

type ESMF_LogPrivate
    private
    sequence        
     
    integer                                         ::  maxElements
    integer                                         ::  stream 
    integer                              	    ::  fIndex
    integer                                         ::  unitNumber
    integer                                         ::  petNumber	
    logical					    ::  stopprogram
    logical					    ::  pad ! memory alignment
    type(ESMF_Logical)                              ::  flushImmediately    
    type(ESMF_Logical)                              ::  rootOnly    
    type(ESMF_Logical)                              ::  verbose  
    type(ESMF_Logical)			            ::  flushed 
    type(ESMF_Logical)			            ::  dirty
    type(ESMF_HaltType)                             ::  halt
    type(ESMF_LogType)			            ::  logtype      
#ifndef ESMF_NO_INITIALIZERS
    type(ESMF_LogEntry), dimension(:),pointer       ::  LOG_ENTRY=>Null()
    type(ESMF_Logical)                              ::  FileIsOpen=ESMF_FALSE
    integer                                         ::  errorMaskCount=0
    integer, dimension(:), pointer                  ::  errorMask(:)=>Null()
#else
    type(ESMF_LogEntry), dimension(:),pointer       ::  LOG_ENTRY
    type(ESMF_Logical)                              ::  FileIsOpen
    integer                                         ::  errorMaskCount
    integer, dimension(:), pointer                  ::  errorMask(:)
#endif                                          
    character(len=32)                               ::  nameLogErrFile
    character(len=ESMF_MAXSTR)                      ::  petNumLabel
    ESMF_INIT_DECLARE    
end type ESMF_LogPrivate

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
    public ESMF_LOG_INFO
    public ESMF_LOG_WARNING
    public ESMF_LOG_ERROR
    public ESMF_LOG_SINGLE
    public ESMF_LOG_MULTI
    public ESMF_LOG_NONE    
    public ESMF_LOG_HALTNEVER
    public ESMF_LOG_HALTWARNING
    public ESMF_LOG_HALTERROR
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
   public ESMF_LogFoundError
   public ESMF_LogGet
   public ESMF_LogInitialize
   public ESMF_LogMsgFoundAllocError
   public ESMF_LogMsgFoundError
   public ESMF_LogMsgSetError
   public ESMF_LogOpen
   public ESMF_LogSet
   public ESMF_LogWrite
   public ESMF_HaltType
   public ESMF_MsgType

!  Overloaded = operator functions
   public operator(.eq.),operator(.gt.)
   
! overload .eq. and .gt. with additional derived types so you can compare 
!  them as if they were simple integers.
 

interface operator (.eq.)
   module procedure ESMF_lmteq
   module procedure ESMF_lhteq
   module procedure ESMF_llteq
end interface

interface operator (.ne.)
   module procedure ESMF_lltne
end interface

interface operator (.gt.)
   module procedure ESMF_lmtgt
end interface
!EOPI  

type(ESMF_Log),SAVE,target::ESMF_LogDefault
integer, parameter :: ESMF_LogTableMax=1000            ! Max # of files allowed to open
type(ESMF_LogPrivate),SAVE,target :: ESMF_LogTable(ESMF_LogTableMax) ! Users files
integer,SAVE :: ESMF_LogTableCount=0                   ! count users' number of files


!----------------------------------------------------------------------------

contains


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogGetInit"
!BOPI
! !IROUTINE:  ESMF_LogGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_LogGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_Log), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_LogGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt log}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Log} from which to retreive status.
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
#define ESMF_METHOD "ESMF_LogInit"
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
#define ESMF_METHOD "ESMF_LogValidate"
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

     ESMF_INIT_CHECK_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_LogValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogPrivateGetInit"
!BOPI
! !IROUTINE:  ESMF_LogPrivateGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_LogPrivateGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_LogPrivate), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_LogPrivateGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt logprivate}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LogPrivate} from which to retreive status.
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
#define ESMF_METHOD "ESMF_LogPrivateInit"
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
       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_LogPrivateInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogPrivateValidate"
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

     ESMF_INIT_CHECK_SHALLOW(ESMF_LogPrivateGetInit,ESMF_LogPrivateInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_LogPrivateValidate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogEntryGetInit"
!BOPI
! !IROUTINE:  ESMF_LogEntryGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_LogEntryGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_LogEntry), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_LogEntryGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt LogEntry}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LogEntry} from which to retreive status.
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
#define ESMF_METHOD "ESMF_LogEntryInit"
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
#define ESMF_METHOD "ESMF_LogEntryValidate"
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

     ESMF_INIT_CHECK_SHALLOW(ESMF_LogEntryGetInit,ESMF_LogEntryInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_LogEntryValidate

!------------------------------------------------------------------------------
! functions to compare two types to see if they're the same or not

function ESMF_lhteq(ht1, ht2)
logical ESMF_lhteq
type(ESMF_HaltType), intent(in) :: ht1,ht2
    
    ESMF_lhteq = (ht1%htype .eq. ht2%htype)
end function

function ESMF_lmteq(mt1, mt2)
logical ESMF_lmteq
type(ESMF_MsgType), intent(in) :: mt1,mt2

    ESMF_lmteq = (mt1%mtype .eq. mt2%mtype)
end function

function ESMF_llteq(lt1, lt2)
logical ESMF_llteq
type(ESMF_LogType), intent(in) :: lt1,lt2

    ESMF_llteq = (lt1%ftype .eq. lt2%ftype)
end function

function ESMF_lltne(lt1, lt2)
logical ESMF_lltne
type(ESMF_LogType), intent(in) :: lt1,lt2

    ESMF_lltne = (lt1%ftype .ne. lt2%ftype)
end function

function ESMF_lmtgt(mt1, mt2)
logical ESMF_lmtgt
type(ESMF_MsgType), intent(in) :: mt1,mt2

    ESMF_lmtgt = (mt1%mtype .gt. mt2%mtype)
end function


!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogClose - Close Log file(s)

! !INTERFACE: 
    subroutine ESMF_LogClose(log, rc)
!
! !ARGUMENTS:
    type(ESMF_Log)	                                        :: log
    integer, intent(out),optional                               :: rc

! !DESCRIPTION:
!      This routine closes the file(s) associated with the {\tt log}.
!
!      The arguments are:
!      \begin{description}
! 
!      \item [log]
!            An {\tt ESMF\_Log} object.
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

    ESMF_INIT_CHECK_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)

    if(log%logTableIndex.gt.0) then
      alog => ESMF_LogTable(log%logTableIndex)
      ESMF_INIT_CHECK_SHALLOW(ESMF_LogPrivateGetInit,ESMF_LogPrivateInit,alog)

      if (alog%logtype .ne. ESMF_LOG_NONE) then
	if (alog%FileIsOpen .eq. ESMF_TRUE) then
	    call ESMF_LogFlush(log,rc=rc2)		
    	    CLOSE(UNIT=alog%unitNumber)
	    alog%FileIsOpen=ESMF_FALSE
	    deallocate(alog%LOG_ENTRY,stat=status)	
	endif
      endif
    
      if (alog%errorMaskCount .gt. 0) then
        deallocate(alog%errorMask)
      endif
      
    endif 

    if (present(rc)) then
      rc=ESMF_SUCCESS
    endif
	
end subroutine ESMF_LogClose

!--------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LogFinalize - Finalize Log file(s)

! !INTERFACE: 
	subroutine ESMF_LogFinalize(rc)
!
! !ARGUMENTS:
    integer, intent(out),optional	                        :: rc

! !DESCRIPTION:
!      This routine finalizes the global Log.  The default Log will be flushed
!      and the default Log will be closed.
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

        ESMF_INIT_CHECK_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)

        ! Loop through all ESMF_LogTable(*) and close the files
        do k = 1,ESMF_LogTableCount
          log%logTableIndex = k
          call ESMF_LogClose(log, rc)
        enddo

        call c_ESMC_LogFinalize(rc2)

end subroutine ESMF_LogFinalize

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogFlush - Flushes the Log file(s)

! !INTERFACE: 
	subroutine ESMF_LogFlush(log,rc)
!
!
! !ARGUMENTS:
        type(ESMF_Log), target,optional				:: log
	integer, intent(out),optional		                :: rc

! !DESCRIPTION:
!      This subroutine flushes the {\tt ESMF\_Log} buffer to its
!      associated file.
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
    integer 			    :: j
    type(ESMF_LogPrivate),pointer     :: alog
   
    ESMF_INIT_CHECK_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)
    
    nullify(alog) ! ensure that the association status is well defined
    
    if (present(log)) then
      if(log%logTableIndex.gt.0) then
         alog => ESMF_LogTable(log%logTableIndex)
      endif
    else
      alog => ESMF_LogTable(ESMF_LogDefault%logTableIndex)
    endif

    ! Initialize return code; assume routine not implemented
    if (present(rc)) then
      rc=ESMF_FAILURE 
    endif

    if (associated(alog)) then

      ESMF_INIT_CHECK_SHALLOW(ESMF_LogPrivateGetInit,ESMF_LogPrivateInit,alog)

      if (alog%FileIsOpen .ne. ESMF_TRUE) then
        print *, "ESMF_Log not open -- cannot ESMF_LogFlush()."
        return
      endif
      
      if ((alog%FileIsOpen .eq. ESMF_TRUE) .AND. &
        (alog%flushed .eq. ESMF_FALSE) .AND. &
	(alog%dirty .eq. ESMF_TRUE))  then	
	    do j=1, alog%fIndex-1
    	        if (alog%LOG_ENTRY(j)%lineflag) then
    	            if (alog%LOG_ENTRY(j)%methodflag) then
    		        WRITE(alog%unitNumber,122) &
                              alog%LOG_ENTRY(j)%d, " ", &
                              alog%LOG_ENTRY(j)%h, &
                              alog%LOG_ENTRY(j)%m, &
                              alog%LOG_ENTRY(j)%s, ".", &
                              alog%LOG_ENTRY(j)%ms, " ", &
                              alog%LOG_ENTRY(j)%lt, " ", &
			      trim(alog%petNumLabel), " ", &
                              trim(alog%LOG_ENTRY(j)%file) , " ", &
                              alog%LOG_ENTRY(j)%line, " ", &
                              trim(alog%LOG_ENTRY(j)%method), " ", &
                              trim(alog%LOG_ENTRY(j)%msg)
    		    else
    		        WRITE(alog%unitNumber,123) &
                              alog%LOG_ENTRY(j)%d, " ", &
                              alog%LOG_ENTRY(j)%h, &
                              alog%LOG_ENTRY(j)%m, &
                              alog%LOG_ENTRY(j)%s, ".", &
                              alog%LOG_ENTRY(j)%ms, " ", &
                              alog%LOG_ENTRY(j)%lt, " ", &
			      trim(alog%petNumLabel), " ", &
                              trim(alog%LOG_ENTRY(j)%file), " ", &
                              alog%LOG_ENTRY(j)%line, " ", &
                              trim(alog%LOG_ENTRY(j)%msg)
    		    endif
                else
    		    if (alog%LOG_ENTRY(j)%methodflag) then
    		        WRITE(alog%unitNumber,132) &
                              alog%LOG_ENTRY(j)%d, " ", &
                              alog%LOG_ENTRY(j)%h, &
                              alog%LOG_ENTRY(j)%m, &
                              alog%LOG_ENTRY(j)%s, ".", &
                              alog%LOG_ENTRY(j)%ms, " ", &
                              alog%LOG_ENTRY(j)%lt, " ", &
		              trim(alog%petNumLabel), " ", &
    			      trim(alog%LOG_ENTRY(j)%method), " ", &
                              trim(alog%LOG_ENTRY(j)%msg)
    		    else
    		        WRITE(alog%unitNumber,133) &
                              alog%LOG_ENTRY(j)%d, " ", &
                              alog%LOG_ENTRY(j)%h, &
                              alog%LOG_ENTRY(j)%m, &
                              alog%LOG_ENTRY(j)%s , ".", &
                              alog%LOG_ENTRY(j)%ms , " ", &
                              alog%LOG_ENTRY(j)%lt, " ", &
			      trim(alog%petNumLabel), " ", &
                              trim(alog%LOG_ENTRY(j)%msg)
    		    endif
    	        endif
	    enddo
      endif
   
      alog%fIndex = 1 
      122  FORMAT(a8,a,3i2.2,a,i6.6,7a,i0,4a)
      123  FORMAT(a8,a,3i2.2,a,i6.6,7a,i0,2a)
      132  FORMAT(a8,a,3i2.2,a,i6.6,8a)
      133  FORMAT(a8,a,3i2.2,a,i6.6,6a)
   
      alog%flushed = ESMF_TRUE
      alog%dirty = ESMF_FALSE
          
      rc=ESMF_SUCCESS	
      
    endif

end subroutine ESMF_LogFlush
!--------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_LogFoundAllocError - Check Fortran status for allocation error

! !INTERFACE: 
	function ESMF_LogFoundAllocError(statusToCheck, line, file, & 
                                         method, rcToReturn,log)
!
! !RETURN VALUE:
	logical                                     ::ESMF_LogFoundAllocError
! !ARGUMENTS:
!	
	integer, intent(in)                         :: statusToCheck
	integer, intent(in), optional               :: line
	character(len=*), intent(in), optional      :: file
	character(len=*), intent(in), optional      :: method
	integer, intent(out),optional               :: rcToReturn
	type(ESMF_Log),intent(inout),optional	    :: log

! !DESCRIPTION:
!      This function returns a logical true when a Fortran status code
!      returned from a memory allocation indicates an allocation error.  
!      An ESMF predefined memory allocation error 
!      message will be added to the {\tt ESMF\_Log} along with {\tt line}, 
!      {\tt file} and {\tt method}.  Additionally, the 
!      {\tt statusToCheck} will be converted to a {\tt rcToReturn}.
!
!      The arguments are:
!      \begin{description}
! 	
!      \item [statusToCheck]
!            Fortran allocation status to check.
!      \item [{[line]}]
!            Integer source line number.  Expected to be set by
!            using the preprocessor macro {\tt \_\_LINE\_\_} macro.
!      \item [{[file]}]
!            User-provided source file name. 
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[rcToReturn]}]
!            If specified, set the {\tt rcToReturn} value to 
!            {\tt ESMF\_RC\_MEM} which is the error code for a memory 
!            allocation eror.
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!	     of the default Log.
!      
!      \end{description}
! 
!EOP
    character(len=ESMF_MAXSTR)::tempmsg
    character(len=ESMF_MAXSTR)::allocmsg
	integer::msglen=0
	
    ESMF_INIT_CHECK_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)
    ESMF_LogFoundAllocError=.FALSE.
    if (statusToCheck .NE. 0) then
        if (present(rcToReturn)) then
            rcToReturn=ESMF_RC_MEM
        endif
        call c_esmc_loggeterrormsg(ESMF_RC_MEM,tempmsg,msglen)
        allocmsg=tempmsg(1:msglen)
	call ESMF_LogWrite(trim(allocmsg),ESMF_LOG_ERROR,line,file,method,log)
	ESMF_LogFoundAllocError=.TRUE.
    else
        if (present(rcToReturn)) then
            rcToReturn=ESMF_SUCCESS
        endif
    endif	
       
end function ESMF_LogFoundAllocError

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogFoundError - Check ESMF return code for error

! !INTERFACE: 
	function ESMF_LogFoundError(rcToCheck, line, file, method,& 
	         rcToReturn, log)
!
! !RETURN VALUE:
	logical                                         ::ESMF_LogFoundError
! !ARGUMENTS:
!	
	integer, intent(in)                             :: rcToCheck
	integer, intent(in), optional                   :: line
	character(len=*), intent(in), optional          :: file
	character(len=*), intent(in), optional	        :: method
	integer, intent(out), optional                  :: rcToReturn
	type(ESMF_Log),intent(inout), target, optional  :: log
	
! !DESCRIPTION:
!      This function returns a logical true for ESMF return codes that indicate 
!      an error.  A predefined error message will added to the {\tt ESMF\_Log} 
!      along with {\tt line}, {\tt file} and {\tt method}.  Additionally, 
!      {\tt rcToReturn} will be set to {\tt rcToCheck}.
!
!      The arguments are:
!      \begin{description}
! 	
!      \item [rcToCheck]
!            Return code to check.
!      \item [{[line]}]
!            Integer source line number.  Expected to be set by
!            using the preprocessor macro {\tt \_\_LINE\_\_} macro.
!      \item [{[file]}]
!            User-provided source file name. 
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[rcToReturn]}]
!            If specified, copy the {\tt rcToCheck} value to {\tt rc}.
!            This is not the return code for this function; it allows
!            the calling code to do an assignment of the error code
!            at the same time it is testing the value.
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!	     of the default Log.
!      
!      \end{description}
! 
!EOP
	
    integer:: i
    logical:: masked = .false.
    type(ESMF_LogPrivate), pointer          :: alog

    ESMF_INIT_CHECK_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)

    nullify(alog) ! ensure that the association status is well defined
    
    if (present(log)) then
      if(log%logTableIndex.gt.0) then
         alog => ESMF_LogTable(log%logTableIndex)
      endif
    else
      alog => ESMF_LogTable(ESMF_LogDefault%logTableIndex)
    endif
    
    if (associated(alog)) then

      ESMF_INIT_CHECK_SHALLOW(ESMF_LogPrivateGetInit,ESMF_LogPrivateInit,alog)

      ! set default returns
      ESMF_LogFoundError = .FALSE.
      if (present(rcToReturn)) rcToReturn = ESMF_SUCCESS
    
      ! check the error code
      if (rcToCheck .NE. ESMF_SUCCESS) then
        do i=1, alog%errorMaskCount
          if (alog%errorMask(i) .eq. rcToCheck) masked = .true.
        enddo
        if (.not.masked) then
          call ESMF_LogWrite("StandardError",ESMF_LOG_ERROR,line,file,method,&
	  log)
          ESMF_LogFoundError = .TRUE.
          if (present(rcToReturn)) rcToReturn = rcToCheck
        endif
      endif
    
    else    
      if (present(rcToReturn)) rcToReturn = ESMF_SUCCESS
    endif
       
end function ESMF_LogFoundError

!--------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LogGet - Return information about a log object

! !INTERFACE: 
	subroutine ESMF_LogGet(log,verbose,flush,rootOnly,halt,logtype,stream,&
	                       maxElements,rc)
!
! !ARGUMENTS:
!	
        type(ESMF_Log), target,optional			        :: log
	type(ESMF_Logical), intent(out),optional		:: verbose
	type(ESMF_Logical), intent(out),optional		:: flush
	type(ESMF_Logical), intent(out),optional		:: rootOnly
	type(ESMF_HaltType), intent(out),optional               :: halt
	type(ESMF_LogType), intent(out),optional	        :: logtype
	integer, intent(out),optional			        :: stream  
	integer, intent(out),optional			        :: maxElements
	integer, intent(out),optional			        :: rc
	

! !DESCRIPTION:
!      This subroutine returns properties about a Log object.
!
!      The arguments are:
!      \begin{description}
!
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!            of the default Log.
!      \item [{[verbose]}]
!            Verbose flag.
!      \item [{[flush]}]
!            Flush flag.
!      \item [{[rootOnly]}]
!	     Root only flag.
!      \item [{[halt]}]
!            Halt definition, with the following valid values:
!            \begin{description}
!              \item {\tt ESMF\_LOG\_HALTWARNING};
!              \item {\tt ESMF\_LOG\_HALTERROR};
!              \item {\tt ESMF\_LOG\_HALTNEVER}.
!            \end{description}
!      \item [{[logtype]}]
!            Defines either single or multilog.
!      \item [{[stream]}]
!            The type of stream, with the following valid values and meanings:
!            \begin{description}
!              \item 0 \  free;
!              \item 1 \  preordered.
!            \end{description}
!      \item [{[maxElements]}]
!            Maximum number of elements in the Log.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOPI

        type(ESMF_LogPrivate),pointer          :: alog

        ! Initialize return code; assume routine not implemented
	if (present(rc)) then
          rc=ESMF_FAILURE
        endif

        ESMF_INIT_CHECK_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)

    nullify(alog) ! ensure that the association status is well defined
    
    if (present(log)) then
      if(log%logTableIndex.gt.0) then
         alog => ESMF_LogTable(log%logTableIndex)
      endif
    else
      alog => ESMF_LogTable(ESMF_LogDefault%logTableIndex)
    endif
    
    if (associated(alog)) then

      ESMF_INIT_CHECK_SHALLOW(ESMF_LogPrivateGetInit,ESMF_LogPrivateInit,alog)

	if (present(verbose)) then
          verbose=alog%verbose
        endif
	if (present(flush)) then
          flush=alog%flushImmediately
        endif
	if (present(rootOnly)) then
          rootOnly=alog%rootOnly
        endif
	if (present(halt)) then
          halt=alog%halt
        endif
	if (present(logtype)) then
          logtype=alog%logtype
        endif
	if (present(stream)) then
          stream=alog%stream
        endif
	if (present(maxElements)) then
          maxElements=alog%maxElements	
        endif

	if (present(rc)) then
          rc=ESMF_SUCCESS
        endif
    endif

end subroutine ESMF_LogGet

!--------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LogInitialize - Initialize Log file(s)

! !INTERFACE: 
      subroutine ESMF_LogInitialize(filename, logtype, rc)
!
! !ARGUMENTS:
      character(len=*)                                  :: filename
      type(ESMF_LogType), intent(in),optional           :: logtype  
      integer, intent(out),optional	                :: rc

! !DESCRIPTION:
!      This routine initializes the global default {\tt ESMF\_Log}.  
!      The default {\tt ESMF\_Log} is assigned the {\tt filename} and 
!      is associated with an open Fortran unit number.
!
!      The arguments are:
!      \begin{description}
! 
!      \item [filename]
!            Name of file.  Maximum length 26 characters to allow for
!            the PET number to be added and keep the total file name
!            length under 32 characters.
!      \item [{[logtype]}]
!            Specifies {\tt ESMF\_LOG\_SINGLE}, {\tt ESMF\_LOG\_MULTI} or
!            {\tt ESMF\_LOG\_NONE}.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOPI
    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    call ESMF_LogOpen(ESMF_LogDefault, filename, logtype, rc)

end subroutine ESMF_LogInitialize

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogMsgFoundAllocError - Check Fortran status for allocation 
!            error and write message

! !INTERFACE: 
	function ESMF_LogMsgFoundAllocError(statusToCheck,msg,line,file, &
                                            method,rcToReturn,log)
!
! !RETURN VALUE:
	logical                                     ::ESMF_LogMsgFoundAllocError
! !ARGUMENTS:
!	
	integer, intent(in)                         :: statusToCheck
	character(len=*), intent(in)                :: msg
	integer, intent(in), optional               :: line
	character(len=*), intent(in), optional      :: file
	character(len=*), intent(in), optional	    :: method
        integer, intent(out),optional               :: rcToReturn	
        type(ESMF_Log), intent(inout), optional	    :: log

! !DESCRIPTION:
!      This function returns a logical true when a Fortran status code
!      returned from a memory allocation indicates an allocation error.
!      An ESMF predefined memory allocation error message 
!      will be added to the {\tt ESMF\_Log} along with a user added {\tt msg}, 
!      {\tt line}, {\tt file} and 
!      {\tt method}.  Additionally, statusToCheck will be converted to 
!      {\tt rcToReturn}.
!
!      The arguments are:
!      \begin{description}
! 	
!      \item [statusToCheck]
!            Fortran allocation status to check.
!      \item [msg]
!            User-provided message string.
!      \item [{[line]}]
!            Integer source line number.  Expected to be set by
!            using the preprocessor macro {\tt \_\_LINE\_\_} macro.
!      \item [{[file]}]
!            User-provided source file name. 
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[rcToReturn]}]
!            If specified, set the {\tt rcToReturn} value to 
!            {\tt ESMF\_RC\_MEM} which is the error code for a memory 
!            allocation eror.
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!	     of the default Log.
!      
!      \end{description}
! 
!EOP
    character(len=ESMF_MAXSTR)::tempmsg
    character(len=ESMF_MAXSTR)::allocmsg
    integer::msglen=0
    
    ESMF_INIT_CHECK_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)
    ESMF_LogMsgFoundAllocError=.FALSE.
    if (statusToCheck .NE. 0) then
        call c_esmc_loggeterrormsg(ESMF_RC_MEM,tempmsg,msglen)
	if (present(rcToReturn)) then
            rcToReturn=ESMF_RC_MEM
        endif
        allocmsg=tempmsg(1:msglen)
	call ESMF_LogWrite(trim(allocmsg)//" "//msg,ESMF_LOG_ERROR,line,file,method,log)	
	ESMF_LogMsgFoundAllocError=.TRUE.
#ifdef ESMF_SUCCESS_DEFAULT_ON
    else
        if (present(rcToReturn)) then
            rcToReturn=ESMF_SUCCESS
        endif
#endif
    endif	
       
end function ESMF_LogMsgFoundAllocError

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogMsgFoundError - Check ESMF return code for error and write message

! !INTERFACE: 
	function ESMF_LogMsgFoundError(rcToCheck, msg, line, file, method, &
                                       rcToReturn, log)
!
! !RETURN VALUE:
	logical                                         ::ESMF_LogMsgFoundError
! !ARGUMENTS:
!	
	integer, intent(in)                             :: rcToCheck
	character(len=*), intent(in)                    :: msg
	integer, intent(in), optional                   :: line
	character(len=*), intent(in), optional          :: file
	character(len=*), intent(in), optional	        :: method
	integer, intent(out),optional                   :: rcToReturn
	type(ESMF_Log), intent(inout), target, optional    :: log
	

! !DESCRIPTION:
!      This function returns a logical true for ESMF return codes that indicate
!      an error.  A predefined error message will added to the {\tt ESMF\_Log} 
!      along with
!      a user added {\tt msg}, {\tt line}, {\tt file} and {\tt method}.  
!      Additionally, {\tt rcToReturn} is set to {\tt rcToCheck}.
!
!      The arguments are:
!      \begin{description}
! 	
!      \item [rcToCheck]
!            Return code to check.
!      \item [msg]
!            User-provided message string.
!      \item [{[line]}]
!            Integer source line number.  Expected to be set by
!            using the preprocessor macro {\tt \_\_LINE\_\_} macro.
!      \item [{[file]}]
!            User-provided source file name. 
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[rcToReturn]}]
!            If specified, copy the {\tt rcToCheck} value to {\tt rc}.
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
    logical:: masked = .false.
    type(ESMF_LogPrivate), pointer          :: alog
    character(len=ESMF_MAXSTR)::tempmsg
    character(len=ESMF_MAXSTR)::allocmsg
    integer::msglen=0

    ESMF_INIT_CHECK_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)

    nullify(alog) ! ensure that the association status is well defined
    
    if (present(log)) then
      if(log%logTableIndex.gt.0) then
         alog => ESMF_LogTable(log%logTableIndex)
      endif
    else
      alog => ESMF_LogTable(ESMF_LogDefault%logTableIndex)
    endif
    
    if (associated(alog)) then

      ESMF_INIT_CHECK_SHALLOW(ESMF_LogPrivateGetInit,ESMF_LogPrivateInit,alog)

      ! set default returns
      ESMF_LogMsgFoundError = .FALSE.
#ifdef ESMF_SUCCESS_DEFAULT_ON	
      if (present(rcToReturn)) rcToReturn = ESMF_SUCCESS
#endif
    
      ! check the error code
      if (rcToCheck .NE. ESMF_SUCCESS) then
        do i=1, alog%errorMaskCount
          if (alog%errorMask(i) .eq. rcToCheck) masked = .true.
        enddo
        if (.not.masked) then
          call c_esmc_loggeterrormsg(rcToCheck,tempmsg,msglen)
          allocmsg=tempmsg(1:msglen)
	  call ESMF_LogWrite(trim(allocmsg)//" "//msg,ESMF_LOG_ERROR,line,file,&
            method,log)	
          ESMF_LogMsgFoundError=.TRUE.
          if (present(rcToReturn)) rcToReturn = rcToCheck
        endif
      endif
#ifdef ESMF_SUCCESS_DEFAULT_ON	
    else    
      if (present(rcToReturn)) rcToReturn = ESMF_SUCCESS
#endif
    endif
       
end function ESMF_LogMsgFoundError

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogMsgSetError - Set ESMF return code for error and write msg

! !INTERFACE: 
	subroutine ESMF_LogMsgSetError(rcValue, msg, line, file, method, &
                                       rcToReturn, log)

! !ARGUMENTS:
!	
	integer, intent(in)                             :: rcValue
	character(len=*), intent(in)                    :: msg
	integer, intent(in), optional                   :: line
	character(len=*), intent(in), optional          :: file
	character(len=*), intent(in), optional	        :: method
	integer, intent(out),optional                   :: rcToReturn
	type(ESMF_Log), intent(inout), target, optional    :: log
	

! !DESCRIPTION:
!      This subroutine sets the {\tt rcToReturn} value to {\tt rcValue} if
!      {\tt rcToReturn} is present and writes this error code to the {\tt ESMF\_Log}
!      if an error is generated.  A predefined error message will added to the 
!      {\tt ESMF\_Log} along with a user added {\tt msg}, {\tt line}, {\tt file}
!      and {\tt method}.  
!
!      The arguments are:
!      \begin{description}
! 	
!      \item [rcValue]
!            rc value for set
!      \item [msg]
!            User-provided message string.
!      \item [{[line]}]
!            Integer source line number.  Expected to be set by
!            using the preprocessor macro {\tt \_\_LINE\_\_} macro.
!      \item [{[file]}]
!            User-provided source file name. 
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[rcToReturn]}]
!            If specified, copy the {\tt rcValue} value to {\tt rcToreturn}.
!            This is not the return code for this function; it allows
!            the calling code to do an assignment of the error code
!            at the same time it is testing the value.
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!	     of the default Log.
!      
!      \end{description}
! 
!EOP

    integer:: i
    logical:: masked = .false.
    type(ESMF_LogPrivate), pointer          :: alog
    character(len=ESMF_MAXSTR)::tempmsg
    character(len=ESMF_MAXSTR)::allocmsg
    integer::msglen=0

    ESMF_INIT_CHECK_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)

    nullify(alog) ! ensure that the association status is well defined
    
    if (present(log)) then
      if(log%logTableIndex.gt.0) then
         alog => ESMF_LogTable(log%logTableIndex)
      endif
    else
      alog => ESMF_LogTable(ESMF_LogDefault%logTableIndex)
    endif
    
    if (associated(alog)) then

      ESMF_INIT_CHECK_SHALLOW(ESMF_LogPrivateGetInit,ESMF_LogPrivateInit,alog)

      ! set default returns
      if (present(rcToReturn)) rcToReturn = ESMF_SUCCESS
	
      ! check the error code
      if (rcValue .NE. ESMF_SUCCESS) then
        do i=1, alog%errorMaskCount
          if (alog%errorMask(i) .eq. rcValue) masked = .true.
        enddo
        if (.not.masked) then
          call c_esmc_loggeterrormsg(rcValue,tempmsg,msglen)
          allocmsg=tempmsg(1:msglen)
	  call ESMF_LogWrite(trim(allocmsg)//" "//msg,ESMF_LOG_ERROR,line,file,&
            method,log)	
          if (present(rcToReturn)) rcToReturn = rcValue
        endif
      endif	
    else    
      if (present(rcToReturn)) rcToReturn = ESMF_SUCCESS
    endif
       
end subroutine ESMF_LogMsgSetError


!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogOpen - Open Log file(s)

! !INTERFACE: 
    subroutine ESMF_LogOpen(log, filename, logtype, rc)
!
! !ARGUMENTS:
    type(ESMF_Log)			                :: log
    character(len=*)			                :: filename
    type(ESMF_LogType), intent(in),optional             :: logtype  
    integer, intent(out),optional	                :: rc

! !DESCRIPTION:
!      This routine opens a file with {\tt filename} and associates
!      it with the {\tt ESMF\_Log}.  This is only
!      used when the user does not want to use the default Log.
!
!      The arguments are:
!      \begin{description}
! 
!      \item [log]
!            An {\tt ESMF\_Log} object.
!      \item [filename]
!            Name of file.  Maximum length 26 characters to allow for
!            the PET number to be added and keep the total file name
!            length under 32 characters.
!      \item [{[logtype]}]
!            Set the logtype. See section \ref{opt:logtype} for a list of
!            valid options.
!            If not specified, defaults to {\tt ESMF\_LOG\_MULTI}.
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

    integer 				                   :: status, i, j, rc2
    type(ESMF_LogEntry), dimension(:), pointer             :: localbuf
    character(len=32)                                      :: fname
    character(len=4)                                       :: fnum
    character(ESMF_MAXSTR)                                 :: petNumChar
    logical                                                :: inuse

    type(ESMF_LogPrivate),pointer     :: alog

    ESMF_INIT_CHECK_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)

    ! Initialize return code; assume routine not implemented
    if (present(rc)) then
        rc=ESMF_FAILURE
    endif

    if(log%logTableIndex.gt.0) then
      alog => ESMF_LogTable(log%logTableIndex)
    else
      ESMF_LogTableCount = ESMF_LogTableCount + 1   ! counting number of files
      log%logTableIndex = ESMF_LogTableCount                ! Assign log
      alog => ESMF_LogTable(log%logTableIndex)
    endif
        
    ESMF_INIT_CHECK_SHALLOW(ESMF_LogPrivateGetInit,ESMF_LogPrivateInit,alog)

    ! Test if it is open or closed
    if (alog%FileIsOpen .eq. ESMF_TRUE) then
       	print *, "This ESMF_Log is already open with file '", &
      		 trim(ESMF_LogTable(log%logTableIndex)%nameLogErrFile), "'"
	return
    endif

    alog%maxElements = 10
    alog%stream = 0
    alog%fIndex = 1

    call f_ESMF_VMGlobalGet(alog%petNumber)
    ! Convert PET to contiguous character label
    write(petNumChar, *) alog%petNumber
    alog%petNumLabel = "PET" // trim(adjustl(petNumChar))

    alog%stopprogram = .false.
    alog%flushImmediately = ESMF_FALSE
    alog%rootOnly = ESMF_FALSE
    alog%verbose = ESMF_FALSE
    alog%flushed = ESMF_FALSE
    alog%dirty = ESMF_FALSE
    alog%FileIsOpen=ESMF_FALSE
    alog%halt=ESMF_LOG_HALTNEVER
    nullify(alog%errorMask)
    alog%errorMaskCount=0
    if (present(logtype)) then
      	alog%logtype=logtype
    else
        alog%logtype=ESMF_LOG_MULTI
    endif
    
  if(alog%logtype .ne. ESMF_LOG_NONE) then
    	
    if (alog%logtype .eq. ESMF_LOG_SINGLE) then
        alog%nameLogErrFile=trim(filename)
    else
        if (alog%petNumber .le. 9) then
	    write(fnum,11) alog%petNumber 
        else if (alog%petNumber .le. 99) then
            write(fnum,21) alog%petNumber
        else if (alog%petNumber .le. 999) then
            write(fnum,31) alog%petNumber
        else if (alog%petNumber .le. 9999) then
            write(fnum,41) alog%petNumber
        else if (alog%petNumber .le. 99999) then
            write(fnum,51) alog%petNumber
        else if (alog%petNumber .le. 999999) then
            write(fnum,61) alog%petNumber
        else 
            write(fnum,*) alog%petNumber
        endif
        fname = "PET" // trim(fnum) // "." // trim(filename)
        alog%nameLogErrFile=fname
    endif
 11     format(I1.1)
 21     format(I2.2)
 31     format(I3.3)
 41     format(I4.4)
 51     format(I5.5)
 61     format(I6.6)
    if (len(alog%nameLogErrFile) .gt. 32) then
        print *, "Filename exceeded 32 characters."
        if (present(rc)) then
            rc = ESMF_FAILURE
        endif
        return
    endif

    ! find an available unit number
    do j=ESMF_LOG_FORT_STDOUT, ESMF_LOG_UPPER
        inquire(unit=j, opened=inuse, iostat=status)
        if (.not. inuse .and. status .eq. 0) then
	  alog%unitNumber= j
          exit
     	endif
    enddo

    ! if no available unit number then error out
    if (status .ne. 0) then
        if (present(rc)) then
            rc=ESMF_FAILURE
        endif
        return
    endif

    ! open the file, with retries
    do i=1, ESMF_LOG_MAXTRYOPEN
        OPEN(UNIT=alog%unitNumber,File=alog%nameLogErrFile,& 
	     POSITION="APPEND", ACTION="WRITE", STATUS="UNKNOWN", IOSTAT=status)
        if (status.eq.0) then
            alog%FileIsOpen = ESMF_TRUE
            exit
        endif
    enddo

    ! if unable to open file then error out
    if (alog%FileIsOpen .ne. ESMF_TRUE) then
        if (present(rc)) then
            rc=ESMF_FAILURE
        endif
        return
    endif

    ! BEWARE:  absoft 8.0 compiler bug - if you try to allocate directly
    ! you get an error.  if you allocate a local buffer and then point the
    ! derived type buffer at it, it works.  go figure.
    
    allocate(localbuf(alog%maxElements), stat=status)
    if (status .ne. 0) then
      print *, "Allocation of buffer failed."
      if (present(rc)) then
          rc = ESMF_FAILURE
      endif
      return
    endif
    alog%LOG_ENTRY => localbuf
    
  endif
  
    !TODO: this is really strange because every time ESMF_LogOpen() is called
    !TODO: the _default_ Log on the C side is initialized, odd, isn't it? *gjt*
    call c_ESMC_LogInitialize(filename,alog%petNumber,alog%logtype,rc2)
    if (present(rc)) then
        rc=ESMF_SUCCESS    
    endif
    
end subroutine ESMF_LogOpen	

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogSet - Set Log parameters

! !INTERFACE: 
	subroutine ESMF_LogSet(log,verbose,flush,rootOnly,halt, &
                               stream,maxElements,errorMask,rc)
!
! !ARGUMENTS:
!	
	type(ESMF_Log), target,optional                         :: log
	type(ESMF_Logical), intent(in),optional			:: verbose
	type(ESMF_Logical), intent(in),optional			:: flush
	type(ESMF_Logical), intent(in),optional			:: rootOnly
	type(ESMF_HaltType), intent(in),optional                :: halt
	integer, intent(in),optional			        :: stream  
	integer, intent(in),optional			        :: maxElements
	integer, intent(in),optional			        :: errorMask(:)
	integer, intent(out),optional			        :: rc
	
! !DESCRIPTION:
!      This subroutine sets the properties for the Log object.
!
!      The arguments are:
!      \begin{description}
!
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!            of the default Log.
!      \item [{[verbose]}]
!            Verbose flag.
!      \item [{[rootOnly]}]
!	     Root only flag.
!      \item [{[halt]}]
!	     Halt definition, with the following valid values:
!            \begin{description}
!              \item {\tt ESMF\_LOG\_HALTWARNING};
!              \item {\tt ESMF\_LOG\_HALTERROR};
!              \item {\tt ESMF\_LOG\_HALTNEVER}.
!            \end{description}
!      \item [{[stream]}]
!            The type of stream, with the following valid values and meanings:
!            \begin{description}
!              \item 0 \  free;
!              \item 1 \  preordered. 
!            \end{description}
!      \item [{[maxElements]}]
!            Maximum number of elements in the Log.
!      \item [{[errorMask]}]
!            List of error codes that will {\em not} be logged as errors.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOP
    integer :: i, status, status2
    logical :: isDefault=.false.
    type(ESMF_LogPrivate), pointer          :: alog
    type(ESMF_LogEntry), dimension(:), pointer :: localbuf

    ESMF_INIT_CHECK_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)
    
    nullify(alog) ! ensure that the association status is well defined
    
    if (present(log)) then
      if(log%logTableIndex.gt.0) then
         alog => ESMF_LogTable(log%logTableIndex)
      endif
    else
      alog => ESMF_LogTable(ESMF_LogDefault%logTableIndex)
      isDefault = .true.
    endif
    
    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    if (associated(alog)) then

      ESMF_INIT_CHECK_SHALLOW(ESMF_LogPrivateGetInit,ESMF_LogPrivateInit,alog)

      if (alog%FileIsOpen .ne. ESMF_TRUE) then
        print *, "ESMF_Log not open -- cannot ESMF_LogSet()."
        return
      endif
    
      if (present(verbose)) then
        alog%verbose=verbose
      endif
      if (present(flush)) then
        alog%flushImmediately=flush
      endif
      if (present(rootOnly)) then
        alog%rootOnly=rootOnly
      endif
      if (present(halt)) then
        alog%halt=halt
      endif
      if (present(stream)) then
        alog%stream=stream
      endif
      if (present(maxElements)) then
        if (maxElements.gt.0 .AND. alog%maxElements.ne.maxElements) then
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
        if (alog%errorMaskCount .gt. 0) then
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

      if (present(rc)) then
        rc=ESMF_SUCCESS 
      endif
    endif

end subroutine ESMF_LogSet


!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogWrite - Write to Log file(s)

! !INTERFACE: 
	recursive subroutine ESMF_LogWrite(msg,MsgType,line,file,method,log,rc)
!
!
! !ARGUMENTS:
	character(len=*), intent(in)                :: msg
	type(ESMF_MsgType), intent(in)              :: msgtype
	integer, intent(in), optional               :: line
	character(len=*), intent(in), optional      :: file
	character(len=*), intent(in), optional	    :: method
	type(ESMF_Log),target,optional   	    :: log
	integer, intent(out),optional		    :: rc

! !DESCRIPTION:
!      This subroutine writes to the file associated with an {\tt ESMF\_Log}.
!      A message is passed in along with the {\tt msgtype}, {\tt line}, 
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
!      \item [msgtype]
!            The type of message.  See Section~\ref{opt:msgtype} for
!            possible values.
!      \item [{[line]}]
!            Integer source line number.  Expected to be set by
!            using the preprocessor macro {\tt \_\_LINE\_\_} macro.
!      \item [{[file]}]
!            User-provided source file name. 
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!	     of the default Log.
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
    
    character(len=10)               :: t
    character(len=8)                :: d
    !character(len=7)               :: lt
    character(len=32)               ::tmethod,tfile
    integer			    ::tline
    integer                         ::h,m,s,ms,y,mn,dy
    integer			    ::rc2,index
    type(ESMF_LogPrivate), pointer    :: alog
    
    ESMF_INIT_CHECK_SHALLOW(ESMF_LogGetInit,ESMF_LogInit,log)
    
    nullify(alog) ! ensure that the association status is well defined
    
    if (present(log)) then
      if(log%logTableIndex.gt.0) then
         alog => ESMF_LogTable(log%logTableIndex)
      endif
    else
      alog => ESMF_LogTable(ESMF_LogDefault%logTableIndex)
    endif

    ! Initialize return code; assume routine not implemented
    if (present(rc)) then
      rc=ESMF_RC_NOT_IMPL
    endif

    if (associated(alog)) then

      if (alog%logtype .ne. ESMF_LOG_NONE) then

        if (alog%FileIsOpen .ne. ESMF_TRUE) then
          print *, "ESMF_Log not open -- cannot ESMF_LogWrite()."
          rc=ESMF_FAILURE
          return
        endif

        index = alog%fIndex
      
    	alog%dirty = ESMF_TRUE    
    	call c_esmc_timestamp(y,mn,dy,h,m,s,ms)
    	call DATE_AND_TIME(d,t)	
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
    	select case (msgtype%mtype)
        case (1)
    	    alog%LOG_ENTRY(index)%lt="INFO"
        case (2)
    	    alog%LOG_ENTRY(index)%lt="WARNING"
   	case default
    	    alog%LOG_ENTRY(index)%lt="ERROR"
    	end select	
    	alog%LOG_ENTRY(alog%fIndex)%d = d
    	alog%LOG_ENTRY(alog%fIndex)%h = h
    	alog%LOG_ENTRY(alog%fIndex)%m = m
    	alog%LOG_ENTRY(alog%fIndex)%s = s
    	alog%LOG_ENTRY(alog%fIndex)%ms = ms	
    	alog%LOG_ENTRY(alog%fIndex)%msg = msg
	alog%flushed = ESMF_FALSE	
    	if ((ESMF_LogTable(1)%halt .eq. ESMF_LOG_HALTERROR).and. (msgtype .eq. ESMF_LOG_ERROR)) then
        	alog%stopprogram=.TRUE.
        	call ESMF_LogClose(ESMF_LogDefault,rc=rc2)
    	endif    	 
    	if ((alog%halt .eq. ESMF_LOG_HALTWARNING).and. (msgtype .gt. ESMF_LOG_WARNING)) then
        	alog%stopprogram=.TRUE.
        	call ESMF_LogClose(log,rc=rc2)
    	endif
    	if (alog%fIndex .eq. alog%maxElements .or. &
            alog%flushImmediately .eq. ESMF_TRUE) then
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
    endif
end subroutine ESMF_LogWrite

!--------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LogEntryCopy - Copy a Log entry

! !INTERFACE: 
	subroutine ESMF_LogEntryCopy(logEntryIn, logEntryOut, rc)
!
! !ARGUMENTS:
        type(ESMF_LogEntry), intent(inout)  :: logEntryIn
        type(ESMF_LogEntry), intent(out) :: logEntryOut
        integer, intent(out), optional   :: rc

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
    
    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_SHALLOW(ESMF_LogEntryGetInit,ESMF_LogEntryInit,logEntryIn)

    logEntryOut%h    = logEntryIn%h
    logEntryOut%m    = logEntryIn%m
    logEntryOut%s    = logEntryIn%s
    logEntryOut%ms   = logEntryIn%ms
    logEntryOut%line = logEntryIn%line

    logEntryOut%methodflag = logEntryIn%methodflag
    logEntryOut%lineflag   = logEntryIn%lineflag
    logEntryOut%fileflag   = logEntryIn%fileflag

    logEntryOut%msg    = logEntryIn%msg
    logEntryOut%file   = logEntryIn%file
    logEntryOut%method = logEntryIn%method
    logEntryOut%d      = logEntryIn%d
    logEntryOut%lt     = logEntryIn%lt
    
    if (present(rc)) then
        rc=ESMF_SUCCESS
    endif

end subroutine ESMF_LogEntryCopy

end module ESMF_LogErrMod

