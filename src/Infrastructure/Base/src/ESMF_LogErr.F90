! $Id: ESMF_LogErr.F90,v 1.12 2004/06/14 02:27:15 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
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
#include "ESMF_Conf.inc"
#include "ESMF_ErrReturnCodes.inc"
#include "ESMF_LogConstants.inc"
#include "ESMF_Macros.inc"
#include "ESMF_Version.inc"


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
    use ESMF_BaseTypesMod

implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!------------------------------------------------------------------------------
!     ! ESMF_MsgType
type ESMF_MsgType
    sequence
    integer      :: ftype
end type

!     ! Msg Types
type(ESMF_MsgType), parameter           ::  &
    ESMF_LOG_INFO  = ESMF_MsgType(1), &
    ESMF_LOG_WARNING = ESMF_MsgType(2), &
    ESMF_LOG_ERROR = ESMF_MsgType(3)
     
!     ! Log Entry                            
type ESMF_LOGENTRY
    private
    sequence
    character(len=32) context
    integer err_type
    character(len=32) mod_name
	integer number
	integer pe_number	
end type ESMF_LOGENTRY

!     ! Log  
type ESMF_Log
    private
    sequence       
                                            
    type(ESMF_Logical)                      ::  FileIsOpen
    type(ESMF_Logical)                      ::  flush
    integer                                     halt
    type(ESMF_LOGENTRY), dimension(1)       ::  LOG_ENTRY
    type(ESMF_MsgType)                      ::  msgtype ! TODO: this is wrong
    integer                                     maxElements
    character(len=32)                           nameLogErrFile 
    type(ESMF_Logical)                      ::  rootOnly
    integer                                     stream 
    integer                                     unitNumber 
    type(ESMF_Logical)                      ::  verbose
    
!If standardout not unit 6, must be changed here.
#ifndef ESMF_NO_INITIALIZERS
    type(ESMF_Logical)                      ::  fileIO=ESMF_FALSE
    
#else
    type(ESMF_Logical)                      ::  fileIO
#endif
end type ESMF_Log
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
    public ESMF_LOG_INFO
    public ESMF_LOG_WARNING
    public ESMF_LOG_ERROR
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
   public ESMF_Log
   public ESMF_LogClose
   public ESMF_LogFinalize
   public ESMF_LogFoundAllocError
   public ESMF_LogFoundError
   public ESMF_LogGet
   public ESMF_LogInitialize
   public ESMF_LogMsgFoundAllocError
   public ESMF_LogMsgFoundError
   public ESMF_LogOpen
   public ESMF_LogSet
   public ESMF_LogWrite

!EOPI  

type(ESMF_Log),SAVE::ESMF_LogDefault	
!----------------------------------------------------------------------------

contains

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogClose - Close Log file(s)

! !INTERFACE: 
    subroutine ESMF_LogClose(log, rc)
!
! !ARGUMENTS:
    type(ESMF_Log)	            :: log
    integer, intent(out),optional   :: rc

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

	character(len=10) 					:: t
	character(len=8) 					:: d
	
	if (present(rc)) then
	  	rc=ESMF_FAILURE
	endif
	if (log%FileIsOpen .eq. ESMF_TRUE) then
		!call DATE_AND_TIME(d,t)
		!WRITE(log%unitnumber,100) d,t,"INFO     Log Close"
		!100 FORMAT(a8,2x,a10,2x,a)
		!if (log%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  INFO       Log Close"
		!CLOSE(UNIT=log%stdOutUnitNumber)
		log%FileIsOpen=ESMF_FALSE
		if (present(rc)) rc=ESMF_SUCCESS
	endif	
	
end subroutine ESMF_LogClose

!--------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LogFinalize - Finalize Log file(s)

! !INTERFACE: 
	subroutine ESMF_LogFinalize(rc)
!
! !ARGUMENTS:
    integer, intent(out),optional	:: rc

! !DESCRIPTION:
!      This routine finalizes the global log.  The default log will be flushed
!      and the default log will be closed.
!
!      The arguments are:
!      \begin{description}
! 
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOPI
	if (present(rc)) then
	  	rc=ESMF_FAILURE
	endif
	if (ESMF_LogDefault%FileIsOpen .eq. ESMF_TRUE) then
!	FLUSH	
		ESMF_LogDefault%FileIsOpen=ESMF_FALSE
		if (present(rc)) then
			rc=ESMF_SUCCESS
		endif
	endif	
	
end subroutine ESMF_LogFinalize

!--------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_LogFoundAllocError - Check Fortran status for allocation error

! !INTERFACE: 
	function ESMF_LogFoundAllocError(statusToCheck, line, file, & 
                                         method, rcToReturn)
!
! !RETURN VALUE:
	logical                                 ::ESMF_LogFoundAllocError
! !ARGUMENTS:
!	
	integer, intent(in)                     :: statusToCheck
	integer, intent(in), optional           :: line
	character(len=*), intent(in), optional  :: file
	character(len=*), intent(in), optional  :: method
	integer, intent(out),optional           :: rcToReturn

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
!      \item [line]
!            cpp provided line number.
!      \item [file]
!            cpp provided file string.
!      \item [method]
!            cpp provided method string.
!      \item [rcToReturn]
!            Return code to return.
!      
!      \end{description}
! 
!EOP
    logical :: logrc
    character(len=ESMF_MAXSTR)::tempmsg
    character(len=ESMF_MAXSTR)::allocmsg
	integer::msglen=0
	
    ESMF_LogFoundAllocError=.FALSE.
    if (present(rcToReturn)) rcToReturn=statusToCheck
    if (statusToCheck .NE. 0) then
        call c_esmc_loggeterrormsg(ESMF_RC_MEM,tempmsg,msglen)
        allocmsg=tempmsg(1:msglen)
	    logrc = ESMF_LogWrite(trim(allocmsg),ESMF_LOG_ERROR,line,file,method)
	    if (.not. logrc) then
            print *, "Error writing previous error to log file"
            ! what now?  we're already in the error code...
            ! just fall through and return i guess.
        endif
	    ESMF_LogFoundAllocError=.TRUE.
    endif	
       
end function ESMF_LogFoundAllocError

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogFoundError - Check ESMF return code for error

! !INTERFACE: 
	function ESMF_LogFoundError(rcToCheck, line, file, method, rcToReturn)
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
!            User-provided source file name.  Expected to be set by
!            using the preprocessor {\tt \_\_FILE\_\_} macro.
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[rcToReturn]}]
!            If specified, copy the {\tt rcToCheck} value to {\tt rc}.
!            This is not the return code for this function; it allows
!            the calling code to do an assignment of the error code
!            at the same time it is testing the value.
!      
!      \end{description}
! 
!EOP
    logical :: logrc
	
    ESMF_LogFoundError = .FALSE.
    if (present(rcToReturn)) rcToReturn = rcToCheck
    if (rcToCheck .NE. ESMF_SUCCESS) then
        logrc = ESMF_LogWrite("StandardError",ESMF_LOG_ERROR,line,file,method)
        if (.not. logrc) then
            print *, "Error writing previous error to log file"
            ! what now?  we're already in the error code...
            ! just fall through and return i guess.
       endif
       ESMF_LogFoundError = .TRUE.
    endif	
       
end function ESMF_LogFoundError

!--------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LogGet - Return information about a log object

! !INTERFACE: 
	subroutine ESMF_LogGet(log,verbose,flush,rootOnly,halt,msgtype,stream,maxElements,rc)
!
! !ARGUMENTS:
!	
	type(ESMF_Log), intent(in) 		                :: log
	type(ESMF_Logical), intent(out),optional		:: verbose
	type(ESMF_Logical), intent(out),optional		:: flush
	type(ESMF_Logical), intent(out),optional		:: rootOnly
	integer, intent(out),optional		                :: halt
	type(ESMF_MsgType), intent(in),optional	                :: msgtype
	integer, intent(out),optional			        :: stream  
	integer, intent(out),optional			        :: maxElements
	integer, intent(out),optional			        :: rc
	

! !DESCRIPTION:
!      This subroutine returns properties about a log object.
!
!      The arguments are:
!      \begin{description}
!
!      \item [log]
!            Log object.
!      \item [verbose]
!            Verbose flag.
!      \item [rootOnly]
!	     Root only flag
!      \item [halt]
!	     Halt definitions (halterr(0), haltwarn(1),haltnever(2))
!      \item [msgtype]
!            TODO: This argument is wrong.
!      \item [stream]
!            The type of stream (free(0), preordered(1))
!      \item [maxElements]
!            Maximum number of elements in the log.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOPI
	if (present(rc)) then
	  rc=ESMF_FAILURE
	endif
	if (present(verbose)) then
	  verbose=log%verbose
	endif
	if (present(flush)) then
	  flush=log%flush
	endif
	if (present(rootOnly)) then
	  rootOnly=log%rootOnly
	endif
	if (present(halt)) then
	  halt=log%halt
	endif
	!if (present(msgtype)) then
	!  msgtype=log%msgtype
	!endif
	if (present(stream)) then
	  stream=log%stream
	endif
	if (present(maxElements)) then
	  maxElements=log%maxElements
	endif	
	if (present(rc)) then
	  rc=ESMF_SUCCESS 
	endif 
end subroutine ESMF_LogGet

!--------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LogInitialize - Initialize Log file(s)

! !INTERFACE: 
      subroutine ESMF_LogInitialize(filename, rc)
!
! !ARGUMENTS:
      character(len=*)                          :: filename
      integer, intent(out),optional	        :: rc

! !DESCRIPTION:
!      This routine initializes the global default {\tt ESMF\_Log}.  
!      The default {\tt ESMF\_Log} is assigned the {\tt filename} and 
!      is associated with an open Fortran unit number.
!
!      The arguments are:
!      \begin{description}
! 
!      \item [filename]
!            Name of file.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOPI
	
    integer 				                    :: status, i, rc2	
	
    if (present(rc)) rc=ESMF_FAILURE
    ESMF_LogDefault%FileIsOpen=ESMF_FALSE
    ESMF_LogDefault%nameLogErrFile=filename
    ESMF_LogDefault%unitnumber=ESMF_LOG_FORT_STDOUT   
    do i=ESMF_LogDefault%unitnumber, ESMF_LOG_UPPER
        inquire(unit=i,iostat=status)
        if (status .eq. 0) then
            ESMF_LogDefault%FileIsOpen = ESMF_TRUE
            exit
     	endif
   	enddo 
	if (ESMF_LogDefault%FileIsOpen .eq. ESMF_FALSE) return
	ESMF_LogDefault%unitNumber = i  
	call c_ESMC_LogInitialize(filename,rc2)
	if (present(rc)) rc=ESMF_SUCCESS
	
end subroutine ESMF_LogInitialize

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogMsgFoundAllocError - Check Fortran status for allocation error and write message

! !INTERFACE: 
	function ESMF_LogMsgFoundAllocError(statusToCheck,msg,line,file, &
                                            method,rcToReturn)
!
! !RETURN VALUE:
	logical                                 ::ESMF_LogMsgFoundAllocError
! !ARGUMENTS:
!	
	integer, intent(in)                     :: statusToCheck
	character(len=*), intent(in)            :: msg
	integer, intent(in), optional           :: line
	character(len=*), intent(in), optional  :: file
	character(len=*), intent(in), optional	:: method
    integer, intent(out),optional           :: rcToReturn	

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
!            User-provided context string.
!      \item [line]
!            cpp provided line number.
!      \item [file]
!            cpp provided file string.
!      \item [method]
!            cpp provided method string.
!      \item [rcToReturn]
!            Return code to Return.
!      
!      \end{description}
! 
!EOP
    logical :: logrc
    character(len=ESMF_MAXSTR)::tempmsg
    character(len=ESMF_MAXSTR)::allocmsg
	integer::msglen=0
    
    ESMF_LogMsgFoundAllocError=.FALSE.
    if (present(rcToReturn)) rcToReturn=statusToCheck
    if (statusToCheck .NE. 0) then
        call c_esmc_loggeterrormsg(ESMF_RC_MEM,tempmsg,msglen)
        allocmsg=tempmsg(1:msglen)
	    logrc = ESMF_LogWrite(trim(allocmsg)//msg,ESMF_LOG_ERROR,line,file,method)
	    if (.not. logrc) then
            print *, "Error writing previous error to log file"
            ! what now?  we're already in the error code...
            ! just fall through and return i guess.
        endif
	    ESMF_LogMsgFoundAllocError=.TRUE.
    endif	
       
end function ESMF_LogMsgFoundAllocError

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogMsgFoundError - Check ESMF return code for error and write message

! !INTERFACE: 
	function ESMF_LogMsgFoundError(rcToCheck, msg, line, file, method, &
                                       rcToReturn)
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
	

! !DESCRIPTION:
!      This function returns a logical true for ESMF return codes that indicate
!      an error.  A predefined error message will added to the {\tt ESMF\_Log} 
!      along with
!      a user added {\tt msg}, {\tt line}, {\tt file} and {\tt method}.  
!      Additionally, {\tt rcToReturn} will be set to {\tt rcToCheck}.
!
!      The arguments are:
!      \begin{description}
! 	
!      \item [rcToCheck]
!            Return code to check.
!      \item [msg]
!            User-provided context string.
!      \item [line]
!            cpp provided line number.
!      \item [file]
!            cpp provided file string.
!      \item [method]
!            cpp provided method string.
!      \item [rcToReturn]
!      \item [{[rcToReturn]}]
!            If specified, copy the {\tt rcToCheck} value to {\tt rc}.
!            This is not the return code for this function; it allows
!            the calling code to do an assignment of the error code 
!            at the same time it is testing the value.
!      
!      \end{description}
! 
!EOP
    logical :: logrc
	
    ESMF_LogMsgFoundError=.FALSE.
	if (present(rcToReturn)) rcToReturn = rcToCheck
	if (rcToCheck .NE. ESMF_SUCCESS) then
	    logrc = ESMF_LogWrite(msg,ESMF_LOG_ERROR,line,file,method)
        if (.not. logrc) then
            print *, "Error writing previous error to log file"
            ! what now?  we're already in the error code...
            ! just fall through and return i guess.
       endif
	   ESMF_LogMsgFoundError=.TRUE.
	endif	
       
end function ESMF_LogMsgFoundError

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogOpen - Open Log file(s)

! !INTERFACE: 
    subroutine ESMF_LogOpen(log, filename, rc)
!
! !ARGUMENTS:
    type(ESMF_Log)			    :: log
    character(len=*)			    :: filename
    integer, intent(out),optional	    :: rc

! !DESCRIPTION:
!      This routine opens a file with {\tt filename} and associates
!      it with the {\tt log}.  This is only
!      used when the user does not want to use the default {\tt ESMF\_Log}.
!
!      The arguments are:
!      \begin{description}
! 
!      \item [log]
!            An {\tt ESMF\_Log} object.
!      \item [filename]
!            Name of file.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOP
	
	integer 				    :: status, i
	character(len=10) 			:: t
	character(len=8) 			:: d
	
	if (present(rc)) rc=ESMF_FAILURE
	log%FileIsOpen=ESMF_FALSE
	log%nameLogErrFile=filename
	log%unitnumber=ESMF_LOG_FORT_STDOUT
	do i=log%unitnumber, ESMF_LOG_UPPER
        inquire(unit=i,iostat=status)
        if (status .eq. 0) then
            log%FileIsOpen = ESMF_TRUE
            exit
        endif
   	enddo 
	if (log%FileIsOpen .eq. ESMF_FALSE) return
	log%unitNumber = i  
	if (present(rc)) rc=ESMF_SUCCESS	
    
end subroutine ESMF_LogOpen	

!--------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LogSet - Set Log parameters

! !INTERFACE: 
	subroutine ESMF_LogSet(log,verbose,flush,rootOnly,halt,msgtype, &
                               stream,maxElements,rc)
!
! !ARGUMENTS:
!	
	type(ESMF_Log)		 				:: log
	type(ESMF_Logical), intent(in),optional			:: verbose
	type(ESMF_Logical), intent(in),optional			:: flush
	type(ESMF_Logical), intent(in),optional			:: rootOnly
	integer, intent(in),optional			        :: halt
	type(ESMF_MsgType), intent(in),optional                 :: msgtype
	integer, intent(in),optional			        :: stream  
	integer, intent(in),optional			        :: maxElements
	integer, intent(out),optional			        :: rc
	
! !DESCRIPTION:
!      This subroutine sets the properties for the log object.
!
!      The arguments are:
!      \begin{description}
!
!      \item [log]
!            Log object.
!      \item [verbose]
!            Verbose flag.
!      \item [rootOnly]
!	     Root only flag
!      \item [halt]
!	     Halt definitions (halterr(0), haltwarn(1),haltnever(2))
!      \item [msgtype]
!            TODO:  This argument is wrong.
!      \item [stream]
!            The type of stream (free(0), preordered(1))
!      \item [maxElements]
!            Maximum number of elements in the log.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOPI 
	if (present(rc)) rc=ESMF_FAILURE
	if (present(verbose)) log%verbose=verbose
	if (present(flush)) log%flush=flush
	if (present(rootOnly)) log%rootOnly=rootOnly
	if (present(halt)) log%halt=halt
	if (present(msgtype)) log%msgtype=msgtype
	if (present(stream)) log%stream=stream
	if (present(maxElements)) log%maxElements=maxElements
	if (present(rc)) rc=ESMF_SUCCESS 
end subroutine ESMF_LogSet

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogWrite - Write to Log file(s)

! !INTERFACE: 
	function ESMF_LogWrite(msg,MsgType,line,file,method)
!
!
! !RETURN VALUE:
	logical                                 ::ESMF_LogWrite
! !ARGUMENTS:
	character(len=*), intent(in)            :: msg
	type(ESMF_MsgType), intent(in)          :: msgtype
	integer, intent(in), optional           :: line
	character(len=*), intent(in), optional  :: file
	character(len=*), intent(in), optional	:: method

! !DESCRIPTION:
!      This function writes to the file associated with an {\tt ESMF\_Log}.
!      A message is
!      passed in along with the {\tt msgtype}, {\tt line}, {\tt file} and 
!      {\tt method}.  If the write to
!      the {\tt ESMF\_Log} is successful, the function will return a 
!      logical {\tt true}.  This function
!      is the base function used by all the other {\tt ESMF\_Log} 
!      writing methods.
!
!      The arguments are:
!      \begin{description}
! 
!      \item [msg]
!            User-provided context string.
!      \item [msgtype]
!            The type of message.  See Section~\ref{opt:msgtype} for
!            possible values.
!      \item [line]
!            cpp provided line number.
!      \item [file]
!            cpp provided file string.
!      \item [method]
!            User-provided method string.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOP
    
    character(len=10)               :: t
    character(len=8)                :: d
    character(len=7)                :: lt
    character(len=32)               :: f
    character(len=32)               ::tmethod,tfile
    integer			      	        ::status,tline
    integer                         ::ok
    integer	                        ::i
    integer                         ::h,m,s,ms,y,mn,dy
    
    ESMF_LogWrite=.FALSE.
    if (present(method)) tmethod=adjustl(method)
    if (present(line)) tline=line 
    if (present(file)) then
        tfile=adjustl(file)
    else
        tfile=""
    endif
    call c_esmc_timestamp(y,mn,dy,h,m,s,ms)
    
    if (ESMF_LogDefault%FileIsOpen .eq. ESMF_TRUE) then
    	call DATE_AND_TIME(d,t)	
    	select case (msgtype%ftype)
          case (1)
    	    lt="INFO"
          case (2)
    	    lt="WARNING"
   	  case default
    	    lt="ERROR"
   	end select				
    	ok=0
    	do i=1, ESMF_LOG_MAXTRYOPEN
    	    OPEN(UNIT=ESMF_LogDefault%unitnumber, &
                File=ESMF_LogDefault%nameLogErrFile, POSITION="APPEND", &
    		ACTION="WRITE", STATUS="UNKNOWN", IOSTAT=status)
            if (status.eq.0) then
    		if (present(line)) then								
    		    if (present(method)) then
    			WRITE(ESMF_LogDefault%unitnumber,122) d," ",h,&
                                m,s,".",ms," ",trim(lt)," ",trim(tfile)," ",&
    				tline," ",trim(tmethod)," ",trim(msg)
			!if (log%verbose .eq. ESMF_TRUE) print *, &
                        ! d,"  ",t,"  ", lt,"    ",tfile,tline,"  ",tmethod,msg
    		    else
    			WRITE(ESMF_LogDefault%unitnumber,123) d," ",h,&
                                m,s,".",ms," ",trim(lt)," ",trim(tfile)," ",&
    				tline," ",trim(msg)
			!if (log%verbose .eq. ESMF_TRUE) print *, &
                        ! d,"  ",t,"  ", lt,"    ",tfile,tline,"  ",msg
    		    endif	
            else
    		if (present(method)) then
    		    WRITE(ESMF_LogDefault%unitnumber,132) d," ",h,&
                        m,s,".",ms," ",trim(lt)," ",&
    			" ",trim(tmethod)," ",trim(msg)
		    !if (log%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  ",&
    		    !lt,"    ","  ",tmethod,msg
    		else
    		    WRITE(ESMF_LogDefault%unitnumber,133) d," ",h,&
                        m,s,".",ms," ",trim(lt)," "," ",trim(msg)
		    !if (log%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  ",&
    		    !lt,"    ","  ",msg
    		endif	
    	    endif
    	    CLOSE(UNIT=ESMF_LogDefault%unitnumber)
    	    ESMF_LogWrite=.TRUE.
    	    ok=1
    	endif	
    	if (ok.eq.1) exit    
   enddo
   endif
122  FORMAT(a8,a,i2.2,i2.2,i2.2,a,i6.6,a,a,a,a,a,i0,a,a,a,a)
123  FORMAT(a8,a,i2.2,i2.2,i2.2,a,i6.6,a,a,a,a,a,i0,a,a)
132  FORMAT(a8,a,i2.2,i2.2,i2.2,a,i6.6,a,a,a,a,a,a,a,a)
133  FORMAT(a8,a,i2.2,i2.2,i2.2,a,i6.6,a,a,a,a,a,a)

end function ESMF_LogWrite

end module ESMF_LogErrMod









