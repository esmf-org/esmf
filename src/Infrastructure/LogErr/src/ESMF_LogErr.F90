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
!! individually.  if we add files to ESMF.h they also need to be added here.
#include "ESMF_Macros.inc"
#include "ESMF_Conf.inc"
#include "ESMF_Version.inc"
#include "ESMF_ErrReturnCodes.inc"
#include "ESMF_LogConstants.inc"

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
   use ESMF_BaseMod
   
!------------------------------------------------------------------------------
!     ! ESMF_LogFileType
!
!     ! Log File Types


!EOPI
implicit none

type ESMF_LogFileType
    sequence
    integer                                 :: ftype
end type

type(ESMF_LogFileType), parameter ::  &
    ESMF_LOG_INFO  = ESMF_LogFileType(1), &
    ESMF_LOG_WARNING = ESMF_LogFileType(2), &
    ESMF_LOG_ERROR = ESMF_LogFileType(3)

                                  
type ESMF_LOGENTRY
    private
    sequence
	integer number
	integer pe_number
	integer err_type
	character(len=32) mod_name
	character(len=32) context
end type ESMF_LOGENTRY

type ESMF_Log
    private
    sequence       
                                            
    type(ESMF_Logical)                      ::  FileIsOpen
    type(ESMF_Logical)                      ::  flush
    integer                                     halt
    type(ESMF_LOGENTRY), dimension(1)       ::  LOG_ENTRY
    type(ESMF_LogFileType)                  ::  logtype
    integer                                     maxElements
    character(len=32)                           nameLogErrFile 
    type(ESMF_Logical)                      ::  rootOnly
    integer                                     stream 
    integer                                     unitNumber 
    type(ESMF_Logical)                      ::  verbose
    
!If standardout not unit 6, must be changed here.
#ifndef ESMF_NO_INITIALIZERS
    type(ESMF_Logical)                      ::  fileIO=ESMF_FALSE
    integer                                 ::  maxTryOpen=100000
    integer                                 ::  stdOutUnitNumber=6
    
#else
    type(ESMF_Logical)                      ::  fileIO
    integer                                 ::  maxTryOpen
    integer                                 ::  stdOutUnitNumber
#endif
end type ESMF_Log


   public ESMF_Log,ESMF_LogClose,ESMF_LogFinalize,ESMF_LogFoundAllocError,&
   ESMF_LogFoundError,ESMF_LogGet,ESMF_LogInitialize,&
   ESMF_LogMsgFoundAllocError,ESMF_LogMsgFoundError,ESMF_LogOpen,&
   ESMF_LogSet,ESMF_LogWrite

type(ESMF_Log),SAVE::ESMF_LogDefault	
!----------------------------------------------------------------------------

contains

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogClose - Close Log file(s)

! !INTERFACE: 
	subroutine ESMF_LogClose(aLog,rc)
!
! !ARGUMENTS:
      type(ESMF_Log)			:: aLog
      integer, intent(out),optional	:: rc

! !DESCRIPTION:
!      This routine closes the file(s) associated with {\tt aLog}.
!
!      The arguments are:
!      \begin{description}
! 
!      \item [aLog]
!            Log object.
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
	if (aLog%FileIsOpen .eq. ESMF_TRUE) then
		!call DATE_AND_TIME(d,t)
		!WRITE(aLog%unitnumber,100) d,t,"INFO     Log Close"
		!100 FORMAT(a8,2x,a10,2x,a)
		!if (aLog%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  INFO       Log Close"
		!CLOSE(UNIT=aLog%stdOutUnitNumber)
		aLog%FileIsOpen=ESMF_FALSE
		if (present(rc)) rc=ESMF_SUCCESS
	endif	
	
end subroutine ESMF_LogClose

!--------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LogFinalize - Finalizes the Log file(s)

! !INTERFACE: 
	subroutine ESMF_LogFinalize(rc)
!
! !ARGUMENTS:
    integer, intent(out),optional	:: rc

! !DESCRIPTION:
!      This routine finalizes the global log
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
! !IROUTINE: ESMF_LogFoundAllocError - Returns logical associated with finding an error

! !INTERFACE: 
	function ESMF_LogFoundAllocError(rcToCheck,line,file,method,rcToReturn)
!
! !RETURN VALUE:
	logical                                 ::ESMF_LogFoundAllocError
! !ARGUMENTS:
!	
	integer, intent(in)                     :: rcToCheck
	integer, intent(in), optional           :: line
	character(len=*), intent(in), optional  :: file
	character(len=*), intent(in), optional  :: method
	integer, intent(out),optional           :: rcToReturn

! !DESCRIPTION:
!      This function returns a logical true for return codes that indicate an error
!
!      The arguments are:
!      \begin{description}
! 	
!      \item [rcToCheck]
!            Return code to check.
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
	
    ESMF_LogFoundAllocError=.FALSE.
    if (present(rcToReturn)) rcToReturn=rcToCheck
    if (rcToCheck .NE. 0) then
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
! !IROUTINE: ESMF_LogFoundError - Returns logical associated with finding an error

! !INTERFACE: 
	function ESMF_LogFoundError(rcToCheck,line,file,method,rcToReturn)
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
!      This function returns a logical true for return codes that indicate an error
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
!            If specified, copy the {\tt status} value to {\tt rc}.
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
!BOP
! !IROUTINE: ESMF_LogMsgFoundAllocError - Returns logical associated with finding an error

! !INTERFACE: 
	function ESMF_LogMsgFoundAllocError(rcToCheck,msg,line,file,method,rcToReturn)
!
! !RETURN VALUE:
	logical                                 ::ESMF_LogMsgFoundAllocError
! !ARGUMENTS:
!	
	integer, intent(in)                     :: rcToCheck
	character(len=*), intent(in)            :: msg
	integer, intent(in), optional           :: line
	character(len=*), intent(in), optional  :: file
	character(len=*), intent(in), optional	:: method
    integer, intent(out),optional            :: rcToReturn	

! !DESCRIPTION:
!      This function returns a logical true for return codes that indicate an error
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
    if (present(rcToReturn)) rcToReturn=rcToCheck
    if (rcToCheck .NE. 0) then
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
! !IROUTINE: ESMF_LogGet - Returns information about a log object

! !INTERFACE: 
	subroutine ESMF_LogGet(aLog,verbose,flush,rootOnly,halt,logtype,stream,maxElements,rc)
!
! !ARGUMENTS:
!	
	type(ESMF_Log), intent(in) 		                :: aLog
	type(ESMF_Logical), intent(out),optional		:: verbose
	type(ESMF_Logical), intent(out),optional		:: flush
	type(ESMF_Logical), intent(out),optional		:: rootOnly
	integer, intent(out),optional		                :: halt
	type(ESMF_LogFileType), intent(in),optional	        :: logtype
	integer, intent(out),optional			        :: stream  
	integer, intent(out),optional			        :: maxElements
	integer, intent(out),optional			        :: rc
	

! !DESCRIPTION:
!      This subroutine returns various information about a log object.
!
!      The arguments are:
!      \begin{description}
!
!      \item [aLog]
!            Log object.
!      \item [verbose]
!            Verbose flag.
!      \item [rootOnly]
!	     Root only flag
!      \item [halt]
!	     Halt definitions (halterr(0), haltwarn(1),haltnever(2))
!      \item [logtype]
!             The type of file (singlelog(0), multilog(1)).
!      \item [stream]
!            The type of stream (free(0), preordered(1))
!      \item [maxElements]
!            Maximum number of elements in the log.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOP 
	if (present(rc)) then
	  rc=ESMF_FAILURE
	endif
	if (present(verbose)) then
	  verbose=aLog%verbose
	endif
	if (present(flush)) then
	  flush=aLog%flush
	endif
	if (present(rootOnly)) then
	  rootOnly=aLog%rootOnly
	endif
	if (present(halt)) then
	  halt=aLog%halt
	endif
	!if (present(logtype)) then
	!  logtype=aLog%logtype
	!endif
	if (present(stream)) then
	  stream=aLog%stream
	endif
	if (present(maxElements)) then
	  maxElements=aLog%maxElements
	endif	
	if (present(rc)) then
	  rc=ESMF_SUCCESS 
	endif 
end subroutine ESMF_LogGet

!--------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LogInitialize - Initializes Log file(s)

! !INTERFACE: 
      subroutine ESMF_LogInitialize(filename, rc)
!
! !ARGUMENTS:
      character(len=*)                          :: filename
      integer, intent(out),optional	            :: rc

! !DESCRIPTION:
!      This routine initializes the global log
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
    if (ESMF_LogDefault%stdOutUnitNumber .gt. ESMF_LOG_UPPER) return
    ESMF_LogDefault%nameLogErrFile=filename
    ESMF_LogDefault%unitnumber=ESMF_LogDefault%stdOutUnitNumber
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
! !IROUTINE: ESMF_LogMsgFoundError - Returns logical associated with finding an error

! !INTERFACE: 
	function ESMF_LogMsgFoundError(rcToCheck,msg,line,file,method,rcToReturn)
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
	integer, intent(out),optional                    :: rcToReturn
	

! !DESCRIPTION:
!      This function returns a logical true for return codes that indicate an error
!
!      The arguments are:
!      \begin{description}
! 	
!      \item [rc]
!            Return code to check.
!      \item [string]
!            User-provided context string.
!      \item [logtype]
!            The type of message (info(0), warning(1), error(2)).
!      \item [method]
!            User-provided method string.
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
    subroutine ESMF_LogOpen(aLog, filename, rc)
!
! !ARGUMENTS:
    type(ESMF_Log)			        :: aLog
    character(len=*)			    :: filename
    integer, intent(out),optional	:: rc

! !DESCRIPTION:
!      This routine opens the file(s) associated with {\tt aLog}.
!
!      The arguments are:
!      \begin{description}
! 
!      \item [aLog]
!            Log object.
!      \item [filename]
!            Name of file.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOP
	
	integer 				:: status, i
	character(len=10) 			:: t
	character(len=8) 			:: d
	
	if (present(rc)) rc=ESMF_FAILURE
	aLog%FileIsOpen=ESMF_FALSE
	if (aLog%stdOutUnitNumber .gt. ESMF_LOG_UPPER) return
	aLog%nameLogErrFile=filename
	aLog%unitnumber=aLog%stdOutUnitNumber
	do i=aLog%unitnumber, ESMF_LOG_UPPER
        inquire(unit=i,iostat=status)
        if (status .eq. 0) then
            aLog%FileIsOpen = ESMF_TRUE
            exit
        endif
   	enddo 
	if (aLog%FileIsOpen .eq. ESMF_FALSE) return
	aLog%unitNumber = i  
	if (present(rc)) rc=ESMF_SUCCESS	
    
end subroutine ESMF_LogOpen	

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogSet - Sets Log Parameters

! !INTERFACE: 
	subroutine ESMF_LogSet(aLog,verbose,flush,rootOnly,halt,logtype,stream,maxElements,rc)
!
! !ARGUMENTS:
!	
	type(ESMF_Log)		 				:: aLog
	type(ESMF_Logical), intent(in),optional			:: verbose
	type(ESMF_Logical), intent(in),optional			:: flush
	type(ESMF_Logical), intent(in),optional			:: rootOnly
	integer, intent(in),optional			        :: halt
	type(ESMF_LogFileType), intent(in),optional             :: logtype
	integer, intent(in),optional			        :: stream  
	integer, intent(in),optional			        :: maxElements
	integer, intent(out),optional			        :: rc
	
! !DESCRIPTION:
!      This subroutine sets the various options for the log object.
!
!      The arguments are:
!      \begin{description}
!
!      \item [aLog]
!            Log object.
!      \item [verbose]
!            Verbose flag.
!      \item [rootOnly]
!	     Root only flag
!      \item [halt]
!	     Halt definitions (halterr(0), haltwarn(1),haltnever(2))
!      \item [filetype]
!            The type of file (singlelog(0), multilog(1)).
!      \item [stream]
!            The type of stream (free(0), preordered(1))
!      \item [maxElements]
!            Maximum number of elements in the log.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOP 
	if (present(rc)) rc=ESMF_FAILURE
	if (present(verbose)) aLog%verbose=verbose
	if (present(flush)) aLog%flush=flush
	if (present(rootOnly)) aLog%rootOnly=rootOnly
	if (present(halt)) aLog%halt=halt
	if (present(logtype)) aLog%logtype=logtype
	if (present(stream)) aLog%stream=stream
	if (present(maxElements)) aLog%maxElements=maxElements
	if (present(rc)) rc=ESMF_SUCCESS 
end subroutine ESMF_LogSet

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogWrite - Write to Log file(s)

! !INTERFACE: 
	function ESMF_LogWrite(msg,logtype,line,file,method)
!
!
! !RETURN VALUE:
	logical                                 ::ESMF_LogWrite
! !ARGUMENTS:
	character(len=*), intent(in)            :: msg
	type(ESMF_LogFileType), intent(in)      :: logtype
	integer, intent(in), optional           :: line
	character(len=*), intent(in), optional  :: file
	character(len=*), intent(in), optional	:: method

! !DESCRIPTION:
!      This routine writes to the file(s) associated with {\tt aLog}.
!
!      The arguments are:
!      \begin{description}
! 
!      \item [msg]
!            User-provided context string.
!      \item [logtype]
!            The type of message (info(0), warning(1), error(2))..
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
    	select case (logtype%ftype)
          case (1)
    	    lt="INFO"
          case (2)
    	    lt="WARNING"
   	  case default
    	    lt="ERROR"
   	end select				
    	ok=0
    	do i=1, 100000
    	    OPEN(UNIT=ESMF_LogDefault%unitnumber, &
                File=ESMF_LogDefault%nameLogErrFile, POSITION="APPEND", &
    		ACTION="WRITE", STATUS="UNKNOWN", IOSTAT=status)
            if (status.eq.0) then
    		if (present(line)) then								
    		    if (present(method)) then
    			WRITE(ESMF_LogDefault%unitnumber,122) d," ",h,&
                                m,s,".",ms," ",trim(lt)," ",trim(tfile)," ",&
    				tline," ",trim(tmethod)," ",trim(msg)
			!if (aLog%verbose .eq. ESMF_TRUE) print *, &
                        ! d,"  ",t,"  ", lt,"    ",tfile,tline,"  ",tmethod,msg
    		    else
    			WRITE(ESMF_LogDefault%unitnumber,123) d," ",h,&
                                m,s,".",ms," ",trim(lt)," ",trim(tfile)," ",&
    				tline," ",trim(msg)
			!if (aLog%verbose .eq. ESMF_TRUE) print *, &
                        ! d,"  ",t,"  ", lt,"    ",tfile,tline,"  ",msg
    		    endif	
            else
    		if (present(method)) then
    		    WRITE(ESMF_LogDefault%unitnumber,132) d," ",h,&
                        m,s,".",ms," ",trim(lt)," ",&
    			" ",trim(tmethod)," ",trim(msg)
		    !if (aLog%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  ",&
    		    !lt,"    ","  ",tmethod,msg
    		else
    		    WRITE(ESMF_LogDefault%unitnumber,133) d," ",h,&
                        m,s,".",ms," ",trim(lt)," "," ",trim(msg)
		    !if (aLog%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  ",&
    		    !lt,"    ","  ",msg
    		endif	
    	    endif
    	    CLOSE(UNIT=ESMF_LogDefault%stdOutUnitNumber)
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
