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
#include "ESMF.h"
#include "ESMF_Macros.inc"
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

!EOPI
   implicit none
   private
   
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
           
    type(ESMF_Logical) :: FileIsOpen            
    integer unitNumber                                                   
    type(ESMF_Logical) :: verbose
    type(ESMF_Logical) :: flush
    type(ESMF_Logical) :: root_only
    integer halt
    integer filetype
    integer stream 
    integer max_elements
    type(ESMF_LOGENTRY), dimension(1)::LOG_ENTRY
    character(len=32) nameLogErrFile      
    integer :: MaxTryOpen=100000
!If standardout not unit 6, must be changed here.
#ifndef ESMF_NO_INITIALIZERS
    integer :: stdOutUnitNumber=6
    type(ESMF_Logical) :: fileIO=ESMF_FALSE !File written to standard out
#else
    integer :: stdOutUnitNumber
    type(ESMF_Logical) :: fileIO
#endif
end type ESMF_Log


   public ESMF_Log,ESMF_LogOpen, ESMF_LogClose, ESMF_LogWrite, ESMF_LogInit, ESMF_LogFoundError, ESMF_LogSet, ESMF_LogGet
	
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
		if (present(rc)) then
			rc=ESMF_SUCCESS
		endif
	endif	
	
end subroutine ESMF_LogClose

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogOpen - Open Log file(s)

! !INTERFACE: 
      subroutine ESMF_LogOpen(aLog, filename, rc)
!
! !ARGUMENTS:
      type(ESMF_Log)			:: aLog
      character(len=*)			:: filename
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
	!call DATE_AND_TIME(d,t)

	!OPEN(UNIT=aLog%unitnumber,File=aLog%nameLogErrFile,POSITION="APPEND", &
	!ACTION="WRITE",STATUS="UNKNOWN",IOSTAT=STATUS)
	!if (status .eq. 0) then
    		!aLog%FileIsOpen=ESMF_TRUE
		!WRITE(aLog%stdOutUnitNumber,100) d,t,"INFO     Log Open"
		!100 FORMAT(a8,2x,a10,2x,a)
		!if (aLog%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  INFO       Log Open"	
		!rc=0
   	!else
    		!aLog%FileIsOpen=ESMF_FALSE
   	!end if  
	
end subroutine ESMF_LogOpen	

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogWrite - Write to Log file(s)

! !INTERFACE: 
	subroutine ESMF_LogWrite(aLog, string,logtype,module, method, rc)
!
! !ARGUMENTS:
	type(ESMF_Log), intent(in) 		:: aLog
	character(len=*), intent(in)		:: string
	integer, intent(in)			:: logtype
	character(len=*), intent(in), optional  :: module
	character(len=*), intent(in), optional	:: method
	integer, intent(out),optional		:: rc

! !DESCRIPTION:
!      This routine writes to the file(s) associated with {\tt aLog}.
!
!      The arguments are:
!      \begin{description}
! 
!      \item [aLog]
!            Log object.
!      \item [string]
!            User-provided context string.
!      \item [logtype]
!            The type of message (info(0), warning(1), error(2)).
!      \item [module]
!            User-provided module string.
!      \item [method]
!            User-provided method string.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOP
	
   	character(len=10) 					:: t
	character(len=8) 					:: d
	character(len=7)					:: lt
	character(len=32)					:: f
	character(len=32)					::tmodule
	character(len=32)					::tmethod
	integer							::status
	integer							::ok
	integer							::i
	
	if (present(rc)) rc=ESMF_FAILURE
	if (present(method)) tmethod=adjustl(method)
	if (present(module)) tmodule=adjustl(module)	  
	if (aLog%FileIsOpen .eq. ESMF_TRUE) then
		call DATE_AND_TIME(d,t)	
		select case (logtype)
			case (0)
				lt="INFO"
			case (1)
				lt="WARNING"
			case default
				lt="ERROR"
		end select				
		f=adjustl(__FILE__)
		ok=0
		do i=1, 100000
			OPEN(UNIT=aLog%unitnumber,File=aLog%nameLogErrFile,POSITION="APPEND", &
			ACTION="WRITE",STATUS="UNKNOWN",IOSTAT=status)
			if (status.eq.0) then
				if ((present(module)).and.(present(method))) then
		  			WRITE(aLog%unitnumber,100) d,t,lt,f,__LINE__,tmodule,tmethod,string
                        		100 FORMAT(a8,2x,a10,2x,a7,2x,a32,2x,i5,2x,a,a,a)
					if (aLog%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  ",&
					lt,"    ",f,__LINE__,"  ",tmodule,tmethod,string		
				else if ((present(module)).and. .not.(present(method))) then
		  			WRITE(aLog%unitnumber,101) d,t,lt,f,__LINE__,tmodule,string
					101 FORMAT(a8,2x,a10,2x,a7,2x,a32,2x,i5,2x,a,a)
					if (aLog%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  ",&
					lt,"    ",f,__LINE__,"  ",tmodule,string
				else if (.not.(present(module)).and.(present(method))) then
		  			WRITE(aLog%unitnumber,102) d,t,lt,f,__LINE__,tmodule,string
					102 FORMAT(a8,2x,a10,2x,a7,2x,a32,2x,i5,2x,a,a)
					if (aLog%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  ",&
					lt,"    ",f,__LINE__,"  ",tmodule,string	
				else
					WRITE(aLog%unitnumber,103) d,t,lt,f,__LINE__,string
					103 FORMAT(a8,2x,a10,2x,a7,2x,a32,2x,i5,2x,a)
					if (aLog%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  ",&
					lt,"    ",f,__LINE__,"  ",string
				endif	
				CLOSE(UNIT=aLog%stdOutUnitNumber)
				if (present(rc)) rc=ESMF_SUCCESS
				ok=1
			endif	
			if (ok.eq.1) exit
		enddo
	endif
	       
end subroutine ESMF_LogWrite

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogFoundError - Returns logical associated with finding an error

! !INTERFACE: 
	function ESMF_LogFoundError(aLog,rc,string,logtype,module,method)
!
! !RETURN VALUE:
	logical								::ESMF_LogFoundError
! !ARGUMENTS:
!	
	type(ESMF_Log), intent(in) 					:: aLog
	integer, intent(in)						:: rc
	character(len=*), intent(in),optional				:: string
	integer, intent(in),optional					:: logtype
	character(len=*), intent(in), optional  			:: module
	character(len=*), intent(in), optional				:: method
	

! !DESCRIPTION:
!      This function returns a logical true for return codes that indicate an error
!
!      The arguments are:
!      \begin{description}
! 	
!      \item [aLog]
!            Log object.
!      \item [rc]
!            Return code to check.
!      \item [string]
!            User-provided context string.
!      \item [logtype]
!            The type of message (info(0), warning(1), error(2)).
!      \item [module]
!            User-provided module string.
!      \item [method]
!            User-provided method string.
!      
!      \end{description}
! 
!EOP
	integer								::trc
	
	ESMF_LogFoundError=ESMF_FALSE
	if (rc .NE. ESMF_SUCCESS) then
		call ESMF_LogWrite(aLog,string,logtype,module,method,trc)
		ESMF_LogFoundError=ESMF_TRUE
	endif	
       
end function ESMF_LogFoundError

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogSet - Sets Log Parameters

! !INTERFACE: 
	subroutine ESMF_LogSet(aLog,verbose,flush,root_only,halt,filetype,stream,max_elements,rc)
!
! !ARGUMENTS:
!	
	type(ESMF_Log)		 					:: aLog
	type(ESMF_Logical), intent(in),optional					:: verbose
	type(ESMF_Logical), intent(in),optional					:: flush
	type(ESMF_Logical), intent(in),optional					:: root_only
	integer, intent(in),optional					:: halt
	integer, intent(in),optional					:: filetype
	integer, intent(in),optional					:: stream  
	integer, intent(in),optional					:: max_elements
	integer, intent(out),optional					:: rc
	

!      \item [aLog]
!            Log object.
!      \item [verbose]
!            Verbose flag.
!      \item [root_only]
!	     Root only flag
!      \item [halt]
!	     Halt definitions (halterr(0), haltwarn(1),haltnever(2))
!      \item [filetype]
!            The type of file (singlelog(0), multilog(1)).
!      \item [stream]
!            The type of stream (free(0), preordered(1))
!      \item [max_elements]
!            Maximum number of elements in the log.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOP 
	if (present(rc)) rc=ESMF_FAILURE
	if (present(verbose)) aLog%verbose=verbose
	if (present(flush)) aLog%flush=flush
	if (present(root_only)) aLog%root_only=root_only
	if (present(halt)) aLog%halt=halt
	if (present(filetype)) aLog%filetype=filetype
	if (present(stream)) aLog%stream=stream
	if (present(max_elements)) aLog%max_elements=max_elements
	if (present(rc)) rc=ESMF_SUCCESS 
end subroutine ESMF_LogSet

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogGet - Returns logical associated with finding an error

! !INTERFACE: 
	subroutine ESMF_LogGet(aLog,verbose,flush,root_only,halt,filetype,stream,max_elements,rc)
!
! !ARGUMENTS:
!	
	type(ESMF_Log), intent(in) 					:: aLog
	type(ESMF_Logical), intent(out),optional					:: verbose
	type(ESMF_Logical), intent(out),optional					:: flush
	type(ESMF_Logical), intent(out),optional					:: root_only
	integer, intent(out),optional					:: halt
	integer, intent(out),optional					:: filetype
	integer, intent(out),optional					:: stream  
	integer, intent(out),optional					:: max_elements
	integer, intent(out),optional					:: rc
	

!      \item [aLog]
!            Log object.
!      \item [verbose]
!            Verbose flag.
!      \item [root_only]
!	     Root only flag
!      \item [halt]
!	     Halt definitions (halterr(0), haltwarn(1),haltnever(2))
!      \item [filetype]
!             The type of file (singlelog(0), multilog(1)).
!      \item [stream]
!            The type of stream (free(0), preordered(1))
!      \item [max_elements]
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
	if (present(root_only)) then
	  root_only=aLog%root_only
	endif
	if (present(halt)) then
	  halt=aLog%halt
	endif
	if (present(filetype)) then
	  filetype=aLog%filetype
	endif
	if (present(stream)) then
	  stream=aLog%stream
	endif
	if (present(max_elements)) then
	  max_elements=aLog%max_elements
	endif	
	if (present(rc)) then
	  rc=ESMF_SUCCESS 
	endif 
end subroutine ESMF_LogGet

subroutine ESMF_LogInit(aLog)
	type(ESMF_Log) 						:: aLog

	aLog%verbose=ESMF_TRUE
	aLog%flush=ESMF_TRUE
	aLog%root_only=ESMF_TRUE	
	
end subroutine ESMF_LogInit

end module ESMF_LogErrMod

