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
   
!------------------------------------------------------------------------------
!     ! ESMF_LogFileType
!
!     ! Log File Types


!EOPI
   implicit none

type ESMF_LogFileType
      sequence
        integer :: ftype
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
           
    type(ESMF_Logical) :: FileIsOpen            
    integer unitNumber                                                   
    type(ESMF_Logical) :: verbose
    type(ESMF_Logical) :: flush
    type(ESMF_Logical) :: root_only
    integer halt
    type(ESMF_LogFileType) :: logtype
    integer stream 
    integer max_elements
    type(ESMF_LOGENTRY), dimension(1)::LOG_ENTRY
    character(len=32) nameLogErrFile      
!If standardout not unit 6, must be changed here.
#ifndef ESMF_NO_INITIALIZERS
    integer :: MaxTryOpen=100000
    integer :: stdOutUnitNumber=6
    type(ESMF_Logical) :: fileIO=ESMF_FALSE !File written to standard out
#else
    integer :: MaxTryOpen
    integer :: stdOutUnitNumber
    type(ESMF_Logical) :: fileIO
#endif
end type ESMF_Log


   public ESMF_Log,ESMF_LogOpen, ESMF_LogClose, ESMF_LogWrite,&
   ESMF_LogInitialize, ESMF_LogFinalize,ESMF_LogFoundError,& 
   ESMF_LogSet, ESMF_LogGet

   type(ESMF_Log),SAVE::gLog	
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
end subroutine ESMF_LogOpen	

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogWrite - Write to Log file(s)

! !INTERFACE: 
	subroutine ESMF_LogWrite(msg,logtype,module,method,rc,aLog)
!
! !ARGUMENTS:
	character(len=*), intent(in)			        :: msg
	type(ESMF_LogFileType), intent(in)		:: logtype
	character(len=*), intent(in), optional          :: module
	character(len=*), intent(in), optional	        :: method
	integer, intent(out),optional			        :: rc
	type(ESMF_Log), intent(in) , optional	        :: aLog

! !DESCRIPTION:
!      This routine writes to the file(s) associated with {\tt aLog}.
!
!      The arguments are:
!      \begin{description}
! 
!      \item [aLog]
!            Log object.
!      \item [msg]
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
	
   	character(len=10) 				:: t
	character(len=8) 				:: d
	character(len=7)				:: lt
	character(len=32)				:: f
	character(len=32)				::tmodule
	character(len=32)				::tmethod
	integer					        ::status
	integer						::ok
	integer						::i
	integer						::h,m,s,ms,y,mn,dy
	if (present(rc)) rc=ESMF_FAILURE
	if (present(method)) tmethod=adjustl(method)
	if (present(module)) tmodule=adjustl(module)
	call c_esmc_timestamp(y,mn,dy,h,m,s,ms)
	if (present(aLog)) then	  
		if (aLog%FileIsOpen .eq. ESMF_TRUE) then
			call DATE_AND_TIME(d,t)	
			select case (logtype%ftype)
				case (1)
					lt="INFO"
				case (2)
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
						WRITE(aLog%unitnumber,102) d," ",h,m,s,".",ms," ",trim(lt)," ",trim(f)," ",&
						__LINE__," ",trim(tmethod),trim(tmodule),trim(msg)
						100  FORMAT(a8,a,i2.2,i2.2,i2.2,a,i6.6,a,a,a,a,a,i0,a,a,a,a)
						!if (aLog%verbose .eq. ESMF_TRUE) print *,d,"
						!",y,mn,dy,h,m,s,ms," ",&
						!lt,"    ",f,__LINE__,"  ",tmodule,tmethod,msg		
					else if ((present(module)).and. .not.(present(method))) then
					WRITE(aLog%unitnumber,102) d," ",h,m,s,".",ms," ",trim(lt)," ",trim(f)," ",&
						__LINE__," ",trim(tmodule),trim(msg)
						101  FORMAT(a8,a,i2.2,i2.2,i2.2,a,i6.6,a,a,a,a,a,i0,a,a,a)
						!if (aLog%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  ",&
						!lt,"    ",f,__LINE__,"  ",tmodule,msg
					else if (.not.(present(module)).and.(present(method))) then
						WRITE(aLog%unitnumber,102) d," ",h,m,s,".",ms," ",trim(lt)," ",trim(f)," ",&
						__LINE__," ",trim(tmethod),trim(msg)
						102  FORMAT(a8,a,i2.2,i2.2,i2.2,a,i6.6,a,a,a,a,a,i0,a,a,a)
						!if (aLog%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  ",&
						!lt,"    ",f,__LINE__,"  ",tmodule,msg
					else
						WRITE(aLog%unitnumber,103) d," ",h,m,s,".",ms," ",trim(lt)," ",trim(f)," ",&
						__LINE__," ",trim(msg)
						103  FORMAT(a8,a,i2.2,i2.2,i2.2,a,i6.6,a,a,a,a,a,i0,a,a)
						!if (aLog%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  ",&
						!lt,"    ",f,__LINE__,"  ",msg
					endif	
					CLOSE(UNIT=aLog%stdOutUnitNumber)
					if (present(rc)) rc=ESMF_SUCCESS
					ok=1
				endif	
				if (ok.eq.1) exit
			enddo
		endif

	else
		if (gLog%FileIsOpen .eq. ESMF_TRUE) then
			call DATE_AND_TIME(d,t)	
			select case (logtype%ftype)
				case (1)
					lt="INFO"
				case (2)
					lt="WARNING"
				case default
					lt="ERROR"
			end select				
			f=adjustl(__FILE__)
			ok=0
			do i=1, 100000
				OPEN(UNIT=gLog%unitnumber,File=gLog%nameLogErrFile,POSITION="APPEND", &
				ACTION="WRITE",STATUS="UNKNOWN",IOSTAT=status)
				if (status.eq.0) then
					if ((present(module)).and.(present(method))) then
						WRITE(gLog%unitnumber,103) d," ",h,m,s,".",ms," ",trim(lt)," ",trim(f)," ",&
						__LINE__," ",trim(tmodule),trim(tmethod),trim(msg)
						104  FORMAT(a8,a,i2.2,i2.2,i2.2,a,i6.6,a,a,a,a,a,i0,a,a,a,a)
						!if (aLog%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  ",&
						!lt,"    ",f,__LINE__,"  ",tmodule,tmethod,msg		
					else if ((present(module)).and. .not.(present(method))) then
		  				WRITE(gLog%unitnumber,103) d," ",h,m,s,".",ms," ",trim(lt)," ",trim(f)," ",&
						__LINE__," ",trim(tmodule),trim(msg)
						105  FORMAT(a8,a,i2.2,i2.2,i2.2,a,i6.6,a,a,a,a,a,i0,a,a,a)
						!if (aLog%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  ",&
						!lt,"    ",f,__LINE__,"  ",tmodule,msg
					else if (.not.(present(module)).and.(present(method))) then
						WRITE(gLog%unitnumber,103) d," ",h,m,s,".",ms," ",trim(lt)," ",trim(f)," ",&
						__LINE__," ",trim(tmodule),trim(msg)
						106  FORMAT(a8,a,i2.2,i2.2,i2.2,a,i6.6,a,a,a,a,a,i0,a,a,a)
						!if (gLog%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  ",&
						!lt,"    ",f,__LINE__,"  ",tmodule,msg	
					else
						WRITE(gLog%unitnumber,103) d," ",h,m,s,".",ms," ",trim(lt)," ",trim(f)," ",&
						__LINE__," ",trim(msg)
						107  FORMAT(a8,a,i2.2,i2.2,i2.2,a,i6.6,a,a,a,a,a,i0,a,a)
						!if (gLog%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  ",&
						!lt,"    ",f,__LINE__,"  ",msg
					endif	
					CLOSE(UNIT=gLog%stdOutUnitNumber)
					if (present(rc)) rc=ESMF_SUCCESS
					ok=1
				endif	
				if (ok.eq.1) exit
			enddo
		endif
	endif	
	       
end subroutine ESMF_LogWrite

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogFoundError - Returns logical associated with finding an error

! !INTERFACE: 
	function ESMF_LogFoundError(rc,msg,logtype,module,method,aLog)
!
! !RETURN VALUE:
	logical								::ESMF_LogFoundError
! !ARGUMENTS:
!	
	integer, intent(in)						:: rc
	character(len=*), intent(in),optional				:: msg
	type(ESMF_LogFileType), intent(in) 	                        :: logtype
	character(len=*), intent(in), optional  			:: module
	character(len=*), intent(in), optional				:: method
	type(ESMF_Log), intent(in), optional				:: aLog
	

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
		if (present(aLog)) then
			call ESMF_LogWrite(msg,logtype,module,method,trc,aLog)
		else 
			call ESMF_LogWrite(msg,logtype,module,method,trc)
		endif
		ESMF_LogFoundError=ESMF_TRUE
	endif	
       
end function ESMF_LogFoundError

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogSet - Sets Log Parameters

! !INTERFACE: 
	subroutine ESMF_LogSet(aLog,verbose,flush,root_only,halt,logtype,stream,max_elements,rc)
!
! !ARGUMENTS:
!	
	type(ESMF_Log)		 					:: aLog
	type(ESMF_Logical), intent(in),optional					:: verbose
	type(ESMF_Logical), intent(in),optional					:: flush
	type(ESMF_Logical), intent(in),optional					:: root_only
	integer, intent(in),optional					        :: halt
	type(ESMF_LogFileType), intent(in),optional		        :: logtype
	integer, intent(in),optional					        :: stream  
	integer, intent(in),optional					        :: max_elements
	integer, intent(out),optional					        :: rc
	

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
	if (present(logtype)) aLog%logtype=logtype
	if (present(stream)) aLog%stream=stream
	if (present(max_elements)) aLog%max_elements=max_elements
	if (present(rc)) rc=ESMF_SUCCESS 
end subroutine ESMF_LogSet

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogGet - Returns logical associated with finding an error

! !INTERFACE: 
	subroutine ESMF_LogGet(aLog,verbose,flush,root_only,halt,logtype,stream,max_elements,rc)
!
! !ARGUMENTS:
!	
	type(ESMF_Log), intent(in) 					                :: aLog
	type(ESMF_Logical), intent(out),optional					:: verbose
	type(ESMF_Logical), intent(out),optional					:: flush
	type(ESMF_Logical), intent(out),optional					:: root_only
	integer, intent(out),optional					            :: halt
	type(ESMF_LogFileType), intent(in),optional		            :: logtype
	integer, intent(out),optional					            :: stream  
	integer, intent(out),optional					            :: max_elements
	integer, intent(out),optional					            :: rc
	

!      \item [aLog]
!            Log object.
!      \item [verbose]
!            Verbose flag.
!      \item [root_only]
!	     Root only flag
!      \item [halt]
!	     Halt definitions (halterr(0), haltwarn(1),haltnever(2))
!      \item [logtype]
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
	!if (present(logtype)) then
	!  logtype=aLog%logtype
	!endif
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

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogInitialize - Initializes Log file(s)

! !INTERFACE: 
      subroutine ESMF_LogInitialize(filename, rc)
!
! !ARGUMENTS:
      character(len=*)			:: filename
      integer, intent(out),optional	:: rc

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
!EOP
	
	integer 				:: status, i, rc2	
	if (present(rc)) rc=ESMF_FAILURE
	gLog%FileIsOpen=ESMF_FALSE
	if (gLog%stdOutUnitNumber .gt. ESMF_LOG_UPPER) return
	gLog%nameLogErrFile=filename
	gLog%unitnumber=gLog%stdOutUnitNumber
	do i=gLog%unitnumber, ESMF_LOG_UPPER
     		inquire(unit=i,iostat=status)
     		if (status .eq. 0) then
       			gLog%FileIsOpen = ESMF_TRUE
       		exit
     		endif
   	enddo 
	if (gLog%FileIsOpen .eq. ESMF_FALSE) return
	gLog%unitNumber = i  
	!call c_ESMC_LogInitialize(filename,rc2)
	if (present(rc)) rc=ESMF_SUCCESS
	
end subroutine ESMF_LogInitialize

!--------------------------------------------------------------------------
!BOP
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
!EOP
	if (present(rc)) then
	  	rc=ESMF_FAILURE
	endif
	if (gLog%FileIsOpen .eq. ESMF_TRUE) then
!	FLUSH	
		gLog%FileIsOpen=ESMF_FALSE
		if (present(rc)) then
			rc=ESMF_SUCCESS
		endif
	endif	
	
end subroutine ESMF_LogFinalize

end module ESMF_LogErrMod

