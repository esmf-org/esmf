! $Id: ESMF_LogErr.F90,v 1.5 2004/04/08 06:30:59 cpboulder Exp $
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

#include "ESMF.h"
#include "ESMF_Macros.inc"
#include "ESMF_LogConstants.inc"
module ESMF_LogErrMod

   use ESMF_BaseMod

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
	type(ESMF_LOGENTRY), dimension(1)::LOG_ENTRY
    character(len=32) nameLogErrFile      

!If standardout not unit 6, must be changed here.
#ifndef ESMF_NO_INITIALIZERS
    integer :: stdOutUnitNumber=6
    type(ESMF_Logical) :: fileIO=ESMF_FALSE !File written to standard out
#else
    integer :: stdOutUnitNumber
    type(ESMF_Logical) :: fileIO
#endif
end type ESMF_Log


   public ESMF_Log,ESMF_LogOpen, ESMF_LogClose, ESMF_LogWrite, ESMF_LogInit, ESMF_LogFoundError

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
	
	rc=1
	if (aLog%FileIsOpen .eq. ESMF_TRUE) then
		call DATE_AND_TIME(d,t)
		WRITE(aLog%unitnumber,100) d,t,"INFO     Log Close"
		100 FORMAT(a8,2x,a10,2x,a)
		if (aLog%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  INFO       Log Close"
		CLOSE(UNIT=aLog%stdOutUnitNumber)
		rc=0
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
	
	rc=1
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
	
	call DATE_AND_TIME(d,t)

	OPEN(UNIT=aLog%unitnumber,File=aLog%nameLogErrFile,POSITION="APPEND", &
	ACTION="WRITE",STATUS="UNKNOWN",IOSTAT=STATUS)
	if (status .eq. 0) then
    	aLog%FileIsOpen=ESMF_TRUE
		WRITE(aLog%stdOutUnitNumber,100) d,t,"INFO     Log Open"
		100 FORMAT(a8,2x,a10,2x,a)
		if (aLog%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  INFO       Log Open"	
		rc=0
   	else
    	aLog%FileIsOpen=ESMF_FALSE
   	end if  
	
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
	
	rc=ESMF_FAILURE
	if (present(method)) then
		tmethod=adjustl(method)
	else
		tmethod=""
	endif
	if (present(module)) then
		tmodule=adjustl(module)
	else
		tmodule=""
	endif	  
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
		WRITE(aLog%unitnumber,100) d,t,lt,f,__LINE__,tmodule,tmethod,string
		100 FORMAT(a8,2x,a10,2x,a7,2x,a32,2x,i5,2x,a32,a32,a)
		if (aLog%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  ",&
		lt,"    ",f,__LINE__,"  ",tmodule,tmethod,string
		rc=ESMF_SUCCESS
	endif	
       
end subroutine ESMF_LogWrite

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogFoundError - Returns logical associated with finding an error

! !INTERFACE: 
	function ESMF_LogFoundError(aLog,rc,string,logtype,module,method)
!
! !RETURN VALUE:
	logical									::ESMF_LogFoundError
! !ARGUMENTS:
!	
	type(ESMF_Log), intent(in) 				:: aLog
	integer, intent(in)						:: rc
	character(len=*), intent(in),optional	:: string
	integer, intent(in),optional			:: logtype
	character(len=*), intent(in), optional  :: module
	character(len=*), intent(in), optional	:: method
	

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
	character(len=32)					::tstring
	character(len=32)					::tmodule
	character(len=32)					::tmethod
	integer								::trc
	integer								::tlogtype
	
	ESMF_LogFoundError=ESMF_FALSE
	if (rc .NE. ESMF_SUCCESS) then
		if (present(method)) then
			tmethod=adjustl(method)
		else
			tmethod=""
		endif
		if (present(module)) then
			tmodule=adjustl(module)
		else
			tmodule=""
		endif	  
		if (present(module)) then
			tmodule=adjustl(module)
		else
			tmodule=""
		endif	  
		call ESMF_LogWrite(aLog,tstring,logtype,tmodule,tmethod,trc)
		ESMF_LogFoundError=ESMF_TRUE
	endif	
       
end function ESMF_LogFoundError

subroutine ESMF_LogInit(aLog)
	type(ESMF_Log) 						:: aLog

	aLog%verbose=ESMF_TRUE
	aLog%flush=ESMF_TRUE
	aLog%root_only=ESMF_TRUE	
	
end subroutine ESMF_LogInit

end module ESMF_LogErrMod

