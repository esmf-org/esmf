! $Id: ESMF_LogErr.F90,v 1.37 2004/03/24 17:42:24 cpboulder Exp $
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


   public ESMF_Log,ESMF_LogOpen, ESMF_LogClose, ESMF_LogWrite, ESMF_LogInit

!----------------------------------------------------------------------------

contains

!--------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ESMF_LogOpen - Open a Log file
! !INTERFACE: 
subroutine ESMF_LogOpen(aLog, filename,rc)
!
! !ARGUMENTS:
!
type(ESMF_Log)						:: aLog
character(len=*)					:: filename
integer, intent(out),optional		:: rc

! !DESCRIPTION:
! This routine opens the file(s) associated with {\tt alLog}.
!
! The arguments are:
! \begin{description}
! 
! \item[aLog]
! Log object.
!
! \item[filetype]
! Set to either ESMF\_SINGLE\_FILE or ESMF\_MULTIPLE\_FILE
! 
!  \item[filename]
!  name of file
! 
!  \end{description}
! 
!EOP
	
	integer 						:: status, i
	character(len=10) 				:: t
	character(len=8) 				:: d
	
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

subroutine ESMF_LogClose(aLog,rc)

	type(ESMF_Log)						:: aLog
	integer, intent(out),optional		:: rc
	
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

subroutine ESMF_LogWrite(aLog,logtype,context,rc)
	type(ESMF_Log), intent(in) 			:: aLog
	integer, intent(in)					:: logtype
	character(len=*), intent(in)		:: context
	integer, intent(out),optional		:: rc
	
   	character(len=10) 					:: t
	character(len=8) 					:: d
	character(len=7)					:: lt
	character(len=32)					:: f
	
	rc=1
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
		WRITE(aLog%unitnumber,100) d,t,lt,f,__LINE__,context
		100 FORMAT(a8,2x,a10,2x,a7,2x,a32,2x,i5,2x,a)
		if (aLog%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  ",&
		lt,"    ",f,__LINE__,"  ",context
		rc=0
	endif	
       
end subroutine ESMF_LogWrite

subroutine ESMF_LogInit(aLog)
	type(ESMF_Log) 						:: aLog

	aLog%verbose=ESMF_TRUE
	aLog%flush=ESMF_TRUE
	aLog%root_only=ESMF_TRUE	
	
end subroutine ESMF_LogInit

end module ESMF_LogErrMod
