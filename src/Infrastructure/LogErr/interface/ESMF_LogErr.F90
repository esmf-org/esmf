! $Id: ESMF_LogErr.F90,v 1.34 2004/03/24 06:56:06 cpboulder Exp $
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
   use ESMF_IOSpecMod

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
    type(ESMF_Logical) :: oneLogErrFile           !An ESMF_Log
                                                  !object
                                                  !will write to
					          !one file if
			                          !oneLogErrFile
					          !is set to
					          !ESMF_TRUE

    type(ESMF_Logical) :: standardOut             !If standardOut
                                                  !set 
                                                  !to ESMF_TRUE
					          !output
                                                  !goes to standard
					          !out.

    type(ESMF_Logical) :: fortIsOpen              !If fortIsOpen
                                                  !is set to
                                                  !ESMF_TRUE an 
			                          !ESMF_Log object
					          !has a Fortran file
					          !open.

    integer unitNumber                            !Fortran unit number
                                                  !for output

    integer numFilePtr                            !Index into global
                                                  !array of File
                                                  !pointers for C/C++ I/O

    integer numFileFort                           !Index into global
                                                  !array of
                                                  !Fortran unit numbers
	
	type(ESMF_Logical) :: verbose
	type(ESMF_Logical) :: flush
	type(ESMF_Logical) :: root_only
	integer halt
	integer filetype
	integer stream 
	character(len=32) LogFileName
	type(ESMF_LOGENTRY), dimension(1)::LOG_ENTRY


!An ESMF_Log object will write output only if verbose is set to ESMF_TRUE
!An ESMF_Log object will flush its output if flushSet is set to ESMF_TRUE
!An ESMF_Log object will halt on a warning if haltOnWarn is set to ESMF_TRUE
!An ESMF_Log object will halt on an error if haltOnErr is set to ESMF_TRUE.

#ifndef ESMF_NO_INITIALIZERS
    type(ESMF_Logical) :: verboseSet=ESMF_TRUE
    type(ESMF_Logical) :: flushSet=ESMF_FALSE
    type(ESMF_Logical) :: haltOnWarn=ESMF_FALSE
    type(ESMF_Logical) :: haltOnErr=ESMF_TRUE
#else
    type(ESMF_Logical) :: verboseSet
    type(ESMF_Logical) :: flushSet
    type(ESMF_Logical) :: haltOnWarn
    type(ESMF_Logical) :: haltOnErr
#endif

    character(len=32) nameLogErrFile      !Name of an ESMF_Log objects's
                                          !output file 

!If standardout not unit 6, must be changed here.
#ifndef ESMF_NO_INITIALIZERS
    integer :: stdOutUnitNumber=6
    type(ESMF_Logical) :: fileIO=ESMF_FALSE !File written to standard out
#else
    integer :: stdOutUnitNumber=6
    type(ESMF_Logical) :: fileIO
#endif
end type ESMF_Log



   public ESMF_Log,ESMF_LogGet, ESMF_LogGetUnit
   public ESMF_LogSet, ESMF_LogOpenFortran, ESMF_LogCloseFortran
   public ESMF_LogPrintString, ESMF_LogPrintNewline
   public ESMF_LogCloseFile,ESMF_LogOpenFile
   public ESMF_LogErr_,ESMF_LogErrMsg_, ESMF_LogWarn_, ESMF_LogWarnMsg_
   public ESMF_LogOpen, ESMF_LogClose, ESMF_LogWrite



   type(ESMF_Log), public, save :: ESMF_LOG_WORLD

!----------------------------------------------------------------------------

contains

subroutine ESMF_LogOpen(aLog, filetype, filename,rc)

	type(ESMF_Log)					:: aLog
	integer 					:: filetype 
	character(len=*)				:: filename
	integer, intent(out),optional			:: rc
	
	character(len=10) :: t
	character(len=8) :: d
	
	aLog%nameLogErrFile=filename
	call DATE_AND_TIME(d,t)
	OPEN(UNIT=aLog%stdOutUnitNumber,File=aLog%nameLogErrFile,POSITION="APPEND")
	WRITE(aLog%stdOutUnitNumber,*) t," ",d," Log Close"
	rc=0
	
end subroutine ESMF_LogOpen	

subroutine ESMF_LogClose(aLog,rc)

	type(ESMF_Log), intent(in) 			:: aLog
	integer, intent(out),optional			:: rc
	
	character(len=10) :: t
	character(len=8) :: d
	
	call DATE_AND_TIME(d,t)
	WRITE(aLog%stdOutUnitNumber,*) t," ",d," Log Close"
	CLOSE(UNIT=aLog%stdOutUnitNumber)
	rc=0
	
end subroutine ESMF_LogClose

subroutine ESMF_LogWrite(aLog,logtype,context,rc)
	type(ESMF_Log), intent(in) 			:: aLog
	integer, intent(in)				:: logtype
	character(len=*), intent(in)			:: context
	integer, intent(out),optional			:: rc
	
   	character(len=10) :: t
	character(len=8) :: d
	
	call DATE_AND_TIME(d,t)
	
	WRITE(aLog%stdOutUnitNumber,*) t," ",d," ",ESMF_SRCLINE," ",context
	print *, ESMF_SRCLINE," ",t," ",d," ",context
	rc=0
       

end subroutine ESMF_LogWrite



!------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogCloseFile - Close a Log file 
!
! !INTERFACE: 
     subroutine ESMF_LogCloseFile(aLog)
!

! !ARGUMENTS:
  type(ESMF_Log), intent(in) :: aLog
!
! !DESCRIPTION:
! Closes the file(s) associated with the Log {\tt aLog}.
!
! The arguments are
! \begin{description}
! \item[aLog]
!  an ESMG\_Log object
!
!  \end{description}
!
!EOP
!-----------------------------------------------------------------------------
   print *, "call C_ESMF_LogCloseFile"
   call C_ESMF_LogCloseFile(aLog)
   
end subroutine ESMF_LogCloseFile

!--------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ESMF_LogOpenFile - Open a Log file
! !INTERFACE: 
subroutine ESMF_LogOpenFile(aLog, numFile, name)
!
! !ARGUMENTS:
!
  type(ESMF_Log) :: aLog

  integer :: numFile                              

  character(len=*) :: name

! !DESCRIPTION:
! This routine opens the file(s) associated with {\tt alLog}.
!
! The arguments are:
! \begin{description}
! 
! \item[aLog]
! Log object.
!
! \item[numFile]
! Set to either ESMF\_SINGLE\_FILE or ESMF\_MULTIPLE\_FILE
! 
!  \item[name]
!  name of file
! 
!  \end{description}
! 
!EOP
  
  call C_ESMF_LogOpenFile(aLog, numFile, name)
  print *, "C_ESMF_LogOpenFile"
  
end subroutine





!----------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogSet - Initialize or set options for a Log.
!
! !INTERFACE:
subroutine ESMF_LogSet(aLog, verbose, flush, haltOnErr, haltOnWarn)
!
! !ARGUMENTS:
!
 type(ESMF_Log), intent(in) :: aLog
 type(ESMF_Logical), intent(in), optional :: verbose
 type(ESMF_Logical), intent(in), optional :: flush
 type(ESMF_Logical), intent(in), optional :: haltOnErr
 type(ESMF_Logical), intent(in), optional :: haltOnWarn 
!
! !DESCRIPTION:
!
!   This routines sets the options for a Log.  With the exception of 
!   the {\tt ESMF\_Log} object, all the arguments
!   are optional.  See the Log Examples section for a 
!   discussion of how to use this routine.
!
!   The arguments are:
!   \begin{description}
!
!   \item[verbose]
!   If set to ESMF\_TRUE, output written to Log file.                   
!  \item[flush]                                                       
!   If set to ESMF\_TRUE, output is flushed.                                  
!  \item[haltOnWarn]                                              
!   If set to ESMF\_TRUE, code stops on warnings.                             
!  \item[haltOnErr]                                                   
!   If set to ESMF\_TRUE, code stops on errors.                               
!
!  \end{description}
!
!EOP


 if (present(verbose)) then
    if (verbose .eq. ESMF_TRUE) then
      call c_esmf_logsetverbose(aLog)
    else
      call c_esmf_logsetnotverbose(aLog)
    end if
  end if

 if (present(flush)) then
    if (flush .eq. ESMF_TRUE) then
      call c_esmf_logsetflush(aLog)
    else
      call c_esmf_logsetnotflush(aLog)
    end if
 end if

 if (present(haltOnErr)) then
    if (haltOnErr .eq. ESMF_TRUE) then
      call c_esmf_logsethaltonerr(aLog)
    else
      call c_esmf_logsetnothaltonerr(aLog)
    end if
 end if

 if (present(haltOnWarn)) then
    if (haltOnWarn .eq. ESMF_TRUE) then
      call c_esmf_logsethaltonwarn(aLog)
    else
      call c_esmf_logsetnothaltonwarn(aLog)
    end if
 end if

end subroutine ESMF_LogSet


!----------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogGet - Get attributes of a Log 
!
! !INTERFACE:
subroutine ESMF_LogGet(aLog, verbose, flush, haltOnErr, haltOnWarn)
!
! !ARGUMENTS:
!
 type(ESMF_Log), intent(in) :: aLog
 type(ESMF_Logical), intent(out), optional :: verbose
 type(ESMF_Logical), intent(out), optional :: flush 
 type(ESMF_Logical), intent(out), optional :: haltOnWarn 
 type(ESMF_Logical), intent(out), optional :: haltOnErr
!
! !DESCRIPTION:
!
!   Gets the attributes of a Log.  With the exception of the 
!   {\tt ESMF\_Log} object, all the arguments
!   are optional. See the Log Examples section for a 
!   discussion of how to use the routine.
!  
!   The arguments are:
!   \begin{description}
!
!   \item[verbose]
!   If present, return value in argument. 
!  \item[flush]
!   If present, return value in argument. 
!  \item[haltOnWarn]
!   If present, return value in argument. 
!  \item[haltOnErr]
!   If present, return value in argument. 
!
!  \end{description}
!
!
!EOP

 if (present(verbose)) call c_esmf_loggetverbose(aLog,verbose)

 if (present(flush)) call c_esmf_loggetflush(aLog,flush)

 if (present(haltOnErr)) call c_esmf_loggethaltonerr(aLog,haltOnErr) 

 if (present(haltOnWarn)) call c_esmf_loggethaltonwarn(aLog,haltOnWarn) 

end subroutine ESMF_LogGet


!---------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ESMF_LogWarnMsg  - Write a warning to a Log with a user message
!
! !INTERFACE:
!
subroutine ESMF_LogWarnMsg_(aLog, errCode, line, file, dir, msg)

! !ARGUMENTS:
    type(ESMF_Log) :: aLog

    integer, intent(in) :: errCode               

    character(len=*), intent(in) :: msg 

    integer, intent(in) :: line        
                                     

    character(len=*), intent(in) :: file  
                                         

    character(len=*), intent(in) :: dir   
                                        

! !DESCRIPTION:
!    This routine writes a warning message to the Log file(s).  This warning
!    message consists of the {\tt errCode}, a description of the warning, the 
!    line number, file, and directory of the error, and a user-specified 
!    message. A preprocessor macro adds the predefined preprocessor symbolic
!    constants {\tt \_\_LINE\_\_}, {\tt \_\_FILE\_\_}, and {\tt \_\_DIR\_\_}
!    when {\tt ESMF\_LogWarnMsg} is called user code.  Note,
!    the value of {\tt \_\_DIR\_\_} must be suppliled by the user 
!    (usually done in the makefile).  
!
!    The arguments are
!    \begin{description}
!   
!    \item[errCode]
!    integer value for error code
!
!    \item[msg]
!    msg written to log file
!
!    \item[line]
!    line number of warning; argument supplied by macro
!
!    \item[file]
!    file where warning occurred in;argument supplied by macro
!
!    \item[dir]
!    directory where warning occurred in; argument supplied by macro
!  
!    \end{description}
!
!EOP
!--------------------------------------------------------------------------

   integer :: msgLength
   integer :: fileLength
   integer :: dirLength


   msgLength=len(msg)
   fileLength=len(file)
   dirLength=len(dir)

   call c_esmf_logwarnmsg(aLog,errcode, line, file,dir,msg,fileLength, &
                          dirlength, msglength)

 end subroutine ESMF_LogWarnMsg_

!-----------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogWarn - Write a standard warning to a Log 

! !INTERFACE:
  subroutine ESMF_LogWarn_(aLog, errCode,line,file,dir)

! !ARGUMENTS:
    type(ESMF_Log) :: aLog

    integer, intent(in) :: errCode         

    integer, intent(in) :: line            
                                         

    character(len=*), intent(in) :: file   
                                           

    character(len=*), intent(in) :: dir    
                                         

!
! !DESCRIPTION:
!   This routine is identical to {\tt ESMF\_LogWarnMsg}, except a message is
!   not written to the Log.
!
!   The arguments are:
!   \begin{description}
!
!   \item[errCode]
!   Error code
!
!   \item[line]
!   line number of warning; argument supplied by macro
!
!   \item[file]
!   file where warning occurred in; argument supplied by macro
!
!   \item[dir]
!   directory where warning occurred in; argument supplied by macro
!
!   \end{description}
!
!EOP
!-----------------------------------------------------------------------

   integer :: fileLength
   integer :: dirLength

   fileLength=len(file)
   dirLength=len(file)

   call C_ESMF_LogWarn(aLog,errCode,line,file,dir,fileLength,dirLength)

 end subroutine ESMF_LogWarn_

!-----------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogErrMsg - Write an error to a Log with a user message

! !INTERFACE:
 subroutine ESMF_LogErrMsg_(aLog, errCode,line,file,dir,msg)

! !ARGUMENTS:
    type(ESMF_Log) :: aLog

    integer, intent(in) :: errCode       

    character(len=*), intent(in) :: msg   

    integer, intent(in) :: line            
                                           

    character(len=*), intent(in) :: file   
                                          

    character(len=*), intent(in) :: dir   
                                          

! !DESCRIPTION:
!    This routine writes an error message to the Log file(s).  This 
!    message consists of the {\tt errCode}, a description of the error, the 
!    line number, file, and directory of the error, and a user-specified 
!    message. A preprocessor macro adds the predefined preprocessor symbolic
!    constants {\tt \_\_LINE\_\_}, {\tt \_\_FILE\_\_}, and {\tt \_\_DIR\_\_}
!    when {\tt ESMF\_LogWarnMsg} is called user code.  Note,
!    the value of {\tt \_\_DIR\_\_} must be suppliled by the user 
!    (usually done in the makefile).  
!
!   The arguments are:
!   \begin{description}
!
!   \item[errCode]
!   Error code   
!
!   \item[line]
!   line number of warning; argument supplied by macro
!
!   \item[file]
!   file where warning occurred in; argument supplied by macro
!
!   \item[dir]
!   directory where warning occurred in; argument supplied by macro
!
!   \end{description}
!EOP
!-----------------------------------------------------------------------

    integer :: fileLength
    integer :: dirLength
    integer :: msgLength

    fileLength=len(file)
    dirLength=len(dir)
    msgLength=len(msg)
    call C_ESMF_LogErrMsg(aLog,errCode,line,file,dir,msg,fileLength,dirLength)

 end subroutine ESMF_LogErrMsg_
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogErr - Write an error message to a Log

! !INTERFACE:
 subroutine  ESMF_LogErr_(aLog, errCode,line,file,dir)

! !ARGUMENTS:
    type(ESMF_Log) :: aLog

    integer, intent(in) :: errCode     

    integer, intent(in) :: line           
                                        

    character(len=*), intent(in) :: file 
                                          

    character(len=*), intent(in) :: dir    
                                        

! !DESCRIPTION:
!   This routine is identical to {\tt ESMF\_LogErrMsg}, except a msg is
!   not written to the log file.
!
!   The arguments are:
!   \begin{description}
!
!   \item[errCode]
!   Error code   
!
!   \item[line]
!   line number of warning; argument supplied by macro
!
!   \item[file]
!   file where warning occurred in; argument supplied by macro
!
!   \item[dir]
!   directory where warning occurred in; argument supplied by macro
!                                                                   
!   \end{description} 
!EOP
!-----------------------------------------------------------------------

    integer :: fileLength
    integer :: dirLength

    fileLength=len(file)
    dirLength=len(dir)
    call C_ESMF_LogErr(aLog,errCode,line,file,dir,fileLength,dirLength)

 end subroutine ESMF_LogErr_

!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
!BOP
 
! !IROUTINE: ESMF_LogGetUnit - Returns the Fortran unit number.
 
! !INTERFACE:
 function ESMF_LogGetUnit(aLog) result(unitNum)
! !ARGUMENTS:
    type(ESMF_Log) :: aLog
    integer :: unitNum
! !DESCRIPTION:
!   This function called from with a Fortran write statement, e.g.
!   write(ESMF\_LogGetUnit(aLog),*)"Hi".  The {\tt ESMF\_LogUnit} 
!   calls {\tt C\_ESMC\_GetUnit} and the C routine calls the C++ method
!   {\tt ESMC\_Log\_GetUnit}.  The C++ routine returns  
!   appends some header information (time,date etc.) to what ever is printed out
!   from the write, e.g. Hi.
!EOP
!------------------------------------------------------------------------------
   call C_ESMF_LogGetUnit(aLog)
   unitNum=aLog%unitNumber

end function ESMF_LogGetUnit

!------------------------------------------------------------------------------

!BOPI
! !IROUTINE: ESMF_LogOpenFortran
!
! !INTERFACE:
subroutine ESMF_LogOpenFortran(isOpen, unitNumber, nameLogFile)
!
! !ARGUMENTS:
  type(ESMF_Logical), intent(out) ::  isOpen     
		                           
			                   

  integer, intent(inout) ::  unitNumber 

  character (len=32), intent(in) :: nameLogFile
!    
!
! !DESCRIPTION:
! This routine opens the log file and is called by {\tt ESMC\_LogWrite}.
! See the ESMC\_LogErr.C file for more details.
! This routine is not a module procedure because F90 mangles
! the names of functions
! inside modules and this routine is called by ESMC\_LogWrite() - a C++
! method.
!
! The arguments are:
! \begin{description}
!
! \item[isOpen]
! If file successfully opened isOpen set to ESMF\_TRUE otherwise set
! to ESMF\_FALSE
!
! \item[unitNumber]
! standard Fortran unit number for I/O
!
! \item[nameLogFile]
! name of log file
!
! \end{description}
!
!EOPI

   integer :: status, i

   ! Assume failure until we know we have an open unit number
   isOpen=ESMF_FALSE

   if (unitNumber .gt. ESMF_LOG_UPPER) return

   do i=unitNumber, ESMF_LOG_UPPER
     inquire(unit=i,iostat=status)
     if (status .eq. 0) then
       isOpen = ESMF_TRUE
       exit
     endif
   enddo 

   if (isOpen .eq. ESMF_FALSE) return

   unitNumber = i
   open(unit=unitNumber,file=nameLogFile, status='unknown', &
        action='write',position='append',iostat=status)

   if (status .eq. 0) then
    isOpen=ESMF_TRUE
   else
    isOpen=ESMF_FALSE
   end if    

 end subroutine  ESMF_LogOpenFortran


!----------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LogCloseFortran
!
! !INTERFACE:
   subroutine ESMF_LogCloseFortran(unitNumber)
!
! !ARGUMENTS:
    integer, intent(in) :: unitNumber
!
! !DESCRIPTION:
! This routine closes any log files that have been written to using
! the Fortran interface.  It is called by by the C/C++ Log
! method ESMC\_LogFinalize().
!
! The arguments are;
! \begin{description}
!
! \item[unitNumber]
! standard Fortran unit number for I/O
!
! \end{description}
!EOPI
  
    close(unitNumber)

end subroutine ESMF_LogCloseFortran


!----------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LogPrintNewLine - Print a newline character
!
! !INTERFACE:
  subroutine ESMF_LogPrintNewLine(unitNumber,flushSet)
!
! !ARGUMENTS:
  type(ESMF_Logical), intent(in)::flushSet
  integer, intent(in)::unitNumber

! !DESCRIPTION:
! Prints a newline character. 
!
! The arguments are:
! \begin{description}
!
! \item[flushSet]
! If set to ESMF\_TRUE, out flushed.
!
! \item[unitNumber]
! standard Fortran unit number for I/O
!
! \end{description}
!EOPI

  integer :: istat

  write(unitNumber,*)
  if (flushSet .eq. ESMF_TRUE) call ESMF_IOFlush(unitNumber, istat)

 end subroutine ESMF_LogPrintNewLine


!----------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LogPrintString - Print a string.
!
! !INTERFACE:
 subroutine ESMF_LogPrintString(unitNumber,stringToPrint,flushSet)

! !ARGUMENTS:
  type(ESMF_Logical), intent(in)::flushSet
  integer, intent(in) :: unitNumber
  character(len=*), intent(in)::stringToPrint

! !DESCRIPTION:
! This routine is used by ESMC\_LogPrint() and
! ESMC\_LogPrintHeader() in the Log class to print a string.
! Ordinarily, these Log routines would
! have just used fprintf.  However, because we need to allow one
! to use the Fortran I/O 
! libraries when Fortran I/O is selected
! (see the discussion about the class design), we need to call a
! Fortran routine to do the printing
! and this is the one!
!  
! ! The arguments are:
! \begin{description}
!
! \item[unitNumber]
! standard Fortran unit number for I/O
!
! \item[stringToPrint]
! String to be printed.
!
! \item[flushSet]
! If set to ESMF\_TRUE, out flushed.
!
! \end{description}
!EOPI

  integer :: i,istat

  write(unitNumber,10) stringToPrint
  if (flushSet .eq. ESMF_TRUE) call ESMF_IOFlush(unitNumber, istat)
  10 format(A)
 end subroutine ESMF_LogPrintString

  
end module ESMF_LogErrMod
