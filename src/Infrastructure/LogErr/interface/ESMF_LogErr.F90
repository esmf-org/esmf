! $Id: ESMF_LogErr.F90,v 1.15 2003/07/02 18:58:25 rstaufer Exp $
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

#include "ESMF_Macros.inc"
#include "ESMF_LogConstants.inc"

module ESMF_LogErrMod

   use ESMF_BaseMod
   use ESMF_IOMod

!BOP
!============================================================================
! !MODULE: Fortran Interface to Log class. 
!
! !DESCRIPTION:
!
! The Fortran interface to the {\tt ESMF\_Log} class is written in both Fortran and C/C++.
! This file contains the interface code written in Fortran.  It also contains
! some utility functions used by the {\tt ESMF\_Log} class.
!
!----------------------------------------------------------------------------
!
   implicit none
   private

type ESMF_Log
    private
    sequence
    integer oneLogErrFile     !An ESMF_Log object will write to one file if
			      !oneLogErrFile is set to ESMF_LOG_TRUE.

    integer standardOut       !If standardOut set to ESMF_LOG_TRUE, output
                              !goes to standard out.

    integer fortIsOpen        !If fortIsOpen is set to ESMF_LOG_TRUE, an 
			      !ESMF_Log object has a fortran file open.

    integer unitNumber        !Fortran unit number for output

    integer numFilePtr        !Index into global array of File pointers
                              !for C/C++ I/O

    integer numFileFort       !Index into global array of Fortran unit numbers


    integer verboseSet        !An ESMF_Log object will write output only if
			      !verbose is set to ESMF_LOG_TRUE.
    
    integer flushSet          !An ESMF_Log object will have its output flushed
			      !if flushSet is set to ESMF_LOG_TRUE.

    integer haltOnWarn        !An ESMF_Log object will halt on encountering
			      !a warning if haltOnWarn is set to ESMF_LOG_TRUE. 

    integer haltOnErr         !An ESMF_Log object will halt on encountering
			      !an error if haltOnErr is set to ESMF_LOG_TRUE. 

    character(len=32) nameLogErrFile !Name of an ESMF_Log objects's output file 

end type ESMF_Log

! !PUBLIC MEMBER Functions

   public ESMF_Log
   public ESMF_LogInit, ESMF_LogOpenFortran, ESMF_LogCloseFortran
   public ESMF_LogPrintString, ESMF_LogPrintNewline
   public ESMF_LogErr_,ESMF_LogErrMsg_, ESMF_LogWarn_, ESMF_LogWarnMsg_

!EOP
!----------------------------------------------------------------------------

contains

!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogInit - initialize an error object.
!
! !INTERFACE:
subroutine ESMF_LogInit(aLog, verbose, flushflag, haltOnErr, haltOnWarn)
!
! !ARGUMENTS:
!
 type(ESMF_Log), intent(in) :: aLog
 integer, intent(in), optional :: verbose
 integer, intent(in), optional :: flushflag
 integer, intent(in), optional :: haltOnErr
 integer, intent(in), optional :: haltOnWarn 
!
! !DESCRIPTION:
!   Most of the Fortran wrapper routines for the C/C++ {\tt ESMC\_Log} class are
!   written in C. This is the only routine that isn't. See the class design
!   section for the rational for doing this. 
!
!   With the exception of the {\tt ESMF\_Log} object, all the arguments are optional.
!   See the Examples section of the document for a discussion of how to use the
!   routine.
!
!EOP

 integer :: verbosity, stopOnErr, stopOnWarn, flushOut

 verbosity=ESMF_LOG_TRUE
 flushOut=ESMF_LOG_FALSE
 stopOnErr=ESMF_LOG_TRUE
 stopOnWarn=ESMF_LOG_FALSE

 if (present(verbose)) verbosity=verbose
 if (present(flushflag)) flushOut=flushflag
 if (present(haltOnErr)) stopOnErr=haltOnErr
 if (present(haltOnWarn)) stopOnWarn=haltOnWarn

 call esmf_loginit_c(aLog,verbosity,flushOut,stopOnErr,stopOnWarn)

end subroutine ESMF_LogInit



!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: ESMF_LogWarnMsg  - writes a warning message to the log file
!
! !INTERFACE:
!
subroutine ESMF_LogWarnMsg_(aLog, errCode, line,file,dir,msg)

! !ARGUMENTS:
    type(ESMF_Log) :: aLog

    integer, intent(in) :: errCode        !integer value for error code         

    character(len=*), intent(in) :: msg    !msg written to log file

    integer, intent(in) :: line           !line number of warning; argument
                                          !supplied by macro

    character(len=*), intent(in) :: file  !file where warning occurred in;
                                          !argument supplied by macro

    character(len=*), intent(in) :: dir   !directory where warning occurred in;
                                          !argument supplied by macro

! !DESCRIPTION:
!    This routine calls esmf_logerrmsg_ln in ESMC_LogErrInterface.C
!    to write a warning message to the log file.  This warning
!    message consists of the erroCode, a description of the warning, the 
!    line number, file, and directory of the error, and a message. A 
!    preprocessor macro adds the predefined preprocessor symbolic
!    constants \_\_LINE\_\_, \_\_FILE\_\_, and \_\_DIR\_\_ when
!    {\tt ESMF\_LogWarnMsg} is called user code.  Note,
!    the value of \_\_DIR\_\_ 
!    must be suppliled by the user (usually done in
!    the makefile.).  By default, execution continues after encountering
!    a warning, but by calling the routine ESMF\_LogWarnHalt(), the user
!    can halt on warnings.
!
!EOP
!--------------------------------------------------------------------------

   integer :: msgLength
   integer :: fileLength
   integer :: dirLength


   msgLength=len(msg)
   fileLength=len(file)
   dirLength=len(dir)

   call esmf_logwarnmsg_ln(aLog,errcode, line, file,dir,msg,fileLength, &
                          dirlength, msglength)

 end subroutine ESMF_LogWarnMsg_

!-----------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogWarn - writes a warning message to log file

! !INTERFACE:
  subroutine ESMF_LogWarn_(aLog, errCode,line,file,dir)

! !ARGUMENTS:
    type(ESMF_Log) :: aLog

    integer, intent(in) :: errCode         !Error code

    integer, intent(in) :: line            !line number of warning; argument
                                           !supplied by macro

    character(len=*), intent(in) :: file   !file where warning occurred in;
                                           !argument supplied by macro

    character(len=*), intent(in) :: dir    !directory where warning occurred in;
                                           !argument supplied by macro

!
! !DESCRIPTION:
!   This routine is identical to {\tt ESMF\_LogWarnMsg}, except a msg is
!   not written to the log file.
!
!EOP
!-----------------------------------------------------------------------

   integer :: fileLength
   integer :: dirLength

   fileLength=len(file)
   dirLength=len(file)

   call ESMF_LogWarn_Ln(aLog,errCode,line,file,dir,fileLength,dirLength)

 end subroutine ESMF_LogWarn_

!-----------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogErr - writes a error message to log file

! !INTERFACE:
 subroutine ESMF_LogErrMsg_(aLog, errCode,line,file,dir,msg)

! !ARGUMENTS:
    type(ESMF_Log) :: aLog

    integer, intent(in) :: errCode         !integer value for error code

    character(len=*), intent(in) :: msg    !msg written to log file

    integer, intent(in) :: line            !line number of warning; argument
                                           !supplied by macro

    character(len=*), intent(in) :: file   !file where warning occurred in;
                                           !argument supplied by macro

    character(len=*), intent(in) :: dir    !directory where warning occurred in;
                                           !argument supplied by macro

! !DESCRIPTION:
!   This routine is identical to {\tt ESMF\_LogErrMsg}, except a msg is
!   not written to the log file.
!
!EOP
!-----------------------------------------------------------------------

    integer :: fileLength
    integer :: dirLength
    integer :: msgLength

    fileLength=len(file)
    dirLength=len(dir)
    msgLength=len(msg)
    call ESMF_LogErrMsg_Ln(aLog,errCode,line,file,dir,msg,fileLength,dirLength)

 end subroutine ESMF_LogErrMsg_
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogErr - writes a error message to log file

! !INTERFACE:
 subroutine  ESMF_LogErr_(aLog, errCode,line,file,dir)

! !ARGUMENTS:
    type(ESMF_Log) :: aLog

    integer, intent(in) :: errCode         !integer value for error code

    integer, intent(in) :: line            !line number of warning; argument
                                           !supplied by macro

    character(len=*), intent(in) :: file   !file where warning occurred in;
                                           !argument supplied by macro

    character(len=*), intent(in) :: dir    !directory where warning occurred in;
                                           !argument supplied by macro

! !DESCRIPTION:
!   This routine is identical to {\tt ESMF\_LogErrMsg}, except a msg is
!   not written to the log file.
!
!EOP
!-----------------------------------------------------------------------

    integer :: fileLength
    integer :: dirLength

    fileLength=len(file)
    dirLength=len(dir)
    call ESMF_LogErr_Ln(aLog,errCode,line,file,dir,fileLength,dirLength)

 end subroutine ESMF_LogErr_

!----------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogOpenFortran
!
! !INTERFACE:
subroutine ESMF_LogOpenFortran(isOpen, unitNumber, nameLogFile)
!
! !ARGUMENTS:
  integer, intent(out) ::  isOpen     !if file successfully opened
		                      !isOpen set to ESMF_LOG_TRUE	
				      !otherwise set to ESMF_LOG_FALSE

  integer, intent(inout) ::  unitNumber !standard Fortran unit number for I/O

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
!EOP

   integer :: status, i

   ! Assume failure until we know we have an open unit number
   isOpen=ESMF_LOG_FALSE

   if (unitNumber .gt. ESMF_LOG_UPPER) return

   do i=unitNumber, ESMF_LOG_UPPER
     inquire(unit=i,iostat=status)
     if (status .eq. 0) then
       isOpen = ESMF_LOG_TRUE
       exit
     endif
   enddo 

   if (isOpen .eq. ESMF_LOG_FALSE) return

   unitNumber = i
   open(unit=unitNumber,file=nameLogFile, status='unknown', &
        action='write',position='append',iostat=status)

   if (status .eq. 0) then
    isOpen=ESMF_LOG_TRUE
   else
    isOpen=ESMF_LOG_FALSE
   end if    

 end subroutine  ESMF_LogOpenFortran


!----------------------------------------------------------------------------
!BOP
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
!EOP
  
    close(unitNumber)

end subroutine ESMF_LogCloseFortran


!----------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogPrintNewLine - prints a newline character
!
! !INTERFACE:
  subroutine ESMF_LogPrintNewLine(unitNumber,flushSet)
!
! !ARGUMENTS:
  integer, intent(in)::unitNumber,flushSet

! !DESCRIPTION:
! Prints a newline character. 
!EOP

  integer :: istat

  write(unitNumber,*)
  if (flushSet .eq. ESMF_LOG_TRUE) call ESMF_IOFlush(unitNumber, istat)

 end subroutine ESMF_LogPrintNewLine


!----------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogPrintString - prints a string
!
! !INTERFACE:
 subroutine ESMF_LogPrintString(unitNumber,stringToPrint,flushSet)

! !ARGUMENTS:
  integer, intent(in)::unitNumber,flushSet
  character(len=*), intent(in)::stringToPrint

! !DESCRIPTION:
! This routine, is used by ESMC\_LogPrint() and
! ESMC\_LogPrintHeader() in the Log class to print a string.
! Ordinarily, these Log routines would
! have just used fprintf.  However, because we need to allow one to use the Fortran I/O 
! libraries when Fortran I/O is selected
! (see the discussion about the class design), we need to call a Fortran routine to do the printing
! and this is the one!
!  
!EOP

  integer :: i,istat

  write(unitNumber,10) stringToPrint
  if (flushSet .eq. ESMF_LOG_TRUE) call ESMF_IOFlush(unitNumber, istat)
  10 format(A)
 end subroutine ESMF_LogPrintString

  
end module ESMF_LogErrMod
