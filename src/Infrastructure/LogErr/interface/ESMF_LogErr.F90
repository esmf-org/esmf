! $Id: ESMF_LogErr.F90,v 1.13 2003/04/17 20:48:08 nscollins Exp $
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
! The Fortran interface to the Log class is written in both Fortran and C/C++.
! This file contains the interface code written in Fortran.  It also contains
! some utility functions used by the Log class.
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
   public ESMF_LogPrintString, ESMF_LogPrintChar, ESMF_LogPrintInt
   public ESMF_LogPrintReal, ESMF_LogPrintNewline

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
!   Most of the Fortran wrapper routines for the C/C++ ESMC\_Log class are
!   written in C. This is the only routine that isn't. See the class design
!   section for the rational for doing this. 
!
!   With the exception of the ESMF\_Log object, all the arguments are optional.
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
! This routine opens the log file and is called by ESMC\_LogWrite.
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
! !IROUTINE: ESMF_LogPrintChar  - Prints a character and an optional message
!
! !INTERFACE:
  subroutine ESMF_LogPrintChar(unitNumber,charData,flushSet,msg,length)

! !ARGUMENTS:
  integer, intent(in)::unitNumber,flushSet !See ESMF_Log data type

  character, intent(in)::charData          !character to be printed out

  character(len=32), intent(in):: msg      !optional message; only
					   !printed out if length is
					   !greater than zero.
  integer, intent(in) :: length

! !DESCRIPTION:
! This routine, and the routines that follow it, are used by ESMC\_LogPrint()
! ESMC\_LogPrintHeader() in the Log class.  Ordinarily, these Log routines would
! have just used fprintf.  However, because we needed to use the Fortran I/O 
! libraries when calling Log from a Fortran code
! (see the discussion about the class design), we had to make 
! calls to ESMF\_LogPrintChar() and the subroutines below, in addition to
! C's fprintf() (We still have to support C/C++, so we still need fprintf() ).
! ESMF\_LogPrintChar() and the routines below are not particularly general,
! but do the trick.
!EOP

  integer :: istat, i

  if (length .ne. 0) write(unitNumber,10,ADVANCE="no") (msg(i:i),i=1,length) 
  write(unitNumber,10,ADVANCE="no") charData
  if (flushSet .eq. ESMF_LOG_TRUE) call ESMF_IOFlush(unitNumber, istat)
  10 format(A1)

 end subroutine

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
! Prints a newline character.  See ESMF\_LogPrintChar for more 
! discussion.
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
 subroutine ESMF_LogPrintString(unitNumber,stringData,len1,flushSet,msg,len2)

! !ARGUMENTS:
  integer, intent(in)::unitNumber,flushSet,len1,len2
  character(len=32), intent(in)::msg,stringData

! !DESCRIPTION:
! Prints a string; see ESMF\_LogPrintChar() for a fuller discussion
!EOP

  integer :: i,istat

  if (len2 .ne. 0) write(unitNumber,10,ADVANCE="no") (msg(i:i),i=1,len2)
  write(unitNumber,10,ADVANCE="no") (stringData(i:i), i=1,len1)
  if (flushSet .eq. ESMF_LOG_TRUE) call ESMF_IOFlush(unitNumber, istat)
  10 format(A1)
 end subroutine ESMF_LogPrintString

  
!----------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogPrintInt - prints an int
!
! !INTERFACE:
 subroutine ESMF_LogPrintInt(unitnumber, intData, flushSet, msg, length)
!
! !ARGUMENTS:
  integer, intent(in) :: unitNumber
  integer, intent(in) :: intData
  integer, intent(in) :: flushSet
  character(len=32), intent(in) :: msg
  integer, intent(in) :: length

! !DESCRIPTION
! Prints an integer; see ESMF\_LogPrintChar() for more details
!
!EOP
  integer :: istat, i

  if (length .ne. 0) write(unitNumber,20,ADVANCE="no") (msg(i:i),i=1,length)
  write(unitNumber,10,ADVANCE="no") intData
  if (flushSet .eq. ESMF_LOG_TRUE) call ESMF_IOFlush(unitNumber, istat)
  10 format(I3)
  20 format(A1)

 end subroutine ESMF_LogPrintInt

!----------------------------------------------------------------------------
! !IROUTINE: ESMF_LogPrintReal - prints a real number
!
! !INTERFACE:
 subroutine ESMF_LogPrintReal(unitNumber,floatData,flushSet,msg,length)

! !ARGUMENTS:
  integer, intent(in) :: unitNumber
  real, intent(in) :: floatData
  integer, intent(in) :: flushSet
  character(len=32), intent(in) :: msg
  integer, intent(in) :: length

! !DESCRIPTION:
! Prints a real number; see ESMF\_LogPrintChar() for a longer discussion
!
!EOP
  integer :: istat, i

  if (length .ne. 0) write(unitNumber,20,ADVANCE="no") (msg(i:i),i=1,length)
  write(unitNumber,10,ADVANCE="no") floatdata 
  if (flushSet .eq. ESMF_LOG_TRUE) call ESMF_IOFlush(unitNumber, istat)

  10 format(F14.7)
  20 format(A1)

 end subroutine ESMF_LogPrintReal

end module ESMF_LogErrMod
