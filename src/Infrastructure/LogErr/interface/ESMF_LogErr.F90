! $Id: ESMF_LogErr.F90,v 1.10 2003/04/15 16:25:34 nscollins Exp $
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

module ESMF_LogErr

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
!============================================================================
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
!EOP

contains

!==========================================================================
!BOP
! !PUBLIC MEMBER Functions
! !IROUTINE: ESMF_LogInit - initialize an error object.
!
! !INTERFACE:
subroutine ESMF_LogInit(aLog,verbose,flushflag,haltOnErr,haltOnWarn)
!
! !ARGUMENTS:
!
 type(ESMF_Log), intent(in) :: aLog
 integer, intent(in),optional::verbose, flushflag, haltOnErr,haltOnWarn 
!
! !DESCRIPTION:
!   Most of the Fortran wrapper routines for the C/C++ ESMC/_Log class are
!   written in C. This is the only routine that isn't. See the class design
!   section for the rational for doing this. 
!
!   With the exception of the ESMF\_Log object, all the arguments are optional.
!   See the Examples section of the document for a discussion of how to use the
!   routine.
!
!EOP
!==========================================================================

 integer :: verbosity,stopOnErr,stopOnWarn,flushOut

 verbosity=ESMF_LOG_TRUE
 flushOut=ESMF_LOG_FALSE
 stopOnErr=ESMF_LOG_TRUE
 stopOnWarn=ESMF_LOG_FALSE

 if (present(verbose)) verbosity=verbose
 if (present(flushflag)) flushOut=flushflag
 if (present(haltOnErr)) stopOnErr=haltOnErr
 if (present(haltOnWarn)) stopOnWarn=haltOnWarn
 call esmf_loginit_c(aLog,verbosity,flushOut,stopOnErr,stopOnWarn)
end subroutine



!======================================================================
!
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
! method ESMC/_LogFinalize().
! Note: This routine is not a module procedure
! because it needs to be called from Log's C++ method and
! F90 mangles the names of functions
! inside modules.
!
!EOP
!=========================================================================


  
    close(unitNumber)
end subroutine ESMF_LogCloseFortran

!======================================================================
!
!BOP
! !IROUTINE: ESMF_LogOpenFortran
!
! !INTERFACE:
subroutine ESMF_LogOpenFortran(isOpen,unitNumber, nameLogFile)
  implicit none
!
!
! !ARGUMENTS:
  integer, intent(inout) ::  unitNumber !standard Fortran unit number for I/O

  integer, intent(inout) ::  isOpen     !if file successfully opened
				        !isOpen set to ESMF_LOG_TRUE	
					!otherwise set to ESMF_LOG_FALSE

  character (len=32), intent(in) :: nameLogFile
!    
!
! !DESCRIPTION:
! This routine opens the log file and is called by ESMC/_LogWrite.
! See the ESMC/_LogErr.C file for more details.
! This routine is not a module procedure because F90 mangles
! the names of functions
! inside modules and this routine is called by ESMC/_LogWrite() - a C++
! method.
!
!EOP
!=========================================================================



  integer :: istatInquire,istatOpen
  if (unitNumber > ESMF_LOG_UPPER) then
    isOpen=ESMF_LOG_FALSE
    return
  end if
  do
   inquire(unit=unitNumber,iostat=istatInquire)
   if (unitNumber <= ESMF_LOG_UPPER) then
    if (istatInquire /= 0 ) then
      unitNumber=unitNumber+1 
    else
      exit
    end if
   else
    isOpen=ESMF_LOG_FALSE
    return
   end if
  end do

  open(unit=unitNumber,file=nameLogFile, status='unknown', &
  action='write',position='append',iostat=istatOpen)
  if (istatOpen == 0) then
    isOpen=ESMF_LOG_TRUE
    return
  else
    isOpen=ESMF_LOG_FALSE
    return
  end if    
 end subroutine  ESMF_LogOpenFortran



!========================================================================
!BOP
! !IROUTINE: ESMF_LogPrintChar  - Prints a character and an optional 
!                                 message
!
! !INTERFACE:
 subroutine ESMF_LogPrintChar(unitNumber,charData,flushSet,msg,length)
!EOP
  implicit none
  integer :: istat,length,i
!BOP
! !ARGUMENTS:
  integer, intent(in)::unitNumber,flushSet !See ESMF_Log data type

  character, intent(in)::charData          !character to be printed out

  character(len=32), intent(in):: msg      !optional message; only
					   !printed out if length is
					   !greater than zero.
! !DESCRIPTION:
! This routine, and the routines that follow it, are used by ESMC/_LogPrint()
! ESMC/_LogPrintHeader() in the Log class.  Ordinarily, these Log routines would
! have just used fprintf.  However, because we needed to use the Fortran I/O 
! libraries when calling Log from a Fortran code
! (see the discussion about the class design), we had to make 
! calls to ESMF\_LogPrintChar() and the subroutines below, in addition to
! C's fprintf() (We still have to support C/C++, so we still need fprintf() ).
! ESMF\_LogPrintChar() and the routines below are not particularly general,
! but do the trick.
!EOP
!========================================================================

  if (length /= 0) write(unitNumber,10) (msg(i:i),i=1,length) 
  write(unitNumber,10) charData
  if (flushSet == ESMF_LOG_TRUE) call ESMF_IOFlush(unitNumber, istat)
  10 format('+',A)
 end subroutine

!======================================================================
!BOP
! 
! !IROUTINE: ESMF_LogPrintNewLine - prints a newline character
!
! !INTERFACE:
 subroutine ESMF_LogPrintNewLine(unitNumber,flushSet)
!
!EOP
  implicit none
!BOP
! !ARGUMENTS:
  integer, intent(in)::unitNumber,flushSet  !see above
! !DESCRIPTION:
! Prints a newline character.  See ESMF\_LogPrintChar for more 
! discussion
!EOP
!=====================================================================
  integer :: istat
  write(unitNumber,*)
  if (flushSet .eq. ESMF_LOG_TRUE) call ESMF_IOFlush(unitNumber, istat)
 end subroutine


!========================================================================
!BOP
!
! !IROUTINE: ESMF_LogPrintString - prints a string
!
! !INTERFACE:
 subroutine ESMF_LogPrintString(unitNumber,stringData,len1,flushSet,msg,len2)
!EOP
  implicit none
!BOP
! !ARGUMENTS:
  integer, intent(in)::unitNumber,flushSet,len1,len2
  character(len=32), intent(in)::msg,stringData
! !DESCRIPTION:
! Prints a string; see ESMF\_LogPrintChar() for a fuller discussion
!EOP
!======================================================================
  integer :: i,istat
  if (len2 /= 0) write(unitNumber,10) (msg(i:i),i=1,len2)
  write(unitNumber,10) (stringData(i:i), i=1,len1)
  if (flushSet .eq. ESMF_LOG_TRUE) call ESMF_IOFlush(unitNumber, istat)
  10 format('+',A)
 end subroutine

  
!=====================================================================
!BOP
! !IROUTINE: ESMF_LogPrintInt - prints an int
!
! !INTERFACE:
 subroutine ESMF_LogPrintInt(unitnumber,intData,flushSet,msg,length)
!EOP
  implicit none
  integer :: istat,length,i
!BOP
! !ARGUMENTS:
  integer, intent(in)::unitNumber,flushSet,intData
  character(len=32), intent(in):: msg
! !DESCRIPTION
! Prints an integer; see ESMF\_LogPrintChar() for more details
!
!EOP
!======================================================================
  if (length /= 0) write(unitNumber,20) (msg(i:i),i=1,length)
  write(unitNumber,10) intData
  if (flushSet .eq. ESMF_LOG_TRUE) call ESMF_IOFlush(unitNumber, istat)
  10 format('+',I3)
  20 format('+',A1)
 end subroutine

!======================================================================
! !IROUTINE: ESMF_LogPrintReal - prints a real number
!
! !INTERFACE:
 subroutine ESMF_LogPrintReal(unitNumber,floatData,flushSet,msg,length)
!EOP
  implicit none
  integer :: istat,length,i
!BOP
! !ARGUMENTS:
  integer, intent(in)::unitNumber,flushSet
  real, intent(in):: floatData
  character(len=32), intent(in):: msg
! !DESCRIPTION:
! Prints a real number; see ESMF\_LogPrintChar() for a longer discussion
!
!EOP
!======================================================================
  if (length /= 0) write(unitNumber,20) (msg(i:i),i=1,length)
  write(unitNumber,10) floatdata 
  if (flushSet .eq. ESMF_LOG_TRUE) call ESMF_IOFlush(unitNumber, istat)
  10 format('+',F14.7)
  20 format('+',A)
 end subroutine ESMF_LogPrintReal

end module ESMF_LogErr
