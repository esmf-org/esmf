#include "/home/sjs/ESMF/esmf/src/Infrastructure/LogErr/include/ESMF_LogConstants.inc"
module ESMF

implicit none

type:: ESMF_Log
    sequence
    integer oneLogErrFile
    integer standardOut
    integer fortIsOpen
    integer unitNumber
    integer numFilePtr
    integer logLevel
    integer flushSet
    integer haltOnwarn
    integer haltOnErr
    character(len=32) nameLogErrFile
end type ESMF_Log

contains
subroutine ESMF_LogInit(aLog)
!verbose, flush,standardOut,haltOnErr,haltOnWarn)
 implicit none
 type(ESMF_Log), intent(in) :: aLog
! integer, optional ::verbose, flush, standardOut,haltOnErr,haltOnWarn 
 integer ::verbose, flush, standardOut,haltOnErr,haltOnWarn 
 verbose=ESMF_LOG_TRUE
 flush=ESMF_LOG_FALSE
 haltOnErr=ESMF_LOG_TRUE
 haltOnWarn=ESMF_LOG_FALSE
! if (.not. present(verbose)) verbose=ESMF_LOG_TRUE
! if (.not. present(flush)) flush=ESMF_LOG_FALSE
! if (.not. present(standardOut)) standardOut=ESMF_LOG_TRUE
! if (.not. present(haltOnErr)) haltOnErr=ESMF_LOG_TRUE
! if (.not. present(haltOnWarn)) haltOnWarn=ESMF_LOG_FALSE
 call esmf_loginit_c(aLog,verbose,flush,haltOnErr,haltOnWarn)
end subroutine

end module ESMF



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
! the ESMC\_LogWrite() method.  It is called by ESMC\_LogFinalize().
! Note: This routine is not a module procedure
! because F90 mangles the names of functions
! inside modules and this routine is called by ESMC\_LogFinalize() - a C++
! method.
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
  integer, intent(inout) ::  unitNumber
  integer, intent(inout) ::  isOpen
  character (len=32), intent(in) :: nameLogFile
!    isOpen: an integer, equal to 0 if the open succeeds, or 1 if the
!    opens fails.
!
!    unitNumber: an integer corresponding to the unit number assigned
!    to the log file by ESMF\_LogOpenFortran
!
!    nameLogFile: character string storing the name of the log file
!    
!
! !DESCRIPTION:
! This routine opens the log file and is called by ESMC\_LogWrite.
! See ESMC\_Log.C for more details.
! This routine is not a module procedure because F90 mangles
! the names of functions
! inside modules and this routine is called by ESMC\_LogWrite() - a C++
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



 subroutine ESMF_LogPrintChar(unitNumber,charData,flushSet,msg,length)
  implicit none
  integer :: istat,length,i
  integer, intent(in)::unitNumber,flushSet
  character, intent(in)::charData
  character(len=32), intent(in):: msg
!  length=len_trim(msg)
  if (length /= 0) write(unitNumber,10) (msg(i:i),i=1,length) 
  write(unitNumber,10) charData
  if (flushSet == ESMF_LOG_TRUE) call flush(unitNumber, istat)
  10 format(\A)
 end subroutine

 subroutine ESMF_LogPrintNewLine(unitNumber,flushSet)
  implicit none
  integer :: istat
  integer, intent(in)::unitNumber,flushSet
  write(unitNumber,*)
  if (flushSet .eq. ESMF_LOG_TRUE) call flush(unitNumber, istat)
 end subroutine


 subroutine ESMF_LogPrintString(unitNumber,stringData,len1, &
            flushSet,msg,len2)
  implicit none
  integer :: i,istat
  integer, intent(in)::unitNumber,flushSet,len1,len2
  character(len=32), intent(in)::msg,stringData
!  length=len_trim(msg)
  if (len2 /= 0) write(unitNumber,10) (msg(i:i),i=1,len2)
!  length=len_trim(stringData)
  write(unitNumber,10) (stringData(i:i), i=1,len1)
  if (flushSet .eq. ESMF_LOG_TRUE) call flush(unitNumber, istat)
  10 format(\A)
 end subroutine

  
 subroutine ESMF_LogPrintInt(unitnumber,intData,flushSet,msg,length)
  implicit none
  integer :: istat,length,i
  integer, intent(in)::unitNumber,flushSet,intData
  character(len=32), intent(in):: msg
!  length=len_trim(msg)
  if (length /= 0) write(unitNumber,10) (msg(i:i),i=1,length)
  write(unitNumber,10) intData
  if (flushSet .eq. ESMF_LOG_TRUE) call flush(unitNumber, istat)
  10 format(\I3)
  20 format(I3)
 end subroutine

 subroutine ESMF_LogPrintReal(unitNumber,floatData,flushSet,msg,length)
  implicit none
  integer :: istat,length,i
  integer, intent(in)::unitNumber,flushSet
  real, intent(in):: floatData
  character(len=32), intent(in):: msg
!  length=len_trim(msg)
  if (length /= 0) write(unitNumber,10) (msg(i:i),i=1,length)
  write(unitNumber,10) floatdata 
  if (flushSet .eq. ESMF_LOG_TRUE) call flush(unitNumber, istat)
  10 format(\F14.7)
  20 format(F14.7)
 end subroutine

