! $Id: ESMF_Test.F90,v 1.6 2004/10/11 20:13:37 nscollins Exp $
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
!     ESMF Test Module
      module ESMF_TestMod
!
!==============================================================================
!
! This file contains the Test class definition and all Test class
! methods.
!
!------------------------------------------------------------------------------

!===============================================================================
!BOP
!
! !MODULE: ESMF_TestMod
!
! !DESCRIPTION:
!   contains methods to support testing
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_LogErrMod
      implicit none

! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_Test
      public ESMF_TestStart
      public ESMF_TestEnd
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Test.F90,v 1.6 2004/10/11 20:13:37 nscollins Exp $'

!==============================================================================

      contains

!==============================================================================

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_Test - Print PASS/FAIL messages for tests
!
! !INTERFACE:
      subroutine ESMF_Test(condition, name, failMsg, result, file, line, unit)

! !ARGUMENTS:
      logical, intent(in) :: condition      ! pass/fail condition
      character(*), intent(in) :: name      ! test name
      character(*), intent(in) :: failMsg   ! fail message
      integer, intent(inout) :: result      ! accumulated result
      character(*), intent(in) :: file      ! test file name
      integer, intent(in) :: line           ! test file line number
      integer, intent(in), optional :: unit ! additional output unit number

! !DESCRIPTION:
!     Prints a {\tt PASS} message to stdout if {\tt condition} is true,
!     and a {\tt FAIL} message if {\tt condition} is false.  If {\tt unit}
!     is specified, will in addition write the same message to that 
!     Fortran unit number.
!
!EOP
!-------------------------------------------------------------------------------

      character(ESMF_MAXSTR) :: msg

      if(condition) then
        write(msg, *) "PASS ", trim(name), ", ", trim(file), ", line", line
        print *, msg
        call ESMF_LogWrite(msg, ESMF_LOG_INFO)
        if (present(unit)) write(unit, *) msg
      else
        write(msg, *) "FAIL ", trim(name), ", ", trim(file), ", line", &
                      line, trim(failMsg)
        print *, msg
        call ESMF_LogWrite(msg, ESMF_LOG_INFO)
        if (present(unit)) write(unit, *) msg
        result = result + 1  ! count total failures; 0 = all pass
      end if

      end subroutine ESMF_Test

!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_TestStart - Print information at the start of testing
!
! !INTERFACE:
      subroutine ESMF_TestStart(numPETs, file, line, unit)

! !ARGUMENTS:
      integer, intent(in) :: numPETs        ! number of PETs in the test
      character(*), intent(in) :: file      ! test file name
      integer, intent(in) :: line           ! test file line number
      integer, intent(in), optional :: unit ! additional output unit number

! !DESCRIPTION:
!     Prints a standard message; intended to be called at the start of any
!     test code.  If {\tt unit}
!     is specified, will in addition write the same message to that 
!     Fortran unit number.
!
!EOP
!-------------------------------------------------------------------------------

      character(ESMF_MAXSTR) :: msg

      write(msg, *) "Beginning Test, file ", trim(file), ", line", line
      print *, msg
      call ESMF_LogWrite(msg, ESMF_LOG_INFO)
      if (present(unit)) write(unit, *) msg

      write(msg, *) "NUMBER OF PROCESSORS", numPETs
      print *, msg
      call ESMF_LogWrite(msg, ESMF_LOG_INFO)
      if (present(unit)) write(unit, *) msg

      end subroutine ESMF_TestStart

!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_TestEnd - Print information at the end of testing
!
! !INTERFACE:
      subroutine ESMF_TestEnd(result, file, line, unit)

! !ARGUMENTS:
      integer, intent(in) :: result         ! number of successful tests
      character(*), intent(in) :: file      ! test file name
      integer, intent(in) :: line           ! test file line number
      integer, intent(in), optional :: unit ! additional output unit number

! !DESCRIPTION:
!     Prints a standard message; intended to be called at the end of any
!     test code.  If {\tt unit}
!     is specified, will in addition write the same message to that 
!     Fortran unit number.
!
!EOP
!-------------------------------------------------------------------------------

      character(ESMF_MAXSTR) :: msg

      write(msg, *) "Number of failed tests:", result
      print *, msg
      call ESMF_LogWrite(msg, ESMF_LOG_INFO)
      if (present(unit)) write(unit, *) msg

      write(msg, *) "Ending Test, file ", trim(file), ", line", line
      print *, msg
      call ESMF_LogWrite(msg, ESMF_LOG_INFO)
      if (present(unit)) write(unit, *) msg

      end subroutine ESMF_TestEnd

!------------------------------------------------------------------------------

      end module ESMF_TestMod
