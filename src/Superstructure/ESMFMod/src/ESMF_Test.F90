! $Id: ESMF_Test.F90,v 1.6 2004/12/28 07:19:24 theurich Exp $
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
      use ESMF_BaseTypesMod
      use ESMF_BaseMod
      use ESMF_LogErrMod
      use ESMF_VMMod
      use ESMF_InitMod
      implicit none

! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_Test
      public ESMF_TestEnd
      public ESMF_TestMinPETs
      public ESMF_TestStart
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Test.F90,v 1.6 2004/12/28 07:19:24 theurich Exp $'

!==============================================================================

      contains

!==============================================================================


!-------------------------------------------------------------------------------
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

      character(2*ESMF_MAXSTR) :: msg

      if(condition) then
        write(msg, *) "PASS ", trim(name), ", ", trim(file), ", line", line
        print *, msg
        call ESMF_LogWrite("PASS "//trim(name), ESMF_LOG_INFO, line, file)
        if (present(unit)) write(unit, *) msg
      else
        write(msg, *) "FAIL ", trim(name), ", ", trim(file), ", line", &
                      line, trim(failMsg)
        print *, msg
        call ESMF_LogWrite("FAIL "//trim(name), ESMF_LOG_INFO, line, file)
        if (present(unit)) write(unit, *) msg
        result = result + 1  ! count total failures; 0 = all pass
      end if

      end subroutine ESMF_Test


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

      integer :: rc
      character(ESMF_MAXSTR) :: msg

      write(msg, *) "Number of failed tests:", result
      print *, msg
      call ESMF_LogWrite(msg, ESMF_LOG_INFO)
      if (present(unit)) write(unit, *) msg

      write(msg, *) "Ending Test, file ", trim(file), ", line", line
      print *, msg
      call ESMF_LogWrite("Ending Test", ESMF_LOG_INFO, line, file)
      if (present(unit)) write(unit, *) msg

      call ESMF_Finalize(rc)
      if (rc .ne. ESMF_SUCCESS) then
          write(msg, *) "Failure in Finalizing ESMF"
          print *, msg
      endif

      end subroutine ESMF_TestEnd

!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_TestMinPETs - Verify there are a sufficient number of PETs
!
! !INTERFACE:
      function ESMF_TestMinPETs(petCount, file, line, unit)

! !RETURN VALUE:
      logical :: ESMF_TestMinPETs

! !ARGUMENTS:
      integer, intent(in) :: petCount       ! minimum number of acceptable PETs
      character(*), intent(in) :: file      ! test file name
      integer, intent(in) :: line           ! test file line number
      integer, intent(in), optional :: unit ! additional output unit number

! !DESCRIPTION:
!     Verifies we are running on at least the minimum number of PETs.
!     If {\tt unit} is specified, will in addition write the same message 
!     to that Fortran unit number.
!
!EOP
!-------------------------------------------------------------------------------

      character(ESMF_MAXSTR) :: msg, failMsg
      type(ESMF_VM) :: globalVM
      integer :: numPETs, localrc

      ! assume failure until sure of success
      ESMF_TestMinPETs = .false.

      ! Get the global VM and pet count.
      call ESMF_VMGetGlobal(globalVM, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
        failMsg = "Unable to get global VM" 
        write(msg, *) "FAIL ", trim(file), ", line", line, trim(failMsg)
        print *, msg
        call ESMF_LogWrite("FAIL ", ESMF_LOG_INFO, line, file)
        if (present(unit)) write(unit, *) msg
        return
      end if

      call ESMF_VMGet(globalVM, petCount=numPETs, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
        failMsg = "Unable to query global VM" 
        write(msg, *) "FAIL ", trim(file), ", line", &
                      line, trim(failMsg)
        print *, msg
        call ESMF_LogWrite("FAIL ", ESMF_LOG_INFO, line, file)
        if (present(unit)) write(unit, *) msg
        return
      endif

      if (petCount .gt. numPETs) then
        write(failMsg, *) "This test must run on at least", petCount, "processors."
        write(msg, *) "FAIL ", trim(file), ", line", &
                      line, trim(failMsg)
        print *, msg
        call ESMF_LogWrite("FAIL ", ESMF_LOG_INFO, line, file)
        if (present(unit)) write(unit, *) msg
        return
      endif

      ESMF_TestMinPETs = .true.
      return

      end function ESMF_TestMinPETs

!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_TestStart - Print information at the start of testing
!
! !INTERFACE:
      subroutine ESMF_TestStart(file, line, unit, rc)

! !ARGUMENTS:
      character(*), intent(in) :: file      ! test file name
      integer, intent(in) :: line           ! test file line number
      integer, intent(in), optional :: unit ! additional output unit number
      integer, intent(out), optional :: rc  ! return code

! !DESCRIPTION:
!     Initializes the ESMF framework, and prints a standard start message
!     which is parsed by the nightly build scripts.  Must be called once
!     at the start of test code.
!     If {\tt unit} is specified, will in addition write the same message 
!     to that Fortran unit number.
!
!EOP
!-------------------------------------------------------------------------------

      character(ESMF_MAXSTR) :: msg
      type(ESMF_VM) :: globalVM
      integer :: numPETs, localrc

      ! initialize the framework.  if this fails, print a message directly
      ! because there is no guarentee that the log code will be working.
      call ESMF_Initialize(vm=globalVM, defaultlogfilename="UTestLog", &
                           defaultlogtype=ESMF_LOG_SINGLE, rc=localrc)
                           !defaultlogtype=ESMF_LOG_MULTI, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
          print *, "FAIL: Unable to initialize the ESMF Framework.  Error code ", localrc
          if (present(rc)) rc = localrc
          return
      endif

      call ESMF_VMGet(globalVM, petCount=numPETs, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
          print *, "FAIL: Unable to get information about number of PETs.  Error code ", localrc
          if (present(rc)) rc = localrc
          return
       endif

      write(msg, *) "Beginning Test, file ", trim(file), ", line", line
      print *, msg
      call ESMF_LogWrite("Beginning Test", ESMF_LOG_INFO, line, file)
      if (present(unit)) write(unit, *) msg

      write(msg, *) "NUMBER_OF_PROCESSORS", numPETs
      print *, msg
      call ESMF_LogWrite(msg, ESMF_LOG_INFO)
      if (present(unit)) write(unit, *) msg

      end subroutine ESMF_TestStart

!------------------------------------------------------------------------------

      end module ESMF_TestMod
