! $Id: ESMF_Test.F90,v 1.10.2.2 2009/01/21 21:25:25 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
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
      use ESMF_UtilTypesMod
      use ESMF_BaseMod
      use ESMF_LogErrMod
      use ESMF_VMMod
      use ESMF_InitMod
      implicit none

! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_Test
      public ESMF_TestGlobal
      public ESMF_TestEnd
      public ESMF_TestNumPETs
      public ESMF_TestMinPETs
      public ESMF_TestMaxPETs
      public ESMF_TestResultsGather
      public ESMF_TestStart
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Test.F90,v 1.10.2.2 2009/01/21 21:25:25 cdeluca Exp $'

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
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOG_INFO)
        if (present(unit)) write(unit, *) trim(msg)
      else
        write(msg, *) "FAIL ", trim(name), ", ", trim(file), ", line", &
                      line, trim(failMsg)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOG_INFO)
        if (present(unit)) write(unit, *) trim(msg)
        result = result + 1  ! count total failures; 0 = all pass
      end if

      end subroutine ESMF_Test


!-------------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_TestGlobal - Print PASS/FAIL messages for global test results
!
! !INTERFACE:
      subroutine ESMF_TestGlobal(condition, name, failMsg, result, file, line, &
        unit)

! !ARGUMENTS:
      logical, intent(in) :: condition      ! pass/fail condition
      character(*), intent(in) :: name      ! test name
      character(*), intent(in) :: failMsg   ! fail message
      integer, intent(inout) :: result      ! accumulated result
      character(*), intent(in) :: file      ! test file name
      integer, intent(in) :: line           ! test file line number
      integer, intent(in), optional :: unit ! additional output unit number

! !DESCRIPTION:
!     Prints a {\tt PASS} message to stdout if {\tt condition} is true on all
!     PETs, and a {\tt FAIL} message otherwise.  If {\tt unit}
!     is specified, will in addition write the same message to that 
!     Fortran unit number.
!
!EOP
!-------------------------------------------------------------------------------

!      character(2*ESMF_MAXSTR) :: msg
      type(ESMF_VM):: vm
      integer:: petCount, localPet, localResult, localrc


      if(condition) then
        localResult = ESMF_SUCCESS
      else
        localResult = ESMF_FAILURE
        result = result + 1  ! count total failures; 0 = all pass
      end if
      
      ! for now "name" and "failMsg" are ignored because this impl. is 
      ! based on ESMF_TestResultsGather() which does not take those arguments

      call ESMF_VMGetGlobal(vm, rc=localrc)
      call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=localrc)
      call ESMF_TestResultsGather(vm, localPet, petCount, localResult, &
        file, line, unit, rc=localrc)

      end subroutine ESMF_TestGlobal


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
      print *, trim(msg)
      call ESMF_LogWrite(trim(msg), ESMF_LOG_INFO)
      if (present(unit)) write(unit, *) trim(msg)

      write(msg, *) "Ending Test, file ", trim(file), ", line", line
      print *, trim(msg)
      call ESMF_LogWrite(trim(msg), ESMF_LOG_INFO)
      if (present(unit)) write(unit, *) trim(msg)

      call ESMF_Finalize(rc=rc)
      if (rc .ne. ESMF_SUCCESS) then
          write(msg, *) "Failure in Finalizing ESMF"
          print *, trim(msg)
          call ESMF_LogWrite(trim(msg), ESMF_LOG_INFO)
          if (present(unit)) write(unit, *) trim(msg)
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
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOG_INFO)
        if (present(unit)) write(unit, *) trim(msg)
        return
      end if

      call ESMF_VMGet(globalVM, petCount=numPETs, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
        failMsg = "Unable to get number of PETS from global VM" 
        write(msg, *) "FAIL ", trim(file), ", line", &
                      line, trim(failMsg)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOG_INFO)
        if (present(unit)) write(unit, *) trim(msg)
        return
      endif

      ! Return neither a PASS or FAIL message, but SKIP.  The nightly
      ! build scripts are smarter about not looking for output from a
      ! file which only contains multiproc tags if it is being run uni,
      ! but this is more for the user to see.
      if (petCount .gt. numPETs) then
        write(failMsg, *) "These tests must run on at least", petCount, " processors."
        write(msg, *) "SKIP ", trim(file), ", line", &
                      line, trim(failMsg)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOG_INFO)
        if (present(unit)) write(unit, *) trim(msg)
        return
      endif

      ESMF_TestMinPETs = .true.
      return

      end function ESMF_TestMinPETs

!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_TestMaxPETs - Verify there are not too many PETs
!
! !INTERFACE:
      function ESMF_TestMaxPETs(petCount, file, line, unit)

! !RETURN VALUE:
      logical :: ESMF_TestMaxPETs

! !ARGUMENTS:
      integer, intent(in) :: petCount       ! maximum number of acceptable PETs
      character(*), intent(in) :: file      ! test file name
      integer, intent(in) :: line           ! test file line number
      integer, intent(in), optional :: unit ! additional output unit number

! !DESCRIPTION:
!     Verifies we are not running on too many PETs.
!     If {\tt unit} is specified, will in addition write the same message 
!     to that Fortran unit number.
!
!EOP
!-------------------------------------------------------------------------------

      character(ESMF_MAXSTR) :: msg, failMsg
      type(ESMF_VM) :: globalVM
      integer :: numPETs, localrc

      ! assume failure until sure of success
      ESMF_TestMaxPETs = .false.

      ! Get the global VM and pet count.
      call ESMF_VMGetGlobal(globalVM, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
        failMsg = "Unable to get global VM" 
        write(msg, *) "FAIL ", trim(file), ", line", line, trim(failMsg)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOG_INFO)
        if (present(unit)) write(unit, *) trim(msg)
        return
      end if

      call ESMF_VMGet(globalVM, petCount=numPETs, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
        failMsg = "Unable to query global VM" 
        write(msg, *) "FAIL ", trim(file), ", line", &
                      line, trim(failMsg)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOG_INFO)
        if (present(unit)) write(unit, *) trim(msg)
        return
      endif

      ! Return neither a PASS or FAIL message, but SKIPPED.  The nightly
      ! build scripts are smarter about not looking for output from a
      ! file which only contains multiproc tags if it is being run uni,
      ! but this is more for the user to see.
      if (petCount .lt. numPETs) then
        write(failMsg, *) "These tests must run not more than", petCount, " processors."
        write(msg, *) "SKIP ", trim(file), ", line", &
                      line, trim(failMsg)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOG_INFO)
        if (present(unit)) write(unit, *) trim(msg)
        return
      endif

      ESMF_TestMaxPETs = .true.
      return

      end function ESMF_TestMaxPETs

!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_TestNumPETs - Verify there are the correct number of PETs
!
! !INTERFACE:
      function ESMF_TestNumPETs(petCount, file, line, unit)

! !RETURN VALUE:
      logical :: ESMF_TestNumPETs

! !ARGUMENTS:
      integer, intent(in) :: petCount       ! exact number of acceptable PETs
      character(*), intent(in) :: file      ! test file name
      integer, intent(in) :: line           ! test file line number
      integer, intent(in), optional :: unit ! additional output unit number

! !DESCRIPTION:
!     Verifies we are running on exactly the required number of PETs.
!     If {\tt unit} is specified, will in addition write the same message 
!     to that Fortran unit number.
!
!EOP
!-------------------------------------------------------------------------------

      character(ESMF_MAXSTR) :: msg, failMsg
      type(ESMF_VM) :: globalVM
      integer :: numPETs, localrc

      ! assume failure until sure of success
      ESMF_TestNumPETs = .false.

      ! Get the global VM and pet count.
      call ESMF_VMGetGlobal(globalVM, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
        failMsg = "Unable to get global VM" 
        write(msg, *) "FAIL ", trim(file), ", line", line, trim(failMsg)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOG_INFO)
        if (present(unit)) write(unit, *) trim(msg)
        return
      end if

      call ESMF_VMGet(globalVM, petCount=numPETs, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
        failMsg = "Unable to query global VM" 
        write(msg, *) "FAIL ", trim(file), ", line", &
                      line, trim(failMsg)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOG_INFO)
        if (present(unit)) write(unit, *) trim(msg)
        return
      endif

      ! Return neither a PASS or FAIL message, but SKIPPED.  The nightly
      ! build scripts are smarter about not looking for output from a
      ! file which only contains multiproc tags if it is being run uni,
      ! but this is more for the user to see.
      if (petCount .ne. numPETs) then
        write(failMsg, *) "These tests must run on exactly", petCount, " processors."
        write(msg, *) "SKIP ", trim(file), ", line", &
                      line, trim(failMsg)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOG_INFO)
        if (present(unit)) write(unit, *) trim(msg)
        return
      endif

      ESMF_TestNumPETs = .true.
      return

      end function ESMF_TestNumPETs


!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_TestResultsGather - Gathers test results from all Pets and prints out a PASS/FAIL message
!
! !INTERFACE:
      subroutine ESMF_TestResultsGather(vm, localPet, petCount, testResults, file, line, unit,  rc)

! !ARGUMENTS:
      type(ESMF_VM), intent(in) :: vm     ! the vm of this pet
      integer, intent(in) :: localPet     ! number of this pet
      integer, intent(in) :: petCount     ! number of pets
      integer, intent(in) :: testResults  ! test results for this pet
      character(*), intent(in) :: file  ! test file name
      integer, intent(in) :: line           ! test file line number
      integer, intent(in), optional :: unit ! additional output unit number
      integer, intent(out), optional :: rc ! return code

! !DESCRIPTION:
!     The gatherPet gathers the test results, PASS/FAIl, from all other
!     Pets and prints out a PASS/FAIL Message . This subroutine should
!     be called at the end of system tests and use test cases.
!
!EOP
!-------------------------------------------------------------------------------
      character(ESMF_MAXSTR) :: msg
!      character(ESMF_MAXSTR) :: failMsg
      integer, allocatable:: array1(:), array2(:)	
      integer:: finalrc, gatherRoot, i, localrc

      allocate(array1(petCount))
      allocate(array2(1))
      ! Store test results
      array2(1) = testResults
      gatherRoot = 0

      ! Don't Gather until all pets are done
      call ESMF_VMBarrier(vm, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
          write(msg, *) " FAIL  ESMF_VMBarrier failed.  Error code ", localrc
          print *, trim(msg)
          if (present(unit)) write(unit, *) trim(msg)
          if (present(rc)) rc = localrc
          return
      endif

      

      ! Gather test results
      call ESMF_VMGather(vm, sendData=array2, recvData=array1, count=1, &
      root=gatherRoot, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
          write(msg, *) " FAIL  ESMF_VMGather failed.  Error code ", localrc
          print *, trim(msg)
          if (present(unit)) write(unit, *) trim(msg)
          if (present(rc)) rc = localrc
          return
      endif


      ! assume success
      finalrc=ESMF_SUCCESS

      ! The gather pet checks the results and prints out PASS/FAIL message.
      if (localPet==gatherRoot) then
        do i=1, petCount
                if (array1(i).EQ.ESMF_FAILURE) finalrc = ESMF_FAILURE
        enddo
        if (finalrc.EQ.ESMF_SUCCESS) then
            print *, " PASS: ", trim(file), line
            call c_ESMC_PrintPassFlush(); ! print and flush out of C++
        else
            print *, " FAIL: ", trim(file), line
        endif
      endif
      deallocate(array1)
      deallocate(array2)

      ! Don't end test until all pets are done
      call ESMF_VMBarrier(vm, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
          write(msg, *) " FAIL  ESMF_VMBarrier failed.  Error code ", localrc
          print *, trim(msg)
          if (present(unit)) write(unit, *) trim(msg)
          if (present(rc)) rc = localrc
          return
      endif
      rc=ESMF_SUCCESS
      return
                                                 
      end subroutine ESMF_TestResultsGather

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

      character(ESMF_MAXSTR) :: msg, logFileName
      type(ESMF_VM) :: globalVM
      integer :: numPETs, localrc, underScore, Period

      ! create a file name for the log file
      ! find locations of the underscore and period
      underScore = index (file, "_")
      Period = index (file, ".")
      logFileName = file(underScore+1:Period)  // "Log"


      ! initialize the framework.  if this fails, print a message directly
      ! because there is no guarentee that the log code will be working.
      call ESMF_Initialize(vm=globalVM, defaultlogfilename=logFileName, &
                           defaultlogtype=ESMF_LOG_MULTI, rc=localrc)
                           !defaultlogtype=ESMF_LOG_SINGLE, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
          write(msg, *) "FAIL  Unable to initialize the ESMF Framework.  Error code ", localrc
          print *, trim(msg)
          if (present(unit)) write(unit, *) trim(msg)
          if (present(rc)) rc = localrc
          return
      endif

      call ESMF_VMGet(globalVM, petCount=numPETs, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
          write(msg, *) "FAIL  Unable to get number of PETs.  Error code ", localrc
          print *, trim(msg)
          if (present(unit)) write(unit, *) trim(msg)
          if (present(rc)) rc = localrc
          return
       endif

      write(msg, *) "Beginning Test, file ", trim(file), ", line", line
      print *, trim(msg)
      call ESMF_LogWrite(trim(msg), ESMF_LOG_INFO)
      if (present(unit)) write(unit, *) trim(msg)

      write(msg, *) "NUMBER_OF_PROCESSORS", numPETs
      print *, trim(msg)
      call ESMF_LogWrite(trim(msg), ESMF_LOG_INFO)
      if (present(unit)) write(unit, *) trim(msg)

      end subroutine ESMF_TestStart

!------------------------------------------------------------------------------

      end module ESMF_TestMod
