! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
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
      use ESMF_IOUtilMod
      implicit none

! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_Test
      public ESMF_STest
      public ESMF_TestGlobal
      public ESMF_TestEnd
      public ESMF_TestFileCompare
      public ESMF_TestNumPETs
      public ESMF_TestMinPETs
      public ESMF_TestMaxPETs
      public ESMF_TestResultsGather
      public ESMF_TestStart
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

!==============================================================================
      
!     ! Test start time
      real(ESMF_KIND_R8), save :: start_time
      integer, save :: PETnum

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
      character(16) :: linestr

      write (linestr,*) line
      linestr = adjustl (linestr)

      if(condition) then
        write(msg, *) "PASS ", trim(name), ", ", trim(file), ", line ", trim (linestr)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
        if (present(unit)) write(unit, *) trim(msg)
      else
        write(msg, *) "FAIL ", trim(name), ", ", trim(file), ", line ", &
                      trim (linestr), ": ", trim(failMsg)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
        if (present(unit)) write(unit, *) trim(msg)
        result = result + 1  ! count total failures; 0 = all pass
      end if

      end subroutine ESMF_Test



!-------------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_STest - Print PASS/FAIL and number of processor messages for tests
!
! !INTERFACE:
      subroutine ESMF_STest(condition, name, failMsg, result, file, line, unit, &
        petCount)

! !ARGUMENTS:
      logical, intent(in) :: condition      ! pass/fail condition
      character(*), intent(in) :: name      ! test name
      character(*), intent(in) :: failMsg   ! fail message
      integer, intent(inout) :: result      ! accumulated result
      character(*), intent(in) :: file      ! test file name
      integer, intent(in) :: line           ! test file line number
      integer, intent(in), optional :: unit ! additional output unit number
      integer, intent(in), optional :: petCount ! number of PETs if need override
      

! !DESCRIPTION:
!     Gets the PET count and prints out a number of processors message.
!     Prints a {\tt PASS} message to stdout if {\tt condition} is true,
!     and a {\tt FAIL} message if {\tt condition} is false.  If {\tt unit}
!     is specified, will in addition write the same message to that
!     Fortran unit number.
!
!EOP
!-------------------------------------------------------------------------------

      type(ESMF_VM):: vm
      integer:: petCountOpt, localrc
      character(ESMF_MAXSTR) :: msg
     
      if (present(petCount)) then
        petCountOpt = petCount
      else
        call ESMF_VMGetGlobal(vm, rc=localrc)
        call ESMF_VMGet(vm, petCount=petCountOpt, rc=localrc)
      endif
      write(msg, *) "NUMBER_OF_PROCESSORS", petCountOpt
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)

      call ESMF_Test(condition, name, failMsg, result,  file, line, unit)

      end subroutine ESMF_STest


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

      !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
      !TODO: Remove the following dummy test when implementing this method
      if (name==name) continue

      !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
      !TODO: Remove the following dummy test when implementing this method
      if (failMsg==failMsg) continue

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
      subroutine ESMF_TestEnd(file, line, unit)

! !ARGUMENTS:
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

      integer :: rc, localrc
      character(ESMF_MAXSTR) :: msg
      real(ESMF_KIND_R8) :: end_time, elapsed_time
      character(16) :: linestr

      write (linestr,*) line
      linestr = adjustl (linestr)

      write(msg, *) "Ending Test, file ", trim(file), ", line ", trim (linestr)
      print *, trim(msg)
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
      if (present(unit)) write(unit, *) trim(msg)

      call ESMF_Finalize(rc=rc)
      if (rc .ne. ESMF_SUCCESS) then
          write(msg, *) "Failure in Finalizing ESMF"
          print *, trim(msg)
          call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
          if (present(unit)) write(unit, *) trim(msg)
      endif

      ! Calculate & print out test elasped time
      call cpu_time(end_time)
      elapsed_time = (end_time-start_time) * 1000.0  ! msec
      write(msg, *) "PET", PETnum, " Test Elapsed Time ", elapsed_time, " msec."
      print *, trim(msg)

      end subroutine ESMF_TestEnd


#undef ESMF_METHOD
#define ESMF_METHOD 'ESMF_TestFileCompare'
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_TestFileCompare - Compare two text files for equivalence
!
! !INTERFACE:
      function ESMF_TestFileCompare(file1, file2, exclusionList)

! !RETURN VALUE:
      logical :: ESMF_TestFileCompare

! !ARGUMENTS:
      character(*), intent(in) :: file1     ! test file name
      character(*), intent(in) :: file2     ! test file name
      character(*), intent(in), optional :: exclusionList(:)

! !DESCRIPTION:
!     Compares two files to see if they are identical.
!
!     Restrictions:
!     1.) Only text files are supported
!     2.) On systems which do not support recursive I/O, this function
!     should not be called from the I/O list of an I/O statement.
!     3.) On Windows, blank lines are ignored to avoid issues with cr/lfs.
!
!     The arguments are:
!     \begin{description}
!     \item [file1]
!       First of two files to be compared.
!     \item [file2]
!        Second of two files to be compared.
!     \item [{[exclusionList]}]
!       Character strings which, if any are present in text records being
!       compared, will cause a comparison error to be bypassed.  This is
!       useful for records which might legitimately differ between the two
!       files - such as a date or version string.
!     \end{description}
!EOP
!-------------------------------------------------------------------------------

      logical :: exclusions
      integer :: i
      integer :: ioerr1, ioerr2
      integer :: localrc
      character(1024) :: string1, string2
      character(ESMF_MAXSTR) :: errmsg
      integer :: unit1, unit2

      ESMF_TestFileCompare = .false.
      exclusions = present (exclusionList)

      call ESMF_UtilIOUnitGet (unit=unit1, rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
        call ESMF_LogWrite (msg='Can not obtain IO unit number',  &
            logmsgFlag=ESMF_LOGMSG_ERROR)
        write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
            ': Can not obtain IO unit number'
        return
      end if        

      open (unit1, file=file1,  &
        form='formatted', status='old', action='read',  &
        iostat=ioerr1)
      if (ioerr1 /= 0) then
        errmsg = 'Can not open file: ' // file1
        call ESMF_LogWrite (msg=errmsg, logmsgFlag=ESMF_LOGMSG_ERROR)
        write (ESMF_UtilIOStderr,*) ESMF_METHOD, ': ' // trim (errmsg)
        return
      end if        

      call ESMF_UtilIOUnitGet (unit=unit2, rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
        call ESMF_LogWrite (msg='Can not obtain IO unit number',  &
            logmsgFlag=ESMF_LOGMSG_ERROR)
        write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
            ': Can not obtain IO unit number'
        close (unit1)
        return
      end if

      open (unit2, file=file2,  &
        form='formatted', status='old', action='read',  &
        iostat=ioerr2)
      if (ioerr2 /= 0) then
        errmsg = 'Can not open file: ' // file2
        call ESMF_LogWrite (msg=errmsg, logmsgFlag=ESMF_LOGMSG_ERROR)
        write (ESMF_UtilIOStderr,*) ESMF_METHOD, ': ' // trim (errmsg)
        close (unit1)
        return
      end if        

read_loop:  &
      do
        do
          read (unit1, '(a)', iostat=ioerr1) string1
          if (ioerr1 /= 0) exit
          ! Ignore blank lines due to cr/lf vs newline issues
          do, i=1, len (string1)
            string1(i:i) = merge (string1(i:i), ' ', string1(i:i) /= achar (13))
          end do
          if (string1 /= ' ') exit
        end do

        do
          read (unit2, '(a)', iostat=ioerr2) string2
          if (ioerr2 /= 0) exit
          ! Ignore blank lines due to cr/lf vs newline issues
          do, i=1, len (string2)
            string2(i:i) = merge (string2(i:i), ' ', string2(i:i) /= achar (13))
          end do
          if (string2 /= ' ') exit
        end do

        if (ioerr1 /= ioerr2) then
!          print *, ESMF_METHOD, ': read iostats differ:', ioerr1, ioerr2
          exit
        end if

        select case (ioerr1)
        case (:-1)
          ESMF_TestFileCompare = .true.
          exit

        case (0)
          if (string1 /= string2) then
            if (exclusions) then
exclusion_loop:  &
              do, i=1, size (exclusionList)
                if (index (string1, trim (exclusionList(i))) /= 0 .and.  &
                    index (string2, trim (exclusionList(i))) /= 0) then
                  exit exclusion_loop
                end if
              end do exclusion_loop
              if (i > size (exclusionList)) exit read_loop
            else
#if 0
              print *, ESMF_METHOD, ': comparison error:'
              print *, '  string1 = >', trim (string1), '<'
              print *, '  string2 = >', trim (string2), '<'
#endif
              exit read_loop
            end if
          end if

        case (1:)
          write (errmsg, '(a,i4)') 'unknown iostat = ', ioerr1
          call ESMF_LogWrite (msg=errmsg, logmsgFlag=ESMF_LOGMSG_ERROR)
          print *, ESMF_METHOD, ': ', trim (errmsg)
          exit
        end select

      end do read_loop

      close (unit2)
      close (unit1)


      end function ESMF_TestFileCompare

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
      character(16) :: linestr

      write (linestr,*) line
      linestr = adjustl (linestr)

      ! assume failure until sure of success
      ESMF_TestMinPETs = .false.

      ! Get the global VM and pet count.
      call ESMF_VMGetGlobal(globalVM, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
        failMsg = "Unable to get global VM" 
        write(msg, *) "FAIL ", trim(file), ", line ", trim (linestr), ": ", trim(failMsg)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
        if (present(unit)) write(unit, *) trim(msg)
        return
      end if

      call ESMF_VMGet(globalVM, petCount=numPETs, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
        failMsg = "Unable to get number of PETS from global VM" 
        write(msg, *) "FAIL ", trim(file), ", line ", &
                      trim (linestr), ": ", trim(failMsg)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
        if (present(unit)) write(unit, *) trim(msg)
        return
      endif

      ! Return neither a PASS or FAIL message, but SKIP.  The nightly
      ! build scripts are smarter about not looking for output from a
      ! file which only contains multiproc tags if it is being run uni,
      ! but this is more for the user to see.
      if (petCount .gt. numPETs) then
        write(failMsg, *) "These tests must run on at least", petCount, " processors."
        write(msg, *) "SKIP ", trim(file), ", line ", &
                      trim (linestr), ": ", trim(failMsg)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
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
      character(16) :: linestr

      write (linestr,*) line
      linestr = adjustl (linestr)

      ! assume failure until sure of success
      ESMF_TestMaxPETs = .false.

      ! Get the global VM and pet count.
      call ESMF_VMGetGlobal(globalVM, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
        failMsg = "Unable to get global VM" 
        write(msg, *) "FAIL ", trim(file), ", line ", trim (linestr), ": ", trim(failMsg)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
        if (present(unit)) write(unit, *) trim(msg)
        return
      end if

      call ESMF_VMGet(globalVM, petCount=numPETs, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
        failMsg = "Unable to query global VM" 
        write(msg, *) "FAIL ", trim(file), ", line ", &
                      trim (linestr), ": ", trim(failMsg)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
        if (present(unit)) write(unit, *) trim(msg)
        return
      endif

      ! Return neither a PASS or FAIL message, but SKIPPED.  The nightly
      ! build scripts are smarter about not looking for output from a
      ! file which only contains multiproc tags if it is being run uni,
      ! but this is more for the user to see.
      if (petCount .lt. numPETs) then
        write(failMsg, *) "These tests must run not more than", petCount, " processors."
        write(msg, *) "SKIP ", trim(file), ", line ", &
                      trim (linestr), ": ", trim(failMsg)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
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
      character(16) :: linestr

      write (linestr,*) line
      linestr = adjustl (linestr)

      ! assume failure until sure of success
      ESMF_TestNumPETs = .false.

      ! Get the global VM and pet count.
      call ESMF_VMGetGlobal(globalVM, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
        failMsg = "Unable to get global VM" 
        write(msg, *) "FAIL ", trim(file), ", line ", trim (linestr), ": ", trim(failMsg)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
        if (present(unit)) write(unit, *) trim(msg)
        return
      end if

      call ESMF_VMGet(globalVM, petCount=numPETs, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
        failMsg = "Unable to query global VM" 
        write(msg, *) "FAIL ", trim(file), ", line ", &
                      trim (linestr), ": ", trim(failMsg)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
        if (present(unit)) write(unit, *) trim(msg)
        return
      endif

      ! Return neither a PASS or FAIL message, but SKIPPED.  The nightly
      ! build scripts are smarter about not looking for output from a
      ! file which only contains multiproc tags if it is being run uni,
      ! but this is more for the user to see.
      if (petCount .ne. numPETs) then
        write(failMsg, *) "These tests must run on exactly", petCount, " processors."
        write(msg, *) "SKIP ", trim(file), ", line ", &
                      trim (linestr), ": ", trim(failMsg)
        print *, trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
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
      subroutine ESMF_TestResultsGather(vm, localPet, petCount, testResults, &
        file, line, unit,  rc)

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
      character(16) :: linestr

      write (linestr,*) line
      linestr = adjustl (linestr)

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
      rootPet=gatherRoot, rc=localrc)
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
            print *, " PASS: ", trim(file), ' ', trim (linestr)
            call c_ESMC_PrintPassFlush(); ! print and flush out of C++
        else
            print *, " FAIL: ", trim(file), ' ', trim (linestr)
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
      
      if (present(rc)) rc=ESMF_SUCCESS
                                                 
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
      character(16) :: linestr

      write (linestr,*) line
      linestr = adjustl (linestr)

      ! create a file name for the log file
      ! find locations of the underscore and period
      underScore = index (file, "_")
      Period = index (file, ".")
      logFileName = file(underScore+1:Period)  // "Log"



      ! initialize the framework.  if this fails, print a message directly
      ! because there is no guarentee that the log code will be working.
      call ESMF_Initialize(vm=globalVM, defaultlogfilename=logFileName, &
                           logkindflag=ESMF_LOGKIND_MULTI, rc=localrc)
                           !logkindflag=ESMF_LOGKIND_SINGLE, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
          write(msg, *) "FAIL  Unable to initialize the ESMF Framework.  Error code ", localrc
          print *, trim(msg)
          if (present(unit)) write(unit, *) trim(msg)
          if (present(rc)) rc = localrc
          return
      endif

      call ESMF_LogSet (flush=.true.)

      ! get test start time
      call cpu_time(start_time)

      call ESMF_VMGet(globalVM, petCount=numPETs, localPet=PETnum, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
          write(msg, *) "FAIL  Unable to get number of PETs.  Error code ", localrc
          print *, trim(msg)
          if (present(unit)) write(unit, *) trim(msg)
          if (present(rc)) rc = localrc
          return
       endif

      write(msg, *) "Beginning Test, file ", trim(file), ", line ", trim (linestr)
      print *, trim(msg)
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
      if (present(unit)) write(unit, *) trim(msg)

      write(msg, *) "NUMBER_OF_PROCESSORS", numPETs
      print *, trim(msg)
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
      if (present(unit)) write(unit, *) trim(msg)
      
      if (present(rc)) rc=ESMF_SUCCESS

      end subroutine ESMF_TestStart

!------------------------------------------------------------------------------

      end module ESMF_TestMod
