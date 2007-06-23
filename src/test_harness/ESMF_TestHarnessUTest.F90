!
! Earth System Modeling Framework
! Copyright 2002-2005, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
  program esmf_test_harness

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>

!------------------------------------------------------------------------------
!USE_TEST_CASE
!==============================================================================
!BOP
! !PROGRAM: ESMF_TEST_HARNESS - Data redistribution and regridding tests
!
! !DESCRIPTION:
!
! The code in this file drives the testing harness for testing the redistribution
! and regridding methods. 
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod  
  use ESMF_Mod
  use ESMF_TestHarnessMod
  implicit none

  ! cumulative result: count failures; no failure equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc, finalrc

  ! local args needed to create/construct objects
  type(ESMF_VM)          :: vm

  ! global storage of test specification
  type (problem_descriptor_record), allocatable :: problem_descriptor(:)

  ! local variables
  integer :: localPet, petCount
  integer :: test_report_flag
  integer :: problem_descriptor_count
  integer :: count
  character(ESMF_MAXSTR) :: test_harness_name = "test_harness.rc"

  ! -------- beginning of executable code below here -------
  !   !Set finalrc to success
  finalrc = ESMF_SUCCESS

  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

  ! get global vm information
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

  call ESMF_VMGet(vm, localPet, petCount=petCount, rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

  !
  call read_test_filenames(test_report_flag,problem_descriptor_count,rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

  do count=1,problem_descriptor_count
     call interpret_descriptor_string(problem_descriptor(count),rc)
  enddo

  do count=1,problem_descriptor_count
     call read_distgrid(problem_descriptor(count), rc)
  enddo



  ! report results
  call TestHarnessReport(test_report_flag,rc)

  call ESMF_TestEnd(result, ESMF_SRCLINE)

  ! clean up and release memory
  deallocate( problem_descriptor )

  ! -------- end of unit test code ------------------------

!============================================================================

contains

!============================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
! !IROUTINE: read_test_name

! !INTERFACE:
  subroutine read_test_filenames(report_flag,npds,returnrc)
!
! !ARGUMENTS:
  integer, intent(  out) :: report_flag
  integer, intent(  out) :: returnrc

!
! !DESCRIPTION:
! Routine opens the test_harness.rc config file, extracts the problem descriptor file 
! names, and then opens those files and extracts the the problem descriptor strings 
! and saves the names of the associated specification files.
!

  ! local ESMF types
  type(ESMF_Config)      :: localcf

  ! local parameters
  character(ESMF_MAXSTR), parameter :: test_class_name  = "test_class:"
  character(ESMF_MAXSTR), parameter :: test_report_name = "test_report:"


  ! local character types
  type (character_array), allocatable :: descriptor_file(:)

  ! local character strings
  character(ESMF_MAXSTR) :: ltest_class, ltag

  integer :: file, nfiles, ncolumns, npds
  integer :: localrc = ESMF_SUCCESS
  logical :: flag = .true.

  ! initialize return flag to success
  returnrc = ESMF_SUCCESS

  !--------------------------------------------------------------------------
  ! create config handle and load the testing harness config file
  !--------------------------------------------------------------------------
  localcf = ESMF_ConfigCreate(localrc)
  if (localrc .ne. ESMF_SUCCESS) then
     print*,'error creating config tag'
     returnrc = ESMF_FAILURE
     return
  endif
  print*,'reading ',trim( test_harness_name )
  call ESMF_ConfigLoadFile(localcf, test_harness_name, rc=localrc )
  if (localrc .ne. ESMF_SUCCESS) then
     print*,'error loading config file'
     returnrc = ESMF_FAILURE
     return
  endif
  
  !--------------------------------------------------------------------------
  ! find and load the test class 
  !--------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(test_class_name), rc=localrc )
  if (localrc .ne. ESMF_SUCCESS) then
     print*,'error finding label test_class'
     returnrc = ESMF_FAILURE
     return
  endif

  call ESMF_ConfigGetAttribute(localcf, ltest_class, rc=localrc )
  if (localrc .ne. ESMF_SUCCESS) then
     print*,'error finding label test_class'
     returnrc = ESMF_FAILURE
     return
  endif
  print*,'test class is ',trim( ltest_class )

  !--------------------------------------------------------------------------
  ! determine if test report is requested
  !--------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(test_report_name), rc=localrc )
  if (localrc .ne. ESMF_SUCCESS) then
     print*,'error finding label test_report'
     returnrc = ESMF_FAILURE
     return
  endif

  !--------------------------------------------------------------------------
  ! read test report flag 
  !	0 = no output
  !	1 = report successful tests
  !    -1 = report test failures
  !	2 = report both successes and failures.
  !--------------------------------------------------------------------------
  call ESMF_ConfigGetAttribute(localcf, report_flag, rc=localrc )
  if (localrc .ne. ESMF_SUCCESS) then
     print*,'error finding label test_class'
     returnrc = ESMF_FAILURE
     return
  endif
  print*,'test report flag is', report_flag
  if ( report_flag.lt.-2 .or. report_flag.gt.2) then
     print*,'error, report flag improperly set.'
     returnrc = ESMF_FAILURE
     return
  endif

  !--------------------------------------------------------------------------
  ! based on whether exhaustive or nonexhaustive tests are to be run,  find 
  ! and load the problem descriptor file names
  !--------------------------------------------------------------------------
!#ifdef ESMF_EXHAUSTIVE
  ltag = 'exhaustive::'
  print *, "exhaustive"
!#else
!  ltag = 'nonexhaustive::'
!  print *, "nonexhaustive"
!#endif
  call ESMF_ConfigFindLabel(localcf, trim(ltag), rc=localrc )
  if (localrc .ne. ESMF_SUCCESS) then
     print*,'error can not find tag exhaustive/nonexhaustive'
     returnrc = ESMF_FAILURE
     return
  endif

  ! determine the number of entries
  call ESMF_ConfigGetDim(localcf, nfiles, ncolumns, trim(ltag), rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) then
     print*,'error getting table size'
     returnrc = ESMF_FAILURE
     return
  endif
  print*,'nfiles',nfiles

  !--------------------------------------------------------------------------
  ! find the problem descriptor file names and read them
  !--------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(ltag), rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) then
     print*,'error finding problem descriptor filenames'
     returnrc = ESMF_FAILURE
     return
  endif

  allocate( descriptor_file(nfiles) )

  do file=1,nfiles
     ! advance to new line in table
     call ESMF_ConfigNextLine(localcf, flag, rc=localrc)
 
     ! retrieve the problem descriptor filenames 
     call ESMF_ConfigGetAttribute(localcf, descriptor_file(file)%name, rc=localrc)
     if( localrc .ne. ESMF_SUCCESS ) then
        print*,'error getting attribute descriptor file name',                       &
                                                  trim(descriptor_file(file)%name)
        returnrc = ESMF_FAILURE
        return
     endif

     print*,'descriptor file name:',trim(descriptor_file(file)%name)
  enddo   ! file

  !--------------------------------------------------------------------------
  ! clean up CF
  !--------------------------------------------------------------------------
  call ESMF_ConfigDestroy(localcf, localrc)
  if (localrc .ne. ESMF_SUCCESS) then
     print*,'error creating config tag'
     returnrc = ESMF_FAILURE
     return
  endif

  !--------------------------------------------------------------------------
  ! take the list of problem descriptor files provided by test_harness.rc
  ! and extract all the problem descriptor strings and the associated 
  ! specifier file names. 
  !--------------------------------------------------------------------------
  call read_problem_descriptor_names(nfiles,descriptor_file,npds,localrc)

  !--------------------------------------------------------------------------
  !--------------------------------------------------------------------------
  deallocate( descriptor_file )
  !--------------------------------------------------------------------------
  !--------------------------------------------------------------------------
  end subroutine read_test_filenames

!23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! !IROUTINE: read_problem_descriptor_names

! !INTERFACE:
  subroutine read_problem_descriptor_names(nfiles,descriptor_file,npds,returnrc)
!
! !ARGUMENTS:
  integer, intent(in   ) :: nfiles
  type (character_array), intent(in   ) :: descriptor_file(:)
  integer, intent(  out) :: npds
  integer, intent(  out) :: returnrc

!
! !DESCRIPTION:

  ! local ESMF types
  type(ESMF_Config)      :: localcf

  ! local parameters
  character(ESMF_MAXSTR), parameter :: descriptor_name  = "problem_descriptor_string::"

  ! local character types
  type (character_array), allocatable :: dist_files(:), igrid_files(:)

  ! local character strings
  character(ESMF_MAXSTR) :: ltmp

  integer :: localrc = ESMF_SUCCESS
  logical :: flag = .true.

! local integer variables
  integer :: file, string, count, ncolumns, k
  integer :: nIGridFiles, nDistFiles
  integer, allocatable :: nstrings(:), ncol(:)

  ! initialize return flag to success
  returnrc = ESMF_SUCCESS

  !--------------------------------------------------------------------------
  ! open problem descriptor file(s) and count the number of descriptor strings
  !--------------------------------------------------------------------------
  allocate( nstrings(nfiles) )
  do file=1,nfiles
     ! create a new config handle for reading problem descriptor strings
     localcf = ESMF_ConfigCreate(localrc)
     if (localrc .ne. ESMF_SUCCESS) then
        print*,'error creating config tag'
        returnrc = ESMF_FAILURE
        return
     endif

     print*,'reading descriptor file: ',trim(descriptor_file(file)%name)

     call ESMF_ConfigLoadFile(localcf, trim(descriptor_file(file)%name),rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) then
        print*,'error loading problem descriptor config file',                     &
                                                trim(descriptor_file(file)%name)
        returnrc = ESMF_FAILURE
        return
     endif

     ! Search for the problem descriptor string table
     call ESMF_ConfigFindLabel(localcf, trim(descriptor_name), rc=localrc )
     if (localrc .ne. ESMF_SUCCESS) then
        print*,'error can not find tag problem_descriptor_string'
        returnrc = ESMF_FAILURE
        return
     endif

     ! determine the number of entries
     call ESMF_ConfigGetDim(localcf, nstrings(file), ncolumns,                    &
                                               trim(descriptor_name), rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) then
        print*,'error getting descriptor table size'
        returnrc = ESMF_FAILURE
        return
     endif
     if( nstrings(file) .le. 0 ) then
        print*,'error problem descriptor table empty'
        returnrc = ESMF_FAILURE
        return
     endif

     ! clean up CF
     call ESMF_ConfigDestroy(localcf, localrc)
     if (localrc .ne. ESMF_SUCCESS) then
        print*,'error creating config tag'
        returnrc = ESMF_FAILURE
        return
     endif

  enddo  ! file

  ! total number of problem descriptor strings in all files
  npds = sum(nstrings)
  print*,'allocating problem descriptor string of size', npds
  allocate( problem_descriptor( npds ) )

  !--------------------------------------------------------------------------
  ! open problem descriptor file(s) and extract test specifications
  !--------------------------------------------------------------------------
  count = 0
  do file=1,nfiles

     ! create a new config handle for reading problem descriptor strings
     localcf = ESMF_ConfigCreate(localrc)
     if (localrc .ne. ESMF_SUCCESS) then
        print*,'error creating config tag'
        returnrc = ESMF_FAILURE
        return
     endif

     call ESMF_ConfigLoadFile(localcf, trim(descriptor_file(file)%name), rc=localrc )
     if (localrc .ne. ESMF_SUCCESS) then
        print*,'error loading problem descriptor config file',                       &
                                                   trim(descriptor_file(file)%name)
        returnrc = ESMF_FAILURE
        return
     endif

     ! read the problem descriptor file names
     call ESMF_ConfigFindLabel(localcf, trim(descriptor_name), rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) then
        print*,'error finding problem descriptor filenames'
        returnrc = ESMF_FAILURE
        return
     endif

     ! extract column lengths of this file
     allocate( ncol(nstrings(file)) )

     do string=1,nstrings(file)
        ! advance to new line in table
        call ESMF_ConfigNextLine(localcf, flag , rc=localrc)
     
        ncol(string) = ESMF_ConfigGetLen(localcf, rc = localrc)
        if (localrc .ne. ESMF_SUCCESS) then
           print*,'error reading problem descriptor line #',file
           print*,'error with reading column length'
           returnrc = ESMF_FAILURE
           return
        endif
     enddo

     ! read the problem descriptor file names
     call ESMF_ConfigFindLabel(localcf, trim(descriptor_name), rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) then
        print*,'error finding problem descriptor filenames'
        returnrc = ESMF_FAILURE
        return
     endif

     do string=1,nstrings(file)
        ! advance to new line in table
        call ESMF_ConfigNextLine(localcf, flag , rc=localrc)
     
        allocate( dist_files(ncol(string)-1) )
        allocate( igrid_files(ncol(string)-1) )

        ! retrieve the problem descriptor filenames
        call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) then
           print*,'error reading problem descriptor string, line #',file
           returnrc = ESMF_FAILURE
           return
        endif

        ! validate the problem descriptor string
!       call validate_problem_descriptor_string(ltmp,localrc)
!       if (localrc .ne. ESMF_SUCCESS) then
!          print*,'error reading problem descriptor line #',line
!          returnrc = ESMF_FAILURE
!          return
!      endif

        count = count + 1
        if ( count.le.npds ) then
           problem_descriptor(count)%string = ltmp
        else
           print*,'error mismatch in problem descriptor string size'
           returnrc = ESMF_FAILURE
           return
        endif
!
        ! retrieve filenames associated with problem descriptor strings
        call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) then
           print*,'error reading problem descriptor line #',file
           returnrc = ESMF_FAILURE
           return
        endif

        !--------------------------------------------------------------------
        ! initialize local counters
        !--------------------------------------------------------------------
        nDistFiles = 0
        nIGridFiles = 0

        !--------------------------------------------------------------------
        ! check for specifier files
        ! if the flag exists extract the file name
        !--------------------------------------------------------------------
        do while (len_trim(ltmp) /= 0 )
           select case( trim(ltmp) )

           case('-d')     ! distIGrid specifier files
              call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
              if (localrc .ne. ESMF_SUCCESS) then
                 print*,'error reading problem descriptor line #',file
                 returnrc = ESMF_FAILURE
                 return
              endif
              ! once you find a flag, extract all the associated filenames 
              do while (trim(ltmp) /= '-g'.and. len_trim(ltmp) /= 0 )
                 nDistFiles = nDistFiles + 1
                 dist_files(nDistFiles)%name = trim(ltmp)
                 call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
              enddo  ! while

           case('-g')     ! IGrid specifier files
              call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
              if (localrc .ne. ESMF_SUCCESS) then
                 print*,'error reading problem descriptor line #',file
                 returnrc = ESMF_FAILURE
                 return
              endif
              ! once you find a flag, extract all the associated filenames 
              do while (trim(ltmp) /= '-d' .and. len_trim(ltmp) /= 0 )
                 nIGridFiles = nIGridFiles + 1
                 igrid_files(nIGridFiles)%name = trim(ltmp)
                 call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
              enddo  ! while

           case default   ! error, no flag
              ! flag error
              print*,'syntax error in problem descriptor string, line #',file
              localrc = ESMF_FAILURE
              return

           end select  ! case of filename
        enddo  ! while

        problem_descriptor(count)%distfiles%size = nDistFiles
        problem_descriptor(count)%igridfiles%size = nIGridFiles

        allocate( problem_descriptor(count)%distfiles%string(nDistFiles) )
        allocate( problem_descriptor(count)%igridfiles%string(nIGridFiles) )

        do  k=1,nDistFiles
          problem_descriptor(count)%distfiles%string(k)%name =dist_files(k)%name
        enddo
        ! clean up allocated workspace
        deallocate(dist_files)

        do  k=1,nIGridFiles
          problem_descriptor(count)%igridfiles%string(k)%name =igrid_files(k)%name
        enddo
        ! clean up allocated workspace
        deallocate(igrid_files)
     enddo   ! string
     deallocate( ncol )
  enddo   ! file
  !--------------------------------------------------------------------------
  ! clean up allocated workspace
  !--------------------------------------------------------------------------
  deallocate( nstrings )
  !--------------------------------------------------------------------------
  !--------------------------------------------------------------------------
  end subroutine read_problem_descriptor_names

!23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890

 !----------------------------------------------------------------------------

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! !IROUTINE: TestHarnessReport

! !INTERFACE:
  subroutine TestHarnessReport(report_flag,localrc)
!
! !ARGUMENTS:
  integer, intent(in   ) :: report_flag
  integer, intent(  out) :: localrc

!
! !DESCRIPTION:
! This routine provides an additional reporting capability of the test results
! in a more human readable form.
!--------------------------------------------------------------------------
! Action associated to test report flag value.
!    -2 = debug report
!    -1 = report test failures
!     0 = no output
!     1 = report successful tests
!     2 = report both successes and failures.
!--------------------------------------------------------------------------
   ! local variables
   integer :: count,k

   ! initialize local rc
   localrc = ESMF_SUCCESS

!--------------------------------------------------------------------------
! begin 
!--------------------------------------------------------------------------
  select case (report_flag)
     case(-2)
     ! debug report
  do count=1,problem_descriptor_count
     print*,'Problem Descriptor String:',problem_descriptor(count)%string
     print*,'nDistFiles:',problem_descriptor(count)%distfiles%size
     do  k=1,problem_descriptor(count)%distfiles%size
        print*,k,problem_descriptor(count)%distfiles%string(k)%name
     enddo

     print*,'nIGridFiles:',problem_descriptor(count)%igridfiles%size
     do  k=1,problem_descriptor(count)%igridfiles%size
       print*,k,problem_descriptor(count)%igridfiles%string(k)%name
     enddo
     print*,'------------------------------------------'

  print*,'          '
  print*,'process: ',problem_descriptor(count)%process%name
  print*,'process tag ',problem_descriptor(count)%process%tag
  print*,'          '
  print*,'Source dist rank:',problem_descriptor(count)%src_dist%rank
  print*,'Source dist topology:',problem_descriptor(count)%src_dist%topology
  print*,'Source dist order:',problem_descriptor(count)%src_dist%order
  print*,'----'
  print*,'Source igrid rank:',problem_descriptor(count)%src_igrid%rank
  print*,'Source igrid topology:',problem_descriptor(count)%src_igrid%topology
  print*,'Source igrid order:',problem_descriptor(count)%src_igrid%order
  print*,'==========='
  print*,'Destination dist rank:',problem_descriptor(count)%dst_dist%rank
  print*,'Destination dist topology:',problem_descriptor(count)%dst_dist%topology
  print*,'Destination dist order:',problem_descriptor(count)%dst_dist%order
  print*,'----'
  print*,'Destination igrid rank:',problem_descriptor(count)%dst_igrid%rank
  print*,'Destination igrid topology:',problem_descriptor(count)%dst_igrid%topology
  print*,'Destination igrid order:',problem_descriptor(count)%dst_igrid%order
  enddo

     case(-1)
        ! only report failures
        call ReportFailure(localrc)

     case(0)
        ! no report
        return

     case(1)
        ! only report success
        call ReportSuccess(localrc)
  
     case(2)
        ! report both successes and failures
        call ReportFailure(localrc)
        call ReportSuccess(localrc)

     case default
     ! flag error
     print*,'error, report flag improperly set.'
     localrc = ESMF_FAILURE
     return

  end select  ! case of report flag

  end subroutine TestHarnessReport

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! !IROUTINE: ReportSuccess

! !INTERFACE:
  subroutine ReportSuccess(localrc)
!
! !ARGUMENTS: 
  integer, intent(out) :: localrc

!
! !DESCRIPTION:
! This routine produces a human readable report of the successful test results.
!--------------------------------------------------------------------------
  ! initialize local rc
  localrc = ESMF_SUCCESS

  print*,'Output human readable report of test successes'

  end subroutine ReportSuccess

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! !IROUTINE: ReportFailure

! !INTERFACE: 
  subroutine ReportFailure(localrc)
!
! !ARGUMENTS:
  integer, intent(out) :: localrc

!
! !DESCRIPTION:
! This routine produces a human readable report of the failed test results.
!--------------------------------------------------------------------------
  ! initialize local rc
  localrc = ESMF_SUCCESS

  print*,'Output human readable report of test failures'

  end subroutine ReportFailure

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------


  end program ESMF_Test_Harness
