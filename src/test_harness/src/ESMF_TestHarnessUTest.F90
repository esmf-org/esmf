!
! Earth System Modeling Framework
! Copyright 2002-2005, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!===============================================================================
!
  program esmf_test_harness

!-------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>

!-------------------------------------------------------------------------------
!USE_TEST_CASE
!===============================================================================
!BOP
! !PROGRAM: ESMF_TEST_HARNESS - Data redistribution and regridding tests
!
! !DESCRIPTION:
!
! The code in this file drives the testing harness for testing the redistribution
! and regridding methods. 
!
!-------------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod  
  use ESMF_Mod
  use ESMF_TestHarnessMod
  use ESMF_TestHarnessGridMod
  use ESMF_TestHarnessDistMod
  implicit none

  ! cumulative result: count failures; no failure equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc, finalrc

  ! local args needed to create/construct objects
  type(ESMF_VM)          :: vm

  ! global storage of test specification
  type (harness_descriptor), save :: har

  ! local variables
  integer :: localPet, petCount, rootPet = 0
  integer :: test_report_flag
  integer :: problem_descriptor_count
  integer :: ncount
  integer :: k, n, kfile

  logical :: debugflag = .false.

  ! top level test harness config file
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


  !-----------------------------------------------------------------------------
  ! first stage of test
  !   - open the testharness configuration file "test_harness.rc" & extract info
  !     * read test class
  !     * read test harness report style
  !     * read names of problem descriptor files
  !     * populate the data structure with the information
  !   - open each problem descriptor file
  !     * extract the problem descriptor string
  !     * extract the accompanying specifier file names
  !     * populate the information into problem descritpion data structure
  !   - go back and open each specifier file & populate the data structure
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! read testharness configuration file, create the test harness record
  ! variable "harness" and extract (1) the test class, (2) report type, (3)
  ! number of records or problem descriptor files.
  !-----------------------------------------------------------------------------
  call read_testharness_config(rc)
  if (rc .ne. ESMF_SUCCESS) then
     print*,'Error reading file test_harness.rc - see log file for details'
     finalrc = ESMF_FAILURE
     call ESMF_TestEnd(result, ESMF_SRCLINE)
   ! goto 999
  endif
  !-----------------------------------------------------------------------------
  ! debugging statements
  !-----------------------------------------------------------------------------
  if(debugflag) then
     print*,'=----------in main-------------------------------='
     print*,'  Read test harness config debug statements '
     print*,'Problem descriptor Test Class:',har%testClass
     print*,'Total number of problem descriptor files:',har%numRecords
     do kfile=1,har%numRecords
        print*,kfile,' Descriptor file name:',trim(har%rcrd(kfile)%filename)
     enddo
     print*,'=------------------------------------------------='
  endif
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  ! read each of the problem descriptor files obtained from read_testharness_config 
  ! and extract the problem descriptor strings and the accompanying specifier
  ! filenames.
  !-----------------------------------------------------------------------------
  call read_descriptor_files(rc)
  if (rc .ne. ESMF_SUCCESS) then
     print*,'Error reading problem descriptor files - see log file for details'
     finalrc = ESMF_FAILURE
     call ESMF_TestEnd(result, ESMF_SRCLINE)
   ! goto 999
  endif
  !-----------------------------------------------------------------------------
  ! debugging statements
  !-----------------------------------------------------------------------------
  if(debugflag) then
     print*,'=---------in main--------------------------------='
     print*,'  Read descriptor file debug statements '
     do kfile=1,har%numRecords
        print*,'Problem descriptor filename: ',trim(har%rcrd(kfile)%filename)
        do k=1,har%rcrd(kfile)%numStrings
           print*,'>',trim( har%rcrd(kfile)%str(k)%pds )
           print*,'Number of Distribution Specifier files',                    &
                              har%rcrd(kfile)%str(k)%nDfiles
           do n=1,har%rcrd(kfile)%str(k)%nDfiles
              print*,'   ',n,                                                  &
                 trim( har%rcrd(kfile)%str(k)%Dfiles(n)%filename )
           enddo     ! n
           print*,'Number of Grid Specifier files',                            &
                              har%rcrd(kfile)%str(k)%nGfiles
           do n=1,har%rcrd(kfile)%str(k)%nGfiles
              print*,'   ',n,                                                  &
                 trim( har%rcrd(kfile)%str(k)%Gfiles(n)%filename )
              print*,'                                            '
           enddo     ! n
        enddo      ! k
     enddo      ! kfile
     print*,'=------------------------------------------------='
  endif

  !-----------------------------------------------------------------------------
  ! loops through the list of descriptor files and reads each problem
  ! descriptor string and its associated specifer files for (1) class, (2)
  ! distribution ensemble, and (3) grid ensemble.
  !-----------------------------------------------------------------------------
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

  !-----------------------------------------------------------------------------
  ! parse each of the problem descriptor strings and fill in the test harness
  ! record variable.
  !-----------------------------------------------------------------------------
  call parse_descriptors(rc)
  if (rc .ne. ESMF_SUCCESS) then
     print*,'Error parsing problem descriptor files - see log file for details'
     finalrc = ESMF_FAILURE
     goto 999
!    call ESMF_TestEnd(result, ESMF_SRCLINE)
  endif
  print*,'exiting parse descriptor string'

  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

  ! report results
  print*,'entering report test'
  call TestHarnessReport(rc)
  if (rc .ne. ESMF_SUCCESS) then
     print*,'Error reporting tests - see log file for details'
     finalrc = ESMF_FAILURE
     goto 999
!    call ESMF_TestEnd(result, ESMF_SRCLINE)
  endif
  print*,'exiting report test'

  ! clean up and release memory

  !-----------------------------------------------------------------------------
999 continue
  !-----------------------------------------------------------------------------

  call ESMF_TestEnd(result, ESMF_SRCLINE)


  ! -------- end of unit test code ------------------------

!===============================================================================

contains

!===============================================================================
! !IROUTINE: read_testharness_config

! !INTERFACE:
  subroutine read_testharness_config(localrc)
!
! !ARGUMENTS:
  integer, intent(  out) :: localrc

!
! !DESCRIPTION:
! Routine opens the top level config file "test_harness.rc", which specifies the 
! test class, the reporting style, and depending on how the ESMF_EXHAUSTIVE flag
! is set, extracts the list of files containing the problem descriptor strings.
!
! Upon completion, the routine returns the values to a public record
!       har%testClass               Problem Descriptor Test Class
!       har%reportType              Output Report type
!       har%numRecords              number of problem descriptor filenames
!       har%rcrd(k)%filename        kth problem descriptor filename
!===============================================================================

  ! local ESMF types
  type(ESMF_Config)      :: localcf

  ! local parameters
  character(ESMF_MAXSTR), parameter :: test_class_name   = "test_class:"
  character(ESMF_MAXSTR), parameter :: setup_report_name = "setup_report:"
  character(ESMF_MAXSTR), parameter :: test_report_name  = "test_report:"


  ! local character strings
  character(ESMF_MAXSTR) :: ltag

  ! local integer variables
  integer :: kfile, ncolumns

  ! local  logical
  logical :: flag = .true.

  ! initialize return code
  localrc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! create config handle and load the testing harness config file
  !-----------------------------------------------------------------------------
  localcf = ESMF_ConfigCreate(localrc)
  if( ESMF_LogMsgFoundError(localrc, "cannot create config object",            &
                            rcToReturn=localrc) ) return
                            
  call ESMF_ConfigLoadFile(localcf, trim(adjustL(test_harness_name)),          &
           rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
           "cannot load config file " // trim(adjustL(test_harness_name)),     &
           rcToReturn=localrc) ) return
  
  !-----------------------------------------------------------------------------
  ! find and read the test class 
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(adjustL(test_class_name)),           &
           rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
           "cannot find config label " // trim(adjustL(test_class_name)),      &
           rcToReturn=localrc) ) return

  call ESMF_ConfigGetAttribute(localcf, har%testClass, rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
           "cannot get value for label " // trim(adjustL(test_class_name)),    &
           rcToReturn=localrc) ) return

  !-----------------------------------------------------------------------------
  ! if the class is not supported, then post an error
  !-----------------------------------------------------------------------------
  if ( trim(adjustL(har%testClass)) /= 'ARRAY'  .and.                          &
       trim(adjustL(har%testClass)) /= 'BUNDLE' .and.                          &
       trim(adjustL(har%testClass)) /= 'FIELD'  .and.                          &
       trim(adjustL(har%testClass)) /= 'GRID'   .and.                          &
       trim(adjustL(har%testClass)) /= 'REGRID' ) then
     call ESMF_LogMsgSetError( ESMF_FAILURE,                                   &
          "class name not of valid type " // trim(adjustL(har%testClass)),     &
          rcToReturn=localrc)
     return
  endif

  !-----------------------------------------------------------------------------
  ! determine type of test report and toggle setup report
  !-----------------------------------------------------------------------------

  call ESMF_ConfigFindLabel(localcf,trim(adjustL(setup_report_name)),rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
          "cannot find config label " // trim(adjustL(setup_report_name)),     &
          rcToReturn=localrc) ) return

  call ESMF_ConfigGetAttribute(localcf, har%setupReportType, rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
          "cannot get value for label " // trim(adjustL(setup_report_name)),   &
          rcToReturn=localrc) ) return

  if ( har%setupReportType /= "TRUE" .and. har%setupReportType /= "FALSE" ) then
     call ESMF_LogMsgSetError( ESMF_FAILURE,                                   &
            "setup report flag improperly set " // trim(har%setupReportType),  &
            rcToReturn=localrc)
     return
  endif

  !-----------------------------------------------------------------------------
  ! read test report flag 
  ! test_report: FULL  - full report presenting both success and failure configs
  ! test_report: FAILURE - report only failure configurations
  ! test_report: SUCCESS - report only successful configurations
  ! test_report: NONE - no report
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf,trim(adjustL(test_report_name)),rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
           "cannot find config label " // trim(adjustL(test_report_name)),     &
           rcToReturn=localrc) ) return

  call ESMF_ConfigGetAttribute(localcf, har%reportType, rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
           "cannot get value for label " // trim(adjustL(test_report_name)),   &
           rcToReturn=localrc) ) return

  if ( har%reportType /= "FULL" .and. har%reportType /= "FAILURE" .and.        &
       har%reportType /= "SUCCESS" .and. har%reportType /= "NONE" ) then
     call ESMF_LogMsgSetError( ESMF_FAILURE,                                   &
               "report flag improperly set " // trim(har%reportType),          &
               rcToReturn=localrc)
     return
  endif

  !-----------------------------------------------------------------------------
  ! based on whether exhaustive or nonexhaustive tests are to be run,  find 
  ! and load the problem descriptor file names
  !-----------------------------------------------------------------------------
#ifdef ESMF_TESTEXHAUSTIVE
  ltag = 'exhaustive::'
  print *, "running exhaustive tests"
#else
  ltag = 'nonexhaustive::'
  print *, "running nonexhaustive tests"
#endif
  call ESMF_ConfigFindLabel(localcf, trim(adjustL(ltag)), rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
           "cannot find config label " // trim(adjustL(ltag)),                 &
           rcToReturn=localrc) ) return

  ! determine the number of entries
  call ESMF_ConfigGetDim(localcf, har%numRecords, ncolumns,                    &
           trim(adjustL(ltag)), rc=localrc)
  if( ESMF_LogMsgFoundError(localrc,                                           &
           "cannot find table size of " // trim(adjustL(ltag)),                &
           rcToReturn=localrc) ) return

  ! if there are no entries post an error
  if ( har%numRecords .le. 0 ) then
     call ESMF_LogMsgSetError( ESMF_FAILURE,                                   &
               "no problem descriptor files specified",                        &
               rcToReturn=localrc)
     return
  endif

  !-----------------------------------------------------------------------------
  ! find the problem descriptor file names and read them
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(adjustL(ltag)), rc=localrc)
  if( ESMF_LogMsgFoundError(localrc,                                           &
           "cannot find table label of " // trim(adjustL(ltag)),               &
           rcToReturn=localrc) ) return

  !-----------------------------------------------------------------------------
  ! allocate space to hold problem descriptor filenames and advance through the
  ! table extracting the problem descriptor filenames
  !-----------------------------------------------------------------------------
  allocate( har%rcrd(har%numRecords) )

  do kfile=1,har%numRecords
     ! advance to new line in table
     call ESMF_ConfigNextLine(localcf, flag, rc=localrc)
     if( ESMF_LogMsgFoundError(localrc,                                        &
              "cannot advance to next line of table " // trim(adjustL(ltag)),  &
              rcToReturn=localrc) ) return
 
     ! retrieve the problem descriptor filenames 
     call ESMF_ConfigGetAttribute(localcf, har%rcrd(kfile)%filename,           &
                                  rc=localrc)
     if( ESMF_LogMsgFoundError(localrc,                                        &
              "cannot get descriptor filename in " // trim(adjustL(ltag)),     &
              rcToReturn=localrc) ) return

  enddo   ! file

  !-----------------------------------------------------------------------------
  ! clean up CF
  !-----------------------------------------------------------------------------
  call ESMF_ConfigDestroy(localcf, localrc)
  if( ESMF_LogMsgFoundError(localrc,                                           &
           "cannot destroy config file " // trim(test_harness_name),           &
           rcToReturn=localrc) ) return

  ! if I've gotten this far without an error, then the routine has succeeded.
  localrc = ESMF_SUCCESS

!===============================================================================
  end subroutine read_testharness_config
!===============================================================================

!-------------------------------------------------------------------------------
 
!===============================================================================
! !IROUTINE: read_descriptor_files

! !INTERFACE:
  subroutine read_descriptor_files(localrc)
!
! !ARGUMENTS:
  integer, intent(  out) :: localrc

!
! !DESCRIPTION:
! This routine takes the problem descriptor file names specified in the top 
! level config file "test_harness.rc" and extracts from a config table the
! "problem descriptor string" and all the "problem specifier files." These
! helper files are divided into groups by flags preceeded by a dash. The
! "-c" flag (currently not implemented) indicates file(s) containing the CLASS 
! specific settings. The "-d" flag indicates the file(s) containing an ensemble
! of distribution configurations to be run with the specific "problem descriptor
! string." Likewise the "-g" flag indicates the file(s) containing an ensemble
! of grid configurations to be run with the specific "problem descriptor string."
! This routine only extracts the information from the configuration file,
! additional processing occurs in a later routine.
!
! Upon completion, the routine returns the values to a public record

!   har%rcrd(n)%numStrings      number of problem descriptor strings in 
!                               the n'th problem descriptor file.

!   har%rcrd(n)%str(k)%pds   k'th problem descriptor from the n'th
!                               problem descriptor file.
!
!   har%rcrd(n)%str(k)%nDfiles   number of distribution specifier files

!   har%rcrd(n)%str(k)%Dfiles(l)%filename   filename string for
!                                        the l'th distribution specifier file 
!                                        associated with the k'th problem descriptor
!                                        string, located in the n'th problem 
!                                        descriptor file.                  
!
!   har%rcrd(n)%str(k)%nGfiles        number of grid specifier files
!
!   har%rcrd(n)%str(k)%Gfile(l)%filename   filename string for
!                                        the l'th grid specifier file associated
!                                        with the k'th problem descriptor string
!                                        located in the n'th problem descriptor
!                                        file.                  
!===============================================================================

  ! local ESMF types
  type(ESMF_Config)      :: localcf

  ! local parameters
  character(ESMF_MAXSTR), parameter :: descriptor_label      &
                                               = "problem_descriptor_string::"

  ! local character types
  type (sized_char_array), allocatable :: ltmpstring(:), lstring(:)

  ! local character strings
  character(ESMF_MAXSTR) :: lfilename, ltmp
  character(ESMF_MAXSTR) :: lchar, lchar1, lchar2
  
  logical :: flag = .true.

! local integer variables
  integer :: n, nn, k, pos, kcol, ncount, npds, ntmp
  integer :: kfile, nfiles, kstr, pstring
  integer :: cpos, dpos, gpos, csize, dsize, gsize
  integer, allocatable :: kcount(:), ncolumns(:), nstrings(:)
  integer, allocatable :: pds_loc(:), pds_flag(:)

! local logical variable
  logical :: endflag = .false.
  logical :: cflag, dflag, gflag
  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! open each problem descriptor and extract the contents of the table
  ! containing the problem descriptor strings and the specifier filenames
  !-----------------------------------------------------------------------------
  nfiles = har%numRecords
  allocate( nstrings(nfiles) )

  do kfile=1,nfiles
    !---------------------------------------------------------------------------
    ! create a new config handle for reading problem descriptor strings
    !---------------------------------------------------------------------------
    localcf = ESMF_ConfigCreate(localrc)
    if( ESMF_LogMsgFoundError(localrc, "cannot create config object",          &
                         rcToReturn=localrc) ) return

    !---------------------------------------------------------------------------
    ! load file holding the problem descriptor strings
    !---------------------------------------------------------------------------
    lfilename = trim(adjustL(har%rcrd(kfile)%filename))

    call ESMF_ConfigLoadFile(localcf, trim(adjustL(lfilename)), rc=localrc )
    if( ESMF_LogMsgFoundError(localrc, "cannot load config file " //           &
            trim(adjustL(lfilename)), rcToReturn=localrc) ) return

    !---------------------------------------------------------------------------
    ! Search for the problem descriptor string table
    !---------------------------------------------------------------------------
    call ESMF_ConfigFindLabel(localcf, trim(adjustL(descriptor_label)),        &
             rc=localrc )
    if( ESMF_LogMsgFoundError(localrc, "cannot find config label " //          &
             trim(adjustL(descriptor_label)), rcToReturn=localrc) ) return

    !---------------------------------------------------------------------------
    ! determine the number of entries
    !---------------------------------------------------------------------------
    call ESMF_ConfigGetDim(localcf, nstrings(kfile), ntmp,                     &
             trim(adjustL(descriptor_label)), rc=localrc)
    if( ESMF_LogMsgFoundError(localrc, "cannot get descriptor table size in "  &
            // "file " // trim(adjustL(lfilename)), rcToReturn=localrc) ) return

    !---------------------------------------------------------------------------
    ! determine that the table has entries before preceeding
    !---------------------------------------------------------------------------
    if( nstrings(kfile) .le. 0 ) then
      call ESMF_LogMsgSetError( ESMF_FAILURE, "problem descriptor table empty" &
               // " in file " // trim(adjustL(lfilename)), rcToReturn=localrc)
      return
    endif

    !---------------------------------------------------------------------------
    ! extract column lengths of the table to determine the number of specifier files
    !---------------------------------------------------------------------------
    call ESMF_ConfigFindLabel(localcf, trim(adjustL(descriptor_label)),        &
           rc=localrc )
    if( ESMF_LogMsgFoundError(localrc, "cannot find config label" //           &
           trim(adjustL(descriptor_label)), rcToReturn=localrc) ) return

    allocate( ncolumns(nstrings(kfile)) )
    allocate ( ltmpstring(nstrings(kfile)) )

    do kstr=1,nstrings(kfile)
      call ESMF_ConfigNextLine(localcf, flag , rc=localrc)
      if( ESMF_LogMsgFoundError(localrc, "cannot advance to the next line of"  &
              //" the table "// trim(adjustL(descriptor_label)) // " in file " &
              // trim(adjustL(lfilename)), rcToReturn=localrc) ) return

      ncolumns(kstr) = ESMF_ConfigGetLen(localcf, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS .or. ncolumns(kstr) .lt. 1 ) then
        write(lchar,"(i5)")  kstr
        call ESMF_LogMsgSetError( ESMF_FAILURE, "problem reading line " //     &
                 trim(adjustL(lchar)) // " of table in file " //               &
                 trim(adjustL(lfilename)), rcToReturn=localrc)
        return
      endif

      !-------------------------------------------------------------------------
      ! allocate tempory storage so that the file needs to be read only once
      !-------------------------------------------------------------------------
      allocate ( ltmpstring(kstr)%tag( ncolumns(kstr) ) )
      ltmpstring(kstr)%tagsize = ncolumns(kstr)
    enddo    ! end string

    !---------------------------------------------------------------------------
    ! Starting again at the top of the table, extract the table contents into
    ! a local character array structure for later processing
    !---------------------------------------------------------------------------
    call ESMF_ConfigFindLabel(localcf, trim(adjustL(descriptor_label)),        &
             rc=localrc )
    if( ESMF_LogMsgFoundError(localrc,                                         &
            "cannot find config label " // trim(adjustL(descriptor_label)),    &
            rcToReturn=localrc) ) return

    do kstr=1,nstrings(kfile)
    !---------------------------------------------------------------------------
    ! copy the table into a character array
    !---------------------------------------------------------------------------
      call ESMF_ConfigNextLine(localcf, flag , rc=localrc)
      if( ESMF_LogMsgFoundError(localrc, "cannot advance to the next line " // &
              "of table " // trim(adjustL(descriptor_label)) // " in file " // &
              trim(adjustL(lfilename)), rcToReturn=localrc) ) return

      do kcol=1, ncolumns(kstr)
        call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
        write(lchar,"(i5)") kstr
        if( ESMF_LogMsgFoundError(localrc, "cannot get table entry from line " &
                // trim(adjustl(lchar)) //  " column " // char(kcol)  //       &
                "of file " // trim(adjustL(lfilename)),                        &
                rcToReturn=localrc) ) return
         ltmpstring(kstr)%tag(kcol)%string = trim( ltmp )
      enddo     ! end col
    enddo       ! end string

    !---------------------------------------------------------------------------
    ! count the number of actual problem descriptor strings & continuation lines
    !---------------------------------------------------------------------------
    ncount = 0
    npds = 0
    allocate( pds_flag(nstrings(kfile)) )
    do kstr=1,nstrings(kfile)
       if( trim(adjustL(ltmpstring(kstr)%tag(1)%string)) /= "&") then
         pds_flag(kstr) = 1         
         npds = npds + 1
       else
         ncount = ncount + 1
         pds_flag(kstr) = 0         
       endif
    enddo     ! end string
    ! sanity check
    if( (npds + ncount) /= nstrings(kfile) ) then
      write(lchar,"(i5)")  nstrings(kfile)
      write(lchar1,"(i5)")  npds
      write(lchar2,"(i5)")  ncount
      call ESMF_LogMsgSetError( ESMF_FAILURE, "number of rows " //             &
             trim(adjustl(lchar)) // " in the table"  //                       &
             " does not match the sum of strings " // trim(adjustl(lchar1))    &
             // " and continuation lines " //  trim(adjustl(lchar2)) //        &
             " of file " // trim(adjustL(lfilename)), rcToReturn=localrc) 
    endif

    har%rcrd(kfile)%numStrings = npds


    !---------------------------------------------------------------------------
    ! debug statements
    !---------------------------------------------------------------------------
    if(debugflag) then
       print*,'                                                    '
       print*,'=------begin-read_descriptor_files----------------='
       print*,'Opening ',trim( lfilename )
       write(lchar,"(i3)")  nstrings(kfile)
       print*,'  file ',trim(lfilename),' descriptor table has ',              &
            trim(adjustl(lchar)),' rows, but only ',npds,' strings'         
       print*,'=------end-read_descriptor_files------------------='
       print*,'                                                    '
    endif

    !---------------------------------------------------------------------------
    ! save the addresses of the non-continuation lines
    !---------------------------------------------------------------------------
    k = 0
    allocate( pds_loc(npds) )
    do kstr=1,nstrings(kfile)
       if( pds_flag(kstr) == 1 ) then
         k = k + 1
         pds_loc(k) =  kstr        
       endif
    enddo     ! end string
    ! sanity check
    if( npds .ne. k ) then 
      write(lchar,"(i5)")  nstrings(kfile)
      write(lchar1,"(i5)")  npds
      write(lchar2,"(i5)")  ncount
      call ESMF_LogMsgSetError( ESMF_FAILURE, "number of rows " //             &
             trim(adjustl(lchar)) // " in the table" //                        &
             " does not match the sum of strings "//trim(adjustl(lchar1))      &
             // " and continuation lines " // trim(adjustl(lchar2)) //         &
             " of file " // trim(adjustL(lfilename)), rcToReturn=localrc)
    endif

    !---------------------------------------------------------------------------
    ! to simplify the later search algorithm, reshape the input table from a 
    ! series of lines with a PDS plus optional continuations lines, to a single
    ! line with everything on it. Count the total number of elements on both 
    ! type of lines to that we can allocate enough memory to store the whole 
    ! specification.
    !---------------------------------------------------------------------------
    allocate( kcount(npds) )
    do k=1,npds
      if( trim( ltmpstring( pds_loc(k) )%tag(1)%string ) == "&") then
        write(lchar,"(i5)")   pds_loc(k)
        call ESMF_LogMsgSetError( ESMF_FAILURE,                                &
                 "no problem descriptor string on line " //                    &
                 trim(adjustl(lchar)) // " of file " //                        &
                 trim(adjustL(lfilename)),rcToReturn=localrc)
      else    ! at new PDS
        kcount(k) = ncolumns(pds_loc(k))
        pstring =  pds_loc(k)
 21     continue
        !-----------------------------------------------------------------------
        ! if not end of table, look for additional continuation lines 
        !-----------------------------------------------------------------------
        if(pstring < nstrings(kfile)) then
          pstring =  pstring + 1
          !---------------------------------------------------------------------
          ! if find a continuation line add additional elements (minus the
          ! continuation symbol "&")
          !---------------------------------------------------------------------
          if( trim( ltmpstring(pstring)%tag(1)%string ) == "&" ) then 
            kcount(k) = kcount(k) + ncolumns(pstring) -1
            goto 21
          endif
        endif

      endif
    enddo     ! k
    !---------------------------------------------------------------------------
    ! create reshaped workspace to hold the problem descriptor table contents
    !---------------------------------------------------------------------------
    allocate ( lstring(npds) )    
    do k=1, npds
      allocate ( lstring(k)%tag(kcount(k)) )    
      do n=1,ncolumns(pds_loc(k))
        lstring(k)%tag(n)%string = trim( ltmpstring(pds_loc(k))%tag(n)%string )
      enddo     ! n
        
      pstring =  pds_loc(k)
      nn = ncolumns(pds_loc(k))+1
 22   continue

      !-------------------------------------------------------------------------
      ! if not end of table, look for additional continuation lines
      !-------------------------------------------------------------------------
      if(pstring < nstrings(kfile)) then
        pstring =  pstring + 1

        !-----------------------------------------------------------------------
        ! if find a continuation line, and add to the line length (minus the 
        ! continuation symbol)
        !-----------------------------------------------------------------------
        if( trim( ltmpstring(pstring)%tag(1)%string ) == "&" ) then
          do n=2,ncolumns(pstring)
            lstring(k)%tag(nn)%string = trim(ltmpstring(pstring)%tag(n)%string )
            nn = nn + 1
          enddo     ! n
          goto 22
        endif
      endif
    enddo     ! k

    !---------------------------------------------------------------------------
    ! mine the table entries for the problem descriptor strings
    !---------------------------------------------------------------------------
    allocate( har%rcrd(kfile)%str(npds) )
    do k=1,npds
       har%rcrd(kfile)%str(k)%pds = trim( lstring(k)%tag(1)%string )
    enddo     ! k

    !---------------------------------------------------------------------------
    ! mine the table entries for the names of the specifier files
    !---------------------------------------------------------------------------
    do k=1,npds
      pos = 2
      endflag = .true.
      ! drs debug
      cflag = .true.  ! set to true so that it doesn't look for a "-c" argument
      ! drs debug
      dflag = .false.
      gflag = .false.
      !-------------------------------------------------------------------------
      ! loop through the specifiers for each of the problem desriptor strings
      !-------------------------------------------------------------------------
      do while(endflag)
      ltmp = trim( lstring(k)%tag(pos)%string )

        select case ( trim(adjustL(ltmp)) )

         !----------------------------------------------------------------------
         ! class descriptor file
         !----------------------------------------------------------------------
         case('-c')
         if( cflag ) then
           write(lchar,"(i5)") k
           call ESMF_LogMsgSetError( ESMF_FAILURE, "the -c specifier flag"  &
                    // " is used more than once on the " //                 &
                   trim(adjustl(lchar))//"th string of the problem " //     &
                   "descriptor table in file" // trim(lfilename),           &
                   rcToReturn=localrc)
           return
         endif
         ! starting position
         cpos = pos
 11      continue
         ! if not at the end of the row, then check next element
         if( pos < kcount(k) ) then
           pos = pos + 1
           ltmp =  trim(adjustL( lstring(k)%tag(pos)%string ))
           ! if not a flag, repeat until a flag
           if( ltmp(1:1) /= '-' ) goto 11
           csize = pos-1-cpos
           endflag = .true.
         else  ! at end of row
           csize = pos-cpos
           endflag = .false.
         endif

         allocate( har%rcrd(kfile)%str(k)%classfile%tag(csize) )
         har%rcrd(kfile)%str(k)%classfile%tagsize = csize
         do n=1,csize
           har%rcrd(kfile)%str(k)%classfile%tag(n)%string =                 &
                                 trim(adjustL( lstring(k)%tag(cpos+n)%string ))
         enddo      ! n
         cflag = .true.

         !----------------------------------------------------------------------
         ! distribution descriptor file
         !----------------------------------------------------------------------
         case('-d')
         if( dflag ) then
           write(lchar,"(i5)") k
           call ESMF_LogMsgSetError( ESMF_FAILURE, "the -d specifier flag"     &
                    // " is used more than once on the " //                    &
                   trim(adjustl(lchar))//"th string of the problem " //        &
                   "descriptor table in file" // trim(adjustL(lfilename)),     &
                   rcToReturn=localrc)
           return
         endif
         ! starting position
         dpos = pos

 12      continue
         ! if not at the end of the row, then check next element
         if( pos < kcount(k) ) then
           pos = pos + 1
           ltmp =  trim(adjustL( lstring(k)%tag(pos)%string ))
           ! if not a flag, repeat until a flag
           if( ltmp(1:1) /= '-' ) goto 12
           dsize = pos-1-dpos
           endflag =.true. 
         else  ! at end of row
           dsize = pos-dpos
           endflag =.false. 
         endif

         allocate( har%rcrd(kfile)%str(k)%Dfiles(dsize) )
         har%rcrd(kfile)%str(k)%nDfiles = dsize
         do n=1,dsize
           har%rcrd(kfile)%str(k)%Dfiles(n)%filename =                      &
                               trim(adjustL( lstring(k)%tag(dpos+n)%string ))
         enddo      ! n
         dflag = .true.

         !----------------------------------------------------------------------
         ! grid descriptor file
         !----------------------------------------------------------------------
         case('-g')
         if( gflag ) then
           write(lchar,"(i5)") k
           call ESMF_LogMsgSetError( ESMF_FAILURE, "the -g specifier flag" //  &
                    " is used more than once on the " // trim(adjustl(lchar))  &
                    //"th string of the problem descriptor table in file " //  &
                    trim(adjustL(lfilename)), rcToReturn=localrc)
           return
         endif
         ! starting position
         gpos = pos

 13      continue
         ! if not at the end of the row, then check next element
         if( pos < kcount(k) ) then
           pos = pos + 1
           ltmp =  trim(adjustL( lstring(k)%tag(pos)%string ))
           ! if not a flag, repeat until a flag
           if( ltmp(1:1) /= '-' ) goto 13
           gsize = pos-1-gpos
           endflag = .true.
         else  ! at end of row
           gsize = pos-gpos
           endflag = .false.
         endif  

         allocate( har%rcrd(kfile)%str(k)%Gfiles(gsize) )
         har%rcrd(kfile)%str(k)%nGfiles = gsize
         do n=1,gsize
           har%rcrd(kfile)%str(k)%Gfiles(n)%filename =                      &
                             trim(adjustL(lstring(k)%tag(gpos+n)%string))
         enddo     ! n
         gflag = .true.

         !----------------------------------------------------------------------
         ! syntax error - entry after pds should be a flag
         !----------------------------------------------------------------------
         case default
         write(lchar,"(i5)")  pds_loc(k)
         call ESMF_LogMsgSetError( ESMF_FAILURE,                               &
               "no specifier flag on line " // trim(adjustl(lchar)) //         &
               " of file " //trim(lfilename), rcToReturn=localrc)

        end select  ! specifier flag type
      end do  ! while
    enddo      ! k

    !---------------------------------------------------------------------------
    ! finish cleaning up workspace before opening new file
    !---------------------------------------------------------------------------
    deallocate( ncolumns, kcount )
    deallocate( ltmpstring, lstring )
    deallocate( pds_loc, pds_flag )

    !---------------------------------------------------------------------------
    ! clean up CF
    !---------------------------------------------------------------------------
    call ESMF_ConfigDestroy(localcf, localrc)
    if( ESMF_LogMsgFoundError(localrc, "cannot destroy config file "  //       &
            trim(adjustL(lfilename)),  rcToReturn=localrc) ) return
  enddo  ! file

  !-----------------------------------------------------------------------------
  ! final deallocation
  !-----------------------------------------------------------------------------
  deallocate( nstrings )

  !-----------------------------------------------------------------------------
  ! if I've gotten this far without an error, then the routine has succeeded.
  !-----------------------------------------------------------------------------
  localrc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine read_descriptor_files
  !-----------------------------------------------------------------------------


!===============================================================================
! !IROUTINE: parse_descriptors

! !INTERFACE:
  subroutine parse_descriptors(localrc)
!
! !ARGUMENTS:
  integer, intent(  out) :: localrc

!
! !DESCRIPTION:
! This routine parses a problem descriptor string for:
!	operation - redistribution or regrid plus method
!	rank of memory
!	rank of distribution
!	rank of grid association
!
!===============================================================================
! dummy type - ignore - decalred to work around compiler bug. 
! type(character_array), allocatable :: Ttype(:)
  ! local character strings
  character(ESMF_MAXSTR) :: lstring, lname, lchar

  logical :: flag = .true.

  ! local integer variables
  integer :: nPEs
  integer :: k, kfile, kstr
  integer :: irank, igrid
  integer :: iDfile, iGfile
  integer :: nDfiles, nGfiles, nstatus

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! parse each problem descriptor string in each og the nfiles files
  !-----------------------------------------------------------------------------
  do kfile=1,har%numRecords
     do kstr=1,har%rcrd(kfile)%numStrings
        call parse_descriptor_string(har%rcrd(kfile)%numStrings,               &
                  har%rcrd(kfile)%str(kstr), localrc)

        if (ESMF_LogMsgFoundError(localrc," error in problem descriptor file " &
                // trim(adjustL(har%rcrd(kfile)%filename)), rcToReturn=rc))    &
                return
        ! read distribution specifier files
        do k=1,har%rcrd(kfile)%str(kstr)%nDfiles
           nPEs = 12
           ! drs debug nPEs = petCount
           call read_dist_specification(nPEs,                                  &
                                        har%rcrd(kfile)%str(kstr)%Dfiles(k),   &
                                        har%rcrd(kfile)%str(kstr)%DstMem,      &
                                        har%rcrd(kfile)%str(kstr)%SrcMem,      &
                                        localrc)
           if (ESMF_LogMsgFoundError(localrc," error reading dist specifier"   &
                   // " file "  //                                             &
                  trim(adjustL(har%rcrd(kfile)%str(kstr)%Dfiles(k)%filename)), &
                   rcToReturn=rc)) return
        enddo   ! k

        ! read grid specifier files
        do k=1,har%rcrd(kfile)%str(kstr)%nGfiles
           call read_grid_specification(har%rcrd(kfile)%str(kstr)%Gfiles(k),   &
                    localrc)
           if (ESMF_LogMsgFoundError(localrc," error reading grid specifier"   &
                   // " file "  //                                             &
                  trim(adjustL(har%rcrd(kfile)%str(kstr)%Gfiles(k)%filename)), &
                   rcToReturn=rc)) return
        enddo   ! k

        ! allocate and initialize test status
        nDfiles = har%rcrd(kfile)%str(kstr)%nDfiles
        nGfiles = har%rcrd(kfile)%str(kstr)%nGfiles
        allocate( har%rcrd(kfile)%str(kstr)%test_status(nDfiles,nGfiles) )

        ! initialize test result to UNDEFINED
        do iDfile=1,har%rcrd(kfile)%str(kstr)%nDfiles
        do iGfile=1,har%rcrd(kfile)%str(kstr)%nGfiles
           har%rcrd(kfile)%str(kstr)%test_status(iDfile,iGfile) =              &
                                                          HarnessTest_UNDEFINED 
        enddo   ! iGfile
        enddo   ! iDfile

     enddo  ! kstr
  enddo    ! kfile

  !-----------------------------------------------------------------------------
  ! list imported problem configurations before continuing
  !-----------------------------------------------------------------------------
  if( localPet == rootPet .and. trim(har%setupReportType) == "TRUE" ) then
     nstatus = -1
     do kfile=1,har%numrecords
        do kstr=1,har%rcrd(kfile)%numStrings
           call report_descriptor_string(har%rcrd(kfile)%str(kstr),nstatus,    &  
                                         localrc)
        enddo  ! kstr
     enddo    ! kfile
  endif

  !-----------------------------------------------------------------------------
  ! if I've gotten this far without an error, then the routine has succeeded.
  !-----------------------------------------------------------------------------
  localrc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine parse_descriptors
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! !IROUTINE: TestHarnessReport

! !INTERFACE:
  subroutine TestHarnessReport(localrc)
!
! !ARGUMENTS:
  integer, intent(  out) :: localrc

!
! !DESCRIPTION:
! This routine provides an additional reporting capability of the test results
! in a more human readable form.
!-------------------------------------------------------------------------------
! Action associated to test report flag value.
!-------------------------------------------------------------------------------
  ! local variables
  integer :: kfile, kstr, nstatus

  ! initialize local rc
  localrc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! select type of test result report
  !-----------------------------------------------------------------------------
  select case (trim(adjustL(har%reportType)))
     case("FULL")
     ! report both successes and failures
     if( localPet == rootPet ) then
        print*,"                                      "
        print*,">----------FINAL TEST REPORT---------<"
        nstatus = -1
        do kfile=1,har%numRecords
           print*,' Problem descriptor File:',  &
                   trim(adjustL(har%rcrd(kfile)%filename))
           print*,"                                      "
           do kstr=1,har%rcrd(kfile)%numStrings
              call report_descriptor_string(har%rcrd(kfile)%str(kstr),nstatus, &
                                            localrc)
           enddo  ! kstr
           print*,"                                      "
        enddo    ! kfile
     endif

     case("FAILURE")
     ! only report failures
     if( localPet == rootPet ) then
        print*,"                                      "
        print*,">----------FINAL TEST REPORT---------<"
        nstatus = 0
        do kfile=1,har%numRecords
           print*,' Problem descriptor File:',  &
                   trim(adjustL(har%rcrd(kfile)%filename))
           print*,"                                      "
           do kstr=1,har%rcrd(kfile)%numStrings
              call report_descriptor_string(har%rcrd(kfile)%str(kstr),nstatus, &
                                            localrc)
           enddo  ! kstr
           print*,"                                      "
        enddo    ! kfile
     endif

     case("SUCCESS")
        ! only report success
     if( localPet == rootPet ) then
        print*,"                                      "
        print*,">----------FINAL TEST REPORT---------<"
        nstatus = 1
        do kfile=1,har%numRecords
           print*,' Problem descriptor File:',  &
                   trim(adjustL(har%rcrd(kfile)%filename))
           print*,"                                      "
           do kstr=1,har%rcrd(kfile)%numStrings
              call report_descriptor_string(har%rcrd(kfile)%str(kstr),nstatus, &
                                            localrc)
           enddo  ! kstr
           print*,"                                      "
        enddo    ! kfile
     endif

     case("NONE")
        ! no report
  
     case default
     ! flag error
     print*,"error, report flag improperly set"
     call ESMF_LogMsgSetError( ESMF_FAILURE," report flag improperly set",     &
               rcToReturn=localrc)

  end select  ! case of report flag

  !-----------------------------------------------------------------------------
  ! if I've gotten this far without an error, then the routine has succeeded.
  !-----------------------------------------------------------------------------
  localrc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine TestHarnessReport
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------
end program ESMF_Test_Harness
!-------------------------------------------------------------------------------


