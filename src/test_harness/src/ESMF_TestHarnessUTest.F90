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
  implicit none

  ! cumulative result: count failures; no failure equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc, finalrc

  ! local args needed to create/construct objects
  type(ESMF_VM)          :: vm

  ! global storage of test specification
  type (harness_descriptor) :: harness

  ! local variables
  integer :: localPet, petCount
  integer :: test_report_flag
  integer :: problem_descriptor_count
  integer :: ncount
  integer :: k, n, kfile

  logical :: debugflag = .true.

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
     goto 999
  endif
  !-----------------------------------------------------------------------------
  ! debugging statements
  !-----------------------------------------------------------------------------
  if(debugflag) then
     print*,'=----------in main-------------------------------='
     print*,'  Read test harness config debug statements '
     print*,'Problem descriptor Test Class:',harness%testClass
     print*,'Total number of problem descriptor files:',harness%numRecords
     do kfile=1,harness%numRecords
        print*,kfile,' Descriptor file name:',trim(harness%Record(kfile)%filename)
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
     goto 999
  endif
  !-----------------------------------------------------------------------------
  ! debugging statements
  !-----------------------------------------------------------------------------
  if(debugflag) then
     print*,'=---------in main--------------------------------='
     print*,'  Read descriptor file debug statements '
     do kfile=1,harness%numRecords
        print*,'Problem descriptor filename: ',trim(harness%Record(kfile)%filename)
        do k=1,harness%Record(kfile)%numStrings
           print*,'>',trim( harness%Record(kfile)%string(k)%pds )
           print*,'Number of Distribution Specifier files', &
                                 harness%Record(kfile)%string(k)%distfiles%tagsize
           do n=1,harness%Record(kfile)%string(k)%distfiles%tagsize
              print*,'   ',n,   &
                 trim( harness%Record(kfile)%string(k)%distfiles%tag(n)%string )
           enddo     ! n
           print*,'Number of Grid Specifier files',   &
                                 harness%Record(kfile)%string(k)%gridfiles%tagsize
           do n=1,harness%Record(kfile)%string(k)%gridfiles%tagsize
              print*,'   ',n,   &
                 trim( harness%Record(kfile)%string(k)%gridfiles%tag(n)%string )
              print*,'                                            '
           enddo     ! n
        enddo      ! k
     enddo      ! kfile
     print*,'=------------------------------------------------='
     goto 999
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
  print*,'entering parse descriptor string'
  call parse_descriptor_string(rc)
  print*,'exiting parse descriptor string'
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
  print*,'Read specification - number of records ', harness%numRecords

  do kfile=1, harness%numRecords
  print*,' number of problem descriptor strings ',harness%Record(kfile)%numStrings
     do k=1, harness%Record(kfile)%numStrings
  print*,' number of grid files',harness%Record(kfile)%string(k)%gridfiles%tagsize
        do n=1,harness%Record(kfile)%string(k)%gridfiles%tagsize
           print*,'entering read grid specification'
           call read_grid_specification(                                       &
                  harness%Record(kfile)%string(k)%gridfiles%tag(n)%string,rc)
           print*,'exiting read grid specification'
           if (rc .ne. ESMF_SUCCESS) then 
               print*,' read grid spec failed',kfile,k,n
               print*,harness%Record(kfile)%string(k)%gridfiles%tag(n)%string
               finalrc = ESMF_FAILURE
               call ESMF_Finalize(terminationflag=ESMF_ABORT)
           endif

        enddo
     enddo
  enddo
  ! report results
! call TestHarnessReport(test_report_flag,rc)

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
!       harness%testClass               Problem Descriptor Test Class
!       harness%reportType              Output Report type
!       harness%numRecords              number of problem descriptor filenames
!       harnessRecord(k)%filename       kth problem descriptor filename
!===============================================================================

  ! local ESMF types
  type(ESMF_Config)      :: localcf

  ! local parameters
  character(ESMF_MAXSTR), parameter :: test_class_name  = "test_class:"
  character(ESMF_MAXSTR), parameter :: test_report_name = "test_report:"


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

  call ESMF_ConfigGetAttribute(localcf, harness%testClass, rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
           "cannot get value for label " // trim(adjustL(test_class_name)),    &
           rcToReturn=localrc) ) return

  !-----------------------------------------------------------------------------
  ! if the class is not supported, then post an error
  !-----------------------------------------------------------------------------
  if ( trim(adjustL(harness%testClass)) /= 'ARRAY'  .and.                      &
       trim(adjustL(harness%testClass)) /= 'BUNDLE' .and.                      &
       trim(adjustL(harness%testClass)) /= 'FIELD'  .and.                      &
       trim(adjustL(harness%testClass)) /= 'GRID'   .and.                      &
       trim(adjustL(harness%testClass)) /= 'REGRID' ) then
     call ESMF_LogMsgSetError( ESMF_FAILURE,                                   &
          "class name not of valid type " // trim(adjustL(harness%testClass)), &
          rcToReturn=localrc)
     return
  endif

  !-----------------------------------------------------------------------------
  ! determine type of test report to be requested
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf,trim(adjustL(test_report_name)),rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
           "cannot find config label " // trim(adjustL(test_report_name)),     &
           rcToReturn=localrc) ) return

  !-----------------------------------------------------------------------------
  ! read test report flag 
  !	0 = no output
  !	1 = report successful tests
  !    -1 = report test failures
  !	2 = report both successes and failures.
  !-----------------------------------------------------------------------------
  call ESMF_ConfigGetAttribute(localcf, harness%reportType, rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
           "cannot get value for label " // trim(adjustL(test_report_name)),   &
           rcToReturn=localrc) ) return

  if ( harness%reportType .lt.-2 .or. harness%reportType .gt.2) then
     call ESMF_LogMsgSetError( ESMF_FAILURE,                                   &
               "report flag improperly set " // char(harness%reportType),      &
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
  call ESMF_ConfigGetDim(localcf, harness%numRecords, ncolumns,                &
           trim(adjustL(ltag)), rc=localrc)
  if( ESMF_LogMsgFoundError(localrc,                                           &
           "cannot find table size of " // trim(adjustL(ltag)),                &
           rcToReturn=localrc) ) return

  ! if there are no entries post an error
  if ( harness%numRecords .le. 0 ) then
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
  allocate( harness%Record(harness%numRecords) )

  do kfile=1,harness%numRecords
     ! advance to new line in table
     call ESMF_ConfigNextLine(localcf, flag, rc=localrc)
     if( ESMF_LogMsgFoundError(localrc,                                        &
              "cannot advance to next line of table " // trim(adjustL(ltag)),  &
              rcToReturn=localrc) ) return
 
     ! retrieve the problem descriptor filenames 
     call ESMF_ConfigGetAttribute(localcf, harness%Record(kfile)%filename,     &
                                  rc=localrc)
     if( ESMF_LogMsgFoundError(localrc,                                        &
              "cannot get descriptor filename in " // trim(adjustL(ltag)),     &
              rcToReturn=localrc) ) return

  enddo   ! file

  !-----------------------------------------------------------------------------
  ! clean up CF
  !-----------------------------------------------------------------------------
  call ESMF_ConfigDestroy(localcf, localrc)
  if( ESMF_LogMsgFoundError(localrc,                                        &
           "cannot destroy config file " // trim(test_harness_name),        &
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

!   harnessRecord(n)%numStrings          number of problem descriptor strings in 
!                                        the n'th problem descriptor file.

!   harnessRecord(n)%string(k)%pds       k'th problem descriptor from the n'th
!                                        problem descriptor file.
!
!   harnessRecord(n)%string(k)%distfiles%tagsize         number of distribution
!                                                        specifier files
!   harnessRecord(n)%string(k)%distfiles%tag(l)%string   filename string for
!                                        the l'th distribution specifier file 
!                                        associated with the k'th problem descriptor
!                                        string, located in the n'th problem 
!                                        descriptor file.                  
!
!   harnessRecord(n)%string(k)%gridfiles%tagsize         number of grid
!                                                        specifier files

!   harnessRecord(n)%string(k)%gridfiles%tag(l)%string   filename string for
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
  integer :: kfile, nfiles, kstring, pstring
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
  nfiles = harness%numRecords
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
    lfilename = trim(adjustL(harness%Record(kfile)%filename))

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

    do kstring=1,nstrings(kfile)
      call ESMF_ConfigNextLine(localcf, flag , rc=localrc)
      if( ESMF_LogMsgFoundError(localrc, "cannot advance to the next line of"  &
              //" the table "// trim(adjustL(descriptor_label)) // " in file " &
              // trim(adjustL(lfilename)), rcToReturn=localrc) ) return

      ncolumns(kstring) = ESMF_ConfigGetLen(localcf, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS .or. ncolumns(kstring) .lt. 1 ) then
        write(lchar,"(i5)")  kstring
        call ESMF_LogMsgSetError( ESMF_FAILURE, "problem reading line " //     &
                 trim(adjustL(lchar)) // " of table in file " //               &
                 trim(adjustL(lfilename)), rcToReturn=localrc)
        return
      endif

      !-------------------------------------------------------------------------
      ! allocate tempory storage so that the file needs to be read only once
      !-------------------------------------------------------------------------
      allocate ( ltmpstring(kstring)%tag( ncolumns(kstring) ) )
      ltmpstring(kstring)%tagsize = ncolumns(kstring)
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

    do kstring=1,nstrings(kfile)
    !---------------------------------------------------------------------------
    ! copy the table into a character array
    !---------------------------------------------------------------------------
      call ESMF_ConfigNextLine(localcf, flag , rc=localrc)
      if( ESMF_LogMsgFoundError(localrc, "cannot advance to the next line " // &
              "of table " // trim(adjustL(descriptor_label)) // " in file " // &
              trim(adjustL(lfilename)), rcToReturn=localrc) ) return

      do kcol=1, ncolumns(kstring)
        call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
        write(lchar,"(i5)") kstring
        if( ESMF_LogMsgFoundError(localrc, "cannot get table entry from line " &
                // trim(adjustl(lchar)) //  " column " // char(kcol)  //       &
                "of file " // trim(adjustL(lfilename)),                        &
                rcToReturn=localrc) ) return
         ltmpstring(kstring)%tag(kcol)%string = trim( ltmp )
      enddo     ! end col
    enddo       ! end string

    !---------------------------------------------------------------------------
    ! count the number of actual problem descriptor strings & continuation lines
    !---------------------------------------------------------------------------
    ncount = 0
    npds = 0
    allocate( pds_flag(nstrings(kfile)) )
    do kstring=1,nstrings(kfile)
       if( trim(adjustL(ltmpstring(kstring)%tag(1)%string)) /= "&") then
         pds_flag(kstring) = 1         
         npds = npds + 1
       else
         ncount = ncount + 1
         pds_flag(kstring) = 0         
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

    harness%Record(kfile)%numStrings = npds


    !---------------------------------------------------------------------------
    ! debug statements
    !---------------------------------------------------------------------------
    if(debugflag) then
       print*,'                                                    '
       print*,'=------begin-read_descriptor_files----------------='
       print*,'Opening ',trim( lfilename )
       write(lchar,"(i3)")  nstrings(kfile)
       print*,'  file ',trim(lfilename),' descriptor table has ',                &
            trim(adjustl(lchar)),' rows, but only ',npds,' strings'         
       print*,'=------end-read_descriptor_files------------------='
       print*,'                                                    '
    endif

    !---------------------------------------------------------------------------
    ! save the addresses of the non-continuation lines
    !---------------------------------------------------------------------------
    k = 0
    allocate( pds_loc(npds) )
    do kstring=1,nstrings(kfile)
       if( pds_flag(kstring) == 1 ) then
         k = k + 1
         pds_loc(k) =  kstring        
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
    allocate( harness%Record(kfile)%string(npds) )
    do k=1,npds
       harness%Record(kfile)%string(k)%pds = trim( lstring(k)%tag(1)%string )
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

         allocate( harness%Record(kfile)%string(k)%classfile%tag(csize) )
         harness%Record(kfile)%string(k)%classfile%tagsize = csize
         do n=1,csize
           harness%Record(kfile)%string(k)%classfile%tag(n)%string =            &
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

         allocate( harness%Record(kfile)%string(k)%distfiles%tag(dsize) )
         harness%Record(kfile)%string(k)%distfiles%tagsize = dsize
         do n=1,dsize
           harness%Record(kfile)%string(k)%distfiles%tag(n)%string =            &
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

         allocate( harness%Record(kfile)%string(k)%gridfiles%tag(gsize) )
         harness%Record(kfile)%string(k)%gridfiles%tagsize = gsize
         do n=1,gsize
           harness%Record(kfile)%string(k)%gridfiles%tag(n)%string =           &
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
! !IROUTINE: parse_descriptor_string

! !INTERFACE:
  subroutine parse_descriptor_string(localrc)
!
! !ARGUMENTS:
  integer, intent(  out) :: localrc

!
! !DESCRIPTION:
! This routine parses a problem descriptor string for 
!	operation - redistribution or regrid plus method
!	rank of memory
!	rank of distribution
!	rank of grid association
!
!-------------------------------------------------------------------------------

  ! local character types

  ! local character strings
  character(ESMF_MAXSTR) :: lstring, lname
  character(ESMF_MAXSTR) :: src_string, dst_string
  type(character_array), allocatable :: lsrc(:), ldst(:)
  type(character_array), allocatable :: src_grid_type(:), src_dist_type(:)
  type(character_array), allocatable :: dst_grid_type(:), dst_dist_type(:)

  logical :: flag = .true.

  ! local integer variables
  integer :: k, kfile, nfiles,string
  integer, allocatable :: nstrings(:)
  integer :: tag, location(2)
  integer :: dst_beg, dst_end, src_beg, src_end
  integer :: srcMulti, dstMulti
  integer :: srcBlock,dstBlock
  integer :: src_mem_rank, dst_mem_rank
  integer :: src_grid_rank, dst_grid_rank
  integer :: src_dist_rank, dst_dist_rank

  integer, allocatable :: src_grid_order(:), src_dist_order(:)
  integer, allocatable :: src_grid_HaloL(:), src_grid_HaloR(:)
  integer, allocatable :: src_grid_StaggerLoc(:)

  integer, allocatable :: dst_grid_order(:), dst_dist_order(:)
  integer, allocatable :: dst_grid_HaloL(:), dst_grid_HaloR(:)
  integer, allocatable :: dst_grid_StaggerLoc(:)


  ! local logical variable
  logical :: endflag = .false.

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! parse each problem descriptor string
  !-----------------------------------------------------------------------------
  nfiles = harness%numRecords
  print*,'     PARSE: numRecords',nfiles
  allocate( nstrings(nfiles) )

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  do kfile=1,nfiles
  print*,'     PARSE: nstrings', nstrings(kfile)
     do string=1,nstrings(kfile)
        lstring = trim( harness%Record(kfile)%string(string)%pds )
  !-----------------------------------------------------------------------------
  ! find the test operation (address of strings)
  ! 1. search for "->" or "=>" - tip of symbol provides ending address
  ! 2. back up until reach white space - provides begining address
  ! 3. src_beg = 1, src_end = operation_beg-1
  !    dst_beg = operation_end+1, dst_end = length(trim(string)
  !-----------------------------------------------------------------------------
        call process_query(lstring, lname, tag, location, localrc)  
        harness%Record(kfile)%string(string)%process%string = lname
        harness%Record(kfile)%string(string)%process%tag = tag

        call memory_topology(lstring, location, srcMulti, srcBlock,            &
                             dstMulti, dstBlock, localrc)

        ! multiple block memory structure
        if( (srcMulti >= 1).or.(dstMulti >= 1) ) then
           ! TODO break into multiple single block strings
           ! and parse each string separately

        ! single block memory structure
        elseif( (srcMulti == 0).and.(dstMulti == 0) ) then
           if( (srcBlock==1).and.(dstBlock==1) ) then
              src_beg = 1
              src_end = location(1)
              dst_beg = location(2)
              dst_end = len( trim(lstring) )
              !-----------------------------------------------------------------
              ! separate string into source and destination strings
              !-----------------------------------------------------------------
              src_string = adjustL( lstring(src_beg:src_end) )
              dst_string = adjustL( lstring(dst_beg:dst_end) )

              !-----------------------------------------------------------------
              ! check that the source and destination ranks are not empty and
              ! the sizes agree
              !-----------------------------------------------------------------
              src_mem_rank = pattern_query(src_string,';') + 1
              dst_mem_rank = pattern_query(dst_string,';') + 1

              if( (src_mem_rank == 0).or.(dst_mem_rank == 0).or.               &
                  (src_mem_rank /= dst_mem_rank) )  then
                  localrc = ESMF_FAILURE
                  call ESMF_LogMsgSetError(ESMF_FAILURE,                       &
                                       "symbols not properly paired",          &
                                        rcToReturn=localrc)

              endif

              !-----------------------------------------------------------------
              ! create work space for parsing the source descriptor string
              !-----------------------------------------------------------------
              allocate( lsrc(src_mem_rank+1) )
              allocate( src_grid_order(src_mem_rank) )
              allocate( src_grid_type(src_mem_rank) )
              allocate( src_grid_HaloL(src_mem_rank) )
              allocate( src_grid_HaloR(src_mem_rank) )
              allocate( src_grid_StaggerLoc(src_mem_rank) )    
              allocate( src_dist_order(src_mem_rank) )      
              allocate( src_dist_type(src_mem_rank) )      

              !-----------------------------------------------------------------
              ! partition the source descriptor string into separate parts which
              ! correspond to memory locations and parse the substrings for 
              ! grid and distribution descriptions.
              !-----------------------------------------------------------------
              call memory_separate( src_string, src_mem_rank, lsrc, localrc) 

              call interpret_descriptor_string( lsrc, src_mem_rank,            & 
                        src_grid_rank, src_grid_order, src_grid_type,          &
                        src_grid_HaloL, src_grid_HaloR, src_grid_StaggerLoc,   &  
                        src_dist_rank, src_dist_order, src_dist_type,          &
                        localrc)

              print*,'Parse source'
              print*,'--------',trim(src_string)
              do k=1,src_mem_rank
                 print*,lsrc(k)%string
              enddo
              print*,'Memory Rank ',src_mem_rank
              print*,'Grid Rank ',src_grid_rank
              print*,'Dist Rank ',src_dist_rank
              do k=1,src_mem_rank
                  print*,k, 'Grid type:',trim(src_grid_type(k)%string),        &
                      src_grid_order(k),                                       &
                      '  Dist type:',trim(src_dist_type(k)%string),':',        &
                      src_dist_order(k)
              enddo
              print*,'Stagger Location',src_grid_StaggerLoc(1:src_grid_rank)
              print*,' grid layout '
              do k=1,src_mem_rank
                 if( src_grid_order(k) > 0 ) then
                     print*,k,trim(src_grid_type(k)%string),src_grid_order(k), &
                       'H{',src_grid_HaloL(k), src_grid_HaloR(k),'}'
                 endif
              enddo
              print*,' Dist layout '
              do k=1,src_mem_rank
                 if( src_dist_order(k) > 0 ) then
                     print*,k,trim(src_dist_type(k)%string),src_dist_order(k)
                 endif
              enddo
              print*,'----------------------'

              deallocate( lsrc )
              deallocate( src_grid_order, src_grid_type )
              deallocate( src_grid_HaloL, src_grid_HaloR )
              deallocate( src_grid_StaggerLoc )    
              deallocate( src_dist_order, src_dist_type )      

              print*,'deallocate source arrays'

              !-----------------------------------------------------------------
              ! create work space for parsing the destination descriptor string
              !-----------------------------------------------------------------
              allocate( ldst(dst_mem_rank+1) )
              allocate( dst_grid_order(dst_mem_rank) )
              allocate( dst_grid_type(dst_mem_rank) )
              allocate( dst_grid_HaloL(dst_mem_rank) )
              allocate( dst_grid_HaloR(dst_mem_rank) )
              allocate( dst_grid_StaggerLoc(dst_mem_rank) )    
              allocate( dst_dist_order(dst_mem_rank) )
              allocate( dst_dist_type(src_mem_rank) )      

              print*,'allocate destination arrays'

              !-----------------------------------------------------------------
              ! partition the destination descriptor string into separate parts
              ! which correspond to memory locations and parse the substrings 
              ! for grid and distribution descriptions.
              !-----------------------------------------------------------------
              call memory_separate( dst_string, dst_mem_rank, ldst, localrc) 

              print*,'finish memory separate destination'

              call interpret_descriptor_string( ldst, dst_mem_rank,            & 
                        dst_grid_rank, dst_grid_order, dst_grid_type,          &
                        dst_grid_HaloL, dst_grid_HaloR, dst_grid_StaggerLoc,   &  
                        dst_dist_rank, dst_dist_order, dst_dist_type,          &
                        localrc)


              print*,'Parse Destination'
              print*,'--------',trim(dst_string)
              do k=1,dst_mem_rank
                 print*,ldst(k)%string
              enddo
              print*,'Memory Rank ',dst_mem_rank
              print*,'Grid Rank ',dst_grid_rank
              print*,'Dist Rank ',dst_dist_rank
              do k=1,dst_mem_rank
                  print*,k, 'Grid type:',trim(dst_grid_type(k)%string),        &
                      dst_grid_order(k),                                       &
                      '  Dist type:',trim(dst_dist_type(k)%string),':',        &
                      dst_dist_order(k)
              enddo
              print*,'Stagger Location',dst_grid_StaggerLoc(1:dst_grid_rank)
              print*,' grid layout '
              do k=1,dst_mem_rank
                 if( dst_grid_order(k) > 0 ) then
                     print*,k,trim(dst_grid_type(k)%string),dst_grid_order(k), &
                       'H{',dst_grid_HaloL(k), dst_grid_HaloR(k),'}'
                 endif
              enddo
              print*,' Dist layout '
              do k=1,dst_mem_rank
                 if( dst_dist_order(k) > 0 ) then
                     print*,k,trim(dst_dist_type(k)%string),dst_dist_order(k)
                 endif
              enddo
              print*,'----------------------'


              deallocate( ldst )
              deallocate( dst_grid_order, dst_grid_type )
              deallocate( dst_grid_HaloL, dst_grid_HaloR )
              deallocate( dst_grid_StaggerLoc )    
              deallocate( dst_dist_order, dst_dist_type )      

           else
              !error
           endif

        else

        endif



        !-----------------------------------------------------------------------
        !-----------------------------------------------------------------------
    ! print*,' TEST '
    ! print*,src_beg, src_end, dst_beg,dst_end 
    ! print*,'123456789012345678901234567890'
    ! print*,trim(lstring)
    ! print*,'process ', harness%Record(kfile)%string(string)%process%name
    ! print*,'src',trim( src_string )
    ! print*,'dst',trim( dst_string ) 
  !-----------------------------------------------------------------------------
  ! find rank of source and destination strings ( plus addresses of dividers ";")
  ! break into substrings, one for each dimension. Increment through, searching 
  ! for key flags, when found set flags.
  !-----------------------------------------------------------------------------
    enddo    ! string
  enddo    ! file
  !-----------------------------------------------------------------------------
  ! if I've gotten this far without an error, then the routine has succeeded.
  !-----------------------------------------------------------------------------
  localrc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine parse_descriptor_string
  !-----------------------------------------------------------------------------

!2345678901234567890123456789012345678901234567890123456789012345678901234567890

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
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
!-------------------------------------------------------------------------------
! Action associated to test report flag value.
!    -2 = debug report
!    -1 = report test failures
!     0 = no output
!     1 = report successful tests
!     2 = report both successes and failures.
!-------------------------------------------------------------------------------
   ! local variables

   ! initialize local rc
   localrc = ESMF_SUCCESS

!-------------------------------------------------------------------------------
! begin 
!-------------------------------------------------------------------------------
  select case (report_flag)
     case(-2)
     ! debug report

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

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! !IROUTINE: ReportSuccess

! !INTERFACE:
  subroutine ReportSuccess(localrc)
!
! !ARGUMENTS: 
  integer, intent(out) :: localrc

!
! !DESCRIPTION:
! This routine produces a human readable report of the successful test results.
!-------------------------------------------------------------------------------
  ! initialize local rc
  localrc = ESMF_SUCCESS

  print*,'Output human readable report of test successes'

  end subroutine ReportSuccess

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! !IROUTINE: ReportFailure

! !INTERFACE: 
  subroutine ReportFailure(localrc)
!
! !ARGUMENTS:
  integer, intent(out) :: localrc

!
! !DESCRIPTION:
! This routine produces a human readable report of the failed test results.
!-------------------------------------------------------------------------------
  ! initialize local rc
  localrc = ESMF_SUCCESS

  print*,'Output human readable report of test failures'

  end subroutine ReportFailure

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------


  end program ESMF_Test_Harness
