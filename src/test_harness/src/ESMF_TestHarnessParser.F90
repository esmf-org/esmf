! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!===============================================================================
#define ESMF_FILENAME "ESMF_TestHarnessParser"
!
!  ESMF Test Harness Parser
   module ESMF_TestHarnessParser
!
!===============================================================================
!
! This file contains parameters, global data types, and parser 
! functions/subroutines for the Testing Harness.
! These methods are used by the test harness driver ESMF_TestHarnessUTest.F90.
!
!-------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!===============================================================================
!BOPI
! !MODULE: ESMF_TestHarnessParser
!
! !DESCRIPTION:
!
! The code in this file contains data types and basic functions for the
! {\tt ESMF\_TestHarnessParse}.  
!  Expand on the type of routines included here
!
!-------------------------------------------------------------------------------
!
! !USES:

  use ESMF_TestHarnessReportMod

  use ESMF_TestHarnessGridMod
  use ESMF_TestHarnessDistMod

  use ESMF

  implicit none
  private

!-------------------------------------------------------------------------------
! PUBLIC METHODS:
!-------------------------------------------------------------------------------
  public read_testharness_config
  public read_testharness_specifier
  public read_descriptor_files
  public parse_descriptor_string

!===============================================================================
! debug trace switch
logical                       :: checkpoint = .FALSE.

! minimum error neighborhood for regrid interpolation
!real(ESMF_KIND_R8), parameter :: RegridMinNeighborhood = 1.0D-14

  contains 

!===============================================================================
! !IROUTINE: Read_TestHarness_Config

! !INTERFACE:
  subroutine Read_TestHarness_Config(srcPath, configFname, returnrc)
!
! !ARGUMENTS:
  character(len=*), intent(in)  :: srcPath
  character(len=*), intent(in)  :: configFname
  integer,          intent(out) :: returnrc

! actual arguments through globals
!    har structure

!
! !DESCRIPTION:
! Routine opens the top level config file "test_harness.rc", which specifies the 
! test class, the reporting style, and depending on how the ESMF_TESTEXHAUSTIVE
! flag is set, extracts the list of files containing the problem descriptor
! strings.
!
! Upon completion, the routine returns the values to a public record
!       har%configPath              path to configuration files
!       har%topFname                top level configuration filename
!       har%testClass               Problem Descriptor Test Class
!       har%reportType              Output Report type
!       har%numRecords              number of problem descriptor filenames
!       har%rcrd(k)%filename        kth problem descriptor filename
!===============================================================================

  ! local ESMF types
  type(ESMF_Config)      :: localcf

  ! local parameters
  character(THARN_MAXSTR), parameter :: test_class_name   = "test_class:"
  character(THARN_MAXSTR), parameter :: setup_report_name = "setup_report:"
  character(THARN_MAXSTR), parameter :: test_report_name  = "test_report:"


  ! local character strings
  character(THARN_MAXSTR) :: ltag, ltmp
  character(THARN_MAXSTR) :: filename

  ! local integer variables
  integer :: kfile, ncolumns
  integer :: localrc
  integer :: allocRcToTest 

  ! local  logical
  logical :: flag = .true.

  ! initialize return code
  returnrc = ESMF_RC_NOT_IMPL
  localrc = ESMF_RC_NOT_IMPL

  ! save config path for summary report
  har%configPath = adjustL(srcPath)

  ! save top level filename for summary report
  har%topFname = adjustL(configFname)

  !-----------------------------------------------------------------------------
  ! create config handle and load the testing harness config file
  !-----------------------------------------------------------------------------
  localcf = ESMF_ConfigCreate(rc=localrc)
  if( ESMF_LogFoundError(localrc, msg="cannot create config object",            &
                            rcToReturn=returnrc) ) return

  !call ESMF_ConfigLoadFile(localcf, trim(adjustL(test_harness_name)),          &
  !         rc=localrc )

  filename = trim(srcPath) // "/" // trim(configFname)
  call ESMF_ConfigLoadFile (localcf, trim(filename), rc=localrc)
  if( ESMF_LogFoundError(localrc, msg="cannot load config file " //             &
      trim(configFname), rcToReturn=returnrc) ) return
  
  !-----------------------------------------------------------------------------
  ! find and read the test class 
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(adjustL(test_class_name)), rc=localrc) 
  if( ESMF_LogFoundError(localrc, msg="cannot find config label " //            &
      trim(adjustL(test_class_name)),rcToReturn=returnrc) ) return

  call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc )
  har%testClass = trim(adjustL( ltmp ))
  if( ESMF_LogFoundError(localrc, msg="cannot get value for label " //          &
      trim(adjustL(test_class_name)),rcToReturn=returnrc) ) return

  !-----------------------------------------------------------------------------
  ! if the class is not supported, then post an error
  !-----------------------------------------------------------------------------
  if ( trim(adjustL(har%testClass)) /= 'ARRAY'        .and.                    &
       trim(adjustL(har%testClass)) /= 'ARRAYBUNDLE'  .and.                    &
       trim(adjustL(har%testClass)) /= 'FIELD'        .and.                    &
       trim(adjustL(har%testClass)) /= 'FIELDBUNDLE' .and.                     &
       trim(adjustL(har%testClass)) /= 'GRID'   .and.                          &
       trim(adjustL(har%testClass)) /= 'REGRID' ) then
     call ESMF_LogSetError( ESMF_FAILURE,msg="class name not of valid type "//  &
          trim(adjustL(har%testClass)), rcToReturn=returnrc)
     return
  endif

  !-----------------------------------------------------------------------------
  ! determine type of test report and toggle setup report
  !-----------------------------------------------------------------------------

  call ESMF_ConfigFindLabel(localcf,trim(adjustL(setup_report_name)),rc=localrc)
  if( ESMF_LogFoundError(localrc,msg="cannot find config label " //             &
      trim(adjustL(setup_report_name)), rcToReturn=returnrc) ) return

  call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
  har%setupReportType = trim(adjustL( ltmp ))
  if( ESMF_LogFoundError(localrc,  msg="cannot get value for label " //         &
      trim(adjustL(setup_report_name)), rcToReturn=returnrc) ) return

  if((har%setupReportType /= "TRUE").and.(har%setupReportType /= "FALSE")) then
     call ESMF_LogSetError( ESMF_FAILURE, msg="setup report flag " //           &
          "improperly set " // trim(har%setupReportType), rcToReturn=returnrc)
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
  if( ESMF_LogFoundError(localrc, msg="cannot find config label " //            &
      trim(adjustL(test_report_name)), rcToReturn=returnrc) ) return

  call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc )
  har%reportType = trim(adjustL( ltmp ))
  if( ESMF_LogFoundError(localrc, msg="cannot get value for label " //          &
      trim(adjustL(test_report_name)), rcToReturn=returnrc) ) return

  if ( har%reportType /= "FULL" .and. har%reportType /= "FAILURE" .and.        &
       har%reportType /= "SUCCESS" .and. har%reportType /= "NONE" ) then
     call ESMF_LogSetError( ESMF_FAILURE, msg="report flag improperly set" //   &
          trim(har%reportType), rcToReturn=returnrc) 
     return
  endif

  !-----------------------------------------------------------------------------
  ! based on whether exhaustive or nonexhaustive tests are to be run,  find 
  ! and load the problem descriptor file names
  !-----------------------------------------------------------------------------
!#ifdef ESMF_TESTEXHAUSTIVE
! ltag = 'exhaustive::'
! if(localPet == rootPet)  print *, "running exhaustive tests"
!#else
  ltag = 'nonexhaustive::'
  if(localPet == rootPet)  print *, "running nonexhaustive tests"
!#endif
  call ESMF_ConfigFindLabel(localcf, trim(adjustL(ltag)), rc=localrc )
  if( ESMF_LogFoundError(localrc, msg="cannot find config label " //            &
      trim(adjustL(ltag)), rcToReturn=returnrc) ) return

  ! determine the number of entries
  call ESMF_ConfigGetDim(localcf, har%numRecords, ncolumns,                    &
       label=trim(adjustL(ltag)), rc=localrc)
  if( ESMF_LogFoundError(localrc, msg="cannot find the size of the table " //   &
      trim(adjustL(ltag)), rcToReturn=returnrc) ) return

  ! if there are no entries post an error
  if ( har%numRecords .le. 0 ) then
     call ESMF_LogSetError( ESMF_FAILURE, msg="no problem descriptor files "//  &
          "specified", rcToReturn=returnrc)
     return
  endif

  !-----------------------------------------------------------------------------
  ! find the problem descriptor file names and read them
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(adjustL(ltag)), rc=localrc)
  if( ESMF_LogFoundError(localrc, msg="cannot find table label of " //          &
      trim(adjustL(ltag)), rcToReturn=returnrc) ) return

  !-----------------------------------------------------------------------------
  ! allocate space to hold problem descriptor filenames and advance through the
  ! table extracting the problem descriptor filenames
  !-----------------------------------------------------------------------------
  allocate( har%rcrd(har%numRecords), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="rcrd type "//                 &
     " in Read_TestHarness_Config", rcToReturn=returnrc)) then
  endif


  do kfile=1,har%numRecords
     ! advance to new line in table
     call ESMF_ConfigNextLine(localcf, tableEnd=flag, rc=localrc)
     if( ESMF_LogFoundError(localrc, msg="cannot advance to next line of " //   &
         "table " // trim(adjustL(ltag)), rcToReturn=returnrc) ) return
 
     ! retrieve the problem descriptor filenames 
     call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
     if( ESMF_LogFoundError(localrc, msg="cannot get descriptor filename in "// &
         trim(adjustL(ltag)), rcToReturn=returnrc) ) return

     filename = trim(srcPath) // "/" // trim(adjustL(ltmp))
     har%rcrd(kfile)%filename  = trim(filename)
  enddo   ! file

  !-----------------------------------------------------------------------------
  ! clean up CF
  !-----------------------------------------------------------------------------
  call ESMF_ConfigDestroy(localcf, rc=localrc)
  if( ESMF_LogFoundError(localrc, msg="cannot destroy config file " //          &
      trim(configFname), rcToReturn=returnrc) ) return

  ! if I've gotten this far without an error, then the routine has succeeded.
  returnrc = ESMF_SUCCESS

!===============================================================================
  end subroutine Read_TestHarness_Config
!===============================================================================

!-------------------------------------------------------------------------------
 
!===============================================================================
! !IROUTINE: Read_TestHarness_Specifier

! !INTERFACE:
  subroutine Read_TestHarness_Specifier(srcPath, returnrc)
!
! !ARGUMENTS:
  character(len=*), intent (in)   :: srcPath
  integer,          intent(inout) :: returnrc

!
! !DESCRIPTION:
! the routine conducts the three tasks needed to conduct the test runs:
!
! 1. Reads the problem descriptor files, storing each problem descriptor string
! and the names of the accompaning support files in the harness descriptor record.
!
! 2. Parse the descriptor strings and store the information in the harness 
! descriptor record.
!
! 3. Read the grid and distribution specifier files and store the configurations
! in the harness descriptor record.
!
!===============================================================================

  ! local ESMF types

  ! local parameters
  ! logical :: flag = .true.

  ! local integer variables
  integer :: nPEs
  integer :: k, kfile, kstr
  integer :: iDfile, iGfile, iD, iG
  integer :: nDfiles, nGfiles, nstatus
  integer :: nDspec, nGspec
  integer :: localrc
  integer :: allocRcToTest

  ! initialize return flag
  returnrc = ESMF_RC_NOT_IMPL
  localrc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! read each of the problem descriptor files obtained from read_testharness_config 
  ! and extract the problem descriptor strings and the accompanying specifier
  ! filenames.
  !-----------------------------------------------------------------------------
  call read_descriptor_files(srcPath, har%numRecords,har%rcrd,localrc)
  if (ESMF_LogFoundError(localrc, msg="ERROR with read descriptor files",       &
     rcToReturn=returnrc)) return

  !-----------------------------------------------------------------------------
  ! loops through the list of descriptor files and reads each problem
  ! descriptor string and its associated specifer files for (1) class, (2)
  ! distribution ensemble, and (3) grid ensemble.
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! parse each problem descriptor string in each of the numRecords files
  !-----------------------------------------------------------------------------
  do kfile=1,har%numRecords
     do kstr=1,har%rcrd(kfile)%numStrings
        call parse_descriptor_string(har%rcrd(kfile)%numStrings,               &
                  har%rcrd(kfile)%str(kstr), localrc)
        if (ESMF_LogFoundError(localrc,msg=" error in problem descriptor file " &
           // trim(adjustL(har%rcrd(kfile)%filename)),                         &
           rcToReturn=returnrc)) return  

        ! read distribution specifier files
        do k=1,har%rcrd(kfile)%str(kstr)%nDfiles
           nPEs = petCount
           call read_dist_specification(nPEs,                                  &
                    har%rcrd(kfile)%str(kstr)%Dfiles(k),                       &
                    har%rcrd(kfile)%str(kstr)%DstMem,                          &
                    har%rcrd(kfile)%str(kstr)%SrcMem, localrc)       
           if (ESMF_LogFoundError(localrc,msg=" error reading dist specifier"   &
              // " file "  //                                                  &
              trim(adjustL(har%rcrd(kfile)%str(kstr)%Dfiles(k)%filename)),     &
              rcToReturn=returnrc)) return
        enddo   ! k

        ! read grid specifier files
        do k=1,har%rcrd(kfile)%str(kstr)%nGfiles
           call read_grid_specification(har%rcrd(kfile)%str(kstr)%Gfiles(k),   &
                    localrc)
           if (ESMF_LogFoundError(localrc,msg=" error reading grid specifier"   &
              // " file "  //                                                  &
              trim(adjustL(har%rcrd(kfile)%str(kstr)%Gfiles(k)%filename)),     &
              rcToReturn=returnrc)) return
        enddo   ! k

        ! allocate and initialize test status
        nDfiles = har%rcrd(kfile)%str(kstr)%nDfiles
        nGfiles = har%rcrd(kfile)%str(kstr)%nGfiles
        allocate( har%rcrd(kfile)%str(kstr)%test_record(nDfiles,nGfiles),      &
                  stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="rcrd type "//           &
           " in Read_TestHarness_Config", rcToReturn=returnrc)) then
        endif

        ! initialize test result to UNDEFINED
        do iDfile=1,har%rcrd(kfile)%str(kstr)%nDfiles
        do iGfile=1,har%rcrd(kfile)%str(kstr)%nGfiles
           nDspec = har%rcrd(kfile)%str(kstr)%Dfiles(iDfile)%nDspecs
           nGspec = har%rcrd(kfile)%str(kstr)%Gfiles(iGfile)%nGspecs
        !  print*,'==== file  sizes',iDfile,iGfile,nDspec,nGspec
           ! allocate work space for test result
           allocate( har%rcrd(kfile)%str(kstr)%test_record(iDfile,iGfile)%     &
              test_status(nDspec,nGspec), stat=allocRcToTest )
           if (ESMF_LogFoundAllocError(allocRcToTest, msg="test status type"//  &
              " in Read_TestHarness_Config", rcToReturn=returnrc)) then
           endif
           ! allocate work space for test string
           allocate( har%rcrd(kfile)%str(kstr)%test_record(iDfile,iGfile)%     &
              test_string(nDspec,nGspec), stat=allocRcToTest )
           if (ESMF_LogFoundAllocError(allocRcToTest, msg="test status type"//  &
              " in Read_TestHarness_Config", rcToReturn=returnrc)) then
           endif

           do iD=1, nDspec
           do iG=1, nGspec
             har%rcrd(kfile)%str(kstr)%test_record(iDfile,iGfile)%             &
                 test_status(iD,iG) = HarnessTest_UNDEFINED 
           enddo   ! iG
           enddo   ! iD

        enddo   ! iGfile
        enddo   ! iDfile

     enddo  ! kstr
  enddo    ! kfile

  !-----------------------------------------------------------------------------
  ! list imported problem configurations before continuing
  !-----------------------------------------------------------------------------
  nstatus= 0
  if( trim(har%setupReportType) == "TRUE" )  nstatus= 1
  do kfile=1,har%numrecords
     do kstr=1,har%rcrd(kfile)%numStrings
        call construct_descriptor_string(har%rcrd(kfile)%str(kstr),nstatus, &  
                                         localPet, localrc)
     enddo  ! kstr
  enddo    ! kfile

  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! if I've gotten this far without an error, then the routine has succeeded.
  !-----------------------------------------------------------------------------
  returnrc = ESMF_SUCCESS

!===============================================================================
  end subroutine Read_TestHarness_Specifier
!===============================================================================

!-------------------------------------------------------------------------------
!===============================================================================
! Public Methods
!===============================================================================

!-------------------------------------------------------------------------------
  subroutine read_descriptor_files(srcPath, numRecords,rcrd,rc)
!-------------------------------------------------------------------------------
!
! !ARGUMENTS:
  character(len=*),                 intent(in)  :: srcPath
  integer,                          intent(in)  :: numRecords
  type(problem_descriptor_records), pointer     :: rcrd(:) 
  integer,                          intent(out) :: rc

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
  character(THARN_MAXSTR), parameter :: descriptor_label      &
                                               = "problem_descriptor_string::"

  ! local character types
  type (sized_char_array), allocatable :: ltmpstring(:), lstring(:)

  ! local character strings
  character(THARN_MAXSTR) :: lfilename
  character(THARN_MAXSTR) :: ltmp
  character(THARN_MAXSTR) :: lchar, lchar1, lchar2
  
  logical :: flag

! local integer variables
  integer :: n, nn, k, pos, kcol, ncount, npds, ntmp
  integer :: kfile, kstr, pstring
  integer :: cpos, dpos, gpos, csize, dsize, gsize
  integer, allocatable :: kcount(:), ncolumns(:), nstrings(:)
  integer, allocatable :: pds_loc(:), pds_flag(:)
  integer :: localrc
  integer :: allocRcToTest

! local logical variable
  logical :: endflag
  logical :: cflag, dflag, gflag

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! open each problem descriptor and extract the contents of the table
  ! containing the problem descriptor strings and the specifier filenames
  !-----------------------------------------------------------------------------
  allocate( nstrings(numRecords), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array "//             &
     " nstrings in read_descriptor_files", rcToReturn=rc)) then
  endif

  do kfile=1,numRecords
    !---------------------------------------------------------------------------
    ! create a new config handle for reading problem descriptor strings
    !---------------------------------------------------------------------------
    localcf = ESMF_ConfigCreate(rc=localrc)
    if( ESMF_LogFoundError(localrc, msg="cannot create config object",          &
                         rcToReturn=rc) ) return

    !---------------------------------------------------------------------------
    ! load file holding the problem descriptor strings
    !---------------------------------------------------------------------------
    lfilename = trim(adjustL(rcrd(kfile)%filename))

    call ESMF_ConfigLoadFile(localcf, trim(adjustL(lfilename)), rc=localrc )
    if( CheckError(checkpoint, __LINE__, __FILE__, localrc, "cannot load config file " //           &
            trim(adjustL(lfilename)), rcToReturn=rc) ) return

    !---------------------------------------------------------------------------
    ! Search for the problem descriptor string table
    !---------------------------------------------------------------------------
    call ESMF_ConfigFindLabel(localcf, trim(adjustL(descriptor_label)),        &
             rc=localrc )
    if( CheckError(checkpoint, __LINE__, __FILE__, localrc, "cannot find config label " //          &
             trim(adjustL(descriptor_label)), rcToReturn=rc) ) return

    !---------------------------------------------------------------------------
    ! determine the number of entries
    !---------------------------------------------------------------------------
    call ESMF_ConfigGetDim(localcf, nstrings(kfile), ntmp,                     &
             label=trim(adjustL(descriptor_label)), rc=localrc)
    if( CheckError(checkpoint, __LINE__, __FILE__, localrc, "cannot get descriptor table size in "  &
            // "file " // trim(adjustL(lfilename)), rcToReturn=rc) ) return

    !---------------------------------------------------------------------------
    ! determine that the table has entries before preceeding
    !---------------------------------------------------------------------------
    if( nstrings(kfile) .le. 0 ) then
      call ESMF_LogSetError( ESMF_FAILURE, msg="problem descriptor table empty" &
               // " in file " // trim(adjustL(lfilename)), rcToReturn=rc)
      return
    endif

    !---------------------------------------------------------------------------
    ! extract column lengths of the table to determine the number of specifier files
    !---------------------------------------------------------------------------
    call ESMF_ConfigFindLabel(localcf, trim(adjustL(descriptor_label)),        &
           rc=localrc )
    if( CheckError(checkpoint, __LINE__, __FILE__, localrc, "cannot find config label" //           &
           trim(adjustL(descriptor_label)), rcToReturn=rc) ) return

    allocate( ncolumns(nstrings(kfile)), stat=allocRcToTest )
    if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array "//           &
       " nstrings in read_descriptor_files", rcToReturn=rc)) then
    endif
    allocate ( ltmpstring(nstrings(kfile)), stat=allocRcToTest )
    if (ESMF_LogFoundAllocError(allocRcToTest, msg="type "//                    &
       " ltmpstring in read_descriptor_files", rcToReturn=rc)) then
    endif

    do kstr=1,nstrings(kfile)
      call ESMF_ConfigNextLine(localcf, tableEnd=flag , rc=localrc)
      if( CheckError(checkpoint, __LINE__, __FILE__, localrc, "cannot advance to the next line of"  &
              //" the table "// trim(adjustL(descriptor_label)) // " in file " &
              // trim(adjustL(lfilename)), rcToReturn=rc) ) return

      ncolumns(kstr) = ESMF_ConfigGetLen(localcf, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS .or. ncolumns(kstr) .lt. 1 ) then
        write(lchar,"(i5)")  kstr
        call ESMF_LogSetError( ESMF_FAILURE, msg="problem reading line " //     &
                 trim(adjustL(lchar)) // " of table in file " //               &
                 trim(adjustL(lfilename)), rcToReturn=rc)
        return
      endif

      !-------------------------------------------------------------------------
      ! allocate tempory storage so that the file needs to be read only once
      !-------------------------------------------------------------------------
      allocate ( ltmpstring(kstr)%tag( ncolumns(kstr) ), stat=allocRcToTest )
      if (ESMF_LogFoundAllocError(allocRcToTest, msg="type "//                  &
         " ltmpstring in read_descriptor_files", rcToReturn=rc)) then
      endif
      ltmpstring(kstr)%tagsize = ncolumns(kstr)
    enddo    ! end string

    !---------------------------------------------------------------------------
    ! Starting again at the top of the table, extract the table contents into
    ! a local character array structure for later processing
    !---------------------------------------------------------------------------
    call ESMF_ConfigFindLabel(localcf, trim(adjustL(descriptor_label)),        &
             rc=localrc )
    if( CheckError(checkpoint, __LINE__, __FILE__, localrc,                                         &
            "cannot find config label " // trim(adjustL(descriptor_label)),    &
            rcToReturn=rc) ) return

    do kstr=1,nstrings(kfile)
    !---------------------------------------------------------------------------
    ! copy the table into a character array
    !---------------------------------------------------------------------------
      call ESMF_ConfigNextLine(localcf, tableEnd=flag , rc=localrc)
      if( CheckError(checkpoint, __LINE__, __FILE__, localrc, "cannot advance to the next line " // &
              "of table " // trim(adjustL(descriptor_label)) // " in file " // &
              trim(adjustL(lfilename)), rcToReturn=rc) ) return

      do kcol=1, ncolumns(kstr)
        call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
        write(lchar,"(i5)") kstr
        if( CheckError(checkpoint, __LINE__, __FILE__, localrc, "cannot get table entry from line " &
                // trim(adjustl(lchar)) //  " column " // char(kcol)  //       &
                "of file " // trim(adjustL(lfilename)),                        &
                rcToReturn=rc) ) return
         ltmpstring(kstr)%tag(kcol)%string = trim( ltmp )
      enddo     ! end col
    enddo       ! end string

    !---------------------------------------------------------------------------
    ! count the number of actual problem descriptor strings & continuation lines
    !---------------------------------------------------------------------------
    ncount = 0
    npds = 0
    allocate( pds_flag(nstrings(kfile)), stat=allocRcToTest )
    if (ESMF_LogFoundAllocError(allocRcToTest, msg="type "//                    &
       " pdf_flag in read_descriptor_files", rcToReturn=rc)) then
    endif

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
      call ESMF_LogSetError( ESMF_FAILURE, msg="number of rows " //             &
             trim(adjustl(lchar)) // " in the table"  //                       &
             " does not match the sum of strings " // trim(adjustl(lchar1))    &
             // " and continuation lines " //  trim(adjustl(lchar2)) //        &
             " of file " // trim(adjustL(lfilename)), rcToReturn=rc) 
      return
    endif

    rcrd(kfile)%numStrings = npds

    !---------------------------------------------------------------------------
    ! save the addresses of the non-continuation lines
    !---------------------------------------------------------------------------
    k = 0
    allocate( pds_loc(npds), stat=allocRcToTest )
    if (ESMF_LogFoundAllocError(allocRcToTest, msg="type "//                    &
       " pds_loc in read_descriptor_files", rcToReturn=rc)) then
    endif

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
      call ESMF_LogSetError( ESMF_FAILURE, msg="number of rows " //             &
             trim(adjustl(lchar)) // " in the table" //                        &
             " does not match the sum of strings "//trim(adjustl(lchar1))      &
             // " and continuation lines " // trim(adjustl(lchar2)) //         &
             " of file " // trim(adjustL(lfilename)), rcToReturn=rc)
      return
    endif

    !---------------------------------------------------------------------------
    ! to simplify the later search algorithm, reshape the input table from a 
    ! series of lines with a PDS plus optional continuations lines, to a single
    ! line with everything on it. Count the total number of elements on both 
    ! type of lines to that we can allocate enough memory to store the whole 
    ! specification.
    !---------------------------------------------------------------------------
    allocate( kcount(npds), stat=allocRcToTest )
    if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//        &
       " kcount in read_descriptor_files", rcToReturn=rc)) then
    endif

    do k=1,npds
      if( trim( ltmpstring( pds_loc(k) )%tag(1)%string ) == "&") then
        write(lchar,"(i5)")   pds_loc(k)
        call ESMF_LogSetError( ESMF_FAILURE,                                &
                 msg="no problem descriptor string on line " //                    &
                 trim(adjustl(lchar)) // " of file " //                        &
                 trim(adjustL(lfilename)),rcToReturn=rc)
        return
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
    allocate ( lstring(npds), stat=allocRcToTest )    
    if (ESMF_LogFoundAllocError(allocRcToTest, msg="type "//                    &
       " lstring in read_descriptor_files", rcToReturn=rc)) then
    endif

    do k=1, npds
      allocate ( lstring(k)%tag(kcount(k)), stat=allocRcToTest )    
      if (ESMF_LogFoundAllocError(allocRcToTest, msg="type "//                  &
         " lstring tag in read_descriptor_files", rcToReturn=rc)) then
      endif

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
    allocate( rcrd(kfile)%str(npds), stat=allocRcToTest )
    if (ESMF_LogFoundAllocError(allocRcToTest, msg="type "//                    &
       " rcrd string in read_descriptor_files", rcToReturn=rc)) then
    endif
    do k=1,npds
       rcrd(kfile)%str(k)%pds = trim( lstring(k)%tag(1)%string )
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
           call ESMF_LogSetError( ESMF_FAILURE, msg="the -c specifier flag"  &
                    // " is used more than once on the " //                 &
                   trim(adjustl(lchar))//"th string of the problem " //     &
                   "descriptor table in file" // trim(lfilename),           &
                   rcToReturn=rc)
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

         allocate( rcrd(kfile)%str(k)%classfile%tag(csize), stat=allocRcToTest )
         if (ESMF_LogFoundAllocError(allocRcToTest, msg="type "//               &
            " rcrd tag in read_descriptor_files", rcToReturn=rc)) then
         endif

         rcrd(kfile)%str(k)%classfile%tagsize = csize
         do n=1,csize
           rcrd(kfile)%str(k)%classfile%tag(n)%string =                        &
                                 trim(adjustL( lstring(k)%tag(cpos+n)%string ))
         enddo      ! n
         cflag = .true.

         !----------------------------------------------------------------------
         ! distribution descriptor file
         !----------------------------------------------------------------------
         case('-d')
         if( dflag ) then
           write(lchar,"(i5)") k
           call ESMF_LogSetError( ESMF_FAILURE, msg="the -d specifier flag"     &
                    // " is used more than once on the " //                    &
                   trim(adjustl(lchar))//"th string of the problem " //        &
                   "descriptor table in file" // trim(adjustL(lfilename)),     &
                   rcToReturn=rc)
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

         allocate( rcrd(kfile)%str(k)%Dfiles(dsize), stat=allocRcToTest )
         if (ESMF_LogFoundAllocError(allocRcToTest, msg="type "//               &
            " rcrd Dfiles in read_descriptor_files", rcToReturn=rc)) then
         endif

         rcrd(kfile)%str(k)%nDfiles = dsize
         do n=1,dsize
           ! build complete filename icluding source path
           lfilename = trim(srcPath) // "/" // trim(adjustL(lstring(k)%tag(dpos+n)%string))
           rcrd(kfile)%str(k)%Dfiles(n)%filename = lfilename

           !rcrd(kfile)%str(k)%Dfiles(n)%filename =                             &
                              ! trim(adjustL( lstring(k)%tag(dpos+n)%string ))
         enddo      ! n
         dflag = .true.

         !----------------------------------------------------------------------
         ! grid descriptor file
         !----------------------------------------------------------------------
         case('-g')
         if( gflag ) then
           write(lchar,"(i5)") k
           call ESMF_LogSetError( ESMF_FAILURE, msg="the -g specifier flag" //  &
                    " is used more than once on the " // trim(adjustl(lchar))  &
                    //"th string of the problem descriptor table in file " //  &
                    trim(adjustL(lfilename)), rcToReturn=rc)
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

         allocate( rcrd(kfile)%str(k)%Gfiles(gsize), stat=allocRcToTest )
         if (ESMF_LogFoundAllocError(allocRcToTest, msg="type "//               &
            " rcrd Gfiles in read_descriptor_files", rcToReturn=rc)) then
         endif

         rcrd(kfile)%str(k)%nGfiles = gsize
         do n=1,gsize
           ! build complete filename icluding source path
           lfilename = trim(srcPath) // "/" // trim(adjustL(lstring(k)%tag(gpos+n)%string))
           rcrd(kfile)%str(k)%Gfiles(n)%filename = lfilename

           !rcrd(kfile)%str(k)%Gfiles(n)%filename =                             &
                             !trim(adjustL(lstring(k)%tag(gpos+n)%string))
         enddo     ! n
         gflag = .true.

         !----------------------------------------------------------------------
         ! syntax error - entry after pds should be a flag
         !----------------------------------------------------------------------
         case default
         write(lchar,"(i5)")  pds_loc(k)
         call ESMF_LogSetError( ESMF_FAILURE,                               &
               msg="no specifier flag on line " // trim(adjustl(lchar)) //         &
               " of file " //trim(lfilename), rcToReturn=rc)
         return

        end select  ! specifier flag type
      end do  ! while
    enddo      ! k

    !---------------------------------------------------------------------------
    ! finish cleaning up workspace before opening new file
    !---------------------------------------------------------------------------

    do k=1, npds
      deallocate ( lstring(k)%tag )
    enddo
    do kstr=1,nstrings(kfile)
      deallocate ( ltmpstring(kstr)%tag )
    enddo  
    deallocate( ncolumns, kcount )
    deallocate( ltmpstring, lstring )
    deallocate( pds_loc, pds_flag )

    !---------------------------------------------------------------------------
    ! clean up CF
    !---------------------------------------------------------------------------
    call ESMF_ConfigDestroy(localcf, rc=localrc)
    if( CheckError(checkpoint, __LINE__, __FILE__, localrc, "cannot destroy config file "  //       &
            trim(adjustL(lfilename)),  rcToReturn=rc) ) return
  enddo  ! file

  !-----------------------------------------------------------------------------
  ! final deallocation
  !-----------------------------------------------------------------------------
  deallocate( nstrings )

  !-----------------------------------------------------------------------------
  ! if I've gotten this far without an error, then the routine has succeeded.
  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine read_descriptor_files
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine parse_descriptor_string(nstrings, pds, rc)
  !-----------------------------------------------------------------------------
  ! Routine parses a problem descriptor string and extracts:
  !	operation - redistribution or regrid plus method
  !	rank of memory
  !	rank of distribution
  !	rank of grid association
  !     order of dist and grid
  !     type of dist or grid
  !
  ! Upon completion, the routine returns the values to the harness record
  ! Harness%Record(*)%string(*)%   for 
  !
  !     process%string          character string of process
  !     process%tag             numerical tag for process
  !
  !     SrcMem%memRank          rank of source memory block
  !     SrcMem%GridRank         rank of source grid 
  !     SrcMem%DistRank         rank of source distribution
  !     SrcMem%GridType         type of grid
  !     SrcMem%DistType         type of distribution
  !     SrcMem%GridOrder        order of grid elements with dimensions
  !     SrcMem%DistOrder        order of distribution elements with dimensions
  !     SrcMem%HaloL            Left halo size for each dimension
  !     SrcMem%HaloR            Right halo size for each dimension
  !     SrcMem%StagLoc          Stagger location for each dimension
  ! and the equivalent DstMem records.
  !
  !-----------------------------------------------------------------------------
  ! arguments
   integer, intent(in   ) :: nstrings
  type(problem_descriptor_strings), intent(inout) :: pds
  integer, intent(  out) :: rc

  ! local character strings
  character(THARN_MAXSTR) :: lstring, lname
  character(THARN_MAXSTR) :: src_string, dst_string
  type(character_array), allocatable :: lsrc(:), ldst(:)
  type(character_array), allocatable :: grid_type(:), dist_type(:)

  ! logical :: flag = .true.

  ! local integer variables
  integer :: k
  integer :: tag, location(2)
  integer :: dst_beg, dst_end, src_beg, src_end
  integer :: srcMulti, dstMulti
  integer :: srcBlock,dstBlock
  integer :: src_mem_rank, dst_mem_rank
  integer :: dist_rank, grid_rank
  integer :: localrc
  integer :: allocRcToTest

  integer, allocatable :: grid_order(:), dist_order(:)
  integer, allocatable :: grid_HaloL(:), grid_HaloR(:)
  integer, allocatable :: grid_StagLoc(:)

  ! local logical variable
  ! logical :: endflag = .false.

  ! initialize return flag
  rc = ESMF_RC_NOT_IMPL
  localrc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! parse the problem descriptor string
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! search for the test operation
  ! 1. search for "->" or "=>" - tip of symbol provides ending address
  ! 2. back up until reach white space - provides begining address
  ! 3. src_beg = 1, src_end = operation_beg-1
  !    dst_beg = operation_end+1, dst_end = length(trim(string))
  ! 4. LOCATION provides a reference for later splitting the string into
  !    source and destination parts
  !-----------------------------------------------------------------------------
  lstring = trim(adjustL( pds%pds ) )
  call process_query(lstring, lname, tag, location, localrc)  
  if( CheckError(checkpoint, __LINE__, __FILE__, localrc,"syntax error in problem descriptor" //    &
          " string " // trim(adjustL(lstring)),                                &
          rcToReturn=rc) ) return


  ! store name (string) of operation and numerical code
  pds%process%string = lname
  pds%process%tag = tag

  !-----------------------------------------------------------------------------
  ! separate the problem descriptor string into source and destination
  ! chunks to ease with parsing, but first determine if the problem
  ! descriptor string describes a multiple block memory structure
  !-----------------------------------------------------------------------------
  call memory_topology(lstring, location, srcMulti, srcBlock,                  &
                       dstMulti, dstBlock, localrc)
  if( CheckError(checkpoint, __LINE__, __FILE__, localrc,"syntax error in problem descriptor" //    &
          " string " // trim(adjustL(lstring)),                                &
          rcToReturn=rc) ) return

  ! a multiple block memory structure contains (), these are counted in
  ! srcMulti and dstMulti 
  if( (srcMulti >= 1).or.(dstMulti >= 1) ) then
     ! TODO break into multiple single block strings
     ! and parse each string separately

  elseif( (srcMulti == 0).and.(dstMulti == 0) ) then
     ! single block memory structure
     if( (srcBlock==1).and.(dstBlock==1) ) then
        src_beg = 1
        src_end = location(1)
        dst_beg = location(2)
        dst_end = len( trim(lstring) )
        !-----------------------------------------------------------------------
        ! separate string into source and destination strings
        !-----------------------------------------------------------------------
        src_string = adjustL( lstring(src_beg:src_end) )
        dst_string = adjustL( lstring(dst_beg:dst_end) )

        !-----------------------------------------------------------------------
        ! check that the source and destination memory ranks are not empty
        ! and that the sizes agree
        !-----------------------------------------------------------------------
        src_mem_rank = pattern_query(src_string,';') + 1
        pds%SrcMem%memRank = src_mem_rank
        dst_mem_rank = pattern_query(dst_string,';') + 1
        pds%DstMem%memRank = dst_mem_rank

        if( (src_mem_rank == 0).or.(dst_mem_rank == 0).or.                     &
            (src_mem_rank /= dst_mem_rank) )  then
           localrc = ESMF_FAILURE
           call ESMF_LogSetError(ESMF_FAILURE,msg="rank of memory block "       &
                    // "symbols not properly paired", rcToReturn=rc)
           return
        endif

        ! test for common mistake of using commas instead of semicolons
        if( pattern_query(dst_string,',') > 0 ) then
           localrc = ESMF_FAILURE
           call ESMF_LogSetError(ESMF_FAILURE,msg="syntax error - commas"       &
                    // " are not valid deliminators", rcToReturn=rc)
           return
        endif

        !-----------------------------------------------------------------------
        ! create work space for parsing the source descriptor string
        !-----------------------------------------------------------------------
        allocate( lsrc(src_mem_rank+1), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="type "//                &
           " lsrc in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( grid_order(src_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " grid_order in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( grid_type(src_mem_rank), stat=allocRcToTest )      
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="char variable "//       &
           " grid_type in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( grid_HaloL(src_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " grid_HaloL in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( grid_HaloR(src_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " grid_HaloR in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( grid_StagLoc(src_mem_rank), stat=allocRcToTest )    
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " grid_StagLoc in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( dist_order(src_mem_rank), stat=allocRcToTest )      
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " dist_order in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( dist_type(src_mem_rank), stat=allocRcToTest )      
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="char variable "//       &
           " dist_type in parse_descriptor_string", rcToReturn=rc)) then
        endif

        allocate( pds%SrcMem%GridType(src_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="char variable "//       &
           " GridType in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( pds%SrcMem%DistType(src_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="char variable "//       &
           " DistType in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( pds%SrcMem%GridOrder(src_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " GridOrder in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( pds%SrcMem%DistOrder(src_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " DistOrder in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( pds%SrcMem%HaloL(src_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " HaloL in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( pds%SrcMem%HaloR(src_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " HaloR in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( pds%SrcMem%StagLoc(src_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " StagLoc in parse_descriptor_string", rcToReturn=rc)) then
        endif

        !-----------------------------------------------------------------------
        ! partition the source descriptor string into separate parts which
        ! correspond to memory locations and parse the substrings for 
        ! grid and distribution descriptions.
        !-----------------------------------------------------------------------
        call memory_separate( src_string, src_mem_rank, lsrc, localrc) 
        if( CheckError(checkpoint, __LINE__, __FILE__, localrc,"syntax error in SRC portion " //    &
                "of problem descriptor string - memory separate " //           &
                 trim(adjustL(lstring)), rcToReturn=rc) ) return
        call interpret_descriptor_string( lsrc, src_mem_rank,                  & 
                 grid_rank, grid_order, grid_type, grid_HaloL, grid_HaloR,     &
                 grid_StagLoc, dist_rank, dist_order, dist_type, localrc)

        if( CheckError(checkpoint, __LINE__, __FILE__, localrc,"syntax error in SRC portion " //    &
                "of problem descriptor string - interpret string " //          &
                trim(adjustL(lstring)), rcToReturn=rc) ) return

        pds%SrcMem%GridRank = grid_rank
        pds%SrcMem%DistRank = dist_rank

        do k=1,pds%SrcMem%memRank 
           pds%SrcMem%GridType(k)%string = grid_type(k)%string
           pds%SrcMem%DistType(k)%string = dist_type(k)%string

           pds%SrcMem%GridOrder(k) = grid_order(k)
       ! These are getting corrupted when tensor dimensions are specified 
           pds%SrcMem%DistOrder(k) = dist_order(k)
           pds%SrcMem%HaloL(k)     = grid_HaloL(k)
           pds%SrcMem%HaloR(k)     = grid_HaloR(k)
           pds%SrcMem%StagLoc(k)   = grid_StagLoc(k)
        enddo

        deallocate( lsrc )
        deallocate( grid_order, grid_type, grid_HaloL, grid_HaloR )
        deallocate( grid_StagLoc )    
        deallocate( dist_order, dist_type )      

        !-----------------------------------------------------------------------
        ! create work space for parsing the destination descriptor string
        !-----------------------------------------------------------------------
        allocate( ldst(dst_mem_rank+1), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="type "//                &
           " ldst in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( grid_order(dst_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " grid_order in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( grid_type(dst_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="char variable "//       &
           " grid_type in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( grid_HaloL(dst_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " grid_HaloL in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( grid_HaloR(dst_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " grid_HaloR in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( grid_StagLoc(dst_mem_rank), stat=allocRcToTest )    
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " grid_StagLoc in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( dist_order(dst_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " dist_order in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( dist_type(dst_mem_rank), stat=allocRcToTest )      
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="char variable "//       &
           " dist_type in parse_descriptor_string", rcToReturn=rc)) then
        endif

        allocate( pds%DstMem%GridOrder(dst_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " GridOrder in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( pds%DstMem%DistOrder(dst_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " DistOrder in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( pds%DstMem%GridType(dst_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="char variable "//       &
           " GridType in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( pds%DstMem%DistType(dst_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="char variable "//       &
           " DistType in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( pds%DstMem%HaloL(dst_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " HaloL in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( pds%DstMem%HaloR(dst_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " HaloR in parse_descriptor_string", rcToReturn=rc)) then
        endif
        allocate( pds%DstMem%StagLoc(dst_mem_rank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " StagLoc in parse_descriptor_string", rcToReturn=rc)) then
        endif

        !-----------------------------------------------------------------------
        ! partition the destination descriptor string into separate parts
        ! which correspond to memory locations and parse the substrings 
        ! for grid and distribution descriptions.
        !-----------------------------------------------------------------------
        call memory_separate( dst_string, dst_mem_rank, ldst, localrc) 
        if( CheckError(checkpoint, __LINE__, __FILE__, localrc,"syntax error in SRC portion " //    &
                "of problem descriptor string - memory separate " //           &
                trim(adjustL(lstring)), rcToReturn=rc) ) return
        call interpret_descriptor_string( ldst, dst_mem_rank,                  & 
                grid_rank, grid_order, grid_type, grid_HaloL, grid_HaloR,      &
                grid_StagLoc, dist_rank, dist_order, dist_type, localrc)
        if( CheckError(checkpoint, __LINE__, __FILE__, localrc,"syntax error in SRC portion " //    &
                "of problem descriptor string - interpret string " //          &
                trim(adjustL(lstring)), rcToReturn=rc) ) return

        pds%DstMem%GridRank = grid_rank
        pds%DstMem%DistRank = dist_rank

        do k=1,pds%DstMem%memRank 
           pds%DstMem%GridType(k)%string  = grid_type(k)%string
           pds%DstMem%DistType(k)%string  = dist_type(k)%string

           pds%DstMem%GridOrder(k) = grid_order(k)
           pds%DstMem%DistOrder(k) = dist_order(k)

           pds%DstMem%HaloL(k)     = grid_HaloL(k)
           pds%DstMem%HaloR(k)     = grid_HaloR(k)
           pds%DstMem%StagLoc(k)   = grid_StagLoc(k)
        enddo

        deallocate( ldst )
        deallocate( grid_order, grid_type, grid_HaloL, grid_HaloR )
        deallocate( grid_StagLoc )    
        deallocate( dist_order, dist_type )      
     else  ! error does not conform to either single block or multiblock
        localrc = ESMF_FAILURE
        call ESMF_LogSetError(ESMF_FAILURE,msg="syntax error - problem "        &
                 // " descriptor string does not conform to either " //        &
                 "single block syntax or to multiblock syntax",                &
                 rcToReturn=rc)
        return
     endif
  else   ! error does not conform to either single block or multiblock
     localrc = ESMF_FAILURE
     call ESMF_LogSetError(ESMF_FAILURE,msg="syntax error - problem "           &
              // " descriptor string does not conform to either " //           &
              "single block syntax or to multiblock syntax",                   &
              rcToReturn=rc)
     return
  endif

  !-----------------------------------------------------------------------------
  ! if I've gotten this far without an error, then the routine has succeeded.
  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine parse_descriptor_string
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  subroutine interpret_descriptor_string(lstring, nstring,                     &
                                         grid_rank, grid_order, grid_type,     &
                                         HaloL, HaloR, StaggerLoc,             &
                                         dist_rank, dist_order, dist_type,     &
                                         localrc)
  !-----------------------------------------------------------------------------
  ! This routine parses a part ( either source or destination) of a problem
  ! descriptor string for for descriptions of the memory topology and rank,
  ! the grid rank, order, halo and stagger, and the distribution type, rank,
  ! and order. The routine assumes the string has beeen partitioned so that
  ! each memory slot is stored in an element of a character array.
  !-----------------------------------------------------------------------------

  ! arguments
  type(character_array), intent(in   ) :: lstring(:)
  integer,               intent(in   ) :: nstring 
  integer,               intent(  out) :: grid_rank, grid_order(:)
  integer,               intent(  out) :: dist_rank, dist_order(:)
  type(character_array), intent(  out) :: grid_type(:), dist_type(:)
  integer,               intent(  out) :: HaloL(:), HaloR(:)
  integer,               intent(  out) :: StaggerLoc(:)
  integer,               intent(  out) :: localrc


  ! local variables
  character(THARN_MAXSTR) :: ltmp, lstagger, intstr
  integer :: k, n, kstring, rank, halo, ndelim
  integer :: iloc(1), mloc(1)
  integer :: hbeg, hmid, hend, sbeg, send, slen
  integer :: itmp, itmp_beg, itmp_end
  integer, allocatable ::  sdelim(:)
  integer, allocatable ::  assoc_grid(:)
  integer :: allocRcToTest


  !-----------------------------------------------------------------------------
  ! initialize return variable
  !-----------------------------------------------------------------------------
  localrc = ESMF_RC_NOT_IMPL 

  !-----------------------------------------------------------------------------
  ! work array
  !-----------------------------------------------------------------------------
  allocate( assoc_grid(nstring), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg=" type "//                     &
     " assoc_grid in interpret descriptor string", rcToReturn=localrc)) then
  endif
  assoc_grid = 0

  !-----------------------------------------------------------------------------
  ! Determine grid layout (rank and order)
  !-----------------------------------------------------------------------------
  grid_rank = 0 
  do kstring=1, nstring
     rank = set_query(lstring(kstring)%string, 'GU') 
     if( rank == 0 ) then
     ! no associated grid
        grid_type(kstring)%string = ' '
        grid_order(kstring) = 0
     elseif( rank == 1 ) then
     ! associated grid
        grid_rank = grid_rank +  rank
        call set_locate(lstring(kstring)%string, 'GU', rank , mloc)
        grid_type(kstring)%string = lstring(kstring)%string(mloc(1):mloc(1))
        read( lstring(kstring)%string(mloc(1)+1:mloc(1)+1), *) grid_order(kstring)
        !  keep track of associated dimensions
        assoc_grid( grid_order(kstring) ) =  -1

        halo = set_query(lstring(kstring)%string, 'H') 
        if( halo == 1 ) then
        !-----------------------------------------------------------------------
        ! halo is specified, now check that the syntax is correct
        !-----------------------------------------------------------------------
           halo = set_query(lstring(kstring)%string, '{:}') 
           if( halo == 3 ) then
              itmp = 1
              call pattern_locate(lstring(kstring)%string, 'H{', itmp, iloc)    
              hbeg = iloc(1)
              if( itmp /= 1) then
                 !syntax error in halo specification
                 call ESMF_LogSetError( ESMF_FAILURE,                       &
                         msg="halo specification missing prefix",                  &
                         rcToReturn=localrc)
                 return
              endif

              call set_locate(lstring(kstring)%string, ':', itmp, iloc)    
              hmid = iloc(1)
              if( itmp /= 1) then
                 !syntax error in halo specification
                 call ESMF_LogSetError( ESMF_FAILURE,                       &
                         msg="halo specification missing separator",               &
                         rcToReturn=localrc)
                 return
              endif

              call set_locate(lstring(kstring)%string, '}', itmp, iloc)    
              hend = iloc(1)
              if( itmp /= 1) then
                 !syntax error in halo specification
                 call ESMF_LogSetError( ESMF_FAILURE,                       &
                         msg="halo specification missing suffix",                  &
                         rcToReturn=localrc)
                 return
              endif
              !-----------------------------------------------------------------
              ! halo syntax is correct, now read in the halo values as characters 
              ! and convert then to integer values.
              !-----------------------------------------------------------------
              intstr = adjustL( lstring(kstring)%string(hmid-1:hbeg+2) )
              read (intstr, *) HaloL(kstring)
              intstr = adjustL( lstring(kstring)%string(hend-1:hmid+1) )
              read (intstr, *) HaloR(kstring)
  ! drs begug
              ! currently halo must be symmetric and non-negative
              if( HaloR(kstring) < 0 .or. HaloL(kstring) < 0 .or.              &
                  HaloL(kstring) /= HaloR(kstring)  )                          &
                  call ESMF_LogSetError( ESMF_FAILURE,                      &
                      msg="halo specification "//trim(lstring(kstring)%string) //  &
                      " is not symmetric and/or is negative ",                 &
                      rcToReturn=localrc)
                 return
           else
           ! syntax error for halo specification
              call ESMF_LogSetError( ESMF_FAILURE,                          &
                      msg="halo specification "//trim(lstring(kstring)%string),    &
                      rcToReturn=localrc)
              return
           endif   ! halo

        elseif( halo == 0 ) then 
        ! no halo 
           HaloL(kstring) = 0
           HaloR(kstring) = 0
        else
        ! syntax error for halo specification
           HaloL(kstring) = 0
           HaloR(kstring) = 0
           call ESMF_LogSetError( ESMF_FAILURE,                             &
                   msg="halo specification wrong "//trim(lstring(kstring)%string), &
                   rcToReturn=localrc)
           return
        endif    ! halo

     else 
     ! error multiple grid specifications for single memory location
        call ESMF_LogSetError( ESMF_FAILURE,                                &
                msg="multiple grid specifications for single memory location " //  &
                trim(lstring(kstring)%string), rcToReturn=localrc)
        return
     endif

  enddo    !  kstring

  !-----------------------------------------------------------------------------
  ! fill in the sizes for the tensor dimensions
  !-----------------------------------------------------------------------------
  if( nstring > grid_rank ) then
     n = nstring
     do k=grid_rank+1, nstring
        do while( assoc_grid(n) == -1 )
           n = n-1
        enddo   ! while
        grid_order(k) = n 
     enddo
  endif

  !-----------------------------------------------------------------------------
  ! initialize stagger location
  !-----------------------------------------------------------------------------
  do k=1,grid_rank
     staggerloc(k) = 0
  enddo

  !-----------------------------------------------------------------------------
  ! determine if the stagger location is specified by the problem descriptor
  ! string. Look for trailing tag of the form "@{#,#,#}" following the 
  ! closing "]". If it exists it will be placed in the memory_rank+1 element 
  ! of the character array.
  !-----------------------------------------------------------------------------

  itmp_beg = pattern_query(lstring(nstring+1)%string, ']@{')
  if( itmp_beg == 1 ) then
     call pattern_locate( lstring(nstring+1)%string, ']@{', itmp_beg, iloc)
     sbeg = iloc(1)
     slen = len( trim( lstring(nstring+1)%string ) )

     ! extract the stagger substring for further parsing
     ltmp = trim( adjustL( lstring(nstring+1)%string(sbeg+2:slen) ))
     itmp_end = pattern_query(ltmp, '}')

     if( itmp_end == 1 ) then
        call pattern_locate( ltmp, '}', itmp_end, iloc )
        send = iloc(1)
        lstagger = trim( adjustL( ltmp(2:send) ))

        ! determine the number of entries
        ndelim = pattern_query( lstagger, ',')

        if( ndelim >= 1 .and. ndelim <= grid_rank-1 ) then   
           ! identify the separate entries, check that they are not empty,
           ! and read the values
           allocate( sdelim(ndelim), stat=allocRcToTest )
           if (ESMF_LogFoundAllocError(allocRcToTest, msg=" integer "//         &
              "variable sdelim in interpret descriptor string",                &
              rcToReturn=localrc)) then
           endif

           call pattern_locate( lstagger, ',', ndelim, sdelim)

           if(  sdelim(1)-1 >= 1) then
              intstr = adjustL( lstagger( 1:sdelim(1)-1 ) ) 
              read(intstr, *) staggerloc(1)
           else
              ! specification empty
              call ESMF_LogSetError( ESMF_FAILURE,                          &
                      msg="stagger location specification empty ",                 &
                       rcToReturn=localrc)
              return
           endif
        
           do k=2,ndelim
              if(  sdelim(k)-1 > sdelim(k-1) ) then
                 intstr = adjustL( lstagger( sdelim(k-1)+1:sdelim(k)-1) ) 
                 read(intstr, *) staggerloc(k)
              else
                 ! specification empty
                 call ESMF_LogSetError( ESMF_FAILURE,                       &
                         msg="stagger location specification empty ",              &
                         rcToReturn=localrc)
                 return
              endif
           enddo     ! ndelim

           send = len( trim( adjustL( lstagger ) ) )
           if(  send-1 >= sdelim(ndelim) ) then
              intstr = adjustL( lstagger( send-1:sdelim(ndelim)+1 ) ) 
              read(intstr, *) staggerloc(ndelim+1)
           else
           ! specification empty
              call ESMF_LogSetError( ESMF_FAILURE,                          &
                      msg="stagger location specification empty ",                 &
                      rcToReturn=localrc)
              return
           endif
           ! clean up workspace
           deallocate( sdelim )

        else    
        ! wrong number of delimiters for grid rank
           call ESMF_LogSetError( ESMF_FAILURE,                             &
                   msg="wrong number of delimiters for grid rank",                 &
                   rcToReturn=localrc)
           return
        endif    ! number of delimiters
     else
     ! error missing ending delimiter
        call ESMF_LogSetError( ESMF_FAILURE,                                &
                msg="missing stagger location ending delimitor from string",       &
                   rcToReturn=localrc)
        return
     endif     ! proper ending syntax
  elseif( itmp_beg == 0 ) then
  ! no stagger assumption, assume cell center location
     do k=1,grid_rank
        staggerloc(k) = 0
     enddo
  else
  ! syntax error
     call ESMF_LogSetError( ESMF_FAILURE,                                   &
             msg="problem descriptor string syntax error, strings ends with " //   &
             trim(lstring(nstring+1)%string), rcToReturn=localrc)
     return
  endif     ! proper starting syntax

  !-----------------------------------------------------------------------------
  ! check stagger location for acceptable values
  !-----------------------------------------------------------------------------
  do k=1,grid_rank
     if( staggerloc(k) /= 0 .and. staggerloc(k) /= 1 ) then
        ! error invalid staggerlocs
        call ESMF_LogSetError( ESMF_FAILURE,                                &
                msg="invalid stagger locations from problem descriptor string",    &
                 rcToReturn=localrc)
        return
     endif
  enddo

  !-----------------------------------------------------------------------------
  ! Determine Distribution layout (rank and order)
  !-----------------------------------------------------------------------------
  dist_rank = 0
  do kstring=1, nstring
     rank = set_query(lstring(kstring)%string, 'BCA') 
     if( rank == 0 ) then
     ! no associated distribution
        dist_type(kstring)%string = ' '
        dist_order(kstring) = 0
     elseif( rank == 1 ) then
     ! associated grid
        dist_rank = dist_rank +  rank
        call set_locate(lstring(kstring)%string, 'BCA', rank , mloc)
        dist_type(kstring)%string = lstring(kstring)%string(mloc(1):mloc(1))
        read( lstring(kstring)%string(mloc(1)+1:mloc(1)+1), *) dist_order(kstring)
     else 
        ! error multiple distribution specifications for single memory location
        call ESMF_LogSetError( ESMF_FAILURE,                                &
                msg="multiple distribution specifications in single memory" //     &
                "location", rcToReturn=localrc)
        return
     endif

  enddo    !  kstring

  !-----------------------------------------------------------------------------
  ! clean up 
  !-----------------------------------------------------------------------------
  deallocate( assoc_grid )

  !-----------------------------------------------------------------------------
  ! if I've gotten this far without an error, then the routine has succeeded.
  !-----------------------------------------------------------------------------
  localrc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine interpret_descriptor_string
  !-----------------------------------------------------------------------------
!===============================================================================
  end module ESMF_TestHarnessParser
!===============================================================================

