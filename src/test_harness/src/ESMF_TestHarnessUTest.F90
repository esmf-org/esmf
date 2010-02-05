! $Id: ESMF_TestHarnessUTest.F90,v 1.25.2.1 2010/02/05 22:35:15 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
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
#include "ESMF.h"

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

  use ESMF_TestHarnessTypesMod
  use ESMF_TestHarnessMod

  use ESMF_TestHarnessGridMod
  use ESMF_TestHarnessDistMod

  use ESMF_TestHarnessReportMod


  implicit none

  ! individual test result code
  integer :: rc

  ! local args needed to create/construct objects
  type(ESMF_VM)          :: vm

  ! global storage of test specification
  type (harness_descriptor), save :: har

  ! local variables
  integer :: localPet, petCount, rootPet = Harness_rootPet
  integer :: result = 0  ! cumulative result of failures; 0 failures => "all pass"

  character(ESMF_MAXSTR) :: name, failmsg

  ! logical :: localdebugflag = .false.

  ! top level test harness config file
  character(ESMF_MAXSTR) :: test_harness_name = "test_harness.rc"

  ! -------- beginning of executable code below here -------

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  !------------------------------------------------------------------------

  ! get global vm information
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS)                                                      &
                          call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS)                                                      &
                          call ESMF_Finalize(terminationflag=ESMF_ABORT)


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
  call Read_TestHarness_Config(rc)
  if (rc /= ESMF_SUCCESS)  then
     print*,"Error reading file test_harness.rc - see log file"
     call ESMF_Finalize(terminationflag=ESMF_ABORT)
     stop
  endif

  !-----------------------------------------------------------------------------
  ! announce beginning of test
  !-----------------------------------------------------------------------------
  if (localPet .eq. 0) then
      print *, "--------------------------------------- "
      print *, 'Start of test harness test for class ',trim(adjustL(har%testClass))
      print *, "--------------------------------------- "
  endif

  !-----------------------------------------------------------------------------
  ! Prepare to conduct the test runs by specifying complete tests
  ! 1. Reads the problem descriptor files, storing each problem descriptor string
  ! and the names of the accompaning support files in the harness descriptor record.
  ! 2. Parse the descriptor strings and store the information in the harness 
  ! descriptor record.
  ! 3. Read the grid and distribution specifier files and store the configurations
  ! in the harness descriptor record.
  !-----------------------------------------------------------------------------
  call Read_TestHarness_Specifier(rc)
  if (rc /= ESMF_SUCCESS)  then
     print*,"Error reading descriptor and specifier files - see log"
     call ESMF_Finalize(terminationflag=ESMF_ABORT)
     stop
  endif

  !-----------------------------------------------------------------------------
  ! Now that the test configuration have been read successfully, conduct the
  ! tests.
  !-----------------------------------------------------------------------------
  call RunTests(rc)
  if (rc /= ESMF_SUCCESS) then
     print*,'FAIL - one or more test harness tests have failed - see stdout ', &
            'for complete details'
     call ESMF_Finalize(terminationflag=ESMF_ABORT)
  endif

  !-----------------------------------------------------------------------------
  ! if everything has been successful up until now, return success
  ! IMPORTANT: TestGlobal() prints the string "PASS:" that the testing scripts 
  ! look for.
  !-----------------------------------------------------------------------------
  result = har%failures
  rc = ESMF_SUCCESS
  if( result > 0 )  then
    rc = ESMF_FAILURE
  endif
  write(failMsg, *) "Test Harness Failure - see stdout file for details. "
  write(name, *) "Harness Test  for class " // trim(adjustL(har%testClass))
  call ESMF_TestGlobal((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
    ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  ! announce end of test
  !-----------------------------------------------------------------------------
  if (localPet .eq. 0) then
      print *, "--------------------------------------- "
      print *, 'End of harness test for class ',trim(adjustL(har%testClass))
      print *, "--------------------------------------- "
  endif

  !------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE) 
  !------------------------------------------------------------------------


  ! -------- end of unit test code ------------------------

!===============================================================================

contains

!===============================================================================
! !IROUTINE: Read_TestHarness_Config

! !INTERFACE:
  subroutine Read_TestHarness_Config(returnrc)
!
! !ARGUMENTS:
  integer, intent(inout) :: returnrc

!
! !DESCRIPTION:
! Routine opens the top level config file "test_harness.rc", which specifies the 
! test class, the reporting style, and depending on how the ESMF_TESTEXHAUSTIVE
! flag is set, extracts the list of files containing the problem descriptor
! strings.
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
  character(ESMF_MAXSTR) :: ltag, ltmp

  ! local integer variables
  integer :: kfile, ncolumns
  integer :: localrc
  integer :: allocRcToTest 

  ! local  logical
  logical :: flag = .true.

  ! initialize return code
  returnrc = ESMF_RC_NOT_IMPL
  localrc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! create config handle and load the testing harness config file
  !-----------------------------------------------------------------------------
  localcf = ESMF_ConfigCreate(rc=localrc)
  if( ESMF_LogMsgFoundError(localrc, "cannot create config object",            &
                            rcToReturn=returnrc) ) return
                            
  call ESMF_ConfigLoadFile(localcf, trim(adjustL(test_harness_name)),          &
           rc=localrc )
  if( ESMF_LogMsgFoundError(localrc, "cannot load config file " //             &
      trim(adjustL(test_harness_name)), rcToReturn=returnrc) ) return
  
  !-----------------------------------------------------------------------------
  ! find and read the test class 
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(adjustL(test_class_name)), rc=localrc) 
  if( ESMF_LogMsgFoundError(localrc, "cannot find config label " //            &
      trim(adjustL(test_class_name)),rcToReturn=returnrc) ) return

  call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc )
  har%testClass = trim(adjustL( ltmp ))
  if( ESMF_LogMsgFoundError(localrc, "cannot get value for label " //          &
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
     call ESMF_LogMsgSetError( ESMF_FAILURE,"class name not of valid type "//  &
          trim(adjustL(har%testClass)), rcToReturn=returnrc)
     return
  endif

  !-----------------------------------------------------------------------------
  ! determine type of test report and toggle setup report
  !-----------------------------------------------------------------------------

  call ESMF_ConfigFindLabel(localcf,trim(adjustL(setup_report_name)),rc=localrc)
  if( ESMF_LogMsgFoundError(localrc,"cannot find config label " //             &
      trim(adjustL(setup_report_name)), rcToReturn=returnrc) ) return

  call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
  har%setupReportType = trim(adjustL( ltmp ))
  if( ESMF_LogMsgFoundError(localrc,  "cannot get value for label " //         &
      trim(adjustL(setup_report_name)), rcToReturn=returnrc) ) return

  if((har%setupReportType /= "TRUE").and.(har%setupReportType /= "FALSE")) then
     call ESMF_LogMsgSetError( ESMF_FAILURE, "setup report flag " //           &
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
  if( ESMF_LogMsgFoundError(localrc, "cannot find config label " //            &
      trim(adjustL(test_report_name)), rcToReturn=returnrc) ) return

  call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc )
  har%reportType = trim(adjustL( ltmp ))
  if( ESMF_LogMsgFoundError(localrc, "cannot get value for label " //          &
      trim(adjustL(test_report_name)), rcToReturn=returnrc) ) return

  if ( har%reportType /= "FULL" .and. har%reportType /= "FAILURE" .and.        &
       har%reportType /= "SUCCESS" .and. har%reportType /= "NONE" ) then
     call ESMF_LogMsgSetError( ESMF_FAILURE, "report flag improperly set" //   &
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
  if( ESMF_LogMsgFoundError(localrc, "cannot find config label " //            &
      trim(adjustL(ltag)), rcToReturn=returnrc) ) return

  ! determine the number of entries
  call ESMF_ConfigGetDim(localcf, har%numRecords, ncolumns,                    &
       trim(adjustL(ltag)), rc=localrc)
  if( ESMF_LogMsgFoundError(localrc, "cannot find the size of the table " //   &
      trim(adjustL(ltag)), rcToReturn=returnrc) ) return

  ! if there are no entries post an error
  if ( har%numRecords .le. 0 ) then
     call ESMF_LogMsgSetError( ESMF_FAILURE, "no problem descriptor files "//  &
          "specified", rcToReturn=returnrc)
     return
  endif

  !-----------------------------------------------------------------------------
  ! find the problem descriptor file names and read them
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(adjustL(ltag)), rc=localrc)
  if( ESMF_LogMsgFoundError(localrc, "cannot find table label of " //          &
      trim(adjustL(ltag)), rcToReturn=returnrc) ) return

  !-----------------------------------------------------------------------------
  ! allocate space to hold problem descriptor filenames and advance through the
  ! table extracting the problem descriptor filenames
  !-----------------------------------------------------------------------------
  allocate( har%rcrd(har%numRecords), stat=allocRcToTest )
  if (ESMF_LogMsgFoundAllocError(allocRcToTest, "rcrd type "//                 &
     " in Read_TestHarness_Config", rcToReturn=rc)) then
  endif


  do kfile=1,har%numRecords
     ! advance to new line in table
     call ESMF_ConfigNextLine(localcf, tableEnd=flag, rc=localrc)
     if( ESMF_LogMsgFoundError(localrc, "cannot advance to next line of " //   &
         "table " // trim(adjustL(ltag)), rcToReturn=returnrc) ) return
 
     ! retrieve the problem descriptor filenames 
     call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
     har%rcrd(kfile)%filename  = trim(adjustL( ltmp ))
     if( ESMF_LogMsgFoundError(localrc, "cannot get descriptor filename in "// &
         trim(adjustL(ltag)), rcToReturn=returnrc) ) return

  enddo   ! file

  !-----------------------------------------------------------------------------
  ! clean up CF
  !-----------------------------------------------------------------------------
  call ESMF_ConfigDestroy(localcf, rc=localrc)
  if( ESMF_LogMsgFoundError(localrc, "cannot destroy config file " //          &
      trim(test_harness_name), rcToReturn=returnrc) ) return

  ! if I've gotten this far without an error, then the routine has succeeded.
  returnrc = ESMF_SUCCESS

!===============================================================================
  end subroutine Read_TestHarness_Config
!===============================================================================

!-------------------------------------------------------------------------------
 
!===============================================================================
! !IROUTINE: Read_TestHarness_Specifier

! !INTERFACE:
  subroutine Read_TestHarness_Specifier(returnrc)
!
! !ARGUMENTS:
  integer, intent(inout) :: returnrc

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
  call read_descriptor_files(har%numRecords,har%rcrd,localrc)
  if (ESMF_LogMsgFoundError(localrc, "ERROR with read descriptor files",       &
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
        if (ESMF_LogMsgFoundError(localrc," error in problem descriptor file " &
           // trim(adjustL(har%rcrd(kfile)%filename)),                         &
           rcToReturn=returnrc)) return  

        ! read distribution specifier files
        do k=1,har%rcrd(kfile)%str(kstr)%nDfiles
           nPEs = petCount
           call read_dist_specification(nPEs,                                  &
                    har%rcrd(kfile)%str(kstr)%Dfiles(k),                       &
                    har%rcrd(kfile)%str(kstr)%DstMem,                          &
                    har%rcrd(kfile)%str(kstr)%SrcMem, localrc)       
           if (ESMF_LogMsgFoundError(localrc," error reading dist specifier"   &
              // " file "  //                                                  &
              trim(adjustL(har%rcrd(kfile)%str(kstr)%Dfiles(k)%filename)),     &
              rcToReturn=returnrc)) return
        enddo   ! k

        ! read grid specifier files
        do k=1,har%rcrd(kfile)%str(kstr)%nGfiles
           call read_grid_specification(har%rcrd(kfile)%str(kstr)%Gfiles(k),   &
                    localrc)
           if (ESMF_LogMsgFoundError(localrc," error reading grid specifier"   &
              // " file "  //                                                  &
              trim(adjustL(har%rcrd(kfile)%str(kstr)%Gfiles(k)%filename)),     &
              rcToReturn=returnrc)) return
        enddo   ! k

        ! allocate and initialize test status
        nDfiles = har%rcrd(kfile)%str(kstr)%nDfiles
        nGfiles = har%rcrd(kfile)%str(kstr)%nGfiles
        allocate( har%rcrd(kfile)%str(kstr)%test_record(nDfiles,nGfiles),      &
                  stat=allocRcToTest )
        if (ESMF_LogMsgFoundAllocError(allocRcToTest, "rcrd type "//           &
           " in Read_TestHarness_Config", rcToReturn=rc)) then
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
           if (ESMF_LogMsgFoundAllocError(allocRcToTest, "test status type"//  &
              " in Read_TestHarness_Config", rcToReturn=rc)) then
           endif
           ! allocate work space for test string
           allocate( har%rcrd(kfile)%str(kstr)%test_record(iDfile,iGfile)%     &
              test_string(nDspec,nGspec), stat=allocRcToTest )
           if (ESMF_LogMsgFoundAllocError(allocRcToTest, "test status type"//  &
              " in Read_TestHarness_Config", rcToReturn=rc)) then
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
! !IROUTINE: RunTests         

! !INTERFACE:
  subroutine RunTests(returnrc)
!
! !ARGUMENTS:
  integer, intent(  out) :: returnrc

!
! !DESCRIPTION:
! This routine controls the execution of the test harness tests
!-------------------------------------------------------------------------------
  ! local variables
  integer :: kfile, kstr
  integer :: localrc

  ! initialize local rc
  returnrc = ESMF_RC_NOT_IMPL
  localrc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! select type of test result report
  !-----------------------------------------------------------------------------
  select case (trim(adjustL(har%testClass)))
     !--------------------------------------------------------------------------
     ! Field Tests
     !--------------------------------------------------------------------------
     case("ARRAY")
        do kfile=1,har%numRecords
           do kstr=1,har%rcrd(kfile)%numStrings
              !-----------------------------------------------------------------
              ! test of a single problem descriptor string w/ all specifiers
              !-----------------------------------------------------------------
              if(har%rcrd(kfile)%str(kstr)%process%tag == Harness_Redist) then
                 !--------------------------------------------------------------
                 ! run the array test with redist 
                 !--------------------------------------------------------------
                 call array_redist_test(har%rcrd(kfile)%str(kstr),har%failures,&
                                     har%reportType, VM, localrc)
                 if (ESMF_LogMsgFoundError(localrc," error array redist test", &
                     rcToReturn=returnrc)) return
              else
                 !--------------------------------------------------------------
                 ! run the array test with ssm
                 !--------------------------------------------------------------
!                call array_regrid_test(har%rcrd(kfile)%str(kstr),             &
!                                  har%failures, har%reportType, VM, localrc)
!                if (ESMF_LogMsgFoundError(localrc," error array ssm test",    &
!                    rcToReturn=returnrc)) return
              endif
           enddo  ! kstr
        enddo    ! kfile

!    case("ARRAYBUNDLE")
!    return

     !--------------------------------------------------------------------------
     ! Field Tests
     !--------------------------------------------------------------------------
     case("FIELD")
        do kfile=1,har%numRecords
           do kstr=1,har%rcrd(kfile)%numStrings
              !-----------------------------------------------------------------
              ! test of a single problem descriptor string w/ all specifiers
              !-----------------------------------------------------------------
              if(har%rcrd(kfile)%str(kstr)%process%tag == Harness_Redist) then
                 !--------------------------------------------------------------
                 ! run the field test with redist 
                 !--------------------------------------------------------------
                 call field_redist_test(har%rcrd(kfile)%str(kstr),             &
                           har%failures, har%reportType, VM, localrc)
                 if (ESMF_LogMsgFoundError(localrc," error field redist test", &
                     rcToReturn=returnrc)) return
              else
                 !--------------------------------------------------------------
                 ! run the field test with regrid
                 !--------------------------------------------------------------
                 call field_regrid_test(har%rcrd(kfile)%str(kstr),             &
                           har%failures, har%reportType, VM, localrc)
                 if (ESMF_LogMsgFoundError(localrc," error field regrid test", &
                     rcToReturn=returnrc)) return
              endif
           enddo  ! kstr
        enddo    ! kfile

!    case("FIELDBUNDLE")
!    return

     case default
     ! error - class unknown
     call ESMF_LogMsgSetError( ESMF_FAILURE," test class of unknown type",     &
               rcToReturn=returnrc)
     return

  end select  ! case of testclass

  !-----------------------------------------------------------------------------
  ! if I've gotten this far without an error, then the routine has succeeded.
  !-----------------------------------------------------------------------------
  returnrc = ESMF_SUCCESS


  !-----------------------------------------------------------------------------
  end subroutine RunTests
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------
end program ESMF_Test_Harness
!-------------------------------------------------------------------------------


