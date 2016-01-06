! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
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
  use ESMF

  use ESMF_TestHarnessTypesMod
  use ESMF_TestHarnessMod
  use ESMF_TestHarnessParser
  use ESMF_TestHarnessReportMod

  implicit none

  logical,parameter :: checkpoint = .FALSE.

  ! individual test result code
  integer :: rc
  integer :: localrc

  ! local args needed to create/construct objects
  type(ESMF_VM)          :: vm

  ! local variables
  integer :: result = 0  ! cumulative result of failures; 0 failures => "all pass"

  character(THARN_MAXSTR) :: name, failmsg

  ! test harness config file path & top level config file name
  integer                :: argc
  integer                :: argindex

  character(THARN_MAXSTR)  :: srcPath(1)
  character(THARN_MAXSTR)  :: configFname(1)
  character(THARN_MAXSTR)  :: xmlFname(1)
  integer                :: runFlag(1)
  integer                :: xmlFlag(1)

  ! -------- beginning of executable code below here -------

  !allocate (dummystuff(5))
  name = ""
  failmsg = ""
  har%failures = 0

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  !------------------------------------------------------------------------

  ! get global vm information
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS)                                                      &
                          call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS)                                                      &
                          call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! -----------------------------------------
  ! command arg processing
  if (localPet .eq. 0) then
    ! init defaults
    srcPath(1) = "."
    configFname(1) = "test_harness.rc"
    xmlFname(1) = "Summary.xml"
    runFlag(1) = 1
    xmlFlag(1) = 0

    ! get arg cnt
    call ESMF_UtilGetArgC (argc)

    print '("command line arg count = ", I4)', argc

    do argindex = 1, argc
      call ESMF_UtilGetArg (argindex=argindex, argvalue=name, rc=localrc)
      if (CheckError (checkpoint, __LINE__, __FILE__, localrc, "ESMF_UtilGetArg failure", rc)) go to 90
        print '("command line arg [", I2, "] = ", I4, ":", A)', argindex, LEN_TRIM(name), TRIM(name)
    end do

    ! get path info
    call ESMF_UtilGetArgIndex (argvalue="-path", argindex=argindex, rc=localrc)
    if (CheckError (checkpoint, __LINE__, __FILE__, localrc, "ESMF_UtilGetArgIndex failure", rc)) go to 90
    print '("argindex (-path) = ", I4)', argindex
    if ((argindex >= 0) .AND. (argindex < argc - 1)) then
      call ESMF_UtilGetArg (argindex=argindex+1, argvalue=srcPath(1), rc=localrc)
      if (CheckError (checkpoint, __LINE__, __FILE__, localrc, "ESMF_UtilGetArg failure", rc)) go to 90
    end if

    ! get test case info
    call ESMF_UtilGetArgIndex (argvalue="-case", argindex=argindex, rc=localrc)
    if (CheckError (checkpoint, __LINE__, __FILE__, localrc, "ESMF_UtilGetArgIndex failure", rc)) go to 90
    print '("argindex (-case) = ", I4)', argindex
    if ((argindex >= 0) .AND. (argindex < argc)) then
      call ESMF_UtilGetArg (argindex=argindex+1, argvalue=configFname(1), rc=localrc)
      if (CheckError (checkpoint, __LINE__, __FILE__, localrc, "ESMF_UtilGetArg failure", rc)) go to 90
    end if

    ! get xml file info
    call ESMF_UtilGetArgIndex (argvalue="-xml", argindex=argindex, rc=localrc)
    if (CheckError (checkpoint, __LINE__, __FILE__, localrc, "ESMF_UtilGetArgIndex failure", rc)) go to 90
    print '("argindex (-xml) = ", I4)', argindex
    if ((argindex >= 0) .AND. (argindex < argc)) then
      call ESMF_UtilGetArg (argindex=argindex+1, argvalue=xmlFname(1), rc=localrc)
      if (CheckError (checkpoint, __LINE__, __FILE__, localrc, "ESMF_UtilGetArg failure", rc)) go to 90
      xmlFlag(1) = 1
    end if

    ! get no run info
    call ESMF_UtilGetArgIndex (argvalue="-norun", argindex=argindex, rc=localrc)
    if (CheckError (checkpoint, __LINE__, __FILE__, localrc, "ESMF_UtilGetArgIndex failure", rc)) go to 90
    if ((argindex >= 0) .AND. (argindex <= argc)) then
      runFlag(1) = 0
    end if

!error check

    srcPath(1) = adjustL(srcPath(1))
    configFname(1) = adjustL(configFname(1))
    xmlFname(1) = adjustL(xmlFname(1))

    print '("Path Length = ", I5)', LEN_TRIM(srcPath(1))
    print '("Path = ", A)', trim(srcPath(1))
    print '("Config File = ", A)', trim(configFname(1))
    print '("Run Flag = ", I2)', runFlag(1)
    print '("XML Flag = ", I2)', xmlFlag(1)

    if (xmlFlag(1) .ne. 0) then
      print '("XML File = ", A)', trim(xmlFname(1))
    end if
  end if  ! PET 0 command line processing

  ! broadcast command line args to all PETS
! how many characters
  call ESMF_VMBroadcast (vm, bcstData=srcPath, count=THARN_MAXSTR, rootPet=0, rc=localrc)
  if (CheckError (checkpoint, __LINE__, __FILE__, localrc, "Broadcast Failure - runFlag", rc)) go to 90

  call ESMF_VMBroadcast (vm, bcstData=configFname, count=THARN_MAXSTR, rootPet=0, rc=localrc)
  if (CheckError (checkpoint, __LINE__, __FILE__, localrc, "Broadcast Failure - runFlag", rc)) go to 90

  call ESMF_VMBroadcast (vm, bcstData=xmlFname, count=THARN_MAXSTR, rootPet=0, rc=localrc)
  if (CheckError (checkpoint, __LINE__, __FILE__, localrc, "Broadcast Failure - runFlag", rc)) go to 90

  call ESMF_VMBroadcast (vm, bcstData=runFlag, count=1, rootPet=0, rc=localrc)
  if (CheckError (checkpoint, __LINE__, __FILE__, localrc, "Broadcast Failure - runFlag", rc)) go to 90

  call ESMF_VMBroadcast (vm, bcstData=xmlFlag, count=1, rootPet=0, rc=localrc)
  if (CheckError (checkpoint, __LINE__, __FILE__, localrc, "Broadcast Failure - runFlag", rc)) go to 90


    print '(I4, "Path = ", A)', localPet, trim(srcPath(1))
    print '(I4, "Config File = ", A)', localPet, trim(configFname(1))
    print '(I4, "Run Flag = ", I2)', localPet, runFlag(1)
    print '(I4, "XML Flag = ", I2)', localPet, xmlFlag(1)
    if (xmlFlag(1) .ne. 0) then
      print '(I4, "XML File = ", A)', localPet, trim(xmlFname(1))
    end if

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
  call Read_TestHarness_Config(srcPath(1), configFname(1), rc)
  if (rc /= ESMF_SUCCESS)  then
     print '("Error reading file ", A, " - see log file")', trim(configFname(1))
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
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
  call Read_TestHarness_Specifier(SrcPath(1), rc)
  if (rc /= ESMF_SUCCESS)  then
     print*,"Error reading descriptor and specifier files - see log"
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
     stop
  endif

  ! print summary report for current harness config
  if ((xmlFlag(1) .ne. 0) .and. (localPet .eq. 0)) then
    call summary_report_generate (har, xmlFname(1), rc)
  endif

  !-----------------------------------------------------------------------------
  ! Now that the test configuration have been read successfully, conduct the
  ! tests.
  !-----------------------------------------------------------------------------
  if (runFlag(1) .ne. 0) then
    call RunTests(rc)
    if (rc /= ESMF_SUCCESS) then
       print*,'FAIL - one or more test harness tests have failed - see stdout ', &
            'for complete details'
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif
  else
    ! no tests were run
    print *,'FAIL - -norun option selected on command line'
    har%failures = 1
  end if

  !-----------------------------------------------------------------------------
  ! if everything has been successful up until now, return success
  ! IMPORTANT: TestGlobal() prints the string "PASS:" that the testing scripts 
  ! look for.
  !-----------------------------------------------------------------------------
  result = har%failures
  rc = ESMF_SUCCESS
  if( result > 0 )  then
    rc = ESMF_FAILURE
    write(failMsg, *) "Test Harness Failure - see stdout file for details. "
  end if

!
! end of test wrap-up
!   rc must be set to ESMF error status code (e.g. ESMF_ SUCCESS, ESMF_FAILURE, etc)
!   gather aggregate error count?
!
90 continue
  name = "Harness Test  for class " // adjustL(har%testClass)
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
  call ESMF_TestEnd(ESMF_SRCLINE) 
  !------------------------------------------------------------------------


  ! -------- end of unit test code ------------------------

!===============================================================================

contains

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
                 if (ESMF_LogFoundError(localrc,msg=" error array redist test", &
                     rcToReturn=returnrc)) return
              else
                 !--------------------------------------------------------------
                 ! run the array test with ssm
                 !--------------------------------------------------------------
!                call array_regrid_test(har%rcrd(kfile)%str(kstr),             &
!                                  har%failures, har%reportType, VM, localrc)
!                if (ESMF_LogFoundError(localrc," error array ssm test",    &
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
                 if (ESMF_LogFoundError(localrc,msg=" error field redist test", &
                     rcToReturn=returnrc)) return
              else
                 !--------------------------------------------------------------
                 ! run the field test with regrid
                 !--------------------------------------------------------------
                 call field_regrid_test(har%rcrd(kfile)%str(kstr),             &
                           har%failures, har%reportType, VM, localrc)
                 if (ESMF_LogFoundError(localrc,msg=" error field regrid test", &
                     rcToReturn=returnrc)) return
              endif
           enddo  ! kstr
        enddo    ! kfile

!    case("FIELDBUNDLE")
!    return

     case default
     ! error - class unknown
     call ESMF_LogSetError( ESMF_FAILURE,msg=" test class of unknown type",     &
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


