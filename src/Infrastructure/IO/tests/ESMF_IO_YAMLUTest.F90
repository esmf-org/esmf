! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_IO_YAMLUTest

!------------------------------------------------------------------------------

#define ESMF_FILENAME "ESMF_IO_YAMLUTest.F90"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_IO_YAMLUTest -  Tests ESMF YAML I/O API.
!
! !DESCRIPTION:
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF

  implicit none

!-------------------------------------------------------------------------
!=========================================================================

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
  integer :: result = 0
  logical :: success

  ! local variables
  integer            :: rc
  integer            :: i, ipos, epos, clength, length, lineCount
  type(ESMF_IO_YAML) :: yaml
  character(len=800), dimension(:), allocatable :: content

  character(len=*), parameter :: nl = char(10)
  character(len=*), parameter :: test_yaml = "unit_test_content:"//nl &
                                           //"  - key1: value1"  //nl &
                                           //"  - key2: value2"  //nl

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! check if ESMF was built with YAML support
#if ESMF_YAMLCPP
  write(6,'(2x,"INFO",2x,"ESMF was built with YAML support via the external yaml-cpp library")')
#else
  write(6,'(2x,"INFO",2x,"ESMF was built with NO YAML support")')
#endif
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! create IO_YAML object
  rc = ESMF_FAILURE
  yaml = ESMF_IO_YAMLCreate(rc=rc)
  write(failMsg, *) "did not return ESMF_SUCCESS"
  write(name, *) "ESMF_IO_YAMLCreate(): create YAML_IO object"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! read YAML file
  rc = ESMF_FAILURE
  call ESMF_IO_YAMLRead(yaml, "fd.yaml", rc=rc)
  write(name, *) "ESMF_IO_YAMLRead(): read in YAML file"
#if ESMF_YAMLCPP
  write(failMsg, *) "did not return ESMF_SUCCESS"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! write YAML to standard output
  rc = ESMF_FAILURE
  call ESMF_IO_YAMLWrite(yaml, rc=rc)
  write(name, *) "ESMF_IO_YAMLWrite(): write native YAML content to standard output"
#if ESMF_YAMLCPP
  write(failMsg, *) "did not return ESMF_SUCCESS"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! generate content from YAML
  rc = ESMF_FAILURE
  call ESMF_IO_YAMLContentInit(yaml, cflag=ESMF_IOYAML_CONTENT_NATIVE, rc=rc)
  write(name, *) "ESMF_IO_YAMLContentInit(): generate native content from YAML"
#if ESMF_YAMLCPP
  write(failMsg, *) "did not return ESMF_SUCCESS"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! check content - get line count
  lineCount = 0
  rc = ESMF_FAILURE
  call ESMF_IO_YAMLContentGet(yaml, lineCount=lineCount, rc=rc)
  write(failMsg, *) "did not return ESMF_SUCCESS"
  write(name, *) "ESMF_IO_YAMLContentGet(): get content line count"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! check content - get content
  allocate(content(lineCount))
  content = ""
  rc = ESMF_FAILURE
  call ESMF_IO_YAMLContentGet(yaml, content=content, rc=rc)
  write(failMsg, *) "did not return ESMF_SUCCESS"
  write(name, *) "ESMF_IO_YAMLContentGet(): get content"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  do i = 1, lineCount
    print *,i,trim(content(i))
  end do
  deallocate(content)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! now properly parse YAML
  rc = ESMF_FAILURE
  call ESMF_IO_YAMLParse(yaml, parseflag=ESMF_IOYAML_PARSE_NUOPCFD, rc=rc)
  write(name, *) "ESMF_IO_YAMLParse(): parse YAML as NUOPC Field dictionary"
#if ESMF_YAMLCPP
  write(failMsg, *) "did not return ESMF_SUCCESS"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! re-generate content from YAML
  rc = ESMF_FAILURE
  call ESMF_IO_YAMLContentInit(yaml, cflag=ESMF_IOYAML_CONTENT_FREEFORM, rc=rc)
  write(name, *) "ESMF_IO_YAMLParse(): generate FreeFormat content from YAML"
#if ESMF_YAMLCPP
  write(failMsg, *) "did not return ESMF_SUCCESS"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMC_RC_ARG_INCOMP"
  call ESMF_Test((rc==ESMC_RC_ARG_INCOMP), name, failMsg, result, ESMF_SRCLINE)
#endif

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! check content
  lineCount = 0
  rc = ESMF_FAILURE
  call ESMF_IO_YAMLContentGet(yaml, lineCount=lineCount, rc=rc)
  write(failMsg, *) "did not return ESMF_SUCCESS"
  write(name, *) "ESMF_IO_YAMLContentGet(): get FreeFormat content line count"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! check content
  allocate(content(lineCount))
  content = ""
  rc = ESMF_FAILURE
  call ESMF_IO_YAMLContentGet(yaml, content=content, rc=rc)
  write(failMsg, *) "did not return ESMF_SUCCESS"
  write(name, *) "ESMF_IO_YAMLContentGet(): get FreeFormat content"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  do i = 1, lineCount
    print *,i,trim(content(i))
  end do
  deallocate(content)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! ingest new YAML into YAML_IO object
  rc = ESMF_FAILURE
  call ESMF_IO_YAMLIngest(yaml, test_yaml, rc=rc)
  write(name, *) "ESMF_IO_YAMLIngest(): ingest YAML content"
#if ESMF_YAMLCPP
  write(failMsg, *) "did not return ESMF_SUCCESS"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! generate content from ingested YAML
  rc = ESMF_FAILURE
  call ESMF_IO_YAMLContentInit(yaml, cflag=ESMF_IOYAML_CONTENT_NATIVE, rc=rc)
  write(name, *) "ESMF_IO_YAMLContentInit(): generate native content from YAML"
#if ESMF_YAMLCPP
  write(failMsg, *) "did not return ESMF_SUCCESS"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! check content - get length
  clength = 0
  rc = ESMF_FAILURE
  call ESMF_IO_YAMLContentGet(yaml, contentSize=clength, rc=rc)
  write(failMsg, *) "did not return ESMF_SUCCESS"
  write(name, *) "ESMF_IO_YAMLContentGet(): get native content size (length)"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! check content - get line count
  lineCount = 0
  rc = ESMF_FAILURE
  call ESMF_IO_YAMLContentGet(yaml, lineCount=lineCount, rc=rc)
  write(failMsg, *) "did not return ESMF_SUCCESS"
  write(name, *) "ESMF_IO_YAMLContentGet(): get native content line count"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! check content - get content
  allocate(content(lineCount))
  content = ""
  rc = ESMF_FAILURE
  call ESMF_IO_YAMLContentGet(yaml, content=content, rc=rc)
  write(failMsg, *) "did not return ESMF_SUCCESS"
  write(name, *) "ESMF_IO_YAMLContentGet(): get native content"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! check content - verify length: method #1
  write(failMsg, *) "content length is different"
  write(name, *) "ESMF_IO_YAMLContentGet(): verify content size (1)"
#if ESMF_YAMLCPP
  length = 0
  do i = 1, lineCount
    length = length + len_trim(content(i)) + 1
  end do
  success = (length==clength)
#else
  success = .true.
#endif
  call ESMF_Test(success, name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! check content - verify length: method #2
  write(failMsg, *) "content length is different"
  write(name, *) "ESMF_IO_YAMLContentGet(): verify content size (2)"
#if ESMF_YAMLCPP
  success = (length == len(test_yaml))
#else
  success = .true.
#endif
  call ESMF_Test(success, name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! check content - verify content
  ! now check content
#if ESMF_YAMLCPP
  print *,"  Ingested content:"
  print *,"  -----------------"
  print *,test_yaml
  print *,"  Generated content:"
  print *,"  -----------------"
  do i = 1, lineCount
    print *,i,trim(content(i))
  end do
  print *,""

  i = 0
  epos = 0
  ipos = 0
  success = .true.
  do while (success .and. (i < lineCount))
    i = i + 1
    ipos = epos + 1
    success = (ipos <= length)
    if (success) then
      epos = index(test_yaml(ipos:),char(10))
      success = (epos > 0)
    end if
    if (success) then
      epos = epos + ipos - 1
      success = &
        (test_yaml(ipos:epos-1) == content(i)(1:epos-ipos))
    end if
  end do
  deallocate(content)
#else
  success = .true.
#endif
  write(failMsg, *) "generated native ontent does not match ingested YAML content"
  write(name, *) "ESMF_IO_YAMLContentGet(): verify retrieved YAML content"
  call ESMF_Test(success, name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! re-generate content from YAML -- this test should always FAIL
  rc = ESMF_FAILURE
  call ESMF_IO_YAMLContentInit(yaml, cflag=ESMF_IOYAML_CONTENT_FREEFORM, rc=rc)
  write(name, *) "ESMF_IO_YAMLContentInit(): generate incompatible FreeFormat content from YAML"
  write(failMsg, *) "did not return ESMC_RC_ARG_INCOMP"
  call ESMF_Test((rc==ESMC_RC_ARG_INCOMP), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! destroy IO_YAML object
  rc = ESMF_FAILURE
  call ESMF_IO_YAMLDestroy(yaml, rc=rc)
  write(failMsg, *) "did not return ESMF_SUCCESS"
  write(name, *) "ESMF_IO_YAMLDestroy(): destroy ESMF IO_YAML object"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !-----------------------------------------------------------------------------

  end program ESMF_IO_YAMLUTest
