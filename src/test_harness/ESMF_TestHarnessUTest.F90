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
! use harness_mod
  implicit none

  ! cumulative result: count failures; no failure equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc, finalrc

  ! local args needed to create/construct objects
  type(ESMF_RouteHandle) :: redist_rh
  type(ESMF_Grid)        :: grid(2)
  type(ESMF_Field)       :: sfield(10), dfield(10)
  type(ESMF_Bundle)      :: bundle(2)
  type(ESMF_VM)          :: vm
  type(ESMF_Config)      :: cf

  ! local variables
  integer:: localPet, petCount
  character(ESMF_MAXSTR) :: ltest_class, ltag
  character(ESMF_MAXSTR), allocatable :: ldescriptor_file(:)
  integer :: line, nlines, ncolumns
  logical :: flag

  ! -------- beginning of executable code below here -------
  !   !Set finalrc to success
  finalrc = ESMF_SUCCESS

  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  print *, "hi from harness"
  ! get global vm information
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

  call ESMF_VMGet(vm, localPet, petCount=petCount, rc=rc)
  if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

  !
  ! create a config handle and load the testing harness config file
  cf = ESMF_ConfigCreate(rc)
  call ESMF_ConfigLoadFile(cf,'./test_harness.rc', rc=rc)
  if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
  !
  call ESMF_ConfigFindLabel(cf,'test_class:', rc=rc)
  call ESMF_ConfigGetAttribute(cf, ltest_class, rc=rc)
  print*,'test class',trim( ltest_class )
  !
#ifdef ESMF_EXHAUSTIVE
  ltag = 'exhaustive::'
  print *, "exh"
#else
  print *, "nexh"
  ltag = 'nonexhaustive::'
#endif
  call ESMF_ConfigGetDim(cf, nlines, ncolumns, ltag, rc=rc)
  print*,'nlines',nlines
  call ESMF_ConfigFindLabel(cf, ltag, rc=rc)
  allocate( ldescriptor_file(nlines) )
  do line=1,nlines
     ! advance to new line in table
     flag = .true.
     call ESMF_ConfigNextLine(cf, flag , rc=rc)
     ! get string label for configuration
     call ESMF_ConfigGetAttribute(cf, ldescriptor_file(line), rc=rc)
     print*,'descriptor file name', trim( ldescriptor_file(line) )
  enddo   ! lines

  call ESMF_TestEnd(result, ESMF_SRCLINE)

  ! -------- end of unit test code ------------------------

  end program ESMF_Test_Harness
