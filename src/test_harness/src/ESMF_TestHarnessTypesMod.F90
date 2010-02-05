! $Id: ESMF_TestHarnessTypesMod.F90,v 1.9.2.1 2010/02/05 22:35:15 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!===============================================================================
#define ESMF_FILENAME "ESMF_TestHarnessMod"
!
!  ESMF Test Harness Types Module
   module ESMF_TestHarnessTypesMod
!
!===============================================================================
!
! This file contains parameters, global data types, and parser 
! functions/subroutines for the Testing Harness.
! These methods are used by the test harness driver ESMF_TestHarnessUTest.F90
! and all the other test harness modules.
!
!-------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!===============================================================================
!BOPI
! !MODULE: ESMF_TestHarnessTypesMod
!
! !DESCRIPTION:
!
! The code in this file contains data types and basic functions for the
! {\tt ESMF\_TestHarness}.  
!
!-------------------------------------------------------------------------------
! !USES:

  use ESMF_Mod

  implicit none

  ! Debug flag
  logical, parameter :: debug_flag = .false.

  ! Error Parameters 
  integer, parameter :: Harness_Max_Size         = 9000  ! largest single
                                                         ! dimension size
                                                         ! allowed
  ! VM parameters
  integer, parameter :: Harness_rootPet          = 0
  ! Process Parameters
  integer, parameter :: Harness_Error            = 0
  integer, parameter :: Harness_Redist           = 1
  integer, parameter :: Harness_BilinearRegrid   = 2
  integer, parameter :: Harness_PatchRegrid      = 3
  integer, parameter :: Harness_ConservRegrid    = 4
  integer, parameter :: Harness_2ndConservRegrid = 5
  integer, parameter :: Harness_ExchangeRegrid   = 6
  integer, parameter :: Harness_NearNeighRegrid  = 7
  integer, parameter :: Harness_UserProvRegrid   = 8

  ! Distribution Parameters
  integer, parameter :: Harness_DistError        = 100
  integer, parameter :: Harness_BlockDist        = 101
  integer, parameter :: Harness_CyclicDist       = 102
  integer, parameter :: Harness_ArbitraryDist    = 103

  ! Grid Parameters
  integer, parameter :: Harness_GridError        = 200
  integer, parameter :: Harness_TensorGrid       = 201
  integer, parameter :: Harness_SphericalGrid    = 202
  integer, parameter :: Harness_UnstructuredGrid = 203

  ! Test Result Parameters
  integer, parameter :: HarnessTest_SUCCESS      = 1000
  integer, parameter :: HarnessTest_FAILURE      = 1001
  integer, parameter :: HarnessTest_UNDEFINED    = 1002

  ! Physical and Mathematical Parameters
  real(ESMF_KIND_R8), parameter :: zero   = 0.0
  real(ESMF_KIND_R8), parameter :: one    = 1.0
  real(ESMF_KIND_R8), parameter :: two    = 2.0
  real(ESMF_KIND_R8), parameter :: three  = 3.0
  real(ESMF_KIND_R8), parameter :: four   = 4.0
  real(ESMF_KIND_R8), parameter :: five   = 5.0
  real(ESMF_KIND_R8), parameter :: half   = 0.5
  real(ESMF_KIND_R8), parameter :: quart  = 0.25
  real(ESMF_KIND_R8), parameter :: bignum = 1.e+20
  real(ESMF_KIND_R8), parameter :: tiny   = 1.e-14
  real(ESMF_KIND_R8), parameter :: pi     = 3.14159265359
  real(ESMF_KIND_R8), parameter :: pi2    = two*pi
  real(ESMF_KIND_R8), parameter :: pih    = half*pi
  real(ESMF_KIND_R8), parameter :: DtoR   = pi/180.0
  real(ESMF_KIND_R8), parameter :: RtoD   = 180.0/pi

!
!-------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public

  ! character types
  type character_array
     character(ESMF_MAXSTR) :: string
  end type character_array

  ! sized char type
  type sized_char_array
     integer :: tagsize
     type(character_array), pointer :: tag(:)
  end type sized_char_array

!-------------------------------------------------------------------------------
!
  type name_record
     integer :: value
     character(ESMF_MAXSTR) :: descriptor
     character(ESMF_MAXSTR) :: flags
  end type name_record

  type test_record   
     integer, pointer :: test_status(:,:)  ! status of test
     type(character_array), pointer :: test_string(:,:)
  end type test_record

  type test_function_record   
     character(ESMF_MAXSTR) ::string 
     integer :: prank                            ! rank of parameters 
      real(ESMF_KIND_R8), pointer :: param(:)     ! test function parameters
  end type test_function_record   

  type process_record
     character(ESMF_MAXSTR) ::string 
     integer :: tag                   ! process tag
     integer :: location              ! string location of method
  end type process_record


!-------------------------------------------------------------------------------
!  Distribution Types

  type dist_specification_record
     integer :: drank                            ! rank of the distribution
     integer, pointer :: dsize(:)                ! num of dist elements along axis
  end type dist_specification_record

  type dist_record
     character(ESMF_MAXSTR) :: filename   ! grid specifier filename
     integer :: nDspecs                   ! number of grid spec records
     type(dist_specification_record), pointer :: src_dist(:)
     type(dist_specification_record), pointer :: dst_dist(:)
  end type dist_record


!-------------------------------------------------------------------------------
!  Grid Types

  type grid_specification_record
     integer :: grank                            ! rank of the grid
     type(character_array), pointer :: gtype(:)  ! type of grid spacing
     type(character_array), pointer :: gunits(:) ! physical grid units
     integer, pointer :: gsize(:)                ! num of grid elements along axis
     real(ESMF_KIND_R8), pointer :: grange(:,:)  ! physical range of axes
  end type grid_specification_record

  type grid_record
     character(ESMF_MAXSTR) :: filename   ! grid specifier filename
     integer :: nGspecs                   ! number of grid spec records
     type(grid_specification_record), pointer :: src_grid(:)
     type(grid_specification_record), pointer :: dst_grid(:)
     type(test_function_record), pointer ::  testfunction(:)
  end type grid_record

!-------------------------------------------------------------------------------
!  Memory Types

  type memory_config
     character(ESMF_MAXSTR) :: string    ! memory string 
     integer :: memRank                  ! rank of memory chunk
     integer :: DistRank                 ! rank of distribution
     integer :: GridRank                 ! rank of grid
     integer, pointer :: DistOrder(:)
     integer, pointer :: GridOrder(:)
     type(character_array), pointer :: DistType(:)
     type(character_array), pointer :: GridType(:)
     integer, pointer :: HaloL(:)
     integer, pointer :: HaloR(:)
     integer, pointer :: StagLoc(:)
  end type memory_config

  type problem_descriptor_strings
     character(ESMF_MAXSTR) :: pds         ! problem descriptor string
     type(test_record), pointer :: test_record(:,:) ! test status of the config
                                                    ! (nDfiles,nGfiles)
     type(process_record) :: process       ! method process
     type(memory_config) :: DstMem         ! destination memory configuration
     type(memory_config) :: SrcMem         ! source memory configuration
     !
     type(sized_char_array) :: classfile  ! class specification files
     !
     integer :: nDfiles                       ! number of distribution  spec files
     type(dist_record), pointer :: Dfiles(:)  ! distribution specification files
     !
     integer :: nGfiles                       ! number of grid spec files
     type(grid_record), pointer :: Gfiles(:)  ! grid specification files
  end type problem_descriptor_strings

  type problem_descriptor_records
     character(ESMF_MAXSTR) :: filename   ! filename of problem descriptor record
     integer :: numStrings                ! # of problem descriptor strings in record
     type(problem_descriptor_strings), pointer :: str(:)  ! problem descriptor strngs
  end type problem_descriptor_records

  type harness_descriptor
     character(ESMF_MAXSTR) :: testClass       ! test class
     character(ESMF_MAXSTR) :: reportType      ! test result report type 
     character(ESMF_MAXSTR) :: setupReportType ! setup report type 
     integer :: numRecords                     ! number of problem descriptor filenames
     type(problem_descriptor_records), pointer :: rcrd(:)  ! problem descriptor recd 
     integer :: failures                       ! number of test failures
  end type harness_descriptor


!===============================================================================
  end module ESMF_TestHarnessTypesMod
!===============================================================================
