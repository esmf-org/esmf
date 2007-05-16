!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_TestHarnessMod"
!
!  ESMF Test Harness Module
   module ESMF_TestHarnessMod
!
!==============================================================================
!
! This file contains parameters, global data types, and parser 
! functions/subroutines for the Testing Harness.
! These methods are used by the test harness driver ESMF_TestHarnessUTest.F90.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!==============================================================================
!BOPI
! !MODULE: ESMF_TestHarnessMod
!
! !DESCRIPTION:
!
! The code in this file contains data types and basic functions for the
! {\tt ESMF\_TestHarness}.  
!
!------------------------------------------------------------------------------
! !USES:

  use ESMF_Mod

  implicit none

  ! Process Parameters
  integer, parameter :: Harness_Error            = 0
  integer, parameter :: Harness_Redist           = 1
  integer, parameter :: Harness_BilinearRemap    = 2
  integer, parameter :: Harness_ConservRemap     = 3
  integer, parameter :: Harness_2ndConservRemap  = 4
  integer, parameter :: Harness_ExchangeRemap    = 5
  integer, parameter :: Harness_NearNeighRemap   = 6
  integer, parameter :: Harness_UserProvRemap    = 7

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


!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!  private
  type name_record
     integer :: value
     character(ESMF_MAXSTR) :: descriptor
     character(ESMF_MAXSTR) :: flags
  end type name_record

  type process_record
     character(ESMF_MAXSTR) :: name
     integer :: tag                   ! process tag
     integer :: location              ! string location of method
  end type process_record

  type memory_record
     character(ESMF_MAXSTR) :: name
     integer :: chunks               ! number of logically rectangular chunks
     integer, allocatable :: rank
  end type memory_record

  type grid_record
     character(ESMF_MAXSTR) :: name
     integer :: topology                     ! key representing the geometry of the grid
     integer :: rank                         ! rank of the grid
     integer, allocatable :: order(:)        ! axis number, zero for free.
     integer, allocatable :: size(:)         ! number of grid elements along axis
     integer, allocatable :: stagger(:,:)    ! stagger location (axis,rank)
     logical, allocatable :: periodicity(:)  ! (rank) periodicity along axis
     logical, allocatable :: halo(:)         ! (rank) periodicity along axis
     integer, allocatable :: halo_size(:,:)  ! (rank,2) halo size along axis
  end type grid_record

  type dist_record
     character(ESMF_MAXSTR) :: name
     integer :: topology                ! key (simple block, block-cyclic, arbitrary)
     integer :: rank                    ! rank of distribution
     integer, allocatable :: order(:)   ! axis number, zero for free.
     integer, allocatable :: size(:)    ! number of DE for axis number, zero for free.
     integer, allocatable :: period(:)  ! period for block-cyclic, zero for simple block
  end type dist_record



!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:
  public

  ! character types
  type character_array
     character(ESMF_MAXSTR) :: name
  end type character_array

  ! sized char type
  type sized_char_array
     integer :: size
     type(character_array), allocatable :: string(:)
  end type sized_char_array

  type problem_descriptor_record
     character(ESMF_MAXSTR) :: string   ! problem descriptor string
     type(process_record) :: process    ! method process
     type(sized_char_array) :: distfiles   ! distribution specification files
     type(sized_char_array) :: gridfiles   ! grid specification files
     type(memory_record) :: src_memory        ! memory topology
     type(memory_record) :: dst_memory        ! memory topology
     type(dist_record) :: src_dist            ! distribution topology
     type(dist_record) :: dst_dist            ! distribution topology
     type(grid_record) :: src_grid            ! grid topology
     type(grid_record) :: dst_grid            ! grid topology
  end type problem_descriptor_record

!
!==============================================================================

  contains 

!==============================================================================

  subroutine TestHarnessBlank
  end subroutine TestHarnessBlank
!------------------------------------------------------------------------------


  end module ESMF_TestHarnessMod
