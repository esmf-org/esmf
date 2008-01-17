!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!===============================================================================
#define ESMF_FILENAME "ESMF_TestHarnessDistMod"
!
!  ESMF Test Harness DistGrid Utility Module
   module ESMF_TestHarnessDistMod
!
!===============================================================================
!
! These methods are used by the test harness driver ESMF_TestHarnessUTest.F90.
!
!-------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!===============================================================================
!BOPI
! !MODULE: ESMF_TestHarnessDistMod
!
! !DESCRIPTION:
!
! The code in this module contains data types and basic functions for accessing
! distribution specifications needed for the {\tt ESMF\_TestHarness}.
!
!-------------------------------------------------------------------------------
! !USES:

  use ESMF_Mod
  use ESMF_TestHarnessMod

  implicit none

!-------------------------------------------------------------------------------
! !PRIVATE TYPES:

!-------------------------------------------------------------------------------
! !PUBLIC TYPES:

  type dist_record
     character(ESMF_MAXSTR) :: name
     integer :: topology           ! key (simple block, block-cyclic, arbitrary)
     integer :: rank               ! rank of distribution
     integer, pointer :: order(:)  ! axis number, zero for free.
     integer, pointer :: size(:)   ! number of DE for axis number, zero for free.
     integer, pointer :: period(:) ! period for block-cyclic, zero for simple block
     integer, pointer :: specifier(:,:)  ! 
  end type dist_record

  public dist_record
!-------------------------------------------------------------------------------
! PUBLIC METHODS:
  public read_dist_specification

!
!===============================================================================

  contains 

!===============================================================================


  !-----------------------------------------------------------------------------
  subroutine read_dist_specification(lfilename, localrc)
  !-----------------------------------------------------------------------------
  ! driver for reading the distribution specifier files and constructing the 
  ! esmf distgrid object.
  !
  !-----------------------------------------------------------------------------

  ! arguments
  character(ESMF_MAXSTR), intent(in   ) :: lfilename
  integer, intent(  out) :: localrc

  ! local ESMF types
  type(ESMF_Config)      :: localcf

  ! local parameters

  ! local character strings
  character(ESMF_MAXSTR) :: ltmp, lchar

  ! local integer variables

  ! local real variables

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! open the distribution file
  !-----------------------------------------------------------------------------
  localcf = ESMF_ConfigCreate(localrc)
  if( ESMF_LogMsgFoundError(localrc, "cannot create config object",           &
                            rcToReturn=localrc) ) return

  print*,'Opening Dist specifier file  ',trim( lfilename )
  call ESMF_ConfigLoadFile(localcf, trim( lfilename ), rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                          &
         "cannot load config file " // trim( lfilename ),                     &
         rcToReturn=localrc) ) return

  !----------------------------------------------------------------------------
  ! Search and extract the DistGrid type specifier
  !----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, 'dist_type:', rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                          &
         "cannot find config label dist_type", rcToReturn=localrc) ) return  

  call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
  if( ESMF_LogMsgFoundError(localrc,                                          &
         "cannot read config label dist_type:" , rcToReturn=localrc) ) return

  !-----------------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  localrc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------
  end subroutine read_dist_specification
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------


!===============================================================================
  end module ESMF_TestHarnessDistMod
!===============================================================================

