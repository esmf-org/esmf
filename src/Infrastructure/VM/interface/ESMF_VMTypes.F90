! $Id: ESMF_VMTypes.F90,v 1.1 2004/12/14 15:38:19 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_VM.F90"
!==============================================================================
!
! ESMF VM Module
module ESMF_VMTypesMod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the VM and VMPlan types
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_VMTypesMod - The VM (virtual machine) types.
!
! !DESCRIPTION:
!   F90 types and interface blocks for the most basic VM functions.
!   The rest of the VM code is in ESMF_VM.F90
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_BaseTypesMod                       ! ESMF most basic objects
      
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
      
!------------------------------------------------------------------------------
!     ! ESMF_CommHandle
!
! TODO: This needs to be filled with life once we work on non-blocking 
!     
!     ! Shallow sync/async communications type.  Mirrored on C++ side.
!     ! Contains a place to hold
!     ! the MPI handle in the case of nonblocking MPI calls.  The wait
!     ! parameter controls whether the "IsComplete" call blocks/waits
!     ! or simply tests and returns.
      
  type ESMF_CommHandle
  sequence
  !private
#ifndef ESMF_NO_INITIALIZERS
    type(ESMF_Pointer) :: this = ESMF_NULL_POINTER
#else
    type(ESMF_Pointer) :: this
#endif
!    integer :: dummy  !so compiler is satisfied for now...
!    integer :: mpi_handle  ! mpi returns this for async calls
!    integer :: wait        ! after an async call, does query block?
  end type
      
  integer, parameter :: ESMF_TEST_COMPLETE = 1, ESMF_WAIT_COMPLETE = 2

!------------------------------------------------------------------------------

  ! F90 class type to hold pointer to C++ object
  type ESMF_VM
    sequence
    !private
      type(ESMF_Pointer) :: this
  end type

  ! F90 class type to hold pointer to C++ object
  type ESMF_VMPlan
    sequence
    !private
      type(ESMF_Pointer) :: this
  end type

!------------------------------------------------------------------------------

  ! Module parameters
  integer, parameter:: ESMF_PREF_INTRA_PROCESS_SHMHACK  = 0 !default
  integer, parameter:: ESMF_PREF_INTRA_PROCESS_PTHREAD  = 1

  integer, parameter:: ESMF_PREF_INTRA_SSI_POSIXIPC     = 0
  integer, parameter:: ESMF_PREF_INTRA_SSI_MPI1         = 1 !default
      
  integer, parameter:: ESMF_PREF_INTER_SSI_MPI1         = 0 !default
  
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_CommHandle
  public ESMF_VM
  public ESMF_VMPlan
      
!------------------------------------------------------------------------------
! !PUBLIC PARAMETERS:
      
  public ESMF_TEST_COMPLETE, ESMF_WAIT_COMPLETE
  public ESMF_PREF_INTRA_PROCESS_SHMHACK
  public ESMF_PREF_INTRA_PROCESS_PTHREAD
  public ESMF_PREF_INTRA_SSI_POSIXIPC
  public ESMF_PREF_INTRA_SSI_MPI1
  public ESMF_PREF_INTER_SSI_MPI1

!------------------------------------------------------------------------------
! !INTERFACE BLOCKS:

! the minimal set of interfaces which must be used before the actual VM
! code is compiled

end module ESMF_VMTypesMod

#if 0
module ESMF_VMMod
  public ESMF_VMGetGlobal, ESMF_VMGet
  interface

    subroutine ESMF_VMGetGlobal(vm, rc)
      use ESMF_VMTypesMod
      type(ESMF_VM), intent(out)            :: vm
      integer,       intent(out), optional  :: rc           
    end subroutine

    subroutine ESMF_VMGet(vm, localPet, petCount, peCount, mpiCommunicator, &
                          okOpenMpFlag, rc)
      use ESMF_BaseTypesMod
      use ESMF_VMTypesMod
      type(ESMF_VM),      intent(in)              :: vm
      integer,            intent(out),  optional  :: localPet
      integer,            intent(out),  optional  :: petCount
      integer,            intent(out),  optional  :: peCount
      integer,            intent(out),  optional  :: mpiCommunicator
      type(ESMF_Logical), intent(out),  optional  :: okOpenMpFlag
      integer,            intent(out),  optional  :: rc
    end subroutine

  end interface

end module
#endif

