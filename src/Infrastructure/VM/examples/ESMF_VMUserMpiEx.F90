! $Id: ESMF_VMUserMpiEx.F90,v 1.1.2.2 2006/06/19 21:54:02 theurich Exp $
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

!==============================================================================
!EXAMPLE        String used by test script to count examples.
!==============================================================================

!------------------------------------------------------------------------------
!BOE
!
! \subsubsection{VMGet MPI Communicator Example}
!
! The following example code demonstrates how ESMF can be used inside of a 
! user application that explicitly calls MPI\_Init() and MPI\_Finalize().
!
!EOE
!------------------------------------------------------------------------------

program ESMF_VMUserMpiEx

  use ESMF_Mod
  
  implicit none
  !include 'mpif.h'
  
  ! local variables
  integer:: rc, ierr
  ! result code
  integer :: finalrc
  finalrc = ESMF_SUCCESS
  ! user code initializes MPI
  call MPI_Init(ierr)
  if (ierr/=0) finalrc = ESMF_FAILURE
  ! user code initializes ESMF
  call ESMF_Initialize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  ! user code finalizes ESMF
  call ESMF_Finalize(terminationflag=ESMF_KEEPMPI, rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  ! user code finalizes MPI
  call MPI_Finalize(ierr)
  if (ierr/=0) finalrc = ESMF_FAILURE
  ! print result
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_VMUserMpiEx.F90"
  else
    print *, "FAIL: ESMF_VMUserMpiEx.F90"
  endif
end program
