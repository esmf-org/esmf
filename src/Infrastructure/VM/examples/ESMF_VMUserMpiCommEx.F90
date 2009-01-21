! $Id: ESMF_VMUserMpiCommEx.F90,v 1.4.2.3 2009/01/21 21:25:24 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!------------------------------------------------------------------------------
!BOE
!
! \subsubsection{ESMF inside user defined MPI communicator}
!
! The following example code demonstrates how ESMF can run inside of a 
! user defined MPI communicator.
!
!EOE
!------------------------------------------------------------------------------

program ESMF_VMUserMpiCommEx

  use ESMF_Mod
  
  implicit none
#ifndef ESMF_MPIUNI     
  include 'mpif.h'
#endif
  
  ! local variables
  integer:: rc
#ifndef ESMF_MPIUNI     
  integer:: ierr
#endif
  integer:: esmfComm, rank
  ! result code
  integer :: finalrc
  finalrc = ESMF_SUCCESS
  ! user code initializes MPI
#ifndef ESMF_MPIUNI     
!BOC
  call MPI_Init(ierr)
!EOC
  if (ierr/=0) finalrc = ESMF_FAILURE
!BOC
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
!EOC
  if (ierr/=0) finalrc = ESMF_FAILURE
  if (rank < 2) then
    call MPI_COMM_SPLIT(MPI_COMM_WORLD, 0, 0, esmfComm, ierr)
  else
    call MPI_COMM_SPLIT(MPI_COMM_WORLD, 1, 0, esmfComm, ierr)
  endif
#endif
  ! user code initializes ESMF
!BOC
  if (rank < 2) then
    call ESMF_Initialize(mpiCommunicator=esmfComm, rc=rc)
!EOC
    if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
    ! user code only to execute on the local MPI communicator
    print *, "ESMF application on MPI rank:", rank
    ! user code finalizes ESMF
!BOC
    call ESMF_Finalize(terminationflag=ESMF_KEEPMPI, rc=rc)
!EOC
    if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
    ! user code finalizes MPI
!BOC
  endif
!EOC
#ifndef ESMF_MPIUNI     
!BOC
  call MPI_Finalize(ierr)
!EOC
  if (ierr/=0) finalrc = ESMF_FAILURE
#endif
  ! print result
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_VMUserMpiCommEx.F90"
  else
    print *, "FAIL: ESMF_VMUserMpiCommEx.F90"
  endif
end program
