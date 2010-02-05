! $Id: ESMF_VMUserMpiCommEx.F90,v 1.8.2.1 2010/02/05 20:01:28 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
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
! \subsubsection{Nesting ESMF inside a user MPI application on a subset of MPI ranks}
!
! The previous example demonstrated that it is possible to nest an ESMF 
! application, i.e. {\tt ESMF\_Initialize()}...{\tt ESMF\_Finalize()} inside
! {\tt MPI\_Init()}...{\tt MPI\_Finalize()}. It is not necessary that all
! MPI ranks enter the ESMF application. The following example shows how the
! user code can pass an MPI communicator to {\tt ESMF\_Initialize()}, and
! enter the ESMF application on a subset of MPI ranks.
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
#ifndef ESMF_MPIUNI     
!BOC
  call MPI_Init(ierr)
  ! User code initializes MPI.
!EOC
  if (ierr/=0) finalrc = ESMF_FAILURE
!BOC
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  ! User code determines the local rank.
!EOC
  if (ierr/=0) finalrc = ESMF_FAILURE
!BOC
  ! User code prepares MPI communicator "esmfComm" that only contains
  ! rank 0 and 1.
!EOC
  if (rank < 2) then
    call MPI_COMM_SPLIT(MPI_COMM_WORLD, 0, 0, esmfComm, ierr)
  else
    call MPI_COMM_SPLIT(MPI_COMM_WORLD, 1, 0, esmfComm, ierr)
  endif
#endif
!BOC
  if (rank < 2) then
    call ESMF_Initialize(mpiCommunicator=esmfComm, rc=rc)
    ! Only call ESMF_Initialize() on rank 0 and 1, passing the prepared MPI
    ! communicator that spans these ranks.
!EOC
    if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
    ! user code only to execute on the local MPI communicator
    print *, "ESMF application on MPI rank:", rank
    ! user code finalizes ESMF
!BOC
    call ESMF_Finalize(terminationflag=ESMF_KEEPMPI, rc=rc)
    ! Finalize ESMF without finalizing MPI. The user application will call
    ! MPI_Finalize() on all ranks.
!EOC
    if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
  endif
!EOC
#ifndef ESMF_MPIUNI     
!BOC
  call MPI_Finalize(ierr)
  ! User code finalizes MPI.
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
