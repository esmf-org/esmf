! $Id: ESMF_VMGetMPICommunicatorEx.F90,v 1.16.2.1 2010/02/05 20:01:28 svasquez Exp $
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
! \subsubsection{Getting the MPI Communicator from an VM object}
!
! Sometimes user code requires access to the MPI communicator, e.g. to support
! legacy code that contains explict MPI communication calls. The correct way of
! wrapping such code into ESMF is to obtain the MPI intra-communicator out of
! the VM object. In order not to interfere with ESMF communications it is
! advisable to duplicate the communicator before using it in user-level MPI
! calls. In this example the duplicated communicator is used for a user
! controlled {\tt MPI\_Barrier()}.
!
!EOE
!------------------------------------------------------------------------------

program ESMF_VMGetMPICommunicatorEx

  use ESMF_Mod
  
  implicit none
  !include 'mpif.h'
  
  ! local variables
  integer:: rc
#ifndef ESMF_MPIUNI     
  integer:: ierr
#endif
  type(ESMF_VM):: vm
!BOC
  integer:: mpic
!EOC  
#ifndef ESMF_MPIUNI     
!BOC
  integer:: mpic2
!EOC
#endif

  ! result code
  integer :: finalrc
  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
  call ESMF_VMGet(vm, mpiCommunicator=mpic, rc=rc)
  ! The returned MPI communicator spans the same MPI processes that the VM
  ! is defined on.
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
#ifndef ESMF_MPIUNI     
!BOC
  call MPI_Comm_dup(mpic, mpic2, ierr)
  ! Duplicate the MPI communicator not to interfere with ESMF communications.
  ! The duplicate MPI communicator can be used in any MPI call in the user
  ! code. Here the MPI_Barrier() routine is called.
  call MPI_Barrier(mpic2, ierr)
!EOC
#endif
  call ESMF_Finalize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_VMGetMPICommunicatorEx.F90"
  else
    print *, "FAIL: ESMF_VMGetMPICommunicatorEx.F90"
  endif
end program
