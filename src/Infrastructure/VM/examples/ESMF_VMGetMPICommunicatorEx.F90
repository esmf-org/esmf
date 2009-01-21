! $Id: ESMF_VMGetMPICommunicatorEx.F90,v 1.10.2.3 2009/01/21 21:25:24 cdeluca Exp $
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
! \subsubsection{VMGet MPI Communicator Example}
!
! The following example code shows how to obtain the MPI intra-communicator
! out of a VM object. In order not to interfere with ESMF communications it is 
! advisable to duplicate the communicator before using it in user-level MPI
! calls. In this example the duplicated communicator is used for a user
! controlled barrier accross the context.
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
#ifndef ESMF_MPIUNI     
  integer:: mpic2
#endif
!EOC
  ! result code
  integer :: finalrc
  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
  call ESMF_VMGet(vm, mpiCommunicator=mpic, rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
#ifndef ESMF_MPIUNI     
!BOC
  call MPI_Comm_dup(mpic, mpic2, ierr)
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
