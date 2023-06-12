! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2023, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

!==============================================================================
!ESMF_disabled_EXAMPLE        String used by test script to count examples.
!==============================================================================

!------------------------------------------------------------------------------
!BOE
!
! \subsubsection{Using the MPI Communicator with the Fortran 2008 MPI binding}
!
! The Fortran 2008 MPI language binding defines {\tt type MPI\_Comm} to
! represent the MPI communicator. The following example demonstrates
! how the MPI communicator queried from the VM object can be used with the
! Fortran 2008 MPI binding.
! 
!EOE
!------------------------------------------------------------------------------

program ESMF_VMGetMPICommunicatorF08Ex.F90
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
!BOC
  use mpi_f08
!EOC

  implicit none

  ! local variables
  integer:: rc
#ifndef ESMF_MPIUNI
  integer:: ierr
#endif
  type(ESMF_VM):: vm
!BOC
  integer       :: int_mpic
  type(MPI_Comm):: mpic
!EOC
#ifndef ESMF_MPIUNI
!BOC
  type(MPI_Comm):: mpic2
!EOC
#endif

  ! result code
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_VMGetMPICommunicatorF08Ex"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, defaultlogfilename="VMGetMPICommunicatorEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_VMGet(vm, mpiCommunicator=int_mpic, rc=rc)
  ! The returned MPI communicator spans the same MPI processes that the VM
  ! is defined on.
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#ifndef ESMF_MPIUNI
!BOC
  mpic%mpi_val = int_mpic ! integer version of communicator -> type(MPI_Comm)

  ! Now mpic can be used in the Fortran 2008 MPI binding interfaces

  call MPI_Comm_dup(mpic, mpic2, ierr)
  ! Duplicate the MPI communicator not to interfere with ESMF communications.
  ! The duplicate MPI communicator can be used in any MPI call in the user
  ! code. Here the MPI_Barrier() routine is called.
  call MPI_Barrier(mpic2, ierr)
!EOC
  call MPI_Comm_free(mpic2, ierr)
#endif
  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_VMGetMPICommunicatorF08Ex.F90"
  else
    print *, "FAIL: ESMF_VMGetMPICommunicatorF08Ex.F90"
  endif
end program
