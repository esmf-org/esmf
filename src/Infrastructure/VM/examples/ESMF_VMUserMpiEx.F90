! $Id: ESMF_VMUserMpiEx.F90,v 1.7.2.3 2009/01/21 21:25:24 cdeluca Exp $
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
! \subsubsection{ESMF inside user MPI application}
!
! The following example code demonstrates how ESMF can be used inside of a 
! user application that explicitly calls MPI\_Init() and MPI\_Finalize().
!
!EOE
!------------------------------------------------------------------------------

program ESMF_VMUserMpiEx

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
  ! result code
  integer :: finalrc
  finalrc = ESMF_SUCCESS
  ! user code initializes MPI
#ifndef ESMF_MPIUNI     
!BOC
  call MPI_Init(ierr)
!EOC
  if (ierr/=0) finalrc = ESMF_FAILURE
#endif
  ! user code initializes ESMF
!BOC
  call ESMF_Initialize(rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  ! user code finalizes ESMF
!BOC
  call ESMF_Finalize(terminationflag=ESMF_KEEPMPI, rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  ! user code finalizes MPI
#ifndef ESMF_MPIUNI     
!BOC
  call MPI_Finalize(ierr)
!EOC
  if (ierr/=0) finalrc = ESMF_FAILURE
#endif
  ! print result
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_VMUserMpiEx.F90"
  else
    print *, "FAIL: ESMF_VMUserMpiEx.F90"
  endif
end program
