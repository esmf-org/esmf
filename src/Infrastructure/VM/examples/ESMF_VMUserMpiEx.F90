! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
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
! \subsubsection{Nesting ESMF inside a user MPI application}
! \label{vm_inside_user_mpi}
!
! \begin{sloppypar}
! It is possible to nest an ESMF application inside a user application that
! explicitly initializes MPI. This means that the user code calls
! {\tt MPI\_Init()} or {\tt MPI\_Init\_thread()}.
! The {\tt ESMF\_Initialize()} call automatically checks whether MPI has already
! been initialized, and if so, does {\em not} initialize MPI internally.
! On the finalize side, {\tt ESMF\_Finalize()} can be instructed to {\em not}
! call {\tt MPI\_Finalize()}, making it the responsibility of the outer user
! code to finalize MPI by explicitly by calling {\tt MPI\_Finalize()}.
! \end{sloppypar}
!
!EOE
!------------------------------------------------------------------------------

program ESMF_VMUserMpiEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod

  implicit none
#ifndef ESMF_MPIUNI
  include 'mpif.h'
#endif

  ! local variables
  integer:: rc
#ifndef ESMF_MPIUNI
  integer:: ierr, provided
#endif
  ! result code
  integer :: finalrc, result

  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_VMUserMpiEx"

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

  finalrc = ESMF_SUCCESS

!BOC
  ! For cases where ESMF resource management is desired (e.g. for threading),
  ! ESMF_InitializePreMPI() must be called before MPI_Init().
  call ESMF_InitializePreMPI(rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#ifndef ESMF_MPIUNI
!BOC
  ! User code initializes MPI. In order to support the ESMF resource management
  ! features, MPI must be initialized with {\tt MPI\_THREAD\_SERIALIZED} thread
  ! support or higher.
  call MPI_Init_thread(MPI_THREAD_SERIALIZED, provided, ierr)
!EOC
  if (ierr/=0) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif
!BOC
  ! ESMF_Initialize() does not initialize MPI if it finds MPI initialized
  ! already.
  call ESMF_Initialize(defaultlogfilename="VMUserMpiEx.Log", &
    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  ! Use ESMF here...
!EOC

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

!BOC
  ! Calling ESMF_Finalize() with endflag=ESMF_END_KEEPMPI instructs ESMF
  ! to keep MPI active.
  call ESMF_Finalize(endflag=ESMF_END_KEEPMPI, rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
#ifndef ESMF_MPIUNI
!BOC
  ! It is now the responsibility of the outer user code to finalize MPI.
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
