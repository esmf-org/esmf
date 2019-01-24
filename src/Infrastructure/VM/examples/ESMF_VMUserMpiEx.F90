! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
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
! explicitly calls {\tt MPI\_Init()} and {\tt MPI\_Finalize()}. The
! {\tt ESMF\_Initialize()} call automatically checks whether MPI has already
! been initialized, and if so does not call {\tt MPI\_Init()} internally. 
! On the finalize side, {\tt ESMF\_Finalize()} can be instructed to {\em not}
! call {\tt MPI\_Finalize()}, making it the responsibility of the outer code
! to finalize MPI.
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
  integer:: ierr
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
#ifndef ESMF_MPIUNI     
!BOC
  ! User code initializes MPI.
  call MPI_Init(ierr)
!EOC
  if (ierr/=0) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif
!BOC
  ! ESMF_Initialize() does not call MPI_Init() if it finds MPI initialized.
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
  ! It is the responsibility of the outer user code to finalize MPI.
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
