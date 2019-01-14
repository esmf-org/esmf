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
! \subsubsection{Nesting ESMF inside a user MPI application on a subset of MPI ranks}
! \label{vm_nesting_esmf}
!
! \begin{sloppypar}
! The previous example demonstrated that it is possible to nest an ESMF 
! application, i.e. {\tt ESMF\_Initialize()}...{\tt ESMF\_Finalize()} inside
! {\tt MPI\_Init()}...{\tt MPI\_Finalize()}. It is not necessary that all
! MPI ranks enter the ESMF application. The following example shows how the
! user code can pass an MPI communicator to {\tt ESMF\_Initialize()}, and
! enter the ESMF application on a subset of MPI ranks.
! \end{sloppypar}
!
!EOE
!------------------------------------------------------------------------------

program ESMF_VMUserMpiCommEx
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
  integer:: esmfComm, rank
  ! result code
  integer :: finalrc, result

  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_VMUserMpiCommEx"

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

  finalrc = ESMF_SUCCESS
#ifndef ESMF_MPIUNI     
!BOC
  ! User code initializes MPI.
  call MPI_Init(ierr)
!EOC
  if (ierr/=0) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! User code determines the local rank.
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
!EOC
  if (ierr/=0) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! User code prepares MPI communicator "esmfComm", that allows rank 0 and 1
  ! to be grouped together.
  if (rank < 2) then
    ! first communicator split with color=0
    call MPI_Comm_split(MPI_COMM_WORLD, 0, 0, esmfComm, ierr)
  else
    ! second communicator split with color=1
    call MPI_Comm_split(MPI_COMM_WORLD, 1, 0, esmfComm, ierr)
  endif
!EOC
  if (ierr/=0) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#else
  rank = 0
#endif
!BOC
  if (rank < 2) then
    ! Only call ESMF_Initialize() on rank 0 and 1, passing the prepared MPI
    ! communicator that spans these ranks.
    call ESMF_Initialize(mpiCommunicator=esmfComm, &
      defaultlogfilename="VMUserMpiCommEx.Log", &
      logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
!EOC
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOC
    ! Use ESMF here...
!EOC
    
    ! user code only to execute on the local MPI communicator
    print *, "ESMF application on MPI rank:", rank

    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
    ! file that the scripts grep for.
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

!BOC
    ! Calling ESMF_Finalize() with endflag=ESMF_END_KEEPMPI instructs ESMF
    ! to keep MPI active.
    call ESMF_Finalize(endflag=ESMF_END_KEEPMPI, rc=rc)
!EOC
    if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
  else
    ! Ranks 2 and above do non-ESMF work...
!EOC
!BOC
  endif
!EOC
#ifndef ESMF_MPIUNI     
!BOC
  ! Free the MPI communicator before finalizing MPI.
  call MPI_Comm_free(esmfComm, ierr)
  
  ! It is the responsibility of the outer user code to finalize MPI.
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
