! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
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
! \subsubsection{Multiple concurrent instances of ESMF under separate MPI communicators}
! \label{vm_multi_instance_esmf}
!
! \begin{sloppypar}
! Multiple instances of ESMF can run concurrently under the same user main 
! program on separate MPI communicators. The user program first splits
! {\tt MPI\_COMM\_WORLD} into separate MPI communicators. Each communicator is
! then used to run a separate ESMF instance by passing it into 
! {\tt ESMF\_Initialize()} on the appropriate MPI ranks.
!
! Care must be taken to set the {\tt defaultlogfilename} to be unique on each
! ESMF instances. This prevents concurrent ESMF instances from writing to the
! same log file. 
! Further, each ESMF instances must call 
! {\tt ESMF\_Finalize()} with the {\tt endflag=ESMF\_END\_KEEPMPI} option in
! order to hand MPI control back to the user program. The outer user program is
! ultimately responsible for destroying the MPI communicators and to cleanly
! shut down MPI. 
! \end{sloppypar}
!
!EOE
!------------------------------------------------------------------------------

program ESMF_VMUserMpiCommMultiEx
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
  integer:: esmfComm, rank, size
  ! result code
  integer :: finalrc, result

  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_VMUserMpiCommMultiEx"

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
  ! User code determines the local rank and overall size of MPI_COMM_WORLD
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)
!EOC
  if (ierr/=0) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! User code prepares different MPI communicators.
  ! Here a single MPI_Comm_split() call is used to split MPI_COMM_WORLD
  ! into two non-overlapping communicators:
  ! One communicator for ranks 0 and 1, and the other for ranks 2 and above.
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
  size = 1
#endif
!BOC
  if (rank < 2) then
    ! Ranks 0 and 1 enter ESMF_Initialize() with the prepared communicator.
    ! Care is taken to set a unique log file name.
    call ESMF_Initialize(mpiCommunicator=esmfComm, &
      defaultlogfilename="VMUserMpiCommMultiEx1.Log", &
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
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, &
      ESMF_SRCLINE, petCount=size)

!BOC
    ! Finalize ESMF without finalizing MPI. The user application will call
    ! MPI_Finalize() on all ranks.
    call ESMF_Finalize(endflag=ESMF_END_KEEPMPI, rc=rc)
!EOC
    if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
  else
    ! Ranks 2 and above enter ESMF_Initialize() with the prepared communicator.
    ! Care is taken to set a unique log file name.
    call ESMF_Initialize(mpiCommunicator=esmfComm, &
      defaultlogfilename="VMUserMpiCommMultiEx2.Log", &
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
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, &
      ESMF_SRCLINE, petCount=size)

!BOC
    ! Finalize ESMF without finalizing MPI. The user application will call
    ! MPI_Finalize() on all ranks.
    call ESMF_Finalize(endflag=ESMF_END_KEEPMPI, rc=rc)
!EOC
    if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
  endif
!EOC
#ifndef ESMF_MPIUNI     
!BOC
  ! Free the MPI communicator(s) before finalizing MPI.
  call MPI_Comm_free(esmfComm, ierr)
  
  ! It is the responsibility of the outer user code to finalize MPI.
  call MPI_Finalize(ierr)
!EOC
  if (ierr/=0) finalrc = ESMF_FAILURE
#endif
  ! print result
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_VMUserMpiCommMultiEx.F90"
  else
    print *, "FAIL: ESMF_VMUserMpiCommMultiEx.F90"
  endif
end program
