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
! \subsubsection{Controlling the MPI thread support level}
!
! By default, {\tt ESMF\_Initialize()} initializes MPI, requesting
! the maximum available thread support level, i.e. {\tt MPI\_THREAD\_MULTIPLE}.
! However, all ESMF
! features are available as long as the MPI implementation can provide
! {\tt MPI\_THREAD\_SERIALIZED} or above. In fact, most ESMF features
! are available for the lowest MPI thread support level of
! {\tt MPI\_THREAD\_SINGLE}. The only exception are the advanced resource
! management features, where some of the parent PETs are paused while the
! child component is executing, using the associated resources under a smaller
! set of child PETs. However, when these advanced features are not needed by
! an application, there are cases where running with a lower MPI thread support
! level might be beneficial, e.g. for performance or memory considerations.
!
! One way to affect the MPI thread support level is by explicitly calling
! {\tt MPI\_Init()} or {\tt MPI\_Init\_thread()} in user code {\em before}
! calling the {\tt ESMF\_Initialize()}. This approach has been shown in some
! of the previous examples.
!
! Another, often more convenient way of controlling the MPI thread support
! level requested during initialization is by setting the
! {\tt ESMF\_RUNTIME\_MPI\_THREAD\_SUPPORT} environment variable. This variable
! accepts any of the valid MPI settings.
! \begin{itemize}
! \item {\tt MPI\_THREAD\_SINGLE}
! \item {\tt MPI\_THREAD\_FUNNELED}
! \item {\tt MPI\_THREAD\_SERIALIZED}
! \item {\tt MPI\_THREAD\_MULTIPLE}
! \end{itemize}
! The setting is then used during {\tt ESMF\_Initialize()} when initializing
! MPI.
!
! For example, with
! \begin{verbatim}
! $ export ESMF_RUNTIME_MPI_THREAD_SUPPORT=MPI_THREAD_SINGLE
! \end{verbatim}
! for the bash shell, or
! \begin{verbatim}
! $ setenv ESMF_RUNTIME_MPI_THREAD_SUPPORT MPI_THREAD_SINGLE
! \end{verbatim}
! for under the csh shell, the following example program will execute with
! {\tt MPI\_THREAD\_SINGLE} as the MPI thread support level.
!EOE
!------------------------------------------------------------------------------

!BOC
program ESMF_VMControlMPIThreadSupportEx
!EOC
#include "ESMF.h"
!BOC
  use ESMF
!EOC
  use ESMF_TestMod
!BOC
  implicit none
!EOC
#ifndef ESMF_MPIUNI
!BOC
  include 'mpif.h'
!EOC
#endif
!BOC
  ! local variables
  integer       :: rc
  integer       :: ierr, provided
!EOC
  ! result code
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_VMControlMPIThreadSupportEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


  finalrc = ESMF_SUCCESS
!BOC
  call ESMF_Initialize(defaultlogfilename="VMControlMPIThreadSupportEx.Log", &
    rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! At this point MPI has been initialized, requesting thread support according
! to the setting of environment variable
! {\tt ESMF\_RUNTIME\_MPI\_THREAD\_SUPPORT}. Here it is
! {\tt MPI\_THREAD\_SINGLE}.
!
! MPI can be queried for the current level of thread support.
!EOE

#ifndef ESMF_MPIUNI
!BOC
  call MPI_Query_thread(provided, ierr)
!EOC
  if (ierr/=0) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! This can be used to check whether the correct MPI thread support level has
! been set.
!EOE
!BOC
  if (provided /= MPI_THREAD_SINGLE) then
    print *, "MPI thread support level not at MPI_THREAD_SINGLE."
    print *, "Something went wrong!!"
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
!EOC
#endif

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

!BOC
  call ESMF_Finalize(rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_VMControlMPIThreadSupportEx.F90"
  else
    print *, "FAIL: ESMF_VMControlMPIThreadSupportEx.F90"
  endif
!BOC
end program
!EOC
