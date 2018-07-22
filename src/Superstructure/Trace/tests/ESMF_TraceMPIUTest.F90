! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!

program ESMF_TraceMPIUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_TraceMPIUTest - Trace MPI unit test
!
! !DESCRIPTION:
!
! The code in this file drives F90 Trace MPI unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod
  use ESMF

  implicit none

#ifndef ESMF_MPIUNI
  include 'mpif.h'
#endif

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
  '$Id$'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
  
  ! local variables
  integer                :: rc, i, localPet

  ! cumulative result: count failures; no failures equals "all pass"
  integer                :: result = 0

  type(ESMF_VM) :: vm
  integer       :: petCount
  type(ESMF_GridComp) :: gridcomp

  integer                 :: funit
  integer                 :: ioerr
  character(ESMF_MAXSTR)  :: line
  character(ESMF_MAXSTR)  :: filename

  integer                 :: mpitest_count
  integer(ESMF_KIND_I8)   :: mpitest_time

  integer :: mpicomm
  integer :: send
  integer :: recv
  integer :: ierr
    
  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)       
  !-----------------------------------------------------------------------------

  call ESMF_VMGetGlobal(vm=vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, &
       mpiCommunicator=mpicomm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

#ifndef ESMF_TESTTRACE
  if (localPet == 0) then
    print *, "Note:  Skipping MPI trace tests since ESMF_TESTTRACE is not ON."
  endif
#endif
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test trace user region enter"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_TraceRegionEnter("reg1", rc=rc)
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  mpitest_count = -1
  mpitest_time = -1
  write(name, *) "Test MPI statistics"
  call ESMF_TraceTest_GetMPIWaitStats(mpitest_count, mpitest_time, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(failMsg, *) "Unexpected MPI call count"
  call ESMF_Test((mpitest_count==0), name, failMsg, result, ESMF_SRCLINE)
  !-------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(failMsg, *) "Unexpected MPI time"
  call ESMF_Test((mpitest_time==0), name, failMsg, result, ESMF_SRCLINE)
  !-------------------------------------------------------------------------

  
  ! MPI_ALLREDUCE(SENDBUF, RECVBUF, COUNT, DATATYPE, OP, COMM, IERROR)
  !  <type>    SENDBUF(*), RECVBUF(*)
  !  INTEGER    COUNT, DATATYPE, OP, COMM, IERROR
  send = 1
  recv = -1
  ierr = 0
#ifndef ESMF_MPIUNI
  CALL MPI_ALLREDUCE(send, recv, 1, MPI_INTEGER, MPI_SUM, mpicomm, ierr)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest
  write(failMsg, *) "MPI_ALLREDUCE failed with ierr =", ierr
#if (!defined ESMF_MPIUNI && defined ESMF_TESTTRACE)
  call ESMF_Test((ierr==0), name, failMsg, result, ESMF_SRCLINE)
#else
  call ESMF_Test(.true., name, failMsg, result, ESMF_SRCLINE)
#endif
  !-------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !NEX_UTest
  print *, "MPI_ALLREDUCE returned ", recv
  write(failMsg, *) "MPI_ALLREDUCE produced unexpected result. Expected 4, got ", recv
#if (!defined ESMF_MPIUNI && defined ESMF_TESTTRACE)
  call ESMF_Test((recv==petCount), name, failMsg, result, ESMF_SRCLINE)
#else
  call ESMF_Test(.true., name, failMsg, result, ESMF_SRCLINE)
#endif
  !-------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  mpitest_count = -1
  mpitest_time = -1
  call ESMF_TraceTest_GetMPIWaitStats(mpitest_count, mpitest_time, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !NEX_UTest
  print *, "MPI call count = ", mpitest_count
  write(failMsg, *) "Unexpected MPI call count"
#if (!defined ESMF_MPIUNI && defined ESMF_TESTTRACE)
  call ESMF_Test((mpitest_count==1), name, failMsg, result, ESMF_SRCLINE)
#else
  call ESMF_Test(.true., name, failMsg, result, ESMF_SRCLINE)
#endif
  !-------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  print *, "MPI time = ", mpitest_time
  write(failMsg, *) "Unexpected MPI time"
#if (!defined ESMF_MPIUNI && defined ESMF_TESTTRACE)
  call ESMF_Test((mpitest_time > 0), name, failMsg, result, ESMF_SRCLINE)
#else
  call ESMF_Test(.true., name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------- 
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test trace user region exit"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_TraceRegionExit("reg1", rc=rc)
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test trace memory usage"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_TraceMemInfo(rc=rc)
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Close trace"
  write(failMsg, *) "Error closing trace"
  
  ! this is typically called in ESMF_Finalize, but calling
  ! here so trace files will be flushed and closed
  call ESMF_TraceClose(rc=rc)
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! barrier to ensure all files flushed
  call ESMF_VMBarrier(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  ! sleep to ensure files can be re-opened
  call ESMF_VMWtimeDelay(5.0_ESMF_KIND_R8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify trace metadata exists"
  write(failMsg, *) "Trace metadata does not exist"

  call ESMF_UtilIOUnitGet(funit, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  
  ioerr = 0
  if (localPet == 0) then   
     open (unit=funit, file="traceout/metadata", status="old", &
          action="read", iostat=ioerr )
     if (ioerr /= 0) then
        close(funit)
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
     endif
  endif
  call ESMF_Test((ioerr == 0), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Read trace metadata file"
  write(failMsg, *) "I/O error reading trace metadata file"

  ioerr = 0
  if (localPet == 0) then
     read(funit, '(A)', iostat=ioerr) line
     if (ioerr /= 0) then
        close(funit)
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
     else
        close(funit)
     endif
  endif
  call ESMF_Test((ioerr == 0), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Validate trace metadata header"
  write(failMsg, *) "Invalid header"

  if (localPet == 0) then
     call ESMF_Test((trim(line) == "/* CTF 1.8 */"), name, failMsg, result, ESMF_SRCLINE)
  else
     call ESMF_Test(.true., name, failMsg, result, ESMF_SRCLINE)
  endif
  !------------------------------------------------------------------------


  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify trace stream file exists"
  write(failMsg, *) "Trace stream file does not exist"
    
  write (filename, '(A,I1)') "traceout/esmf_stream_000", localPet
  print *, "Attempt to open trace file: ", trim(filename)
  open (unit=funit, file=trim(filename), status="old", &
       action="read", iostat=ioerr)
  if (ioerr /= 0) then
     print *, "IO error = ", ioerr
  endif
  call ESMF_Test((ioerr == 0), name, failMsg, result, ESMF_SRCLINE)
  close(funit)
  !------------------------------------------------------------------------
  
  
  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_TraceMPIUTest


