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
! \subsubsection{Communication - Scatter and Gather}
!
! The VM layer provides MPI-like collective communication. {\tt ESMF\_VMScatter()}
! scatters data located on {\tt root} PET across all the PETs of the VM. 
! {\tt ESMF\_VMGather()} provides the opposite operation, gathering data from
! all the PETs of the VM onto {\tt root} PET.
!
!EOE
!------------------------------------------------------------------------------

program ESMF_VMScatterVMGatherEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  
  implicit none
  
  ! local variables
  integer:: rc
  type(ESMF_VM):: vm
  integer:: localPet, petCount
!BOC
  integer, allocatable:: array1(:), array2(:)
!EOC
  integer:: nlen, nsize, i, scatterRoot, gatherRoot
  ! result code
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_VMScatterVMGatherEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


  finalrc = ESMF_SUCCESS

  call ESMF_Initialize(vm=vm, defaultlogfilename="VMScatterVMGatherEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
 
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  scatterRoot = 0
  gatherRoot = petCount-1

!BOC
  ! allocate data arrays
  nsize = 2
  nlen = nsize * petCount
  allocate(array1(nlen))
  allocate(array2(nsize))

  ! prepare data array1
  do i=1, nlen
    array1(i) = localPet * 100 + i
  enddo
!EOC
  
  ! verify contents of data array1
  print *, 'contents before scatter/gather:'
  do i=1, nlen
    print *, localPet,' array1: ', array1(i)
  enddo

  ! Scatter/Gather
!BOC
  call ESMF_VMScatter(vm, sendData=array1, recvData=array2, count=nsize, &
    rootPet=scatterRoot, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_VMGather(vm, sendData=array2, recvData=array1, count=nsize, &
    rootPet=gatherRoot, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
 
  ! print the scatter result
  print *, 'scatter result:'
  do i=1, nsize
    print *, localPet,' array2: ', array2(i)
  enddo
  
  ! print the gather result
  print *, 'gather result:'
  do i=1, nlen
    print *, localPet,' array1: ', array1(i)
  enddo
!EOC

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)


  
  call ESMF_Finalize(rc=rc)
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_VMScatterVMGatherEx.F90"
  else
    print *, "FAIL: ESMF_VMScatterVMGatherEx.F90"
  endif
  
end program
