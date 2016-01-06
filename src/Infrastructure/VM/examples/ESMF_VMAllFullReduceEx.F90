! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
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
! \subsubsection{Communication - AllReduce and AllFullReduce}
!
! Use {\tt ESMF\_VMAllReduce()} to reduce data distributed across the PETs of a 
! VM into a result vector, returned on all the PETs. Further, use
! {\tt ESMF\_VMAllFullReduce()} to reduce the data into a single scalar returned
! on all PETs.
!
!EOE
!------------------------------------------------------------------------------

program ESMF_VMAllFullReduceEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  
  implicit none
  
  ! local variables
  integer:: rc
  type(ESMF_VM):: vm
  integer:: localPet
!BOC
  integer, allocatable:: array1(:), array2(:)
!EOC
  integer:: result
  integer:: nsize, i
  ! result code
  integer :: finalrc
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    write(failMsg, *) "Example failure"
    write(testname, *) "Example ESMF_VMAllFullReduceEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


  finalrc = ESMF_SUCCESS

  call ESMF_Initialize(vm=vm, defaultlogfilename="VMAllFullReduceEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  ! allocate data arrays
  nsize = 2
  allocate(array1(nsize))
  allocate(array2(nsize))

  ! prepare data array1
  do i=1, nsize
    array1(i) = localPet * 100 + i
  enddo
!EOC
  
  ! verify contents of data array1
  print *, 'data array1:'
  do i=1, nsize
    print *, localPet,' array1: ', array1(i)
  enddo

!BOC
  call ESMF_VMAllReduce(vm, sendData=array1, recvData=array2, count=nsize, &
    reduceflag=ESMF_REDUCE_SUM, rc=rc)
  ! Reduce distributed sendData, element by element into recvData and
  ! return it on all the PETs.
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! global sum
!BOC
  call ESMF_VMAllFullReduce(vm, sendData=array1, recvData=result, &
    count=nsize, reduceflag=ESMF_REDUCE_SUM, rc=rc)
  ! Fully reduce the distributed sendData into a single scalar and
  ! return it in recvData on all PETs.
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! print the scatter result
  print *, 'Global sum:'
  print *, localPet,' result: ', result

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_VMAllFullReduceEx.F90"
  else
    print *, "FAIL: ESMF_VMAllFullReduceEx.F90"
  endif
  
end program
