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
! \subsubsection{Using VM communication methods with data of rank greater than one}
! \label{vm_higherrank}
!
! In the current implementation of the VM communication methods all the data
! array arguments are declared as {\em assumed shape} dummy arrays of rank one.
! The assumed shape flavor was chosen in order to minimize the chance of 
! copy in/out problems, associated with the other options for declaring the 
! dummy data arguments.
! However, currently the interfaces are not overloaded for higher ranks. This
! restriction requires that users that need to communicate data arrays with
! rank greater than one, must only pass the first dimension of the data array
! into the VM communication calls. Specifying the full size of the data arrays
! (considering {\em all} dimensions) ensure that the complete data is
! transferred in or out of the contiguous array memory.
!EOE
!------------------------------------------------------------------------------

program ESMF_VMHigherRankDataEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  
  implicit none
  
  ! local variables
  integer:: i, j, k, l, rc
  type(ESMF_VM):: vm
  integer:: localPet, petCount
  integer:: count1, count2, count3, count4, src, dst
!BOC
  integer, allocatable:: sendData(:,:)
  integer, allocatable:: recvData(:,:,:,:)
!EOC
  ! result code
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_VMHigherRankDataEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


  finalrc = ESMF_SUCCESS
  
  call ESMF_Initialize(vm=vm, defaultlogfilename="VMHigherRankDataEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  count1 = 5
  count2 = 8
  allocate(sendData(count1,count2)) ! 5 x 8 = 40 elements
  do j=1, count2
    do i=1, count1
      sendData(i,j) = localPet*100 + i + (j-1)*count1
    enddo
  enddo
  
  count1 = 2
  count2 = 5
  count3 = 1
  count4 = 4
  allocate(recvData(count1,count2,count3,count4)) ! 2 x 5 x 1 x 4 = 40 elements
  do l=1, count4
    do k=1, count3
      do j=1, count2
        do i=1, count1
          recvData(i,j,k,l) = 0
        enddo
      enddo
    enddo
  enddo
  
!EOC

  src = 0
  dst = petCount - 1
!BOC
  if (localPet==src) then
    call ESMF_VMSend(vm, &
      sendData=sendData(:,1), & ! 1st dimension as contiguous array section
      count=count1*count2, &    ! total count of elements
      dstPet=dst, rc=rc)
  endif
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  if (localPet==dst) then
    call ESMF_VMRecv(vm, &
      recvData=recvData(:,1,1,1), & ! 1st dimension as contiguous array section
      count=count1*count2*count3*count4, &  ! total count of elements
      srcPet=src, rc=rc)
  endif
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  do l=1, count4
    do k=1, count3
      do j=1, count2
        do i=1, count1
          print *, 'recvData for PET ',localPet,': ', recvData(i,j,k,l)
        enddo
      enddo
    enddo
  enddo


  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)



  call ESMF_Finalize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_VMHigherRankDataEx.F90"
  else
    print *, "FAIL: ESMF_VMHigherRankDataEx.F90"
  endif
  
end program
