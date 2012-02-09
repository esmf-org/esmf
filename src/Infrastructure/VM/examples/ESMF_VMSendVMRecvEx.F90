! $Id: ESMF_VMSendVMRecvEx.F90,v 1.24 2012/02/09 23:15:41 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
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
! \subsubsection{Send/Recv}
!
! The VM layer provides MPI-like point-to-point communication. Use 
! {\tt ESMF\_VMSend()} and {\tt ESMF\_VMRecv()} to pass data between two PETs.
! The following code sends data from PET 'src' and receives it on PET 'dst'.
! Both PETs must be part of the same VM. The sendData and recvData arguments
! must be 1-dimensional arrays.
!
!EOE
!------------------------------------------------------------------------------

program ESMF_VMSendVMRecvEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  
  implicit none
  
  ! local variables
  integer:: i, rc
  type(ESMF_VM):: vm
  integer:: localPet, petCount
  integer:: count, src, dst
  integer, allocatable:: localData(:)
  ! result code
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_VMSendVMRecvEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


  finalrc = ESMF_SUCCESS
  
  call ESMF_Initialize(vm=vm, defaultlogfilename="VMSendVMRecvEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE

  count = 10
  allocate(localData(count))
  do i=1, count
    localData(i) = localPet*100 + i
  enddo 
 
  src = 0
  dst = petCount - 1
!BOC
  if (localPet==src) &
    call ESMF_VMSend(vm, sendData=localData, count=count, dstPet=dst, rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
  if (localPet==dst) &
    call ESMF_VMRecv(vm, recvData=localData, count=count, srcPet=src, rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE

  do i=1, count
    print *, 'localData for PET ',localPet,': ', localData(i)
  enddo 


  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)



  call ESMF_Finalize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_VMSendVMRecvEx.F90"
  else
    print *, "FAIL: ESMF_VMSendVMRecvEx.F90"
  endif
  
end program
