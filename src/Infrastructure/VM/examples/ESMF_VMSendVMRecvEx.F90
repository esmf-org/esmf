! $Id: ESMF_VMSendVMRecvEx.F90,v 1.7.2.3 2009/01/21 21:25:24 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
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
! \subsubsection{VMSend/VMRecv Example}
!
! The VM layer provides MPI-like point-to-point communication. Use VMSend and
! VMRecv to communicate between two PETs. The following SPMD code sends data
! from PET 'src' and receives it on PET 'dst' of the VM. The sendData and
! recvData arguments must be 1-dimensional arrays.
!
!EOE
!------------------------------------------------------------------------------

program ESMF_VMSendVMRecvEx

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: i, rc
  type(ESMF_VM):: vm
  integer:: localPet, petCount
  integer:: count, src, dst
  integer, allocatable:: localData(:)
  ! result code
  integer :: finalrc
  finalrc = ESMF_SUCCESS
  
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE

  call ESMF_VMGet(vm, localPet, petCount, rc=rc)
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
    call ESMF_VMSend(vm, sendData=localData, count=count, dst=dst, rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
  if (localPet==dst) &
    call ESMF_VMRecv(vm, recvData=localData, count=count, src=src, rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE

  do i=1, count
    print *, 'localData for PET ',localPet,': ', localData(i)
  enddo 

  call ESMF_Finalize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_VMSendVMRecvEx.F90"
  else
    print *, "FAIL: ESMF_VMSendVMRecvEx.F90"
  endif
  
end program
