! Simple ESMF application demonstrating VM features
!
!BOP
!
! !DESCRIPTION:
! The VM layer provides MPI-like point-to-point communication. Use send and
! receive to communicate between two PETs. The following code sends data from
! the first to the last PET in the VM.
!
!EOP
!

program ESMF_VMTest3Ex

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: i, rc
  type(ESMF_VM):: vm
  integer:: localPet, petCount
  integer:: count, src, dst
  integer, allocatable:: localData(:)
  
  call ESMF_Initialize(vm=vm, rc=rc)

  count = 10
  allocate(localData(count))
  do i=1, count
    localData(i) = i
  enddo 
 
!BOC
  call ESMF_VMGet(vm, localPet, petCount, rc=rc)
  src = 0
  dst = petCount - 1
  if (localPet==src) &
    call ESMF_VMSend(vm, localData, count, dst, rc=rc)
  if (localPet==dst) &
    call ESMF_VMRecv(vm, localData, count, src, rc=rc)
!EOC

  call ESMF_Finalize(rc)
  
end program
