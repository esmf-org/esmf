! Simple ESMF application demonstrating VM features
!
!BOP
!
! !DESCRIPTION:
! The VM layer provides MPI-like collective communication. This example 
! demonstrates the use of VM-wide scatter and gather.
!
!EOP
!

program vm_test4

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: rc
  type(ESMF_VM):: vm
  integer:: localPet, petCount
!BOC
  integer, allocatable:: array1(:), array2(:)
!EOC
  integer:: nlen, nsize, i, scatterRoot, gatherRoot

  call ESMF_Initialize(vm=vm, rc=rc)
 
!BOC
  call ESMF_VMGet(vm, localPet, petCount, rc=rc)

  scatterRoot = 0
  gatherRoot = petCount-1

  ! allocate data arrays
  nsize = 2
  nlen = nsize * petCount
  allocate(array1(nlen))
  allocate(array2(nsize))

  ! prepare data array1
  do i=1, nlen
    array1(i) = localPet * 100 + i
  enddo
  
  ! verify contents of data array1
  print *, 'data array1:'
  do i=1, nlen
    print *, localPet,' array1: ', array1(i)
  enddo

  ! Scatter/Gather
  call ESMF_VMScatter(vm, array1, array2, nsize, scatterRoot, rc=rc)
  call ESMF_VMGather(vm, array2, array1, nsize, gatherRoot, rc=rc)
  
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
  
  call ESMF_Finalize(rc)
  
end program
