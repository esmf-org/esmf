! Simple ESMF application demonstrating VM features
!
!BOP
!
! !DESCRIPTION:
! This example demonstrates how to use the VMAllFullReduce method to 
! find the VM-wide global sum of a data set.
!
!EOP
!

program ESMF_VMTest5Ex

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: rc
  type(ESMF_VM):: vm
  integer:: localPet
!BOC
  integer, allocatable:: array1(:)
  integer:: result
!EOC
  integer:: nsize, i

  call ESMF_Initialize(vm=vm, rc=rc)

  call ESMF_VMGet(vm, localPet, rc=rc)

  ! allocate data arrays
  nsize = 2
!BOC
  allocate(array1(nsize))

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
  ! global sum
  call ESMF_VMAllFullReduce(vm, array1, result, nsize, ESMF_SUM, rc=rc)
!EOC
  
  ! print the scatter result
  print *, 'Global sum:'
  print *, localPet,' result: ', result
  
  call ESMF_Finalize(rc)
  print *, '======================= finished ================================='
  
end program
