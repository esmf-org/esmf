! Simple ESMF application demonstrating VM features
!
!BOP
!
! !DESCRIPTION:
! The following example code shows how to obtain the MPI intra-communicator
! out of a VM object. In order not to interfer with ESMF communication a
! duplicate communicator is created before used in user-level MPI calls.
!
!EOP
!

program ESMF_VMTest2Ex

  use ESMF_Mod
  
  implicit none
  include 'mpif.h'
  
  ! local variables
  integer:: rc, ierr
  type(ESMF_VM):: vm
!BOC
  integer:: mpic, mpic2
!EOC
  
  call ESMF_Initialize(vm=vm, rc=rc)
 
!BOC
  call ESMF_VMGet(vm, mpiCommunicator=mpic, rc=rc)
  call MPI_Comm_Dup(mpic, mpic2, ierr)
  call MPI_Barrier(mpic2, ierr)
!EOC

  call ESMF_Finalize(rc)
  
end program
