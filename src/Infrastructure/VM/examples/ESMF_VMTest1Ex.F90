! Simple ESMF application demonstrating VM features
!
!BOE
!
! \subsubsection{VM Default Basics Example}
!
! This example demonstrates the simplest ESMF application, consisting of only a 
! main program without any components. The global default VM, which is 
! automatically created during the {\tt ESMF\_Initialize()} call, is obtained
! and then used in its print method and several VM query calls.
!
!EOE
!
!BOC

program ESMF_VMDefaultBasicsEx

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: rc
  type(ESMF_VM):: vm
  integer:: localPet, petCount, peCount, ssiId
  
  call ESMF_Initialize(vm=vm, rc=rc)

  call ESMF_VMPrint(vm, rc)

  call ESMF_VMGet(vm, localPet, petCount, peCount, rc=rc)
  print *, 'localPet is: ', localPet,' out of a total of ',petCount,' PETs.'
  print *, 'there are ', peCount,' PEs referenced by this VM'

  call ESMF_VMGetPET(vm, localPet, peCount, ssiId, rc)
  print *, 'localPet is: ', localPet,' and it is claiming ',peCount,&
    ' PEs on SSI ', ssiId

  call ESMF_Finalize(rc)
  
end program
!EOC
