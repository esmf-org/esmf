!BOC
! Simple ESMF application demonstrating VM features

program vm_test1

  ! modules
  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: rc
  type(ESMF_VM):: vm
  integer:: mypet, npets, npes, ssiid
  

  print *, 'hi from program vm_test1'

  call ESMF_Initialize(rc=rc)
  if (rc /= ESMF_SUCCESS) stop
 
  call ESMF_VMGetGlobal(vm, rc)
  call ESMF_VMPrint(vm, rc)

  call ESMF_VMGet(vm, mypet, npets, npes, rc=rc)
  print *, 'mypet is: ', mypet,' out of a total of ',npets,' PETs.'
  print *, 'there are ', npes,' PEs referenced by this VM'

  call ESMF_VMGetPET(vm, mypet, npes, ssiid, rc)
  print *, 'mypet is: ', mypet,' and I am claiming ',npes,' PEs on SSI ',ssiid

  call ESMF_Finalize(rc)
  print *, '======================= finished ================================='
  
end program
!EOC
