! Simple ESMF application

program esmf_application

  ! modules
  use ESMF
  
  implicit none
  
  ! local variables
  integer:: rc
  
  call ESMF_Initialize(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_NORMAL)
  
  print *, "hi from program esmf_application"
  print *, "======================= finished ================================="

  call ESMF_Finalize(rc=rc)
  
end program
