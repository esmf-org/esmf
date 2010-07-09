program ESMF_Info

  ! modules
  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: rc
  
  call ESMF_Initialize(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  print *, "ESMF_Info"
  
  print *, "  ESMF_MAJOR_VERSION:   ", ESMF_MAJOR_VERSION
  print *, "  ESMF_MINOR_VERSION:   ", ESMF_MINOR_VERSION
  print *, "  ESMF_REVISION:        ", ESMF_REVISION
  print *, "  ESMF_PATCHLEVEL:      ", ESMF_PATCHLEVEL
  print *, "  ESMF_VERSION_STRING:  ", ESMF_VERSION_STRING

  call ESMF_Finalize()
  
end program
