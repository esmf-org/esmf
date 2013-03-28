! $Id$

program ESMF_Info

  ! modules
  use ESMF
  
  implicit none
  
  ! local variables
  integer:: rc, localPet
  integer:: argIndex
  type(ESMF_VM):: vm
    
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_VMGet(vm, localPet=localPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (localPet == 0) then
    ! check for standard command line arguments
    call ESMF_UtilGetArgIndex(argvalue="--help", argIndex=argIndex, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (argIndex >= 0) then
      ! standard --help argument was specified
      print *, "ESMF_Info: Print information about the ESMF installation."
      print *, "Options:"
      print *, "  --help        Display this information"
    else
      ! regular execution
      print *, "ESMF_Info"
      print *
      print *, "  ESMF_VERSION_STRING:       ", ESMF_VERSION_STRING
      print *
      print *, "  ESMF_VERSION_MAJOR:        ", ESMF_VERSION_MAJOR
      print *, "  ESMF_VERSION_MINOR:        ", ESMF_VERSION_MINOR
      print *, "  ESMF_VERSION_REVISION:     ", ESMF_VERSION_REVISION
      print *, "  ESMF_VERSION_PATCHLEVEL:   ", ESMF_VERSION_PATCHLEVEL
      print *, "  ESMF_VERSION_PUBLIC:       ", ESMF_VERSION_PUBLIC
      print *, "  ESMF_VERSION_BETASNAPSHOT: ", ESMF_VERSION_BETASNAPSHOT
    end if
  endif
  
  call ESMF_Finalize()

end program
