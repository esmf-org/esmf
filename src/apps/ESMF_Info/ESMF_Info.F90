! $Id: ESMF_Info.F90,v 1.7 2011/06/22 15:08:07 rokuingh Exp $

program ESMF_Info

  ! modules
  use ESMF_Mod
  
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
      print *, "  ESMF_MAJOR_VERSION:   ", ESMF_MAJOR_VERSION
      print *, "  ESMF_MINOR_VERSION:   ", ESMF_MINOR_VERSION
      print *, "  ESMF_REVISION:        ", ESMF_REVISION
      print *, "  ESMF_PATCHLEVEL:      ", ESMF_PATCHLEVEL
      print *, "  ESMF_VERSION_STRING:  ", ESMF_VERSION_STRING
    end if
  endif
  
  call ESMF_Finalize()

end program
