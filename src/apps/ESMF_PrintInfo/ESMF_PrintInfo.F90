! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

program ESMF_PrintInfoF

  ! modules
  use ESMF
  
  implicit none
  
  ! local variables
  integer :: rc, localPet
  integer :: nargs
  integer :: argIndex
  logical :: argFlag
  character(ESMF_MAXSTR) :: argname
  type(ESMF_VM) :: vm

  integer :: i
    
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_VMGet(vm, localPet=localPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (localPet == 0) then
    call ESMF_UtilGetArgC (count=nargs)
    do, i=1, nargs
      call ESMF_UtilGetArg (argindex=i, argvalue=argname, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      select case (argname)
      case ('--help')
        print *, "ESMF_PrintInfo: Print information about the ESMF installation."
        print *, "Options:"
        print *, "  --help        Display this information and exit."
        print *, "  --version     Display ESMF version and license information and exit."
        print *, "  -V            Display ESMF version string and exit."
        print *, ""

      case ('--version')
        call ESMF_UtilVersionPrint (versionFlag=.true., rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      case ('-V')
        call ESMF_UtilVersionPrint (vFlag=.true., rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      case default
        print *, 'unknown argument: ', trim (argname)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

      end select

    end do

    if (nargs == 0) then
      ! regular execution
      print *, "ESMF_PrintInfo"
      print *
      print *, "  ESMF_VERSION_STRING:       ", ESMF_VERSION_STRING
      print *
      print *, "  ESMF_VERSION_MAJOR:        ", ESMF_VERSION_MAJOR
      print *, "  ESMF_VERSION_MINOR:        ", ESMF_VERSION_MINOR
      print *, "  ESMF_VERSION_REVISION:     ", ESMF_VERSION_REVISION
      print *, "  ESMF_VERSION_PATCHLEVEL:   ", ESMF_VERSION_PATCHLEVEL
      print *, "  ESMF_VERSION_PUBLIC:       ", ESMF_VERSION_PUBLIC
      print *, "  ESMF_VERSION_BETASNAPSHOT: ", ESMF_VERSION_BETASNAPSHOT
      print *
      print *, "I/O feature support enabled:"
      print *, "  ESMF_IO_NETCDF_PRESENT     ", ESMF_IO_NETCDF_PRESENT
      print *, "  ESMF_IO_PIO_PRESENT        ", ESMF_IO_PIO_PRESENT
      print *, "  ESMF_IO_PNETCDF_PRESENT    ", ESMF_IO_PNETCDF_PRESENT
    end if

  end if
  
  call ESMF_Finalize()

end program
