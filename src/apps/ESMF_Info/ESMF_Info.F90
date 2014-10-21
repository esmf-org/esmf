! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2014, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

program ESMF_Info

  ! modules
  use ESMF
  
  implicit none
  
  ! local variables
  integer:: rc, localPet
  integer:: argIndex
  logical:: argFlag
  type(ESMF_VM):: vm
    
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_VMGet(vm, localPet=localPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (localPet == 0) then
    argFlag = .false.
    ! check for standard command line arguments
    call ESMF_UtilGetArgIndex(argvalue="--help", argIndex=argIndex, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (argIndex >= 0) then
      argFlag = .true.
      ! standard --help argument was specified
      print *, "ESMF_Info: Print information about the ESMF installation."
      print *, "Options:"
      print *, "  --help        Display this information and exit."
      print *, "  --version     Display ESMF version and license information and exit."
      print *, "  -V            Display ESMF version string and exit."
      print *, ""
    endif
    call ESMF_UtilGetArgIndex(argvalue="--version", argIndex=argIndex, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (argIndex >= 0) then
      argFlag = .true.
      ! standard --version argument was specified
      print *, "  ESMF_VERSION_STRING:       ", ESMF_VERSION_STRING
      print *, "  ESMF_VERSION_MAJOR:        ", ESMF_VERSION_MAJOR
      print *, "  ESMF_VERSION_MINOR:        ", ESMF_VERSION_MINOR
      print *, "  ESMF_VERSION_REVISION:     ", ESMF_VERSION_REVISION
      print *, "  ESMF_VERSION_PATCHLEVEL:   ", ESMF_VERSION_PATCHLEVEL
      print *, "  ESMF_VERSION_PUBLIC:       ", ESMF_VERSION_PUBLIC
      print *, "  ESMF_VERSION_BETASNAPSHOT: ", ESMF_VERSION_BETASNAPSHOT
      print *, ""
      print *, "Earth System Modeling Framework"
      print *, ""
      print *, "Copyright (c) 2002-2014 University Corporation for Atmospheric Research,"
      print *, "Massachusetts Institute of Technology, Geophysical Fluid Dynamics Laboratory,"
      print *, "University of Michigan, National Centers for Environmental Prediction,"
      print *, "Los Alamos National Laboratory, Argonne National Laboratory,"
      print *, "NASA Goddard Space Flight Center.  All rights reserved."
      print *, ""
      print *, "Permission is hereby granted, free of charge, to any person obtaining a copy"
      print *, 'of this software and associated documentation files (the "Software"), to'
      print *, "deal with the Software without restriction, including without limitation the"
      print *, "rights to use, copy, modify, merge, publish, distribute, sublicense, and/or"
      print *, "sell copies of the Software, and to permit persons to whom the Software is"
      print *, "furnished to do so, subject to the following conditions:"
      print *, "   1. Redistributions of source code must retain the above copyright notice,"
      print *, "      this list of conditions and the following disclaimers."
      print *, "   2. Redistributions in binary form must reproduce the above copyright"
      print *, "      notice, this list of conditions and the following disclaimers in the"
      print *, "      documentation and/or other materials provided with the distribution."
      print *, "   3. Neither the names of the organizations developing this software, nor"
      print *, "      its contributors may be used to endorse or promote products derived"
      print *, "      from this Software without specific prior written permission."
      print *, ""
      print *, 'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR'
      print *, "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,"
      print *, "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE"
      print *, "CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER"
      print *, "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING"
      print *, "FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS"
      print *, "WITH THE SOFTWARE."
      print *, ""
    endif
    call ESMF_UtilGetArgIndex(argvalue="-V", argIndex=argIndex, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (argIndex >= 0) then
      argFlag = .true.
      ! standard -V argument was specified
      print *, "  ESMF_VERSION_STRING:       ", ESMF_VERSION_STRING
      print *, ""
    endif
    if (.not.argFlag) then
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
      print *
      print *, "I/O feature support enabled:"
      print *, "  ESMF_IO_NETCDF_PRESENT     ", ESMF_IO_NETCDF_PRESENT
      print *, "  ESMF_IO_PIO_PRESENT        ", ESMF_IO_PIO_PRESENT
      print *, "  ESMF_IO_PNETCDF_PRESENT    ", ESMF_IO_PNETCDF_PRESENT
    end if
  endif
  
  call ESMF_Finalize()

end program
