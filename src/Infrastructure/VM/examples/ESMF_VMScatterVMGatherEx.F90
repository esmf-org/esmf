! $Id: ESMF_VMScatterVMGatherEx.F90,v 1.10.2.1 2010/02/05 20:01:28 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!------------------------------------------------------------------------------
!BOE
!
! \subsubsection{Scatter and Gather}
!
! The VM layer provides MPI-like collective communication. {\tt ESMF\_VMScatter()}
! scatters data located on {\tt root} PET across all the PETs of the VM. 
! {\tt ESMF\_VMGather()} provides the opposite operation, gathering data from
! all the PETs of the VM onto {\tt root} PET.
!
!EOE
!------------------------------------------------------------------------------

program ESMF_VMScatterVMGatherEx

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: rc
  type(ESMF_VM):: vm
  integer:: localPet, petCount
  integer, allocatable:: array1(:), array2(:)
  integer:: nlen, nsize, i, scatterRoot, gatherRoot
  ! result code
  integer :: finalrc
  finalrc = ESMF_SUCCESS

  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
 
  call ESMF_VMGet(vm, localPet, petCount, rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE

  scatterRoot = 0
  gatherRoot = petCount-1

  ! allocate data arrays
  nsize = 2
  nlen = nsize * petCount
  allocate(array1(nlen))
  allocate(array2(nsize))

  ! prepare data array1
  do i=1, nlen
    array1(i) = localPet * 100 + i
  enddo
  
  ! verify contents of data array1
  print *, 'contents before scatter/gather:'
  do i=1, nlen
    print *, localPet,' array1: ', array1(i)
  enddo

  ! Scatter/Gather
!BOC
  call ESMF_VMScatter(vm, sendData=array1, recvData=array2, count=nsize, &
    root=scatterRoot, rc=rc)
  ! Both sendData and recvData must be 1-d arrays.
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
  call ESMF_VMGather(vm, sendData=array2, recvData=array1, count=nsize, &
    root=gatherRoot, rc=rc)
  ! Both sendData and recvData must be 1-d arrays.
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
 
  ! print the scatter result
  print *, 'scatter result:'
  do i=1, nsize
    print *, localPet,' array2: ', array2(i)
  enddo
  
  ! print the gather result
  print *, 'gather result:'
  do i=1, nlen
    print *, localPet,' array1: ', array1(i)
  enddo
!EOC
  
  call ESMF_Finalize(rc=rc)
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_VMScatterVMGatherEx.F90"
  else
    print *, "FAIL: ESMF_VMScatterVMGatherEx.F90"
  endif
  
end program
