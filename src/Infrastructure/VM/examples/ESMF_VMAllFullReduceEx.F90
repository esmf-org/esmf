! $Id: ESMF_VMAllFullReduceEx.F90,v 1.8.2.1 2010/02/05 20:01:22 svasquez Exp $
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
! \subsubsection{AllReduce and AllFullReduce}
!
! Use {\tt ESMF\_VMAllReduce()} to reduce data distributed across the PETs of a 
! VM into a result vector, returned on all the PETs. Further, use
! {\tt ESMF\_VMAllFullReduce()} to reduce the data into a single scalar returned
! on all PETs.
!
!EOE
!------------------------------------------------------------------------------

program ESMF_VMAllFullReduceEx

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: rc
  type(ESMF_VM):: vm
  integer:: localPet
  integer, allocatable:: array1(:), array2(:)
  integer:: result
  integer:: nsize, i
  ! result code
  integer :: finalrc
  finalrc = ESMF_SUCCESS

  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE

  call ESMF_VMGet(vm, localPet, rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE

  ! allocate data arrays
  nsize = 2
  allocate(array1(nsize))
  allocate(array2(nsize))

  ! prepare data array1
  do i=1, nsize
    array1(i) = localPet * 100 + i
  enddo
  
  ! verify contents of data array1
  print *, 'data array1:'
  do i=1, nsize
    print *, localPet,' array1: ', array1(i)
  enddo

!BOC
  call ESMF_VMAllReduce(vm, sendData=array1, recvData=array2, count=nsize, &
    reduceflag=ESMF_SUM, rc=rc)
  ! Both sendData and recvData must be 1-d arrays. Reduce distributed sendData
  ! element by element into recvData and return in on all PETs.
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  
  ! global sum
!BOC
  call ESMF_VMAllFullReduce(vm, sendData=array1, recvData=result, count=nsize, &
    reduceflag=ESMF_SUM, rc=rc)
  ! sendData must be 1-d array. Fully reduce the distributed sendData into a
  ! single scalar and return it in recvData on all PETs.
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  
  ! print the scatter result
  print *, 'Global sum:'
  print *, localPet,' result: ', result
  
  call ESMF_Finalize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_VMAllFullReduceEx.F90"
  else
    print *, "FAIL: ESMF_VMAllFullReduceEx.F90"
  endif
  
end program
