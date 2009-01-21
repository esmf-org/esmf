! $Id: ESMF_VMAllFullReduceEx.F90,v 1.4.2.3 2009/01/21 21:25:24 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
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
! \subsubsection{VMAllFullReduce Example}
!
! The VMAllFullReduce method can be used to find the VM-wide global sum of a
! data set.
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
  integer, allocatable:: array1(:)
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

  ! prepare data array1
  do i=1, nsize
    array1(i) = localPet * 100 + i
  enddo
  
  ! verify contents of data array1
  print *, 'data array1:'
  do i=1, nsize
    print *, localPet,' array1: ', array1(i)
  enddo

  ! global sum
!BOC
  call ESMF_VMAllFullReduce(vm, sendData=array1, recvData=result, count=nsize, &
    reduceflag=ESMF_SUM, rc=rc)
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
