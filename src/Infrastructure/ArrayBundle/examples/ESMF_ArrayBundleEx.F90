! $Id: ESMF_ArrayBundleEx.F90,v 1.1.2.5 2009/01/21 21:25:19 cdeluca Exp $
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

program ESMF_ArrayBundleEx

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: rc, petCount, localPet
  integer:: i, arrayCount
  type(ESMF_VM):: vm
  type(ESMF_DistGrid):: distgrid
  type(ESMF_ArraySpec):: arrayspec
  type(ESMF_Array):: array(2)
  type(ESMF_Array), allocatable:: arrayList(:)
  type(ESMF_ArrayBundle):: arraybundle

  ! result code
  integer :: finalrc
  
  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  if (petCount /= 4) goto 10 ! TODO: use EXAMPLES_MULTI_ONLY once available
  
!BOE
! \subsubsection{ArrayBundle creation from a list of Arrays}
!
! First create an array of two {\tt ESMF\_Array} objects.
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  array(1) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  array(2) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! Now the {\tt array} of Arrays can be used to create an ArrayBundle object.
!EOE

!BOC
  arraybundle = ESMF_ArrayBundleCreate(arrayList=array, &
    name="MyArrayBundle", rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! The ArrayBundle object can be printed.
!EOE

!BOC
  call ESMF_ArrayBundlePrint(arraybundle, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! \subsubsection{Access Arrays inside the ArrayBundle}
!
!EOE

!BOE
! Use {\tt ESMF\_ArrayBundleGet()} to determine how many Arrays are stored
! in an ArrayBundle.
!EOE

!BOC
  call ESMF_ArrayBundleGet(arraybundle, arrayCount=arrayCount, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! The {\tt arrayCount} can be used to correctly allocate the {\tt arrayList}
! variable for a second call to {\tt ESMF\_ArrayBundleGet()} to gain access
! to the bundled Array objects.
!EOE

!BOC
  allocate(arrayList(arrayCount))
  call ESMF_ArrayBundleGet(arraybundle, arrayList=arraylist, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! The {\tt arrayList} variable can be used to access the individual Arrays,
! e.g. to print them.
!EOE

!BOC
  do i=1, arrayCount
    call ESMF_ArrayPrint(arrayList(i), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  enddo
!EOC

!BOE
! \subsubsection{Destroy an ArrayBundle and its constituents}
!
!EOE

!BOE
! The ArrayBundle object can be destroyed.
!EOE

!BOC
  call ESMF_ArrayBundleDestroy(arraybundle, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! After the ArrayBundle object has been destroyed it is safe to destroy its
! constituents.
!EOE


!BOC
  call ESMF_ArrayDestroy(array(1), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOC
  call ESMF_ArrayDestroy(array(2), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOC
  call ESMF_DistGridDestroy(distgrid, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


10 continue
  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_ArrayBundleEx.F90"
  else
    print *, "FAIL: ESMF_ArrayBundleEx.F90"
  endif
  
end program
