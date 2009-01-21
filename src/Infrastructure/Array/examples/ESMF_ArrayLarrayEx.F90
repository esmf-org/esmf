! $Id: ESMF_ArrayLarrayEx.F90,v 1.7.2.6 2009/01/21 21:25:19 cdeluca Exp $
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

!BOE
! \subsubsection{Array from {\tt ESMF\_LocalArray}}
! 
! Alternative to the direct usage of Fortran arrays during Array creation
! it is also possible to first create an {\tt ESMF\_LocalArray} and create the
! Array from it. While this may seem more burdensome for the 1 DE per PET cases
! discussed in the previous sections it allows a straight forward 
! generallization to the multiple DE per PET case. The following example first
! recaptures the previous example using an {\tt ESMF\_LocalArray} and then
! expands to the multiple DE per PET case.
!EOE
!BOC
program ESMF_ArrayLarrayEx

  use ESMF_Mod
  
  implicit none
  
!EOC
!BOE
! The current {\tt ESMF\_LocalArray} interface requires Fortran arrays to be 
! defined with pointer attribute.
!EOE
!BOC
  ! local variables
  real(ESMF_KIND_R8), pointer :: farrayP(:,:)       ! Fortran array pointer
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)     ! matching Fortran array pointer 
  type(ESMF_LocalArray)       :: larray             ! ESMF_LocalArray object
  type(ESMF_LocalArray)       :: larrayRef          ! ESMF_LocalArray object
  type(ESMF_DistGrid)         :: distgrid           ! DistGrid object
  type(ESMF_Array)            :: array              ! Array object
  integer                     :: rc, i, j, de
  real                        :: localSum
  type(ESMF_LocalArray), allocatable :: larrayList(:)      ! ESMF_LocalArray object list
  type(ESMF_LocalArray), allocatable :: larrayRefList(:)   ! ESMF_LocalArray object list
  
  type(ESMF_VM):: vm
  integer:: localPet, petCount
  
!EOC
  ! result code
  integer :: finalrc
  
  finalrc = ESMF_SUCCESS
  
!BOC
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  if (petCount /= 4) goto 10 ! TODO: use EXAMPLES_MULTI_ONLY once available
!EOC
!BOE
! DistGrid and array allocation remains unchanged.
!
!EOE
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/40,10/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  allocate(farrayP(14,14))    ! allocate Fortran array on each PET with halo
!EOC
!BOE
! Now instead of directly creating an Array object using the PET-local 
! {\tt farrayP}s an {\tt ESMF\_LocalArray} object will be created on each PET.
!EOE
!BOC
  larray = ESMF_LocalArrayCreate(farrayP, ESMF_DATA_REF, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! The Array object can now be created from {\tt larray}. The Array 
! creation method checks for each PET that the LocalArray can 
! accomodate the requested regions.
!EOE
!BOC
  array = ESMF_ArrayCreate(larrayList=(/larray/), distgrid=distgrid, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! Once created there is no difference in how the Array object can be used.
! The exclusive Array region on each PET can be accessed through a suitable
! Fortran array pointer as before.
!EOE
!BOC
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  farrayPtr = 123.456d0 ! initialize
!call ESMF_ArrayPrint(array, rc=rc)
!print *, "farrayPtr:", lbound(farrayPtr), ubound(farrayPtr)
!BOE
! Alternatively it is also possible (independent of how the Array object was
! created) to obtain the reference to the array allocation held by Array in 
! form of an {\tt ESMF\_LocalArry} object. The {\tt farrayPtr} can then be
! extracted using LocalArray methods.
!EOE
!BOC
  call ESMF_ArrayGet(array, larray=larrayRef, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  call ESMF_LocalArrayGet(larrayRef, farrayPtr, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!call ESMF_ArrayPrint(array, rc=rc)
!print *, "farrayPtr:", lbound(farrayPtr), ubound(farrayPtr)
!BOE
! Either way the {\tt farrayPtr} reference can be used now to add up the values
! of the local exclusive region for each DE. The following loop 
! works regardless of how the bounds were chosen for the original PET-local 
! {\tt farrayP} arrays and consequently the PET-local {\tt larray} objects.
!EOE
!BOC
  localSum = 0.
  do j=1, 10
    do i=1, 10
      localSum = localSum + farrayPtr(i, j)
    enddo
  enddo
  print *, "localSum=", localSum
!EOC
!print *, "localSum=", localSum
!BOE
! Cleanup.
!EOE
!BOC
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_LocalArrayDestroy(larray, rc=rc)
  deallocate(farrayP)   ! use the pointer that was used in allocate statement
  call ESMF_DistGridDestroy(distgrid, rc=rc)
!EOC
!BOE
! While the usage of LocalArrays is unnecessarily cumbersome for 1 DE per PET
! Arrays, it provides a straight forward path for extenting the interfaces 
! to multiple DEs per PET. 
!
! In the following example a 8 x 8 index space will be decomposed into
! 2 x 4 = 8 DEs. The situation is captured by the following DistGrid object.
!EOE
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/8,8/), &
    regDecomp=(/2,4/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
!
! The {\tt distgrid} object created in this manner will contain 8 DEs no 
! matter how many PETs are available during execution. Assuming an execution
! on 4 PETs will result in the following distribution of the decomposition.
!
! \begin{verbatim}
! 
!  +---------------------------------------> 2nd dimension
!  |  (1,1)
!  |    +-----------+-----------+-----------+-----------+
!  |    | DE0, PET0 | DE2, PET1 | DE4, PET2 | DE6, PET3 |
!  |    |  *    *   |  *    *   |  *    *   |  *    *   |
!  |    |           |           |           |           |
!  |    |  *    *   |  *    *   |  *    *   |  *    *   |
!  |    |           |           |           |           |
!  |    |  *    *   |  *    *   |  *    *   |  *    *   |
!  |    |           |           |           |           |
!  |    |  *    *   |  *    *   |  *    *   |  *    *   |
!  |    +-----------+-----------+-----------+-----------+
!  |    | DE1, PET0 | DE3, PET1 | DE5, PET2 | DE7, PET3 |
!  |    |  *    *   |  *    *   |  *    *   |  *    *   |
!  |    |           |           |           |           |
!  |    |  *    *   |  *    *   |  *    *   |  *    *   |
!  |    |           |           |           |           |
!  |    |  *    *   |  *    *   |  *    *   |  *    *   |
!  |    |           |           |           |           |
!  |    |  *    *   |  *    *   |  *    *   |  *    *   |
!  |    +-----------+-----------+-----------+-----------+
!  |                                                    (8,8)
!  v 
! 1st dimension
!
! \end{verbatim}
!
! Obviously each PET is associated with 2 DEs. Each PET must allocate enough
! space for {\em all} its DEs. This is done by allocating 
! as many DE-local arrays as there are DEs on the PET. The reference to these
! array allocations is passed into ArrayCreate via a LocalArray list argument
! that holds as many elements as there are DEs on the PET. Here each PET must
! allocate for two DEs.
!
!EOE
!BOC
  allocate(larrayList(2))   ! 2 DEs per PET
  allocate(farrayP(4, 2))   ! without halo each DE is of size 4 x 2 
  farrayP = 123.456d0
  larrayList(1) = ESMF_LocalArrayCreate(farrayP, ESMF_DATA_REF, rc=rc)  ! 1st DE
  allocate(farrayP(4, 2))   ! without halo each DE is of size 4 x 2 
  farrayP = 456.789d0
  larrayList(2) = ESMF_LocalArrayCreate(farrayP, ESMF_DATA_REF, rc=rc)  ! 2nd DE  
!EOC
!BOE
! Notice that it is perfectly fine to {\em re}-use {\tt farrayP} for all
! allocations of DE-local Fortran arrays. The allocated memory can be 
! deallocated at the end using the array pointer contained in the 
! {\tt larrayList}.
!
! With this information an Array object can be created. The {\tt distgrid}
! object indicates 2 DEs for each PET and ArrayCreate() expects to find two
! LocalArray elements in {\tt larrayList}.
!EOE
!BOC
  array = ESMF_ArrayCreate(larrayList=larrayList, distgrid=distgrid, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! Usage of a LocalArray list is the only way to provide a list of variable 
! length of Fortran array allocations to ArrayCreate() for each PET. The 
! {\tt array} object created by the above call is an ESMF distributed 
! object. As such it must follow the ESMF convention that requires that 
! the call to {\tt ESMF\_ArrayCreate()} must be issued in unison by all 
! PETs of the current context. Each PET only calls ArrayCreate() once, even if
! there are multiple DEs per PET.
!
! The ArrayGet() method provides access to the list of LocalArrays on each PET.
!EOE
!BOC
  allocate(larrayRefList(2))
  call ESMF_ArrayGet(array, larrayList=larrayRefList, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! Finally, access to the actual Fortran pointers is done on a per DE basis.
! Generally each PET will loop over its DEs.
!EOE
!BOC
  do de=1, 2
    call ESMF_LocalArrayGet(larrayRefList(de), farrayPtr, rc=rc)
    localSum = 0.
    do j=1, 2
      do i=1, 4
        localSum = localSum + farrayPtr(i, j)
      enddo
    enddo
    print *, "localSum=", localSum
  enddo
!EOC  
!BOE
! Note: If the VM associates multiple PEs with a PET the application writter 
! may decide to use OpenMP loop parallelization on the {\tt de} loop.
!
! Cleanup requires that the PET-local deallocations are done before the 
! pointers to the actual Fortran arrays are lost. Notice that {\tt larrayList}
! is used to obtain the pointers used in the deallocate statement. Pointers
! obtained from the {\tt larrayRefList}, while pointing to the same data, 
! {\em cannot} be used to deallocated the array allocations!
!EOE
!BOC
  do de=1, 2
    call ESMF_LocalArrayGet(larrayList(de), farrayPtr, rc=rc)
    deallocate(farrayPtr)
    call ESMF_LocalArrayDestroy(larrayList(de), rc=rc)
  enddo
  deallocate(larrayList)
  deallocate(larrayRefList)
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
!EOC  
!BOE
! With that ESMF can be shut down cleanly.
!EOE
10 continue
!BOC

  call ESMF_Finalize(rc=rc)
  
!EOC
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_ArrayLarrayEx.F90"
  else
    print *, "FAIL: ESMF_ArrayLarrayEx.F90"
  endif

!BOC
end program
!EOC
