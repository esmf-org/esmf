! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

!==============================================================================
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================

!BOE
! \subsubsection{Array from native Fortran array with extra elements for halo or padding}
! \label{Array:fpadding}
!
! The example of the previous section showed how easy it is to create an Array
! object from existing PET-local Fortran arrays. The example did, however, not
! define any halo elements around the DE-local regions. The following code
! demonstrates how an Array object with space for a halo can be set up.
!EOE
!BOC
program ESMF_ArrayFarrayHaloEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  
  implicit none
  
!EOC
!BOE
! The allocatable array {\tt farrayA} will be used to provide the PET-local
! Fortran array for this example.
!EOE
!BOC
  ! local variables
  real(ESMF_KIND_R8), allocatable :: farrayA(:,:) ! allocatable Fortran array
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)   ! matching Fortran array ptr
  type(ESMF_DistGrid)         :: distgrid         ! DistGrid object
  type(ESMF_Array)            :: array            ! Array object
  integer                     :: rc, i, j
  real(ESMF_KIND_R8)          :: localSum
  
!EOC
  type(ESMF_VM):: vm
  integer:: petCount
  
  ! result code
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_ArrayFarrayHaloEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

  
  finalrc = ESMF_SUCCESS
  
!BOC
  call ESMF_Initialize(defaultlogfilename="ArrayFarrayHaloEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!EOC
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (petCount /= 4) then
    finalrc = ESMF_FAILURE
    goto 10
  endif
  
!BOE
! The Array is to cover the exact same index space as in the previous
! example. Furthermore decomposition and distribution are also kept the same.
! Hence the same DistGrid object will be created and it is expected to 
! execute this example with 4 PETs.
!
!EOE
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/40,10/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! This DistGrid describes a 40 x 10 index space that will be decomposed into 
! 4 DEs when executed on 4 PETs, associating 1 DE per PET. Each DE-local 
! exclusive region contains 10 x 10 elements. The DistGrid also stores and provides
! information about the relationship between DEs in index space, however,
! DistGrid does not contain information about halos. Arrays contain halo 
! information and it is possible to create multiple Arrays covering the same
! index space with identical decomposition and distribution using the same
! DistGrid object, while defining different, Array-specific halo regions.
!
! The extra memory required to cover the halo in the Array object must be 
! taken into account when allocating the PET-local {\tt farrayA} arrays. For
! a halo of 2 elements in each direction the following allocation will suffice.
!EOE
!BOC
  allocate(farrayA(14,14))    ! Fortran array with halo: 14 = 10 + 2 * 2
!EOC
  farrayA = 36.71d0 ! initialize
!BOE
! The {\tt farrayA} can now be used to create an Array object with enough space
! for a two element halo in each direction. The Array creation method checks for 
! each PET that the local Fortran array can accommodate the requested regions.
!
! The default behavior of ArrayCreate() is to center the exclusive region within
! the total region. Consequently the following call will provide the 2 extra 
! elements on each side of the exclusive 10 x 10 region without having to specify
! any additional arguments.
!EOE
!BOC
  array = ESMF_ArrayCreate(farray=farrayA, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
call ESMF_ArrayPrint(array)
!BOE
! The exclusive Array region on each PET can be accessed through a suitable
! Fortran array pointer. See section \ref{Array_regions_and_default_bounds}
! for more details on Array regions.
!EOE
!BOC
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Following Array bounds convention, which by default puts the beginning of 
! the exclusive region at (1, 1, ...), the following loop will add up the 
! values of the local exclusive region for each DE, regardless of how the bounds
! were chosen for the original PET-local {\tt farrayA} arrays.
!EOE
!BOC
  localSum = 0.
  do j=1, 10
    do i=1, 10
      localSum = localSum + farrayPtr(i, j)
    enddo
  enddo
!EOC
print *, "localSum=", localSum
!BOE
! Elements with $i$ or $j$ in the [-1,0] or [11,12] ranges are located outside the
! exclusive region and may be used to define extra computational points or 
! halo operations.
!
! Cleanup and shut down ESMF.
!EOE
!BOC
  call ESMF_ArrayDestroy(array, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  deallocate(farrayA)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
10 continue
  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

!BOC
  call ESMF_Finalize(rc=rc)
!EOC
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_ArrayFarrayHaloEx.F90"
  else
    print *, "FAIL: ESMF_ArrayFarrayHaloEx.F90"
  endif

!BOC
end program
!EOC
