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

!==============================================================================
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================

program ESMF_ArrayBundleHaloEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  
  implicit none
  
  ! local variables
  integer:: rc, petCount, localPet, i
  type(ESMF_VM):: vm
  type(ESMF_DistGrid):: distgrid
  type(ESMF_ArraySpec):: arrayspec
  type(ESMF_Array):: arrayList(5)
  type(ESMF_ArrayBundle):: arraybundle
  type(ESMF_RouteHandle):: haloHandle
  
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)
  
  ! result code
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg


  
  finalrc = ESMF_SUCCESS

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_ArrayBundleHaloEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------


  call ESMF_Initialize(vm=vm, defaultlogfilename="ArrayBundleHaloEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (petCount /= 4) then
    finalrc = ESMF_FAILURE
    goto 10
  endif
  
!BOE
! \subsubsection{Halo communication}
!
! One of the most fundamental communication pattern in domain decomposition
! codes is the {\em halo} operation. The ESMF Array class supports halos
! by allowing memory for extra elements to be allocated on each DE. See
! section \ref{Array:Halo} for a discussion of the Array level halo operation.
! The ArrayBundle level extents the Array halo operation to bundles of Arrays.
!
! First create an {\tt ESMF\_ArrayBundle} object containing a set of ESMF
! Arrays.
!EOE
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/4,4/), &
    regDecomp=(/2,2/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  arrayList(1) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  arrayList(2) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  arrayList(3) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/1,1/), totalUWidth=(/1,1/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  arrayList(4) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/1,1/), totalUWidth=(/1,1/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  arrayList(5) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/1,1/), totalUWidth=(/1,0/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  arraybundle = ESMF_ArrayBundleCreate(arrayList=arrayList, &
    name="MyArrayBundle", rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! initialize Arrays with values for verification
  do i=1, 5
    call ESMF_ArrayGet(arrayList(i), farrayPtr=farrayPtr, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    farrayPtr(:,:) = real(localPet + 100 * i, ESMF_KIND_R8)
  enddo
  
!BOE
! The ArrayBundle object can be treated as a single entity. The
! {\tt ESMF\_ArrayBundleHaloStore()} call determines the most efficient
! halo exchange pattern for {\em all} Arrays that are part of
! {\tt arraybundle}.
!EOE
  
!BOC
  call ESMF_ArrayBundleHaloStore(arraybundle=arraybundle, &
    routehandle=haloHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOE
! The halo exchange pattern stored in {\tt haloHandle} can now be applied to
! the {\tt arraybundle} object, or any other ArrayBundle that is compatible
! to the one used during the {\tt ESMF\_ArrayBundleHaloStore()} call.
!EOE
    
!BOC
  call ESMF_ArrayBundleHalo(arraybundle=arraybundle, routehandle=haloHandle, &
    rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOE
! Finally, when no longer needed, the resources held by {\tt haloHandle} need
! to be returned to the system by calling {\tt ESMF\_ArrayBundleHaloRelease()}.
!EOE

!BOC
  call ESMF_ArrayBundleHaloRelease(routehandle=haloHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! print Arrays to allow verification by inspection
  do i=1, 5
    call ESMF_ArrayPrint(arrayList(i), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  enddo

!BOE
! Finally the ArrayBundle object can be destroyed.
!EOE

!BOC
  call ESMF_ArrayBundleDestroy(arraybundle, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  do i=1, 5
    call ESMF_ArrayDestroy(arrayList(i), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  enddo
  
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


10 continue

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_ArrayBundleHaloEx.F90"
  else
    print *, "FAIL: ESMF_ArrayBundleHaloEx.F90"
  endif
  
end program
