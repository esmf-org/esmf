! $Id: ESMF_ArrayBundleEx.F90,v 1.19 2012/02/10 23:45:26 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
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

program ESMF_ArrayBundleEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  
  implicit none
  
  ! local variables
  integer:: rc, petCount, localPet
  integer:: i, arrayCount
  type(ESMF_VM):: vm
  type(ESMF_DistGrid):: distgrid
  type(ESMF_ArraySpec):: arrayspec
  type(ESMF_Array), allocatable:: arrayList(:)
  type(ESMF_ArrayBundle):: arraybundle
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg, finalMsg


  ! result code
  integer :: finalrc, result
  
  finalrc = ESMF_SUCCESS
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_ArrayBundleEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_Initialize(vm=vm, defaultlogfilename="ArrayBundleEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (petCount /= 4) then
    finalrc = ESMF_FAILURE
    goto 10
  endif
  
!BOE
! \subsubsection{Create an ArrayBundle from a list of Arrays}
!
! First create a Fortran array of two {\tt ESMF\_Array} objects.
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  allocate(arrayList(2))
  arrayList(1) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
                 rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  arrayList(2) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
                 rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Now the {\tt arrayList} of Arrays can be used to create an ArrayBundle object.
!EOE

!BOC
  arraybundle = ESMF_ArrayBundleCreate(arrayList=arrayList, &
    name="MyArrayBundle", rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOE
! The temporary {\tt arrayList} can be deallocated now. This will not affect
! the ESMF Array objects. The Array objects must not be deallocated while 
! the ArrayBundle refers to them!
!EOE
!BOC
  deallocate(arrayList)
!EOC

!BOE
! The ArrayBundle object can be printed.
!EOE

!BOC
  call ESMF_ArrayBundlePrint(arraybundle, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

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
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! \begin{sloppypar}
! The {\tt arrayCount} can be used to correctly allocate the {\tt arrayList}
! variable for a second call to {\tt ESMF\_ArrayBundleGet()} to gain access
! to the bundled Array objects.
! \end{sloppypar}
!EOE

!BOC
  allocate(arrayList(arrayCount))
  call ESMF_ArrayBundleGet(arraybundle, arrayList=arraylist, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The {\tt arrayList} variable can be used to access the individual Arrays,
! e.g. to print them.
!EOE

!BOC
  do i=1, arrayCount
    call ESMF_ArrayPrint(arrayList(i), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
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
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! After the ArrayBundle object has been destroyed it is safe to destroy its
! constituents.
!EOE


!BOC
  call ESMF_ArrayDestroy(arrayList(1), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  call ESMF_ArrayDestroy(arrayList(2), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  deallocate(arrayList)
  
  call ESMF_DistGridDestroy(distgrid, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  

10 continue
 ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
    ! file that the scripts grep for.
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)


  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_ArrayBundleEx.F90"
  else
    print *, "FAIL: ESMF_ArrayBundleEx.F90"
  endif
  
end program
