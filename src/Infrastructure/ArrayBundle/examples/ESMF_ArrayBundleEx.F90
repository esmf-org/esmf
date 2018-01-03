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
  type(ESMF_ArraySpec):: arrayspec, arrayspec2
  type(ESMF_Array), allocatable:: arrayList(:)
  type(ESMF_Array)        :: arrayOut
  type(ESMF_ArrayBundle)  :: arraybundle
  character(ESMF_MAXSTR)  :: testname
  character(ESMF_MAXSTR)  :: failMsg


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
! \subsubsection{Creating an ArrayBundle from a list of Arrays}
!
! An ArrayBundle is created from a list of {\tt ESMF\_Array} objects.
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
! Now {\tt arrayList} is used to create an ArrayBundle object.
!EOE

!BOC
  arraybundle = ESMF_ArrayBundleCreate(arrayList=arrayList, &
    name="MyArrayBundle", rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOE
! Here the temporary {\tt arrayList} can be deallocated. This will not affect
! the ESMF Array objects inside the ArrayBundle. However, the Array objects
! must not be deallocated while the ArrayBundle references them.
!EOE
!BOC
  deallocate(arrayList)
!EOC

!BOE
! \subsubsection{Adding, removing, replacing Arrays in the ArrayBundle}
!
! Individual Arrays can be added using the Fortran array constructor syntax
! {\tt (/ ... /)}. Here an ESMF\_Array is created on the fly and immediately
! added to the ArrayBundle.
!EOE
!BOC
  call ESMF_ArrayBundleAdd(arraybundle, arrayList=(/ &
    ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, name="AonFly")/), &
    rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Items in the ArrayBundle can be replaced by items with the same name.
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec2, typekind=ESMF_TYPEKIND_R4, rank=2, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_ArrayBundleReplace(arraybundle, arrayList=(/ &
    ESMF_ArrayCreate(arrayspec=arrayspec2, distgrid=distgrid, name="AonFly")/), &
    rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Items can be removed from the ArrayBundle by providing their name.
!EOE
!BOC
  call ESMF_ArrayBundleRemove(arraybundle, arrayNameList=(/"AonFly"/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The ArrayBundle AddReplace() method can be used to conveniently add an
! item to the ArrayBundle, or replacing an existing item of the same name.
!EOE
!BOC
  call ESMF_ArrayBundleAddReplace(arraybundle, arrayList=(/ &
    ESMF_ArrayCreate(arrayspec=arrayspec2, distgrid=distgrid, name="AonFly")/), &
    rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The ArrayBundle object can be printed at any time to list its contents by name.
!EOE

!BOC
  call ESMF_ArrayBundlePrint(arraybundle, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! \subsubsection{Accessing Arrays inside the ArrayBundle}
!
! Individual items in the ArrayBundle can be accessed directly by their
! name.
!EOE
!BOC
  call ESMF_ArrayBundleGet(arraybundle, arrayName="AonFly", array=arrayOut, &
    rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! A list containing all of the Arrays in the ArrayBundle can also be requested
! in a single call.
! This requires that a large enough list argument is passed into the
! {\tt ESMF\_ArrayBundleGet()} method. The exact number of items in the
! ArrayBundle can be queried using the {\tt arrayCount} argument first.
!EOE

!BOC
  call ESMF_ArrayBundleGet(arraybundle, arrayCount=arrayCount, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! \begin{sloppypar}
! Then use {\tt arrayCount} to correctly allocate the {\tt arrayList}
! variable for a second call to {\tt ESMF\_ArrayBundleGet()}.
! \end{sloppypar}
!EOE
!BOC
  allocate(arrayList(arrayCount))
  call ESMF_ArrayBundleGet(arraybundle, arrayList=arrayList, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Now the {\tt arrayList} variable can be used to access the individual Arrays,
! e.g. to print them.
!EOE
!BOC
  do i=1, arrayCount
    call ESMF_ArrayPrint(arrayList(i), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  enddo
!EOC

!BOE
! \begin{sloppypar}
! By default the {\tt arrayList} returned by {\tt ESMF\_ArrayBundleGet()}
! contains the items in alphabetical order. To instead return the items in the
! same order in which they were added to the ArrayBundle, the
! {\tt itemorderflag} argument is passed with a value of 
! {\tt ESMF\_ITEMORDER\_ADDORDER}.
! 
! \end{sloppypar}
!EOE
!BOC
  call ESMF_ArrayBundleGet(arraybundle, arrayList=arrayList, &
    itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! \subsubsection{Destroying an ArrayBundle and its constituents}
!
!EOE

!BOE
! Destroying an ArrayBundle does not destroy the Arrays. In fact, it leaves the
! Arrays totally unchanged.
!EOE
!BOC
  call ESMF_ArrayBundleDestroy(arraybundle, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The Arrays must be destroyed separately.
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
