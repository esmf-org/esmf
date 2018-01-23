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

program ESMF_ArrayRedistEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod

  
  implicit none
  
  ! local variables
  integer:: rc, petCount, localPet
  type(ESMF_VM):: vm
  type(ESMF_DistGrid):: srcDistGrid, dstDistGrid
  type(ESMF_Array):: srcArray, dstArray
  type(ESMF_Array):: srcArray1, dstArray1
  type(ESMF_Array):: srcArray2
  type(ESMF_ArraySpec):: arrayspec, arrayspec3d, arrayspec4d
  type(ESMF_RouteHandle):: redistHandle
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

  
  integer:: counter,i,j
  real(ESMF_KIND_R8), pointer:: farray2d(:,:)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_ArrayRedistEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, defaultlogfilename="ArrayRedistEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (petCount /= 4) then
    finalrc = ESMF_FAILURE
    goto 10
  endif
  
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

!BOE
!
! \subsubsection{Communication -- Redist}
! \label{Array:Redist}
! 
! Arrays used in different models often cover the same index space region,
! however, the distribution of the Arrays may be different, e.g. the models
! run on exclusive sets of PETs. Even if the Arrays are defined on the same
! list of PETs the decomposition may be different.
!EOE
!BOC
  srcDistgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/4,1/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  dstDistgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/1,4/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The number of elements covered by {\tt srcDistgrid} is identical to the number
! of elements covered by {\tt dstDistgrid} -- in fact the index space regions
! covered by both DistGrid objects are congruent. However, the decomposition
! defined by {\tt regDecomp}, and consequently the distribution of source and
! destination, are different.
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  srcArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=srcDistgrid, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  dstArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=dstDistgrid, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! By construction {\tt srcArray} and {\tt dstArray} are of identical type and
! kind. Further the number of exclusive elements matches between both Arrays.
! These are the prerequisites for the application of an Array redistribution
! in default mode. In order to increase performance of the actual 
! redistribution the communication pattern is precomputed once, and stored in
! an {\tt ESMF\_RouteHandle} object.
!EOE
!BOC
  call ESMF_ArrayRedistStore(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
!BOE
! The {\tt redistHandle} can now be used repeatedly to transfer data from
! {\tt srcArray} to {\tt dstArray}.
!EOE
!BOC
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! \begin{sloppypar}
! The use of the precomputed {\tt redistHandle} is {\em not} restricted to
! the ({\tt srcArray}, {\tt dstArray}) pair. Instead the {\tt redistHandle}
! can be used to redistribute data between any two Arrays that are weakly 
! congruent to the Array pair used during precomputation. Arrays are 
! {\em congruent} if they are defined on DistGrids that match with 
! {\tt ESMF\_DISTGRIDMATCH\_EXACT} or higher, and have matching DE-local
! array allocations (i.e. the same amount of padding around the exclusive 
! region as well as the same shape in the undistributed dimensions). For Arrays
! to be {\em weakly} congruent the shape and size of the undistributed
! dimensions that have a smaller stride than the first distributed dimension
! need not be the same. This definition covers even the case where an Array 
! does not have any undistributed dimensions.
! \end{sloppypar}
!
! For instance, an Array where the first two dimensions are undistributed, 
! and are of size 5 and 6 (i.e. undistributed shape (5,6)), is weakly congruent
! to an Array that only has a single undistributed first dimension. Furthermore,
! the size of this single undistributed dimension is not restricted by the
! number of undistributed elements in the first Array (i.e. here it does not
! have to be 5x6=30). The Array is also weakly congruent to an Array that does
! not have a first undistributed dimension at all. In either case the only 
! restriction is that the distributed dimensions must be congruent.
!
! The transferability of RouteHandles between Array pairs that are weakly
! congruent can greatly reduce the number of communication store calls needed.
! In a typical application Arrays are often defined on the same decomposition,
! typically leading to congruent distributed dimensions. However, these Arrays
! do not always have the same shape or size in the undistributed dimensions.
!
! For the current case, the {\tt redistHandle} was precomputed for simple 2D
! Arrays without undistributed dimensions. The RouteHandle transferability
! rule allows us to use this same RouteHandle to redistribute between two 
! 3D Array that are built on the same 2D DistGrid, but have an undistributed
! dimension that has a smaller stride than the distributed dimensions.
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec3d, typekind=ESMF_TYPEKIND_R8, rank=3, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  srcArray1 = ESMF_ArrayCreate(arrayspec=arrayspec3d, distgrid=srcDistgrid, &
    distgridToArrayMap=(/2,3/), undistLBound=(/1/), undistUBound=(/10/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  dstArray1 = ESMF_ArrayCreate(arrayspec=arrayspec3d, distgrid=dstDistgrid, &
    distgridToArrayMap=(/2,3/), undistLBound=(/1/), undistUBound=(/10/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE

!BOC
  call ESMF_ArrayRedist(srcArray=srcArray1, dstArray=dstArray1, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The following variation of the code shows that the same RouteHandle can be
! applied to an Array pair where the number of undistributed dimensions does
! not match between source and destination Array. The two requirements are 
! simply that each side is weakly congruent to the Array used during store, and
! that the {\em total} number of undistributed elements on source and 
! destination side is the same. Here we prepare a source Array with two leading
! undistributed dimensions that multiply out to 2x5=10 undistributed elements.
! The destination array is the same as before with only a single leading 
! undistributed dimension of size 10.
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec4d, typekind=ESMF_TYPEKIND_R8, rank=4, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  srcArray2 = ESMF_ArrayCreate(arrayspec=arrayspec4d, distgrid=srcDistgrid, &
    distgridToArrayMap=(/3,4/), undistLBound=(/1,1/), undistUBound=(/2,5/), &
    rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  call ESMF_ArrayRedist(srcArray=srcArray2, dstArray=dstArray1, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! When done, the resources held by {\tt redistHandle} need to be deallocated
! by the user code before the RouteHandle becomes inaccessible.
!EOE
!BOC
  call ESMF_ArrayRedistRelease(routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_ArrayDestroy(srcArray2, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArrayDestroy(srcArray1, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ArrayDestroy(dstArray1, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArrayDestroy(dstArray, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(dstDistgrid, rc=rc) ! destroy the DistGrid object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOE
! \begin{sloppypar}
! In {\em default} mode, i.e. without providing the optional
! {\tt srcToDstTransposeMap} argument, {\tt ESMF\_ArrayRedistStore()} does not
! require equal number of dimensions in source and destination Array. Only the
! total number of elements must match.
! Specifying {\tt srcToDstTransposeMap} switches {\tt ESMF\_ArrayRedistStore()}
! into {\em transpose} mode. In this mode each dimension of {\tt srcArray}
! is uniquely associated with a dimension in {\tt dstArray}, and the sizes of 
! associated dimensions must match for each pair.
! \end{sloppypar}
! 
!EOE
!BOC
  dstDistgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/20,10/), &
      rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  dstArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=dstDistgrid, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArrayGet(srcArray, farrayPtr=farray2d, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  counter = localPet*100
  do j=lbound(farray2d,2),ubound(farray2d,2)
    do i=lbound(farray2d,1),ubound(farray2d,1)
      farray2d(i,j) = Real(counter, ESMF_KIND_R8)
      counter = counter+1
    enddo
  enddo
  
!  call ESMF_ArrayPrint(srcArray)
  
!BOE
! This {\tt dstArray} object covers a 20 x 10 index space while the
! {\tt srcArray}, defined further up, covers a 10 x 20 index space. Setting
! {\tt srcToDstTransposeMap = (/2,1/)} will associate the first and second 
! dimension of {\tt srcArray} with the second and first dimension of
! {\tt dstArray}, respectively. This corresponds to a transpose of dimensions.
! Since the decomposition and distribution of dimensions may be different for
! source and destination redistribution may occur at the same time.
!EOE
!BOC
  call ESMF_ArrayRedistStore(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, srcToDstTransposeMap=(/2,1/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!  call ESMF_ArrayPrint(dstArray)

  call ESMF_ArrayRedistRelease(routehandle=redistHandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_ArrayDestroy(srcArray, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(srcDistgrid, rc=rc) ! destroy the DistGrid object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ArrayDestroy(dstArray, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(dstDistgrid, rc=rc) ! destroy the DistGrid object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! \begin{sloppypar}
! The transpose mode of {\tt ESMF\_ArrayRedist()} is not limited to
! distributed dimensions of Arrays. The {\tt srcToDstTransposeMap} argument
! can be used to transpose undistributed dimensions in the same manner.
! Furthermore transposing distributed and undistributed dimensions between
! Arrays is also supported.
! \end{sloppypar}
!
! The {\tt srcArray} used in the following examples is of rank 4 with 2 
! distributed and 2 undistributed dimensions. The distributed dimensions
! are the two first dimensions of the Array and are distributed according to the
! {\tt srcDistgrid} which describes a total index space region of 100 x 200
! elements. The last two Array dimensions are undistributed dimensions of size
! 2 and 3, respectively.
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=4, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  srcDistgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/100,200/), &
    rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  srcArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=srcDistgrid, &
    undistLBound=(/1,1/), undistUBound=(/2,3/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The first {\tt dstArray} to consider is defined on a DistGrid that also 
! describes a 100 x 200 index space region. The distribution indicated
! by {\tt dstDistgrid} may be different from the source distribution. Again
! the first two Array dimensions are associated with the DistGrid dimensions in
! sequence. Furthermore, the last two Array dimensions are undistributed
! dimensions, however, the sizes are 3 and 2, respectively.
!EOE
!BOC
  dstDistgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/100,200/), &
    rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  dstArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=dstDistgrid, &
    undistLBound=(/1,1/), undistUBound=(/3,2/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The desired mapping between {\tt srcArray} and {\tt dstArray} dimensions
! is expressed by {\tt srcToDstTransposeMap = (/1,2,4,3/)}, transposing only
! the two undistributed dimensions.
!EOE
!BOC
  call ESMF_ArrayRedistStore(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, srcToDstTransposeMap=(/1,2,4,3/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArrayRedistRelease(routehandle=redistHandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_ArrayDestroy(dstArray, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Next consider a {\tt dstArray} that is defined on the same {\tt dstDistgrid},
! but with a different order of Array dimensions. The desired order is
! specified during Array creation using the argument 
! {\tt distgridToArrayMap = (/2,3/)}. This map associates the first and second
! DistGrid dimensions with the second and third Array dimensions, respectively,
! leaving Array dimensions one and four undistributed.
!EOE  

!BOC
  dstArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=dstDistgrid, &
    distgridToArrayMap=(/2,3/), undistLBound=(/1,1/), undistUBound=(/3,2/), &
    rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOE
! Again the sizes of the undistributed dimensions are chosen in reverse order
! compared to {\tt srcArray}. The desired transpose mapping in this case will
! be {\tt srcToDstTransposeMap = (/2,3,4,1/)}.
!EOE

!BOC
  call ESMF_ArrayRedistStore(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, srcToDstTransposeMap=(/2,3,4,1/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArrayRedistRelease(routehandle=redistHandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_ArrayDestroy(dstArray, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(dstDistgrid, rc=rc) ! destroy the DistGrid object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Finally consider the case where {\tt dstArray} is constructed on a 
! 200 x 3 index space and where the undistributed dimensions are of size
! 100 and 2.
!EOE

!BOC
  dstDistgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/200,3/), &
    rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  dstArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=dstDistgrid, &
    undistLBound=(/1,1/), undistUBound=(/100,2/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! By construction {\tt srcArray} and {\tt dstArray} hold the same number of
! elements, albeit in a very different layout. Nevertheless, with a
! {\tt srcToDstTransposeMap} that maps matching dimensions from source to
! destination an Array redistribution becomes a well defined operation between
! {\tt srcArray} and {\tt dstArray}.
!EOE

!BOC
  call ESMF_ArrayRedistStore(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, srcToDstTransposeMap=(/3,1,4,2/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArrayRedistRelease(routehandle=redistHandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------

  call ESMF_ArrayDestroy(srcArray, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(srcDistgrid, rc=rc) ! destroy the DistGrid object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ArrayDestroy(dstArray, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(dstDistgrid, rc=rc) ! destroy the DistGrid object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! \begin{sloppypar}
! The default mode of Array redistribution, i.e. without providing a
! {\tt srcToDstTransposeMap} to {\tt ESMF\_ArrayRedistStore()}, also supports
! undistributed Array dimensions. The requirement in this case is that the 
! total undistributed element count, i.e. the product of the sizes of all
! undistributed dimensions, be the same for source and destination Array.
! In this mode the number of undistributed dimensions need not match between
! source and destination.
! \end{sloppypar}
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=4, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  srcDistgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/4,1/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  srcArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=srcDistgrid, &
    undistLBound=(/1,1/), undistUBound=(/2,4/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  dstDistgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/1,4/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
!BOC
  dstArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=dstDistgrid, &
    distgridToArrayMap=(/2,3/), undistLBound=(/1,1/), undistUBound=(/2,4/), &
    rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Both {\tt srcArray} and {\tt dstArray} have two undistributed dimensions and
! a total count of undistributed elements of $ 2 \times 4 = 8$.
!
! The Array redistribution operation is defined in terms of sequentialized
! undistributed dimensions. In the above case this means that a unique sequence
! index will be assigned to each of the 8 undistributed elements. The sequence
! indices will be 1, 2, ..., 8, where sequence index 1 is assigned to the first
! element in the first (i.e. fastest varying in memory) undistributed dimension.
! The following undistributed elements are labeled in consecutive order as they
! are stored in memory.
!EOE
!BOC
  call ESMF_ArrayRedistStore(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
!BOE
! The redistribution operation by default applies the identity operation between
! the elements of undistributed dimensions. This means that source element with
! sequence index 1 will be mapped against destination element with sequence
! index 1 and so forth. Because of the way source and destination Arrays
! in the current example were constructed this corresponds to a mapping of
! dimensions 3 and 4 on {\tt srcArray} to dimensions 1 and 4 on {\tt dstArray},
! respectively.
!EOE
!BOC
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArrayRedistRelease(routehandle=redistHandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ArrayDestroy(dstArray, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Array redistribution does {\em not} require the same number of undistributed
! dimensions in source and destination Array, merely the total number of
! undistributed elements must match.
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=3, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  dstArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=dstDistgrid, &
    distgridToArrayMap=(/1,3/), undistLBound=(/11/), undistUBound=(/18/), &
    rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! This {\tt dstArray} object only has a single undistributed dimension, while
! the {\tt srcArray}, defined further back, has two undistributed dimensions.
! However, the total undistributed element count for both Arrays is 8.
!EOE
!BOC
  call ESMF_ArrayRedistStore(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
!BOE
! In this case the default identity operation between the elements of
! undistributed dimensions corresponds to a {\em merging} of dimensions
! 3 and 4 on {\tt srcArray} into dimension 2 on {\tt dstArray}.
!EOE
!BOC
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArrayRedistRelease(routehandle=redistHandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! THIS IS WHERE I NEED TO ADD:
!!!! - example for new ArrayRedistStore() interface that allows override of
!!!!   default identity operation in tensor space -> transpose of undistr. dims.
!!!! - example that demonstrates transposing between distributed and undistr. d


  call ESMF_ArrayDestroy(srcArray, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(srcDistgrid, rc=rc) ! destroy the DistGrid object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ArrayDestroy(dstArray, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(dstDistgrid, rc=rc) ! destroy the DistGrid object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
10 continue
  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_ArrayRedistEx.F90"
  else
    print *, "FAIL: ESMF_ArrayRedistEx.F90"
  endif
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
  
end program
