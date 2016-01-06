! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
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

program ESMF_ArrayEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  
  implicit none
  
  ! local variables
  integer:: rc, i, j, k, petCount, localDeCount, localPet, localDe
  type(ESMF_VM):: vm
  type(ESMF_DELayout):: delayout
  type(ESMF_DistGrid):: distgrid, distgrid3D, distgrid2D, distgrid1D
  type(ESMF_ArraySpec):: arrayspec
  type(ESMF_Array):: array, array1, array2, array1D, array2D, array3D
  type(ESMF_Array):: arrayTracer, arrayScalar
  type(ESMF_LocalArray), allocatable:: larrayList(:)
  type(ESMF_LocalArray), allocatable:: larrayList1(:), larrayList2(:)
  real(ESMF_KIND_R8), pointer:: myFarray(:,:)
  real(ESMF_KIND_R8), pointer:: myFarray1D(:), myFarray3D(:,:,:)
  real(ESMF_KIND_R8), pointer:: myFarray2D(:,:)
  real(ESMF_KIND_R8):: dummySum
  type(ESMF_Index_Flag):: indexflag
  integer, allocatable:: minIndex(:,:), maxIndex(:,:), regDecomp(:,:)
  integer, allocatable:: connectionList(:,:)
  integer, allocatable:: arrayToDistGridMap(:)
  integer, allocatable:: localDeToDeMap(:)
  integer, allocatable:: exclusiveLBound(:,:), exclusiveUBound(:,:)
  integer, allocatable:: totalLWidth(:,:), totalUWidth(:,:)
  integer, allocatable:: totalLBound(:,:), totalUBound(:,:)
!  integer, allocatable:: totalElementMask(:,:)
  integer, allocatable:: computationalLWidth(:,:), computationalUWidth(:,:)
  integer, allocatable:: computationalLBound(:,:), computationalUBound(:,:)
!  integer, allocatable:: haloLDepth(:), haloUDepth(:)
!  type(ESMF_RouteHandle):: haloHandle, haloHandle2
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg


  ! result code
  integer :: finalrc, result
  
   finalrc = ESMF_SUCCESS
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_ArrayEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_Initialize(vm=vm, defaultlogfilename="ArrayEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (petCount /= 4) then
    finalrc = ESMF_FAILURE
    goto 10
  endif
  
!BOE
! \subsubsection{Create Array with automatic memory allocation}
!
! In the examples of the previous sections the user provided memory allocations
! for each of the DE-local regions for an Array object. The user was able to 
! use any of the Fortran methods to allocate memory, or go through
! the {\tt ESMF\_LocalArray} interfaces to obtain memory allocations before
! passing them into ArrayCreate(). Alternatively ESMF offers methods that 
! handle Array memory allocations inside the library.
! 
! As before, to create an {\tt ESMF\_Array} object an {\tt ESMF\_DistGrid}
! must be created. The DistGrid object holds information about the entire 
! index space and how it is decomposed into DE-local exclusive regions. The 
! following line of code creates a DistGrid for a 5x5 global index space that 
! is decomposed into 2 x 3 = 6 DEs.
!EOE
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The following is a representation of the index space and its decomposition into
! DEs. Each asterisk (*) represents a single element.
!
! \begin{verbatim}
! 
!  +---------------------------------------> 2nd dimension
!  |  (1,1)
!  |    +-----------+-----------+------+
!  |    | DE 0      | DE 2      | DE 4 |
!  |    |           |           |      |
!  |    |  *    *   |  *    *   |  *   |
!  |    |           |           |      |
!  |    |  *    *   |  *    *   |  *   |
!  |    |           |           |      |
!  |    |  *    *   |  *    *   |  *   |
!  |    +-----------+-----------+------+
!  |    |           |           |      |
!  |    | DE 1      | DE 3      | DE 5 |
!  |    |           |           |      |
!  |    |  *    *   |  *    *   |  *   |
!  |    |           |           |      |
!  |    |  *    *   |  *    *   |  *   |
!  |    +-----------+-----------+------+
!  |                                 (5,5)
!  v 
! 1st dimension
!
! \end{verbatim}
!
! Besides the DistGrid it is the {\em type, kind} and {\em rank} information,
! "tkr" for short, that is required to create an Array object. It turns out that
! the rank of the Array object is fully determined by the DistGrid and other 
! (optional) arguments passed into ArrayCreate(), so that explicit 
! specification of the Array rank is redundant.
!
! The simplest way to supply the type and kind information of the Array is
! directly through the {\tt typekind} argument. Here a double precision Array
! is created on the previously created DistGrid. Since no other arguments are
! specified that could alter the rank of the Array it becomes equal to the 
! dimCount of the DistGrid, i.e a 2D Array is created on top of the DistGrid.
!EOE
!BOC
  array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, distgrid=distgrid, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The different methods on how an Array object is created have no effect on
! the use of {\tt ESMF\_ArrayDestroy()}.
!EOE
!BOC
  call ESMF_ArrayDestroy(array, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Alternatively the same Array can be created specifying the "tkr" information
! in form of an ArraySpec variable. The ArraySpec explicitly contains the 
! Array rank and thus results in an over specification on the ArrayCreate()
! interface. ESMF checks all input information for consistency and returns 
! appropriate error codes in case any inconsistencies are found.
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!  call ESMF_ArrayPrint(array, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The Array object created by the above call is an ESMF distributed 
! object. As such it must follow the ESMF convention that requires that 
! the call to {\tt ESMF\_ArrayCreate()} must be issued in unison by all 
! PETs of the current context.
!

! \subsubsection{Native language memory access}
! \label{Array_native_language_localde}
!
! There are two different methods by which the user can access the data held 
! inside an ESMF Array object. The first method provides direct access to a
! native language array object. Specifically, the {\tt farrayPtr} argument
! returned by {\tt ESMF\_ArrayGet()} is a Fortran array pointer that can be
! used to access the PET-local data inside the Array object.
!
! Many applications work in the 1 DE per PET mode, with exactly one DE on 
! every PET. Accessing the Array memory on each PET for this situation is 
! especially simple as is shown in section \ref{Array_from_native_1_to_1}.
! However, the Array class is not restricted to the special 1 DE per PET case,
! but supports multiple separate memory allocations on each PET.
! The number of such PET-local allocations is given by the {\tt localDeCount},
! i.e. there is one memory allocation for every DE that is associated with the
! local PET.
!
! Access to a specific local memory allocation of an Array object is still
! accomplished by returning the {\tt farrayPtr} argument. However, for
! $ localDeCount > 1 $ the formally optional {\tt localDe} argument to
! {\tt ESMF\_ArrayGet()} turns into a practically required argument. While
! in general the {\tt localDe} in ESMF is simply a local index variable that 
! enumerates the DEs that are associated with the local PET (e.g. see section
! \ref{DELayout_general_mapping}), the bounds of this index variable are 
! strictly defined as {\tt [0,...,localDeCount-1]} when it is used as an 
! input argument. The following code demonstrates this.
!
! First query the Array for {\tt localDeCount}. This number may be different
! on each PET and indicates how many DEs are mapped against the local PET.
!EOE
!BOC
  call ESMF_ArrayGet(array, localDeCount=localDeCount, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Looping the {\tt localDe} index variable from 0 to {\tt localDeCount-1} allows
! access to each of the local memory allocations of the Array object:
!EOE
!BOC
  do localDe=0, localDeCount-1
    call ESMF_ArrayGet(array, farrayPtr=myFarray, localDe=localDe, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    ! use myFarray to access local DE data
  enddo
!EOC
!
!BOE
! The second method to access the memory allocations in an Array object is to
! go through the ESMF LocalArray object. To this end the Array is queried
! for a list of PET-local LocalArray objects. The LocalArray objects in the list
! correspond to the DEs on the local PET. Here the {\tt localDe} argument is
! solely a user level index variable, and in principle the lower bound can be 
! chosen freely. However, for better alignment with the previous case (where 
! {\tt localDe} served as an input argument to an ESMF method) the following
! example again fixes the lower bound at zero.
!EOE
!BOC
  allocate(larrayList(0:localDeCount-1))
  call ESMF_ArrayGet(array, localarrayList=larrayList, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  do localDe=0, localDeCount-1
    call ESMF_LocalArrayGet(larrayList(localDe), myFarray, &
       datacopyflag=ESMF_DATACOPY_REFERENCE, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    ! use myFarray to access local DE data
  enddo
!EOC
!BOE
! See section \ref{Array:LocalArray} for more on LocalArray usage in Array. 
! In most cases memory access through a LocalArray list is less convenient than
! the direct {\tt farrayPtr} method because it adds an extra object level 
! between the ESMF Array and the native language array. 
!
!
! \subsubsection{Regions and default bounds}
! \label{Array_regions_and_default_bounds}
!
! Each {\tt ESMF\_Array} object is decomposed into DEs as specified by the
! associated {\tt ESMF\_DistGrid} object. Each piece of this decomposition, i.e.
! each DE, holds a chunk of the Array data in its own local piece of memory.
! The details of the Array decomposition are described in the following 
! paragraphs.
!
! At the center of the Array decomposition is the {\tt ESMF\_DistGrid} class.
! The DistGrid object specified during Array creation contains three essential
! pieces of information:
! \begin{itemize}
! \item The extent and topology of the global domain covered by the Array object
!       in terms of indexed elements. The total extent may be a composition of
!       smaller logically rectangular (LR) domain pieces called tiles.
! \item The decomposition of the entire domain into "element exclusive" DE-local
!       LR chunks. {\em Element exclusive} means that there is no element overlap
!       between DE-local chunks. This, however, does not exclude degeneracies 
!       on edge boundaries for certain topologies (e.g. bipolar).
! \item The layout of DEs over the available PETs and thus the distribution of
!       the Array data.
! \end{itemize}
!
! Each element of an Array is associated with a {\em single} DE. The union of
! elements associated with a DE, as defined by the DistGrid above, corresponds
! to a LR chunk of index space, called the {\em exclusive region} of the DE.
!
! There is a hierarchy of four regions that can be identified for each DE in an
! Array object. Their definition and relationship to each other is as follows:
! \begin{itemize}
! \item {\em Interior Region}: Region that only contains local elements that are
!       {\em not} mapped into the halo of any other DE. The shape and size of 
!       this region for a particular DE depends non-locally on the halos defined
!       by other DEs and may change during computation as halo operations are
!       precomputed and released. Knowledge of the interior elements may be used
!       to improve performance by overlapping communications with ongoing 
!       computation for a DE.
! \item {\em Exclusive Region}: Elements for which a DE claims exclusive
!       ownership. Practically this means that the DE will be the sole source
!       for these elements in halo and reduce operations. There are exceptions
!       to this in some topologies. The exclusive region includes all elements
!       of the interior region.
! \item {\em Computational Region}: Region that can be set arbitrarily within
!       the bounds of the total region (defined next). The typical use of the
!       computation region is to define bounds that only include elements that
!       are updated by a DE-local computation kernel. The computational region
!       does not need to include all exclusive elements and it may also contain
!       elements that lie outside the exclusive region.
! \item {\em Total (Memory) Region}: Total of all DE-locally allocated elements.
!       The size and shape of the total memory region must accommodate the
!       union of exclusive and computational region but may contain 
!       additional elements. Elements outside the exclusive region may overlap
!       with the exclusive region of another DE which makes them potential 
!       receivers for Array halo operations. Elements outside the exclusive
!       region that do not overlap with the exclusive region of another DE
!       can be used to set boundary conditions and/or serve as extra memory 
!       padding.
! \end{itemize}
!
! \begin{verbatim}
!
!   +-totalLBound(:)----------------------------------+
!   |\                                                |
!   | \ <--- totalLWidth(:)                           |
!   |  \                                              |
!   |   +-computationalLBound(:)------------------+   |
!   |   |\                                        |   |
!   |   | \ <--- computationalLWidth(:)           |   |
!   |   |  \                                      |   |
!   |   |   +-exclusiveLBound(:)-------------+    |   |
!   |   |   |                                |    |   |
!   |   |   |     +------+      +-----+      |    |   |
!   |   |   |     |      |      |     |      |    |   |
!   |   |   |     |      +------+     |      |    |   |
!   |   |   |     | "Interior Region" |      |    |   |
!   |   |   |     +-----+             |      |    |   |
!   |   |   |           |             |      |    |   |
!   |   |   |           +-------------+      |    |   |
!   |   |   |                                |    |   |
!   |   |   | "Exclusive Region"             |    |   |
!   |   |   +-------------exclusiveUBound(:)-+    |   |
!   |   |                                     \   |   |
!   |   |           computationalUWidth(:) --> \  |   |
!   |   |                                       \ |   |
!   |   | "Computational Region"                 \|   |
!   |   +------------------computationalUBound(:)-+   |
!   |                                              \  | 
!   |                             totalUWidth(:) -> \ | 
!   | "Total Region"                                 \| 
!   +--------------------------------- totalUBound(:)-+
! \end{verbatim}
!
!
! With the following definitions:
! \begin{verbatim}
!
! computationalLWidth(:) = exclusiveLBound(:) - computationalLBound(:)
! computationalUWidth(:) = computationalUBound(:) - exclusiveUBound(:)
!
! \end{verbatim}
! and
! \begin{verbatim}
!
! totalLWidth(:) = exclusiveLBound(:) - totalLBound(:)
! totalUWidth(:) = totalUBound(:) - exclusiveUBound(:)
!
! \end{verbatim}
!
!
! The {\em exclusive region} is determined during Array creation by the 
! DistGrid argument. Optional arguments may be used to specify the 
! {\em computational region} when the Array is created, by default it will be
! set equal to the exclusive region. The {\em total region}, i.e. the actual
! memory allocation for each DE, is also determined during Array creation. When
! creating the Array object from existing Fortran arrays the total region is
! set equal to the memory provided by the Fortran arrays. Otherwise the 
! default is to allocate as much memory as is needed to accommodate the union
! of the DE-local exclusive and computational region. Finally it is also
! possible to use optional arguments to the ArrayCreate() call to specify the
! total region of the object explicitly.
!
! The {\tt ESMF\_ArrayCreate()} call checks that the input parameters are
! consistent and will result in an Array that fulfills all of the above 
! mentioned requirements for its DE-local regions.
!
! Once an Array object has been created the exclusive and total regions are
! fixed. The computational region, however, may be adjusted within the limits
! of the total region using the {\tt ArraySet()} call.
!
! The {\em interior region} is very different from the other regions in that
! it cannot be specified. The {\em interior region} for each DE is a {\em
! consequence} of the choices made for the other regions collectively across
! all DEs into which an Array object is decomposed. An Array object can be
! queried for its DE-local {\em interior regions} as to offer additional
! information to the user necessary to write more efficient code.
! %See section 
! %\ref{ArrayEx_interiorRegion}(not yet implemented) for more details.
!
! By default the bounds of each DE-local {\em total region} are defined as
! to put the start of the DE-local {\em exclusive region} at the "origin" of 
! the local index space, i.e. at {\tt (1, 1, ..., 1)}. With that definition the
! following loop will access each element of the DE-local memory segment for
! each PET-local DE of the Array object used in the previous sections and
! print its content.
!EOE
!
!BOC
  do localDe=0, localDeCount-1
    call ESMF_LocalArrayGet(larrayList(localDe), myFarray, &
       datacopyflag=ESMF_DATACOPY_REFERENCE, rc=rc)
    do i=1, size(myFarray, 1)
      do j=1, size(myFarray, 2)
        print *, "localPET=", localPet, " localDE=", &
            localDe, ": array(",i,",",j,")=", myFarray(i,j)
      enddo
    enddo
  enddo
!EOC
!BOE

! \subsubsection{Array bounds}
!
! The loop over Array elements at the end of the last section only works
! correctly because of the default definition of the {\em computational} and
! {\em total regions} used in the example. In general, without such specific
! knowledge about an Array object, it is necessary to use a more formal approach
! to access its regions with DE-local indices.
!
! The DE-local {\em exclusive region} takes a central role in the definition
! of Array bounds. Even as the {\em computational region} may adjust during 
! the course of execution the {\em exclusive region} remains unchanged.
! The {\em exclusive region} provides a unique reference frame
! for the index space of all Arrays associated with the same DistGrid.
!
! There is a choice between two indexing options that needs to be made during 
! Array creation. By default each DE-local exclusive region starts at 
! {\tt (1, 1, ..., 1)}. However, for some computational kernels it may be more
! convenient to choose the index bounds of the DE-local exclusive regions to 
! match the index space coordinates as they are defined in the corresponding
! DistGrid object. The second option is only available if the DistGrid object 
! does not contain any non-contiguous decompositions (such as cyclically
! decomposed dimensions).
!
! The following example code demonstrates the safe way of dereferencing the
! DE-local exclusive regions of the previously created {\tt array} object.
!
!
!BOC
  allocate(exclusiveUBound(2, 0:localDeCount-1))  ! dimCount=2
  allocate(exclusiveLBound(2, 0:localDeCount-1))  ! dimCount=2
  call ESMF_ArrayGet(array, indexflag=indexflag, &
    exclusiveLBound=exclusiveLBound, exclusiveUBound=exclusiveUBound, rc=rc)
  if (indexflag == ESMF_INDEX_DELOCAL) then
    ! this is the default
!    print *, "DE-local exclusive regions start at (1,1)"
    do localDe=0, localDeCount-1
      call ESMF_LocalArrayGet(larrayList(localDe), myFarray, &
          datacopyflag=ESMF_DATACOPY_REFERENCE, rc=rc)
      do i=1, exclusiveUBound(1, localDe)
        do j=1, exclusiveUBound(2, localDe)
!          print *, "DE-local exclusive region for localDE=", localDe, &
!            ": array(",i,",",j,")=", myFarray(i,j)
        enddo
      enddo
    enddo
  else if (indexflag == ESMF_INDEX_GLOBAL) then
    ! only if set during ESMF_ArrayCreate()
!    print *, "DE-local exclusive regions of this Array have global bounds"
    do localDe=0, localDeCount-1
      call ESMF_LocalArrayGet(larrayList(localDe), myFarray, &
         datacopyflag=ESMF_DATACOPY_REFERENCE, rc=rc)
      do i=exclusiveLBound(1, localDe), exclusiveUBound(1, localDe)
        do j=exclusiveLBound(2, localDe), exclusiveUBound(2, localDe)
!          print *, "DE-local exclusive region for localDE=", localDe, &
!            ": array(",i,",",j,")=", myFarray(i,j)
        enddo
      enddo
    enddo
  endif
  call ESMF_ArrayDestroy(array, rc=rc) ! destroy the array object
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Obviously the second branch of this simple code will work for either case, 
! however, if a complex computational kernel was written assuming 
! {\tt ESMF\_INDEX\_DELOCAL} type bounds the second branch would simply be 
! used to indicate the problem and bail out.
!
! The advantage of the {\tt ESMF\_INDEX\_GLOBAL} index option is that
! the Array bounds directly contain information on where the DE-local
! Array piece is located in a global index space sense. When the
! {\tt ESMF\_INDEX\_DELOCAL} option is used the correspondence between local
! and global index space must be made by querying the associated DistGrid for
! the DE-local {\tt indexList} arguments.


! \subsubsection{Computational region and extra elements for halo or padding}
! \label{Array:padding}
!
! In the previous examples the computational region of {\tt array} was chosen 
! by default to be identical to the exclusive region defined by the DistGrid
! argument during Array creation. In the following the same {\tt arrayspec} and
! {\tt distgrid} objects as before will be used to create an Array but now a 
! larger computational region shall be defined around each DE-local exclusive 
! region. Furthermore, extra space will be defined around the computational
! region of each DE to accommodate a halo and/or serve as memory padding.
!
! In this example the {\tt indexflag} argument is set to 
! {\tt ESMF\_INDEX\_GLOBAL} indicating that the bounds of the exclusive region
! correspond to the index space coordinates as they are defined by the DistGrid
! object.
!
! The same {\tt arrayspec} and {\tt distgrid} objects as before are used
! which also allows the reuse of the already allocated {\tt larrayList}
! variable.
!EOE
!BOC
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    computationalLWidth=(/0,3/), computationalUWidth=(/1,1/), &
    totalLWidth=(/1,4/), totalUWidth=(/3,1/), &
    indexflag=ESMF_INDEX_GLOBAL, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_ArrayPrint(array, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Obtain the {\tt larrayList} on every PET.
!EOE
!BOC
  allocate(localDeToDeMap(0:localDeCount-1))
  call ESMF_ArrayGet(array, localarrayList=larrayList, &
    localDeToDeMap=localDeToDeMap, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The bounds of DE 1 for {\tt array} are shown in the following 
! diagram to illustrate the situation. Notice that the {\tt totalLWidth} and
! {\tt totalUWidth} arguments in the ArrayCreate() call define the total region 
! with respect to the exclusive region given for each DE by the {\tt distgrid} 
! argument.
! 
! \begin{verbatim}
!      +-(3,-3)---------------------------------+ 
!      |\                                       | 
!      | +-(4,-2)-+-(4,1)--------------------+--+ 
!      | |        |                          |  | 
!      | |        |                          |  | 
!      | |        |          DE 1            |  | 
!      | |        |                          |  | 
!      | |        |                          |  | 
!      | |        | Exclusive Region         |  | 
!      | |        +--------------------(5,2)-+  | 
!      | | Computational Region                 | 
!      | +-------------------------------(6,3)--+ 
!      |                                        | 
!      | Total Region                           | 
!      +---------------------------------(8,3)--+ 
! \end{verbatim}
!
! When working with this {\tt array} it is possible for the computational
! kernel to overstep the exclusive region for both read/write access 
! (computational region) and potentially read-only access into the total region
! outside of the computational region, if a halo operation provides valid 
! entries for these elements. 
!
! The Array object can be queried for absolute {\em bounds}
!EOE
!BOC
  allocate(computationalLBound(2, 0:localDeCount-1))  ! dimCount=2
  allocate(computationalUBound(2, 0:localDeCount-1))  ! dimCount=2
  allocate(totalLBound(2, 0:localDeCount-1))          ! dimCount=2
  allocate(totalUBound(2, 0:localDeCount-1))          ! dimCount=2
  call ESMF_ArrayGet(array, exclusiveLBound=exclusiveLBound, &
    exclusiveUBound=exclusiveUBound, &
    computationalLBound=computationalLBound, &
    computationalUBound=computationalUBound, &
    totalLBound=totalLBound, &
    totalUBound=totalUBound, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! or for the relative {\em widths}.
!EOE
!BOC
  allocate(computationalLWidth(2, 0:localDeCount-1))  ! dimCount=2
  allocate(computationalUWidth(2, 0:localDeCount-1))  ! dimCount=2
  allocate(totalLWidth(2, 0:localDeCount-1))          ! dimCount=2
  allocate(totalUWidth(2, 0:localDeCount-1))          ! dimCount=2
  call ESMF_ArrayGet(array, computationalLWidth=computationalLWidth, &
    computationalUWidth=computationalUWidth, totalLWidth=totalLWidth, &
    totalUWidth=totalUWidth, rc=rc)
!EOC
!BOE
! Either way the dereferencing of Array data is centered around the DE-local
! exclusive region:
!EOE
!BOC
  do localDe=0, localDeCount-1
    call ESMF_LocalArrayGet(larrayList(localDe), myFarray, &
       datacopyflag=ESMF_DATACOPY_REFERENCE, rc=rc)
    ! initialize the DE-local array
    myFarray = 0.1d0 * localDeToDeMap(localDe)
    ! first time through the total region of array    
!    print *, "myFarray bounds for DE=", localDeToDeMap(localDe), &
!      lbound(myFarray),  ubound(myFarray)
    do j=exclusiveLBound(2, localDe), exclusiveUBound(2, localDe)
      do i=exclusiveLBound(1, localDe), exclusiveUBound(1, localDe)
!        print *, "Excl region DE=", localDeToDeMap(localDe), &
!        ": array(",i,",",j,")=",  myFarray(i,j)
      enddo
    enddo
    do j=computationalLBound(2, localDe), computationalUBound(2, localDe)
      do i=computationalLBound(1, localDe), computationalUBound(1, localDe)
!        print *, "Excl region DE=", localDeToDeMap(localDe), &
!        ": array(",i,",",j,")=", myFarray(i,j)
      enddo
    enddo
    do j=totalLBound(2, localDe), totalUBound(2, localDe)
      do i=totalLBound(1, localDe), totalUBound(1, localDe)
!        print *, "Total region DE=", localDeToDeMap(localDe), &
!        ": array(",i,",",j,")=", myFarray(i,j)
      enddo
    enddo

    ! second time through the total region of array    
    do j=exclusiveLBound(2, localDe)-totalLWidth(2, localDe), &
      exclusiveUBound(2, localDe)+totalUWidth(2, localDe)
      do i=exclusiveLBound(1, localDe)-totalLWidth(1, localDe), &
        exclusiveUBound(1, localDe)+totalUWidth(1, localDe)
!        print *, "Excl region DE=", localDeToDeMap(localDe), &
!        ": array(",i,",",j,")=", myFarray(i,j)
      enddo
    enddo
  enddo
!EOC

  deallocate(larrayList)
  deallocate(localDeToDeMap)
  call ESMF_ArrayDestroy(array, rc=rc) ! finally destroy the array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!!!! UNTIL FURTHER IMPLEMENTATION SKIP SECTIONS OF THIS EXAMPLE >>>>>>>>>>>>>>>>
#ifdef NOSKIP
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

!BOEI
! 
! The index space topology of this example is very simple. The DistGrid is
! defined by a single LR tile and does not contain any extra connections.
! Consequently,
! DEs that are located at the edge of the index space may contain DE-local 
! computational and total regions that reach beyond the index space as it is
! defined by 
! the DistGrid. For example DE 1's computational and total region, as shown 
! in the diagram above, contain extra elements that are not covered by the
! index space described by the DistGrid object. 
!
! The situation is different for interior DEs, or when the widths specified
! during Array creation are asymmetric. For the current example DE 3, for
! instance,
! only contains elements in its total and computational region that lie within
! the DistGrid index space. Elements in the total region that are outside of a 
! DE's exclusive region are then redundant with another DE's exclusive elements. 
! Redundancies in the computational region may be enforced or monitored by
! special Array communication calls. Total region elements that are outside the 
! computational region may be part of a halo operation and may be updated with 
! the value of the corresponding exclusive element in another DE using halo 
! communication methods.
!
! Computational elements that are outside the DistGrid index space are kept for 
! convenience, simplifying the formulation of the computational kernel. 
! Naturally, redundancy cannot be checked or enforced for these elements. 
! Similarly, halo elements that do not correspond to elements within the index space
! of the DistGrid will not be updated during halo communications. It is up to
! the application writer to utilize the provided elements as is appropriate for the
! computational algorithm. The Array class offers mask type information 
! through the {\tt ESMF\_ArrayGet()} method in order to assist the user in 
! dealing with elements that are not part of the Array index space as it is 
! defined in the corresponding DistGrid object.
! %Please see section 
! %\ref{ArrayEx_interiorRegion} for details.
!
!
! \subsubsection{Interior region and Array's total element mask}
! \label{ArrayEx_interiorRegion}
!
! The {\em Array total element mask} is a native Fortran integer array of
! the same rank as the ESMF Array object. The array must be allocated and 
! deallocated by the user and is filled by the ArrayGet() method with the
! mask information. (The Array object does not store the mask information
! internally (to save memory) but generates the information on the fly when
! inquired.)
!
! The array variable used as mask argument must be allocated to 
! accommodate the total region for the inquired DE. The bounds must
! be chosen to match the totalLBound(:) and totalUBound(:) parameters of the
! Array.
!
! For the {\tt array} object created in the previous section the total element mask
! for DE 1 would need to be allocated as
!EOEI
!BOCI
  allocate(totalElementMask(3:8, -3:3))              ! rank=2
!EOCI
!BOEI
! Then, assuming that DE 1 was localDE 1 on the PET issuing the following
! call the mask can be obtained in the following manner.
!EOEI
!BOCI
  call ESMF_ArrayGet(array, localDe=1, totalElementMask=totalElementMask, rc=rc)
!EOCI
!BOEI
! Now the {\tt totalElementMask} variable contains the mask for the total
! region for DE 1 for {\tt array}. For the example the mask will contain the
! following values:
!
! \begin{verbatim}
!
!  +-------------------------------------------> 2nd dimension
!  |  
!  |
!  |    (3,-3)
!  |      +-----------------------------+
!  |      | -1  -1  -1  -1   0   0   2  |
!  |      |    +-----------+-------+----+
!  |      | -1 |-1  -1  -1 |-2  -2 | 3  |
!  |      |    |           |       |    |
!  |      | -1 |-1  -1  -1 |-2  -2 | 3  |
!  |      |    |           +-------+    |
!  |      | -1 |-1  -1  -1  -1  -1  -1  |
!  |      |    +------------------------+
!  |      | -1  -1  -1  -1  -1  -1  -1  |
!  |      |                             |
!  |      | -1  -1  -1  -1  -1  -1  -1  |
!  |      +-----------------------------+
!  |                                  (8,3)
!  |
!  v 1st dimension
!
! \end{verbatim}
!
! The inner most rectangle corresponds to the exclusive region on DE 1. The
! value of "-2" in an exclusive element indicates that other DEs of the Array
! have elements in their total region that correspond to this DE's exclusive element.
! A value of "-3" for an exclusive element indicates that there exists a redundancy
! with another DE's exclusive region and a value of "-4" indicates redundancy 
! and dependency between DEs. Finally, a element value of "-1" in the exclusive
! region indicates that there are no other DEs in the Array that depend on this
! exclusive element. The union of exclusive elements with mask value "-1" define the 
! {\em absolute interior region} of the DE. In this example DE 1 does not 
! contain any absolute interior elements.
!
! The next larger rectangle in the above diagram corresponds to the 
! computational region of DE 1 and the entire array extent is that of this DE's
! total region. A element mask value of "-1" in these regions indicates that 
! there are no elements in any of the DEs' exclusive regions that corresponds to 
! this element, i.e. the element lies outside the index space covered by DistGrid.
!
! Mask values $>= 0$ in the computational and total regions indicate the DE 
! which contains the corresponding element in its exclusive region. Thus a element 
! with mask value $>= 0$ lies within the DistGrid index space.
!
! The query call used above was only able to return information based on the
! information stored in the Array object and the underlying DistGrid. However,
! it is often more valuable to obtain the information stored in a total element
! mask specifically for a particular Array communication pattern or a whole
! set thereof. The following lines of code demonstrate this aspect for one of 
! the Array halo operations used in the previous section.
!EOEI
!BOCI
  call ESMF_ArrayHaloStore(array, haloLDepth=(/0,0/), haloUDepth=(/0,1/), &
    routehandle=haloHandle2, rc=rc)
!EOCI
!BOEI
! The same {\tt totalElementMask} variable may be used to obtain the specific total
! element mask. Only difference is that the {\tt routehandle} must be provided
! as an additional input parameter. Since specific total element masks may be 
! required as an "OR" over a whole set of communication operations the optional
! input parameter is defined as a list of RouteHandles.
!EOEI
!BOCI
  call ESMF_ArrayGet(array, routehandlelist=(/haloHandle2/), &
    localDe=1, totalElementMask=totalElementMask, rc=rc)
!EOCI
!BOEI
! Now {\tt totalElementMask} holds the total element mask for {\tt array} specifically
! for the Route referenced by {\tt haloHandle2}. For DE1 the output will look
! as follows:
! \begin{verbatim}
!
!  +-------------------------------------------> 2nd dimension
!  |  
!  |
!  |    (3,-3)
!  |      +-----------------------------+
!  |      | -1  -1  -1  -1  -1  -1  -1  |
!  |      |    +-----------+-------+----+
!  |      | -1 |-1  -1  -1 |-1  -1 | 3  |
!  |      |    |           |       |    |
!  |      | -1 |-1  -1  -1 |-1  -1 | 3  |
!  |      |    |           +-------+    |
!  |      | -1 |-1  -1  -1  -1  -1  -1  |
!  |      |    +------------------------+
!  |      | -1  -1  -1  -1  -1  -1  -1  |
!  |      |                             |
!  |      | -1  -1  -1  -1  -1  -1  -1  |
!  |      +-----------------------------+
!  |                                  (8,3)
!  |
!  v 1st dimension
!
! \end{verbatim}
!
! The meaning of the values stored in the total element mask for a specific 
! query call are now related to the specified operations. The "-1" in the 
! exclusive region (inner most rectangle) indicates that the operation
! referenced by {\tt haloHandle2} requires none of the exclusive elements of DE 1.
! Thus all exclusive elements of DE 1 can be considered interior elements with 
! respect to {\tt haloHandle2}. Consequently, if a non-blocking HaloRun() were
! to be executed it would be safe to read from and write to all of DE 1's
! exclusive elements.
!
! For the next larger rectangle, the computational region, {\tt totalElementMask}
! indicates that there are only two elements that will be updated with values from 
! exclusive elements of DE 3 during execution of {\tt haloHandle2}, all other elements
! are unaffected by this communication pattern.
!
! Finally, none of the points in DE 1's total region outside of the 
! computational region are affected by {\tt haloHandle2}.
!
! After usage it is the user's responsibility to clean up.
!EOEI
!BOCI
  deallocate(totalElementMask)
  call ESMF_RouteHandleRelease(routehandle=haloHandle2, rc=rc)
  call ESMF_ArrayDestroy(array, rc=rc) ! destroy array object
!EOCI  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOEI
! 
!
!
! \subsubsection{Total region, memory padding and changing computational region}
!
! The default total region during Array creation is chosen as to exactly
! accommodate the computational region. Once an Array object has been created 
! the total region for each DE is fixed and cannot be changed. The computational
! width, however, may be changed using the ArraySet() call. Changes are allowed
! within the limits set by the fixed total region.
!
! The following code illustrates how an Array can be created to accommodate the
! largest total region that is expected to occur during the compute cycle. The 
! example also shows how the computational region may change during the compute
! cycle. Again the same {\tt arrayspec} and {\tt distgrid} arguments as before 
! are used.
!EOEI
!BOCI
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/3,3/), totalUWidth=(/3,3/), rc=rc)
!EOCI  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOEI
! This {\tt array} has DE-local total regions that are three elements wider in 
! each direction than the corresponding exclusive region. The default
! computational region for each DE is equal to the DE's exclusive region. The
! extra elements between exclusive and total region are available for halos or
! can be used to expand the computational region of the Array.
!
! The following compute loop first executes a halo update which by default
! will attempt to halo all elements that are outside of the computational region.
! Then the a three step cycle is entered which sets the computational region to
! 2, 1 and 0 elements around the exclusive region and each time calls a 
! computational kernel that will access elements from (i-1,j-1) to (i+1,j+1). 
! After the computational width has reached 0 a new halo updated needs to be
! performed.
!EOEI
!BOCI
  do j=1, 10
    call ESMF_ArrayHalo(array, rc=rc)
    do k=2, 0, -1
      call ESMF_ArraySet(array, &
        computationalLWidth=(/k,k/), computationalUWidth=(/k,k/), rc=rc)
      ! call second_order_kernel(array)  ! will access (i-1) and (i+1) elements
    enddo
  enddo
!EOCI
!BOCI
  call ESMF_ArrayDestroy(array, rc=rc) ! finally destroy the array object
!EOCI  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOEI
!EOCI
!BOEI
! Sometimes it may be desirable to add extra memory padding around the DE-local
! data elements, e.g. to prevent false sharing on shared memory architectures. 
! For this the total region can be chosen larger than is needed by the 
! algorithm. The following lines of code recapture the example from above but
! now the total region is chosen with a 5 element wide rim around the exclusive
! region. Still only a 3 element wide halo is needed for the computational
! algorithm. By default the ArrayHalo() call will halo all extra elements, to keep
! the halo to 3 elements in each direction the haloLDepth and haloUDepth arguments
! must be used. With that a 5 - 3 = 2 element wide memory padding has been placed
! around the elements used by each DE.
!
!EOEI
!BOCI
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/5,5/), totalUWidth=(/5,5/), rc=rc)
!EOCI  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOCI
  do j=1, 10
    call ESMF_ArrayHalo(array, haloLDepth=(/3,3/), haloUDepth=(/3,3/), rc=rc)
    do k=2, 0, -1
      call ESMF_ArraySet(array, &
        computationalLWidth=(/k,k/), computationalUWidth=(/k,k/), rc=rc)
      ! call second_order_kernel(array)  ! will access (i-1) and (i+1) elements
    enddo
  enddo
!EOCI
!BOEI
! Done, {\tt array} and {\tt distgrid} can be destroyed.
!EOEI
!BOCI  
  call ESMF_ArrayDestroy(array, rc=rc) ! finally destroy the array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOCI  

#else
  ! need to clean up distgrid that was created several sections ago
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#endif
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
!BOE
! \subsubsection{Create 1D and 3D Arrays}
!
! All previous examples were written for the 2D case. There is, however, no
! restriction within the Array or DistGrid class that limits the dimensionality
! of Array objects beyond the language specific limitations (7D for Fortran). 
!
! In order to create an {\tt n}-dimensional Array the rank indicated by both
! the {\tt arrayspec} and the {\tt distgrid} arguments specified during Array
! create must be equal to {\tt n}. A 1D Array of double precision real data
! hence requires the following {\tt arrayspec}.
!EOE  
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=1, rc=rc)
!EOC
!BOE
! The index space covered by the Array and the decomposition description is
! provided to the Array create method by the {\tt distgrid} argument. The index
! space in this example has 16 elements and covers the interval $[-10, 5]$. It is 
! decomposed into as many DEs as there are PETs in the current context.
!EOE
!BOC
  distgrid1D = ESMF_DistGridCreate(minIndex=(/-10/), maxIndex=(/5/), &
    regDecomp=(/petCount/), rc=rc)
!EOC
!BOE
! A 1D Array object with default regions can now be created.
!EOE
!BOC
  array1D = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid1D, rc=rc)
!EOC
  call ESMF_ArrayDestroy(array1D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(distgrid1D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! 
! The creation of a 3D Array proceeds analogous to the 1D case. The rank of the
! {\tt arrayspec} must be changed to 3
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=3, rc=rc)
!EOC
!BOE
! and an appropriate 3D DistGrid object must be created
!EOE
!BOC
  distgrid3D = ESMF_DistGridCreate(minIndex=(/1,1,1/), &
    maxIndex=(/16,16,16/), regDecomp=(/4,4,4/), rc=rc)
!EOC
!BOE
! before an Array object can be created.
!EOE
!BOC
  array3D = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid3D, rc=rc)
!EOC
!BOE
! The {\tt distgrid3D} object decomposes the 3-dimensional index space into
! $4\times 4\times 4 = 64$ DEs. These DEs are laid out across the computational
! resources (PETs) of the current component according to a default DELayout that
! is created during the DistGrid create call. Notice that in the index space 
! proposal a DELayout does not have a sense of dimensionality. The DELayout
! function is simply to map DEs to PETs. The DistGrid maps chunks of index space
! against DEs and thus its rank is equal to the number of index space 
! dimensions.
!
! The previously defined DistGrid and the derived Array object decompose 
! the index space along all three dimension. It is, however, not a requirement
! that the decomposition be along all dimensions. An Array with the same 3D
! index space could as well be decomposed along just one or along two of the
! dimensions. The following example shows how for the same index space only the
! last two dimensions are decomposed while the first Array dimension has full
! extent on all DEs.
!EOE
!BOC
  call ESMF_ArrayDestroy(array3D, rc=rc)
  call ESMF_DistGridDestroy(distgrid3D, rc=rc)
  distgrid3D = ESMF_DistGridCreate(minIndex=(/1,1,1/), &
    maxIndex=(/16,16,16/), regDecomp=(/1,4,4/), rc=rc)
  array3D = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid3D, rc=rc)
!EOC
!BOE

! \subsubsection{Working with Arrays of different rank}
! Assume a computational kernel that involves the {\tt array3D} object as it was
! created at the end of the previous section. Assume further that the kernel 
! also involves a 2D Array on a 16x16 index space where each point (j,k) was
! interacting with each (i,j,k) column of the 3D Array. An efficient formulation
! would require that the decomposition of the 2D Array must match that of the 3D
! Array and further the DELayout be identical. The following code shows how this
! can be accomplished.
!EOE
!BOC
  call ESMF_DistGridGet(distgrid3D, delayout=delayout, rc=rc) ! get DELayout
  distgrid2D = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/16,16/), &
    regDecomp=(/4,4/), delayout=delayout, rc=rc)
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
  array2D = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid2D, rc=rc)
!EOC
!BOE
! Now the following kernel is sure to work with {\tt array3D} and {\tt array2D}.
!EOE
!BOC
  call ESMF_DELayoutGet(delayout, localDeCount=localDeCount, rc=rc)
  allocate(larrayList1(0:localDeCount-1))
  call ESMF_ArrayGet(array3D, localarrayList=larrayList1, rc=rc)
  allocate(larrayList2(0:localDeCount-1))
  call ESMF_ArrayGet(array2D, localarrayList=larrayList2, rc=rc)
  do localDe=0, localDeCount-1
    call ESMF_LocalArrayGet(larrayList1(localDe), myFarray3D, &
      datacopyflag=ESMF_DATACOPY_REFERENCE, rc=rc)
    myFarray3D = 0.1d0 * localDe ! initialize
    call ESMF_LocalArrayGet(larrayList2(localDe), myFarray2D, &
      datacopyflag=ESMF_DATACOPY_REFERENCE, rc=rc)
    myFarray2D = 0.5d0 * localDe ! initialize
    do k=1, 4
      do j=1, 4
        dummySum = 0.d0
        do i=1, 16
          dummySum = dummySum + myFarray3D(i,j,k) ! sum up the (j,k) column
        enddo
        dummySum = dummySum * myFarray2D(j,k) ! multiply with local 2D element
!        print *, "dummySum(",j,k,")=",dummySum
      enddo
    enddo
  enddo
!EOC

!call ESMF_ArrayPrint(array3D)
!call ESMF_ArrayPrint(array2D)
  call ESMF_ArrayDestroy(array2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ArrayDestroy(array3D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(distgrid3D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!
! \subsubsection{Array and DistGrid rank -- 2D+1 Arrays}
!
! Except for the special Array create interface that implements a copy from
! an existing Array object all other Array create interfaces require the 
! specification of at least two arguments: {\tt farray} and {\tt distgrid},
! {\tt larrayList} and {\tt distgrid}, or {\tt arrayspec} and {\tt distgrid}.
! In all these cases both required arguments contain a sense of dimensionality.
! The relationship between these two arguments deserves extra attention.
!
! The first argument, {\tt farray}, {\tt larrayList} or {\tt arrayspec}, 
! determines the rank of the created Array object, i.e. the dimensionality
! of the actual data storage. The rank of a native language array, extracted 
! from an Array object, is equal to the rank specified by either of these
! arguments. So is the {\tt rank} that is returned by the {\tt ESMF\_ArrayGet()}
! call.
!
! The rank specification contained in the {\tt distgrid} argument, which is of 
! type {\tt ESMF\_DistGrid}, on the other hand has no affect on the 
! rank of the Array. The {\tt dimCount} specified by the DistGrid object,
! which may be equal, greater or less than the Array rank, determines the 
! dimensionality of the {\em decomposition}.
!
! While there is no constraint between DistGrid {\tt dimCount} and Array
! {\tt rank}, there is an important relationship between the two, resulting in
! the concept of index space dimensionality. Array dimensions can be
! arbitrarily mapped against DistGrid dimension, rendering them {\em decomposed}
! dimensions. The index space dimensionality is equal to the number of 
! decomposed Array dimensions.
!
! \begin{sloppypar}
! Array dimensions that are not mapped to DistGrid dimensions are the 
! {\em undistributed} dimensions of the Array. They are not part
! of the index space. The mapping is specified during {\tt ESMF\_ArrayCreate()}
! via the {\tt distgridToArrayMap} argument. DistGrid dimensions that have
! not been associated with Array dimensions are {\em replicating} dimensions.
! The Array will be replicated across the DEs that lie along replication
! DistGrid dimensions.
! \end{sloppypar}
!
! Undistributed Array dimensions can be used to store multi-dimensional data for
! each Array index space element. One application of this is to store the 
! components of a vector quantity in a single Array. The same 2D {\tt distgrid}
! object as before will be used.
!EOE
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The rank in the {\tt arrayspec} argument, however, must change from 2 to 3 in
! order to provide for the extra Array dimension.
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=3, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! During Array creation with extra dimension(s) it is necessary to specify the
! bounds of these undistributed dimension(s). This requires two additional
! arguments, {\tt undistLBound} and {\tt undistUBound}, which are vectors in 
! order to accommodate multiple undistributed dimensions. The other arguments
! remain unchanged and apply across all undistributed components. 
!
!EOE
!BOC
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/0,1/), totalUWidth=(/0,1/), &
    undistLBound=(/1/), undistUBound=(/2/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC
    
!  call ESMF_ArrayPrint(array, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOE
! This will create {\tt array} with 2+1 dimensions. The 2D DistGrid is used
! to describe decomposition into DEs with 2 Array dimensions mapped to the 
! DistGrid dimensions resulting in a 2D index space. The extra Array dimension
! provides storage for multi component user data within the Array object.
!
! By default the {\tt distgrid} dimensions are associated
! with the first Array dimensions in sequence. For the example above this means
! that the first 2 Array dimensions are decomposed according to the provided 2D
! DistGrid. The 3rd Array dimension does not have an associated DistGrid
! dimension, rendering it an undistributed Array dimension.
!
! Native language access to an Array with undistributed dimensions is in
! principle the same as without extra dimensions.
!EOE
!BOC
  call ESMF_ArrayGet(array, localDeCount=localDeCount, rc=rc)
  allocate(larrayList(0:localDeCount-1))
  call ESMF_ArrayGet(array, localarrayList=larrayList, rc=rc)
!BOE
! The following loop shows how a Fortran pointer to the DE-local data chunks
! can be obtained and used to set data values in the exclusive regions. The
! {\tt myFarray3D} variable must be of rank 3 to match the Array rank of
! {\tt array}. However, variables such as {\tt exclusiveUBound} that store the
! information about the decomposition, remain to be allocated for the 2D 
! index space.
!EOE
!BOC
  call ESMF_ArrayGet(array, exclusiveLBound=exclusiveLBound, &
    exclusiveUBound=exclusiveUBound, rc=rc)
  do localDe=0, localDeCount-1
    call ESMF_LocalArrayGet(larrayList(localDe), myFarray3D, &
       datacopyflag=ESMF_DATACOPY_REFERENCE, rc=rc)
    myFarray3D = 0.0 ! initialize
    myFarray3D(exclusiveLBound(1,localDe):exclusiveUBound(1,localDe), &
      exclusiveLBound(2,localDe):exclusiveUBound(2,localDe), &
      1) = 5.1 ! dummy assignment
    myFarray3D(exclusiveLBound(1,localDe):exclusiveUBound(1,localDe), &
      exclusiveLBound(2,localDe):exclusiveUBound(2,localDe), &
      2) = 2.5 ! dummy assignment
  enddo
  deallocate(larrayList)
!EOC
!  call ESMF_ArrayPrint(array, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! For some applications the default association rules between DistGrid and Array
! dimensions may not satisfy the user's needs. The optional {\tt distgridToArrayMap} 
! argument can be used during Array creation to explicitly specify the mapping 
! between DistGrid and Array dimensions. To demonstrate this the following lines
! of code reproduce the above example but with rearranged dimensions. Here the
! {\tt distgridToArrayMap} argument is a list with two elements corresponding to
! the DistGrid {\tt dimCount} of 2. The first element indicates which Array
! dimension the first DistGrid dimension is mapped against. Here the
! 1st DistGrid dimension maps against the 3rd Array dimension and the 2nd 
! DistGrid dimension maps against the 1st Array dimension. This leaves the 2nd
! Array dimension to be the extra and undistributed dimension in the resulting
! Array object.
!EOE
!BOC
  call ESMF_ArrayDestroy(array, rc=rc)
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    distgridToArrayMap=(/3, 1/), totalLWidth=(/0,1/), totalUWidth=(/0,1/), &
    undistLBound=(/1/), undistUBound=(/2/), rc=rc)
!EOC
!BOE
! Operations on the Array object as a whole are unchanged by the different
! mapping of dimensions.
!
! When working with Arrays that contain explicitly mapped Array and DistGrid 
! dimensions it is critical to know the order in which the entries of
! {\em width} and {\em bound} arguments that are associated with distributed
! Array dimensions are specified. The size of these arguments is equal to the
! DistGrid {\tt dimCount}, because the maximum number of distributed Array
! dimensions is given by the dimensionality of the index space.
!
! The order of dimensions in these arguments, however, is {\em not} that of
! the associated DistGrid. Instead each entry corresponds to the distributed
! Array dimensions in sequence. In the example above the entries in 
! {\tt totalLWidth} and {\tt totalUWidth} correspond to Array dimensions 1 and
! 3 in this sequence. 
!
! The {\tt distgridToArrrayMap} argument optionally provided during Array create
! indicates how the DistGrid dimensions map to Array dimensions. The inverse
! mapping, i.e. Array to DistGrid dimensions, is just as important. The 
! {\tt ESMF\_ArrayGet()} call offers both mappings as {\tt distgridToArrrayMap}
! and {\tt arrayToDistGridMap}, respectively. The number of elements in 
! {\tt arrayToDistGridMap} is equal to the rank of the Array. Each element
! corresponds to an Array dimension and indicates the associated DistGrid
! dimension by an integer number. An entry of "0" in {\tt arrayToDistGridMap}
! indicates that the corresponding Array dimension is undistributed.
!
! Correct understanding about the association between Array and DistGrid
! dimensions becomes critical for correct data access into the Array.
!EOE
!BOC
  allocate(arrayToDistGridMap(3))  ! arrayRank = 3
  call ESMF_ArrayGet(array, arrayToDistGridMap=arrayToDistGridMap, &
    exclusiveLBound=exclusiveLBound, exclusiveUBound=exclusiveUBound, &
    localDeCount=localDeCount, rc=rc)  
  if (arrayToDistGridMap(2) /= 0) then   ! check if extra dimension at 
    ! expected index indicate problem and bail out
  endif
  ! obtain larrayList for local DEs
  allocate(larrayList(0:localDeCount-1))
  call ESMF_ArrayGet(array, localarrayList=larrayList, rc=rc)
  do localDe=0, localDeCount-1
    call ESMF_LocalArrayGet(larrayList(localDe), myFarray3D, &
       datacopyflag=ESMF_DATACOPY_REFERENCE, rc=rc)
    myFarray3D(exclusiveLBound(1,localDe):exclusiveUBound(1,localDe), &
      1, exclusiveLBound(2,localDe):exclusiveUBound(2, &
      localDe)) = 10.5 !dummy assignment
    myFarray3D(exclusiveLBound(1,localDe):exclusiveUBound(1,localDe), &
      2, exclusiveLBound(2,localDe):exclusiveUBound(2, &
      localDe)) = 23.3 !dummy assignment
  enddo
  deallocate(exclusiveLBound, exclusiveUBound)
  deallocate(arrayToDistGridMap)
  deallocate(larrayList)
  call ESMF_ArrayDestroy(array, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC

!BOE
!
! \subsubsection{Arrays with replicated dimensions}
!
! Thus far most examples demonstrated cases where the DistGrid {\tt dimCount}
! was equal to the Array {\tt rank}. The previous section introduced the
! concept of Array {\em tensor} dimensions when {\tt dimCount < rank}. In this
! section {\tt dimCount} and {\tt rank} are assumed completely unconstrained and
! the relationship to {\tt distgridToArrayMap} and {\tt arrayToDistGridMap} will
! be discussed.
!
! The Array class allows completely arbitrary mapping between Array and
! DistGrid dimensions. Most cases considered in the previous sections used
! the default mapping which assigns the DistGrid dimensions in sequence to the
! lower Array dimensions. Extra Array dimensions, if present, are considered
! non-distributed tensor dimensions for which the optional {\tt undistLBound}
! and {\tt undistUBound} arguments must be specified.
!
! The optional {\tt distgridToArrayMap} argument provides the option to override
! the default DistGrid to Array dimension mapping. The entries of the
! {\tt distgridToArrayMap} array correspond to the DistGrid dimensions in
! sequence and assign a unique Array dimension to each DistGrid dimension.
! DistGrid and Array dimensions are indexed starting at {\tt 1} for the lowest
! dimension. A value of {\tt "0"} in the {\tt distgridToArrayMap} array 
! indicates that the respective DistGrid dimension is {\em not} mapped against
! any Array dimension. What this means is that the Array will be replicated 
! along this DistGrid dimension.
! 
! As a first example consider the case where a 1D Array
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=1, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! is created on the 2D DistGrid used during the previous section.
!EOE
!BOC
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_ArrayPrint(array, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Here the default DistGrid to Array dimension mapping is used which assigns
! the Array dimensions in sequence to the DistGrid dimensions starting with
! dimension "1". Extra DistGrid dimensions are considered replicator dimensions
! because the Array will be replicated along those dimensions. In the above
! example the 2nd DistGrid dimension will cause 1D Array pieces to be
! replicated along the DEs of the 2nd DistGrid dimension. Replication in the
! context of {\tt ESMF\_ArrayCreate()} does not mean that data values are
! communicated and replicated between different DEs, but it means that different
! DEs provide memory allocations for {\em identical} exclusive elements.
!
! Access to the data storage of an Array that has been replicated along 
! DistGrid dimensions is the same as for Arrays without replication.
!EOE
!BOC
  call ESMF_ArrayGet(array, localDeCount=localDeCount, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  allocate(larrayList(0:localDeCount-1))
  allocate(localDeToDeMap(0:localDeCount-1))
  call ESMF_ArrayGet(array, localarrayList=larrayList, &
    localDeToDeMap=localDeToDeMap, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The {\tt array} object was created without additional padding which means
! that the bounds of the Fortran array pointer correspond to the bounds of
! the exclusive region. The following loop will cycle through all local DEs, 
! print the DE number as well as the Fortran array pointer bounds. The bounds
! should be:
! \begin{verbatim}
!          lbound       ubound
!
! DE 0:      1            3         --+
! DE 2:      1            3         --|  1st replication set
! DE 4:      1            3         --+
!
! DE 1:      1            2         --+
! DE 3:      1            2         --|  2nd replication set
! DE 5:      1            2         --+
! \end{verbatim}
!EOE
!BOC
  do localDe=0, localDeCount-1
    call ESMF_LocalArrayGet(larrayList(localDe), myFarray1D, &
      datacopyflag=ESMF_DATACOPY_REFERENCE, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    print *, "localPet: ", localPet, "DE ",localDeToDeMap(localDe)," [", &
      lbound(myFarray1D), ubound(myFarray1D),"]"
  enddo
  deallocate(larrayList)
  deallocate(localDeToDeMap)
  call ESMF_ArrayDestroy(array, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The Fortran array pointer in the above loop was of rank 1 because the
! Array object was of rank 1. However, the {\tt distgrid} object associated
! with {\tt array} is 2-dimensional! Consequently DistGrid based information
! queried from {\tt array} will be 2D. The {\tt distgridToArrayMap} and
! {\tt arrayToDistGridMap}
! arrays provide the necessary mapping to correctly associate DistGrid based 
! information with Array dimensions.
!
! The next example creates a 2D Array
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! on the previously used 2D DistGrid. By default, i.e. without the
! {\tt distgridToArrayMap}
! argument, both DistGrid dimensions would be associated with the two Array
! dimensions. However, the {\tt distgridToArrayMap} specified in the following
! call will only associate the second DistGrid dimension with the first Array 
! dimension. This will render the first DistGrid dimension a replicator
! dimension and the second Array dimension a tensor dimension for which 1D
! {\tt undistLBound} and {\tt undistUBound} arguments must be supplied.
!EOE
!BOC
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    distgridToArrayMap=(/0,1/), undistLBound=(/11/), &
    undistUBound=(/14/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_ArrayPrint(array, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_ArrayDestroy(array, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Finally, the same {\tt arrayspec} and {\tt distgrid} arguments are used to
! create a 2D Array that is fully replicated in both dimensions of the DistGrid.
! Both Array dimensions are now tensor dimensions and both DistGrid dimensions
! are replicator dimensions.
!EOE
!BOC
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    distgridToArrayMap=(/0,0/), undistLBound=(/11,21/), &
    undistUBound=(/14,22/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The result will be an Array with local lower bound (/11,21/) and upper bound
! (/14,22/) on all 6 DEs of the DistGrid.
!BOC
  call ESMF_ArrayDestroy(array, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_DistGridDestroy(distgrid, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Replicated Arrays can also be created from existing local Fortran arrays.
! The following Fortran array allocation will provide a 3 x 10 array on each
! PET. 
!EOE
!BOC
  allocate(myFarray2D(3,10))
!EOC
!BOE
! Assuming a petCount of 4 the following DistGrid defines a 2D index space
! that is distributed across the PETs along the first dimension.
!EOE
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/40,10/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The following call creates an Array object on the above distgrid using
! the locally existing {\tt myFarray2D} Fortran arrays. The difference 
! compared to the case with automatic memory allocation is that instead of
! {\tt arrayspec} the Fortran array is provided as argument. Furthermore,
! the {\tt undistLBound} and {\tt undistUBound} arguments can be omitted,
! defaulting into Array tensor dimension lower bound of 1 and an upper
! bound equal to the size of the respective Fortran array dimension.
!EOE
!BOC
  array = ESMF_ArrayCreate(farray=myFarray2D, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, distgridToArrayMap=(/0,2/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The {\tt array} object associates the 2nd DistGrid dimension with the 2nd
! Array dimension. The first DistGrid dimension is not associated with any
! Array dimension and will lead to replication of the Array along the DEs of
! this direction.
!EOE
!BOC
  call ESMF_ArrayDestroy(array, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_DistGridDestroy(distgrid, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  deallocate(myFarray2D)

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!!!! UNTIL FURTHER IMPLEMENTATION SKIP SECTIONS OF THIS EXAMPLE >>>>>>>>>>>>>>>>
#ifdef NOSKIP   
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


!BOEI
!
! \subsubsection{Array for a bipolar DistGrid}
!
! The Murray tripolar grid possesses two poles in the arctic region. The 
! implications of the bipolar grid of the northern hemisphere on the DistGrid
! and Array objects are discussed in this section.
!
! The bipolar region can either be defined as a single tile DistGrid or
! it can be composed of two or more separate tiles. In this example two
! tiles, each covering half of the index space of the bipolar region, will be
! used.
!
! The index space of the bipolar region remains logically rectangular (LR) and
! is assumed to be of size 360 x 50 elements for this example. The index order
! in the example is assumed $i,j$. The line for $j=1$ corresponds to a line of 
! constant latitude in the spherical coordinate system and the {\em bipolar
! fold} is along $j=50$. Two equal sized tiles of each 180 x 50 elements need
! to be connected in a way that corresponds to the bipolar topology.
! 
!EOEI
!BOCI
  allocate(connectionList(2*2+2,1))   ! 1 connection: the bipolar fold
  call ESMF_DistGridConnection(connection=connectionList(:,1), &
    tileIndexA=1, tileIndexB=2, &
    positionVector=(/179, 99/), orientationVector=(/-1, -2/), rc=rc)
!EOCI  
!BOEI
! With this {\tt connectionList} it is now
! possible to define a DistGrid object that captures the index space topology
! for a bipolar grid. The DistGrid consists of two tiles which need to be
! provided in {\tt minIndex} and {\tt maxIndex} list arguments.
!EOEI
!BOCI
  allocate(minIndex(2,2), maxIndex(2,2), regDecomp(2,2))
  minIndex(:,1) = (/1,1/)              ! first tile
  maxIndex(:,1) = (/180,50/)           ! first tile
  regDecomp(:,1) = (/petCount/2, 1/)    ! first tile
  minIndex(:,2) = (/1,1/)              ! second tile
  maxIndex(:,2) = (/180,50/)           ! second tile
  regDecomp(:,2) = (/petCount/2, 1/)    ! second tile
  
  distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
    regDecomp=regDecomp, connectionList=connectionList, rc=rc)
!EOCI  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOEI
! The decomposition described by {\tt regDecomp} assumes that there is an even 
! number of PETs available in the current Component. The decomposition will be 
! into as many DEs as PETs. Half of the DEs handle the first tile and the other
! half of DEs handle the second tile.
!
! In order to create a 2D Array on this DistGrid for single precision
! real data the ArraySpec variable must be set correctly.
!EOEI
!BOCI
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R4, rank=2, rc=rc)
!EOCI
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOEI
! Finally the Array objects can be created for this {\tt arrayspec} and {\tt 
! distgrid}. A scalar tracer Array will be created without halo padding.
!EOEI
!BOCI
  arrayTracer = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, rc=rc)
!EOCI
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOEI
! Next an Array is created for a scalar with space for a halo width of one 
! element in each direction.
!EOEI
!BOCI
  arrayScalar = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/1,1/), totalUWidth=(/1,1/), rc=rc)
!EOCI
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArrayDestroy(arrayTracer, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArrayDestroy(arrayScalar, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_DistGridDestroy(distgrid, rc=rc)


!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#endif
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

10 continue
  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_ArrayEx.F90"
  else
    print *, "FAIL: ESMF_ArrayEx.F90"
  endif

  call ESMF_Finalize(rc=rc)
  
end program
