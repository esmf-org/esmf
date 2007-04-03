! $Id: ESMF_ArrayEx.F90,v 1.13 2007/04/03 16:36:22 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

!==============================================================================
!EXAMPLE        String used by test script to count examples.
!==============================================================================

program ESMF_ArrayEx

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: rc, de, i, j, k, dim, nodeCount, petCount, dimCount, localDeCount
  integer:: deNeighborCount, linkCount, idm1, idm3, localPet
  type(ESMF_VM):: vm
  type(ESMF_DELayout):: delayout
  type(ESMF_DistGrid):: distgrid, distgrid3D, distgrid2D, distgrid1D
  type(ESMF_DistGrid):: distgrid1, distgrid2
  type(ESMF_ArraySpec):: arrayspec
  type(ESMF_Array):: array, array1, array2, array1D, array2D, array3D
  type(ESMF_Array):: arrayTracer, arrayNScalar, arrayNEu, arrayNEv
  type(ESMF_ArrayBundle):: arrayBundle
  type(ESMF_Array), allocatable:: arrayList(:)
  type(ESMF_LocalArray), allocatable:: larrayList(:)
  type(ESMF_LocalArray), allocatable:: larrayList1(:), larrayList2(:)
  real(ESMF_KIND_R8), pointer:: myF90Array(:,:)
  real(ESMF_KIND_R8), pointer:: myF90Array1(:,:), myF90Array2(:,:)
  real(ESMF_KIND_R8), pointer:: myF90Array3(:,:,:)
  real(ESMF_KIND_R8), pointer:: myF90Array2D(:,:), myF90Array3D(:,:,:)
  real(ESMF_KIND_R8):: dummySum
  type(ESMF_IndexFlag):: indexflag
  integer, allocatable:: dimExtent(:,:), indexList(:), regDecompDeCoord(:)
  integer, allocatable:: minCorner(:,:), maxCorner(:,:), regDecomp(:,:)
  integer, allocatable:: deBlockList(:,:), connectionList(:,:), connectionTransformList(:,:)
  integer, allocatable:: deNeighborList(:), deNeighborInterface(:,:)
  integer, allocatable:: localDeList(:), linkList(:,:), inverseDimmap(:)
  integer, allocatable:: exclusiveLBound(:,:), exclusiveUBound(:,:)
  integer, allocatable:: totalLWidth(:,:), totalUWidth(:,:)
  integer, allocatable:: totalLBound(:,:), totalUBound(:,:)
  integer, allocatable:: totalCellMask(:,:)
  integer, allocatable:: computationalLWidth(:,:), computationalUWidth(:,:)
  integer, allocatable:: computationalLBound(:,:), computationalUBound(:,:)
  integer, allocatable:: haloLDepth(:), haloUDepth(:)
  type(ESMF_Logical):: regDecompFlag
  type(ESMF_RouteHandle):: haloHandle, haloHandle2
  type(ESMF_RouteHandle):: sparseMatMulHandle
  real(ESMF_KIND_R8), allocatable:: factorList(:)
  integer, allocatable:: factorIndexList(:,:)

  ! result code
  integer :: finalrc
  
  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  if (petCount /= 4) goto 10 ! TODO: use EXAMPLES_MULTI_ONLY once available
  
!BOE
! \subsubsection{Array creation with automatic memory allocation}
!
! The examples of the previous sections made the user responsible for 
! providing memory allocations for the PET-local regions of the Array object.
! The user was able to use any of the Fortran90 methods to obtain allocated
! arrays and pass them into ArrayCreate(). Alternatively, users may wish for
! ESMF to control memory management of an Array object. The following example
! shows the interfaces that are available to the user to do just this.
! 
! To create an {\tt ESMF\_Array} object without providing an existing
! Fortran90 array at least two pieces of information 
! are required. First the {\em type, kind and rank} (tkr) of the
! array must be specified in form of an {\tt ESMF\_ArraySpec} argument. Here
! a 2D Array of double precision real numbers is to be created:
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, &
    rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! Further an {\tt ESMF\_DistGrid} argument must be constructed that holds
! information about the entire domain (patchwork) and the decomposition into 
! DE-local exclusive
! regions. The following line creates a DistGrid for a 5x5 global LR domain 
! that is decomposed into 2 x 3 = 6 DEs.
!EOE
!BOC
  distgrid = ESMF_DistGridCreate(minCorner=(/1,1/), maxCorner=(/5,5/), &
    regDecomp=(/2,3/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! This is enough information to create a Array object with default settings.
!EOE
!BOC
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!  call ESMF_ArrayPrint(array, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOE
! The {\tt array} object created by the above call is an ESMF distributed 
! object. As such it must follow the ESMF convention that requires that 
! the call to {\tt ESMF\_ArrayCreate()} must be issued in unison by all 
! PETs of the current context.
!
! The index space covered by the Array object and the decomposition into 
! DE-local exclusive cell regions, as it is described by the DistGrid object,
! is illustrated in the following diagram. Each asterix (*) represents a single
! cell.
!
! \begin{verbatim}
! 
!  +---------------------------------------> 2nd dimension
!  |  (1,1)
!  |    +-----------+-----------+------+
!  |    | DE 0      | DE 2      | DE 4 |
!  |    |  *    *   |  *    *   |  *   |
!  |    |           |           |      |
!  |    |  *    *   |  *    *   |  *   |
!  |    |           |           |      |
!  |    |  *    *   |  *    *   |  *   |
!  |    +-----------+-----------+------+
!  |    | DE 1      | DE 3      | DE 5 |
!  |    |  *    *   |  *    *   |  *   |
!  |    |           |           |      |
!  |    |  *    *   |  *    *   |  *   |
!  |    +-----------+-----------+------+
!  |                                 (5,5)
!  v 
! 1st dimension
!
! \end{verbatim}

! \subsubsection{Native language memory access -- the most general way}
!
! The exact decomposition of the index space covered by 
! the {\tt array} object into DEs is contained in the {\tt distgrid} object. 
! Further, the layout of the DEs across the PETs of the component is stored in
! the {\tt delayout} contained within the {\tt distgrid} object. In the 
! above example a default DELayout was created during the 
! {\tt ESMF\_DistGridCreate()} call (see the refDoc / proposal for 
! {\tt ESMF\_DELayout} and {\tt ESMF\_DistGrid} for details).
!
! In order to use the {\tt array} object it is necessary to know how many DEs
! are located on each calling PET. To this end the corresponding DELayout object
! needs to be extracted from {\tt distgrid} first and then can be queried for 
! the PET-local deCount.
!EOE
!BOC
  call ESMF_DistGridGet(distgrid, delayout=delayout, rc=rc)
  call ESMF_DELayoutGet(delayout, localDeCount=localDeCount, rc=rc)
  allocate(localDeList(localDeCount))
  call ESMF_DELayoutGet(delayout, localDeList=localDeList, rc=rc)
!EOC  
!BOE
! In general it must be assumed that there may be multiple DEs associated with
! the calling PET, i.e. {\tt localDeCount} >= 1. The situation where there is 
! exactly one DE for each PET, i.e. {\tt localDeCount} = 1 on every PET, is 
! merely a special case of the more general formulation.
! 
! Consequently, in order to gain access to the DE-local memory segments 
! that have been allocated on each PET by the {\tt ArrayCreate()} call
! the Array must be queried for a {\em list} of {\tt LocalArray} objects, 
! each element corresponding to one PET-local DE.
!EOE  
!BOC
  allocate(larrayList(localDeCount))
  call ESMF_ArrayGet(array, larrayList=larrayList, rc=rc)
!EOC
!BOE
! Now each PET can loop through its local list of DEs and access the associated
! memory through a suitable Fortran90 pointer. In the current example the native
! pointer {\tt myF90Array} must be declared as\newline
! {\tt real(ESMF\_KIND\_R8), pointer:: myF90Array(:,:)}\newline
! in order to match the {\tt arrayspec} that was used to create the
! {\tt array} object. The following loop uses the native language access to
! initialize the entire memory chunks of all PET-local DEs to 0 using 
! Fortran90 array syntax.
!EOE
!BOC
  do de=1, localDeCount
    call ESMF_LocalArrayGetData(larrayList(de), myF90Array, ESMF_DATA_REF, rc=rc)
    myF90Array = 0.
  enddo
!EOC

!  call ESMF_ArrayPrint(array, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! \subsubsection{Regions and default bounds}
! \label{Array_regions_and_default_bounds}
!
! Each {\tt Array} object is decomposed into DEs as specified by the 
! associated {\tt DistGrid} object. Each piece of this decomposition, i.e. each
! DE, holds a chunk of the {\tt array} data in its own private piece of memory.
! The details of the Array decomposition are described in the following 
! paragraphs.
!
! At the center of the Array decomposition is the {\tt ESMF\_DistGrid} class.
! The DistGrid object specified during Array creation contains three essential
! pieces of information:
! \begin{itemize}
! \item The extent and topology of the global domain covered by the Array object
!       in terms of indexed cells. The total extent may be a composition or 
!       patchwork of smaller logically rectangular (LR) domain pieces or patches.
! \item The decomposition of the entire domain into "cell exclusive" DE-local LR
!       chunks. {\em Cell exclusive} means that there is no cell overlap 
!       between DE-local chunks. This, however, does not exclude degeneracies 
!       between staggering locations for certain topologies (e.g. bipolar).
! \item The layout of DEs over the available PETs an thus the distribution of
!       the Array data.
! \end{itemize}
!
! Each cell of an Array is associated with a {\em single} DE. The union of
! cells associated with a DE, as defined by the DistGrid above, will correspond
! to a LR chunk of index space, called the {\em exclusive region} of the DE.
!
! There is a hierarchy of four regions that can be identified for each DE in an
! Array object. Their definition and relationship to each other is as follows:
! \begin{itemize}
! \item {\em Interior Region}: Region that only contains local cells that are
!       {\em not} mapped into the halo of any other DE. The shape and size of 
!       this region for a particular DE depends non-locally on the halos defined
!       by other DEs and may change during computations. Knowledge of the 
!       interior cells may be used to improve performance by overlapping 
!       communications with ongoing computation for a DE.
! \item {\em Exclusive Region}: Cells for which this DE claims exclusive 
!       ownership. Practically this means that the DE will be the source for 
!       these cells in halo and reduce operations. There are exceptions
!       to this for certain staggering locations in some topologies. These 
!       cases remain well-defined with the information available through 
!       DistGrid. This region includes all cells of the interior region.
! \item {\em Computational Region}: Region of all cells that are kept locally
!       and are updated during computation by the local DE. The additional 
!       computational cells, beyond the exclusive cells of the DE, are either 
!       overlapping with the exclusive region of another DE or lie outside the 
!       global domain as defined by the DistGrid. Extra computational points 
!       may be chosen differently for different stagger locations.
! \item {\em Total (Memory) Region}: Total of all DE-locally allocated cells. 
!       The size and shape of the total memory region must accommodate the
!       computational region but may contain additional cells to be used
!       in halos and/or as memory padding.
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
!   |   |   |     +-------------------+      |    |   |
!   |   |   |     |                   |      |    |   |
!   |   |   |     |                   |      |    |   |
!   |   |   |     | "Interior Region" |      |    |   |
!   |   |   |     |                   |      |    |   |
!   |   |   |     |                   |      |    |   |
!   |   |   |     +-------------------+      |    |   |
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
! totalLWidth(:) = exclusiveLBound(:) - totalLBound(:)
! totalUWidth(:) = totalUBound(:) - exclusiveLBound(:)
!
! \end{verbatim}
! and
! \begin{verbatim}
!
! computationalLWidth(:) = exclusiveLBound(:) - computationalLBound(:)
! computationalUWidth(:) = computationalUBound(:) - exclusiveLBound(:)
!
! \end{verbatim}
!
!
! The {\em exclusive region} is determined during Array creation by the 
! DistGrid argument. Optional arguments may be used to specify the 
! {\em computational region} when the Array is created, by default it will be
! set equal to the exclusive region. The {\em total region}, i.e. the actual
! memory allocation for each DE, is also determined during Array creation. When
! creating the Array object from existing Fortran90 arrays the total region is
! set equal to the memory provided by the Fortran90 arrays. Otherwise the 
! default is to allocate as much memory as is needed to accomodate the 
! computational region. Finally it is also possible to use optional arguments to
! the ArrayCreate() call to specify the total region of the object.
!
! Once an Array object has been created its total region cannot be changed. 
! The computational region, however, may be adjusted within the limits of the 
! total region using the {\tt ArraySet()} call.
!
! The {\em interior region} is very different from the other regions in that
! it cannot be specified. The {\em interior region} for each DE is a {\em
! consequence} 
! of the choices made for the other regions collectively across all DEs into
! which an Array object is decomposed. An Array object can be queried for
! its DE-local {\em interior regions} as to offer additional information to 
! the user necessary to write more efficient code. See section 
! \ref{ArrayEx_interiorRegion} for more details.
!
! By default the bounds of 
! each DE-local {\em total region} are defined as to put the start of the
! DE-local {\em exclusive region} at the "origin" of the local index space, i.e.
! at {\tt (1, 1, ..., 1)}. With that the following loop will access each
! element of the DE-local memory segment for each PET-local DE of the Array 
! object created in the previous sections and print its content.
!EOE
!
!BOC
  do de=1, localDeCount
    call ESMF_LocalArrayGetData(larrayList(de), myF90Array, ESMF_DATA_REF, rc=rc)
    do i=1, size(myF90Array, 1)
      do j=1, size(myF90Array, 2)
!        print *, "PET-local DE=", de, ": array(",i,",",j,")=", myF90Array(i,j)
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
! Furthermore the {\em exclusive region} is identical for all stagger locations
! (discussed in a later section) and as such provides a unique reference frame 
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
  allocate(exclusiveUBound(2, localDeCount))  ! dimCount=2
  allocate(exclusiveLBound(2, localDeCount))  ! dimCount=2
  call ESMF_ArrayGet(array, indexflag=indexflag, &
    exclusiveLBound=exclusiveLBound, exclusiveUBound=exclusiveUBound, rc=rc)
  if (indexflag == ESMF_INDEX_DELOCAL) then
!    print *, "DE-local exclusive regions start at (1,1)"
    do de=1, localDeCount
      call ESMF_LocalArrayGetData(larrayList(de), myF90Array, ESMF_DATA_REF, rc=rc)
      do i=1, exclusiveUBound(1, de)
        do j=1, exclusiveUBound(2, de)
!          print *, "DE-local exclusive region for PET-local DE=", de, &
!            ": array(",i,",",j,")=", myF90Array(i,j)
        enddo
      enddo
    enddo
  else
!    print *, "DE-local exclusive regions of this Array have global bounds"
    do de=1, localDeCount
      call ESMF_LocalArrayGetData(larrayList(de), myF90Array, ESMF_DATA_REF, rc=rc)
      do i=exclusiveLBound(1, de), exclusiveUBound(1, de)
        do j=exclusiveLBound(2, de), exclusiveUBound(2, de)
!          print *, "DE-local exclusive region for PET-local DE=", de, &
!            ": array(",i,",",j,")=", myF90Array(i,j)
        enddo
      enddo
    enddo
  endif
!EOC

!BOE
! Obviously the second branch of this simple code will work for either case, 
! however, if a complex computational kernel was written assuming 
! {\tt ESMF\_INDEX\_DELOCAL} type bounds the second branch would simply be 
! used to indicate the problem and bail out.
!
! If the Array uses {\tt ESMF\_INDEX\_DELOCAL} type bounds and the 
! code accessing the Array data requires knowledge of global index space 
! information then the corresponding DistGrid object can be queried for this
! kind of information. Please see the DistGrid proposal for more details.
!

! \subsubsection{Computational region and extra cells for halo or padding}
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
!EOE
!BOC
  call ESMF_ArrayDestroy(array, rc=rc) ! first destroy the old array object
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
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
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  call ESMF_ArrayGet(array, larrayList=larrayList, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
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
! entries for these cells. 
!
! The Array object can be queried for absolute {\em bounds}
!EOE
!BOC
  allocate(computationalLBound(2, localDeCount))  ! dimCount=2
  allocate(computationalUBound(2, localDeCount))  ! dimCount=2
  allocate(totalLBound(2, localDeCount))          ! dimCount=2
  allocate(totalUBound(2, localDeCount))          ! dimCount=2
  call ESMF_ArrayGet(array, exclusiveLBound=exclusiveLBound, &
    exclusiveUBound=exclusiveUBound, computationalLBound=computationalLBound, &
    computationalUBound=computationalUBound, totalLBound=totalLBound, &
    totalUBound=totalUBound, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! or for the relative {\em widths}.
!EOE
!BOC
  allocate(computationalLWidth(2, localDeCount))  ! dimCount=2
  allocate(computationalUWidth(2, localDeCount))  ! dimCount=2
  allocate(totalLWidth(2, localDeCount))          ! dimCount=2
  allocate(totalUWidth(2, localDeCount))          ! dimCount=2
  call ESMF_ArrayGet(array, computationalLWidth=computationalLWidth, &
    computationalUWidth=computationalUWidth, totalLWidth=totalLWidth, &
    totalUWidth=totalUWidth, rc=rc)
!EOC
!BOE
! Either way the dereferencing of Array data is centered around the DE-local
! exclusive region:
!EOE
!BOC
  do de=1, localDeCount
    call ESMF_LocalArrayGetData(larrayList(de), myF90Array, ESMF_DATA_REF, rc=rc)
    ! initialize the DE-local array
    myF90Array = 0.1d0 * localDeList(de)
    ! first time through the total region of array    
!    print *, "myF90Array bounds for DE=", localDeList(de), lbound(myF90Array), &
!      ubound(myF90Array)
!    do j=exclusiveLBound(2, de), exclusiveUBound(2, de)
!      do i=exclusiveLBound(1, de), exclusiveUBound(1, de)
!        print *, "Excl region DE=", localDeList(de), ": array(",i,",",j,")=", &
!          myF90Array(i,j)
!      enddo
!    enddo
!    do j=computationalLBound(2, de), computationalUBound(2, de)
!      do i=computationalLBound(1, de), computationalUBound(1, de)
!        print *, "Excl region DE=", localDeList(de), ": array(",i,",",j,")=", &
!          myF90Array(i,j)
!      enddo
!    enddo
    do j=totalLBound(2, de), totalUBound(2, de)
      do i=totalLBound(1, de), totalUBound(1, de)
!        print *, "Total region DE=", localDeList(de), ": array(",i,",",j,")=", &
!          myF90Array(i,j)
      enddo
    enddo

    ! second time through the total region of array    
    do j=exclusiveLBound(2, de)-totalLWidth(2, de), &
      exclusiveUBound(2, de)+totalUWidth(2, de)
      do i=exclusiveLBound(1, de)-totalLWidth(1, de), &
        exclusiveUBound(1, de)+totalUWidth(1, de)
!        print *, "Excl region DE=", localDeList(de), ": array(",i,",",j,")=", &
!          myF90Array(i,j)
      enddo
    enddo
  enddo
!EOC

  deallocate(larrayList)
  
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!!!! UNTIL FURTHER IMPLEMENTATION SKIP SECTIONS OF THIS EXAMPLE >>>>>>>>>>>>>>>>
#ifdef NOSKIP
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

!BOEI
! 
! The index space topology of this example is very simple. The DistGrid is
! defined by a single LR patch and does not contain any extra connections.
! Consequently,
! DEs that are located at the edge of the index space may contain DE-local 
! computational and total regions that reach beyond the index space as it is
! defined by 
! the DistGrid. For example DE 1's computational and total region, as shown 
! in the diagram above, contain extra cells that are not covered by the
! index space described by the DistGrid object. 
!
! The situation is different for interior DEs, or when the widths specified
! during Array creation are asymmetric. For the current example DE 3, for
! instance,
! only contains cells in its total and computational region that lie within
! the DistGrid index space. Cells in the total region that are outside of a DE's
! exclusive region are then redundant with another DE's exclusive cells. 
! Redundancies in the computational region may be enforced or monitored by
! special Array communication calls. Total region cells that are outside the 
! computational region may be part of a halo operation and may be updated with 
! the value of the corresponding exclusive cell in another DE using halo 
! communication methods.
!
! Computational cells that are outside the DistGrid index space are kept for 
! convenience, simplifying the formulation of the computational kernel. 
! Naturally, redundancy cannot be checked or enforced for these cells. 
! Similarly, halo cells that do not correspond to cells within the index space
! of the DistGrid will not be updated during halo communications. It is up to
! the application writer to utilize the provided cells as is appropriate for the
! computational algorithm. The Array class offers mask type information 
! through the {\tt ESMF\_ArrayGet()} method in order to assist the user in 
! dealing with cells that are not part of the Array index space as it is 
! defined in the corresponding DistGrid object. Please see section 
! \ref{ArrayEx_interiorRegion} for details.
!
!
! \subsubsection{Halo communication}
!
! The {\tt array} variable created and used in the previous examples has the
! following decomposition into DE exclusive regions.
!
! \begin{verbatim}
! 
!  +---------------------------------------> 2nd dimension
!  |  (1,1)
!  |    +-----------+-----------+------+
!  |    | DE 0      | DE 2      | DE 4 |
!  |    |  *    *   |  *    *   |  *   |
!  |    |           |           |      |
!  |    |  *    *   |  *    *   |  *   |
!  |    |           |           |      |
!  |    |  *    *   |  *    *   |  *   |
!  |    +-----------+-----------+------+
!  |    | DE 1      | DE 3      | DE 5 |
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
! The associated DistGrid does not define any extra connections so that the
! global domain has open outside boundaries as is the case for regional models.
! The {\tt array} was created with extra cells around each DE's exclusive 
! region. The total of all DE-local cells make up each DE's total region. 
! Within the total region some of the cells are carried as extra computational 
! cells in the computational region. The exact situation has been illustrated
! for this {\tt array} object in the previous sections.
!
! Now a simple halo operation shall be carried out for {\tt array} that updates
! all the extra cells in the total region for each DE.
!EOEI
!BOCI
  call ESMF_ArrayHalo(array, regionflag=ESMF_REGION_EXCLUSIVE, rc=rc)
!EOCI
!BOEI
! The {\tt regionflag=ESMF\_REGION\_EXCLUSIVE} indicates that the halo operation 
! is to be relative to the exclusive region, i.e. it includes computational 
! cells as is the case during the spin up phase of some models.
!
! The above call to the ArrayHalo() method will have updated all of the extra 
! cells in the DE-local regions that had source cells in one of the DEs of the
! Array object. There are, however, as will be pointed out in the next section,
! some extra cells that do not correspond to any DE's exclusive region. The
! ArrayHalo() operation leaves those cells unchanged.
!
! Next only the cells outside the computational region shall be updated. This
! is the default for ArrayHalo() and the number of arguments that need to be
! specified is minimal.
!EOEI
!BOCI
  call ESMF_ArrayHalo(array, rc=rc)
!EOCI
!BOEI
! (The same could have been accomplished with calling ArrayHalo() with
! {\tt regionflag=ESMF\_REGION\_COMPUTATIONAL}.)
! 
! The ArrayHalo() method allows the halo depth to be specified for each side
! of the DE-local region. In the following example the {\tt haloLDepth} and 
! {\tt haloUDepth} arguments are used to halo a maximum of 1 cell around the
! computational region of each DE.
!EOEI
!BOCI
  call ESMF_ArrayHalo(array, haloLDepth=(/1,1/), haloUDepth=(/1,1/), rc=rc)
!EOCI
!BOEI
! It is not an error to request a halo depth greater than some of the DE-local 
! total regions can accommodate. This situation must be supported since it is 
! possible to define different computational widths and/or total widths for each 
! DE and a large halo depth may make sense for some DEs but not for others. A 
! warning will be logged for the DEs that cannot fully fit the requested halo.
!
! The direct calls to ArrayHalo(), as used above, come with a significant 
! overhead caused by the need to determine the exact data exchange necessary to 
! perform the requested operation. For halo operations used 
! repeatedly it is much more efficient to precompute the exchange patterns 
! once, store this information and reuse it each time the operation is to 
! be performed. The following call will precompute the pattern for the previous
! ArrayHalo() call and store the information as a precomputed communication
! pattern or {\em Route}. A handle to the Route is provided to the user via the 
! {\tt routehandle} argument which accepts {\tt ESMF\_RouteHandle} objects.
!EOEI
!BOCI
  call ESMF_ArrayHaloStore(array, haloLDepth=(/1,1/), haloUDepth=(/1,1/), &
    routehandle=haloHandle, rc=rc)
!EOCI
!BOEI
! The RouteHandle object {\tt haloHandle} can now be used to invoke the 
! associated halo operation with a much reduced overhead. The only input 
! ArrayHaloRun() needs is the Array object on which to perform the halo 
! operation together with the RouteHandle object.
!EOEI
!BOCI
  call ESMF_ArrayHaloRun(array, routehandle=haloHandle, rc=rc)
!EOCI
!BOEI
! Multiple halo operations for the same Array object can be stored and are 
! available to run when needed. For example a halo update for cells in positive
! second dimension of {\tt array} may be stored without loosing the previously
! precomputed operation by supplying a separate RouteHandle object.
!EOEI
!BOCI
  call ESMF_ArrayHaloStore(array, haloLDepth=(/0,0/), haloUDepth=(/0,1/), &
    routehandle=haloHandle2, rc=rc)
!EOCI
!BOEI
! Details of stored halo operations, such as halo depths, are stored within the
! Route object referenced by the RouteHandles. This information can be accessed
! through the overloaded {\tt ESMF\_ArrayGet()} interface that accepts 
! a RouteHandle object in addition to the Array object.
!EOEI
!BOCI
  allocate(haloLDepth(2), haloUDepth(2))
  call ESMF_ArrayGet(array, routehandle=haloHandle2, &
    haloLDepth=haloLDepth, haloUDepth=haloUDepth, rc=rc)
  print *, haloLDepth, haloUDepth
  deallocate(haloLDepth, haloUDepth)
!EOCI
!BOEI
! Finally the RouteHandles can be used to release the associated Routes.
!EOEI
!BOCI
  call ESMF_RouteHandleRelease(routehandle=haloHandle, rc=rc)
  call ESMF_RouteHandleRelease(routehandle=haloHandle2, rc=rc)
!EOCI
!BOEI
! The Array object used to precompute the Routes can be destroyed before or 
! after the Routes have been released.
!
!
! \subsubsection{Interior region and Array's total cell mask}
! \label{ArrayEx_interiorRegion}
!
! The {\em Array total cell mask} is a native Fortran90 integer array of
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
! For the {\tt array} object created in the previous section the total cell mask
! for DE 1 would need to be allocated as
!EOEI
!BOCI
  allocate(totalCellMask(3:8, -3:3))              ! rank=2
!EOCI
!BOEI
! Then, assuming that DE 1 was localDE 1 on the PET issuing the following
! call the mask can be obtained in the following manner.
!EOEI
!BOCI
  call ESMF_ArrayGet(array, localDe=1, totalCellMask=totalCellMask, rc=rc)
!EOCI
!BOEI
! Now the {\tt totalCellMask} variable contains the mask for the total
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
! value of "-2" in an exclusive cell indicates that other DEs of the Array
! have cells in their total region that correspond to this DE's exclusive cell.
! A value of "-3" for an exclusive cell indicates that there exists a redundancy
! with another DE's exclusive region and a value of "-4" indicates redundancy 
! and dependency between DEs. Finally, a cell value of "-1" in the exclusive
! region indicates that there are no other DEs in the Array that depend on this
! exclusive cell. The union of exclusive cells with mask value "-1" define the 
! {\em absolute interior region} of the DE. In this example DE 1 does not 
! contain any absolute interior cells.
!
! The next larger rectangle in the above diagram corresponds to the 
! computational region of DE 1 and the entire array extent is that of this DE's
! total region. A cell mask value of "-1" in these regions indicates that 
! there are no cells in any of the DEs' exclusive regions that corresponds to 
! this cell, i.e. the cell lies outside the index space covered by DistGrid.
!
! Mask values $>= 0$ in the computational and total regions indicate the DE 
! which contains the corresponding cell in its exclusive region. Thus a cell 
! with mask value $>= 0$ lies within the DistGrid index space.
!
! The query call used above was only able to return information based on the
! information stored in the Array object and the underlying DistGrid. However,
! it is often more valuable to obtain the information stored in a total cell
! mask specifically for a particular Array communication pattern or a whole
! set thereof. The following lines of code demonstrate this aspect for one of 
! the Array halo operations used in the previous section.
!EOEI
!BOCI
  call ESMF_ArrayHaloStore(array, haloLDepth=(/0,0/), haloUDepth=(/0,1/), &
    routehandle=haloHandle2, rc=rc)
!EOCI
!BOEI
! The same {\tt totalCellMask} variable may be used to obtain the specific total
! cell mask. Only difference is that the {\tt routehandle} must be provided
! as an additional input parameter. Since specific total cell masks may be 
! required as an "OR" over a whole set of communication operations the optional
! input parameter is defined as a list of RouteHandles.
!EOEI
!BOCI
  call ESMF_ArrayGet(array, routehandlelist=(/haloHandle2/), &
    localDe=1, totalCellMask=totalCellMask, rc=rc)
!EOCI
!BOEI
! Now {\tt totalCellMask} holds the total cell mask for {\tt array} specifically
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
! The meaning of the values stored in the total cell mask for a specific 
! query call are now related to the specified operations. The "-1" in the 
! exclusive region (inner most rectangle) indicates that the operation
! referenced by {\tt haloHandle2} requires none of the exclusive cells of DE 1.
! Thus all exclusive cells of DE 1 can be considered interior cells with 
! respect to {\tt haloHandle2}. Consequently, if a non-blocking HaloRun() were
! to be executed it would be safe to read from and write to all of DE 1's
! exclusive cells.
!
! For the next larger rectangle, the computational region, {\tt totalCellMask}
! indicates that there are only two cells that will be updated with values from 
! exclusive cells of DE 3 during execution of {\tt haloHandle2}, all other cells
! are unaffected by this communication pattern.
!
! Finally, none of the points in DE 1's total region outside of the 
! computational region are affected by {\tt haloHandle2}.
!
! After usage it is the user's responsibility to clean up.
!EOEI
!BOCI
  deallocate(totalCellMask)
  call ESMF_RouteHandleRelease(routehandle=haloHandle2, rc=rc)
  call ESMF_ArrayDestroy(array, rc=rc) ! destroy array object
!EOCI  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
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
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOEI
! This {\tt array} has DE-local total regions that are three cells wider in 
! each direction than the corresponding exclusive region. The default
! computational region for each DE is equal to the DE's exclusive region. The
! extra cells between exclusive and total region are available for halos or
! can be used to expamd the computational region of the Array.
!
! The following compute loop first executes a halo update which by default
! will attempt to halo all cells that are outside of the computational region.
! Then the a three step cycle is entered which sets the computational region to
! 2, 1 and 0 cells around the exclusive region and each time calls a 
! compuitational kernel that will access cells from (i-1,j-1) to (i+1,j+1). 
! After the computational width has reached 0 a new halo updated needs to be
! performed.
!EOEI
!BOCI
  do j=1, 10
    call ESMF_ArrayHalo(array, rc=rc)
    do k=2, 0, -1
      call ESMF_ArraySet(array, &
        computationalLWidth=(/k,k/), computationalUWidth=(/k,k/), rc=rc)
      ! call second_order_kernel(array)  ! will access (i-1) and (i+1) cells
    enddo
  enddo
!EOCI
!BOCI
  call ESMF_ArrayDestroy(array, rc=rc) ! finally destroy the array object
!EOCI  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOEI
!EOCI
!BOEI
! Sometimes it may be desirable to add extra memory padding around the DE-local
! data cells, e.g. to prevent false sharing on shared memory architectures. 
! For this the total region can be chosen larger than is needed by the 
! algorithm. The following lines of code recapture the example from above but
! now the total region is chosen with a 5 cell wide rim around the exclusive
! region. Still only a 3 cell wide halo is needed for the computational
! algorithm. By default the ArrayHalo() call will halo all extra cells, to keep
! the halo to 3 cells in each direction the haloLDepth and haloUDepth arguments
! must be used. With that a 5 - 3 = 2 cell wide memory padding has been placed
! around the cells used by each DE.
!
!EOEI
!BOCI
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/5,5/), totalUWidth=(/5,5/), rc=rc)
!EOCI  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOCI
  do j=1, 10
    call ESMF_ArrayHalo(array, haloLDepth=(/3,3/), haloUDepth=(/3,3/), rc=rc)
    do k=2, 0, -1
      call ESMF_ArraySet(array, &
        computationalLWidth=(/k,k/), computationalUWidth=(/k,k/), rc=rc)
      ! call second_order_kernel(array)  ! will access (i-1) and (i+1) cells
    enddo
  enddo
!EOCI
!BOEI
! Done, {\tt array} can be destroyed.
!EOEI
!BOCI  
  call ESMF_ArrayDestroy(array, rc=rc) ! finally destroy the array object
!EOCI  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#endif
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!BOE
!
! \subsubsection{SparseMatMul communication}
! 
! Sparse matrix multiplication is a fundamental Array communication method. One
! frequently used application of this method is the interpolation between pairs
! of Arrays. The principle is this: the value of each cell in the exclusive 
! region of the destination Array is expressed as a linear combination of {\em 
! all} the exclusive cells of the source Array. Naturally most of the 
! coefficients of these linear combinations will be zero and it is more 
! efficient to store explicit information about the non-zero elements than to 
! keep track of all the coefficients.
!
! There is a choice to be made with respect to the format in which to store the
! information about the non-zero elements. One option is to store the value
! of each coefficient together with the corresponding destination cell index
! and source cell index. Destination and source indices could be expressed in
! terms of the corresponding DistGrid patch index together with the coordinate
! tuple within the patch. While this format may be the most natural way to
! express cells in the source and destination Array, it has a major drawback, it
! is extremly bulky. For two 2D Arrays it requires 6 integers to store the
! source and destination cell information for each non-zero interpolation 
! weight.
!
! An alternative format exists that only requires two integers to be stored 
! with each non-zero interpolation coefficient, regardless of the rank of 
! source and destination Arrays. For this format a unique cell order must be 
! defined which allows to uniquely address each exclusive cell in an Array 
! object by a single integer number. In other words the cells defined by the 
! DistGrid associated with the Array must be sequentialized. The cell sequence 
! suggested in this proposal first moves fastest through the DistGrid 
! dimensions in their order and then moves through the patches of the DistGrid.
!
! In the following example {\tt array1} will be the source Array that will be
! interpolated onto {\tt array2}, the destination Array. Both Arrays are 2D
! Arrays of double precision real numbers. {\tt array1} uses the 5 x 5 
! DistGrid decomposed over 2 x 3 = 6 DEs as was used in previous examples.
! {\tt array2} is defined on a [-4,..,2] x [1,..,3] DistGrid that is decomposed
! over as many DEs as there are PETs in the context at runtime. Notice also that
! {\tt array1} only allocates space for the exclusive DE-local regions
! whereas {\tt array2} allocates a 1 cell wide rim around each exclusive region
! to define a larger total region for each of its DE.
! 
!EOE
!BOC
  distgrid1 = ESMF_DistGridCreate(minCorner=(/1,1/), maxCorner=(/5,5/), &
    regDecomp=(/2,3/), rc=rc)
  array1 = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid1, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridGet(distgrid1, delayout=delayout, rc=rc)
  call ESMF_DELayoutGet(delayout, localDeCount=localDeCount, rc=rc)
  allocate(larrayList(localDeCount))
  call ESMF_ArrayGet(array1, larrayList=larrayList, rc=rc)
  do de=1, localDeCount
    call ESMF_LocalArrayGetData(larrayList(de), myF90Array, ESMF_DATA_REF, rc=rc)
    myF90Array = 1.5 + 2.3 * de
  enddo
  deallocate(larrayList)
!BOC
  distgrid2 = ESMF_DistGridCreate(minCorner=(/-4,1/), maxCorner=(/2,3/), rc=rc)
  array2 = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid2, &
    totalLWidth=(/1,1/), totalUWidth=(/1,1/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridGet(distgrid2, delayout=delayout, rc=rc)
  call ESMF_DELayoutGet(delayout, localDeCount=localDeCount, rc=rc)
  allocate(larrayList(localDeCount))
  call ESMF_ArrayGet(array2, larrayList=larrayList, rc=rc)
  do de=1, localDeCount
    call ESMF_LocalArrayGetData(larrayList(de), myF90Array, ESMF_DATA_REF, rc=rc)
    myF90Array = 1.5 - 0.9 * de
  enddo
  deallocate(larrayList)
!BOE
! 
! The sequentialization of both Arrays is straight forward.
!
! {\tt array1}:
! \begin{verbatim}
!  Array/DistGrid coordinate   -   sequential index
!            (1, 1)            -       1
!            (2, 1)            -       2
!            (3, 1)            -       3
!            (4, 1)            -       4
!            (5, 1)            -       5
!            (1, 2)            -       6
!            (2, 2)            -       7
!            (3, 2)            -       8
!            (4, 2)            -       9
!            (5, 2)            -      10
!            (1, 3)            -      11
!            (2, 3)            -      12
!            (3, 3)            -      13
!            (4, 3)            -      14
!            (5, 3)            -      15
!            (1, 4)            -      16
!            (2, 4)            -      17
!            (3, 4)            -      18
!            (4, 4)            -      19
!            (5, 4)            -      20
!            (1, 5)            -      21
!            (2, 5)            -      22
!            (3, 5)            -      23
!            (4, 5)            -      24
!            (5, 5)            -      25
! \end{verbatim}
!
! {\tt array2}:
! \begin{verbatim}
!  Array/DistGrid coordinate   -   sequential index
!            (-4, 1)           -       1
!            (-3, 1)           -       2
!            (-2, 1)           -       3
!            (-1, 1)           -       4
!            ( 0, 1)           -       5
!            ( 1, 1)           -       6
!            ( 2, 1)           -       7
!            (-4, 2)           -       8
!            (-3, 2)           -       9
!            (-2, 2)           -      10
!            (-1, 2)           -      11
!            ( 0, 2)           -      12
!            ( 1, 2)           -      13
!            ( 2, 2)           -      14
!            (-4, 3)           -      15
!            (-3, 3)           -      16
!            (-2, 3)           -      17
!            (-1, 3)           -      18
!            ( 0, 3)           -      19
!            ( 1, 3)           -      20
!            ( 2, 3)           -      21
! \end{verbatim}
!
! Notice that the number of cells in source and destination Arrays need not
! match. Also, the only meaning the sequential indices carry is to provide
! a unique order for the source and destination cells for the sparse matrix
! multiplication.
!
! In order to carry out a sparse matrix multiplication of the data stored in 
! {\tt array1} and to store the result in {\tt array2} the non-zero values of
! the sparse matrix need to be provided. In case of an Array to Array 
! interpolation the matrix values correspond to the interpolation weights for 
! the specific case. A number of different schemes exist to generate the 
! coefficients from the physical grid information associated with the Arrays. 
! In ESMF the physical information stored in the Grids is not accessible on 
! the index space level on which the Array class is defined. For the Array
! an interpolation or regridding is simply a sparse matix multiplication.
!
! It is very common to compute the interpolation weights between two physical
! grids with an external tool, and then read these values in from file before
! the multiplication is to be performed. The ArraySparseMatMulStore() method 
! requires this information in two arguments on the rootPET. The 
! {\tt factorList} argument provides the non-zero coefficients as a list of 
! real numbers. The {\tt factorIndexList} is a two dimensional array that has 
! as many elements in the second dimension as there are non-zero factors. The 
! first dimension is of size 2 and provides the sequentialized index of the 
! source and destination cell, respectively.
!
! The ArraySparseMatMul() operation performs the following update on the
! destination array:
!
! \begin{verbatim}
!   do n=1, size(factorList)
!       dstArray(factorIndexList(2, n)) += 
!         factorList(n) * srcArray(factorIndexList(1, n))
!   enddo
! \end{verbatim}
!
! In principle, a full linear combination of all cells of {\tt array1} to all
! cells of {\tt array2} would require a total of 21 x 25 = 525 coefficients. 
! However, most of the coefficients will be zero. For this example assume that 
! only the following 4 factors are non-zero.
!
! \begin{verbatim}
!   factorList(:)     -   factorIndexList(1,:)  / factorIndexList(2,:)
!     0.5             -                   8     /     4
!     0.5             -                   9     /     4
!     0.8             -                  11     /    20
!     0.2             -                  15     /    20
! \end{verbatim}
!
!EOE
!BOC
  allocate(factorList(4), factorIndexList(2,4))
  factorList = (/0.5, 0.5, 0.8, 0.2/)       ! weights
  factorIndexList(1,:) = (/8, 9, 11, 15/)   ! source cell indices
  factorIndexList(2,:) = (/4, 4, 20, 20/)   ! destination cell indices
!EOC
!BOE
! With this information available on {\tt rootPET} the Array SparseMatMul
! can be precomputed and stored.
!EOE
!BOC
  call ESMF_ArraySparseMatMulStore(srcArray=array1, dstArray=array2, &
    factorList=factorList, factorIndexList=factorIndexList, rootPET=0, &
    routehandle=sparseMatMulHandle, rc=rc)
!EOC
!call ESMF_ArrayPrint(array1, rc=rc)
!call ESMF_ArrayPrint(array2, rc=rc)
!BOE
! The call to {\tt ESMF\_ArraySparseMatMulStore()} will have distributed the
! sparse matrix coefficients according to the distribution pattern of 
! {\tt array1} and {\tt array2}. Furthermore the exchange patterns of the 
! Array data has been precomputed and is stored in a Route object referenced
! by the returned RouteHandle object. The {\tt sparseMatMulHandle} can now be 
! used to execute the sparse matrix multiplication.
!EOE
!BOC
  call ESMF_ArraySparseMatMul(srcArray=array1, dstArray=array2, &
    routehandle=sparseMatMulHandle, rc=rc)
!EOC
!BOE
! This parallel sparse matrix multiplication call will have updated two cells
! of destination array ({\tt array2}):
!
! \begin{verbatim}
! array2(-1,1)  =  array2(-1,1)  +  0.5 * array1(3,2)  +  0.5 * array1(4, 2)
! \end{verbatim}
!
! and
!
! \begin{verbatim}
! array2(1,3)  =  array2(1,3)  +  0.8 * array1(1,3)  +  0.2 * array1(5, 3)
! \end{verbatim}
! 
! All other valuew in {\tt array2} will be left unchanged. 
!
! Finally the RouteHandle can be used to release all memory allocation 
! associated with the precomputed ArrayMatMul operation.
!
!EOE
!BOC
  call ESMF_RouteHandleRelease(routehandle=sparseMatMulHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
!call ESMF_ArrayPrint(array1, rc=rc)
!call ESMF_ArrayPrint(array2, rc=rc)
  
  call ESMF_ArrayDestroy(array1, rc=rc) ! finally destroy the array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_ArrayDestroy(array2, rc=rc) ! finally destroy the array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!!!! UNTIL FURTHER IMPLEMENTATION SKIP SECTIONS OF THIS EXAMPLE >>>>>>>>>>>>>>>>
#ifdef NOSKIP   
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!BOEI
!
!
!
! \subsubsection{Stagger locations and multiple Arrays with same DistGrid}
! \label{ArrayEx_staggerLocations}
! 
! The Array class is part of the ESMF index space layer. As such it does not
! store or interpret any information in terms of physical space. In index space
! each data element is addressed by a unique index tuple. The interpretation
! of where this data element is located physically is either left to the 
! application writer, who wants to use the index space layer directly, or to
! higher layers in the ESMF class structure (Grids and Fields).
!
! There is, however, a special kind of physical information that does affect
! the index space and must be considered within the Array and DistGrid classes.
! The
! index tuple that are used to identify data elements in index space specify 
! cells, but do not contain any information as to where {\em within} the 
! physical cell
! the data value actually is located. This orientation of the data point with
! respect to the cell is called its stagger location. In many cases the stagger
! location, which is physical information, has no effect on the index space.
! However, in some topologies the stagger location does matter for certain 
! operations and the index space layer must provide a mechanism to address the
! issue of stagger locations.
!
! The ESMF index space layer (DistGrid and Array) provides support for
! staggered data by means of a staggering index that is attached to the data.
! Much like the index space location of a cell the stagger location index does 
! not have a direct physical meaning attached and can be chosen arbitrarily.
! In fact. the staggering location index is by its nature independent of the 
! rank of the data and the decomposition. It is simply an integer number that
! distinguishes different stagger locations. The interpretation of the stagger 
! location in terms of a physical location in the cell is again left to the 
! user of the index space layer or higher ESMF classes.
!
! For the Arrays, that have been used throughout the previous sections
! of this document, support for staggering simply means that an additional 
! index variable is attached. This integer variable ({\tt staggerLoc}) can be 
! specified during Array creation, defaults to 0 if not specified, can be set
! after creation with {\tt ESMF\_ArraySet()} and can be queried for.
!
! In many practical applications a number of different quantities defined on the
! same grid will need to be stored and distributed in Array objects. Naturally
! all of these quantities are to be decomposed and distributed in the same manner
! across the computational resources in order to ensure good data locality.
! There are two ways the ESMF index space layer offers this to be done. First
! it is possible to use multiple Arrays (one Array for each quantity) that are
! all using the same DistGrid. Second a single Array may be used that 
! contains extra dimensions that are not distributed by the DistGrid and can be
! used
! to index the different quantities within the same Array. The first approach is
! a special case of the more general second approach but has the advantage of
! leading to simpler argument lists and thus will be used here to demonstrate 
! the concept.
!
! The following example uses the surface of a cylinder to demonstrate
! the concepts outlined above. There are two physical quantities that are to 
! be defined on the cylinder surface at different stagger locations. 
!
! First a suitable DistGrid must be created to define the index space and its
! topology. In this example the index space is a  {\tt 20 x 100} grid with
! the second dimension having a periodic boundary.
!EOEI
!BOCI
  allocate(connectionList(2*2+2,1))
  call ESMF_ConnectionElementConstruct(connectionElement=connectionList(:,1), &
    patchIndexA=1, patchIndexB=1, &
    positionVector=(/0, 100/), orientationVector=(/1, 2/), rc=rc)
  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  distgrid = ESMF_DistGridCreate(minCorner=(/1,1/), maxCorner=(/20,100/), &
    regDecomp=(/1,2*petCount/), connectionList=connectionList, rc=rc)
!EOCI
!BOEI
! Now {\tt distgrid} defines the index space for the problem. It also contains
! the decomposition description and decomposes the index space into twice as
! many DEs along the peripheral direction (second dimension) as there are PETs 
! in the current context. 
!
! Next the first Array object is created which will provide DE-local memory
! segments for the first quantity defined on {\tt distgrid}. The {\tt arrayspec}
! of the previous sections is suitable for a 2D Array of double precision real 
! numbers.
!EOEI
!BOCI
  array1 = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/0,1/), totalUWidth=(/0,1/), staggerLoc=1, rc=rc)
!EOCI
!BOEI
! Each DE in {\tt array1} will have allocated enough memory to hold the 
! exclusive region of cells (which is determined by the decomposition) plus
! a halo of one cell in positive and negative peripheral direction. Furthermore
! the {\tt staggerLoc} argument was used to indicate that the quantity stored
! in {\tt array1} is located on stagger location "1", which, at this point, is 
! nothing but an arbitrary index whose meaning will become clearer below.
!
! The second quantity will be provided in {\tt array2} using the same DistGrid
! object, thus defining it within the same index space and ensuring identical
! decomposition. However, {\tt array2} will have one more cell along the 
! negative peripheral direction in its computational region and no extra space
! for a halo.
!EOEI
!BOCI
  array2 = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    computationalLWidth=(/0,1/), staggerLoc=2, rc=rc)
!EOCI
!BOEI
! This puts {\tt array2} to be on stagger location "2" and distinguishes it from
! the stagger location given to {\tt array1}. The following diagram shows how the 
! application writer may interpret the current situation physically. But keep
! in mind that the ESMF index space layer makes no such interpretation, all it
! knows is that {\tt array1} and {\tt array2} are associated with 
! {\em different} stagger locations and that {\tt array2} has one more 
! computational cell in the negative second dimension. The diagram depicts 
! the situation for
! a single DE. The labels are {\tt a1} for {\tt array1}, {\tt a2} for 
! {\tt array2}, {\tt a1h} for the halo cells in {\tt array1} and 
! {\tt a2c} for the extra computational cells in {\tt array2}, 
!
! \begin{verbatim}
!   ----------------------------------------> 2nd dim
!   |
!   |   +---------+---------+---------+---------+---------+
!   |   | (1,-1)  | (1,1)   |         |         |         |
!   |   |         |         |         |         |         |
!   |   |  a1h  a2c   a1   a2   a1   a2   a1   a2   a1h   |
!   |   |         |         |         |         |         |
!   |   |         |         |         |         |         |
!   |   +---------+---------+---------+---------+---------+
!   |   |         |         |         |         |         |
!   |   |         |         |         |         |         |
!   |   |  a1h  a2c   a1   a2   a1   a2   a1   a2   a1h   |
!   |   |         |         |         |         |         |
!   |   |         |         |         |         |         |
!   |   +---------+---------+---------+---------+---------+
!   |   |         |         |         |         |         |
!   |   |         |         |         |         |         |
!   |   |  a1h  a2c   a1   a2   a1   a2   a1   a2   a1h   |
!   |   |         |         |         |         |         |
!   |   |         |         |         |         |         |
!   |   +---------+---------+---------+---------+---------+
!   |   |         |         |         | (20,N)  | (20,N+1)|
!   |   |         |         |         |         |         |
!   |   |  a1h  a2c   a1   a2   a1   a2   a1   a2   a1h   |
!   |   |         |         |         |         |         |
!   |   |         |         |         |         |         |
!   |   +---------+---------+---------+---------+---------+
!   | 
!   |
!   v
!  1st dim
! \end{verbatim}
!
! The way that the Array class defines the DE-local index space
! with regards to the exclusive region specified by the DistGrid ensures
! that the indexing of different Array objects, that use the same DistGrid,
! matches, as long as the same setting for {\tt indexflag} has been used. 
! For {\tt array1} and {\tt array2} this means that the following loop is well 
! defined.
!EOEI
!BOCI
  call ESMF_DistGridGet(distgrid, delayout=delayout, rc=rc)
  call ESMF_DELayoutGet(delayout, localDeCount=localDeCount, rc=rc)
  allocate(larrayList1(localDeCount))
  call ESMF_ArrayGet(array1, larrayList=larrayList1, rc=rc)
  allocate(larrayList2(localDeCount))
  call ESMF_ArrayGet(array2, larrayList=larrayList2, rc=rc)
  allocate(computationalLBound(localDeCount, 2))  ! rank=2
  allocate(computationalUBound(localDeCount, 2))  ! rank=2
  call ESMF_ArrayGet(array1, computationalLBound=computationalLBound, &
    computationalUBound=computationalUBound, rc=rc)
  do de=1, localDeCount
    call ESMF_LocalArrayGetData(larrayList1(de), myF90Array1, ESMF_DATA_REF, &
      rc=rc)
    call ESMF_LocalArrayGetData(larrayList2(de), myF90Array2, ESMF_DATA_REF, &
      rc=rc)
    ! use the computational bounds of array1 for the kernel
    do i=computationalLBound(de, 1), computationalUBound(de, 1)
      do j=computationalLBound(de, 2), computationalUBound(de, 2)
        ! (i,j) references the same cell in both Array objects!
        dummySum = dummySum + &
          (myF90Array2(i,j-1) - myF90Array2(i,j)) * myF90Array1(i,j)
      enddo
    enddo
  enddo
!EOCI
!BOEI
! 
!
!
!
! \subsubsection{ArrayBundles and communications}
!
! Arrays that are defined on the same DistGrid or on congruent DistGrids, i.e.
! DistGrids that cover the same index space and have the same decomposition and
! distribution, are also congruent in index space. Index space congruent Arrays
! may still have differing memory layouts, for example by defining differnt
! total regions. However, index space communication operations are stored in 
! Routes in a way to be compatible between index space congruent Arrays. Hence 
! an ArrayHalo stored for one Array object may be applied to any other index 
! space congruent Array.
!
! It is not uncommon in actual applications that the same communication pattern
! must be applied to a whole set of index space congruent Arrays. Fusion of 
! the individual communication operations often provides opportunity for 
! performance enhancements. Furthermore, the encapsulation of a set of Arrays 
! into a single bundle of Arrays makes code more readable and user-friendly.
! Finally, in cases such as demonstrated in the previous section 
! where {\tt array1} and {\tt array2} are defined as quantities on the same 
! DistGrid, differing only in the {\tt staggerLoc} index, it is very convenient
! to have a single object that can be used to reference both Arrays. The 
! {\tt ESMF\_ArrayBundle} class allows to create a single ArrayBundle object
! from a whole list of index space congruent Arrays.
!EOEI
!BOCI
  arrayBundle = ESMF_ArrayBundleCreate(arrayList=(/array1, array2/), rc=rc)
!EOCI
!BOEI
! The communication calls that are available for ArrayBundles are:
! \begin{itemize}
! \item Halo
! \item Redist
! \item SparseMatMul
! \end{itemize}
! It is for example possible to halo both
! {\tt array1} and {\tt array2} in a single operation.
!EOEI
!BOCI
  call ESMF_ArrayBundleHalo(arrayBundle, rc=rc)
!EOCI
!BOEI
! Communication operations can be precomputed and RouteHandles are available
! to use precomputed Routes in subsequent ArrayBundle communication calls.
!EOEI
!BOCI
  call ESMF_ArrayBundleHaloStore(arrayBundle, routehandle=haloHandle, rc=rc)
  call ESMF_ArrayBundleHaloRun(arrayBundle, routehandle=haloHandle, rc=rc)
!EOCI
!BOEI
! An ArrayBundle is destroyed calling the ArrayBundleDestroy method.
!EOEI
!BOCI
  call ESMF_ArrayBundleDestroy(arrayBundle, rc=rc)
!EOCI
!BOEI
! The individual Arrays that make up the bundle remain valid objects, i.e.
! ArrayBundleDestroy does not destroy or deallocate {\tt array1} and {\tt
! array2} of the current example.
!
!
! \subsubsection{Halo communication for staggered Arrays}
! \label{ArrayEx_staggeredArrays}
! 
! One of the key features of the distributed Array class is to support halo
! operations. Section \ref{ArrayEx_staggerLocations}
! introduced the concept of the staggering location index. Two Arrays were
! created on the same DistGrid, but were given different stagger location 
! indices. Until now this index remained unused.
!
! Besides the Array class the DistGrid class also makes reference to stagger
! location when connection transformations are explicitly defined. By default,
! i.e. without explicit {\tt connectionTransformList} argument during DistGrid
! creation, stagger locations are unaffected by connection transformations.
! For some topologies, however, stagger locations experience transformations as
! data is mapped through certain connections. The DistGrid contains this 
! information in the {\tt connectionTransformList} elements as {\tt staggerSrc}
! and {\tt staggerDst} variables.
!
! One of the obvious consequences of such a complex index space topology is that
! halo operations will need access to multiple stagger locations. First we 
! look at {\tt array1} and {\tt array2} as they were defined on the 
! simple topology of the last section. Stagger locations are unaffected
! by the connection that was used to enforce periodic boundary conditions. Both
! Arrays can be haloed independent of each other:
!EOEI
!BOCI
  call ESMF_ArrayHalo(array1, rc=rc)
  call ESMF_ArrayHalo(array2, rc=rc)
!EOCI
!BOEI
! This will change when a more complicated DistGrid is used instead.
!EOEI
!BOCI
  call ESMF_ArrayDestroy(array1, rc=rc)
  call ESMF_ArrayDestroy(array2, rc=rc)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
!EOCI  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOCI
  allocate(connectionTransformList(4+2*2,1))
  call ESMF_ConnectionTransformElementConstruct(connectionTransformList(:,1), &
    connectionIndex=1, direction=0, staggerSrc=2, staggerDst=1, &
    indexOffsetVector=(/0,0/), signChangeVector=(/+1,+1/), rc=rc)
  distgrid = ESMF_DistGridCreate(minCorner=(/1,1/), maxCorner=(/20,100/), &
    regDecomp=(/1,2*petCount/), connectionList=connectionList, &
    connectionTransformList=connectionTransformList, rc=rc)
  array1 = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/0,1/), totalUWidth=(/0,1/), staggerLoc=1, rc=rc)
  array2 = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    computationalLWidth=(/0,1/), staggerLoc=2, rc=rc)
!EOCI
!BOEI
! Now when the first or last DE of {\tt array1} performs a halo update it will 
! need to fill part of the halo region with data from {\tt array2} because 
! stagger location "2" maps to stagger location "1" following the connection 
! rule contained in the {\tt distgrid} object.
!
! Since the data for the two stagger locations are kept in two separate Array
! objects it is not possible to use an ArrayHalo method to satisfy all the 
! data dependencies. Calling ArrayHalo on {\tt array1} will result in an 
! incomplete halo update indicated by an error.
!
! In order to completely halo {\tt array1} it is necessary to construct a 
! self-contained ArrayBundle. In the current case this means that {\tt array1}
! and {\tt array2} must be bundled together
!EOEI
!BOCI
  arrayBundle = ESMF_ArrayBundleCreate(arrayList=(/array1, array2/), rc=rc)
!EOCI
!BOEI
! before the halo operation can be carried out.
!EOEI
!BOCI
  call ESMF_ArrayBundleHalo(arrayBundle, rc=rc)
!EOCI
!BOEI
! In the above case only {\tt array1} will have been haloed because {\tt array2}
! does not define any cells used by default to halo (the total region is 
! identical to the computational region). If, however, {\tt array2} had been
! defined with cells that would default into halo cells, the above halo call
! would try to update the halo regions of both arrays. The optional argument
! {\tt arrayIndex} can then be used to indicate which Array is supposed to be
! haloed.
!EOEI
!BOCI
  call ESMF_ArrayBundleHalo(arrayBundle, arrayIndex=1, rc=rc)
!EOCI
!BOEI
! Finally the objects used in this section can be destroyed.
!EOEI
!BOCI
  call ESMF_ArrayBundleDestroy(arrayBundle, rc=rc)
  call ESMF_ArrayDestroy(array1, rc=rc)
  call ESMF_ArrayDestroy(array2, rc=rc)
!EOCI

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#endif
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!BOE
! \subsubsection{1D and 3D Arrays}
! All previous examples were written for the 2D case. There is, however, no
! restriction within the Array or DistGrid class that limits the dimensionality
! of Array objects beyond the language specific limitations. 
!
! In order to create an {\tt n}-dimensional Array the rank indicated by both
! the {\tt arrayspec} and the {\tt distgrid} arguments specified during Array
! create must be equal to {\tt n}. A 1D Array of double precision real data
! hence requires the following {\tt arrayspec}.
!EOE  
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=1, &
    rc=rc)
!EOC
!BOE
! The index space covered by the Array and the decomposition description is
! provided to the Array create method by the {\tt distgrid} argument. The index
! space in this example has 16 cells and covers the interval $[-10, 5]$. It is 
! decomposed into as many DEs as there are PETs in the current context.
!EOE
!BOC
  distgrid1D = ESMF_DistGridCreate(minCorner=(/-10/), maxCorner=(/5/), &
    regDecomp=(/petCount/), rc=rc)
!EOC
!BOE
! A 1D Array object with default regions can now be created.
!EOE
!BOC
  array1D = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid1D, rc=rc)
!EOC
!BOE
! 
! The creation of a 3D Array proceeds analogous to the 1D case. The rank of the
! {\tt arrayspec} must be changed to 3
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=3, &
    rc=rc)
!EOC
!BOE
! and an appropriate 3D DistGrid object must be created
!EOE
!BOC
  distgrid3D = ESMF_DistGridCreate(minCorner=(/1,1,1/), maxCorner=(/16,16,16/), &
    regDecomp=(/4,4,4/), rc=rc)
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
  distgrid3D = ESMF_DistGridCreate(minCorner=(/1,1,1/), maxCorner=(/16,16,16/), &
    regDecomp=(/1,4,4/), rc=rc)
  array3D = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid3D, rc=rc)
!EOC
!BOE
! 
! Finally, the definition and usage of the stagger location index as it was
! described in sections \ref{ArrayEx_staggerLocations} and
! \ref{ArrayEx_staggeredArrays} for the 2D case applies without change to 
! 1D, 3D or any other dimensionality. Connections defined in the DistGrid object
! may utilize the stagger location index in order to express characteristics of
! the index space topology, The concept is completely rank independent.
!
!
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
  distgrid2D = ESMF_DistGridCreate(minCorner=(/1,1/), maxCorner=(/16,16/), &
    regDecomp=(/4,4/), delayout=delayout, rc=rc)
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, &
    rc=rc)
  array2D = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid2D, rc=rc)
!EOC
!BOE
! Now the following kernel is sure to work with {\tt array3D} and {\tt array2D}.
!EOE
!BOC
  call ESMF_DELayoutGet(delayout, localDeCount=localDeCount, rc=rc)
  allocate(larrayList1(localDeCount))
  call ESMF_ArrayGet(array3D, larrayList=larrayList1, rc=rc)
  allocate(larrayList2(localDeCount))
  call ESMF_ArrayGet(array2D, larrayList=larrayList2, rc=rc)
  do de=1, localDeCount
    call ESMF_LocalArrayGetData(larrayList1(de), myF90Array3D, ESMF_DATA_REF, &
      rc=rc)
    myF90Array3D = 0.1d0 * de ! initialize
    call ESMF_LocalArrayGetData(larrayList2(de), myF90Array2D, ESMF_DATA_REF, &
      rc=rc)
    myF90Array2D = 0.5d0 * de ! initialize
    do k=1, 4
      do j=1, 4
        dummySum = 0.d0
        do i=1, 16
          dummySum = dummySum + myF90Array3D(i,j,k) ! sum up the (j,k) column
        enddo
        dummySum = dummySum * myF90Array2D(j,k) ! multiply with local 2D element
!        print *, "dummySum(",j,k,")=",dummySum
      enddo
    enddo
  enddo
!EOC

!call ESMF_ArrayPrint(array3D)
!call ESMF_ArrayPrint(array2D)

!BOE
!
! \subsubsection{Array and DistGrid rank -- 2D+1 Arrays}
! All of the Array create interfaces require the specification of at least the 
! {\tt arrayspec} and the {\tt distgrid} arguments. Both arguments contain a
! sense of dimensionality. The interaction between these two arguments deserves
! extra attention.
!
! The {\tt arrayspec} argument is of type {\tt ESMF\_ArraySpec} and determines,
! among other things, the rank of the Array (data storage). This means, for 
! example, that the rank
! of a native language array extracted from an Array object is equal to that
! specified in the {\tt arrayspec} argument, which is also what is returned as
! {\tt rank} by the {\tt ESMF\_ArrayGet()} call. The {\tt arrayspec} argument 
! does not determine, however, how the Array dimensions are decomposed and 
! distributed.
!
! The rank specification contained in the {\tt distgrid} argument, which is of 
! type {\tt ESMF\_DistGrid}, on the other hand has no affect on the 
! dimensionality of the Array. The DistGrid rank specifies the dimensionality 
! of the {\em decomposition}, i.e. the number of Array dimensions that are
! decomposed. Consequently, the DistGrid rank must be smaller or equal to the
! Array rank.
!
! The DistGrid rank, furthermore, determines the dimensionality of the index 
! space of the Array. Array dimensions that do not correspond to DistGrid
! dimensions are considered extra or {\em tensor} dimensions of the Array. They
! are not part of the index space. Tensor dimensions are used to address
! multiple data storage arrays in the same Array object. It is, for example,
! possible to store {\tt array1} and {\tt array2} of section
! \ref{ArrayEx_staggeredArrays} in a single Array object using one 
! additional tensor dimension of size 2. The same {\tt distgrid} object as 
! before can be used to create the Array. The rank in the {\tt arrayspec} 
! argument, however, must be changed from 2 to 3 in order to provide for the 
! extra Array dimension. 
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=3, &
    rc=rc)
!EOC
!BOE
! During Array creation with extra dimension(s) it is necessary to specify the
! bounds of these tensor dimension(s). This requires two additional arguments,
! {\tt lbounds} and {\tt ubounds}, which are vectors in order to accommodate
! multiple tensor dimensions. The other arguments remain unchanged and apply
! across all tensor components. 
!
! The optional arguments used in the following call are identical to those
! used to create {\tt array1} of section \ref{ArrayEx_staggeredArrays}. This
! will set the total region and the stagger location of both tensor components 
! to be those of {\tt array1}.
!EOE
!BOC
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/0,1/), totalUWidth=(/0,1/), staggerLoc=1, &
    lbounds=(/1/), ubounds=(/2/), rc=rc)
    
  call ESMF_ArrayPrint(array, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!EOC
    
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!!!! UNTIL FURTHER IMPLEMENTATION SKIP SECTIONS OF THIS EXAMPLE >>>>>>>>>>>>>>>>
#ifdef NOSKIP   
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

!BOE
! This will create {\tt array} with 2+1 dimensions, i.e. a 2D DistGrid is used
! to describe the index space and decomposition into DEs and an extra Array 
! dimension captures the fact that multiple 2D user data arrays are kept in a 
! single data storage object (Array). By default the {\tt distgrid} dimensions 
! are associated with the first Array dimensions in sequence. For the example
! above this means that the first 2 Array dimensions are decomposed according to
! the provided 2D DistGrid. The 3rd Array dimension does not have an associated
! DistGrid dimension, rendering it a tensor dimension.
!
! The optional arguments that were used to create {\tt array} ensure that
! the {\em total region} is large enough to accommodate the 
! {\em halo} for tensor component 1 and the {\em computational region}
! for tensor component 2. However, the regions for tensor component 2 must still
! be adjusted to correctly reflect the {\tt array2} settings as in section
! \ref{ArrayEx_staggeredArrays}. The stagger location, too, must be changed 
! from "1" to "2" to match correctly. The Array class provides a special
! method that allows to individually address tensor elements in an Array and set
! stagger location, computational and halo widths.
!EOE
!BOC
  call ESMF_ArraySet(array, tensorIndex=(/2/), computationalLWidth=(/0,1/), &
    staggerLoc=2, rc=rc)
!EOC
!BOE
!
! The {\tt array} object is now completely self-contained with respect to the
! connection transformation stored in the DistGrid which mixes stagger location
! "1" and "2" when crossing the interface. Consequently, {\tt array} can be 
! haloed without the need to specify a list of Array objects.
!EOE
!BOC
  call ESMF_ArrayHalo(array, rc=rc)
!EOC
!BOE
! Native language access to an Array with tensor dimensions is in principle
! the same as without extra dimensions. The following loop shows how a Fortran90
! pointer to the DE-local data chunks can be obtained and used to set data 
! values in the exclusive regions. The {\tt myF90Array3} variable must be of 
! rank 3 to match the Array rank of {\tt array}.
! However, variables such as {\tt exclusiveUBound} that store the information
! about the decomposition, remain to be allocated for a 2D decomposition.
!EOE
!BOC
  call ESMF_ArrayGet(array, exclusiveLBound=exclusiveLBound, &
    exclusiveUBound=exclusiveUBound, rc=rc)
  do de=1, localDeCount
    call ESMF_LocalArrayGetData(larrayList(de), myF90Array3, ESMF_DATA_REF, rc=rc)
    myF90Array3(exclusiveLBound(de,1):exclusiveUBound(de,1), &
      exclusiveLBound(de,2):exclusiveUBound(de,2), 1) = 1
    myF90Array3(exclusiveLBound(de,1):exclusiveUBound(de,1), &
      exclusiveLBound(de,2):exclusiveUBound(de,2), 2) = 2
  enddo
!EOC
!BOE
! For some applications the default association rules between DistGrid and Array
! dimensions may not satisfy the user's needs. The optional {\tt dimmap} 
! argument may be used during Array creation to explicitly specify the mapping 
! between Array and DistGrid dimensions. To demonstrate this the
! following lines of code reproduce the above example but with rearranged
! dimensions. Here the {\tt dimmap} argument is a list with two elements 
! corresponding to the DistGrid rank of 2. The first element indicates against 
! which Array dimension the first DistGrid dimension is mapped. Here the 1st
! DistGrid dimension maps against the 3rd Array and the 2nd DistGrid dimension
! maps against the 1st Array dimension. This leaves the 2nd Array dimension
! to be the extra or tensor dimension of the created Array object.
!EOE
!BOC
  call ESMF_ArrayDestroy(array, rc=rc)
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    dimmap=(/3, 1/), totalLWidth=(/0,1/), totalUWidth=(/0,1/), &
    lbounds=(/1/), ubounds=(/2/), rc=rc)
  call ESMF_ArraySet(array, tensorIndex=(/1/), staggerLoc=1, rc=rc)
  call ESMF_ArraySet(array, tensorIndex=(/2/), computationalLWidth=(/0,1/), &
    staggerLoc=2, rc=rc)
!EOC
!BOE
! Operations on the Array object as a whole are unchanged by the different
! mapping of dimensions.
!EOE
!BOC
  call ESMF_ArrayHalo(array, rc=rc)
!EOC
!BOE
! When working with Arrays that contain explicitly mapped Array and DistGrid 
! dimensions it is critical to understand that {\em width} and {\em bound}
! arguments are always defined in terms of the DistGrid dimension order. The
! Array dimensions indicate how the data is actually stored in the Array
! object, and that can be different for each Array, even if the same DistGrid
! is used. The index space defined in DistGrid, however, does not change 
! and is the same for each Array that uses it, regardless of the dimension order
! in the Array. The DistGrid dimension order thus becomes a common
! reference order for all Arrays that use the same DistGrid.
!
! The {\tt dimmap} argument optionally provided during Array create indicates
! the DistGrid to Array dimension mapping. Depending on the formulation of the
! computational kernel, the inverse mapping, i.e. Array to DistGrid dimension
! mapping, is just as important. The {\tt ESMF\_ArrayGet()} call offers
! both mappings as {\tt dimmap} and {\tt inverseDimmap}, respectively. The
! number of elements in {\tt inverseDimmap} is equal to the rank of the Array.
! Each element corresponds to an Array dimension and indicates the associated
! DistGrid dimension by an integer number. An entry of "0" indicates an
! extra Array dimension.
!
! The association between Array and DistGrid dimensions becomes critical for
! correct native language access to the Array. In the following example the
! inverse mapping information is used
! to determine the correct bounds or the Array dimensions and to verify that
! the kernel's assumption about which Array dimension is of tensor character is
! correct. 
!EOE
!BOC
  allocate(inverseDimmap(3))  ! arrayRank = 3
  call ESMF_ArrayGet(array, inverseDimmap=inverseDimmap, &
    exclusiveLBound=exclusiveLBound, exclusiveUBound=exclusiveUBound, rc=rc)
  if (inverseDimmap(2) /= 0) then   ! check if extra dimension at expected index
    ! indicate problem and bail out
  endif
  ! prepare inverse dimmap variables for kernel loop
  idm1=inverseDimmap(1)
  idm3=inverseDimmap(3)
  do de=1, localDeCount
    call ESMF_LocalArrayGetData(larrayList(de), myF90Array3, ESMF_DATA_REF, rc=rc)
    myF90Array3(exclusiveLBound(de,idm1):exclusiveUBound(de,idm1), &
      1, exclusiveLBound(de,idm3):exclusiveUBound(de,idm3)) = 10.5 ! dummy assignment
    myF90Array3(exclusiveLBound(de,idm1):exclusiveUBound(de,idm1), &
      2, exclusiveLBound(de,idm3):exclusiveUBound(de,idm3)) = 23.3 ! dummy assignment
  enddo
!EOC

!BOEI
!
! \subsubsection{Array for a bipolar DistGrid}
!
! The Murray tripolar grid possesses two poles in the arctic region. The 
! implications of the bipolar grid of the northern hemisphere on the DistGrid
! and Array objects are discussed in this section.
!
! The bipolar region can either be defined as a single patch DistGrid or
! it can be composed of two or more separate patches. In this example two
! patches, each covering half of the index space of the bipolar region, will be
! used.
!
! The index space of the bipolar region remains logically rectangular (LR) and
! is assumed to be of size 360 x 50 cells for this example. The index order in
! the example is assumed $i,j$. The line for $j=1$ corresponds to a line of 
! constant latitude in the spherical coordinate system and the {\em bipolar
! fold} is along $j=50$. Two equal sized patches of each 180 x 50 cells need
! to be connected in a way that corresponds to the bipolar topology.
! 
!EOEI
!BOCI
  allocate(connectionList(2*2+2,1))   ! 1 connection: the bipolar fold
  call ESMF_ConnectionElementConstruct(connectionElement=connectionList(:,1), &
    patchIndexA=1, patchIndexB=2, &
    positionVector=(/179, 99/), orientationVector=(/-1, -2/), rc=rc)
  allocate(connectionTransformList(4+2*2,3))  ! 3 transforms: N, NE, E
  call ESMF_ConnectionTransformElementConstruct(connectionTransformList(:,1), &
    connectionIndex=1, direction=0, staggerSrc=1, staggerDst=1, &
    indexOffsetVector=(/0,-1/), signChangeVector=(/-1,-1/), rc=rc) ! N face
  call ESMF_ConnectionTransformElementConstruct(connectionTransformList(:,2), &
    connectionIndex=1, direction=0, staggerSrc=2, staggerDst=2, &
    indexOffsetVector=(/-1,-1/), signChangeVector=(/-1,-1/), rc=rc) ! NE point (U)
  call ESMF_ConnectionTransformElementConstruct(connectionTransformList(:,3), &
    connectionIndex=1, direction=0, staggerSrc=3, staggerDst=3, &
    indexOffsetVector=(/-1,0/), signChangeVector=(/-1,-1/), rc=rc) ! E face
!EOCI  
!BOEI
! With this {\tt connectionList} and {\tt connectionTransformList} it is now
! possible to define a DistGrid object that captures the index space topology
! for a bipolar grid. The DistGrid consists of two patches which need to be
! provided in {\tt minCorner} and {\tt maxCorner} list arguments.
!EOEI
!BOCI
  allocate(minCorner(2,2), maxCorner(2,2), regDecomp(2,2))
  minCorner(:,1) = (/1,1/)              ! first patch
  maxCorner(:,1) = (/180,50/)           ! first patch
  regDecomp(:,1) = (/petCount/2, 1/)    ! first patch
  minCorner(:,2) = (/1,1/)              ! second patch
  maxCorner(:,2) = (/180,50/)           ! second patch
  regDecomp(:,2) = (/petCount/2, 1/)    ! second patch
  
  distgrid = ESMF_DistGridCreate(minCorner=minCorner, maxCorner=maxCorner, &
    regDecomp=regDecomp, connectionList=connectionList, &
    connectionTransformList=connectionTransformList, rc=rc)
!EOCI  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOEI
! The decomposition described by {\tt regDecomp} assumes that there is an even 
! number of PETs available in the current context. The decomposition will be 
! into as many DEs as PETs. Half of the DEs handle the first patch and the other
! half of DEs handle the second patch.
!
! In order to create a 2D Array on this DistGrid for single precision
! real data the ArraySpec variable must be set correctly.
!EOEI
!BOCI
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R4, rank=2, &
    rc=rc)
!EOCI
!BOEI
! Finally the Array objects can be created for this {\tt arrayspec} and {\tt 
! distgrid}. The specification of the {\tt staggerLoc} argument will determine
! the connection transformation that will apply for the data stored in the 
! Array. The stagger location indices used in the definitions of the 
! connection transformations above were chosen arbitrarily, but now are used to
! specify Arrays that are to pick corresponding transformations through the
! bipolar fold.
!
! First a scalar tracer Array will be created. The default stagger location can
! be used in this case because "0" (the default) has not been used for any other
! stagger location in the definitions of the connection transformations. The 
! trace Array is created without halo cells.
!EOEI
!BOCI
  arrayTracer = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, rc=rc)
!EOCI
!BOEI
! Next an Array is created for a scalar living at the north face. The
! connection transformation corresponding to the north face was tagged with
! stagger location index "1" so the {\tt staggerLoc} for the Array must be set
! accordingly. Space for a halo width of one cell in each direction will be 
! provided for this quantity. 
!EOEI
!BOCI
  arrayNScalar = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/1,1/), totalUWidth=(/1,1/), staggerLoc=1, rc=rc)
!EOCI
!BOEI
! Finally Arrays for the horizontal velocity components are created at the 
! NE cell corner. The transformation behavior of this point is defined in 
! the DistGrid for stagger location index "2". Again space for a halo of one 
! cell in each direction is taken into account when creating the Arrays.
!EOEI
!BOCI
  arrayNEu = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/1,1/), totalUWidth=(/1,1/), staggerLoc=2, vectorDim=1, rc=rc)
  arrayNEv = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/1,1/), totalUWidth=(/1,1/), staggerLoc=2, vectorDim=2, rc=rc)
!EOCI
!BOEI
! Here the optional {\tt vectorDim} argument has been used to indicate that 
! these Arrays store components of a vector field. The information about which 
! vector component an Array contains is used to apply the correct
! {\tt signChangeVector} component when going though a connection with 
! associated transformation definition. For the bipolar DistGrid a sign change
! for each component of the horizontal velocity field has been indicated by the
! connection transformation.
!
! A consequence of the bipolar topology is that all three Arrays 
! {\tt arrayNScalar}, {\tt arrayNEu} and {\tt arrayNEv} contain redundant
! cells in their DE-local exclusive regions along the bipolar fold. These
! redundancies are automatically detected during Array creation. The Array
! class will define communication methods to monitor and enforce redundancies.
! Furthermore redundant cells in exclusive regions will also be taken into 
! account in Array reduce operations.
!
!EOEI

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#endif
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

10 continue
  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_ArrayEx.F90"
  else
    print *, "FAIL: ESMF_ArrayEx.F90"
  endif
  
end program
