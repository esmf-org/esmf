! $Id: ESMF_ArraySparseMatMulEx.F90,v 1.1.2.1 2007/12/12 06:08:48 theurich Exp $
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

program ESMF_ArraySparseMatMulEx

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: rc, petCount, localPet
  type(ESMF_VM):: vm
  type(ESMF_DistGrid):: srcDistGrid, dstDistGrid
  type(ESMF_Array):: srcArray, dstArray
  type(ESMF_ArraySpec):: arrayspec, arrayspec1D
  integer:: seqIndexList(2)
  type(ESMF_RouteHandle):: sparseMatMulHandle
  real(ESMF_KIND_R8), allocatable:: factorList(:)
  integer, allocatable:: factorIndexList(:,:)
  integer :: finalrc
  
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  if (petCount /= 4) goto 10 ! TODO: use EXAMPLES_MULTI_ONLY once available
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

!BOE
!
! \subsubsection{Communication -- SparseMatMul}
! \label{Array:SparseMatMul}
! 
! Sparse matrix multiplication is a fundamental Array communication method. One
! frequently used application of this method is the interpolation between pairs
! of Arrays. The principle is this: the value of each element in the exclusive 
! region of the destination Array is expressed as a linear combination of {\em 
! potentially all} the exclusive elements of the source Array. Naturally most of
! the coefficients of these linear combinations will be zero and it is more 
! efficient to store explicit information about the non-zero elements than to 
! keep track of all the coefficients.
!
! There is a choice to be made with respect to the format in which to store the
! information about the non-zero elements. One option is to store the value
! of each coefficient together with the corresponding destination element index
! and source element index. Destination and source indices could be expressed in
! terms of the corresponding DistGrid patch index together with the coordinate
! tuple within the patch. While this format may be the most natural way to
! express elements in the source and destination Array, it has two major drawbacks.
! First the coordinate tuple is {\tt dimCount} specific and second the format
! is extremly bulky. For 2D source and destination Arrays it would require 6
! integers to store the source and destination element information for each
! non-zero coefficient and matters get worse for higher dimensions.
!
! Both problems can be circumvented by {\em interpreting} source and destination
! Arrays as sequentialized strings or {\em vectors} of elements. This is done
! by assigning a unique {\em sequence index} to each exclusive element in both
! Arrays. With that the operation of updating the elements in the destination Array
! as linear combinations of source Array elements takes the form of a {\em sparse
! matrix multiplication}.
!
! The default sequence index rule assigns index $1$ to the {\tt minIndex} corner
! element of the first patch of the DistGrid on which the Array is defined. It then
! increments the sequence index by $1$ for each element running through the
! DistGrid dimensions by order. The index space position of the DistGrid patches
! does not affect the sequence labeling of elements. The default sequence indices
! for
!EOE
!BOC
  srcDistgrid = ESMF_DistGridCreate(minIndex=(/-1,0/), maxIndex=(/1,3/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! for each element are:
! \begin{verbatim}
!   -------------------------------------> 2nd dim
!   |
!   |   +------+------+------+------+
!   |   |(-1,0)|      |      |(-1,3)|
!   |   |      |      |      |      |
!   |   |   1  |   4  |   7  |  10  |
!   |   +------+------+------+------+
!   |   |      |      |      |      |
!   |   |      |      |      |      |
!   |   |   2  |   5  |   8  |  11  |
!   |   +------+------+------+------+
!   |   | (1,0)|      |      | (1,3)|
!   |   |      |      |      |      |
!   |   |   3  |   6  |   9  |  12  |
!   |   +------+------+------+------+
!   |
!   v
!  1st dim
! \end{verbatim}
!
! The assigned sequence indices are decomposition and distribution invariant by
! construction. Furthermore, when an Array is created with extra elements per DE on
! a DistGrid the sequence indices (which only cover the exclusive elements) remain
! unchanged.
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
!BOC
  srcArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=srcDistgrid, &
    totalLWidth=(/1,1/), totalUWidth=(/1,1/), indexflag=ESMF_INDEX_GLOBAL, &
    rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! The extra padding of 1 element in each direction around the exclusive elements on
! each DE are "invisible" to the Array spare matrix multiplication method. These
! extra elements are either updated by the computational kernel or by Array halo
! operations (not yet implemented!).
!
! An alternative way to assign sequence indices to all the elements in the patches
! covered by a DistGrid object is to use a special {\tt ESMF\_DistGridCreate()}
! call. This call has been specifically designed for 1D cases with arbitrary,
! user-supplied sequence indices.
!EOE
!BOC
  seqIndexList(1) = localPet*10
  seqIndexList(2) = localPet*10 + 1
  dstDistgrid = ESMF_DistGridCreate(arbSeqIndexList=seqIndexList, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! This call to {\tt ESMF\_DistGridCreate()} is collective across the current VM.
! The {\tt arbSeqIndexList} argument specifies the PET-local arbitrary sequence
! indices that need to be covered by the local DE. The resulting DistGrid has as
! many 1D patches as there are PETs in the current VM. There is one local DE per
! PET which covers the entire PET-local patch. The user supplied sequence
! indices must be unique, but the sequence may be interrupted. The four patches
! of {\tt dstDistgrid} have the following local 1D index space coordinates
! (given between "()") and sequence indices:
! \begin{verbatim}
!  patch 0            patch 1           patch 2           patch 3
!  covered by DE 0    covered by DE 1   covered by DE 2   covered by DE 3
!  on PET 0           on PET 1          on PET 2          on PET 3
!  ----------------------------------------------------------------------
!  (1) : 0            (1) : 10          (1) : 20          (1) : 30
!  (2) : 1            (2) : 11          (2) : 21          (2) : 31
! \end{verbatim}
!
! Again the DistGrid object provides the sequence index labeling for the
! exclusive elements of an Array created on the DistGrid regardless of extra,
! non-exclusive elements.
!EOE
  call ESMF_ArraySpecSet(arrayspec1D, typekind=ESMF_TYPEKIND_R8, rank=1, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  dstArray = ESMF_ArrayCreate(arrayspec=arrayspec1D, distgrid=dstDistgrid, &
    rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! With the definition of sequence indices, either by the default rule or as user
! provided arbitrary sequence indices, it is now possible to uniquely identify
! each exclusive element in the source and destination Array by a single integer
! number. Specifying a pair of source and destination elements takes two integer
! number regardless of the number of dimensions.
!
! The information required to carry out a sparse matrix multiplication are the
! pair of source and destination sequence indices and the associated
! multiplication factor for each pair. ESMF requires this information in form of
! two Fortran arrays. The factors are stored in a 1D array of the appropriate
! type and kind, e.g. {\tt real(ESMF\_KIND\_R8)::factorList(:)}. Array sparse
! matrix multiplications are only supported between Arrays of the same type and
! kind using factors of identical type and kind. The sequence index pairs
! associated with the factors provided by {\tt factorList} are stored in a 2D
! Fortran array of default integer kind of the shape {\tt
! integer::factorIndexList(2,:)}. The sequence indices of the source Array elements
! are stored in the first row of {\tt 
! factorIndexList} while the sequence indices of the destination Array elements are
! stored in the second row.
!
! Each PET in the current VM must call into {\tt ESMF\_ArraySparseMatMulStore()}
! to precompute and store the communication pattern for the sparse matrix
! multiplication. The multiplication factors may be provided in parallel, i.e.
! multiple PETs may specify {\tt factorList} and {\tt factorIndexList} arguments
! when calling into {\tt ESMF\_ArraySparseMatMulStore()}. PETs that do not
! provide factors either call with {\tt factorList} and {\tt factorIndexList}
! arguments containing zero elements or issue the call omitting both arguments.
!EOE
!BOC
  if (localPet == 0) then
    allocate(factorList(1))               ! PET 0 specifies 1 factor
    allocate(factorIndexList(2,1))
    factorList = (/0.2/)                  ! factors
    factorIndexList(1,:) = (/5/)          ! seq indices into srcArray
    factorIndexList(2,:) = (/30/)         ! seq indices into dstArray
    
    call ESMF_ArraySparseMatMulStore(srcArray=srcArray, dstArray=dstArray, &
      routehandle=sparseMatMulHandle, factorList=factorList, &
      factorIndexList=factorIndexList, rc=rc)
      
    deallocate(factorList)
    deallocate(factorIndexList)
  else if (localPet == 1) then
    allocate(factorList(3))               ! PET 1 specifies 3 factor
    allocate(factorIndexList(2,3))
    factorList = (/0.5, 0.5, 0.8/)        ! factors
    factorIndexList(1,:) = (/8, 2, 12/)   ! seq indices into srcArray
    factorIndexList(2,:) = (/11, 11, 30/) ! seq indices into dstArray
    
    call ESMF_ArraySparseMatMulStore(srcArray=srcArray, dstArray=dstArray, &
      routehandle=sparseMatMulHandle, factorList=factorList, &
      factorIndexList=factorIndexList, rc=rc)
      
    deallocate(factorList)
    deallocate(factorIndexList)
  else
    ! PETs 2 and 3 do not provide factors
    
    call ESMF_ArraySparseMatMulStore(srcArray=srcArray, dstArray=dstArray, &
      routehandle=sparseMatMulHandle, rc=rc)
      
  endif
!EOC
!BOE
! The RouteHandle object {\tt sparseMatMulHandle} produced by 
! {\tt ESMF\_ArraySparseMatMulStore()} can now be used to call {\tt
! ESMF\_ArraySparseMatMul()} collectively across all PETs of the current VM to
! perform
! \begin{verbatim}
!   dstArray = 0.0
!   do n=1, size(combinedFactorList)
!       dstArray(combinedFactorIndexList(2, n)) += 
!         combinedFactorList(n) * srcArray(combinedFactorIndexList(1, n))
!   enddo
! \end{verbatim}
! in parallel. Here {\tt combinedFactorList} and {\tt combinedFactorIndexList}
! are the combined lists defined by the respective local lists provided by 
! PETs 0 and 1 in parallel. For this example
!EOE
!BOC
  call ESMF_ArraySparseMatMul(srcArray=srcArray, dstArray=dstArray, &
    routehandle=sparseMatMulHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! will initialize the entire {\tt dstArray} to 0.0 and then update two elements:
!
! \begin{verbatim}
! on DE 1:
! dstArray(2) = 0.5 * srcArray(0,0)  +  0.5 * srcArray(0,2)
! \end{verbatim}
!
! and
!
! \begin{verbatim}
! on DE 3:
! dstArray(1) = 0.2 * srcArray(0,1)  +  0.8 * srcArray(1,3).
! \end{verbatim}
!
! The call to {\tt ESMF\_ArraySparseMatMul()} does provide the option to turn
! the default {\tt dstArray} initialization off. If argument {\tt zeroflag}
! is set to {\tt ESMF\_FALSE}
!EOE
!BOC
  call ESMF_ArraySparseMatMul(srcArray=srcArray, dstArray=dstArray, &
    routehandle=sparseMatMulHandle, zeroflag=ESMF_FALSE, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! skips the initialization and elements in {\tt dstArray} are updated according to:
!
! \begin{verbatim}
!   do n=1, size(combinedFactorList)
!       dstArray(combinedFactorIndexList(2, n)) += 
!         combinedFactorList(n) * srcArray(combinedFactorIndexList(1, n)).
!   enddo
! \end{verbatim}
!
! The resources held by {\tt sparseMatMulHandle} need to be deallocated by the
! user code before the handle becomes inaccessible.
!EOE
!BOC
  call ESMF_RouteHandleRelease(routehandle=sparseMatMulHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
!call ESMF_ArrayPrint(srcArray, rc=rc)
!call ESMF_ArrayPrint(dstArray, rc=rc)
  
  call ESMF_ArrayDestroy(srcArray, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(srcDistgrid, rc=rc) ! destroy the DistGrid object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_ArrayDestroy(dstArray, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(dstDistgrid, rc=rc) ! destroy the DistGrid object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
10 continue
  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_ArraySparseMatMulEx.F90"
  else
    print *, "FAIL: ESMF_ArraySparseMatMulEx.F90"
  endif
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
  
end program
