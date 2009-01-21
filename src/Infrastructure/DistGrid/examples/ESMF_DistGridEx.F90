! $Id: ESMF_DistGridEx.F90,v 1.21.2.3 2009/01/21 21:25:20 cdeluca Exp $
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

program ESMF_DistGridEx

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: rc, de, i, dim, petCount
!  integer:: nodeCount, dimCount, deNeighborCount, linkCount
  integer:: localPet, localDeCount, localDe
  type(ESMF_VM):: vm
  type(ESMF_DELayout):: delayout
  type(ESMF_DistGrid):: distgrid
!  type(ESMF_DistGrid):: distgrid3D, distgrid2D
  integer, allocatable:: dimExtent(:,:), localIndexList(:)
!  integer, allocatable:: regDecompDeCoord(:), connectionTransformList(:,:)
  integer, allocatable:: minIndex(:,:), maxIndex(:,:), regDecomp(:,:)
  integer, allocatable:: deBlockList(:,:,:), connectionList(:,:)
!  integer, allocatable:: deNeighborList(:), deNeighborInterface(:,:)
  integer, allocatable:: localDeList(:)
!  integer, allocatable:: linkList(:,:)
!  type(ESMF_Logical):: regDecompFlag
  ! result code
  integer :: finalrc
  
  
  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  if (petCount /= 4) goto 10 ! TODO: use EXAMPLES_MULTI_ONLY once available

!BOE
! \subsubsection{Single patch DistGrid with regular decomposition}
! 
! The minimum information required to create an {\tt ESMF\_DistGrid} object
! for a single patch with default decomposition are the corners of the patch
! in index space. The following call will create a 1D DistGrid for a 
! 1D index space patch with elements from 1 through 1000.
!EOE

!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/1000/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! A default DELayout with 1 DE per PET will be created during 
! {\tt ESMF\_DistGridCreate()}. The 1000 elements of the specified 1D patch will
! then be block decomposed across the available DEs, i.e. across all PETs. 
! Hence, for 4 PETs the (min) $\sim$ (max) corners of the DE-local LR regions
! will be:
! \begin{verbatim}
!   DE 0 - (1) ~ (250)
!   DE 1 - (251) ~ (500)
!   DE 2 - (501) ~ (750)
!   DE 3 - (751) ~ (1000)
! \end{verbatim}
!
! DistGrids with rank > 1 can also be created with default decompositions,
! specifying only the corners of the patch. The following will create a
! 2D DistGrid for a 5x5 patch with default decomposition.
!EOE

!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! The default decomposition for a DistGrid of rank $N$ will be $ (nDEs \times 1
! \times ... \times 1) $, where $nDEs$ is the number of DEs in the DELayout
! and there are $N-1$ factors of $1$. For the 2D example above this means
! a $4 \times 1$ regular decomposition if executed on 4 PETs and will result
! in the following DE-local LR regions:
! \begin{verbatim}
!   DE 0 - (1,1) ~ (2,5)
!   DE 1 - (3,1) ~ (3,5)
!   DE 2 - (4,1) ~ (4,5)
!   DE 3 - (5,1) ~ (5,5)
! \end{verbatim}
!
! In many cases the default decomposition will not suffice for higher rank
! DistGrids (rank > 1). For this reason a decomposition descriptor 
! {\tt regDecomp} argument is available during {\tt ESMF\_DistGridCreate()}. The
! following call creates a DistGrid on the same 2D patch as before, but now with
! a user specified regular decomposition of $2 \times 3 = 6 $ DEs.
!EOE

!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! The default DE labeling sequence follows column major order for the
! {\tt regDecomp} argument:
! \begin{verbatim}
!   -----------> 2nd dimension
!   |  0  2  4
!   |  1  3  5
!   v
!  1st dimension
! \end{verbatim}
!
! By default grid points along all dimensions are homogeneously divided between
! the DEs. The maximum element count difference between DEs along any dimension
! is 1. The (min) $\sim$ (max) corners of the DE-local LR domains of the above
! example are as follows:
! \begin{verbatim}
!   DE 0 - (1,1) ~ (3,2)
!   DE 1 - (4,1) ~ (5,2)
!   DE 2 - (1,3) ~ (3,4)
!   DE 3 - (4,3) ~ (5,4)
!   DE 4 - (1,5) ~ (3,5)
!   DE 5 - (4,5) ~ (5,5)
! \end{verbatim}
! 
! The specifics of the patch decomposition into DE-local LR domains can be
! modified by the optional {\tt decompflag} argument. The following line shows
! how this argument is used to keep ESMF's default decomposition in the first
! dimension but move extra grid points of the second dimension to the last DEs
! in that direction. Extra elements occur if the number of DEs for a certain
! dimension does not evenly divide its extent. In this example there are
! 2 extra grid points for the second dimension because its extent is 5 but there
! are 3 DEs along this index space axis.
!EOE
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), decompflag=(/ESMF_DECOMP_DEFAULT,ESMF_DECOMP_RESTLAST/),&
    rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! Now DE 4 and DE 5 will hold the extra elements along the 2nd dimension.
! \begin{verbatim}
!   DE 0 - (1,1) ~ (3,1)
!   DE 1 - (4,1) ~ (5,1)
!   DE 2 - (1,2) ~ (3,2)
!   DE 3 - (4,2) ~ (5,2)
!   DE 4 - (1,3) ~ (3,5)
!   DE 5 - (4,3) ~ (5,5)
! \end{verbatim}
!
! An alternative way of indicating the DE-local LR regions is to list the 
! index space coordinate as given by the associated DistGrid patch for each
! dimension. For this 2D example there are two lists (dim 1) / (dim 2) for each
! DE:
! \begin{verbatim}
!   DE 0 - (1,2,3) / (1)
!   DE 1 - (4,5)   / (1)
!   DE 2 - (1,2,3) / (2)
!   DE 3 - (4,5)   / (2)
!   DE 4 - (1,2,3) / (3,4,5)
!   DE 5 - (4,5)   / (3,4,5)
! \end{verbatim}
!
! Information about DE-local LR regions in the latter format can be obtained 
! from the DistGrid object by use of {\tt ESMF\_DistGridGet()} methods:
!
!BOC
  allocate(dimExtent(2, 0:5)) ! (dimCount, deCount)
  call ESMF_DistGridGet(distgrid, delayout=delayout, &
    indexCountPDimPDe=dimExtent, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DELayoutGet(delayout, localDeCount=localDeCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  allocate(localDeList(0:localDeCount-1))
  call ESMF_DELayoutGet(delayout, localDeList=localDeList, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  do localDe=0, localDeCount-1
    de = localDeList(localDe)
    do dim=1, 2
      allocate(localIndexList(dimExtent(dim, de))) ! allocate list to hold indices
      call ESMF_DistGridGet(distgrid, localDe=localDe, dim=dim, &
        indexList=localIndexList, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
      print *, "local DE ", localDe," - DE ",de," localIndexList along dim=", &
        dim," :: ", localIndexList
      deallocate(localIndexList)
    enddo
  enddo
  deallocate(localDeList)
  deallocate(dimExtent)
!EOC  
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! The advantage of the {\tt localIndexList} format over the min-/max-corner 
! format is that it can be used directly for DE-local to patch index 
! dereferencing. Furthermore the {\tt localIndexList} allows to express very
! general decompositions such as the cyclic decompositions in the first
! dimension generated by the following call:
!EOE

!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), decompflag=(/ESMF_DECOMP_CYCLIC,ESMF_DECOMP_RESTLAST/),&
    rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! with decomposition:
! \begin{verbatim}
!   DE 0 - (1,3,5) / (1)
!   DE 1 - (2,4)   / (1)
!   DE 2 - (1,3,5) / (2)
!   DE 3 - (2,4)   / (2)
!   DE 4 - (1,3,5) / (3,4,5)
!   DE 5 - (2,4)   / (3,4,5)
! \end{verbatim}
!
! Finally, a DistGrid object is destroyed by calling
!EOE
!BOC
  call ESMF_DistGridDestroy(distgrid, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOE
! \subsubsection{DistGrid and DELayout}
! 
! The examples of this section use the 2D DistGrid of the previous section 
! to show the interplay between DistGrid and DELayout. By default, i.e.
! without specifying the {\tt delayout} argument, a DELayout will be created
! during DistGrid creation that provides as many DEs as the DistGrid
! object requires. The implicit call to {\tt ESMF\_DELayoutCreate()} is issued
! with a fixed number of DEs and default settings in all other aspects. The
! resulting DE to PET mapping depends on the number of PETs of the current VM
! context. Assuming 6 PETs in the VM
!EOE
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DistGridGet(distgrid, delayout=delayout, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DELayoutPrint(delayout, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! will result in the following domain decomposition in terms of DEs
! \begin{verbatim}
!   0  2  4
!   1  3  5
! \end{verbatim}
! and their layout or distribution over the available PETs:
! \begin{verbatim}
!   DE 0  -> PET 0
!   DE 1  -> PET 1
!   DE 2  -> PET 2
!   DE 3  -> PET 3
!   DE 4  -> PET 4
!   DE 5  -> PET 5
! \end{verbatim}
! 
! Running the same example on a 4 PET VM will not change the domain 
! decomposition into 6 DEs as specified by
! \begin{verbatim}
!   0  2  4
!   1  3  5
! \end{verbatim}
! but the layout across PETs will now contain multiple DE-to-PET mapping with 
! default cyclic distribution:
! \begin{verbatim}
!   DE 0  -> PET 0
!   DE 1  -> PET 1
!   DE 2  -> PET 2
!   DE 3  -> PET 3
!   DE 4  -> PET 0
!   DE 5  -> PET 1
! \end{verbatim}
!
! Sometimes it may be desirable for performance tuning to construct a DELayout
! with specific characteristics. For instance, if the 6 PETs of the above 
! example are running on 3 nodes of a dual-SMP node cluster and there is a 
! higher communication load along the first dimension of the model than along 
! the second dimension it would be sensible to place DEs according to this 
! knowledge.
!EOE

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!!!! UNTIL FURTHER IMPLEMENTATION SKIP >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ifdef NOSKIP
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

!BOEI
! There are two ways to accomplish this in ESMF. First the 
! {\tt fastAxis} argument can be used when creating the DistGrid object to
! indicate which axis should have faster communication characteristics:
!EOEI
!BOCI
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), fastAxis=1, rc=rc)
!EOCI
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOEI
! A second way to achieve the same distribution is to explicitly create a
! suitable DELayout object.
!EOEI

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#endif
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

!BOE
! The following example first creates a DELayout 
! with 6 DEs where groups of 2 DEs are to be in fast connection. This DELayout 
! is then used to create a DistGrid.
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(deCount=6, deGrouping=(/(i/2,i=0,5)/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), delayout=delayout, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! This will ensure a distribution of DEs across the cluster resource 
! in the following way:
! \begin{verbatim}
!   0   2   4
!   1   3   5
!  SMP SMP SMP
! \end{verbatim}
! 
! The interplay between DistGrid and DELayout may at first seem complicated.
! The simple but important rule to understand is that DistGrid describes a 
! domain decomposition and each domain is labeled with a DE number. The DELayout
! describes how these DEs are laid out over the compute resources of the VM, 
! i.e. PETs. The DEs are purely logical elements of decomposition and may be 
! relabeled to fit the algorithm or legacy code better. The following 
! example demonstrates this by describing the exact same distribution of the 
! domain data across the fictitious cluster of SMP-nodes with a different 
! choice of DE labeling:
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(deCount=6, deGrouping=(/(mod(i,3),i=0,5)/), &
    rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), deLabelList=(/0,3,1,4,2,5/), delayout=delayout, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! Here the {\tt deLabelList} argument changes the default DE label sequence from
! column major to row major. The DELayout compensates for this change in DE
! labeling by changing the {\tt deGrouping} argument to map the first dimension
! to SMP nodes as before. The decomposition and layout now looks as follows:
! \begin{verbatim}
!   0   1   2
!   3   4   5
!  SMP SMP SMP
! \end{verbatim}
! 
! Finally, in order to achieve a completely user-defined distribution of the
! domain data across the PETs of the VM a DELayout may be created from a
! {\tt petMap} before using it in the creation of a DistGrid. If for
! instance the desired distribution of a 2 x 3 decomposition puts the DEs of 
! the first row onto 3 separate PETs (PET 0, 1, 2) and groups the DEs of 
! the second row onto PET 3 a {\tt petMap} must first be setup that
! takes the DE labeling of the DistGrid into account.The following lines of 
! code result in the desired distribution using column major DE labeling by 
! first create a DELayout and then using it in the DistGrid creation.
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(petMap=(/0,3,1,3,2,3/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), delayout=delayout, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DistGridGet(distgrid, delayout=delayout, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DELayoutPrint(delayout, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! This decomposes the global domain into
! \begin{verbatim}
!   0   2   4
!   1   3   5
! \end{verbatim}
! and associates the DEs to the following PETs:
! \begin{verbatim}
!   DE 0  -> PET 0
!   DE 1  -> PET 3
!   DE 2  -> PET 1
!   DE 3  -> PET 3
!   DE 4  -> PET 2
!   DE 5  -> PET 3
! \end{verbatim}
!EOE


! - gjt commented out the following section because it's too advanced for now
!BOEI
! \subsubsection{DistGrid and DELayout with explicit virtual DEs}
! 
! The examples of the previous section showed how the DistGrid and DELayout
! classes interact. It was also mentioned that if the number of DEs indicated
! by {\tt regDecomp} exceeds the number of available PETs the implicitly 
! created DELayout will have virtual DEs. By default the virtual DEs will be 
! block distributed across the available PETs.
!
! If the block distribution of virtual DEs is not desirable an explicitly
! defined DELayout must be used. One way of doing this is to use a {\tt petMap}
! as described in the last example of the previous section. This allows a
! completely arbitrary definition of virtual DE sets. Often times, 
! however, it is not necessary to go down to the explicit DE to PET mapping
! level to achieve the desired distribution of virtual DEs.
!
! The following example shows how to ensure for a regular 16 x 32 = 512 DE 
! decomposition that is being run on 16 PETs that DEs along the second 
! dimension are grouped as virtual DEs.
!EOEI
! 
!BOCI
!  delayout = ESMF_DELayoutCreate(deCount=16*32, virtualStride=(/1,16/), rc=rc)
!EOCI  
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOCI
!  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/64,128/), &
!    regDecomp=(/16,32/), delayout=delayout, rc=rc)
!EOCI  
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DistGridDestroy(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DELayoutDestroy(delayout, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOEI
!
! Assuming the associated VM contains PETs with multiple PEs the above example 
! results in a layout of the domain decomposition suitable for
! user-level OpenMP threading along the second dimension.
!
! The next example demonstrates a more complex case of decomposing the same
! global domain and laying the DEs out over an arbitrary number of 4-way SMP
! nodes. The goal is to arrange the decomposition and DELayout in a way to 
! facilitate work queue load-balancing along the second dimension. For this it
! further assumed that the VM context of the component was created with ESMF 
! multi-threading. For the 4=way SMP node case this means that always 4 PETs 
! form a thread group and run in the same virtual address space (VAS). The 
! DELayout created by:
!EOEI
!BOCI
!  nodeCount = petCount/4
!  delayout = ESMF_DELayoutCreate(deCount=nodeCount*32, &
!    virtualStride=(/1, nodeCount/), virtualDePinFlag=ESMF_VIRTUALDE_PIN_VAS, &
!    rc=rc)
!EOCI  
 ! if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOEI
! contains 8 times as many DEs than there are PETs and groups them into
! {\tt nodeCount} virtual groups that are pinned against VAS. Using this
! DELayout in the decomposition of the global domain according to
!EOEI
!BOCI
!  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/64,128/), &
!    regDecomp=(/nodeCount,32/), delayout=delayout, rc=rc)
!EOCI
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DistGridDestroy(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DELayoutDestroy(delayout, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOEI
! ensures that each set of 32 DEs is associated with 4 PETs that form a
! thread group. Work-queue dynamic load balancing within these DE sets can be 
! implemented in user code using the {\tt ESMF\_DELayoutServiceOffer()} and
! {\tt ESMF\_DELayoutServiceComplete()} calls as discussed in the DELayout 
! reference document.
!EOEI


!BOE
! \subsubsection{Single patch DistGrid with decomposition by DE blocks}
! 
! The examples of the previous sections showed how DistGrid objects with
! regular decompositions are created. However, in some cases a regular 
! decomposition may not be specific enough. The following example shows how 
! the {\tt deBlockList} argument is used to create a DistGrid object with 
! completely user-defined decomposition.
!
! A single 5x5 LR domain is to be decomposed into 6 DEs. To this end a list is
! constructed that holds the min and max corners of all six DE
! LR blocks. The DE-local LR blocks are arranged as to cover the whole patch 
! domain without overlap.
!EOE
! 
!BOC
  allocate(deBlockList(2, 2, 6))  ! (dimCount, 2, deCount)
  deBlockList(:,1,1) = (/1,1/)  ! minIndex  1st deBlock
  deBlockList(:,2,1) = (/3,2/)  ! maxIndex  1st deBlock
  deBlockList(:,1,2) = (/4,1/)  ! minIndex  2nd deBlock
  deBlockList(:,2,2) = (/5,2/)  ! maxIndex  2nd deBlock
  deBlockList(:,1,3) = (/1,3/)
  deBlockList(:,2,3) = (/2,4/)
  deBlockList(:,1,4) = (/3,3/)
  deBlockList(:,2,4) = (/5,4/)
  deBlockList(:,1,5) = (/1,5/)
  deBlockList(:,2,5) = (/3,5/)
  deBlockList(:,1,6) = (/4,5/)  ! minIndex  6th deBlock
  deBlockList(:,2,6) = (/5,5/)  ! maxInbex  6th deBlock
!EOC
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    deBlockList=deBlockList, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! \subsubsection{Single patch DistGrid with periodic boundaries}
! 
! By default the edges of all patches have solid wall boundary conditions. 
! Periodic boundary conditions can be imposed by specifying connections between
! patches. For the single LR domain of the last section periodic boundaries 
! along the first dimension are imposed by adding a
! {\tt connectionList} argument with only one element to the create call.
!
! Each {\tt connectionList} element is a vector of {\tt (3 * dimCount + 2)}
! integer numbers:
!EOE
!BOC
  allocate(connectionList(3*2+2, 1))  ! (3*dimCount+2, number of connections)
!EOC
!BOE
! and has the following format:
!
! {\tt (/patchIndex\_A, patchIndex\_B, positionVector, orientationVector,
! repetitionVector/)}.
!
! The following constructor call can be used to construct a suitable connectionList
! element.
!EOE
!BOC
  call ESMF_DistGridConnection(connection=connectionList(:,1), &
     patchIndexA=1, patchIndexB=1, &
     positionVector=(/5, 0/), &
     orientationVector=(/1, 2/), &
     repetitionVector=(/1, 0/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
!  print *, "connectionList(:,1) = ", connectionList(:,1)
  


!BOE
! The {\tt patchIndexA} and {\tt patchIndexB} arguments specify that this is a
! connection within patch 1. The {\tt positionVector} indicates that there is no
! offset between patchB and patchA along the second dimension, but there is
! an offset of 5 along the first dimension (which in this case is the length of
! dimension 1). This aligns patchB (which is patch 1) right next to patchA
! (which is also patch 1).
!
! The {\tt orientationVector} fixes the orientation of the patchB index space to
! be the same as the orientation of patchA (it maps index 1 of patchA to index 1
! of patchB and the same for index 2). The {\tt orientationVector} could have
! been omitted in this case which corresponds to the default orientation.
!
! Finally, the {\tt repetitionVector} idicates that this connetion element will
! be periodically repeated along dimension 1.
!
! The {\tt connectionList} can now be used to create a {\tt DistGrid} object with the
! desired boundary conditions.
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    deBlockList=deBlockList, connectionList=connectionList, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  deallocate(connectionList)
!EOC
  deallocate(deBlockList)
!BOE
! This closes the patch along the first dimension on itself, thus imposing
! periodic boundaries along this direction.
!EOE

!BOE
! \subsubsection{2D patchwork DistGrid with regular decomposition}
! 
! Creating a DistGrid from a list of LR domains is a straight forward
! extension of the case with a single LR domain. The first four 
! arguments of {\tt ESMF\_DistGridCreate()} are promoted to rank 2, the 
! second dimension being the patch count index.
! 
! The following 2D patchwork domain consisting of 3 LR patches will 
! be used in the examples of this section:
! \begin{verbatim}
!   ----------------------------------------> 2nd dim
!   |
!   |                   (1,11)-----(1,20)
!   |                   |               | 
!   |                   |               | 
!   |                   |               | 
!   |                   |               | 
!   |                   |               | 
!   |                   (10,11)---(10,20)
!   |  (11,1)----(11,10)(11,11)---(11,20)
!   |  |               ||               |
!   |  |               ||               |
!   |  |               ||               |
!   |  |               ||               |
!   |  |               ||               |
!   |  (20,1)----(20,10)(20,11)---(20,20)
!   |
!   |
!   v
!  1st dim
! \end{verbatim}
!
! The first step in creating a patchwork global domain is to construct the
! {\tt minIndex} and {\tt maxIndex} arrays.
!EOE
!BOC
  allocate(minIndex(2,3))    ! (dimCount, number of patches)
  allocate(maxIndex(2,3))    ! (dimCount, number of patches)
  minIndex(:,1) = (/11,1/)
  maxIndex(:,1) = (/20,10/)
  minIndex(:,2) = (/11,11/)
  maxIndex(:,2) = (/20,20/)
  minIndex(:,3) = (/1,11/)
  maxIndex(:,3) = (/10,20/)
!EOC  
!BOE
! Next the regular decomposition for each patch is set up in the
! {\tt regDecomp} array. In this example each patch is associated with a
! single DE.
!EOE
!BOC
  allocate(regDecomp(2,3))    ! (dimCount, number of patches)
  regDecomp(:,1) = (/1,1/)    ! one DE
  regDecomp(:,2) = (/1,1/)    ! one DE
  regDecomp(:,3) = (/1,1/)    ! one DE
!EOC  
!BOE
! Finally the DistGrid can be created by calling
!EOE
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
    regDecomp=regDecomp, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! The default DE labeling sequence is identical to the patch labeling sequence
! and follows the sequence in which the patches are defined during the create
! call. However, DE labels start at 0 whereas patch labels start at 1. In this 
! case the DE labels look as:
! \begin{verbatim}
!         2
!     0   1
! \end{verbatim}
!
! Each patch can be decomposed differently into DEs. The default DE labeling 
! follows the column major order for each patch. This is demonstrated in the
! following case where the patchwork global domain is decomposed into 9 DEs,
!EOE
!BOC
  regDecomp(:,1) = (/2,2/)    ! 4 DEs
  regDecomp(:,2) = (/1,3/)    ! 3 DEs
  regDecomp(:,3) = (/2,1/)    ! 2 DEs
  
  distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
    regDecomp=regDecomp, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! resulting in the following decomposition:
! \begin{verbatim}
!           +-------+
!           |   7   |
!           |       |
!           |   8   |
!   +-------+-------+
!   | 0   2 |       |
!   |       | 4 5 6 |
!   | 1   3 |       |
!   +-------+-------+
! \end{verbatim}
!
! \begin{verbatim}
!   DE 0 - (11,1)  ~ (15,5)
!   DE 1 - (16,1)  ~ (20,5)
!   DE 2 - (11,6)  ~ (15,10)
!   DE 3 - (16,6)  ~ (20,10)
!   DE 4 - (11,11) ~ (20,14)
!   DE 5 - (11,15) ~ (20,17)
!   DE 6 - (11,18) ~ (20,20)
!   DE 7 - (1,11)  ~ (5,20)
!   DE 8 - (6,11)  ~ (10,20)
! \end{verbatim}
!
! The {\tt decompflag} and {\tt deLabelList} arguments can be used much like
! in the single LR domain case to overwrite the default grid decomposition 
! (per patch) and to change the overall DE labeling sequence, respectively.
!EOE


!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!!!! UNTIL FURTHER IMPLEMENTATION SKIP THE REST OF THE EXAMPLE >>>>>>>>>>>>>>>>>
#ifdef NOSKIP
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


!BOEI
! \subsubsection{2D patchwork DistGrid with explicitly defined connections}
!
! This section will demonstrate how the {\tt connectionList} and 
! {\tt connectionTransformList} arguments are used to define complex index space
! topologies for a patchwork domain.
!
! Starting point is the patchwork domain of the previous section. It 
! consists of 3 LR patches and may be viewed to represent one half of one of 
! the representations of a cubed sphere grid. The sphere, of course, is a
! closed 2D surface. It requires extra information to incorporate this fact into
! the index space representation of the DistGrid. 
!
! Despite the way that the index ranges of the three considered patches 
! have been chosen all DistGrid objects created in the last section 
! have no concept of connections between the patches. In order to form one half of
! the cubed sphere topology it is necessary to close two inner edges (between
! patch 1 and 2 and between 2 and 3) and one outer edge between patch 1 and 3.
!EOEI
!BOCI
  allocate(connectionList(3*2+2,3))  ! (3*dimCount+2, number of connections)
!EOCI
!BOEI
! Setup of the first two connectionList elements is straight forward:
!EOEI
!BOCI
  call ESMF_DistGridConnection(connection=connectionList(:,1), &
     patchIndexA=1, patchIndexB=2, positionVector=(/0, 10/), rc=rc)   ! 1 <-> 2
  call DistGridConnection(connection=connectionList(:,2), &
     patchIndexA=2, patchIndexB=3, positionVector=(/-10, 0/), rc=rc)  ! 2 <-> 3
!EOCI
!BOEI
! The connection between patch 1 and 3 is a bit more involved and reflects the 
! characteristics of the cubed sphere topology:
!EOEI
!BOCI
  call DistGridConnection(connection=connectionList(:,3), &
     patchIndexA=1, patchIndexB=3, positionVector=(/-1, 0/), &
     orientationVector=(/-2, 1/), rc=rc)
!EOCI
!
!BOEI
! Besides topology information the {\tt DistGrid} object must also store
! information about the operations that are necessary when data is transfered
! through patch connections. These transformation rules are in general stagger 
! location dependent and may furthermore depend on the orientation of the 
! transfered data when dealing with vector components. By default it is assumed
! that data passing through a connection does not change stagger location nor 
! sign. For the inner edge connections of the cubed sphere example the default 
! connection transforms are indeed adequate. The outer edge connection, however,
! is more complicated and requires additional information. This information
! must be provided in form of {\tt connectionTransformList} elements, one for 
! each staggering location. Assuming an application with two staggering locations
! that are being transformed into each other when going through the connection
! the connectionTransformList elements for this example are:
!BOCI
  allocate(connectionTransformList(5+2,2))  ! (4+2*dimCount, number of transforms)
  call DistGridConnectionTrans(connectionTransformList(:,1), &
    connectionIndex=3, direction=0, staggerSrc=1, staggerDst=2, &
    indexOffsetVector=(/0,0/), signChangeVector=(/-1,+1/))   ! N face -> E face
  call DistGridConnectionTrans(connectionTransformList(:,2), &
    connectionIndex=3, direction=0, staggerSrc=2, staggerDst=1, &
    indexOffsetVector=(/+1,0/), signChangeVector=(/-1,+1/))  ! E face -> N face
!EOCI
!BOEI
! The first {\tt connectionTransformList} element indicates that staggering 
! location 1 will map to staggering location 2 when going through connection 3,
! which corresponds to a patch 1 to patch 3 interface according to 
! {\tt connectionList}. The element further contains the information that the
! correct element location on the destination patch, i.e. patch 3 in this 
! case, will be at an offset of (0,0) with respect to the element mapping 
! contained in the connection interface. Finally, the {\tt signChangeVector}
! (-1,+1) indicates that vector components that are aligned along a certain
! dimension will undergo a sign change when going through this connection, 
! whereas components along another directions will not experience a sign change.
! The interpretation of the directions of the {\tt signChangeVector} is outside 
! the scope of the {\tt DistGrid} class which provides storage of this 
! information for convenience sake alone. The user or higher classes may 
! interprete and utilize the sign change information.
!
! The information encoded in the second element of the 
! {\tt connectionTransformList} is similar to that of the first element but
! applies for the staggering location 2 to staggering location 1 transition.
! Notice that the staggering location labels are arbitrary and have only 
! meaning as to identify different staggering locations when transformations 
! for actual data objects are computed. It is up to the user or higher classes
! to interpret staggering location labels in terms of physical location in a
! element.
!
! Finally, the {\tt DistGrid} object with the correct index space topology 
! can be created using the {\tt connectionList} and {\tt connectionTransformList} 
! arguments in the following way:
!EOEI
!BOCI
  distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
    regDecomp=regDecomp, connectionList=connectionList, &
    connectionTransformList=connectionTransformList, rc=rc)
!EOCI  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DistGridDestroy(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOCI
  deallocate(connectionList)
  deallocate(connectionTransformList)
!EOCI  


!BOEI
! \subsubsection{Index space topology around DEs}
!  
! The previous sections detailed how a DistGrid can be created by
! specifying a single LR domain or a patchwork of LR domains and
! optional connection information. Once created, an {\tt ESMF\_DistGrid} 
! object contains the complete index space topology. In addition it also 
! stores information about the exclusive DE-local LR regions into which the
! index space has been decomposed.
! 
! Besides the trivial query methods that return DistGrid internal information
! to the user there is a special query call that can be used to obtain 
! index space topology information around a specified DE. The input information
! required by this query call is the DE and stagger location and lower and upper
! displacement vectors for inner and outer rims around the DE-local region.
! The call returns the number of links that connect index blocks in the rim 
! region to exclusive DE-local regions. This information can be used to
! inquire about redundant elements and halo relationships.
!
! The following example shows how index space topology around DE 2 for stagger
! location 1 of the DistGrid defined in the previous section can be 
! determined and explains the format of the input and output information.
!
! A first query call is necessary to determine the number of links that
! connect to elements in the rim defined by the {\tt lVecInner}, {\tt uVecInner},
! {\tt lVecOuter} and {\tt uVecOuter} arguments.
!EOEI
!BOCI
  call ESMF_DistGridGet(distgrid, de=2, staggerLoc=1, &
    lVecInner=(/0,0/), uVecInner=(/0,0/), & ! inner rim edge is same as DE LR box
    lVecOuter=(/-1,-1/), uVecOuter=(/1,1/), & ! outer rim edge is one element wider than DE LR box
    linkCount=linkCount, rc=rc)
!EOCI
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOEI
! The value returned for {\tt linkCount} will be 6 because there are six index
! space blocks in the 1 element wide rim around the DE-local region that connect
! to exclusive DE-local regions. Now the {\tt linkList} argument can be
! allocated and a second query will return the complete information about
! the links:
!EOEI
!BOCI
  allocate(linkList(5*2+2,6)) ! (5*dimCount+2, linkCount)
  call ESMF_DistGridGet(distgrid, de=2, staggerLoc=1, &
    lVecInner=(/0,0/), uVecInner=(/0,0/), & ! inner rim edge is same as DE LR box
    lVecOuter=(/-1,-1/), uVecOuter=(/1,1/), & ! outer rim edge is one element wider than DE LR box
    linkCount=linkCount, linkList=linkList, rc=rc)
!EOCI
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  deallocate(linkList)
!BOEI
! The contents of {\tt linkList} for the above example looks as follows:
! \begin{verbatim}
!  i  |  linkList(:,i)
!  ---+-------------------------------------------------------------------------
!  1  |  (/11,5,  15,5,  0,   1,      11,5,    15,5,  0,1,          1,1/)
!  2  |  (/16,5,  16,5,  1,   1,      16,5,    16,5,  0,1,          1,1/)
!  3  |  (/16,6,  16,10, 3,   1,      16,6,    16,10  0,1,          1,1/)
!  4  |  (/11,11, 16,11, 4,   1,      11,11,   16,11, 0,1,          1,1/)
!  5  |  (/10,6,  10,10, 8,   2,      6,11,    10,11, 1,0,         -1,1/)
!  6  |  (/10,5,  10,5,  7,   2,      5,11,    5,11,  1,0,         -1,1/)
!        (/minC., maxC., pDe, pStLoc, pStrtC., pEndC, pIndexOrder, signChangeVector/)
! \end{verbatim}
!EOEI
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOEI
! \subsubsection{Flat pseudo global index space}
! 
! The example of the previous sections showed for the cubed sphere case how a
! {\tt DistGrid} object for complex index space topologies can be created
! and used. Using connections between patches it is possible to capture a 
! multitude of index space topologies. However, there is a much simpler class of
! index space topologies that can be defined using a global index space. The
! idea is that the specified patch coordinates fall into a flat global index
! space. Connections between patches are thus defined intriniscally and need not
! be specified by explict connection elements.
!
! The following call demonstrates the effects of setting the {\tt indexflag}
! to {\tt ESMF\_INDEX\_GLOBAL} for the previous three patch example, but
! now without explicit connections:
!EOEI
!BOCI
  distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
    regDecomp=regDecomp, indexflag=ESMF_INDEX_GLOBAL, rc=rc)
!EOCI  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOEI
! When the same {\tt ESMF\_DistGridGet()} call of the previous section is
! used on the above defined {\tt DistGrid} object it shows how the global 
! index space option affects the index space topology. The contents of 
! the {\tt linkList} output vector is as follows:
! \begin{verbatim}
!  i  |  linkList(:,i)
!  ---+-------------------------------------------------------------------------
!  1  |  (/11,5,  15,5,  0,   1,      11,5,    15,5,  0,1,          1,1/)
!  2  |  (/16,5,  16,5,  1,   1,      16,5,    16,5,  0,1,          1,1/)
!  3  |  (/16,6,  16,10, 3,   1,      16,6,    16,10  0,1,          1,1/)
!  4  |  (/11,11, 16,11, 4,   1,      11,11,   16,11, 0,1,          1,1/)
!  5  |  (/10,1,  10,1,  8,   1,      10,1,    10,1,  0,1,          1,1/)
!        (/minC., maxC., pDe, pStLoc, pStrtC., pEndC, pIndexOrder, signChangeVector/)
! \end{verbatim}
!
! Even when using the global index space option it may be necessary to 
! specify some explicit connections, e.g. to define periodic boundary 
! conditions. The following calls demonstrate such a pseudo global index space
! topology by imposing periodic boundary conditions along the first dimension 
! of patch 1 for the otherwise unchanged previous example:
!EOEI
!BOCI
  allocate(connectionList(3*2+2, 1))  ! (3*dimCount+2, number of connections)
  call DistGridConnection(connection=connectionList(:,1), &
     patchIndexA=1, patchIndexB=1, positionVector=(/10, 0/), rc=rc)
!EOCI
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOCI
  distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
    regDecomp=regDecomp, indexflag=ESMF_INDEX_GLOBAL, &
    connectionList=connectionList, rc=rc)
!EOCI  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  deallocate(connectionList)
!BOEI
! The result of the previous {\tt ESMF\_DistGridGet()} call then contains two
! more links:
! \begin{verbatim}
!  i  |  linkList(:,i)
!  ---+-------------------------------------------------------------------------
!  1  |  (/11,5,  15,5,  0,   1,      11,5,    15,5,  0,1,          1,1/)
!  2  |  (/16,5,  16,5,  1,   1,      16,5,    16,5,  0,1,          1,1/)
!  3  |  (/16,6,  16,10, 3,   1,      16,6,    16,10  0,1,          1,1/)
!  4  |  (/11,11, 16,11, 4,   1,      11,11,   16,11, 0,1,          1,1/)
!  5  |  (/10,1,  10,1,  8,   1,      10,1,    10,1,  0,1,          1,1/)
!  6  |  (/10,6,  10,10, 3,   1,      20,6,    20,10, 0,1,          1,1/)
!  7  |  (/10,5,  10,5,  1,   1,      20,5,    20,5,  0,1,          1,1/)
!        (/minC., maxC., pDe, pStLoc, pStrtC., pEndC, pIndexOrder, signChangeVector/)
! \end{verbatim}
!EOEI
  
  
  
!BOEI
! \subsubsection{3D DistGrid and shared DELayout}
! 
! The APIs of DistGrid and DELayout are both formulated to be general in
! rank. In this section a 3D DistGrid will be created that represents a 
! single LR domain to show the similarity to the 2D case.
!EOEI
!BOCI
  distgrid = ESMF_DistGridCreate(minIndex=(/-10,1,5/), &
    maxIndex=(/10,10,15/), regDecomp=(/3,2,2/), rc=rc)
!EOCI  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOEI
! This will decompose the global domain into 3 x 2 x 2 = 12 DEs. The default DE
! labeling again follows column major order with respect to the {\tt regDecomp}
! argument but can be overwritten by optionally providing {\tt deLabelList}. The
! implicitly created DELayout will have 12 DEs which are mapped against the
! available PETs following default DELayout rules.
!
! In contrast to DistGrid the DELayout class has no sense of dimensionality
! of a decomposition. In fact,
! the same DELayout can be used for DistGrids of different rank. This can be
! very practical for ensuring data locality when dealing with 2D and 3D domains
! at the same time. Imagine a 2D domain decomposition that describes a data
! distribution that is to interact with the previous 3D decomposition. Further
! assume that the interaction between the associated data is limited to the
! second DE plane of the 3D DistGrid which is perpendicular to the first
! dimension. This DE plane contains 4 DEs and the DE labels are
! (/1, 4, 7, 10/) following the default column major order.
!
! The following lines show the complete example of the 3D and 2D DistGrid
! using a shared DELayout. The DELayout in this example is implicitly created
! during the 3D DistGrid creation and will be deleted when the 3D DistGrid
! is destroyed. The order of lines is critical!
!
!BOCI
  distgrid3D = ESMF_DistGridCreate(minIndex=(/-10,1,5/), &
    maxIndex=(/10,10,15/), regDecomp=(/3,2,2/), rc=rc)
!EOCI
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOCI
  call ESMF_DistGridGet(distgrid3D, delayout=delayout, rc=rc)
!EOCI
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOCI
  distgrid2D = ESMF_DistGridCreate(minIndex=(/1,5/), maxIndex=(/10,15/), &
    regDecomp=(/2,2/), deLabelList=(/1, 4, 7, 10/), delayout=delayout, rc=rc)
!EOCI
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOCI
  call ESMF_DistGridDestroy(distgrid2D, rc=rc)
!EOCI
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOCI
  call ESMF_DistGridDestroy(distgrid3D, rc=rc)
!EOCI
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!
!BOEI
! Using the shared DELayout in the previous example ensures that the data
! associated with {\tt distgrid2D} is distributed the same way as the 
! interacting portion of the data associated with {\tt distgrid3D}. Ensuring
! data locality with shared DELayouts will in most cases be simpler than
! going down to the PET level, constructing DELayouts from petMaps
! and accomplishing the same.
!EOEI


!BOEI
! \subsubsection{Special coordinate queries for regular decompositions}
! 
! Regular decompositions, i.e. decompositions that specify a {\tt regDecomp}
! parameter in the DistGrid creation call, ascribe a special sense of 
! coordination to the DEs within its patch. The following lines 
! demonstrate how this coordinate information can be obtained by querying 
! a DistGrid object.
!EOEI
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOCI
  call ESMF_DistGridGet(distgrid, dimCount=dimCount, &
    regDecompFlag=regDecompFlag, rc=rc)
  if (regDecompFlag == ESMF_TRUE) then
    ! this distgrid holds a regular decompositon
    allocate(regDecompDeCoord(dimCount))
    call ESMF_DistGridGet(distgrid, de, regDecompDeCoord=regDecompDeCoord, &
      rc=rc)
    print *, regDecompDeCoord
    deallocate(regDecompDeCoord)
  else
    ! this distgrid does not hold a regular decomposition
  endif
    
!EOCI  
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

! gjt - remove these for now, it's just confusing...
!BOEI
! \subsubsection{DistGrid with fixed number of DEs}
! 
! Common to all previous examples was the {\tt regDecomp} argument to the
! DistGridCreate call. Although regular decompositions are very common and
! convenient there are cases that cannot be expressed that way. The DistGrid
! class offers a number of alternative ways to specify a domain decomposition
! and to define a DistGrid object. One of the alternative ways is to 
! explicitly specify the number of DEs during the create call.
!EOEI
!BOCI
!  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
!    deCount=6, rc=rc)
!EOCI
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DistGridDestroy(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOEI
! This will decompose the global domain into 6 DEs following a default algorithm
! still to be formulated. The optional arguments {\tt deLabelList} and 
! {\tt connectionList} are available and behave as expected.
!
! The above call can be extended to define a patchwork of domains. The following
! call uses the {\tt minIndex} and {\tt maxIndex} variables defined in a
! previous example.
!EOEI
!BOCI
!  distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
!    deCount=6, rc=rc)
!EOCI
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DistGridDestroy(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOEI
! Again the global domain is decomposed into 6 DEs following an internal 
! algorithm. A special case is when the number of DEs specified equals the 
! number of provided LR domain patches. Then the patches will be used as
! DE-local LR domains. Again it is possible to change the default DE label
! sequence using the optional argument {\tt deLabelList} or to specify special
! connections via the {\tt connectionList} argument.
!EOEI

!BOEI
! \subsubsection{DistGrid with specified DELayout}
! 
! In the previous section the number of DEs was specified at the 
! DistGridCreate interface and a default DELayout with the correct number of
! DEs was created during the call. Alternatively it is possible to specify
! a DELayout at the interface. The number of DEs is then given indirectly by
! the DELayout argument. Hence the result of
!EOEI
!BOCI
!  delayout = ESMF_DELayoutCreate(deCount=6, rc=rc)
!EOCI
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOCI
!  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
!    delayout=delayout, rc=rc)
!EOCI
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DistGridDestroy(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOEI
! is identical to the first example of the previous section.
!
! The extension to the patchwork case is straight forward. Using the
! {\tt delayout} from above and the previously defined {\tt minIndex} 
! and {\tt maxIndex} variables the corresponding example looks like this:
!EOEI
!BOCI 
!  distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
!    delayout=delayout, rc=rc)
!EOCI
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DistGridDestroy(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DELayoutDestroy(delayout, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  deallocate(minIndex, maxIndex)

!BOEI
! \subsubsection{Default DistGrid from DE-local patches}
! 
! The {\tt delayout} argument in the patchwork case of the previous section is
! optional. If it is not specified the number of DEs will be set equal to the
! number of patches provided and each patch LR domain will be associated with a
! unique DE. This provides a very convenient and general way of specifying the 
! DE-local LR domains of a DistGrid object. The following example 
! demonstrates this by creating a DistGrid that contains 3 specific DE
! domains.
!EOEI
!BOCI
!  allocate(minIndex(2,3))    ! (dimCount, number of patches)
!  allocate(maxIndex(2,3))    ! (dimCount, number of patches)
!  minIndex(:,1) = (/1,2/)
!  maxIndex(:,1) = (/5,5/)
!  minIndex(:,2) = (/6,2/)
!  maxIndex(:,2) = (/10,5/)
!  minIndex(:,3) = (/1,6/)
!  maxIndex(:,3) = (/10,8/)
!  distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
!    rc=rc)
!EOCI
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  call ESMF_DistGridDestroy(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!  deallocate(minIndex, maxIndex)
!BOEI
! The resulting global domain decomposition into DEs looks like this:
! \begin{verbatim}
!   --------------> 2nd dimension
!   | +---+---+
!   | | 0 |   |
!   | +---+ 2 |
!   | | 1 |   |
!   | +---+---+
!   |
!   v
!  1st dimension
! \end{verbatim}
!
! With (min) $\sim$ (max) coordinates:
! \begin{verbatim}
!   DE 0 - (1,2) ~ (5,5)
!   DE 1 - (6,2) ~ (10,5)
!   DE 2 - (1,6) ~ (10,8)
! \end{verbatim}
!EOEI


!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#endif
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


10 continue
  call ESMF_Finalize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_DistGridEx.F90"
  else
    print *, "FAIL: ESMF_DistGridEx.F90"
  endif

end program
