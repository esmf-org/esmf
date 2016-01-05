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

program ESMF_DistGridEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  
  implicit none
  
  ! local variables
  integer:: rc, de, i, dim, petCount
!  integer:: nodeCount
  integer:: localPet, localDeCount, localDe
  type(ESMF_VM):: vm
  type(ESMF_DELayout):: delayout
  type(ESMF_DistGrid):: distgrid
  integer, allocatable:: dimExtent(:,:), localIndexList(:)
  integer, allocatable:: minIndex(:,:), maxIndex(:,:), regDecomp(:,:)
  integer, allocatable:: deBlockList(:,:,:)
  type(ESMF_DistGridConnection), allocatable:: connectionList(:)
  integer, allocatable:: localDeToDeMap(:), arbSeqIndexList(:)
  ! result code
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg
  
  
  finalrc = ESMF_SUCCESS

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_DistGridEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_Initialize(vm=vm, defaultlogfilename="DistGridEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (petCount /= 4) then
    finalrc = ESMF_FAILURE
    goto 10
  endif
  
!BOE
! \subsubsection{Single tile DistGrid with regular decomposition}
! 
! The minimum information required to create an {\tt ESMF\_DistGrid} object
! for a single tile with default decomposition are the corners of the tile
! in index space. The following call will create a 1D DistGrid for a 
! 1D index space tile with elements from 1 through 1000.
!EOE

!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/1000/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! A default DELayout with 1 DE per PET will be created during 
! {\tt ESMF\_DistGridCreate()}. The 1000 elements of the specified 1D tile will
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
! specifying only the corners of the tile. The following will create a
! 2D DistGrid for a 5x5 tile with default decomposition.
!EOE

!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

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
! following call creates a DistGrid on the same 2D tile as before, but now with
! a user specified regular decomposition of $2 \times 3 = 6 $ DEs.
!EOE

!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

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
! The specifics of the tile decomposition into DE-local LR domains can be
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
    regDecomp=(/2,3/), decompflag=(/ESMF_DECOMP_BALANCED, &
    ESMF_DECOMP_RESTLAST/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

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
! index space coordinate as given by the associated DistGrid tile for each
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
    indexCountPDe=dimExtent, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DELayoutGet(delayout, localDeCount=localDeCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  allocate(localDeToDeMap(0:localDeCount-1))
  call ESMF_DELayoutGet(delayout, localDeToDeMap=localDeToDeMap, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  do localDe=0, localDeCount-1
    de = localDeToDeMap(localDe)
    do dim=1, 2
      allocate(localIndexList(dimExtent(dim, de))) ! allocate list 
                                                   ! to hold indices
      call ESMF_DistGridGet(distgrid, localDe=localDe, dim=dim, &
        indexList=localIndexList, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      print *, "local DE ", localDe," - DE ",de, &
        " localIndexList along dim=", dim," :: ", localIndexList
      deallocate(localIndexList)
    enddo
  enddo
  deallocate(localDeToDeMap)
  deallocate(dimExtent)
!EOC  
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The advantage of the {\tt localIndexList} format over the min-/max-corner 
! format is that it can be used directly for DE-local to tile index 
! dereferencing. Furthermore the {\tt localIndexList} allows to express very
! general decompositions such as the cyclic decompositions in the first
! dimension generated by the following call:
!EOE

!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), &
    decompflag=(/ESMF_DECOMP_CYCLIC,ESMF_DECOMP_RESTLAST/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

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
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


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
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_DistGridGet(distgrid, delayout=delayout, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_DELayoutPrint(delayout, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

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
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
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
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), delayout=delayout, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
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
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), deLabelList=(/0,3,1,4,2,5/), delayout=delayout, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
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
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), delayout=delayout, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_DistGridGet(distgrid, delayout=delayout, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_DELayoutPrint(delayout, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
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
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOCI
!  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/64,128/), &
!    regDecomp=(/16,32/), delayout=delayout, rc=rc)
!EOCI  
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_DistGridDestroy(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_DELayoutDestroy(delayout, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
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
 ! if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOEI
! contains 8 times as many DEs than there are PETs and groups them into
! {\tt nodeCount} virtual groups that are pinned against VAS. Using this
! DELayout in the decomposition of the global domain according to
!EOEI
!BOCI
!  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/64,128/), &
!    regDecomp=(/nodeCount,32/), delayout=delayout, rc=rc)
!EOCI
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_DistGridDestroy(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_DELayoutDestroy(delayout, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOEI
! ensures that each set of 32 DEs is associated with 4 PETs that form a
! thread group. Work-queue dynamic load balancing within these DE sets can be 
! implemented in user code using the {\tt ESMF\_DELayoutServiceOffer()} and
! {\tt ESMF\_DELayoutServiceComplete()} calls as discussed in the DELayout 
! reference document.
!EOEI


!BOE
! \subsubsection{Single tile DistGrid with decomposition by DE blocks}
! 
! The examples of the previous sections showed how DistGrid objects with
! regular decompositions are created. However, in some cases a regular 
! decomposition may not be specific enough. The following example shows how 
! the {\tt deBlockList} argument is used to create a DistGrid object with 
! completely user-defined decomposition.
!
! A single 5x5 LR domain is to be decomposed into 6 DEs. To this end a list is
! constructed that holds the min and max corners of all six DE
! LR blocks. The DE-local LR blocks are arranged as to cover the whole tile 
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
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! \subsubsection{Single tile DistGrid with periodic boundaries}
! 
! By default the edges of all tiles have solid wall boundary conditions. 
! Periodic boundary conditions can be imposed by specifying connections between
! tiles. For the single LR domain of the last section periodic boundaries 
! along the first dimension are imposed by adding a {\tt connectionList} 
! argument with only one element to the create call.
!EOE
!BOC
  allocate(connectionList(1))
!EOC
!BOE
!
! The connection element holds information about {\tt tileIndex\_A}, 
! {\tt tileIndex\_B}, {\tt positionVector}, and {\tt orientationVector/)}.
!EOE
!BOC
  call ESMF_DistGridConnectionSet(connection=connectionList(1), &
     tileIndexA=1, tileIndexB=1, &
     positionVector=(/5, 0/), &
     orientationVector=(/1, 2/), &
     rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! \begin{sloppypar}
! The {\tt tileIndexA} and {\tt tileIndexB} arguments specify that this is a
! connection within tile 1. The {\tt positionVector} indicates that there is no
! offset between tileB and tileA along the second dimension, but there is
! an offset of 5 along the first dimension (which in this case is the length of
! dimension 1). This aligns tileB (which is tile 1) right next to tileA
! (which is also tile 1).
! \end{sloppypar}
!
! The {\tt orientationVector} fixes the orientation of the tileB index space to
! be the same as the orientation of tileA (it maps index 1 of tileA to index 1
! of tileB and the same for index 2). The {\tt orientationVector} could have
! been omitted in this case which corresponds to the default orientation.
!
! The {\tt connectionList} can now be used to create a {\tt DistGrid} object 
! with the desired boundary conditions.
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    deBlockList=deBlockList, connectionList=connectionList, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_DistGridPrint(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  deallocate(connectionList)
!EOC
  deallocate(deBlockList)
!BOE
! This closes the tile along the first dimension on itself, thus imposing
! periodic boundaries along this direction.
!EOE

!BOE
! \subsubsection{2D multi-tile DistGrid with regular decomposition}
! 
! Creating a DistGrid from a list of LR domains is a straight forward
! extension of the case with a single LR domain. The first four 
! arguments of {\tt ESMF\_DistGridCreate()} are promoted to rank 2, the 
! second dimension being the tile count index.
! 
! The following 2D multi-tile domain consisting of 3 LR tiles will 
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
! The first step in creating a multi-tile global domain is to construct the
! {\tt minIndex} and {\tt maxIndex} arrays.
!EOE
!BOC
  allocate(minIndex(2,3))    ! (dimCount, number of tiles)
  allocate(maxIndex(2,3))    ! (dimCount, number of tiles)
  minIndex(:,1) = (/11,1/)
  maxIndex(:,1) = (/20,10/)
  minIndex(:,2) = (/11,11/)
  maxIndex(:,2) = (/20,20/)
  minIndex(:,3) = (/1,11/)
  maxIndex(:,3) = (/10,20/)
!EOC  
!BOE
! Next the regular decomposition for each tile is set up in the
! {\tt regDecomp} array. In this example each tile is associated with a
! single DE.
!EOE
!BOC
  allocate(regDecomp(2,3))    ! (dimCount, number of tiles)
  regDecomp(:,1) = (/1,1/)    ! one DE
  regDecomp(:,2) = (/1,1/)    ! one DE
  regDecomp(:,3) = (/1,1/)    ! one DE
!EOC  
!BOE
! Finally the DistGrid can be created by calling
!EOE
!BOC
  distgrid = ESMF_DistGridCreate(minIndexPTile=minIndex, &
    maxIndexPTile=maxIndex, regDecompPTile=regDecomp, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The default DE labeling sequence is identical to the tile labeling sequence
! and follows the sequence in which the tiles are defined during the create
! call. However, DE labels start at 0 whereas tile labels start at 1. In this 
! case the DE labels look as:
! \begin{verbatim}
!         2
!     0   1
! \end{verbatim}
!
! Each tile can be decomposed differently into DEs. The default DE labeling 
! follows the column major order for each tile. This is demonstrated in the
! following case where the multi-tile global domain is decomposed into 9 DEs,
!EOE
!BOC
  regDecomp(:,1) = (/2,2/)    ! 4 DEs
  regDecomp(:,2) = (/1,3/)    ! 3 DEs
  regDecomp(:,3) = (/2,1/)    ! 2 DEs
  
  distgrid = ESMF_DistGridCreate(minIndexPTile=minIndex, &
    maxIndexPTile=maxIndex, regDecompPTile=regDecomp, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
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
! (per tile) and to change the overall DE labeling sequence, respectively.
!EOE

!BOE
! \subsubsection{Arbitrary DistGrids with user-supplied sequence indices}
! \label{DistGrid:ArbitrarySeqInd}
!
! The DistGrid class supports the communication methods of higher classes, 
! like Array and Field, by associating a unique {\em sequence index} with each
! DistGrid index tuple. This sequence index can be used to address every Array
! or Field element. By default, the DistGrid does not actually generate and
! store the sequence index of each element. Instead a default sequence through
! the elements is implemented in the DistGrid code. This default sequence 
! is used internally when needed.
!
! The DistGrid class provides two {\tt ESMF\_DistGridCreate()} calls that 
! allow the user to specify arbitrary sequence indices, overriding the use of
! the default sequence index scheme. The user sequence indices are passed to
! the DistGrid in form of 1d Fortran arrays, one array on each PET. The local
! size of this array on each PET determines the number of DistGrid elements on
! the PET. The supplied sequence indices must be unique across all PETs. 
!
!EOE

!BOC
  allocate(arbSeqIndexList(10))   ! each PET will have 10 elements
  
  do i=1, 10
    arbSeqIndexList(i) = (i-1)*petCount + localPet ! initialize unique 
                                                   ! seq. indices
  enddo
!EOC
  
!BOE
! A default DELayout will be created automatically during 
! {\tt ESMF\_DistGridCreate()}, associating 1 DE per PET.
!EOE

!BOC
  distgrid = ESMF_DistGridCreate(arbSeqIndexList=arbSeqIndexList, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The user provided sequence index array can be deallocated once it has
! been used.
!EOE

!BOC
  deallocate(arbSeqIndexList)
!EOC

!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The {\tt distgrid} object can be used just like any other DistGrid object.
! The "arbitrary" nature of {\tt distgrid} will only become visible during
! Array or Field communication methods, where source and destination objects
! map elements according to the sequence indices provided by the associated
! DistGrid objects.
!EOE

!BOC
  call ESMF_DistGridDestroy(distgrid, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The second {\tt ESMF\_DistGridCreate()} call, that accepts the 
! {\tt arbSeqIndexList} argument, allows the user to specify additional,
! regular DistGrid dimensions. These additional DistGrid dimensions are not
! decomposed across DEs, but instead are simply "added" or "multiplied" to the
! 1D arbitrary dimension.
!
! The same {\tt arbSeqIndexList} array as before is used to define the 
! user supplied sequence indices.
!EOE

!BOC
  allocate(arbSeqIndexList(10))   ! each PET will have 10 elements
  
  do i=1, 10
    arbSeqIndexList(i) = (i-1)*petCount + localPet  ! initialize unique 
                                                    ! seq. indices
  enddo
!EOC

!BOE
! The additional DistGrid dimensions are specified in the usual manner using
! {\tt minIndex} and {\tt maxIndex} arguments. The {\tt dimCount} of the
! resulting DistGrid is the size of the {\tt minIndex} and {\tt maxIndex}
! arguments plus 1 for the arbitrary dimension. The {\tt arbDim} argument is
! used to indicate which or the resulting DistGrid dimensions
! is associated with the arbitrary sequence indices provided by the user.
!EOE

!BOC
  distgrid = ESMF_DistGridCreate(arbSeqIndexList=arbSeqIndexList, &
    arbDim=1, minIndexPTile=(/1,1/), maxIndexPTile=(/5,7/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  deallocate(arbSeqIndexList)
!EOC
!  call ESMF_DistGridPrint(distgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
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
    print *, "PASS: ESMF_DistGridEx.F90"
  else
    print *, "FAIL: ESMF_DistGridEx.F90"
  endif

end program
