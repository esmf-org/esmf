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

program ESMF_RHandleBitForBitEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  
  implicit none
  
  ! local variables
  integer               :: rc, i, iounit, iostat
  integer               :: petCount, localPet
  type(ESMF_VM)         :: vm
  type(ESMF_DistGrid)   :: distgrid
  type(ESMF_Array)      :: srcArray, dstArray
  integer, allocatable  :: indexList(:)
  real(ESMF_KIND_R4), pointer:: farrayPtr(:)
  real(ESMF_KIND_R4)    :: sumA, sumB, sumC, sumD, sumE, sumCompare
  
  integer               :: smmElementCount
  integer, allocatable  :: factorIndexList(:,:)
  real(ESMF_KIND_R4), allocatable:: factorList(:)
  type(ESMF_RouteHandle):: rh
  
  integer               :: srcTermProcessing, pipelineDepth
  character(len=128)    :: msg

  ! result code
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg
  
  
  finalrc = ESMF_SUCCESS

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_RHandleBitForBitEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_Initialize(vm=vm, defaultlogfilename="RHandleBitForBitEx.Log", &
    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (petCount /= 4) then
    finalrc = ESMF_FAILURE
    goto 10
  endif
  
!BOE
! \subsubsection{Bit-for-bit reproducibility}
! \label{RH:bfb}
!
! Bit-for-bit (bfb) reproducibility is at the core of the regression testing
! schemes of many scientific model codes. The bfb requirement makes it possible
! to easily compare the numerical results of simulation runs using standard
! binary diff tools.
!
! While bfb reproducibility is desirable (and often required) for regression
! testing, it does limit the available performance optimization
! opportunities. Especially in highly parallelized code, best performance is 
! often achieved by allowing operations to occur in a flexible order. Under
! some conditions, however, a change in the order of numerical operations
! leads to small numerical differences in the results, breaking bfb
! reproducibility.
!
! ESMF provides the following three levels of bfb reproducibility 
! support, with the associated performance optimization implications:
!
! \begin{itemize}
!
! \item Strict bit-for-bit reproducibility: Results are guaranteed to be 
! bit-for-bit identical even when executing across different numbers of PETs. 
! The optimization options are limited to memory layout and message aggregation.
!
! \item Relaxed bit-for-bit reproducibility: Results are only guaranteed to be
! bit-for-bit identical when running across an unchanged number of PETs. The 
! optimization options include partial sums, allowing computational load to 
! be balanced between source and destination PETs, and message sizes to be 
! reduced.
!
! \item No guarantee for bit-for-bit reproducibility: Results may differ by 
! numerical round-off. The optimization options include dynamic out-of-order
! summation of partial sums.
!
! \end{itemize}
!
! The following discussion uses very simple numerical examples to demonstrate
! how the order of terms in a sum can lead to results that are not
! bit-for-bit identical. The examples use single precision,
! {\tt ESMF\_KIND\_R4} numbers, but the concepts apply the same
! to double precision, {\tt ESMF\_KIND\_R8}; only that the decimals, for
! which bfb differences in the sums occur, are different ones.
!
! With {\tt sumA}, {\tt sumB}, {\tt sumC}, {\tt sumD}, and {\tt sumE} all of
! type {\tt real(ESMF\_KIND\_R4)}, one finds the following bfb differences:
!EOE

!BOC
  sumA = (0.5 + 0.1) + 0.1        ! results in 0.700000048
  sumB = 0.5 + (0.1 + 0.1)        ! results in 0.699999988
  
  sumC = 0.5 +  0.2 + 0.1  + 0.1  ! results in 0.900000036
  sumD = 0.5 + (0.2 + 0.1) + 0.1  ! results in 0.900000036
  sumE = 0.5 + (0.2 + 0.1 + 0.1)  ! results in 0.899999976
!EOC

  if (localPet == 0) then
    print *, "sumA = ", sumA
    print *, "sumB = ", sumB
    print *, "sumC = ", sumC
    print *, "sumD = ", sumD
    print *, "sumE = ", sumE
  endif

!BOE
! These differences result from the fact that many decimals (even very simple
! ones like 0.1 or 0.2) lead to periodic binary floating point numbers.
! Periodic floating point numbers must be truncated when represented by a
! finite number of bits, leading to small rounding errors. Further truncation
! occurs when the radix point of two numbers must be aligned during
! floating point arithmetic, resulting in bit shifts for one of the
! numbers. The resulting truncation error depends on the precise numbers that
! need alignment. As a result, executing the "same" sum in a different order
! can lead to different truncation steps and consequently in results that are
! not bit-for-bit identical.
!
! In order to help users with the implementation of their bfb requirement, 
! ESMF provides different levels of control over the term order in sparse 
! matrix multiplications, while at the same time offering performance 
! optimization options. In all there are {\em three} arguments that will be
! introduced in the following paragraphs: {\tt srcTermProcessing}, 
! {\tt termorderflag}, and {\tt pipelineDepth}.
!
! For the purpose of demonstration, a one-dimensional, arbitrarily distributed 
! source Array is constructed. There are three Array elements on each of the
! four PETs. Their local storage indices, sequence indices, and data values
! are as follows:
!
! \begin{verbatim}
!
!         +-----+-------+----------------+------------+
!         | PET | index | sequence index | data value |
!         +-----+-------+----------------+------------+
!         |  0  |   1   |          1     |     0.5    |
!         |  0  |   2   |          6     |     0.1    |
!         |  0  |   3   |          9     |     0.1    |
!         +-----+-------+----------------+------------+
!         |  1  |   1   |          4     |     0.5    |
!         |  1  |   2   |          3     |     0.1    |
!         |  1  |   3   |         10     |     0.1    |
!         +-----+-------+----------------+------------+
!         |  2  |   1   |         11     |     0.5    |
!         |  2  |   2   |          7     |     0.1    |
!         |  2  |   3   |          5     |     0.1    |
!         +-----+-------+----------------+------------+
!         |  3  |   1   |          8     |     0.1    |
!         |  3  |   2   |          2     |     0.2    |
!         |  3  |   3   |         12     |     0.1    |
!         +-----+-------+----------------+------------+
!
! \end{verbatim}
!EOE
  
  ! -- srcArray --
  allocate(indexList(3))
  if (localPet == 0) then
    indexList(1) = 1
    indexList(2) = 6
    indexList(3) = 9
  elseif (localPet == 1) then
    indexList(1) = 4
    indexList(2) = 3
    indexList(3) = 10
  elseif (localPet == 2) then
    indexList(1) = 11
    indexList(2) = 7
    indexList(3) = 5
  elseif (localPet == 3) then
    indexList(1) = 8
    indexList(2) = 2
    indexList(3) = 12
  endif
  
  distgrid = ESMF_DistGridCreate(indexList, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  srcArray = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R4, &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_ArrayGet(srcArray, farrayPtr=farrayPtr, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  do i=1, 3
    select case (indexList(i))
    case (1)
      farrayPtr(i) = 0.5_ESMF_KIND_R4
    case (2)
      farrayPtr(i) = 0.2_ESMF_KIND_R4
    case (3)
      farrayPtr(i) = 0.1_ESMF_KIND_R4
    case (4)
      farrayPtr(i) = 0.5_ESMF_KIND_R4
    case (5)
      farrayPtr(i) = 0.1_ESMF_KIND_R4
    case (6)
      farrayPtr(i) = 0.1_ESMF_KIND_R4    
    case (7)
      farrayPtr(i) = 0.1_ESMF_KIND_R4    
    case (8)
      farrayPtr(i) = 0.1_ESMF_KIND_R4    
    case (9)
      farrayPtr(i) = 0.1_ESMF_KIND_R4    
    case (10)
      farrayPtr(i) = 0.1_ESMF_KIND_R4    
    case (11)
      farrayPtr(i) = 0.5_ESMF_KIND_R4    
    case (12)
      farrayPtr(i) = 0.1_ESMF_KIND_R4    
    end select
  enddo
  
  deallocate(indexList)

!  call ESMF_ArrayPrint(srcArray)

  ! ---------------------------------------------------------------------------

!BOE
! The destination Array consists of only a single element, located on PET 0:
!
! \begin{verbatim}
!
!         +-----+-------+----------------+------------+
!         | PET | index | sequence index | data value |
!         +-----+-------+----------------+------------+
!         |  0  |   1   |          1     |     n/a    |
!         +-----+-------+----------------+------------+
!
! \end{verbatim}
!EOE
 
  ! -- dstArray --

  if (localPet == 0) then
    allocate(indexList(1))
    indexList(1) = 1
  else
    allocate(indexList(0))
  endif
  
  distgrid = ESMF_DistGridCreate(indexList, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  deallocate(indexList)

  dstArray = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R4, &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  call ESMF_ArrayGet(dstArray, farrayPtr=farrayPtr, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! ---------------------------------------------------------------------------

  ! -- sparse matrix --    
  smmElementCount = 0
  if (localPet == 0) then
    smmElementCount = 3
  endif
  allocate(factorIndexList(2,smmElementCount), factorList(smmElementCount))
  
  ! ---------------------------------------------------------------------------

!BOE
! As a first example consider the following sparse matrix with three entries:
!EOE

  if (localPet == 0) then
    !
    ! Place all src terms on the same PET demonstrating that the src term order
    ! is then defined by SRCSEQ.
    !
    ! -> factors into dst element 1 on PET 0
    ! * sum with 3 addends (order by src seq. index): 0.5 + 0.1 + 0.1
    ! * addend 1 is on PET 0
    ! * addend 2 is on PET 0
    ! * addend 3 is on PET 0
    ! All three addends are on the same PET, therefore src seq index order,
    ! independent of the order in which they are stored in memory on the src Pet
    ! Also order in which sparse matrix elements are given is irrelevant.
    ! (1,1)*s[1]Pet0 + (6,1)*s[6]Pet0 + (9,1)*s[9]Pet0  = 
    !       0.5      +       0.1      +       0.1
!BOC
    factorIndexList(1,1) = 1  ! src seq index
    factorIndexList(2,1) = 1  ! dst seq index
    factorList(1) = 1.
    factorIndexList(1,2) = 6  ! src seq index
    factorIndexList(2,2) = 1  ! dst seq index
    factorList(2) = 1.
    factorIndexList(1,3) = 9  ! src seq index
    factorIndexList(2,3) = 1  ! dst seq index
    factorList(3) = 1.
!EOC
  endif
  
!BOE
! In ESMF, the order in which the sparse matrix entries are specified in 
! {\tt factorIndexList} and {\tt factorList}, or on which PET they
! are provided, is completely irrelevant. The term order in the resulting
! sparse matrix sums is not affected by it.
!
! There is one aspect of the sparse matrix format, however, that is relevant
! to the bfb considerations: When multiple entries for the same (src, dst)
! pair are present in a sparse matrix definition, the entries are summed
! into a single (src, dst) entry. Therefore, even if there are multiple
! sparse matrix entries for the same (src, dst) pair, there will only be a
! single term for it in the resulting expression.
!
! Going back to the three term sparse matrix definition above, the 
! {\em canonical} term order is defined by the source sequence indices in 
! ascending order. With {\tt (src,dst)} denoting the sparse matrix factors,
! and {\tt s(src)} and {\tt d(dst)} denoting source and destination Array
! elements, respectively, for {\tt src} and {\tt dst} sequence indices, the
! sum in canonical order is:
!
!     d(1) = (1,1)*s(1) + (6,1)*s(6) + (9,1)*s(9)
!
! For simplicity, the factors in all of the examples are set to {\tt 1.0}, allowing us
! to drop them in the expressions. This helps focus on the critical issue -- 
! term order:
!
!     d(1) = s(1) + s(6) + s(9)
!
! \begin{sloppypar}
! There are two parameters that affect term order in the ESMF sparse matrix
! multiplication (SMM), and therefore must be considered in the context of bfb
! reproducibility. First there is the {\tt srcTermProcessing} parameter which
! controls grouping of source terms located on the same PET. The value of the
! {\tt srcTermProcessing} parameter indicates the maximum number of terms that
! may be grouped into partial sums on the source PET. Setting
! {\tt srcTermProcessing} to 1 means that no partial sums are formed on the 
! source side, however, the source terms are multiplied with their
! respective sparse matrix factor before being sent to the destination PET. 
! Setting {\tt srcTermProcessing} to 0 prevents these products from being carried
! out on the source side, and the source Array elements are sent unmodified.
! Depending on the distribution of the source Array, values greater than 1
! for {\tt srcTermProcessing} can lead to partial sums and thus may have
! impact on the bfb reproducibility of the SMM.
! \end{sloppypar}
!
! The second parameter that may have bfb effects comes into play at 
! execution-time of a precomputed 
! RouteHandle. It is accessible via the {\tt termorderflag} argument; a typed 
! flag with the following values:
! \begin{itemize}
!   \item {\tt ESMF\_TERMORDER\_SRCSEQ} -- Strictly enforces the canonical order
!      of the source terms according to the source sequence index. However, 
!      terms that are grouped together in the RouteHandle at store-time, as a 
!      consequence of {\tt srcTermProcessing}, are treated as
!      single entities with a sequence index equal to the lowest original
!      sequence index in the group. Use {\tt ESMF\_TERMORDER\_SRCSEQ} together
!      with {\tt srcTermProcessing=0} or {\tt srcTermProcessing=1} when strict
!      bfb reproducibility is required independent of the source Array 
!      distribution, e.g. for different number of PETs.
!   \item {\tt ESMF\_TERMORDER\_SRCPET} -- The source terms in the sum are 
!      first arranged according to the relative position of the PET on which 
!      they reside with respect to the destination PET. Second, all the terms
!      coming from the same PET are sorted in canonical sequence index order
!      and summed into partial sums. Again, terms that are grouped together
!      in the RouteHandle at store-time are treated as
!      single entities with a sequence index equal to the lowest original
!      sequence index in the group. The final result for each destination
!      element is determined by adding the partial sums in an order that is 
!      fixed by the position of the partial sums' source PETs relative to
!      the destination PET. This ensures bfb reproducibility of the result as
!      long as the number of PETs remains unchanged.
!   \item {\tt ESMF\_TERMORDER\_FREE} -- For this option there are no
!      restrictions on the term
!      order. Terms can be summed in any order, and the order may change each
!      time the RouteHandle is executed. This option grants greatest flexibility
!      to the RouteHandle execution implementation. It is available for all the
!      methods that take the {\tt termorderflag} argument. Without a
!      guaranteed source term order, the {\tt ESMF\_TERMORDER\_FREE} option is
!      not suitable for situations that require bfb reproducibility. 
! \end{itemize}
!EOE

!BOE
! {\bf ESMF\_TERMORDER\_SRCSEQ}
! 
! First using {\tt srcTermProcessing=0} at store time and
! {\tt termorderflag=ESMF\_TERMORDER\_SRCSEQ} at execution time,
! the canonical term order is expected:
!
!     d(1) = s(1) + s(6) + s(9) = 0.5 + 0.1 + 0.1 = sumA
!
!EOE

!BOC
  ! forced srcTermProcessing
  srcTermProcessing = 0
  
  call ESMF_ArraySMMStore(srcArray, dstArray, &
    factorIndexList=factorIndexList, factorList=factorList, &
    routehandle=rh, srcTermProcessing=srcTermProcessing, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_ArraySMM(srcArray, dstArray, routehandle=rh, &
    termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
!EOC    
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  if (localPet == 0) then
    print *, "result SRCSEQ#1 = ", farrayPtr(1), " expect: ", sumA
    if (farrayPtr(1) /= sumA) &
      finalrc = ESMF_FAILURE
  endif
!EOC

  call ESMF_RouteHandlePrint(rh, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArraySMMRelease(rh, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The order of source terms across PETs is expected to have no effect on the
! bfb reproducibility of the result for {\tt ESMF\_TERMORDER\_SRCSEQ}. To test
! this, a sparse matrix is used where the source terms originate from different
! PETs.
!EOE

  if (localPet == 0) then
    !
    ! Place the src terms across PETs so that SRCPET order results
    ! in a different order than SRCSEQ - and resulting numerical differences.
    !
    ! -> factors into dst element 1 on PET 0
    ! * sum with 3 addends (order by src seq. index): 0.5 + 0.1 + 0.1
    ! * addend 1 is on PET 1
    ! * addend 2 is on PET 2
    ! * addend 3 is on PET 3
    ! The "SRCPET" order of the three addends is given by the cyclic scheme
    ! src PET scheme, starting at dstPet, going to dstPet-petCount, modulo
    ! petCount. With that the sum order is this:
    ! (12,1)*s[12]Pet3 + (5,1)*s[5]Pet2 + (4,1)*s[4]Pet1  = 
    !       0.1        +       0.1      +       0.5      
!BOC 
    factorIndexList(1,1) = 4  ! src seq index
    factorIndexList(2,1) = 1  ! dst seq index
    factorList(1) = 1.
    factorIndexList(1,2) = 5  ! src seq index
    factorIndexList(2,2) = 1  ! dst seq index
    factorList(2) = 1.
    factorIndexList(1,3) = 12 ! src seq index
    factorIndexList(2,3) = 1  ! dst seq index
    factorList(3) = 1.
!EOC
  endif
  
!BOE
! Again the {\tt srcTermProcessing} argument is kept at 0, ensuring that none
! of the source terms are grouped into partial sums.
!
!EOE

!BOC
  ! forced srcTermProcessing
  srcTermProcessing = 0
  
  call ESMF_ArraySMMStore(srcArray, dstArray, &
    factorIndexList=factorIndexList, factorList=factorList, &
    routehandle=rh, srcTermProcessing=srcTermProcessing, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_ArraySMM(srcArray, dstArray, routehandle=rh, &
    termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
!EOC    
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Under {\tt ESMF\_TERMORDER\_SRCSEQ} it does not matter on which PET a
! source term is located, the order of source terms is strictly defined by the
! order of source sequence indices:
!
!     d(1) = s(4) + s(5) + s(12) = 0.5 + 0.1 + 0.1 = sumA
!
!EOE
!BOC
  if (localPet == 0) then
    print *, "result SRCSEQ#2 = ", farrayPtr(1), " expect: ", sumA
    if (farrayPtr(1) /= sumA) &
      finalrc = ESMF_FAILURE
  endif
!EOC
  call ESMF_ArraySMMRelease(rh, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOE
! The same sparse matrix leads to bfb differences in the result when executed
! with the {\tt ESMF\_TERMORDER\_SRCPET} option. This is demonstrated further
! down in result {\tt SRCPET\#4}.
!EOE


!BOE
! {\bf ESMF\_TERMORDER\_SRCPET}
!
! {\bf All source terms coming from the same PET}
!
! In the following examples the {\tt srcTermProcessing} argument at store-time
! is first set to 0, forcing all of the source terms to be sent to the
! destination PET unmodified. We start by going back to the initial sparse
! matrix where all of the source terms are located on the same PET.
!EOE

  if (localPet == 0) then
    !
    ! Place all src terms on the same PET demonstrating that the src term order
    ! is then defined by SRCSEQ.
    !
    ! -> factors into dst element 1 on PET 0
    ! * sum with 3 addends (order by src seq. index): 0.5 + 0.1 + 0.1
    ! * addend 1 is on PET 0
    ! * addend 2 is on PET 0
    ! * addend 3 is on PET 0
    ! All three addends are on the same PET, therefore src seq index order,
    ! independent of the order in which they are stored in memory on the src Pet
    ! Also order in which sparse matrix elements are given is irrelevant.
    ! (1,1)*s[1]Pet0 + (6,1)*s[6]Pet0 + (9,1)*s[9]Pet0  = 
    !       0.5      +       0.1      +       0.1
!BOC
    factorIndexList(1,1) = 1  ! src seq index
    factorIndexList(2,1) = 1  ! dst seq index
    factorList(1) = 1.
    factorIndexList(1,2) = 6  ! src seq index
    factorIndexList(2,2) = 1  ! dst seq index
    factorList(2) = 1.
    factorIndexList(1,3) = 9  ! src seq index
    factorIndexList(2,3) = 1  ! dst seq index
    factorList(3) = 1.
!EOC
  endif

!BOC
  ! forced srcTermProcessing
  srcTermProcessing=0
  
  call ESMF_ArraySMMStore(srcArray, dstArray, &
    factorIndexList=factorIndexList, factorList=factorList, &
    routehandle=rh, srcTermProcessing=srcTermProcessing, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOE
! Then, at execution time, the {\tt ESMF\_TERMORDER\_SRCPET} option is used.
!EOE

!BOC
  call ESMF_ArraySMM(srcArray, dstArray, routehandle=rh, &
    termorderflag=ESMF_TERMORDER_SRCPET, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOE
! Here all of the source elements originate from the same PET (PET 0). This
! fact, together with the {\tt ESMF\_TERMORDER\_SRCPET} execution-time option,
! results in the following canonical term order:
!
!     d(1) = s(1) + s(6) + s(9) = 0.5 + 0.1 + 0.1 = sumA
!
! This is exactly the same term order that was used above to produce the
! result stored in {\tt sumA}.
!EOE

!BOC
  if (localPet == 0) then
    print *, "result SRCPET#1 = ", farrayPtr(1), " expect: ", sumA
    if (farrayPtr(1) /= sumA) &
      finalrc = ESMF_FAILURE
  endif
!EOC

  call ESMF_ArraySMMRelease(rh, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! ---------------------------------------------------------------------------

!BOE
! The sequence indices of the source terms are the only relevant aspect in 
! determining the source term order. Consider, for example, the following 
! sparse matrix, where again all source terms are located on the same PET 
! (PET 2):
!EOE

  if (localPet == 0) then
    !
    ! Place all src terms on the same PET demonstrating that the src term order
    ! is then defined by SRCSEQ.
    !
    ! -> factors into dst element 1 on PET 0
    ! * sum with 3 addends (order by src seq. index): 0.1 + 0.1 + 0.5
    ! * addend 1 is on PET 2
    ! * addend 2 is on PET 2
    ! * addend 3 is on PET 2
    ! All three addends are on the same PET, therefore src seq index order,
    ! independent of the order in which they are stored in memory on the src Pet
    ! Also order in which sparse matrix elements are given is irrelevant.
    ! (5,1)*s[5]Pet2 + (7,1)*s[7]Pet2 + (11,1)*s[11]Pet2  = 
    !       0.1      +       0.1      +        0.5
!BOC
    factorIndexList(1,1) = 11 ! src seq index
    factorIndexList(2,1) = 1  ! dst seq index
    factorList(1) = 1.
    factorIndexList(1,2) = 5  ! src seq index
    factorIndexList(2,2) = 1  ! dst seq index
    factorList(2) = 1.
    factorIndexList(1,3) = 7  ! src seq index
    factorIndexList(2,3) = 1  ! dst seq index
    factorList(3) = 1.
!EOC
  endif
  
!BOE
! This time the source term order in memory is not the same
! as their sequence index order. Specifically, the sequence indices of the
! source terms, in the order they are stored in memory, is 11, 7, 5 (see the
! source Array diagram above for reference). 
! Further, as mentioned already, the order of entries in the sparse matrix
! also have not bearing on the term order in the SMM sums.
! Then, for the {\tt ESMF\_TERMORDER\_SRCPET} option, and because all source
! terms are located on the same PET, the resulting source term order is the 
! canonical one determined by the source term sequence indices alone:
!
!     d(1) = s(5) + s(7) + s(11)
!
! Filling in the source element data, we find
! 
!     d(1) = 0.1 + 0.1 + 0.5,
!
! which is expected to be bfb equivalent to the result stored in {\tt sumB}
! from above.
!EOE

!BOC
  ! forced srcTermProcessing
  srcTermProcessing=0
  
  call ESMF_ArraySMMStore(srcArray, dstArray, &
    factorIndexList=factorIndexList, factorList=factorList, &
    routehandle=rh, srcTermProcessing=srcTermProcessing, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOC
  call ESMF_ArraySMM(srcArray, dstArray, routehandle=rh, &
    termorderflag=ESMF_TERMORDER_SRCPET, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOC
  if (localPet == 0) then
    print *, "result SRCPET#2 = ", farrayPtr(1), " expect: ", sumB
    if (farrayPtr(1) /= sumB) &
      finalrc = ESMF_FAILURE
  endif
!EOC

  call ESMF_ArraySMMRelease(rh, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! ---------------------------------------------------------------------------

!BOE
! {\bf Source terms coming from different PETs}
!
! When the source terms are distributed across multiple PETs, the 
! {\tt ESMF\_TERMORDER\_SRCPET} option first bundles the terms according to
! the PET on which they are stored. These source term "bundles" are then 
! arranged in an order that depends on the source PET position relative to the
! destination PET: starting with the bundle for which the source PET is the
! same as the destination PET, the source term bundles are placed in descending
! order with respect to their source PET, modulo petCount. The terms within
! each source term bundle are further sorted in the canonical order according
! to their sequence index.
!
! The following sparse matrix demonstrates the effect of the
! {\tt ESMF\_TERMORDER\_SRCPET} option.
!EOE

  if (localPet == 0) then
    !
    ! Placing src terms on PETs so that SRCSEQ and SRCPET order are identical.
    !
    ! -> factors into dst element 1 on PET 0
    ! * sum with 3 addends (order by src seq. index): 0.5 + 0.1 + 0.1
    ! * addend 1 is on PET 0
    ! * addend 2 is on PET 1
    ! * addend 3 is on PET 2
    ! The "SRCPET" order of the three addends is given by the cyclic scheme
    ! src PET scheme, starting at dstPet, going to dstPet-petCount, modulo
    ! petCount. With that the sum order is this:
    ! (1,1)*s[1]Pet0 + (7,1)*s[7]Pet2 + (3,1)*s[3]Pet1  = 
    !       0.5      +       0.1      +       0.1       
!BOC
    factorIndexList(1,1) = 1  ! src seq index
    factorIndexList(2,1) = 1  ! dst seq index
    factorList(1) = 1.
    factorIndexList(1,2) = 3  ! src seq index
    factorIndexList(2,2) = 1  ! dst seq index
    factorList(2) = 1.
    factorIndexList(1,3) = 7  ! src seq index
    factorIndexList(2,3) = 1  ! dst seq index
    factorList(3) = 1.
!EOC
  endif
  
!BOE
! Here the source terms are located on PETs 0, 1, and 2. Using a [] notion to
! indicate the source PET of each term, the term order under 
! {\tt ESMF\_TERMORDER\_SRCPET} is given by:
!
!     d(1) = s(1)[0] + s(7)[2] + s(3)[1] = 0.5 + 0.1 + 0.1
!
! This is again the same order of terms that was used to produce the result 
! stored in {\tt sumA} above.
!EOE

!BOC
  ! forced srcTermProcessing
  srcTermProcessing=0
    
  call ESMF_ArraySMMStore(srcArray, dstArray, &
    factorIndexList=factorIndexList, factorList=factorList, &
    routehandle=rh, srcTermProcessing=srcTermProcessing, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOC
  call ESMF_ArraySMM(srcArray, dstArray, routehandle=rh, &
    termorderflag=ESMF_TERMORDER_SRCPET, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOC
  if (localPet == 0) then
    print *, "result SRCPET#3 = ", farrayPtr(1), " expect: ", sumA
    if (farrayPtr(1) /= sumA) &
      finalrc = ESMF_FAILURE
  endif
!EOC
  
  call ESMF_ArraySMMRelease(rh, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! ---------------------------------------------------------------------------

!BOE
! In the above example, the fact that the terms were ordered by source PET
! first, did not lead to numerical bfb differences compared to the canonical 
! source term order. However, this was purely coincidental in the way the
! numbers worked out for this example. The following case looks at a situation
! where the source PET order {\em does} lead to a result that shows bfb
! differences compared to the canonical term order.
!EOE

  if (localPet == 0) then
    !
    ! This time place the src terms across PETs so that SRCPET order results
    ! in a different order than SRCSEQ - and resulting numerical differences.
    !
    ! -> factors into dst element 1 on PET 0
    ! * sum with 3 addends (order by src seq. index): 0.5 + 0.1 + 0.1
    ! * addend 1 is on PET 1
    ! * addend 2 is on PET 2
    ! * addend 3 is on PET 3
    ! The "SRCPET" order of the three addends is given by the cyclic scheme
    ! src PET scheme, starting at dstPet, going to dstPet-petCount, modulo
    ! petCount. With that the sum order is this:
    ! (12,1)*s[12]Pet3 + (5,1)*s[5]Pet2 + (4,1)*s[4]Pet1  = 
    !       0.1        +       0.1      +       0.5      
!BOC 
    factorIndexList(1,1) = 4  ! src seq index
    factorIndexList(2,1) = 1  ! dst seq index
    factorList(1) = 1.
    factorIndexList(1,2) = 5  ! src seq index
    factorIndexList(2,2) = 1  ! dst seq index
    factorList(2) = 1.
    factorIndexList(1,3) = 12 ! src seq index
    factorIndexList(2,3) = 1  ! dst seq index
    factorList(3) = 1.
!EOC
  endif
  
!BOE
! The canonical source term order of this SMM sum, determined by the source
! sequence indices alone, is:
!
!     d(1) = s(4) + s(5) + s(12) = 0.5 + 0.1 + 0.1,
!
! which again would lead to a result that is bfb identical to {\tt sumA}. 
! However, this is not the term order resulting from the
! {\tt ESMF\_TERMORDER\_SRCPET} option. The actual order for this option is:
!
!     d(1) = s(12)[3] + s(5)[2] + s(4)[1] = 0.1 + 0.1 + 0.5,
!
! resulting in a sum that is bfb identical to {\tt sumB} instead.
!EOE
  
!BOC
  ! forced srcTermProcessing
  srcTermProcessing=0
  
  call ESMF_ArraySMMStore(srcArray, dstArray, &
    factorIndexList=factorIndexList, factorList=factorList, &
    routehandle=rh, srcTermProcessing=srcTermProcessing, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOC
  call ESMF_ArraySMM(srcArray, dstArray, routehandle=rh, &
    termorderflag=ESMF_TERMORDER_SRCPET, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOC
  if (localPet == 0) then
    print *, "result SRCPET#4 = ", farrayPtr(1), " expect: ", sumB
    if (farrayPtr(1) /= sumB) &
      finalrc = ESMF_FAILURE
  endif
!EOC

  call ESMF_ArraySMMRelease(rh, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! ---------------------------------------------------------------------------

!BOE
! {\bf Grouping of source terms coming from the same PET}
!
! So far the {\tt srcTermProcessing} argument was kept at 0, and therefore
! source term grouping had not to be considered. Source term grouping is only
! possible for terms that originate from the same PET. In preparation
! for a closer look at the bfb effects of source term grouping, consider a 
! sparse matrix where two of the source terms are located on the same PET.
!EOE

  ! start looking into srcTermProcessing

  if (localPet == 0) then
    !
    ! Place the last two terms on the same PET, but still have srcTermProcessing
    ! at 0, therefore not different from having them on different PETs.
    !
    ! -> factors into dst element 1 on PET 0
    ! * sum with 3 addends (order by src seq. index): 0.5 + 0.1 + 0.1
    ! * addend 1 is on PET 0
    ! * addend 2 is on PET 2
    ! * addend 3 is on PET 2
    ! The "SRCPET" order of the three addends is given by the cyclic scheme
    ! src PET scheme, starting at dstPet, going to dstPet-petCount, modulo
    ! petCount. With that the sum order is this:
    ! (1,1)*s[1]Pet0 + (5,1)*s[5]Pet2 + (7,1)*s[7]Pet2  = 
    !       0.5      +       0.1      +       0.1       
!BOC
    factorIndexList(1,1) = 1  ! src seq index
    factorIndexList(2,1) = 1  ! dst seq index
    factorList(1) = 1.
    factorIndexList(1,2) = 5  ! src seq index
    factorIndexList(2,2) = 1  ! dst seq index
    factorList(2) = 1.
    factorIndexList(1,3) = 7  ! src seq index
    factorIndexList(2,3) = 1  ! dst seq index
    factorList(3) = 1.
!EOC
  endif
  
!BOE
! Here one of the source terms is located on PET 0 while the other two
! source terms are originating on PET 2. Keeping the {\tt srcTermProcessing}
! argument at 0 first, the term order under {\tt ESMF\_TERMORDER\_SRCPET} is 
! given by:
!
!     d(1) = s(1)[0] + s(5)[2] + s(7)[2] = 0.5 + 0.1 + 0.1
!
! And again the result is expected to be bfb identical to the number stored 
! in {\tt sumA}.
!EOE

!BOC
  ! forced srcTermProcessing
  srcTermProcessing=0
  
  call ESMF_ArraySMMStore(srcArray, dstArray, &
    factorIndexList=factorIndexList, factorList=factorList, &
    routehandle=rh, srcTermProcessing=srcTermProcessing, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOC
  call ESMF_ArraySMM(srcArray, dstArray, routehandle=rh, &
    termorderflag=ESMF_TERMORDER_SRCPET, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOC
  if (localPet == 0) then
    print *, "result SRCPET#5 = ", farrayPtr(1), " expect: ", sumA
    if (farrayPtr(1) /= sumA) &
      finalrc = ESMF_FAILURE
  endif
!EOC
  
  call ESMF_ArraySMMRelease(rh, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! ---------------------------------------------------------------------------

!BOE
! The same result is also expected with {\tt srcTermProcessing} set to 1. A
! value of 1 indicates that the multiplication of the source term with its
! sparse matrix factor is carried out on the source side before being sent to 
! the destination PET. The final sum is still carried out in the same order on
! the destination PET, essentially resulting in the exact same bfb identical
! sum as for {\tt srcTermProcessing} set to 0.
!EOE

!BOC
  ! forced srcTermProcessing
  srcTermProcessing=1
  
  call ESMF_ArraySMMStore(srcArray, dstArray, &
    factorIndexList=factorIndexList, factorList=factorList, &
    routehandle=rh, srcTermProcessing=srcTermProcessing, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOC
  call ESMF_ArraySMM(srcArray, dstArray, routehandle=rh, &
    termorderflag=ESMF_TERMORDER_SRCPET, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOC
  if (localPet == 0) then
    print *, "result SRCPET#6 = ", farrayPtr(1), " expect: ", sumA
    if (farrayPtr(1) /= sumA) &
      finalrc = ESMF_FAILURE
  endif
!EOC

  call ESMF_ArraySMMRelease(rh, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! ---------------------------------------------------------------------------

!BOE
! Increasing the {\tt srcTermProcessing} argument to 2 (or higher) results in 
! source term grouping of the terms (up to the number specified in 
! {\tt srcTermProcessing}) that are on the same source PET.
!
!     d(1) = s(1)[0] + ( s(5)[2] + s(7)[2] ) = 0.5 + (0.1 + 0.1)
!
! This result is bfb identical to first adding 0.1 and 0.1 into a partial sum,
! and then adding this sum to 0.5. This is the exact grouping of
! terms that was used to obtain the result stored in {\tt sumB} from above.
!EOE

!BOC
  ! forced srcTermProcessing
  srcTermProcessing=2
  
  call ESMF_ArraySMMStore(srcArray, dstArray, &
    factorIndexList=factorIndexList, factorList=factorList, &
    routehandle=rh, srcTermProcessing=srcTermProcessing, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOC
  call ESMF_ArraySMM(srcArray, dstArray, routehandle=rh, &
    termorderflag=ESMF_TERMORDER_SRCPET, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOC
  if (localPet == 0) then
    print *, "result SRCPET#7 = ", farrayPtr(1), " expect: ", sumB
    if (farrayPtr(1) /= sumB) &
      finalrc = ESMF_FAILURE
  endif
!EOC
  
  call ESMF_ArraySMMRelease(rh, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! ---------------------------------------------------------------------------

  ! the following examples will have 4 src terms
  deallocate(factorIndexList, factorList)
  smmElementCount = 0
  if (localPet == 0) then
    smmElementCount = 4
  endif
  allocate(factorIndexList(2,smmElementCount), factorList(smmElementCount))
    
  ! ---------------------------------------------------------------------------

!BOE
! In order to explore the effects of the {\tt srcTermProcessing} argument
! further, more terms on the same source PET are needed in the SMM sum.
! The following sparse matrix has four entries, three of which originate from
! the same PET (PET 3).
!EOE

  if (localPet == 0) then
    !
    ! First with srcTermProcessing set to 0
    !
    ! -> factors into dst element 1 on PET 0
    ! * sum with 4 addends (order by src seq. index): 0.5 + 0.2 + 0.1 + 0.1
    ! * addend 1 is on PET 0
    ! * addend 2 is on PET 3
    ! * addend 3 is on PET 3
    ! * addend 4 is on PET 3
    ! The "SRCPET" order of the three addends is given by the cyclic scheme
    ! src PET scheme, starting at dstPet, going to dstPet-petCount, modulo
    ! petCount. With that the sum order is this:
    ! (1,1)*s[1]Pet0 +  (2,1)*s[2]Pet3 + (8,1)*s[8]Pet3 + (12,1)*s[12]Pet3 = 
    !       0.5      +        0.2      +       0.1      +        0.1
!BOC
    factorIndexList(1,1) = 1  ! src seq index
    factorIndexList(2,1) = 1  ! dst seq index
    factorList(1) = 1.
    factorIndexList(1,2) = 2  ! src seq index
    factorIndexList(2,2) = 1  ! dst seq index
    factorList(2) = 1.
    factorIndexList(1,3) = 8  ! src seq index
    factorIndexList(2,3) = 1  ! dst seq index
    factorList(3) = 1.
    factorIndexList(1,4) = 12 ! src seq index
    factorIndexList(2,4) = 1  ! dst seq index
    factorList(4) = 1.
!EOC
  endif
  
!BOE
! Setting the {\tt srcTermProcessing} argument back to 0 puts the terms in 
! PET order, and canonical order for each PET bundle.
!
!     d(1) = s(1)[0] + s(2)[3] + s(8)[3] + s(12)[3] = 0.5 + 0.2 + 0.1 + 0.1
!
! The bfb identical result for this sum was calculated and stored in variable
! {\tt sumC} above.
!EOE

!BOC
  ! forced srcTermProcessing
  srcTermProcessing=0
  
  call ESMF_ArraySMMStore(srcArray, dstArray, &
    factorIndexList=factorIndexList, factorList=factorList, &
    routehandle=rh, srcTermProcessing=srcTermProcessing, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOC
  call ESMF_ArraySMM(srcArray, dstArray, routehandle=rh, &
    termorderflag=ESMF_TERMORDER_SRCPET, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOC
  if (localPet == 0) then
    print *, "result SRCPET#8 = ", farrayPtr(1), " expect: ", sumC
    if (farrayPtr(1) /= sumC) &
      finalrc = ESMF_FAILURE
  endif
!EOC
  
  call ESMF_ArraySMMRelease(rh, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! ---------------------------------------------------------------------------

!BOE
! Setting the {\tt srcTermProcessing} argument to a value of 2 results in the 
! following source term grouping:
!
!     d(1) = s(1)[0] + ( s(2)[3] + s(8)[3] ) + s(12)[3]
!          = 0.5 + ( 0.2 + 0.1 ) + 0.1,
!
! where the (0.2 + 0.1) partial sum is carried out on source PET 3, and
! then sent to the destination PET (PET 0), together with the unmodified data 
! from source element 8 (0.1). The final sum is performed on PET 0. The
! result is identical to the precomputed value stored in {\tt sumD}. The 
! numbers work out in a way where this result is bfb identical to the
! previous result, i.e. {\tt sumC}. However, this bfb match is purely 
! coincidental.
!EOE

    !
    ! Next with srcTermProcessing set to 2, changing the term order, but no
    ! bfb difference here.
    !
    ! -> factors into dst element 1 on PET 0
    ! * sum with 4 addends (order by src seq. index): 0.5 + 0.2 + 0.1 + 0.1
    ! * addend 1 is on PET 0
    ! * addend 2 is on PET 3
    ! * addend 3 is on PET 3
    ! * addend 4 is on PET 3
    ! The "SRCPET" order of the three addends is given by the cyclic scheme
    ! src PET scheme, starting at dstPet, going to dstPet-petCount, modulo
    ! petCount. With that the sum order is this:
    ! (1,1)*s[1]Pet0 + ( (2,1)*s[2]Pet3 + (8,1)*s[8]Pet3 ) + (12,1)*s[12]Pet3 = 
    !       0.5      + (       0.2      +       0.1      ) +        0.1
  
!BOC
  ! forced srcTermProcessing
  srcTermProcessing=2
  
  call ESMF_ArraySMMStore(srcArray, dstArray, &
    factorIndexList=factorIndexList, factorList=factorList, &
    routehandle=rh, srcTermProcessing=srcTermProcessing, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOC
  call ESMF_ArraySMM(srcArray, dstArray, routehandle=rh, &
    termorderflag=ESMF_TERMORDER_SRCPET, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOC
  if (localPet == 0) then
    print *, "result SRCPET#9 = ", farrayPtr(1), " expect: ", sumD
    if (farrayPtr(1) /= sumD) &
      finalrc = ESMF_FAILURE
  endif
!EOC
  
  call ESMF_ArraySMMRelease(rh, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! ---------------------------------------------------------------------------

!BOE
! Increasing the {\tt srcTermProcessing} argument up to 3 results in a three
! term partial sum on PET 3:
!
!     d(1) = s(1)[0] + ( s(2)[3] + s(8)[3] + s(12)[3] )
!          = 0.5 + ( 0.2 + 0.1 + 0.1 ).
!
! Again the final sum is performed on PET 0. The result is bfb identical to
! the number stored in {\tt sumE}, which, for the chosen numbers, works out to
! have a bfb difference compared to {\tt sumC} and {\tt sumD}.
!EOE

    !
    ! Next with srcTermProcessing set to 3, changing the term order, this time
    ! with bfb effect.
    !
    ! -> factors into dst element 1 on PET 0
    ! * sum with 4 addends (order by src seq. index): 0.5 + 0.2 + 0.1 + 0.1
    ! * addend 1 is on PET 0
    ! * addend 2 is on PET 3
    ! * addend 3 is on PET 3
    ! * addend 4 is on PET 3
    ! The "SRCPET" order of the three addends is given by the cyclic scheme
    ! src PET scheme, starting at dstPet, going to dstPet-petCount, modulo
    ! petCount. With that the sum order is this:
    ! (1,1)*s[1]Pet0 + ( (2,1)*s[2]Pet3 + (8,1)*s[8]Pet3 + (12,1)*s[12]Pet3 ) = 
    !       0.5      + (       0.2      +       0.1      +        0.1       )
  
!BOC
  ! forced srcTermProcessing
  srcTermProcessing=3
  
  call ESMF_ArraySMMStore(srcArray, dstArray, &
    factorIndexList=factorIndexList, factorList=factorList, &
    routehandle=rh, srcTermProcessing=srcTermProcessing, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOC
  call ESMF_ArraySMM(srcArray, dstArray, routehandle=rh, &
    termorderflag=ESMF_TERMORDER_SRCPET, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOC
  if (localPet == 0) then
    print *, "result SRCPET#10 = ", farrayPtr(1), " expect: ", sumE
    if (farrayPtr(1) /= sumE) &
      finalrc = ESMF_FAILURE
  endif
!EOC
  
  call ESMF_ArraySMMRelease(rh, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! ---------------------------------------------------------------------------

!BOE
! {\bf Reproducibility and Performance}
!
! The above examples show how bit-for-bit (bfb) reproducibility is a result of
! controlling the term order. ESMF offers several options to control the term
! order in the sparse matrix multiplication (SMM) implementation:
! \begin{sloppypar}
! \begin{itemize}
! \item To guarantee bfb reproducibility between consecutive executions of the
! same RouteHandle object, the {\tt ESMF\_TERMORDER\_SRCPET} execution-time 
! option suffices.
! \item If bfb reproducibility is required between {\em different} RouteHandles, 
! e.g. a RouteHandle that is precomputed each time the application starts, 
! then it must be further ensured that the same value of {\tt srcTermProcessing}
! is specified during the store call. Under these conditions the ESMF SMM 
! implementation guarantees bfb identical results between runs, as long as the
! number of PETs does not change.
! \item To guarantee bfb reproducibility between different runs, even when the
! number of PETs, and therefore the data distribution changes, the execution
! option {\tt ESMF\_TERMORDER\_SRCSEQ} must be chosen together with
! {\tt srcTermProcessing} equal to 0 or 1 (in order to prevent partial sums).
! \end{itemize}
! \end{sloppypar}
!
! The term order in a SMM operation does not only affect the bfb
! reproducibility of the result, but also affects the SMM {\em performance}. 
! The precise performance implications of a specific term order are
! complicated and strongly depend on the exact problem structure, as well as
! on the details of the compute hardware. ESMF implements an auto-tuning 
! mechanism that can be used to conveniently determine a close to optimal set
! of SMM performance parameters.
!
! There are two SMM performance parameters in ESMF that are encoded into a
! RouteHandle during store-time: {\tt srcTermProcessing} and
! {\tt pipelineDepth}. The first one affects the term order in the SMM sums and 
! has been discussed in detail above. The second parameter, {\tt pipelineDepth},
! determines how many in- and out-bound messages may be outstanding on each
! PET. It has no effect on the term order and does not lead to bfb differences
! in the SMM results. However, in order to achieve good performance
! reproducibility, the user has the option to pass in a fixed value of the
! {\tt pipelineDepth} argument when precomputing RouteHandles.
! 
! Store calls that take the {\tt srcTermProcessing} and/or {\tt pipelineDepth}
! argument specify them as {\tt optional} with {\tt intent(inout)}. Omitting the
! argument when calling, or passing a variable that is set to a negative
! number, indicates that the respective parameter needs to be determined by
! the library. Further, if a variable with a negative value was passed in, then
! the variable is overwritten and replaced by the auto-tuned value on return. Through
! this mechanism a user can leverage the built-in auto-tuning feature of ESMF to
! obtain the best possible performance for a specific problem on a particular
! compute hardware, while still ensuring bfb and performance
! reproducibility between runs. The following example shows code that first
! checks if previously stored SMM performance parameters are available in a
! file on disk, and then either reads and uses them, or else uses auto-tuning
! to determine the parameters before writing them to file. For simplicity the
! same sparse matrix as in the previous example is used.
!
!EOE

! repeat the following code a few times to test bfb between SMM with fixed
! parameters
do i=1,5

!BOC
  ! precondition the arguments for auto-tuning and overwriting
  srcTermProcessing = -1  ! init negative value
  pipelineDepth     = -1  ! init negative value

  ! get a free Fortran i/o unit
  call ESMF_UtilIOUnitGet(unit=iounit, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  ! try to open the file that holds the SMM parameters
  open(unit=iounit, file="smmParameters.dat", status="old", action="read", &
    form="unformatted", iostat=iostat)
  
  if (iostat == 0) then
    ! the file was present -> read from it and close it again
    read(unit=iounit, iostat=iostat) srcTermProcessing, pipelineDepth, &
      sumCompare
    close(unit=iounit)
  endif
  
  if ((localPet == 0) .and. (iostat == 0)) then
    print *, "SMM parameters successfully read from file"
    print *, " srcTermProcessing=", srcTermProcessing, " pipelineDepth=", &
      pipelineDepth, " ==>> sumCompare=", sumCompare
  endif

  call ESMF_ArraySMMStore(srcArray, dstArray, &
    factorIndexList=factorIndexList, factorList=factorList, &
    routehandle=rh, srcTermProcessing=srcTermProcessing, &
    pipelineDepth=pipelineDepth, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_ArraySMM(srcArray, dstArray, routehandle=rh, &
    termorderflag=ESMF_TERMORDER_SRCPET, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  if ((localPet == 0) .and. (iostat /= 0)) then
    print *, "SMM parameters determined via auto-tuning -> dump to file"
    open(unit=iounit, file="smmParameters.dat", status="unknown", &
      action="write", form="unformatted")
    write(unit=iounit) srcTermProcessing, pipelineDepth, farrayPtr(1)
    close(unit=iounit)
  endif
  
  if (localPet == 0) then
    if (iostat /= 0) then
      ! cannot do bfb comparison of the result without reference
      print *, "result SRCPET#11 = ", farrayPtr(1)
    else
      ! do bfb comparison of the result against reference
      print *, "result SRCPET#11 = ", farrayPtr(1), " expect: ", sumCompare
      if (farrayPtr(1) /= sumCompare) then
        finalrc = ESMF_FAILURE
        write (msg, *) "Numerical difference detected: ", &
          farrayPtr(1)-sumCompare
        call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
      endif
    endif
  endif
!EOC

  ! barrier ensures that file is written before any PET tries to read
  call ESMF_VMBarrier(vm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! release RH before the next pre-compute
  call ESMF_ArraySMMRelease(rh, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
enddo

!BOE
! Running this example for the first time exercises the auto-tuning branch. The
! auto-tuned {\tt srcTermProcessing} and {\tt pipelineDepth} parameters are
! then used in the SMM execution, as well as written to file. The SMM result
! variable is also written to the same file for test purposes.
! Any subsequent execution of the same example branches into the code that
! reads the previously determined SMM execution parameters from file, re-using
! them during store-time. This ensures bfb reproducibility of the SMM result, 
! which is tested in this example by comparing to the previously stored value.
!EOE

  ! ---------------------------------------------------------------------------

!--------- extra test for ESMF_TERMORDER_SRCSEQ  ---------
  srcTermProcessing = 2
  call ESMF_ArraySMMStore(srcArray, dstArray, &
    factorIndexList=factorIndexList, factorList=factorList, &
    routehandle=rh, srcTermProcessing=srcTermProcessing, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ArraySMM(srcArray, dstArray, routehandle=rh, &
    termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (localPet == 0) then
    print *, "result SRCSEQ#3 = ", farrayPtr(1), " expect: ", sumC
    if (farrayPtr(1) /= sumC) &
      finalrc = ESMF_FAILURE
  endif

  call ESMF_ArraySMMRelease(rh, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!--------- end extra stuff for now ---------

  deallocate(factorIndexList, factorList)

10 continue

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_RHandleBitForBitEx.F90"
  else
    print *, "FAIL: ESMF_RHandleBitForBitEx.F90"
  endif

end program
