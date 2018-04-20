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

     program FieldSMMEx

!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================
!
! !PROGRAM: ESMF_FieldSMMEx - Field SMMribution
!
! !DESCRIPTION:
!
! This program shows examples of Field interfaces for
! sparse matrix multiplication of data.
!-----------------------------------------------------------------------------
#include "ESMF.h"
#include "ESMF_Macros.inc"
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSMMEx"
     ! ESMF Framework module
     use ESMF
     use ESMF_TestMod
     implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------

    ! Local variables
    integer :: rc, finalrc
    type(ESMF_Field)                            :: srcField, dstField
    type(ESMF_Field)                            :: srcFieldA, dstFieldA
    type(ESMF_Grid)                             :: grid, dstGrid
    type(ESMF_DistGrid)                         :: distgrid
    type(ESMF_VM)                               :: vm
    type(ESMF_RouteHandle)                      :: routehandle
    type(ESMF_Array)                            :: srcArray, dstArray
    type(ESMF_ArraySpec)                        :: arrayspec
    integer                                     :: localrc, lpe, i, result

    integer, allocatable                        :: src_farray(:), dst_farray(:)
    integer                                     :: fa_shape(1), tlb(1), tub(1)
    integer, pointer                            :: fptr(:)
    real(ESMF_KIND_R4), pointer                 :: fptr2d(:,:)
    real(ESMF_KIND_R4), pointer                 :: src_farray2(:)

    real(ESMF_KIND_R4), allocatable          :: factorList(:)
    integer, allocatable                        :: factorIndexList(:,:)

    character(ESMF_MAXSTR)                      :: testname
    character(ESMF_MAXSTR)                      :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    write(failMsg, *) "Example failure"
    write(testname, *) "Example ESMF_FieldSMMEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


    rc = ESMF_SUCCESS
    localrc = ESMF_SUCCESS
    finalrc = ESMF_SUCCESS
!------------------------------------------------------------------------------
    call ESMF_Initialize(defaultlogfilename="FieldSMMEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
!------------------------------------------------------------------------------
!BOE
! \subsubsection{Sparse matrix multiplication from source Field to destination Field}
! \label{sec:field:usage:smm_1dptr}
!
! The {\tt ESMF\_FieldSMM()} interface can be used to perform sparse matrix multiplication
! from
! source Field to destination Field. This interface is overloaded by type and kind;
!
! In this example, we first create two 1D Fields, a source Field and a destination
! Field. Then we use {\tt ESMF\_FieldSMM} to
! perform sparse matrix multiplication from source Field to destination Field.
!
! The source and destination Field data are arranged such that each of the 4 PETs has 4
! data elements. Moreover, the source Field has all its data elements initialized to a linear
! function based on local PET number.
! Then collectively on each PET, a SMM according to the following formula
! is preformed: \newline
! $dstField(i) = i * srcField(i), i = 1 ... 4$ \newline
! \newline
!
! Because source Field data are initialized to a linear function based on local PET number,
! the formula predicts that
! the result destination Field data on each PET is {1,2,3,4}. This is verified in the
! example.
!
! Section \ref{Array:SparseMatMul} provides a detailed discussion of the
! sparse matrix multiplication operation implemented in ESMF.
!
!EOE
!BOC

    ! Get current VM and pet number
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_VMGet(vm, localPet=lpe, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! create distgrid and grid
    distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/16/), &
        regDecomp=(/4/), &
        rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    grid = ESMF_GridCreate(distgrid=distgrid, &
        name="grid", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridGetFieldBounds(grid, localDe=0, totalCount=fa_shape, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! create src\_farray, srcArray, and srcField
    ! +--------+--------+--------+--------+
    !      1        2        3        4            ! value
    ! 1        4        8        12       16       ! bounds
    allocate(src_farray(fa_shape(1)) )
    src_farray = lpe+1
    srcArray = ESMF_ArrayCreate(distgrid, src_farray, &
                indexflag=ESMF_INDEX_DELOCAL, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    srcField = ESMF_FieldCreate(grid, srcArray, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! create dst_farray, dstArray, and dstField
    ! +--------+--------+--------+--------+
    !      0        0        0        0            ! value
    ! 1        4        8        12       16       ! bounds
    allocate(dst_farray(fa_shape(1)) )
    dst_farray = 0
    dstArray = ESMF_ArrayCreate(distgrid, dst_farray, &
                indexflag=ESMF_INDEX_DELOCAL, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    dstField = ESMF_FieldCreate(grid, dstArray, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! perform sparse matrix multiplication
    ! 1. setup routehandle from source Field to destination Field
    ! initialize factorList and factorIndexList
    allocate(factorList(4))
    allocate(factorIndexList(2,4))
    factorList = (/1,2,3,4/)
    factorIndexList(1,:) = (/lpe*4+1,lpe*4+2,lpe*4+3,lpe*4+4/)
    factorIndexList(2,:) = (/lpe*4+1,lpe*4+2,lpe*4+3,lpe*4+4/)

    call ESMF_FieldSMMStore(srcField, dstField, routehandle, &
        factorList, factorIndexList, rc=localrc)
    if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! 2. use precomputed routehandle to perform SMM
    call ESMF_FieldSMM(srcfield, dstField, routehandle, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! verify sparse matrix multiplication
    call ESMF_FieldGet(dstField, localDe=0, farrayPtr=fptr, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Verify that the result data in dstField is correct.
    ! Before the SMM op, the dst Field contains all 0.
    ! The SMM op reset the values to the index value, verify this is the case.
    ! +--------+--------+--------+--------+
    !  1 2 3 4  2 4 6 8  3 6 9 12  4 8 12 16       ! value
    ! 1        4        8        12       16       ! bounds
    do i = lbound(fptr, 1), ubound(fptr, 1)
        if(fptr(i) /= i*(lpe+1)) rc = ESMF_FAILURE
    enddo
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Field sparse matrix multiplication can also be applied between Fields 
! that matche the original Fields in {\em type}, {\em kind}, and 
! memory layout of the {\em gridded} dimensions. However, the size, number, 
! and index order of {\em ungridded} dimensions may be different. See section
! \ref{RH:Reusability} for a more detailed discussion of RouteHandle 
! reusability
!EOE
!BOC
    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=2, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Create two fields with ungridded dimensions using the Grid created previously.
! The new Field pair has matching number of elements. The ungridded dimension
! is mapped to the first dimension of either Field.
!EOE
!BOC
    srcFieldA = ESMF_FieldCreate(grid, arrayspec, gridToFieldMap=(/2/), &
        ungriddedLBound=(/1/), ungriddedUBound=(/10/), rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    dstFieldA = ESMF_FieldCreate(grid, arrayspec, gridToFieldMap=(/2/), &
        ungriddedLBound=(/1/), ungriddedUBound=(/10/), rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Using the previously computed routehandle, the sparse matrix multiplication
! can be performed between the Fields.
!EOE
!BOC
    call ESMF_FieldSMM(srcfieldA, dstFieldA, routehandle, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    ! release route handle
    call ESMF_FieldSMMRelease(routehandle, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! destroy all objects created in this example to prevent memory leak
    call ESMF_FieldDestroy(srcField, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldDestroy(dstField, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldDestroy(srcFieldA, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldDestroy(dstFieldA, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_ArrayDestroy(srcArray, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_ArrayDestroy(dstArray, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridDestroy(grid, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_DistGridDestroy(distgrid, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    deallocate(src_farray, dst_farray, factorList, factorIndexList)

!BOE
! In the following discussion, we demonstrate how to set up a SMM routehandle
! between a pair of Fields that are different in number of gridded dimensions
! and the size of those gridded dimensions. The source Field has a 1D decomposition
! with 16 total elements; the destination Field has a 2D decomposition with
! 12 total elements. For ease of understanding of the actual matrix calculation,
! a global indexing scheme is used.
!EOE
!BOC
    distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/16/), &
        indexflag=ESMF_INDEX_GLOBAL, &
        regDecomp=(/4/), &
        rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    grid = ESMF_GridCreate(distgrid=distgrid, &
        indexflag=ESMF_INDEX_GLOBAL, &
        name="grid", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridGetFieldBounds(grid, localDe=0, totalLBound=tlb, &
                       totalUBound=tub, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC
!BOE
! create 1D src\_farray, srcArray, and srcField
!\begin{verbatim}
! +  PET0  +  PET1  +  PET2  +  PET3  +
! +--------+--------+--------+--------+
!      1        2        3        4            ! value
! 1        4        8        12       16       ! bounds of seq indices
!\end{verbatim}
!EOE
!BOC
    allocate(src_farray2(tlb(1):tub(1)) )
    src_farray2 = lpe+1
    srcArray = ESMF_ArrayCreate(distgrid, src_farray2, &
                  indexflag=ESMF_INDEX_GLOBAL, &
      rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    !print *, lpe, '+', tlb, tub, '+', src_farray2

    srcField = ESMF_FieldCreate(grid, srcArray, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!EOC
!BOE
! Create 2D dstField on the following distribution
! (numbers are the sequence indices):
!\begin{verbatim}
! +  PET0  +  PET1  +  PET2  +  PET3  +
! +--------+--------+--------+--------+
! |        |        |        |        |
! |   1    |   4    |   7    |   10   |
! |        |        |        |        |
! +--------+--------+--------+--------+
! |        |        |        |        |
! |   2    |   5    |   8    |   11   |
! |        |        |        |        |
! +--------+--------+--------+--------+
! |        |        |        |        |
! |   3    |   6    |   9    |   12   |
! |        |        |        |        |
! +--------+--------+--------+--------+
!\end{verbatim}
!EOE
!BOC

    ! Create the destination Grid
    dstGrid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/3,4/), &
      indexflag = ESMF_INDEX_GLOBAL, &
      regDecomp = (/1,4/), &
      rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    dstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R4, &
      indexflag=ESMF_INDEX_GLOBAL, &
      rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC
!BOE
! Perform sparse matrix multiplication $dst_i$ = $M_{i,j}$ * $src_j$
! First setup routehandle from source Field to destination Field
! with prescribed factorList and factorIndexList.
!
! The sparse matrix is of size 12x16, however only the following entries
! are filled:
! \begin{verbatim}
! M(3,1) = 0.1
! M(3,10) = 0.4
! M(8,2) = 0.25
! M(8,16) = 0.5
! M(12,1) = 0.3
! M(12,16) = 0.7
! \end{verbatim}
!
! By the definition of matrix calculation, the 8th element on PET2 in the
! dstField equals to 0.25*srcField(2) + 0.5*srcField(16) = 0.25*1+0.5*4=2.25
! For simplicity, we will load the factorList and factorIndexList on
! PET 0 and 1, the SMMStore engine will load balance the parameters on all 4
! PETs internally for optimal performance.
!EOE
!BOC
    if(lpe == 0) then
      allocate(factorList(3), factorIndexList(2,3))
      factorList=(/0.1,0.4,0.25/)
      factorIndexList(1,:)=(/1,10,2/)
      factorIndexList(2,:)=(/3,3,8/)
      call ESMF_FieldSMMStore(srcField, dstField, routehandle=routehandle, &
          factorList=factorList, factorIndexList=factorIndexList, rc=localrc)
      if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    else if(lpe == 1) then
      allocate(factorList(3), factorIndexList(2,3))
      factorList=(/0.5,0.3,0.7/)
      factorIndexList(1,:)=(/16,1,16/)
      factorIndexList(2,:)=(/8,12,12/)
      call ESMF_FieldSMMStore(srcField, dstField, routehandle=routehandle, &
          factorList=factorList, factorIndexList=factorIndexList, rc=localrc)
      if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    else
      call ESMF_FieldSMMStore(srcField, dstField, routehandle=routehandle, &
          rc=localrc)
      if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    ! 2. use precomputed routehandle to perform SMM
    call ESMF_FieldSMM(srcfield, dstField, routehandle=routehandle, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC

    ! verify sparse matrix multiplication
    call ESMF_FieldGet(dstField, localDe=0, farrayPtr=fptr2d, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Verify that the result data in dstField is correct.
    print *, lpe, '-', lbound(fptr2d), '-',ubound(fptr2d),'-', fptr2d

    ! destroy all objects created in this example to prevent memory leak
    call ESMF_FieldSMMRelease(routehandle, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldDestroy(srcField, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldDestroy(dstField, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    !call ESMF_FieldDestroy(srcFieldA, rc=rc)
    !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    !call ESMF_FieldDestroy(dstFieldA, rc=rc)
    !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_ArrayDestroy(srcArray, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridDestroy(grid, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridDestroy(dstgrid, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_DistGridDestroy(distgrid, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    deallocate(src_farray2)
    if(allocated(factorList)) deallocate(factorList, factorIndexList)

    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
    ! file that the scripts grep for.
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)


     call ESMF_Finalize(rc=rc)

     if (rc.NE.ESMF_SUCCESS) then
       finalrc = ESMF_FAILURE
     end if

     if (finalrc.EQ.ESMF_SUCCESS) then
       print *, "PASS: ESMF_FieldSMMEx.F90"
     else
       print *, "FAIL: ESMF_FieldSMMEx.F90"
     end if

    end program FieldSMMEx
