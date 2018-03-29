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
!
    program ESMF_FieldRepDimEx

!------------------------------------------------------------------------------
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================
! !PROGRAM: ESMF_FieldRepDimEx - Field with replicated dimension
!
! !DESCRIPTION:
!
! This program shows examples of Field with replicated dimension
!-----------------------------------------------------------------------------
#include "ESMF.h"
    ! ESMF Framework module
    use ESMF
    use ESMF_TestMod
    implicit none

    ! Local variables
    type(ESMF_VM)       :: vm
    type(ESMF_Field)    :: field
    type(ESMF_Grid)     :: grid
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_ArraySpec):: arrayspec

    ! local arguments used to get info from field
    type(ESMF_Grid)         :: grid1
    type(ESMF_Array)        :: array
    type(ESMF_TypeKind_Flag)     :: typekind
    integer                 :: dimCount, gridrank_repdim
    type(ESMF_StaggerLoc)   :: lstaggerloc
    integer, dimension(4) :: lgridToFieldMap
    integer, dimension(1) :: lungriddedLBound
    integer, dimension(1) :: lungriddedUBound
    integer, dimension(2,1) :: ltotalLWidth
    integer, dimension(2,1) :: ltotalUWidth

    ! local arguments used to verify field get
    integer                                     :: i, ii, ij, ik
    integer, dimension(3)                       :: felb, feub, fclb, fcub, ftlb, ftub
    integer, dimension(3)                       :: fec, fcc, ftc
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farray
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farray1
    real(ESMF_KIND_R8)                          :: n
    integer, dimension(3,1)                     :: aelb, aeub, aclb, acub, atlb, atub
    integer, dimension(:), allocatable          :: audlb, audub
    integer                                     :: arank, adimCount
    integer                                     :: finalrc, rc, result
    integer                                     :: gridToFieldMap(4) = (/1,0,2,0/)

    character(ESMF_MAXSTR) :: testname
    character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    write(failMsg, *) "Example failure"
    write(testname, *) "Example ESMF_FieldRepDimEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


!   !Set finalrc to success
    finalrc = ESMF_SUCCESS

    call ESMF_Initialize(vm=vm, defaultlogfilename="FieldRepDimEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create a Field with replicated dimensions}
!\label{sec:field:usage:create_repdim}
!
!  \begin{sloppypar}
!  In this example an {\tt ESMF\_Field} with replicated dimension is created from an {\tt ESMF\_Grid} and
!  an {\tt ESMF\_Arrayspec}. A user can also use other {\tt ESMF\_FieldCreate()} methods to create replicated
!  dimension Field, this example illustrates the key concepts and use of a replicated dimension Field.
!  \end{sloppypar}
!
!  Normally gridToFieldMap argument in {\tt ESMF\_FieldCreate()} should not contain
!  0 value entries. However, for a Field with replicated dimension, a 0 entry in gridToFieldMap
!  indicates the corresponding Grid dimension is replicated in the Field. In such a Field,
!  the rank of the Field is no longer necessarily greater than its Grid rank.
!  An example will make this clear. We will start by creating Distgrid and Grid.
!
!EOE

!BOC
    ! create 4D distgrid
    distgrid = ESMF_DistGridCreate(minIndex=(/1,1,1,1/), &
        maxIndex=(/6,4,6,4/), regDecomp=(/2,1,2,1/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! create 4D grid on top of the 4D distgrid
    grid = ESMF_GridCreate(distgrid=distgrid, name="grid", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! create 3D arrayspec
    call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R8, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC
!BOE
! In this example, a user creates a 3D Field with replicated dimension
! replicated along the 2nd and 4th dimension of its underlying 4D Grid.
! In addition, the 2nd dimension of the Field is ungridded (why?). The 1st and
! 3rd dimensions of the Field have halos.
!EOE
!BOC
    ! create field, 2nd and 4th dimensions of the Grid are replicated
    field = ESMF_FieldCreate(grid, arrayspec, indexflag=ESMF_INDEX_DELOCAL, &
        gridToFieldMap=(/1,0,2,0/), &
        ungriddedLBound=(/1/), ungriddedUBound=(/4/), &
        totalLWidth=(/1,1/), totalUWidth=(/4,5/), &
        staggerloc=ESMF_STAGGERLOC_CORNER, &
        rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! get basic information from the field
    call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
        dimCount=dimCount, staggerloc=lstaggerloc, &
        gridToFieldMap=lgridToFieldMap, ungriddedLBound=lungriddedLBound, &
        ungriddedUBound=lungriddedUBound, totalLWidth=ltotalLWidth, &
        totalUWidth=ltotalUWidth, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! get bounds information from the field
    call ESMF_FieldGet(field, localDe=0, farrayPtr=farray, &
        exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
        computationalLBound=fclb, computationalUBound=fcub, &
        computationalCount=fcc, totalLBound=ftlb, totalUBound=ftub, &
        totalCount=ftc, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Next we verify that the field and array bounds agree with each other
!EOE
!BOC
    call ESMF_ArrayGet(array, rank=arank, dimCount=adimCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    gridrank_repdim = 0
    do i = 1, size(gridToFieldMap)
        if(gridToFieldMap(i) == 0) gridrank_repdim = gridrank_repdim + 1
    enddo
!EOC
!BOE
! Number of undistributed dimension of the array {\it X} is computed from
! total rank of the array {\it A}, the dimension count of its underlying distgrid
! {\it B} and number of replicated dimension in the distgrid {\it C}.
! We have the following formula: X = A - (B - C)
!EOE
!BOC
    allocate(audlb(arank-adimCount+gridrank_repdim), &
        audub(arank-adimCount+gridrank_repdim))
    call ESMF_ArrayGet(array, exclusiveLBound=aelb, exclusiveUBound=aeub, &
        computationalLBound=aclb, computationalUBound=acub, &
        totalLBound=atlb, totalUBound=atub, &
        undistLBound=audlb, undistUBound=audub, &
        rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! verify the ungridded bounds from field match
    ! undistributed bounds from its underlying array
    do i = 1, arank-adimCount
        if(lungriddedLBound(i) .ne. audlb(i) ) &
            rc = ESMF_FAILURE
    enddo
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    do i = 1, arank-adimCount
        if(lungriddedUBound(i) .ne. audub(i) ) &
            rc = ESMF_FAILURE
    enddo
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! We then verify the data in the replicated dimension Field can be updated and accessed.
!EOE
!BOC
    do ik = ftlb(3), ftub(3)
     do ij = ftlb(2), ftub(2)
      do ii = ftlb(1), ftub(1)
        farray(ii,ij,ik) = ii+ij*2+ik
      enddo
     enddo
    enddo
    ! access and verify
    call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, &
        rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    do ik = ftlb(3), ftub(3)
     do ij = ftlb(2), ftub(2)
      do ii = ftlb(1), ftub(1)
        n = ii+ij*2+ik
        if(farray1(ii,ij,ik) .ne. n ) rc = ESMF_FAILURE
      enddo
     enddo
    enddo
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! release resources
    call ESMF_FieldDestroy(field)
    call ESMF_GridDestroy(grid)
    call ESMF_DistGridDestroy(distgrid)
!EOC
    print *, "Field with replicated dimension returned"

    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
    ! file that the scripts grep for.
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)



!-------------------------------------------------------------------------
     call ESMF_Finalize(rc=rc)
!-------------------------------------------------------------------------

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_FieldRepDimEx.F90"
    else
        print *, "FAIL: ESMF_FieldRepDimEx.F90"
    end if
end program ESMF_FieldRepDimEx
