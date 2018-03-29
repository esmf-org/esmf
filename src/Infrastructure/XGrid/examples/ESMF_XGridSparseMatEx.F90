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
    program ESMF_XGridSparseMatEx

!------------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
! !PROGRAM: ESMF_XGridSparseMatEx - XGrid Examples
!
! !DESCRIPTION:
!
! This program shows examples of XGrid get data pointer methods
!-----------------------------------------------------------------------------
#include "ESMF.h"
    ! ESMF Framework module
    use ESMF_TestMod
    use ESMF
    use ESMF_XGridMod
    use ESMF_XGridCreateMod
    implicit none

    ! Local variables
    integer                             :: localrc, i
    type(ESMF_XGrid)                    :: xgrid
    type(ESMF_Grid)                     :: sideA(2), sideB(1)
    type(ESMF_DistGrid)                 :: sideAdg(2), sideBdg(1), distgrid
    real*8                              :: centroid(12,2), area(12)
    type(ESMF_XGridSpec)                :: sparseMatA2X(2), sparseMatX2B(1)

    type(ESMF_Grid)                     :: l_sideA(2), l_sideB(1)
    type(ESMF_DistGrid)                 :: l_sideAdg(2), l_sideBdg(1)
    real(ESMF_KIND_R8)                  :: l_centroid(12,2), l_area(12)
    type(ESMF_XGridSpec)                :: l_sparseMatA2X(2), l_sparseMatX2B(1)
    type(ESMF_Field)                    :: field, srcField(2), dstField(1)

    integer                             :: eleCount, ngridA, ngridB
    integer                             :: elb, eub, ec

    real(ESMF_KIND_R8), pointer         :: farrayPtr(:,:), xfarrayPtr(:)
    real(ESMF_KIND_R8)                  :: xgrid_area(12), B_area(2,2)
    integer                             :: xlb(1), xub(1)
    type(ESMF_RouteHandle)              :: rh_src2xgrid(2), rh_xgrid2dst(1)

    real(ESMF_KIND_R8)                  :: centroidA1X(2), centroidA1Y(2)
    real(ESMF_KIND_R8)                  :: centroidA2X(2), centroidA2Y(1)
    real(ESMF_KIND_R8)                  :: centroidBX(2), centroidBY(2)
    real(ESMF_KIND_R8), pointer         :: coordX(:), coordY(:)

    integer                             :: finalrc, result
    character(ESMF_MAXSTR)              :: testname
    character(ESMF_MAXSTR)              :: failMsg
!   !Set finalrc to success
    finalrc = ESMF_SUCCESS

    write(failMsg, *) "Example failure"
    write(testname, *) "Example ESMF_XGridSparseMatEx"

    call ESMF_Initialize(defaultlogfilename="XGridSparseMatEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=localrc)
    if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create an XGrid from user input data then use it for regridding}
!\label{sec:xgrid:usage:xgrid_createfromsparsemat}
!
! Alternatively, XGrid can be created from Grids on either side,
! area and centroid information of XGrid cells, sparse matrix matmul information.
! The functionalities provided by the
! XGrid object is constrained by the user supplied input during its creation time.
!
! In this example, we will set up a simple XGrid from overlapping Grids on
! either side of the XGrid. Then we perform a flux exchange from one side
! to the other side of the XGrid. The Grids are laid out in the following figure:
!\begin{center}
!\begin{figure}
!\center
!\scalebox{0.6}{\includegraphics{XGridEx1}}
!\caption{Grid layout for simple XGrid creation example. Overlapping of 3 Grids
!(Green 2x2, Red 2x1, Blue 2x2). Green and red Grids on side A, blue Grid on side
!B, black indicates the resulting XGrid. Color coded sequence indices are shown.
!Physical coordinates are the tuples in parenthesis, e.g. at the four
!corners of rectangular computational domain.}
!\label{fig:xgridsimple}
!\end{figure}
!\end{center}
!
! We start by creating the Grids on both sides and associate coordinates with
! the Grids. For details of Grid creation and coordinate use, please refer to
! Grid class documentation.
!EOE
!BOC
    sideA(1) = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/2,2/), &
        coordDep1=(/1/), &
        coordDep2=(/2/), &
        name='source Grid 1 on side A', rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

!BOC
    sideA(2) = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/2,1/), &
        coordDep1=(/1/), &
        coordDep2=(/2/), &
        name='source Grid 2 on side A', rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

!BOC
    do i = 1, 2
        call ESMF_GridAddCoord(sideA(i), staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
!EOC
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                                                endflag=ESMF_END_ABORT)
!BOC
    enddo
!EOC

!BOE
! Coordinate for the Grids on sideA, refer to the Grid layout diagram for the
! interpretation of the coordinate values:
!EOE
!BOC
    ! SideA first grid
    centroidA1X=(/0.5, 1.5/)
    centroidA1Y=(/0.5, 1.5/)
    call ESMF_GridGetCoord(sideA(1), localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
        farrayPtr=coordX, rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
    coordX = centroidA1X
    call ESMF_GridGetCoord(sideA(1), localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
        farrayPtr=coordY, rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
         endflag=ESMF_END_ABORT)
!BOC
    coordY = centroidA1Y

    ! SideA second grid
    centroidA2X=(/0.5, 1.5/)
    centroidA2Y=(/2.5/)
    call ESMF_GridGetCoord(sideA(2), localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
        farrayPtr=coordX, rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
        endflag=ESMF_END_ABORT)
!BOC
    coordX = centroidA2X
    call ESMF_GridGetCoord(sideA(2), localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
        farrayPtr=coordY, rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
    coordY = centroidA2Y
!EOC

!BOE
! Create the destination grid on side B, only one Grid exists on side B. Also associate
! coordinate with the Grid:
!EOE
!BOC
    sideB(1) = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/2,2/), &
        coordDep1=(/1/), coordDep2=(/2/), &
        name='destination Grid on side B', rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
    do i = 1, 1
        call ESMF_GridAddCoord(sideB(i), staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
!EOC
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
    enddo

    ! SideB grid
    centroidBX=(/0.75, 1.75/)
    centroidBY=(/0.75, 2.25/)
    call ESMF_GridGetCoord(sideB(1), localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, farrayPtr=coordX, &
                rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
    coordX = centroidBX
    call ESMF_GridGetCoord(sideB(1), localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, farrayPtr=coordY, &
                rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
    coordY = centroidBY
!EOC

!EOE
! Now we need to set up the sparse matrix parameter for regridding. These are hardcoded
! for the purpose of demonstration. They are generally read in from an external file and
! distributed to processors.
!EOE
    allocate(sparseMatA2X(1)%factorIndexList(2,9), sparseMatA2X(1)%factorList(9))
    allocate(sparseMatA2X(2)%factorIndexList(2,3), sparseMatA2X(2)%factorList(3))
    allocate(sparseMatX2B(1)%factorIndexList(2,12), sparseMatX2B(1)%factorList(12))

!BOE
!
! Set up the mapping indices and weights from A side to the XGrid. For details of
! sequence indices, factorIndexList, and factorList, please see section
! \ref{Array:SparseMatMul} in the reference manual. Please refer to the figure above
! for interpretation of the sequence indices used here.
!
! In order to compute the destination flux on sideB through the XGrid as an mediator,
! we need to set up the factorList (weights) and factorIndexList (indices)
! for sparse matrix multiplication in this formulation:
! dst\_flux = W'*W*src\_flux, where W' is the weight matrix from the XGrid to
! destination; and W is the weight matrix from source to the XGrid. The weight matrix
! is generated using destination area weighted algorithm. Please refer to figure
! \ref {fig:xgridsimple} for details.
!
!EOE
!BOC
    ! Set up mapping from A1 -> X
    sparseMatA2X(1)%factorIndexList(1,1)=1    ! src seq index (green)
    sparseMatA2X(1)%factorIndexList(1,2)=2    ! src seq index (green)
    sparseMatA2X(1)%factorIndexList(1,3)=2    ! src seq index (green)
    sparseMatA2X(1)%factorIndexList(1,4)=3    ! src seq index (green)
    sparseMatA2X(1)%factorIndexList(1,5)=4    ! src seq index (green)
    sparseMatA2X(1)%factorIndexList(1,6)=4    ! src seq index (green)
    sparseMatA2X(1)%factorIndexList(1,7)=3    ! src seq index (green)
    sparseMatA2X(1)%factorIndexList(1,8)=4    ! src seq index (green)
    sparseMatA2X(1)%factorIndexList(1,9)=4    ! src seq index (green)

    sparseMatA2X(1)%factorIndexList(2,1)=1    ! dst seq index (black)
    sparseMatA2X(1)%factorIndexList(2,2)=2    ! dst seq index (black)
    sparseMatA2X(1)%factorIndexList(2,3)=3    ! dst seq index (black)
    sparseMatA2X(1)%factorIndexList(2,4)=4    ! dst seq index (black)
    sparseMatA2X(1)%factorIndexList(2,5)=5    ! dst seq index (black)
    sparseMatA2X(1)%factorIndexList(2,6)=6    ! dst seq index (black)
    sparseMatA2X(1)%factorIndexList(2,7)=7    ! dst seq index (black)
    sparseMatA2X(1)%factorIndexList(2,8)=8    ! dst seq index (black)
    sparseMatA2X(1)%factorIndexList(2,9)=9    ! dst seq index (black)

    ! Set up mapping from A2 -> X
    sparseMatA2X(2)%factorIndexList(1,1)=1    ! src seq index (red)
    sparseMatA2X(2)%factorIndexList(1,2)=2    ! src seq index (red)
    sparseMatA2X(2)%factorIndexList(1,3)=2    ! src seq index (red)

    sparseMatA2X(2)%factorIndexList(2,1)=10   ! dst seq index (black)
    sparseMatA2X(2)%factorIndexList(2,2)=11   ! dst seq index (black)
    sparseMatA2X(2)%factorIndexList(2,3)=12   ! dst seq index (black)
!EOC

!BOE
! Set up the mapping weights from side A to the XGrid:
!EOE
!BOC
    ! Note that the weights are dest area weighted, they are ratio
    ! of areas with destination area as the denominator.
    ! Set up mapping weights from A1 -> X
    sparseMatA2X(1)%factorList(:)=1.

    ! Set up mapping weights from A2 -> X
    sparseMatA2X(2)%factorList(:)=1.
!EOC

!BOE
! Set up the mapping indices and weights from the XGrid to B side:
!EOE
!BOC
    ! Set up mapping from X -> B
    sparseMatX2B(1)%factorIndexList(1,1)=1    ! src seq index (black)
    sparseMatX2B(1)%factorIndexList(1,2)=2    ! src seq index (black)
    sparseMatX2B(1)%factorIndexList(1,3)=3    ! src seq index (black)
    sparseMatX2B(1)%factorIndexList(1,4)=4    ! src seq index (black)
    sparseMatX2B(1)%factorIndexList(1,5)=5    ! src seq index (black)
    sparseMatX2B(1)%factorIndexList(1,6)=6    ! src seq index (black)
    sparseMatX2B(1)%factorIndexList(1,7)=7    ! src seq index (black)
    sparseMatX2B(1)%factorIndexList(1,8)=8    ! src seq index (black)
    sparseMatX2B(1)%factorIndexList(1,9)=9    ! src seq index (black)
    sparseMatX2B(1)%factorIndexList(1,10)=10  ! src seq index (black)
    sparseMatX2B(1)%factorIndexList(1,11)=11  ! src seq index (black)
    sparseMatX2B(1)%factorIndexList(1,12)=12  ! src seq index (black)

    sparseMatX2B(1)%factorIndexList(2,1)=1    ! dst seq index (blue)
    sparseMatX2B(1)%factorIndexList(2,2)=1    ! dst seq index (blue)
    sparseMatX2B(1)%factorIndexList(2,3)=2    ! dst seq index (blue)
    sparseMatX2B(1)%factorIndexList(2,4)=1    ! dst seq index (blue)
    sparseMatX2B(1)%factorIndexList(2,5)=1    ! dst seq index (blue)
    sparseMatX2B(1)%factorIndexList(2,6)=2    ! dst seq index (blue)
    sparseMatX2B(1)%factorIndexList(2,7)=3    ! dst seq index (blue)
    sparseMatX2B(1)%factorIndexList(2,8)=3    ! dst seq index (blue)
    sparseMatX2B(1)%factorIndexList(2,9)=4    ! dst seq index (blue)
    sparseMatX2B(1)%factorIndexList(2,10)=3   ! dst seq index (blue)
    sparseMatX2B(1)%factorIndexList(2,11)=3   ! dst seq index (blue)
    sparseMatX2B(1)%factorIndexList(2,12)=4   ! dst seq index (blue)

    ! Set up mapping weights from X -> B
    sparseMatX2B(1)%factorList(1)=4./9.
    sparseMatX2B(1)%factorList(2)=2./9.
    sparseMatX2B(1)%factorList(3)=2./3.
    sparseMatX2B(1)%factorList(4)=2./9.
    sparseMatX2B(1)%factorList(5)=1./9.
    sparseMatX2B(1)%factorList(6)=1./3.
    sparseMatX2B(1)%factorList(7)=2./9.
    sparseMatX2B(1)%factorList(8)=1./9.
    sparseMatX2B(1)%factorList(9)=1./3.
    sparseMatX2B(1)%factorList(10)=4./9.
    sparseMatX2B(1)%factorList(11)=2./9.
    sparseMatX2B(1)%factorList(12)=2./3.
!EOC

!BOE
! Optionally the area can be setup to compute surface area weighted flux integrals:
!EOE
!BOC
    ! Set up destination areas to adjust weighted flux
    xgrid_area(1) = 1.
    xgrid_area(2) = 0.5
    xgrid_area(3) = 0.5
    xgrid_area(4) = 0.5
    xgrid_area(5) = 0.25
    xgrid_area(6) = 0.25
    xgrid_area(7) = 0.5
    xgrid_area(8) = 0.25
    xgrid_area(9) = 0.25
    xgrid_area(10) = 1.
    xgrid_area(11) = 0.5
    xgrid_area(12) = 0.5
!EOC

    B_area(1,1) = 9./4
    B_area(2,1) = 3./4
    B_area(1,2) = 9./4
    B_area(2,2) = 3./4

!BOE
! Create an XGrid based on the user supplied regridding parameters:
!EOE
!BOC
    xgrid = ESMF_XGridCreateFromSparseMat(sideAGrid=sideA, &
        sideBGrid=sideB, area=xgrid_area, &
        centroid=centroid, sparseMatA2X=sparseMatA2X, &
        sparseMatX2B=sparseMatX2B, rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)

!BOE
! Create an {\tt ESMF\_Field} on the XGrid:
!EOE
!BOC
    field = ESMF_FieldCreate(xgrid, typekind=ESMF_TYPEKIND_R8, &
                rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOE
! Query the Field for its Fortran data pointer and its exclusive bounds:
!EOE
!BOC
    call ESMF_FieldGet(field, farrayPtr=xfarrayPtr, &
        exclusiveLBound=xlb, exclusiveUBound=xub, rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)

    xfarrayPtr = 0.0

!BOE
! Setup and initialize src and dst Fields on side A and side B Grids,
! source Fields have different source flux:
!EOE
!BOC
    do i = 1, 2
        srcField(i) = ESMF_FieldCreate(sideA(i), &
                typekind=ESMF_TYPEKIND_R8, rc=localrc)
!EOC
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
        call ESMF_FieldGet(srcField(i), farrayPtr=farrayPtr, rc=localrc)
!EOC
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
        farrayPtr = i
    enddo
    do i = 1, 1
        dstField(i) = ESMF_FieldCreate(sideB(i), &
                typekind=ESMF_TYPEKIND_R8, rc=localrc)
!EOC
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
        call ESMF_FieldGet(dstField(i), farrayPtr=farrayPtr, rc=localrc)
!EOC
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
        farrayPtr = 0.0
    enddo
!EOC

!BOE
!
! The current implementation requires that Grids used to generate the XGrid
! must not match, i.e. they are different either topologically or geometrically or both.
! In this example, the first source Grid is topologically identical to the destination
! Grid but their geometric coordinates are different. This requirement will be relaxed
! in a future release.
!
! First we compute the regrid routehandles, these routehandles can be used repeatedly
! afterwards. Then we initialize the values in the Fields. Finally we execute the Regrid.
!
!EOE
!BOC
    ! Compute regrid routehandles. The routehandles can be used
    ! repeatedly afterwards.
    ! From A -> X
    do i = 1, 2
        call ESMF_FieldRegridStore(xgrid, srcField(i), field, &
                routehandle=rh_src2xgrid(i), rc = localrc)
!EOC
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
    enddo
    ! from X -> B
    do i = 1, 1
        call ESMF_FieldRegridStore(xgrid, field, dstField(i), &
                routehandle=rh_xgrid2dst(i), rc = localrc)
!EOC
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
    enddo

    ! Initialize values in the source Fields on side A
    do i = 1, 2
        call ESMF_FieldGet(srcField(i), farrayPtr=farrayPtr, rc=localrc)
!EOC
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
        farrayPtr = i
    enddo
    ! Initialize values in the destination Field on XGrid
    xfarrayPtr = 0.0
    ! Initialize values in the destination Field on Side B
    do i = 1, 1
        call ESMF_FieldGet(dstField(i), farrayPtr=farrayPtr, rc=localrc)
!EOC
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
        farrayPtr = 0.0
    enddo
!EOC
!BOE
! First we regrid from the Fields on side A to the Field on the XGrid:
!EOE
!BOC
    ! Execute regrid from A -> X
    do i = 1, 2
        call ESMF_FieldRegrid(srcField(i), field, &
            routehandle=rh_src2xgrid(i), &
            zeroregion=ESMF_REGION_SELECT, rc = localrc)
!EOC
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
    enddo
!EOC

    ! xfarrayPtr should be all 1. at this point
    ! To get the surface integral of flux on XGrid, adjust by dst area

    !do i = xlb(1), xub(1)
    !    xfarrayPtr(i) = xfarrayPtr(i) * xgrid_area(i)
    !enddo

    print *, '- after SMM from A -> X'
    print *, xfarrayPtr ! should be xgrid_area

    print *, '- B before SMM from X -> B'
    print *, farrayPtr ! should be 0.
!BOE
! Next we regrid from the Field on XGrid to the destination Field on side B:
!EOE
!BOC
    ! Execute the regrid store
    do i = 1, 1
        call ESMF_FieldRegrid(field, dstField(i), &
            routehandle=rh_xgrid2dst(i), rc = localrc)
!EOC
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
    enddo
!EOC

    print *, '- B after SMM from X -> B'
    print *, farrayPtr ! should be 1/B_area

!BOE
! In the above example, we first set up all the required parameters to create an XGrid from user
! supplied input. Then we create Fields on the XGrid and the Grids on either side. Finally
! we use the {\tt ESMF\_FieldRegrid()} interface to perform a flux exchange from the source side
! to the destination side.
!EOE

!BOE
!\subsubsection{Query the XGrid for its internal information}
!\label{sec:xgrid:usage:xgrid_get}
! One can query the XGrid for its internal information:
!EOE
!BOC
    call ESMF_XGridGet(xgrid, &
        ngridA=ngridA, &    ! number of Grids on side A
        ngridB=ngridB, &    ! number of Grids on side B
        sideAGrid=l_sideA, &    ! list of Grids on side A
        sideBGrid=l_sideB, &    ! list of Grids on side B
        area=l_area, &      ! list of area of XGrid
        centroid=l_centroid, &  ! list of centroid of XGrid
        distgridA=l_sideAdg, &  ! list of Distgrids on side A
        distgridM = distgrid, & ! balanced distgrid
        sparseMatA2X=l_sparseMatA2X, & !sparse matrix matmul parameters A to X
        sparseMatX2B=l_sparseMatX2B, & !sparse matrix matmul parameters X to B
        rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)

!BOC
    call ESMF_XGridGet(xgrid, localDe=0, &
        elementCount=eleCount, &    ! elementCount on the localDE
        exclusiveCount=ec, &        ! exclusive count
        exclusiveLBound=elb, &      ! exclusive lower bound
        exclusiveUBound=eub, &      ! exclusive upper bound
        rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)

!BOC
    call ESMF_XGridGet(xgrid, &
        xgridSide=ESMF_XGRIDSIDE_A, & ! side of the XGrid to query
        gridIndex=1, &              ! index of the distgrid
        distgrid=distgrid, &        ! the distgrid returned
        rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
    call ESMF_XGridGet(xgrid, xgridSide=ESMF_XGRIDSIDE_A, gridIndex=2, &
        distgrid=distgrid, rc=localrc)
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
    call ESMF_XGridGet(xgrid, xgridSide=ESMF_XGRIDSIDE_B, gridIndex=1, &
        distgrid=distgrid, rc=localrc)
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)

!BOE
!\subsubsection{Destroying the XGrid and other resources}
!\label{sec:xgrid:usage:xgrid_destroy}
! Clean up the resources by destroying the XGrid and other objects:
!EOE
!BOC
    ! After the regridding is successful.
    ! Clean up all the allocated resources:
    call ESMF_FieldDestroy(field, rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)

!BOC
    call ESMF_XGridDestroy(xgrid, rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)

!BOC
    do i = 1, 2
        call ESMF_FieldDestroy(srcField(i), rc = localrc)
!EOC
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
        call ESMF_GridDestroy(sideA(i), rc = localrc)
!EOC
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
    enddo

    do i = 1, 1
        call ESMF_FieldDestroy(dstField(i), rc = localrc)
!EOC
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
        call ESMF_GridDestroy(sideB(i), rc = localrc)
!EOC
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
                endflag=ESMF_END_ABORT)
!BOC
    enddo

    deallocate(sparseMatA2X(1)%factorIndexList, sparseMatA2X(1)%factorList)
    deallocate(sparseMatA2X(2)%factorIndexList, sparseMatA2X(2)%factorList)
    deallocate(sparseMatX2B(1)%factorIndexList, sparseMatX2B(1)%factorList)
!EOC

    if(localrc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    print *, "Regridding through XGrid example returned"

    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
    ! file that the scripts grep for.
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
    call ESMF_Finalize(rc=localrc)
    if (localrc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!-------------------------------------------------------------------------

    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_XGridSparseMatEx.F90"
    else
        print *, "FAIL: ESMF_XGridSparseMatEx.F90"
    end if
end program ESMF_XGridSparseMatEx
