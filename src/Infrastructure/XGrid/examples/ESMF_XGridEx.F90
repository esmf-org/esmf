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
!
    program ESMF_XGridEx

!------------------------------------------------------------------------------
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================
! !PROGRAM: ESMF_XGridEx - XGrid Examples
!
! !DESCRIPTION:
!
! This program shows examples of XGrid creation and usage
!-----------------------------------------------------------------------------
#include "ESMF.h"
    ! ESMF Framework module
    use ESMF_TestMod
    use ESMF
    use ESMF_XGridMod
    use ESMF_XGridCreateMod
    implicit none
    
    ! Local variables
    integer                             :: localrc, i, j
    type(ESMF_XGrid)                    :: xgrid
    type(ESMF_Grid)                     :: sideA(2), sideB(1)
    type(ESMF_DistGrid)                 :: sideAdg(2), sideBdg(1), distgrid
    real(ESMF_KIND_R8)                  :: centroid(12,2), area(12)
    type(ESMF_XGridSpec)                :: sparseMatA2X(2), sparseMatX2B(1)

    type(ESMF_Grid)                     :: l_sideA(2), l_sideB(1)
    type(ESMF_DistGrid)                 :: l_sideAdg(2), l_sideBdg(1)
    type(ESMF_XGridSpec)                :: l_sparseMatA2X(2), l_sparseMatX2B(1)
    type(ESMF_Field)                    :: field, srcField(2), dstField(1)
    type(ESMF_Field)                    :: dstFrac, dstFrac2

    integer                             :: eleCount, ngridA, ngridB
    integer                             :: elb, eub, ec

    real(ESMF_KIND_R8), pointer         :: farrayPtr(:,:), xfarrayPtr(:)
    integer                             :: xlb(1), xub(1)
    type(ESMF_RouteHandle)              :: rh_src2xgrid(2), rh_xgrid2dst(1)

    real(ESMF_KIND_R8), pointer         :: coordX(:,:), coordY(:,:)

    integer                             :: finalrc, result
    character(ESMF_MAXSTR)              :: testname
    character(ESMF_MAXSTR)              :: failMsg
!   !Set finalrc to success
    finalrc = ESMF_SUCCESS

    write(failMsg, *) "Example failure"
    write(testname, *) "Example ESMF_XGridEx"

    call ESMF_Initialize(defaultlogfilename="XGridEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=localrc)
    if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create an XGrid from Grids then use it for regridding}
!\label{sec:xgrid:usage:xgrid_create}
!
! An {\tt ESMF\_XGrid} object can be created from Grids on either side
! of the exchange grid. Internally the
! weight matrices and index mapping are computed and stored in the XGrid, along
! with other necessary information for flux exchange calculation between
! any pair of model components used for the XGrid creation. 
! 
! In this example, we create an XGrid from overlapping Grids on
! either side of the XGrid. Then we perform a flux exchange from one side
! to the other side of the XGrid. 
!
! We start by creating the Grids on both sides and associate coordinates with
! the Grids on the corner stagger. The Grids use global indexing and padding 
! for coordinates on the corner stagger.
!
! For details of Grid creation and coordinate use, 
! please refer to Grid class documentation: \ref{example:2DRegUniGrid}.
!EOE
!BOC
    ! First Grid on side A
    sideA(1) = ESMF_GridCreateNoPeriDim(maxIndex=(/20, 20/), &
      indexflag=ESMF_INDEX_GLOBAL, &
      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/1,1/), &
      name='source Grid 1 on side A', rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

!BOC
    ! Second Grid on side A
    sideA(2) = ESMF_GridCreateNoPeriDim(maxIndex=(/20, 10/), &
      indexflag=ESMF_INDEX_GLOBAL, &
      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/1,1/), &
      name='source Grid 2 on side A', rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

!BOC
    ! Allocate coordinates for Grid corner stagger
    do i = 1, 2
      call ESMF_GridAddCoord(sideA(i), staggerloc=ESMF_STAGGERLOC_CORNER, &
          rc=localrc)
!EOC
      if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
          endflag=ESMF_END_ABORT)
!BOC
    enddo
!EOC

!BOE
! Assign coordinate for the Grids on sideA at corner stagger.
!EOE
!BOC
    ! SideA first grid spans (0-20, 0-20) with 1.0x1.0 degree resolution
    ! X corner
    call ESMF_GridGetCoord(sideA(1), localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
        farrayPtr=coordX, rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!BOC
    ! Y corner
    call ESMF_GridGetCoord(sideA(1), localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
        farrayPtr=coordY, rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!BOC
    do i = lbound(coordX,1), ubound(coordX,1)
      do j = lbound(coordX, 2), ubound(coordX, 2)
        coordX(i,j) = (i-1)*1.0
        coordY(i,j) = (j-1)*1.0
      enddo
    enddo
!EOC

!BOC
    ! SideA second grid spans (14.3-24.3, 14.2-24.2) with 0.5x1.0 degree 
    ! resolution X corner
    call ESMF_GridGetCoord(sideA(2), localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
        farrayPtr=coordX, rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!BOC
    ! Y corner
    call ESMF_GridGetCoord(sideA(2), localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
        farrayPtr=coordY, rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!BOC
    do i = lbound(coordX,1), ubound(coordX,1)
      do j = lbound(coordX, 2), ubound(coordX, 2)
        coordX(i,j) = 14.3+(i-1)*0.5
        coordY(i,j) = 14.2+(j-1)*1.0
      enddo
    enddo
!EOC

!BOE
! Create the destination grid on side B, only one Grid exists on side B. Also associate
! coordinate with the Grid:
!EOE
!BOC
    sideB(1) = ESMF_GridCreateNoPeriDim(maxIndex=(/30, 30/), &
      indexflag=ESMF_INDEX_GLOBAL, &
      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/1,1/), &
      name='source Grid 1 on side B', rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
		endflag=ESMF_END_ABORT)
!BOC
    do i = 1, 1
      call ESMF_GridAddCoord(sideB(i), staggerloc=ESMF_STAGGERLOC_CORNER, &
          rc=localrc)
!EOC
      if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
      endflag=ESMF_END_ABORT)
!BOC
    enddo
!EOC

!BOC
    ! SideB grid spans (0-30, 0-30) with 1.0x1.0 degree resolution
    ! X corner
    call ESMF_GridGetCoord(sideB(1), localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
        farrayPtr=coordX, rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!BOC
    ! Y corner
    call ESMF_GridGetCoord(sideB(1), localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
        farrayPtr=coordY, rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!BOC
    do i = lbound(coordX,1), ubound(coordX,1)
      do j = lbound(coordX, 2), ubound(coordX, 2)
        coordX(i,j) = (i-1)*1.0
        coordY(i,j) = (j-1)*1.0
      enddo
    enddo
!EOC

!BOE
! Create an {\tt ESMF\_XGrid} object from the two lists of Grids on side A and B.
! In this example both Grids on side A overlaps with the Grid on side B. It's an error to have a Grid
! on either side that is spatially disjoint with the XGrid. Neither of the Grid on side A is
! identical to the Grid on side B. Calling the {\tt ESMF\_XGridCreate()} method is straightforward:
!EOE
!BOC
    xgrid = ESMF_XGridCreate(sideAGrid=sideA, sideBGrid=sideB, rc=localrc)
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

!BOE
! Create src and dst Fields on side A and side B Grids.
!EOE
!BOC
    do i = 1, 2
        srcField(i) = ESMF_FieldCreate(sideA(i), &
                typekind=ESMF_TYPEKIND_R8, rc=localrc)
!EOC
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
          endflag=ESMF_END_ABORT)
!BOC
    enddo
    do i = 1, 1
        dstField(i) = ESMF_FieldCreate(sideB(i), &
                typekind=ESMF_TYPEKIND_R8, rc=localrc)
!EOC
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
          endflag=ESMF_END_ABORT)
!BOC
    enddo
!EOC
    dstFrac = ESMF_FieldCreate(sideB(1), &
      typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
      endflag=ESMF_END_ABORT)
    dstFrac2 = ESMF_FieldCreate(sideB(1), &
      typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
      endflag=ESMF_END_ABORT)


!BOE
!
! The current implementation requires that Grids used to generate the XGrid
! must not match, i.e. they are different either topologically or geometrically or both.
! In this example, the first source Grid is topologically identical to the destination
! Grid but their geometric coordinates are different. 
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
    ! from X -> B, retrieve the destination fraction Fields.
    do i = 1, 1
      call ESMF_FieldRegridStore(xgrid, field, dstField(i), &
        dstFracField=dstFrac, dstMergeFracField=dstFrac2, &
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

    !print *, '- after SMM from A -> X'
    !print *, xfarrayPtr ! should be xgrid_area

    !print *, '- B before SMM from X -> B'
    !print *, farrayPtr ! should be 0.
!BOE
! Next we regrid from the Field on XGrid to the destination Field on side B:
!EOE
!BOC
    ! Execute the regrid store
    do i = 1, 1
      call ESMF_FieldRegrid(field, dstField(i), &
        routehandle=rh_xgrid2dst(i), &
        rc = localrc)
!EOC
      if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
        endflag=ESMF_END_ABORT)
!BOC
    enddo
!EOC

    !print *, '- B after SMM from X -> B'
    !print *, farrayPtr ! should be 1/B_area

!BOE
! After the regridding calls, the routehandle can be released by calling the
! {\tt ESMF\_FieldRegridRelease()} method.
!EOE
!BOC
    do i = 1, 2
      call ESMF_FieldRegridRelease(routehandle=rh_src2xgrid(i), rc=localrc)
!EOC
      if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
        endflag=ESMF_END_ABORT)
!BOC
    enddo
    call ESMF_FieldRegridRelease(routehandle=rh_xgrid2dst(1), rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
      endflag=ESMF_END_ABORT)
!EOC

    call ESMF_XGridDestroy(xgrid, rc=localrc)
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
      endflag=ESMF_END_ABORT)

!BOE
! In the above example, we first set up all the required parameters to create an XGrid from user
! supplied input. Then we create Fields on the XGrid and the Grids on either side. Finally
! we use the {\tt ESMF\_FieldRegrid()} interface to perform a flux exchange from the source side
! to the destination side.
!EOE

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Using XGrid in Earth System modeling}
!\label{sec:xgrid:usage:xgrid_create_masking}
!
! A typical application in Earth System Modeling is to calculate flux exchange 
! through the planetary boundary layer that can be represented by {\tt ESMF\_XGrid}. 
! Atmosphere is above the planetary boundary layer while land and ocean are below the boundary layer.
! To create an XGrid, the land and ocean Grids that are usually different in resolution 
! need to be merged first to create a super Mesh. This merging process is enabled through the support
! of masking. 
!
! The global land and ocean Grids need to be created with masking enabled.
! In practice, each Grid cell has an integer masking value attached to it. For examples using masking in
! {\tt ESMF\_Grid} please refer to section \ref{sec:usage:items}.
!
! When calling the {\tt ESMF\_XGridCreate()} method, user can supply the optional arguments 
! sideAMaskValues and sideBMaskValues. 
! These arguments are one dimensional Fortran integer arrays. If any of the sideAMaskValues entry
! matches the masking value used in sideA Grid, the sideA Grid cell is masked out, vice versa for sideB.
! Thus by specifying different regions of a land and ocean Grids to be masked out, the two global Grids
! can be merged into a new global Mesh covering the entire Earth.
!
! The following call shows how to use the {\tt ESMF\_XGridCreate()} method with the optional 
! arguments sideAMaskValues and sideBMaskValues.
! 
!EOE

!BOC
    xgrid = ESMF_XGridCreate(sideAGrid=sideA, sideBGrid=sideB, &
      sideAMaskValues=(/2/), sideBMaskValues=(/3,4/), rc=localrc)
!EOC
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
      endflag=ESMF_END_ABORT)

    call ESMF_XGridGet(xgrid, &
      ngridA=ngridA, &    ! number of Grids on side A
      ngridB=ngridB, &    ! number of Grids on side B
      sideAGrid=l_sideA, &    ! list of Grids on side A
      sideBGrid=l_sideB, &    ! list of Grids on side B
      distgridA=l_sideAdg, &  ! list of Distgrids on side A
      distgridM = distgrid, & ! balanced distgrid
      sparseMatA2X=l_sparseMatA2X, & !sparse matrix matmul parameters A to X
      sparseMatX2B=l_sparseMatX2B, & !sparse matrix matmul parameters X to B
      rc=localrc)
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
      endflag=ESMF_END_ABORT)

    call ESMF_XGridGet(xgrid, localDe=0, &
      elementCount=eleCount, &    ! elementCount on the localDE
      exclusiveCount=ec, &        ! exclusive count
      exclusiveLBound=elb, &      ! exclusive lower bound
      exclusiveUBound=eub, &      ! exclusive upper bound
      rc=localrc)
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
      endflag=ESMF_END_ABORT)

    call ESMF_XGridGet(xgrid, &
      xgridSide=ESMF_XGRIDSIDE_A, & ! side of the XGrid to query
      gridIndex=1, &              ! index of the distgrid
      distgrid=distgrid, &        ! the distgrid returned
      rc=localrc)
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

    ! After the regridding is successful. 
    ! Clean up all the allocated resources:
    call ESMF_FieldDestroy(field, rc=localrc)
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
      endflag=ESMF_END_ABORT)

    call ESMF_XGridDestroy(xgrid, rc=localrc)
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
      endflag=ESMF_END_ABORT)

    do i = 1, 2
      call ESMF_FieldDestroy(srcField(i), rc = localrc)
      if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
        endflag=ESMF_END_ABORT)
      call ESMF_GridDestroy(sideA(i), rc = localrc)
      if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
        endflag=ESMF_END_ABORT)
    enddo

    do i = 1, 1
      call ESMF_FieldDestroy(dstField(i), rc = localrc)
      if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
        endflag=ESMF_END_ABORT)
      call ESMF_GridDestroy(sideB(i), rc = localrc)
      if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
        endflag=ESMF_END_ABORT)
    enddo
    call ESMF_FieldDestroy(dstFrac, rc = localrc)
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
      endflag=ESMF_END_ABORT)
    call ESMF_FieldDestroy(dstFrac2, rc = localrc)
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, &
      endflag=ESMF_END_ABORT)

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
        print *, "PASS: ESMF_XGridEx.F90"
    else
        print *, "FAIL: ESMF_XGridEx.F90"
    end if
end program ESMF_XGridEx
