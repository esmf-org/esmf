! $Id: ESMF_RegridConserv.F90,v 1.69 2007/06/22 23:21:39 cdeluca Exp $
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
!
#define ESMF_FILENAME "ESMF_RegridConserv.F90"
!
!     ESMF Conservative Regrid Module
      module ESMF_RegridConservMod
!
!==============================================================================
!
! This file contains the Regrid class methods for conservative regridding.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!==============================================================================
!BOPI
! !MODULE: ESMF_RegridConservMod - Conservative interpolation
!
! !DESCRIPTION:
!
! The code in this file implements the conservative methods for the ESMF Regrid
! class.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_BaseMod        ! ESMF base   class
      use ESMF_LocalArrayMod
      use ESMF_InternArrayDataMapMod
      use ESMF_InternArrayMod ! ESMF internal array  class
      use ESMF_InternArrayGetMod    ! ESMF array  class
      use ESMF_PhysCoordMod   ! ESMF physical interngrid domain class
      use ESMF_PhysGridMod    ! ESMF physical interngrid class
      use ESMF_InternGridMod        ! ESMF interngrid   class
      use ESMF_FieldDataMapMod
      use ESMF_FieldMod       ! ESMF field  class
      use ESMF_BundleMod      ! ESMF bundle class
      use ESMF_RegridTypesMod ! ESMF regrid data structures
      use ESMF_InitMacrosMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------

      ! TODO:  these should really be in Base or somewhere else
      real(ESMF_KIND_R8), parameter ::  pi            = 3.1416d0
      real(ESMF_KIND_R8), parameter ::  pi2           = 2.0d0 * pi
      real(ESMF_KIND_R8), parameter ::  offset        = 1.0d-12
      real(ESMF_KIND_R8), parameter :: northThreshold =  1.48d0
      real(ESMF_KIND_R8), parameter :: southThreshold = -1.48d0
                            ! threshold latitude above/below which to use polar
                            ! transformation for finding intersections in
                            ! spherical coordinates

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

    public ESMF_RegridConstructConserv ! create and fill a regrid object
                                       ! for a conservative regridding

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:
!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RegridConserv.F90,v 1.69 2007/06/22 23:21:39 cdeluca Exp $'

!==============================================================================

      contains

!==============================================================================
!
! This section includes the conservative Regrid construct methods.
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridConstructConserv"
!BOPI
! !IROUTINE: ESMF_RegridConstructConserv - Constructs conservative Regrid structure 

! !INTERFACE:
     subroutine ESMF_RegridConstructConserv(rh, &
                                            srcArray, srcInternGrid, srcDataMap, &
                                            hasSrcData, &
                                            dstArray, dstInternGrid, dstDataMap, &
                                            hasDstData, &
                                            parentVM, routeIndex, &
                                            srcMask, dstMask, &
                                            regridnorm, order, rc)
!
! !ARGUMENTS:
      type(ESMF_Routehandle),  intent(inout) :: rh
      type(ESMF_InternArray),        intent(in ) :: srcArray
      type(ESMF_InternGrid),         intent(inout) :: srcInternGrid
      type(ESMF_FieldDataMap), intent(inout) :: srcDataMap
      logical,                 intent(inout) :: hasSrcData
      type(ESMF_InternArray),        intent(in ) :: dstArray
      type(ESMF_InternGrid),         intent(inout) :: dstInternGrid
      type(ESMF_FieldDataMap), intent(inout) :: dstDataMap
      logical,                 intent(inout) :: hasDstData
      type(ESMF_VM),           intent(in ) :: parentVM
      integer,                 intent(in ) :: routeIndex
      type(ESMF_Mask),         intent(in ), optional :: srcMask
      type(ESMF_Mask),         intent(in ), optional :: dstMask
      type(ESMF_RegridNormOpt),intent(in ), optional :: regridnorm
      integer,                 intent(in ), optional :: order
      integer,                 intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a source array and destination array (and their related
!     interngrids and datamaps), this routine constructs a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     field to the destination field using a conservative interpolation.  
!     Returns a pointer to a new {\tt Regrid}.  This routine implements
!     the method described in Jones, P.W., 1999: First- and Second-order
!     Conservative Remapping Schemes for InternGrids in Spherical Coordinates,
!     Mon. Weath. Rev., 127, 2204-2210.  This method was extended from
!     an original method in Cartesian coordinates and this routine
!     incorporates the Cartesian implementation as well as the spherical
!     coordinate implementation.
!
!     The arguments are:
!     \begin{description}
!     \item[srcArray]
!          {\tt ESMF\_Array} to be regridded.
!     \item[srcInternGrid]
!          {\tt ESMF\_InternGrid} corresponding to [srcArray].
!     \item[srcDataMap]
!          {\tt ESMF\_DataMap} corresponding to [srcArray].
!     \item[dstArray]
!          Resultant {\tt ESMF\_Array} where regridded source array will be
!          stored.
!     \item[dstInternGrid]
!          {\tt ESMF\_InternGrid} corresponding to [dstArray].
!     \item[dstDataMap]
!          {\tt ESMF\_DataMap} corresponding to [dstArray].
!     \item[{[srcMask]}]
!          Optional mask to specify or eliminate source points from
!          regridding.  Default is that all source points participate. 
!     \item[{[dstMask]}]
!          Optional mask to specify or eliminate destination points from
!          regridding.  Default is that all destination points participate. 
!     \item[{[regridnorm]}]
!          Optional normalization option.  
!          Default is {\tt ESMF\_REGRID\_NORM\_FRACAREA}.
!     \item[{[order]}]
!          Optional integer to set conservation order.  The default is 1,
!          for first-order.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      ! local variables
      integer :: localrc           ! Error status
      integer :: &
           aSize,                 &!
           nC,                    &!
           dataRank,              &!
           haloWidth,             &!
           orderUse,              &!
           i,                     &! loop counter
           numDstCorners,         &!
           numSrcCorners           !
      integer, dimension(2) :: &
           srcIndexMod, dstIndexMod !
      integer, dimension(3) :: &
           srcOrder, dstOrder,    &!
           srcCounts, dstCounts    !
      integer, dimension(:), allocatable :: &
           dataOrder, lbounds
      integer(ESMF_KIND_I4), dimension(:), pointer :: &
           srcGatheredMask         !
      integer(ESMF_KIND_I4), dimension(:,:), pointer :: &
           srcLocalMask            !
      logical, dimension(:), pointer :: &
           srcUserMask, dstUserMask
      real(ESMF_KIND_R8), dimension(:), pointer :: &
           srcGatheredCoordX,     &!
           srcGatheredCoordY,     &!
           temp1d                  !
      real(ESMF_KIND_R8), dimension(:), allocatable :: &
           srcArea,               &! src cell area computed during regrid create
           srcUsrArea,            &! src cell area supplied by user through interngrid
           srcFracArea,           &! fractional area of src cell overlapping src
           srcCentroidX,          &! area-weighted cell coord in x
           srcCentroidY            ! area-weighted cell coord in y
      real(ESMF_KIND_R8), dimension(:,:), pointer :: &
           dstLocalCoordX,        &!
           dstLocalCoordY,        &!
           srcGatheredCornerX,    &!
           srcGatheredCornerY,    &!
           srcLocalCoordX,        &!
           srcLocalCoordY,        &!
           temp2d                  !
      real(ESMF_KIND_R8), dimension(:,:), allocatable :: &
           dstArea,               &! dst cell area computed during regrid create
           dstUsrArea,            &! dst cell area supplied by user through interngrid
           dstFracArea,           &! fractional area of dst cell overlapping src
           dstCentroidX,          &! area-weighted cell coord in x
           dstCentroidY            ! area-weighted cell coord in y
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: &
           dstLocalCornerX,       &!
           dstLocalCornerY,       &!
           srcLocalCornerX,       &!
           srcLocalCornerY
      type(ESMF_InternArray) :: &
           srcMaskArray            !
      type(ESMF_CoordSystem) :: &
           coordSystem             !
      type(ESMF_DomainList) :: &
           recvDomainList          !
! TODO: currently the ESMF_Regrid object is not used anywhere, so all references
!       are commented out
!     type(ESMF_Regrid) :: &
!          tempRegrid              !
      type(ESMF_RegridNormOpt) :: &
           regridNormUse           !
      type(ESMF_RelLoc) :: &
           srcRelLoc, dstRelLoc    !
      type(ESMF_Route) :: &
           route, tempRoute        !
      type(ESMF_TransformValues) :: tv
      type(ESMF_InternArray), dimension(:), pointer :: &
           dstLocalCoordArray,    &!
           dstLocalCornerArray,   &!
           srcLocalCoordArray,    &!
           srcLocalCornerArray     !


      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,srcDataMap)
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,dstDataMap)
      ESMF_INIT_CHECK_SHALLOW(ESMF_DomainListGetInit,ESMF_DomainListInit,recvDomainList)
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,srcInternGrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,dstInternGrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,parentVM,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,rh,rc)

      ! Set optional parameters if present - otherwise set defaults
      orderUse      = 1
      regridNormUse = ESMF_REGRID_NORM_FRACAREA
      if (present(order     )) orderUse      = order
      if (present(regridnorm)) regridNormUse = regridnorm

! TODO: remove this code.  rh is created above and passed down now.
      !! Construct an empty routehandle structure
      !rh = ESMF_RouteHandleCreate(rc=localrc)
      !if (ESMF_LogMsgFoundError(localrc, &
      !                          ESMF_ERR_PASSTHRU, &
      !                          ESMF_CONTEXT, rc)) return

! TODO: currently the ESMF_Regrid object is not used anywhere, so all references
!       are commented out
!     ! Construct an empty regrid structure
!     tempRegrid = ESMF_RegridCreateEmpty(rc=localrc)
!     if (ESMF_LogMsgFoundError(localrc, &
!                               ESMF_ERR_PASSTHRU, &
!                               ESMF_CONTEXT, rc)) return

!     ! Set regrid method and array pointers       TODO: add name
!     if (orderUse.eq.1) then
!       call ESMF_RegridSet(tempRegrid, &
!                           srcArray=srcArray, dstArray=dstArray, &
!                           method = ESMF_REGRID_METHOD_CONSERV1, rc=localrc)
!     else
!       call ESMF_RegridSet(tempRegrid, &
!                           srcArray=srcArray, dstArray=dstArray, &
!                           method = ESMF_REGRID_METHOD_CONSERV2, rc=localrc)
!     endif
!     if (ESMF_LogMsgFoundError(localrc, &
!                               ESMF_ERR_PASSTHRU, &
!                               ESMF_CONTEXT, rc)) return

      ! get dataRank and allocate rank-sized arrays
      call ESMF_InternArrayGet(srcArray, rank=dataRank, rc=localrc)
      allocate(dataOrder(dataRank), &
                 lbounds(dataRank), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "dataRank arrays", &
                                     ESMF_CONTEXT, rc)) return

      ! set reordering information
      srcOrder(:) = interngridOrder(:,srcInternGrid%ptr%coordOrder%order,2)
      dstOrder(:) = interngridOrder(:,dstInternGrid%ptr%coordOrder%order,2)

      ! get destination interngrid info
      !TODO: Get interngrid masks?
      call ESMF_FieldDataMapGet(dstDataMap, horzRelloc=dstRelLoc, &
                                dataIndexList=dataOrder, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      dstCounts(3) = 2
      call ESMF_InternGridGetDELocalInfo(dstInternGrid, horzRelLoc=dstRelLoc, &
                                   localCellCountPerDim=dstCounts(1:2), &
                                   reorder=.false., rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (hasDstData) then
        allocate(dstLocalCoordArray (2), &
                 dstLocalCornerArray(2), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "dst local arrays", &
                                       ESMF_CONTEXT, rc)) return

        call ESMF_InternGridGetCoord(dstInternGrid, horzRelLoc=dstRelLoc, &
                               centerCoord=dstLocalCoordArray, &
                               cornerCoord=dstLocalCornerArray, &
                               reorder=.false., rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        call ESMF_InternArrayGetData(dstLocalCoordArray(1), dstLocalCoordX, &
                               ESMF_DATA_COPY, localrc)
        call ESMF_InternArrayGetData(dstLocalCoordArray(2), dstLocalCoordY, &
                               ESMF_DATA_COPY, localrc)
        call ESMF_InternArrayGetData(dstLocalCornerArray(1), dstLocalCornerX, &
                               ESMF_DATA_COPY, localrc)
        call ESMF_InternArrayGetData(dstLocalCornerArray(2), dstLocalCornerY, &
                               ESMF_DATA_COPY, localrc)
      endif

      ! get source interngrid info
      call ESMF_FieldDataMapGet(srcDataMap, horzRelloc=srcRelLoc, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_InternGridGet(srcInternGrid, horzCoordSystem=coordSystem, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      srcCounts(3) = 2
      call ESMF_InternGridGetDELocalInfo(srcInternGrid, horzRelLoc=srcRelLoc, &
                                   localCellCountPerDim=srcCounts(1:2), &
                                   reorder=.false., rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (hasSrcData) then
        allocate(srcLocalCoordArray (2), &
                 srcLocalCornerArray(2), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "src local arrays", &
                                       ESMF_CONTEXT, rc)) return

        call ESMF_InternGridGetCoord(srcInternGrid, horzRelLoc=srcRelLoc, &
                               centerCoord=srcLocalCoordArray, &
                               cornerCoord=srcLocalCornerArray, &
                               reorder=.false., rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        call ESMF_InternArrayGetData(srcLocalCoordArray(1), srcLocalCoordX, &
                               ESMF_DATA_REF, localrc)
        call ESMF_InternArrayGetData(srcLocalCoordArray(2), srcLocalCoordY, &
                               ESMF_DATA_REF, localrc)
        call ESMF_InternArrayGetData(srcLocalCornerArray(1), srcLocalCornerX, &
                               ESMF_DATA_REF, localrc)
        call ESMF_InternArrayGetData(srcLocalCornerArray(2), srcLocalCornerY, &
                               ESMF_DATA_REF, localrc)
        call ESMF_InternGridGetCellMask(srcInternGrid, srcMaskArray, relloc=srcRelLoc, &
                                  rc=localrc)
        call ESMF_InternArrayGetData(srcMaskArray, srcLocalMask, ESMF_DATA_REF, &
                               localrc)
      endif

      ! Calculate two separate Routes:
      !    the first will be used in the code to gather the data for running
      !              the regrid, saved in routehandle
      !    the second sends and receives cell coordinate information and
      !              is used internal to this routine to get coordinate 
      !              information locally to calculate the regrid weights

      route = ESMF_RegridRouteConstruct(dataRank, srcInternGrid, dstInternGrid, &
                         recvDomainList, parentVM, &
                         srcArray=srcArray, srcDataMap=srcDataMap, &
                         dstArray=dstArray, dstDataMap=dstDataMap, &
                         hasSrcData=hasSrcData, hasDstData=hasDstData, &
                         total=.false., rc=localrc)
      call ESMF_RouteHandleSet(rh, which_route=routeIndex, &
                               route=route, rc=localrc)

      tempRoute = ESMF_RegridRouteConstruct(2, srcInternGrid, dstInternGrid, &
                             recvDomainList, parentVM, &
                             srcDataMap=srcDataMap, dstDataMap=dstDataMap, &
                             hasSrcData=hasSrcData, hasDstData=hasDstData, &
                             reorder=.false., total=.false., &
                             rc=localrc)

      ! Now use temporary route to gather necessary coordinates
      ! Create arrays for gathered coordinates 
      if (hasSrcData) nC = size(srcLocalCornerX,1)
      if (hasDstData) nC = size(dstLocalCornerX,1)
                                   !TODO: number if corners for the destination
                                   !      interngrid is OK for now, but should be a
                                   !      call to the source interngrid eventually
      if (hasDstData) then
        call ESMF_RouteGetRecvItems(tempRoute, aSize, localrc)
        allocate(srcGatheredCoordX (   aSize), &
                 srcGatheredCoordY (   aSize), &
                 srcGatheredMask   (   aSize), &
                 srcGatheredCornerX(nC,aSize), &
                 srcGatheredCornerY(nC,aSize), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "src gathered arrays", &
                                       ESMF_CONTEXT, rc)) return
      endif

      ! Execute Route now to gather interngrid center coordinates from source
      ! These arrays are just wrappers for the local coordinate data
      call ESMF_RouteRunF90PtrR821D(tempRoute, srcLocalCoordX, &
                                    srcGatheredCoordX, localrc)
      call ESMF_RouteRunF90PtrR821D(tempRoute, srcLocalCoordY, &
                                    srcGatheredCoordY, localrc)
      ! TODO: move this loop to a Route routine?
      do i = 1,nC
        nullify(temp1d)
        nullify(temp2d)
        if (hasDstData) temp1d => srcGatheredCornerX(i,:)
        if (hasSrcData) temp2d => srcLocalCornerX(i,:,:)
        call ESMF_RouteRunF90PtrR821D(tempRoute, temp2d, temp1d, localrc)
        nullify(temp1d)
        nullify(temp2d)
        if (hasDstData) temp1d => srcGatheredCornerY(i,:)
        if (hasSrcData) temp2d => srcLocalCornerY(i,:,:)
        call ESMF_RouteRunF90PtrR821D(tempRoute, temp2d, temp1d, localrc)
      enddo
      call ESMF_RouteRunF90PtrI421D(tempRoute, srcLocalMask, &
                                    srcGatheredMask, localrc)

      ! now all necessary data is local

      ! TODO: the *6 is to guarantee the max allocation possible is enough
      !  for conservative interpolation.  eventually the addlinks routine should
      !  grow the arrays internally.
      if (hasDstData) then
        aSize = ((dstCounts(1)*dstCounts(2)) + 1) * 10
        ! Create a Transform Values object
        tv = ESMF_TransformValuesCreate(aSize, rc)

        ! set up user masks for search    ! TODO: combine user and interngrid masks
        if (present(dstMask)) then
  !        dstUserMask = dstMask
        else
          allocate(dstUserMask(dstCounts(1)*dstCounts(2)), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "dstUsermask", &
                                         ESMF_CONTEXT, rc)) return
          dstUserMask = .TRUE.
        endif
        if (present(srcMask)) then
  !        srcUserMask = srcMask
        else
          call ESMF_RouteGetRecvItems(tempRoute, aSize, localrc)
          allocate(srcUserMask(aSize), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "srcUsermask", &
                                         ESMF_CONTEXT, rc)) return
          srcUserMask = .TRUE.
        endif
     
        ! allocate various local dst interngrid arrays
        allocate(dstArea    (dstCounts(1),dstCounts(2)), &
                 dstUsrArea (dstCounts(1),dstCounts(2)), &
                 dstFracArea(dstCounts(1),dstCounts(2)), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "dst area arrays", &
                                       ESMF_CONTEXT, rc)) return

    !    dstUsrArea  (:) = ?       ! should come from user

        ! For spherical coordinates, convert all coordinates to radians
        if (coordSystem .eq. ESMF_COORD_SYSTEM_SPHERICAL) then
          dstLocalCoordX  =  dstLocalCoordX*pi/180.0d0
          dstLocalCoordY  =  dstLocalCoordY*pi/180.0d0
          dstLocalCornerX = dstLocalCornerX*pi/180.0d0
          dstLocalCornerY = dstLocalCornerY*pi/180.0d0
        ! convert destination longitudes to 0,2pi interval
          where (dstLocalCoordX < 0.0d0)  dstLocalCoordX =  &
                                        modulo( dstLocalCoordX, -pi2 ) + pi2
          dstLocalCoordX = modulo( dstLocalCoordX, pi2 )
        endif

        ! These are computed later - initialize to zero
        dstArea    (:,:) = 0.0d0
        dstFracArea(:,:) = 0.0d0

        if (order.gt.1) then
          allocate(dstCentroidX(dstCounts(1),dstCounts(2)), &
                   dstCentroidY(dstCounts(1),dstCounts(2)), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "dst centroid arrays", &
                                         ESMF_CONTEXT, rc)) return
          dstCentroidX(:,:) = 0.0d0
          dstCentroidY(:,:) = 0.0d0
        endif
        allocate(srcArea    (aSize), &
                 srcUsrArea (aSize), &
                 srcFracArea(aSize), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "src area arrays", &
                                       ESMF_CONTEXT, rc)) return

   !     srcUsrArea  (:) = ?

        ! For spherical coordinates, convert all coordinates to radians
        if (coordSystem .eq. ESMF_COORD_SYSTEM_SPHERICAL) then
          srcGatheredCoordX  =  srcGatheredCoordX*pi/180.0d0
          srcGatheredCoordY  =  srcGatheredCoordY*pi/180.0d0
          srcGatheredCornerX = srcGatheredCornerX*pi/180.0d0
          srcGatheredCornerY = srcGatheredCornerY*pi/180.0d0
        ! convert destination longitudes to 0,2pi interval
          where (srcGatheredCoordX < 0.0d0)  srcGatheredCoordX =  &
                                        modulo( srcGatheredCoordX, -pi2 ) + pi2
          srcGatheredCoordX = modulo( srcGatheredCoordX, pi2 )
        endif

        ! These are computed later - initialize to zero here
        srcArea    (:) = 0.0d0
        srcFracArea(:) = 0.0d0

        if (order.gt.1) then
          allocate(srcCentroidX(aSize), &
                   srcCentroidY(aSize), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "src centroid arrays", &
                                         ESMF_CONTEXT, rc)) return
          srcCentroidX(:) = 0.0d0
          srcCentroidY(:) = 0.0d0
        endif

        ! Loop through domains for the search routine
        call ESMF_InternGridGet(srcInternGrid, horzCoordSystem=coordSystem, rc=localrc)
 !       numSrcCorners = size(srcLocalCornerX,1)   ! TODO: should be from a InternGrid call
        numSrcCorners = size(dstLocalCornerX,1)   ! TODO: should be from a InternGrid call
        numDstCorners = size(dstLocalCornerX,1)

        ! calculate the offsets due to haloWidths
        call ESMF_InternArrayGet(dstArray, haloWidth=haloWidth, lbounds=lbounds, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        dstIndexMod = 0
        srcIndexMod = 0
        do i = 1,dataRank
          if (dataOrder(i).eq.1) dstIndexMod(1) = haloWidth + lbounds(i) - 1
          if (dataOrder(i).eq.2) dstIndexMod(2) = haloWidth + lbounds(i) - 1
        enddo

        if (order.eq.1) then
          call ESMF_RegridConservSearch(tv, orderUse, coordSystem, &
                                        aSize, numSrcCorners, &
                                        dstCounts(1), dstCounts(2), &
                                        numDstCorners, &
                                        srcIndexMod, srcOrder, &
                                        dstIndexMod, dstOrder, &
                                        dstLocalCoordX, dstLocalCoordY, &
                                        dstArea, dstUsrArea, dstFracArea, &
                                        dstUserMask, &
                                        dstLocalCornerX, dstLocalCornerY, &
                                        srcGatheredCoordX, srcGatheredCoordY, &
                                        srcArea, srcUsrArea, srcFracArea, &
                                        srcUserMask, &
                                        srcGatheredCornerX, srcGatheredCornerY, &
                                        rc=localrc)
          call ESMF_RegridConservNormalize(tv, orderUse, regridNormUse, &
                                        aSize, dstCounts(1), dstCounts(2), &
                                        srcIndexMod, srcOrder, &
                                        dstIndexMod, dstOrder, &
                                        dstArea, dstUsrArea, dstFracArea, &
                                        srcArea, srcUsrArea, srcFracArea, &
                                        rc=localrc)
        else
          call ESMF_RegridConservSearch(tv, orderUse, coordSystem, &
                                        aSize, numSrcCorners, &
                                        dstCounts(1), dstCounts(2), &
                                        numDstCorners, &
                                        srcIndexMod, srcOrder, &
                                        dstIndexMod, dstOrder, &
                                        dstLocalCoordX, dstLocalCoordY, &
                                        dstArea, dstUsrArea, dstFracArea, &
                                        dstUserMask, &
                                        dstLocalCornerX, dstLocalCornerY, &
                                        srcGatheredCoordX, srcGatheredCoordY, &
                                        srcArea, srcUsrArea, srcFracArea, &
                                        srcUserMask, &
                                        srcGatheredCornerX, srcGatheredCornerY, &
                                        dstCentroidX, dstCentroidY, &
                                        srcCentroidX, srcCentroidY, localrc)
          call ESMF_RegridConservNormalize(tv, orderUse, regridNormUse, &
                                        aSize, dstCounts(1), dstCounts(2), &
                                        srcIndexMod, srcOrder, &
                                        dstIndexMod, dstOrder, &
                                        dstArea, dstUsrArea, dstFracArea, &
                                        srcArea, srcUsrArea, srcFracArea, &
                                        dstCentroidX, dstCentroidY, &
                                        srcCentroidX, srcCentroidY, localrc)
        endif

      endif   ! hasDstData

      ! Set the routehandle
      call ESMF_RouteHandleSet(rh, which_tv=routeIndex, tdata=tv, rc=localrc)

      ! Clean up some allocatables and return
      call ESMF_RouteDestroy(tempRoute, localrc)
      deallocate(dataOrder, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(  lbounds, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                     ESMF_CONTEXT, rc)) return
      if (hasSrcData) then
        deallocate( srcLocalCoordArray, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(srcLocalCornerArray, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                       ESMF_CONTEXT, rc)) return
      endif
      if (hasDstData) then
        deallocate(            dstArea, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(         dstUsrArea, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(        dstFracArea, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(            srcArea, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(         srcUsrArea, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(        srcFracArea, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(  srcGatheredCoordX, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(  srcGatheredCoordY, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(    srcGatheredMask, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                       ESMF_CONTEXT, rc)) return
        deallocate( srcGatheredCornerX, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                       ESMF_CONTEXT, rc)) return
        deallocate( srcGatheredCornerY, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                       ESMF_CONTEXT, rc)) return
        deallocate( dstLocalCoordArray, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(dstLocalCornerArray, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(        dstUserMask, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(        srcUserMask, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                       ESMF_CONTEXT, rc)) return
        if (order.gt.1) then
          deallocate(     dstCentroidX, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                         ESMF_CONTEXT, rc)) return
          deallocate(     dstCentroidY, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                         ESMF_CONTEXT, rc)) return
          deallocate(     srcCentroidX, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                         ESMF_CONTEXT, rc)) return
          deallocate(     srcCentroidY, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                         ESMF_CONTEXT, rc)) return
        endif
      endif
      
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridConstructConserv

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridConservSearch"
!BOPI
! !IROUTINE: ESMF_RegridConservSearch - Searches a conservative Regrid structure

! !INTERFACE:
      subroutine ESMF_RegridConservSearch(tv, order, coordSystem, &
                                          srcSize, numSrcCorners, &
                                          dstSizeX, dstSizeY, numDstCorners, &
                                          srcIndexMod, srcOrder, &
                                          dstIndexMod, dstOrder, &
                                          dstCenterX, dstCenterY, &
                                          dstArea, dstUsrArea, dstFracArea, &
                                          dstMask, dstCornerX, dstCornerY, &
                                          srcCenterX, srcCenterY, &
                                          srcArea, srcUsrArea, srcFracArea, &
                                          srcMask, srcCornerX, srcCornerY, &
                                          dstCentroidX, dstCentroidY, &
                                          srcCentroidX, srcCentroidY, rc)
!
! !ARGUMENTS:
      type(ESMF_TransformValues), intent(inout) :: tv
      integer, intent(in) :: order
      type(ESMF_CoordSystem), intent(in) :: coordSystem
      integer, intent(in) :: srcSize
      integer, intent(in) :: numSrcCorners
      integer, intent(in) :: dstSizeX  ! in the lines below.
      integer, intent(in) :: dstSizeY
      integer, intent(in) :: numDstCorners
      integer, dimension(:), intent(in) :: srcIndexMod
      integer, dimension(:), intent(in) :: srcOrder
      integer, dimension(:), intent(in) :: dstIndexMod
      integer, dimension(:), intent(in) :: dstOrder
      real(ESMF_KIND_R8), dimension(dstSizeX,dstSizeY), intent(inout) :: dstCenterX
      real(ESMF_KIND_R8), dimension(dstSizeX,dstSizeY), intent(inout) :: dstCenterY
      real(ESMF_KIND_R8), dimension(dstSizeX,dstSizeY), intent(inout) :: dstArea
      real(ESMF_KIND_R8), dimension(dstSizeX,dstSizeY), intent(inout) :: dstUsrArea
      real(ESMF_KIND_R8), dimension(dstSizeX,dstSizeY), intent(inout) :: dstFracArea
      logical,            dimension(dstSizeX,dstSizeY), intent(in)    :: dstMask
      real(ESMF_KIND_R8), dimension(numDstCorners,dstSizeX,dstSizeY), &
                                                        intent(inout) :: dstCornerX
      real(ESMF_KIND_R8), dimension(numDstCorners,dstSizeX,dstSizeY), &
                                                        intent(inout) :: dstCornerY
      real(ESMF_KIND_R8), dimension(srcSize), intent(inout) :: srcCenterX
      real(ESMF_KIND_R8), dimension(srcSize), intent(inout) :: srcCenterY
      real(ESMF_KIND_R8), dimension(srcSize), intent(inout) :: srcArea
      real(ESMF_KIND_R8), dimension(srcSize), intent(inout) :: srcUsrArea
      real(ESMF_KIND_R8), dimension(srcSize), intent(inout) :: srcFracArea
      logical,            dimension(srcSize), intent(in)    :: srcMask
      real(ESMF_KIND_R8), dimension(numSrcCorners,srcSize), &
                                                        intent(inout) :: srcCornerX
      real(ESMF_KIND_R8), dimension(numSrcCorners,srcSize), &
                                                        intent(inout) :: srcCornerY
      real(ESMF_KIND_R8), dimension(dstSizeX,dstSizeY), intent(inout), &
                                                        optional      :: dstCentroidX
      real(ESMF_KIND_R8), dimension(dstSizeX,dstSizeY), intent(inout), &
                                                        optional      :: dstCentroidY
      real(ESMF_KIND_R8), dimension(srcSize), intent(inout), &
                                                       optional      :: srcCentroidX
      real(ESMF_KIND_R8), dimension(srcSize), intent(inout), &
                                                        optional      :: srcCentroidY
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a source field and destination field (and their attached
!     interngrids), this routine constructs a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     field to the destination field using a conservative interpolation.
!     Returns a pointer to a new {\tt Regrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[srcArray]
!          Field to be regridded.
!     \item[dstArray]
!          Resultant field where regridded source field will be stored.
! \item[TODO:]  make match actual arglist
!     \item[name]
!          {\tt Regrid}name.
!     \item[srcCornerX]
!          X cell corner coords for gathered source interngrid cells that potentially
!          overlap destination domain.
!     \item[srcCornerY]
!          Y cell corner coords for gathered source interngrid cells that potentially
!          overlap destination domain.
!     \item[dstCornerX]
!          X cell corner coords for gathered destination interngrid cells.
!     \item[dstCornerY]
!          Y cell corner coords for gathered destination interngrid cells.
!     \item[srcMask]
!          Optional user-defined mask to specify or eliminate source points from
!          regridding.  Default is that all source points participate.
!     \item[dstUserMmask]
!          Optional user-defined mask to specify or eliminate destination points
!          from regridding.  Default is that all destination points participate.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      integer :: localrc                          ! Error status
      integer ::           &
         iDst, jDst,       &! more loop counters
         ibDst, ieDst,     &! beg, end of exclusive domain in i-dir of dest interngrid
         jbDst, jeDst,     &! beg, end of exclusive domain in j-dir of dest interngrid
         ibSrc, ieSrc       ! beg, end of exclusive domain in i-dir of source interngrid

      integer :: srcAdd,    &! address in gathered source interngrid
                 dstAdd(2)   ! address in dest interngrid
         
      integer(ESMF_KIND_I4), parameter :: maxSubseg = 10000
                                 ! max number of subsegments per segment
                                 ! to prevent infinite loop
      integer(ESMF_KIND_I4), parameter :: targetBinSize = 250

      integer :: corner          ! corner of cell that segment starts from
      integer :: nextCorner      ! corner of cell that segment ends on
      integer :: numSubseg       ! number of subsegments
      integer :: startCorner

      integer :: ij, n, nDst
      integer :: minAddr, maxAddr, numBins(2), numSearchCells, searchAdd
      integer, dimension(:), allocatable :: searchAddList
      integer, dimension(:), pointer :: binAddrMin, binAddrMax
      logical, dimension(:), allocatable :: searchMask, searchTmpMask
      real(ESMF_KIND_R8), dimension(:), allocatable :: searchCenterX, &
                                                       searchCenterY
      real(ESMF_KIND_R8), dimension(:,:), allocatable :: searchCornerX, &
                                                         searchCornerY
      real(ESMF_KIND_R8), dimension(:,:), pointer :: binMin, binMax

      logical :: dummy
      logical :: coinc    ! flag for coincident segments
      logical :: reverse  ! flag for reversing direction of segment

      real(ESMF_KIND_R8) :: xIntersect      ! coordinates of intersection
      real(ESMF_KIND_R8) :: yIntersect      ! coordinates of intersection
      real(ESMF_KIND_R8) :: xIntersectLast  ! coordinates of last intersection
      real(ESMF_KIND_R8) :: yIntersectLast  !
      real(ESMF_KIND_R8) :: xbeg, xend      ! endpoints of current segment
      real(ESMF_KIND_R8) :: ybeg, yend      !
      real(ESMF_KIND_R8) :: xref, yref      ! reference x,y for current cell
      real(ESMF_KIND_R8) :: xoff, yoff      ! offsets to nudge past intersection
      real(ESMF_KIND_R8) :: srcMinX, srcMinY, srcMaxX, srcMaxY
      real(ESMF_KIND_R8) :: dstMinX, dstMinY, dstMaxX, dstMaxY

      real(ESMF_KIND_R8), dimension(4) :: fullLine
                                ! coordinates for full cell side line segment
      real(ESMF_KIND_R8), dimension(:), allocatable :: weights
                                ! local regridding weight array
      real(ESMF_KIND_R8), dimension(:), allocatable :: cornerX, cornerY

      type(ESMF_RegridIndex) :: index

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Check variables
      ESMF_INIT_CHECK_DEEP(ESMF_TransformValuesGetInit,tv,rc)

      ! Create the RegridIndex structure for use in the AddLink calls
      index = ESMF_RegridIndexCreate(srcSize, (/dstSizeX+2*dstIndexMod(1), &
                                                dstSizeY+2*dstIndexMod(2)/), &
                                                localrc)

      ! determine number of weights for each entry based
      ! on input order of interpolation
      if (order.eq.1) then
        allocate(weights(1), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "weights", &
                                       ESMF_CONTEXT, rc)) return
      else
        dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                   "2nd order conservative", &
                   ESMF_CONTEXT, rc)
        return
        allocate(weights(3), stat=localrc)  ! 2nd-order requires 3 weights
        if (ESMF_LogMsgFoundAllocError(localrc, "weights", &
                                       ESMF_CONTEXT, rc)) return
      endif

      ibDst = 1
      ieDst = dstSizeX
      jbDst = 1
      jeDst = dstSizeY
      ibSrc = 1
      ieSrc = srcSize

      allocate(cornerX(numDstCorners), &
               cornerY(numDstCorners), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "corner arrays", &
                                     ESMF_CONTEXT, rc)) return

      allocate(searchMask(dstSizeX*dstSizeY))

      dstMinX = minval(dstCornerX)
      dstMinY = minval(dstCornerY)
      dstMaxX = maxval(dstCornerX)
      dstMaxY = maxval(dstCornerY)

      ! allocate and fill bins, if necessary
      ! for now, assume vertical bins only
      call ESMF_VertBinsCreate(2, targetBinSize, &
                               (/ibDst, jbDst/), (/ieDst, jeDst/), &
                               numBins, binMin, binMax, binAddrMin, binAddrMax, &
                               cornerX2D=dstCornerX, cornerY2D=dstCornerY, &
                               rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      iDst = 0
      jDst = 0
      nDst = 0
      ! loop through each cell on source interngrid and
      ! perform line integrals around each cell
      srcLoop: do srcAdd = ibSrc,ieSrc
               
        if (.not. srcMask(srcAdd)) cycle srcLoop  ! TODO: right mask?

        srcMinX = minval(srcCornerX(:,srcAdd))
        srcMinY = minval(srcCornerY(:,srcAdd))
        srcMaxX = maxval(srcCornerX(:,srcAdd))
        srcMaxY = maxval(srcCornerY(:,srcAdd))

        ! restrict searches first using search bins
        minAddr = dstSizeX*dstSizeY
        maxAddr = 1
        do n = 1, numBins(2)
          if (srcMaxY.ge.binMin(2,n) .AND. srcMinY.le.binMax(2,n)) then
            minAddr = min(minAddr, binAddrMin(n))
            maxAddr = max(maxAddr, binAddrMax(n))
          endif
        enddo

        ! further restrict searches using cell information
        numSearchCells = 0
        do searchAdd = minAddr, maxAddr
          jDst = (searchAdd-1)/dstSizeX + 1
          iDst = searchAdd - (jDst-1)*dstSizeX
          dstMinX = minval(dstCornerX(:,iDst,jDst))
          dstMinY = minval(dstCornerY(:,iDst,jDst))
          dstMaxX = maxval(dstCornerX(:,iDst,jDst))
          dstMaxY = maxval(dstCornerY(:,iDst,jDst))
          searchMask(searchAdd) = (dstMinX.le.srcMaxX .AND. &
                                   dstMaxX.ge.srcMinX .AND. &
                                   dstMinY.le.srcMaxY .AND. &
                                   dstMaxY.ge.srcMinY)
          if (searchMask(searchAdd)) numSearchCells = numSearchCells + 1
        enddo

        ! create search arrays
        allocate(searchAddList(               numSearchCells), &
                 searchTmpMask(               numSearchCells), &
                 searchCenterX(               numSearchCells), &
                 searchCenterY(               numSearchCells), &
                 searchCornerX(numDstCorners, numSearchCells), &
                 searchCornerY(numDstCorners, numSearchCells))
        n = 0
        do searchAdd = minAddr, maxAddr
          jDst = (searchAdd-1)/dstSizeX + 1
          iDst = searchAdd - (jDst-1)*dstSizeX
          if (searchMask(searchAdd)) then
            n = n + 1
            searchAddList(  n) = searchAdd
            searchTmpMask(  n) =    dstMask(  iDst,jDst)
            searchCenterX(  n) = dstCenterX(  iDst,jDst)
            searchCenterY(  n) = dstCenterY(  iDst,jDst)
            searchCornerX(:,n) = dstCornerX(:,iDst,jDst)
            searchCornerY(:,n) = dstCornerY(:,iDst,jDst)
          endif
        enddo

        xref = srcCenterX(srcAdd)
        yref = srcCenterY(srcAdd)

        ! integrate around this cell
        srcCornerLoop: do corner = 1,numSrcCorners
          nextCorner = mod(corner,numSrcCorners) + 1

          ! define endpoints of the current segment
          xbeg = srcCornerX(corner    ,srcAdd)
          ybeg = srcCornerY(corner    ,srcAdd)
          xend = srcCornerX(nextCorner,srcAdd)
          yend = srcCornerY(nextCorner,srcAdd)
          fullLine(1) = xbeg
          fullLine(2) = ybeg
          fullLine(3) = xend
          fullLine(4) = yend

          ! to ensure exact path taken during both
          ! sweeps, always integrate segments in the same
          ! direction (south to north or west to east).
          reverse = .false.
          if ((yend.lt.ybeg) .OR. &
              (yend.eq.ybeg .AND. xend.lt.xbeg)) then
            xbeg = srcCornerX(nextCorner,srcAdd)
            ybeg = srcCornerY(nextCorner,srcAdd)
            xend = srcCornerX(corner    ,srcAdd)
            yend = srcCornerY(corner    ,srcAdd)
            reverse = .true.
          endif

          ! save the beginning coordinates for later use and
          ! initialize sub-segment counter
          xIntersect     = xbeg
          yIntersect     = ybeg
          xIntersectLast = xbeg
          yIntersectLast = ybeg
          xoff           = 0.d0
          yoff           = 0.d0
          numSubseg      = 0
          startCorner    = corner

          ! the line integral contributions are defined such
          ! that the contribution is zero if this is a constant-x
          ! segment (where x=longitude in spherical coords).
          if (xend.eq.xbeg) cycle srcCornerLoop

          ! integrate along this segment, detecting intersections
          ! and computing the line integral for each sub-segment
          do while (ybeg.ne.yend .OR. xbeg.ne.xend)

            ! prevent infinite loops if integration gets stuck
            ! near cell or threshold boundary
            numSubseg = numSubseg + 1
            if (numSubseg.gt.maxSubseg) then
              dummy=ESMF_LogMsgFoundError(ESMF_RC_INTNRL_BAD, &
                         "numSubseg exceeded maxSubseg", &
                         ESMF_CONTEXT, rc)
              return
            endif

            ! find next intersection of this segment with a interngrid
            ! line on the destination interngrid.  Offset from last
            ! intersection to nudge into next interngrid cell.
            if (coordSystem .eq. ESMF_COORD_SYSTEM_SPHERICAL) then
              call ESMF_RegridConservXSphr(searchAdd, startCorner, &
                                           xIntersect, yIntersect, &
                                           xoff, yoff, coinc, &
                                           xbeg, ybeg, xend, yend, fullLine, &
                                           searchCenterX, searchCenterY, &
                                           searchCornerX, searchCornerY, &
                                           searchTmpMask, cornerX, cornerY, &
                                           localrc)
            else
              call ESMF_RegridConservXRect(searchAdd, startCorner, &
                                           xIntersect, yIntersect, &
                                           xoff, yoff, coinc, &
                                           xbeg, ybeg, xend, yend, fullLine, &
                                           searchCornerX, searchCornerY, &
                                           searchTmpMask, cornerX, cornerY, &
                                           localrc)
            endif

              ! compute line integral for this subsegment.
              call ESMF_RegridConservLineInt(weights, coordSystem,       &
                                             xIntersectLast, xIntersect, &
                                             yIntersectLast, yIntersect, &
                                             xref, yref, localrc)

              ! if integrating in reverse order, change
              ! sign of weights
              if (reverse) then
                weights = -weights
              endif
              startCorner = 0

              ! store the appropriate addresses and weights.
              ! also add contributions to cell areas and centroids.
              nDst = 0
              if (searchAdd.gt.0) nDst = searchAddList(searchAdd)
              if (nDst.ne.0) then
                jDst = (nDst-1)/dstSizeX + 1
                iDst = nDst - (jDst-1)*dstSizeX
                ! is this the right mask -- in SCRIP it looks like the srcMask
                ! but we have also kicked out of this loop if not srcMask
                if (dstMask(iDst,jDst)) then
                  dstAdd(dstOrder(1)) = iDst + dstIndexMod(1)
                  dstAdd(dstOrder(2)) = jDst + dstIndexMod(2)
                  call ESMF_RegridAddLink(tv, srcAdd, dstAdd, weights(1), &
                                          aggregate=.true., index=index, &
                                          rc=localrc)
                  if (ESMF_LogMsgFoundError(localrc, &
                                            ESMF_ERR_PASSTHRU, &
                                            ESMF_CONTEXT, rc)) return

                  srcFracArea(srcAdd)    = srcFracArea(srcAdd)    + weights(1)
                  dstFracArea(iDst,jDst) = dstFracArea(iDst,jDst) + weights(1)
                endif
              endif

              srcArea(srcAdd)        =      srcArea(srcAdd) + weights(1)
              if (order.gt.1) then
                srcCentroidX(srcAdd) = srcCentroidX(srcAdd) + weights(2)
                srcCentroidY(srcAdd) = srcCentroidY(srcAdd) + weights(3)
              endif

              ! reset ybeg and xbeg for next subsegment.
              xIntersectLast = xIntersect
              yIntersectLast = yIntersect
              xbeg           = xIntersect + xoff
              ybeg           = yIntersect + yoff
            enddo
            ! end of segment

          enddo srcCornerLoop
          ! finished with this cell - start on next cell

          deallocate(searchAddList, searchTmpMask, searchCenterX, &
                     searchCenterY, searchCornerX, searchCornerY)

      enddo srcLoop

      call ESMF_VertBinsDestroy(binMin, binMax, binAddrMin, binAddrMax, &
                                rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      deallocate(cornerX, &
                 cornerY, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate corner arrays", &
                                     ESMF_CONTEXT, rc)) return

      deallocate(searchMask)

      ! now integrate around each cell on destination interngrid
      allocate(cornerX(numSrcCorners), &
               cornerY(numSrcCorners), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "corner arrays", &
                                     ESMF_CONTEXT, rc)) return

      allocate(searchMask(srcSize))

      srcMinX = minval(srcCornerX)
      srcMinY = minval(srcCornerY)
      srcMaxX = maxval(srcCornerX)
      srcMaxY = maxval(srcCornerY)

      ! allocate and fill bins, if necessary
      ! for now, assume vertical bins only
      call ESMF_VertBinsCreate(1, targetBinSize, (/ibSrc, 1/), (/ieSrc, 1/), &
                               numBins, binMin, binMax, binAddrMin, binAddrMax, &
                               cornerX1D=srcCornerX, cornerY1D=srcCornerY, &
                               rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      srcAdd = 0
      dstLoopOuter: do jDst   = jbDst,jeDst
        dstLoopInner: do iDst = ibDst,ieDst

          if (.not. dstMask(iDst,jDst)) cycle dstLoopInner

          dstMinX = minval(dstCornerX(:,iDst,jDst))
          dstMinY = minval(dstCornerY(:,iDst,jDst))
          dstMaxX = maxval(dstCornerX(:,iDst,jDst))
          dstMaxY = maxval(dstCornerY(:,iDst,jDst))

          ! restrict searches first using search bins
          ij      = (jDst-jbDst)*dstSizeX + (iDst-ibDst) + 1
          minAddr = srcSize
          maxAddr = 1
          do n = 1, numBins(2)
            if (dstMaxY.ge.binMin(2,n) .AND. dstMinY.le.binMax(2,n)) then
              minAddr = min(minAddr, binAddrMin(n))
              maxAddr = max(maxAddr, binAddrMax(n))
            endif
          enddo

          ! further restrict searches using cell information
          numSearchCells = 0
          do searchAdd = minAddr, maxAddr
            srcMinX = minval(srcCornerX(:,searchAdd))
            srcMinY = minval(srcCornerY(:,searchAdd))
            srcMaxX = maxval(srcCornerX(:,searchAdd))
            srcMaxY = maxval(srcCornerY(:,searchAdd))
            searchMask(searchAdd) = (srcMinX.le.dstMaxX .AND. &
                                     srcMaxX.ge.dstMinX .AND. &
                                     srcMinY.le.dstMaxY .AND. &
                                     srcMaxY.ge.dstMinY)
            if (searchMask(searchAdd)) numSearchCells = numSearchCells + 1
          enddo

          ! create search arrays
          allocate(searchAddList(               numSearchCells), &
                   searchTmpMask(               numSearchCells), &
                   searchCenterX(               numSearchCells), &
                   searchCenterY(               numSearchCells), &
                   searchCornerX(numDstCorners, numSearchCells), &
                   searchCornerY(numDstCorners, numSearchCells))
          n = 0
          do searchAdd = minAddr, maxAddr
            if (searchMask(searchAdd)) then
              n = n + 1
              searchAddList(  n) = searchAdd
              searchTmpMask(  n) =    srcMask(  searchAdd)
              searchCenterX(  n) = srcCenterX(  searchAdd)
              searchCenterY(  n) = srcCenterY(  searchAdd)
              searchCornerX(:,n) = srcCornerX(:,searchAdd)
              searchCornerY(:,n) = srcCornerY(:,searchAdd)
            endif
          enddo

          xref = dstCenterX(iDst,jDst)
          yref = dstCenterY(iDst,jDst)

          ! integrate around this cell
          dstCornerLoop: do corner = 1,numDstCorners
            nextCorner = mod(corner,numDstCorners) + 1

            ! define endpoints of the current segment
            xbeg = dstCornerX(corner    ,iDst,jDst)
            ybeg = dstCornerY(corner    ,iDst,jDst)
            xend = dstCornerX(nextCorner,iDst,jDst)
            yend = dstCornerY(nextCorner,iDst,jDst)
            fullLine(1) = xbeg
            fullLine(2) = ybeg
            fullLine(3) = xend
            fullLine(4) = yend

            ! to ensure exact path taken during both sweeps, always integrate
            ! segments in the same direction (south to north or west to east).
            reverse = .false.
            if ((yend.lt.ybeg) .OR. &
                (yend.eq.ybeg .AND. xend.lt.xbeg)) then
              xbeg = dstCornerX(nextCorner,iDst,jDst)
              ybeg = dstCornerY(nextCorner,iDst,jDst)
              xend = dstCornerX(corner    ,iDst,jDst)
              yend = dstCornerY(corner    ,iDst,jDst)
              reverse = .true.
            endif

            ! initialize sub-segment counter and initial endpoint
            xIntersect     = xbeg
            yIntersect     = ybeg
            xIntersectLast = xbeg
            yIntersectLast = ybeg
            xoff           = 0.0d0
            yoff           = 0.0d0
            numSubseg      = 0
            startCorner    = corner

            ! the line integral contributions are defined such that the
            ! contribution is zero if this is a constant-x segment
            ! (where x=longitude in spherical coords).
            if (xend.eq.xbeg) cycle dstCornerLoop

            ! integrate along this segment, detecting intersections
            ! and computing the line integral for each sub-segment
            do while (ybeg.ne.yend .OR. xbeg.ne.xend)

              ! prevent infinite loops if integration gets stuck near cell or
              ! threshold boundary
              numSubseg = numSubseg + 1
              if (numSubseg.gt.maxSubseg) then
                dummy=ESMF_LogMsgFoundError(ESMF_RC_INTNRL_BAD, &
                           "numSubseg exceeded maxSubseg", &
                           ESMF_CONTEXT, rc)
                return
              endif

              ! find next intersection of this segment with a interngrid line on the
              ! destination interngrid.  Offset from last intersection to nudge into
              ! next interngrid cell.
              if (coordSystem .eq. ESMF_COORD_SYSTEM_SPHERICAL) then
                call ESMF_RegridConservXSphr(searchAdd, startCorner, &
                                             xIntersect, yIntersect, &
                                             xoff, yoff, coinc, &
                                             xbeg, ybeg, xend, yend, fullLine, &
                                             searchCenterX, searchCenterY, &
                                             searchCornerX, searchCornerY, &
                                             searchTmpMask, cornerX, cornerY, &
                                             localrc)
              else
                call ESMF_RegridConservXRect(searchAdd, startCorner, &
                                             xIntersect, yIntersect, &
                                             xoff, yoff, coinc, &
                                             xbeg, ybeg, xend, yend, fullLine, &
                                             searchCornerX, searchCornerY, &
                                             searchTmpMask, cornerX, cornerY, &
                                             localrc)
              endif

              ! compute line integral for this subsegment.
              call ESMF_RegridConservLineInt(weights, coordSystem,       &
                                             xIntersectLast, xIntersect, &
                                             yIntersectLast, yIntersect, &
                                             xref, yref, localrc)

              ! if integrating in reverse order, change sign of weights
              if (reverse) then
                weights = -weights
              endif
              startCorner = 0

              ! store the appropriate addresses and weights.
              ! also add contributions to cell areas and centroids.
              srcAdd = 0
              if (searchAdd.gt.0) srcAdd = searchAddList(searchAdd)
              if (srcAdd.ne.0 .AND. (.not.coinc)) then
                if (srcMask(srcAdd)) then
                  dstAdd(dstOrder(1)) = iDst + dstIndexMod(1)
                  dstAdd(dstOrder(2)) = jDst + dstIndexMod(2)
                  call ESMF_RegridAddLink(tv, srcAdd, dstAdd, weights(1), &
                                          aggregate=.true., index=index, &
                                          rc=localrc)
                  if (ESMF_LogMsgFoundError(localrc, &
                                            ESMF_ERR_PASSTHRU, &
                                            ESMF_CONTEXT, rc)) return

                  srcFracArea(srcAdd)    = srcFracArea(srcAdd)    + weights(1)
                  dstFracArea(iDst,jDst) = dstFracArea(iDst,jDst) + weights(1)
                endif
              endif

              dstArea(iDst,jDst)        =      dstArea(iDst,jDst) + weights(1)
              if (order.gt.1) then
                dstCentroidX(iDst,jDst) = dstCentroidX(iDst,jDst) + weights(2)
                dstCentroidY(iDst,jDst) = dstCentroidY(iDst,jDst) + weights(3)
              endif

              ! reset ybeg and xbeg for next subsegment.
              xIntersectLast = xIntersect
              yIntersectLast = yIntersect
              xbeg           = xIntersect + xoff
              ybeg           = yIntersect + yoff
            enddo
            ! end of segment

          enddo dstCornerLoop
          ! finished with this cell - start on next cell

          deallocate(searchAddList, searchTmpMask, searchCenterX, &
                     searchCenterY, searchCornerX, searchCornerY)

        enddo dstLoopInner
      enddo dstLoopOuter

      call ESMF_VertBinsDestroy(binMin, binMax, binAddrMin, binAddrMax, &
                                rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      deallocate(cornerX, &
                 cornerY, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate corner arrays", &
                                     ESMF_CONTEXT, rc)) return
      !
      ! For interngrids in spherical coordinates:
      ! correct for situations where N/S pole not explicitly included in
      ! interngrid (i.e. as a interngrid corner point). if pole is missing from only
      ! one interngrid, need to correct only the area and centroid of that
      ! interngrid.  if missing from both, do complete weight calculation.
      if (coordSystem .eq. ESMF_COORD_SYSTEM_SPHERICAL) then

        ! North Pole
        weights(1)   =  pi2
        if (order.gt.1) then
          weights(2) =  pi*pi
          weights(3) =  0.0d0
        endif

        poleLoop1: do srcAdd = ibSrc,ieSrc
            if (srcArea   (srcAdd).lt.-1.5*pi .AND. &
                srcCenterY(srcAdd).gt.0.0) then
              srcArea(srcAdd) = srcArea(srcAdd) + weights(1)
              if (order.gt.1) then
                srcCentroidX(srcAdd) = srcCentroidX(srcAdd) + weights(2)
                srcCentroidY(srcAdd) = srcCentroidY(srcAdd) + weights(3)
              endif
              if (srcMask(srcAdd)) srcFracArea(srcAdd) = &
                                   srcFracArea(srcAdd) + weights(1)
              exit poleLoop1
            endif
        enddo poleLoop1
        if (srcAdd.gt.srcSize) srcAdd = 0

        poleLoop2: do jDst = jbDst,jeDst
          do iDst          = ibDst,ieDst
            if (dstArea   (iDst,jDst).lt.-1.5*pi .AND. &
                dstCenterY(iDst,jDst).gt.0.0) then
              dstArea(iDst,jDst) = dstArea(iDst,jDst) + weights(1)
              if (order.gt.1) then
                dstCentroidX(iDst,jDst) = &
                dstCentroidX(iDst,jDst) + weights(2)
                dstCentroidY(iDst,jDst) = &
                dstCentroidY(iDst,jDst) + weights(3)
              endif
              if (dstMask(iDst,jDst)) dstFracArea(iDst,jDst) = &
                                      dstFracArea(iDst,jDst) + weights(1)
              exit poleLoop2
            endif
          enddo
        enddo poleLoop2
        if (iDst.gt.dstSizeX .OR. jDst.gt.dstSizeY) then
          iDst = 0
          jDst = 0
        endif

        if (srcAdd.ne.0 .AND. iDst.ne.0 .AND. jDst.ne.0) then
          dstAdd(dstOrder(1)) = iDst + dstIndexMod(1)
          dstAdd(dstOrder(2)) = jDst + dstIndexMod(2)
          call ESMF_RegridAddLink(tv, srcAdd, dstAdd, weights(1), &
                                  aggregate=.true., index=index, &
                                  rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif

        ! South Pole
        weights(1)   =  2.0d0*pi
        if (order.gt.1) then
          weights(2) = -pi*pi
          weights(3) =  0.0d0
        endif

        poleLoop3: do srcAdd = ibSrc,ieSrc
            if (srcArea   (srcAdd).lt.-1.5*pi .AND. &
                srcCenterY(srcAdd).lt.0.0d0) then
              srcArea(srcAdd) = srcArea(srcAdd) + weights(1)
              if (order.gt.1) then
                srcCentroidX(srcAdd) = srcCentroidX(srcAdd) + weights(2)
                srcCentroidY(srcAdd) = srcCentroidY(srcAdd) + weights(3)
              endif
              if (srcMask(srcAdd)) srcFracArea(srcAdd) = &
                                   srcFracArea(srcAdd) + weights(1)
              exit poleLoop3
            endif
        enddo poleLoop3
        if (srcAdd.gt.srcSize) srcAdd = 0

        poleLoop4: do jDst = jbDst,jeDst
          do iDst          = ibDst,ieDst
            if (dstArea   (iDst,jDst).lt.-1.5*pi .AND. &
                dstCenterY(iDst,jDst).lt.0.0d0) then
              dstArea(iDst,jDst) = dstArea(iDst,jDst) + weights(1)
              if (order.gt.1) then
                dstCentroidX(iDst,jDst) = &
                dstCentroidX(iDst,jDst) + weights(2)
                dstCentroidY(iDst,jDst) = &
                dstCentroidY(iDst,jDst) + weights(3)
              endif
              if (dstMask(iDst,jDst)) dstFracArea(iDst,jDst) = &
                                      dstFracArea(iDst,jDst) + weights(1)
              exit poleLoop4
            endif
          enddo
        enddo poleLoop4
        if (iDst.gt.dstSizeX .OR. jDst.gt.dstSizeY) then
          iDst = 0
          jDst = 0
        endif

        if (srcAdd.ne.0 .AND. iDst.ne.0 .AND. jDst.ne.0) then
          dstAdd(dstOrder(1)) = iDst + dstIndexMod(1)
          dstAdd(dstOrder(2)) = jDst + dstIndexMod(2)
          call ESMF_RegridAddLink(tv, srcAdd, dstAdd, weights(1), &
                                  aggregate=.true., index=index, &
                                  rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif

      endif  ! coordSystem_Spherical

      ! finish centroid computation if second-order regrid
      if (order.gt.1) then
        where (srcArea.ne.0.0d0)
          srcCentroidX = srcCentroidX/srcArea
          srcCentroidY = srcCentroidY/srcArea
        endwhere
        where (dstArea.ne.0.0d0)
          dstCentroidX = dstCentroidX/dstArea
          dstCentroidY = dstCentroidY/dstArea
        endwhere
      endif

      ! Clean up some allocatables and return
      call ESMF_RegridIndexDestroy(index, localrc)
      deallocate(weights, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate weights", &
                                     ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      
      end subroutine ESMF_RegridConservSearch

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridConservNormalize"
!BOPI
! !IROUTINE: ESMF_RegridConservNormalize - Normalizes a conservative Regrid structure

! !INTERFACE:
      subroutine ESMF_RegridConservNormalize(tv, order, regridnorm, &
                                             srcSize, dstSizeX, dstSizeY, &
                                             srcIndexMod, srcOrder, &
                                             dstIndexMod, dstOrder, &
                                             dstArea, dstUsrArea, dstFracArea, &
                                             srcArea, srcUsrArea, srcFracArea, &
                                             dstCentroidX, dstCentroidY, &
                                             srcCentroidX, srcCentroidY, rc)
!
! !ARGUMENTS:
      type(ESMF_TransformValues), intent(inout) :: tv
      integer, intent(in) :: order
      type (ESMF_RegridNormOpt), intent(in) :: regridnorm
      integer, intent(in) :: srcSize
      integer, intent(in) :: dstSizeX
      integer, intent(in) :: dstSizeY
      integer, dimension(:), intent(in) :: srcIndexMod
      integer, dimension(:), intent(in) :: srcOrder
      integer, dimension(:), intent(in) :: dstIndexMod
      integer, dimension(:), intent(in) :: dstOrder
      real(ESMF_KIND_R8), dimension(dstSizeX,dstSizeY), intent(inout) :: dstArea
      real(ESMF_KIND_R8), dimension(dstSizeX,dstSizeY), intent(inout) :: dstUsrArea
      real(ESMF_KIND_R8), dimension(dstSizeX,dstSizeY), intent(inout) :: dstFracArea
      real(ESMF_KIND_R8), dimension(srcSize), intent(inout) :: srcArea
      real(ESMF_KIND_R8), dimension(srcSize), intent(inout) :: srcUsrArea
      real(ESMF_KIND_R8), dimension(srcSize), intent(inout) :: srcFracArea
      real(ESMF_KIND_R8), dimension(dstSizeX,dstSizeY), intent(inout), &
                                                        optional      :: dstCentroidX
      real(ESMF_KIND_R8), dimension(dstSizeX,dstSizeY), intent(inout), &
                                                        optional      :: dstCentroidY
      real(ESMF_KIND_R8), dimension(srcSize), intent(inout), &
                                                        optional      :: srcCentroidX
      real(ESMF_KIND_R8), dimension(srcSize), intent(inout), &
                                                        optional      :: srcCentroidY
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a source field and destination field (and their attached
!     interngrids), this routine constructs a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     field to the destination field using a conservative interpolation.
!     Returns a pointer to a new {\tt Regrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[srcArray]
!          Field to be regridded.
!     \item[dstArray]
!          Resultant field where regridded source field will be stored.
! \item[TODO:]  make match actual arglist
!     \item[name]
!          {\tt Regrid}name.
!     \item[srcCornerX]
!          X cell corner coords for gathered source interngrid cells that potentially
!          overlap destination domain.
!     \item[srcCornerY]
!          Y cell corner coords for gathered source interngrid cells that potentially
!          overlap destination domain.
!     \item[dstCornerX]
!          X cell corner coords for gathered destination interngrid cells.
!     \item[dstCornerY]
!          Y cell corner coords for gathered destination interngrid cells.
!     \item[srcMask]
!          Optional user-defined mask to specify or eliminate source points from
!          regridding.  Default is that all source points participate.
!     \item[dstUserMmask]
!          Optional user-defined mask to specify or eliminate destination points
!          from regridding.  Default is that all destination points participate.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      character(len=ESMF_MAXSTR) :: logMsg
      integer :: localrc                          ! Error status
      integer :: &
           iDst, jDst,            &!
           n, numLinks,           &!
           srcAdd                  !
      integer(ESMF_KIND_I4), dimension(:), pointer :: &
           dstIndex, srcIndex      !
      logical :: dummy
      real(ESMF_KIND_R8) :: &
           normFactor              ! factor for normalizing wts
      real(ESMF_KIND_R8), dimension(:), pointer :: &
           weightsData             !
      real(ESMF_KIND_R8), dimension(:), allocatable :: &
           weights                 ! local regridding weight array
      real(ESMF_KIND_R8), dimension(:,:), allocatable :: &
           temp2d
      type(ESMF_LocalArray) :: &
           srcindexarr,           &!
           dstindexarr,           &!
           weightsarr              !

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ESMF_INIT_CHECK_DEEP(ESMF_TransformValuesGetInit,tv,rc)

      ! determine number of weights for each entry based
      ! on input order of interpolation
      if (order.eq.1) then
        allocate(weights(1), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "weights", &
                                       ESMF_CONTEXT, rc)) return
      else
        dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                     "2nd order conservative", &
                     ESMF_CONTEXT, rc)
        return
        allocate(weights(3), stat=localrc)  ! 2nd-order requires 3 weights
        if (ESMF_LogMsgFoundAllocError(localrc, "weights", &
                                       ESMF_CONTEXT, rc)) return
      endif

      ! normalize weights based on normalization option
      ! include centroid contribution to 2nd-order weights
      call ESMF_TransformValuesGet(tv, numlist=numlinks, srcindex=srcindexarr, &
                                   dstindex=dstindexarr, weights=weightsarr, rc=rc)
      call ESMF_LocalArrayGetData(srcindexarr, srcIndex, ESMF_DATA_REF, rc)
      call ESMF_LocalArrayGetData(dstindexarr, dstIndex, ESMF_DATA_REF, rc)
      call ESMF_LocalArrayGetData(weightsarr, weightsData, ESMF_DATA_REF, rc)

      do n = 1,numlinks
        srcAdd     = srcIndex(n)
        iDst       = dstIndex((n-1)*2 + dstOrder(1)) - dstIndexMod(1)
        jDst       = dstIndex((n-1)*2 + dstOrder(2)) - dstIndexMod(2)
        weights(1) = weightsData(n)            ! TODO: fix this for second order

        select case (regridnorm%regridNormOpt)
        ! ESMF_REGRID_NORM_NONE
        case (1)
          normFactor = 1.0d0

        ! ESMF_REGRID_NORM_DSTAREA
        case (2)
          normFactor = 1.0d0/dstArea(iDst,jDst)

        ! ESMF_REGRID_NORM_FRACAREA
        case (3)
          normFactor = 1.0d0/dstFracArea(iDst,jDst)

        case default
          normFactor = 0.0d0
        end select

        weightsData(n)   =  weights(1)*normFactor
        if (order.gt.1) then            ! TODO: fix this for second order
                                       !       maybe store weights like dst indexes
          weightsData(n) = (weights(2) -                      &
                            weights(1)*srcCentroidX(srcAdd)) * normFactor
          weightsData(n) = (weights(3) -                      &
                            weights(1)*srcCentroidY(srcAdd)) * normFactor
        endif
      enddo

      where (srcArea.ne.0.0d0) srcFracArea = srcFracArea/srcArea
      where (dstArea.ne.0.0d0) dstFracArea = dstFracArea/dstArea

      ! perform some error checking on final weights
      ! use temp array to hold sums
      allocate (temp2d(dstSizeX,dstSizeY), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "temp2d", &
                                     ESMF_CONTEXT, rc)) return

      do srcAdd = 1,srcSize
        if (srcArea(srcAdd).lt.-0.01d0) then
          write(logMsg, *) "Source interngrid area < -0.01"
          call ESMF_LogWrite(logMsg, ESMF_LOG_WARNING)
          write(logMsg, *) " value is ", srcArea(srcAdd)
          call ESMF_LogWrite(logMsg, ESMF_LOG_WARNING)
          write(logMsg, *) " at location ", srcAdd
          call ESMF_LogWrite(logMsg, ESMF_LOG_WARNING)
        endif
      enddo

      do jDst   = 1,dstSizeY
        do iDst = 1,dstSizeX
          if (dstArea(iDst,jDst).lt.-0.01d0) then
            write(logMsg, *) "Dest interngrid area < -0.01"
            call ESMF_LogWrite(logMsg, ESMF_LOG_WARNING)
            write(logMsg, *) " value is ", dstArea(iDst,jDst)
            call ESMF_LogWrite(logMsg, ESMF_LOG_WARNING)
            write(logMsg, *) " at location ", iDst, jDst
            call ESMF_LogWrite(logMsg, ESMF_LOG_WARNING)
          endif
          temp2d(iDst,jDst) = 0.0d0
        enddo
      enddo

      do n = 1,numlinks
        srcAdd     = srcIndex(n)
        iDst       = dstIndex((n-1)*2 + dstOrder(1)) - dstIndexMod(1)
        jDst       = dstIndex((n-1)*2 + dstOrder(2)) - dstIndexMod(2)
        weights(1) = weightsData(n)             ! TODO: fix this for second order

        if (weightsData(n).lt.-0.05d0) then
          write(logMsg, *) "Regrid weight < 0"
          call ESMF_LogWrite(logMsg, ESMF_LOG_WARNING)
          write(logMsg, *) " value is ", weights(1)
          call ESMF_LogWrite(logMsg, ESMF_LOG_WARNING)
          write(logMsg, *) " at location ", srcAdd, iDst, jDst
          call ESMF_LogWrite(logMsg, ESMF_LOG_WARNING)
        endif
        if (regridnorm.ne.ESMF_REGRID_NORM_NONE .AND. weights(1).gt.1.05d0) then
          write(logMsg, *) "Regrid weight > 1.05"
          call ESMF_LogWrite(logMsg, ESMF_LOG_WARNING)
          write(logMsg, *) " value is ", weights(1)
          call ESMF_LogWrite(logMsg, ESMF_LOG_WARNING)
          write(logMsg, *) " at location ", srcAdd, iDst, jDst
          call ESMF_LogWrite(logMsg, ESMF_LOG_WARNING)
        endif
        ! sum the weight for each dest interngrid point
        temp2d(iDst,jDst) = temp2d(iDst,jDst) + weights(1)
      enddo

      ! check whether weights sum to the correct values
      do jDst   = 1,dstSizeY
        do iDst = 1,dstSizeX

          select case(regridnorm%regridNormOpt)
          ! ESMF_REGRID_NORM_NONE
          case (1)
            normFactor = dstFracArea(iDst,jDst)*dstArea(iDst,jDst)
          ! ESMF_REGRID_NORM_DSTAREA
          case (2)
            normFactor = dstFracArea(iDst,jDst)
          ! ESMF_REGRID_NORM_FRACAREA
          case (3)
            normFactor = 1.0d0
          end select

          if (abs(temp2d(iDst,jDst))           .gt.1.0d-12 .AND. &
              abs(temp2d(iDst,jDst)-normFactor).gt.0.05d0) then
            write(logMsg, *) "Sum of weights for regrid"
            call ESMF_LogWrite(logMsg, ESMF_LOG_WARNING)
            write(logMsg, *) " value is ", temp2d(iDst,jDst), &
                             " should be ", normFactor
            call ESMF_LogWrite(logMsg, ESMF_LOG_WARNING)
            write(logMsg, *) " at location ", srcAdd, iDst, jDst
            call ESMF_LogWrite(logMsg, ESMF_LOG_WARNING)
          endif
        enddo
      enddo

      ! clean up and set return value
      deallocate(weights, &
                  temp2d, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                     ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      
      end subroutine ESMF_RegridConservNormalize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridConservLineInt"
!BOPI
! !IROUTINE: ESMF_RegridConservLineInt - Line integrals for conservative scheme

! !INTERFACE:

      subroutine ESMF_RegridConservLineInt(weights, coordSystem, xbeg, xend, &
                                           ybeg, yend, xref, yref, rc)
!
! !ARGUMENTS:

      real(ESMF_KIND_R8), dimension(:), intent(out) :: weights
      type(ESMF_CoordSystem), intent(in) :: coordSystem
      real(ESMF_KIND_R8), intent(in) :: xbeg
      real(ESMF_KIND_R8), intent(in) :: ybeg
      real(ESMF_KIND_R8), intent(in) :: xend
      real(ESMF_KIND_R8), intent(in) :: yend
      real(ESMF_KIND_R8), intent(in) :: xref
      real(ESMF_KIND_R8), intent(in) :: yref
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     This routine computes the line integral of the functions that
!     result in the conservative interpolation weights.  For a first
!     order regridding, the weights are area overlaps.  For a second
!     order regridding, the additional two weights are the area-weighted
!     mean distance from the source cell centroid.  The line integral
!     for all these weights is computed along a line defined by the
!     input x,y of the endpoints and the path is assumed to be linear in
!     the coordinate space.  A simple trapezoidal rule using endpoint
!     values is used to evaluate the line integrals in cases where
!     analytic forms are not available. For spherical coordinates,
!     x is assumed to be longitude; y is latitude, both in radians.
!     TODO: add optional midpoint values and quadratic approximation
!
!     The arguments are:
!     \begin{description}
!     \item[weights]
!        The contribution to the first and second order weights
!        from this line segment.
!     \item[coordSystem]
!        The {\tt ESMF_CoordSystem} for this interngrid to determine which
!        line integral form should be used.
!     \item[xbeg, ybeg]
!        The x,y coordinates for the beginning endpoint of this segment.
!        x,y refer to lon,lat in radians for spherical coordinates.
!     \item[xend, yend]
!        The x,y coordinates for the second endpoint of this segment
!        in the same convention as the beginning endpoint.
!     \item[xref, yref]
!        A reference x,y coordinate to use when determining appropriate
!        coordinate range (e.g. for longitude).  These should
!        be the coordinates of the source cell center.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! !REQUIREMENTS:  TODO
!EOPI

      ! variables used locally
      integer :: iorder     ! order of interpolation
      !integer :: localrc    ! for internal error flags

      logical :: dummy

      real (ESMF_KIND_R8) :: &
        dx,               &! x,longitude difference for integral
        lonThresh,        &! longitude threshold to check proper range
        sinlat1, sinlat2, &! sines of latitude endpoints
        coslat1, coslat2, &! cosines of latitude endpoint
        lon1, lon2         ! longitude differences

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! determine order of scheme from size of weight array
      iorder = size(weights)

      !  determine coordinate system and compute integrals for
      !  that coordinate system
      select case(coordSystem%coordSystem)

      ! Spherical coordinates
      case(2)     ! ESMF_COORD_SYSTEM_SPHERICAL

        ! check for proper longitude range and include negative sign and
        ! trapezoidal 1/2 factor in dx
        lonThresh = pi
        dx = xbeg - xend
        if     (dx.gt. lonThresh) then
          dx = dx - 2.d0*pi
        elseif (dx.lt.-lonThresh) then
          dx = dx + 2.d0*pi
        endif
        dx = 0.5d0*dx

        ! calculate common endpoint values
        sinlat1 = SIN(ybeg)
        sinlat2 = SIN(yend)

        ! always calculate first-order weight which is
        !      int(-sin(lat)d(lon)) = dx*(sin(lat1)+sin(lat2))
        weights(1) = dx*(sinlat1 + sinlat2)

        if (iorder.gt.1) then  ! calculate second-order weights

          ! compute additional endpoint function values
          coslat1 = COS(ybeg)
          coslat2 = COS(yend)

          ! the latitude centroid weight is
          !      int((-cos(lat) - lat*sin(lat))dlon) =
          !               dx*(coslat1+coslat2+lat1*sinlat1+lat2*sinlat2)
          weights(3) = dx*(coslat1 + coslat2 + (ybeg*sinlat1 + yend*sinlat2))

          ! the longitude centroid weight is
          !      int(-0.5*lon*(sinlat*coslat + lat)dlon) =
          !                   0.5*dx*(lon1*(sinlat1*coslat1+lat1) +
          !                           lon2*(sinlat2*coslat2+lat2))
          ! must insure lon1,lon2 are in proper longitude range
          if     ((xbeg - xref).gt. lonThresh) then
            lon1 = lon1 - 2.d0*pi
          elseif ((xbeg - xref).lt.-lonThresh) then
            lon1 = lon1 + 2.d0*pi
          endif
          if     ((xend - xref).gt. lonThresh) then
            lon2 = lon2 - 2.d0*pi
          elseif ((xend - xref).lt.-lonThresh) then
            lon2 = lon2 + 2.d0*pi
          endif

          weights(2) = 0.5*dx*(lon1*(coslat1*sinlat1 + ybeg) + &
                               lon2*(coslat2*sinlat2 + yend))
        endif

      ! Cartesian coordinates
      case(3)     ! ESMF_COORD_SYSTEM_CARTESIAN

        ! include negative sign and trapezoidal 0.5 factor in dx
        dx = 0.5d0*(xbeg - xend)
 
        ! always compute first order weight which is
        !         int(-ydx) = dx*(ybeg+yend)
        weights(1) = dx*(ybeg + yend)

        if (iorder.gt.1) then  ! calculate second-order weights

          ! the x centroid weight is
          !        int(-xy dx) = dx*(xbeg*ybeg + xend*yend)
          weights(2) = dx*(xbeg*ybeg + xend*yend)

          ! the y centroid weight is
          !        int(-.5*y**2 dx) = 0.5*dx*(ybeg**2 + yend**2)
          weights(3) = 0.5d0*dx*(ybeg**2 + yend**2)
        endif

      ! Unknown or invalid coordSystem
      case default
        dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                    "Unknown or invalid coordSystem", &
                                    ESMF_CONTEXT, rc)
        return
      end select

      ! successful return
      if (present(rc)) rc = ESMF_SUCCESS
 
      end subroutine ESMF_RegridConservLineInt

!-----------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridConservXRect"
!BOPI
! !IROUTINE: ESMF_RegridConservXRect - finds next intersection
! !INTERFACE:

      subroutine ESMF_RegridConservXRect(nLoc, srcCorner, &
                                         xIntersect, yIntersect, &
                                         xoff, yoff, coinc, &
                                         xbeg, ybeg, xend, yend, fullLine,  &
                                         srchCornerX, srchCornerY, &
                                         mask, cornerX, cornerY, rc) 

!
! !ARGUMENTS:
      integer, intent(out) :: nLoc
      integer, intent(in)  :: srcCorner
      real(ESMF_KIND_R8), intent(out) :: xIntersect
      real(ESMF_KIND_R8), intent(out) :: yIntersect
      real(ESMF_KIND_R8), intent(out) :: xoff
      real(ESMF_KIND_R8), intent(out) :: yoff
      logical, intent(out) :: coinc
      real(ESMF_KIND_R8), intent(in) :: xbeg
      real(ESMF_KIND_R8), intent(in) :: ybeg
      real(ESMF_KIND_R8), intent(in) :: xend
      real(ESMF_KIND_R8), intent(in) :: yend
      real(ESMF_KIND_R8), dimension(4), intent(in) :: fullLine
      real(ESMF_KIND_R8), dimension(:,:), intent(in) :: srchCornerX
      real(ESMF_KIND_R8), dimension(:,:), intent(in) :: srchCornerY
      logical, dimension(:), intent(in) :: mask
      real(ESMF_KIND_R8), dimension(:), intent(inout) :: cornerX
      real(ESMF_KIND_R8), dimension(:), intent(inout) :: cornerY
      integer, intent(out), optional :: rc      

! !DESCRIPTION:
!  Given the endpoints of a line segment, this routine finds the next 
!  intersection of that line segment with a interngrid line of an input interngrid. 
!  The location of the beginning point in the interngrid is returned together
!  with the intersection point.  A coincidence flag is returned if the 
!  segment is entirely coincident with a interngrid line.  If the segment
!  lies entirely within a interngrid cell (no intersection), the endpoints
!  of the segment are returned as the intersection point.  If the
!  beginning point lies outside the interngrid, a location of zero is 
!  returned but an intersection may still be returned if the segment
!  enters the interngrid from outside the interngrid domain.  If the current segment
!  is a sub-segment of a longer line segment, an optional argument with
!  the coordinates of the longer segment endpoints can be supplied
!  to provide consistent intersections along the longer segment.
!
!     The arguments are:
!     \begin{description}
!     \item[nLoc]
!        The location in the searched interngrid containing the beginning
!        endpoint of the line segment.  If point could not be found,
!        returns a zero signifying point outside interngrid.
!     \item[xIntersect, yIntersect]
!        Coordinates of the next intersection of the input line segment
!        with the input interngrid cells.
!     \item[xoff, yoff]
!        Amount to offset intersection coordinates for next call to
!        this routine (to step into next cell).
!     \item[coinc]
!        Logical flag to denote cases where line segment is coincident
!        with cell side.
!     \item[xbeg, ybeg]
!        The x,y coordinates for the beginning endpoint of this segment.
!        x,y refer to lon,lat in radians for spherical coordinates.
!     \item[xend, yend]
!        The x,y coordinates for the second endpoint of this segment
!        in the same convention as the beginning endpoint.
!     \item[fullLine]
!        The starting and ending coordinates of the full line segment
!        in which the current segment is a subsegment.  This is necessary
!        to provide consistent intersection calculations for different
!        paths through the interngrid.
!     \item[srchCornerX, srchCornerY]
!        Coordinate points of cell corners for the list of cells to search
!        for next intersection. x,y refer to lon, lat in radians for
!        spherical coordinates.
!     \item[mask]
!        Logical mask to flag which of the above cells are participating 
!        in regrid.
!     \item[coordSystem]
!        The {\tt ESMF_CoordSystem} for this interngrid to check for special
!        conditions in spherical coordinates.
!     \item[[rc]]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! !REQUIREMENTS:  TODO
!EOPI

      ! local variables
      integer ::         &
        n,               &! dummies for addresses
        corner, nextCorner,        &! loop index, next index
        nCells,          &! search interngrid size
        numCorners        ! number of corners in each search interngrid cell

      logical ::    & 
        reverse,    &! segment in opposite direction of full segment
        outside,    &! true if beg point outside interngrid
        found        ! true if interngrid cell found

      real (ESMF_KIND_R8) ::     &
        xb, yb, xe, ye,         &! local coordinates for segment endpoints
        x1, y1, x2, y2,         &! coordinates for interngrid side endpoints
        dx, dy,                 &! difference in x,y for stepping along seg
        s1, s2, determ,         &! variables used for linear solve to
        mat1, mat2, mat3, mat4, &! matrix entries for intersect linear system
        rhs1, rhs2,             &! rhs for linear system to find intersect
        vec1X, vec1Y,           &! vectors for cross product tests
        vec2X, vec2Y,           &!
        crossProduct             ! cross product to use in various tests 

                                  
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! initialize defaults, flags, etc.
      reverse = .false.
      coinc   = .false.
      outside = .false.
      found   = .false.
      nLoc    = 0                 ! default is zero if no cell location found
      
      nCells     = size(srchCornerX, 2)
      numCorners = size(srchCornerX, 1)
      xIntersect = xend
      yIntersect = yend
      if (nCells.eq.0) return
      
      xoff = 0.0d0
      yoff = 0.0d0
      s1   = 0.001d0
      s2   = 0.0d0
      xb = xbeg
      yb = ybeg
      xe = xend
      ye = yend

      dx = xe - xb
      dy = ye - yb
      if (xend.eq.fullLine(1) .AND. yend.eq.fullLine(2)) reverse = .true.
 
      ! search for location of the beginning of the segment in input interngrid
    5 cellLoop: do n = 1, nCells
        
        ! set up local info for this cell
        do corner = 1,numCorners
          cornerX(corner) = srchCornerX(corner,n)
          cornerY(corner) = srchCornerY(corner,n)
        enddo

        ! if point is outside the cell bounding box, move on to the next cell
        if (xe.lt.minval(cornerX) .OR. xb.gt.maxval(cornerX) .OR. &
            ye.lt.minval(cornerY) .OR. yb.gt.maxval(cornerY)) cycle cellLoop
     !   if (xb.gt.maxval(cornerX) .OR. yb.gt.maxval(cornerY)) cycle cellLoop

        ! congratulations.  you have jumped through another hoop successfully.
        ! now check this cell more carefully to see if the point is inside
        cornerLoop: do corner = 1,numCorners
          nextCorner = MOD(corner,numCorners) + 1

          ! here we take the cross product of the vector making 
          ! up each cell side with the vector formed by the vertex
          ! and search point.  if all the cross products are 
          ! positive, the point is contained in the cell.
          ! TODO: the crossProduct >0 assumes a counterclockwise
          ! TODO:   ordering of corner points.  A more general
          ! TODO:   test is that all the cross products are same sign

          x1 = cornerX(corner    )
          x2 = cornerX(nextCorner)
          y1 = cornerY(corner    )
          y2 = cornerY(nextCorner)

          vec1X = x2 - x1
          vec1Y = y2 - y1
          vec2X = xb - x1
          vec2Y = yb - y1

          ! if this side has zero length, skip the side
          if (vec1X.eq.0.0d0 .AND. vec1Y.eq.0.0d0) cycle cornerLoop

          ! if endpoint coincident with vertex, offset the endpoint before
          !   computing cross product
          if (vec2X.eq.0.0d0 .AND. vec2Y.eq.0.0d0) then
              vec2X = (xb + offset*(xe-xb)) - x1
              vec2Y = (yb + offset*(ye-yb)) - y1
          endif

          crossProduct = vec1X*vec2Y - vec2X*vec1Y

          if (crossProduct.eq.0.0d0) then

            ! if the cross product for a side is zero, the point 
            !   lies exactly on the side. perform another cross
            !   product between the side and the segment itself. 
            ! if this cross product is also zero, the line is 
            !   coincident with the cell boundary - perform the 
            !   dot product and only choose the cell if the dot 
            !   product is positive (parallel vs anti-parallel).
            vec2X = xe - xb
            vec2Y = ye - yb

            crossProduct = vec1X*vec2Y - vec2X*vec1Y

            if (crossProduct.eq.0.0d0) then
              coinc = .true.
              crossProduct = vec1X*vec2X + vec1Y*vec2Y
              if (reverse) crossProduct = -crossProduct
            endif
          endif

          ! if cross product is less than zero, this cell doesn't work
          if (crossProduct.lt.0.0d0) exit cornerLoop

        enddo cornerLoop

        ! if cross products all positive, we found the location
        if (corner.gt.numCorners) then
          found = .true.
          nLoc  = n

          ! if the first part of the segment was outside the interngrid the found point
          !   corresponds to the first point inside the interngrid. invert the segment
          !   to find first intersection with interngrid boundary
          if (outside) then
            xe   = xbeg
            ye   = ybeg
            nLoc = 0
          endif

          go to 10
        endif

        ! otherwise move on to next cell
      enddo cellLoop

      ! cell not found - assume beg point outside interngrid
      ! take baby steps along segment to find a point inside the interngrid
      s2 = s2 + s1
      if (s2.gt.1.0d0) go to 10
      outside = .true.
      xb   = xb + s1*dx
      yb   = yb + s1*dy
      go to 5

   10 continue

      ! now that a cell is found, search for the next intersection.
      ! loop over sides of the cell to find intersection with side
      ! must check all sides for coincidences or intersections
      if (found) then

        intrsctLoop: do corner = 1,numCorners
          nextCorner = mod(corner,numCorners) + 1

          x1 = cornerX(corner    )
          x2 = cornerX(nextcorner)
          y1 = cornerY(corner    )
          y2 = cornerY(nextCorner)

          ! set up linear system to solve for intersection
          mat1 = xe - xb
          mat2 = x1 - x2
          mat3 = ye - yb
          mat4 = y1 - y2
          rhs1 = x1 - xb
          rhs2 = y1 - yb

          determ = mat1*mat4 - mat2*mat3

          ! if the determinant is zero, the segments are either parallel or
          !   coincident.  coincidences were detected above so do nothing.
          ! if the determinant is non-zero, solve for the linear parameters
          !   s for the intersection point on each line segment.
          ! if 0<s1,s2<1 then the segment intersects with this side.
          !   return the point of intersection (adding a small
          !   number so the intersection is off the interngrid line).
          if (abs(determ).gt.1.d-30) then

            s1 = (rhs1*mat4 - mat2*rhs2)/determ
            s2 = (mat1*rhs2 - rhs1*mat3)/determ

            if (s2.ge.0.0d0 .AND. s2.le.1.0d0 .AND. &
                s1.gt.0.0d0 .AND. s1.le.1.0d0) then

              ! recompute intersection based on full segment so intersections
              !   are consistent in cases where computing intersections between
              !   two interngrids
              if (.not. reverse) then
                mat1 = fullLine(3) - fullLine(1)
                mat3 = fullLine(4) - fullLine(2)
                rhs1 = x1 - fullLine(1)
                rhs2 = y1 - fullLine(2)
              else
                mat1 = fullLine(1) - fullLine(3)
                mat3 = fullLine(2) - fullLine(4)
                rhs1 = x1 - fullLine(3)
                rhs2 = y1 - fullLine(4)
              endif

              determ = mat1*mat4 - mat2*mat3

              ! sometimes due to roundoff, the previous determinant is non-zero,
              !   but the lines are actually coincident.  if this is the case,
              !   skip the rest.
              if (determ.ne.0.0d0) then
                s1 = (rhs1*mat4 - mat2*rhs2)/determ
                s2 = (mat1*rhs2 - rhs1*mat3)/determ
 
                xoff = abs(offset/determ)
                if ((s1 + xoff).gt.1.0d0) xoff = 1.0d0 - s1
                yoff = mat3*xoff
                xoff = mat1*xoff

                if (.not. reverse) then
                  xIntersect = fullLine(1) + mat1*s1
                  yIntersect = fullLine(2) + mat3*s1
                else
                  xIntersect = fullLine(3) + mat1*s1
                  yIntersect = fullLine(4) + mat3*s1
                endif
                exit intrsctLoop
              endif
            endif
          endif

          ! no intersection this side, move on to next side

        enddo intrsctLoop

      endif ! found cell

      end subroutine ESMF_RegridConservXRect

!-----------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridConservXSphr"
!BOPI
! !IROUTINE: ESMF_RegridConservXSphr - finds next intersection
! !INTERFACE:

      subroutine ESMF_RegridConservXSphr(nLoc, srcCorner, &
                                         xIntersect, yIntersect, &
                                         xoff, yoff, coinc, &
                                         xbeg, ybeg, xend, yend, fullLine,  &
                                         srchCenterX, srchCenterY, &
                                         srchCornerX, srchCornerY, &
                                         mask, cornerX, cornerY, rc) 

!
! !ARGUMENTS:
      integer, intent(out) :: nLoc
      integer, intent(in)  :: srcCorner
      real(ESMF_KIND_R8), intent(out) :: xIntersect
      real(ESMF_KIND_R8), intent(out) :: yIntersect
      real(ESMF_KIND_R8), intent(out) :: xoff
      real(ESMF_KIND_R8), intent(out) :: yoff
      logical, intent(out) :: coinc
      real(ESMF_KIND_R8), intent(in) :: xbeg
      real(ESMF_KIND_R8), intent(in) :: ybeg
      real(ESMF_KIND_R8), intent(in) :: xend
      real(ESMF_KIND_R8), intent(in) :: yend
      real(ESMF_KIND_R8), dimension(4), intent(in) :: fullLine
      real(ESMF_KIND_R8), dimension(:), intent(in) :: srchCenterX
      real(ESMF_KIND_R8), dimension(:), intent(in) :: srchCenterY
      real(ESMF_KIND_R8), dimension(:,:), intent(in) :: srchCornerX
      real(ESMF_KIND_R8), dimension(:,:), intent(in) :: srchCornerY
      logical, dimension(:), intent(in) :: mask
      real(ESMF_KIND_R8), dimension(:), intent(inout) :: cornerX
      real(ESMF_KIND_R8), dimension(:), intent(inout) :: cornerY
      integer, intent(out), optional :: rc      

! !DESCRIPTION:
!  Given the endpoints of a line segment, this routine finds the next 
!  intersection of that line segment with a interngrid line of an input interngrid. 
!  The location of the beginning point in the interngrid is returned together
!  with the intersection point.  A coincidence flag is returned if the 
!  segment is entirely coincident with a interngrid line.  If the segment
!  lies entirely within a interngrid cell (no intersection), the endpoints
!  of the segment are returned as the intersection point.  If the
!  beginning point lies outside the interngrid, a location of zero is 
!  returned but an intersection may still be returned if the segment
!  enters the interngrid from outside the interngrid domain.  If the current segment
!  is a sub-segment of a longer line segment, an optional argument with
!  the coordinates of the longer segment endpoints can be supplied
!  to provide consistent intersections along the longer segment.
!
!     The arguments are:
!     \begin{description}
!     \item[nLoc]
!        The location in the searched interngrid containing the beginning
!        endpoint of the line segment.  If point could not be found,
!        returns a zero signifying point outside interngrid.
!     \item[xIntersect, yIntersect]
!        Coordinates of the next intersection of the input line segment
!        with the input interngrid cells.
!     \item[xoff, yoff]
!        Amount to offset intersection coordinates for next call to
!        this routine (to step into next cell).
!     \item[coinc]
!        Logical flag to denote cases where line segment is coincident
!        with cell side.
!     \item[xbeg, ybeg]
!        The x,y coordinates for the beginning endpoint of this segment.
!        x,y refer to lon,lat in radians for spherical coordinates.
!     \item[xend, yend]
!        The x,y coordinates for the second endpoint of this segment
!        in the same convention as the beginning endpoint.
!     \item[fullLine]
!        The starting and ending coordinates of the full line segment
!        in which the current segment is a subsegment.  This is necessary
!        to provide consistent intersection calculations for different
!        paths through the interngrid.
!     \item[srchCenterX, srchCenterY]
!        Coordinate points of cell centers for the list of cells to search
!        for next intersection.  x,y refer to lon, lat in radians for
!        spherical coordinates.
!     \item[srchCornerX, srchCornerY]
!        Coordinate points of cell corners for the list of cells to search
!        for next intersection. x,y refer to lon, lat in radians for
!        spherical coordinates.
!     \item[mask]
!        Logical mask to flag which of the above cells are participating 
!        in regrid.
!     \item[coordSystem]
!        The {\tt ESMF_CoordSystem} for this interngrid to check for special
!        conditions in spherical coordinates.
!     \item[[rc]]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! !REQUIREMENTS:  TODO
!EOPI

      ! local variables
      integer ::            &
        n,                  &! dummies for addresses
        corner, nextCorner, &! loop index, next index
        localrc,            &! error signal
        nCells,             &! search interngrid size
        numCorners           ! number of corners in each search interngrid cell
      integer :: nStrt=1

      logical ::    & 
        reverse,    &! segment in opposite direction of full segment
        thresh,     &! flags segments crossing threshold bndy
        outside,    &! true if beg point outside interngrid
        found        ! true if interngrid cell found

      real (ESMF_KIND_R8) ::    &
        refx,                   &! temporary for manipulating longitudes
        xb, yb, xe, ye,         &! local coordinates for segment endpoints
        x1, y1, x2, y2,         &! coordinates for interngrid side endpoints
        dx, dy,                 &! difference in x,y for stepping along seg
        s1, s2, determ,         &! variables used for linear solve to
        mat1, mat2, mat3, mat4, &! matrix entries for intersect linear system
        rhs1, rhs2,             &! rhs for linear system to find intersect
        vec1X, vec1Y,           &! vectors for cross product tests
        vec2X, vec2Y,           &!
        crossProduct             ! cross product to use in various tests 

                                  
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! initialize defaults, flags, etc.
      reverse = .false.
      coinc   = .false.
      thresh  = .false.
      outside = .false.
      found   = .false.
      
      nCells     = size(srchCenterX)
      numCorners = size(srchCornerX, 1)
      
      xIntersect = xend
      yIntersect = yend
      xoff = 0.0d0
      yoff = 0.0d0
      s1   = 0.01d0
      s2   = 0.0d0
      xb = xbeg
      yb = ybeg
      xe = xend
      ye = yend

      ! correct for longitude crossings if necessary
      if     ((xe-xb).gt. 1.5*pi) then
        xe = xe - pi2
      elseif ((xe-xb).lt.-1.5*pi) then
        xe = xe + pi2
      endif

      dx = xe - xb
      dy = ye - yb
      if (xend.eq.fullLine(1) .AND. yend.eq.fullLine(2)) reverse = .true.
 
      ! if this point is poleward of the threshold latitudes, compute the
      ! intersection in a transformed coordinate system to avoid pole singularity
      if (yb.gt.northThreshold .OR. yb.lt.southThreshold) then
        call ESMF_RegridConservIntrsctPole(nLoc,                     &
                                   xIntersect, yIntersect,           &
                                   xoff, yoff, coinc,                &
                                   xbeg, ybeg, xend, yend, fullLine, &
                                   northThreshold, southThreshold,   &
                                   srchCornerX, srchCornerY,         &
                                   mask, localrc) 
         return
       endif

      ! search for location of the beginning of the segment in input interngrid
      nLoc = 0 ! default is zero if no cell location found

    5 cellLoop: do n = nStrt, nCells

        ! set up local info for this cell
        refx    = srchCenterX(n)
        do corner = 1,numCorners
          cornerX(corner) = srchCornerX(corner,n)
          cornerY(corner) = srchCornerY(corner,n)
        enddo

        ! check for longitude crossings in spherical coords
        if ((xb - refx).gt. pi) then
          xb = xb - pi2
          xe = xe - pi2
        endif
        if ((xb - refx).lt.-pi) then
          xb = xb + pi2
          xe = xe + pi2
        endif

        do corner = 1,numCorners
          if ((cornerX(corner) - xb).gt. 1.5*pi) cornerX = cornerX - pi2
          if ((cornerX(corner) - xb).lt.-1.5*pi) cornerX = cornerX + pi2
        enddo

        ! if point is outside the cell bounding box, move on to the next cell
        if (xe.lt.minval(cornerX) .OR. xb.gt.maxval(cornerX) .OR. &
            ye.lt.minval(cornerY) .OR. yb.gt.maxval(cornerY)) cycle cellLoop
     !   if (xb.gt.maxval(cornerX) .OR. yb.gt.maxval(cornerY)) cycle cellLoop

        ! congratulations.  you have jumped through another hoop successfully.
        ! now check this cell more carefully to see if the point is inside
        cornerLoop: do corner = 1,numCorners
          nextCorner = MOD(corner,numCorners) + 1

          ! here we take the cross product of the vector making 
          ! up each cell side with the vector formed by the vertex
          ! and search point.  if all the cross products are 
          ! positive, the point is contained in the cell.
          ! TODO: the crossProduct >0 assumes a counterclockwise
          ! TODO:   ordering of corner points.  A more general
          ! TODO:   test is that all the cross products are same sign

          x1 = cornerX(corner    )
          x2 = cornerX(nextCorner)
          y1 = cornerY(corner    )
          y2 = cornerY(nextCorner)

          vec1X = x2 - x1
          vec1Y = y2 - y1
          vec2X = xb - x1
          vec2Y = yb - y1

          ! if this side has zero length, skip the side
          if (vec1X.eq.0.0d0 .AND. vec1Y.eq.0.0d0) cycle cornerLoop

          ! if endpoint coincident with vertex, offset the endpoint before
          !   computing cross product
          if (vec2X.eq.0.0d0 .AND. vec2Y.eq.0.0d0) then
              vec2X = (xb + offset*(xe-xb)) - x1
              vec2Y = (yb + offset*(ye-yb)) - y1
          endif

          crossProduct = vec1X*vec2Y - vec2X*vec1Y

          if (crossProduct.eq.0.0d0) then

            ! if the cross product for a side is zero, the point 
            !   lies exactly on the side. perform another cross
            !   product between the side and the segment itself. 
            ! if this cross product is also zero, the line is 
            !   coincident with the cell boundary - perform the 
            !   dot product and only choose the cell if the dot 
            !   product is positive (parallel vs anti-parallel).
            vec2X = xe - xb
            vec2Y = ye - yb

            crossProduct = vec1X*vec2Y - vec2X*vec1Y

            if (crossProduct.eq.0.0d0) then
              coinc = .true.
              crossProduct = vec1X*vec2X + vec1Y*vec2Y
              if (reverse) crossProduct = -crossProduct
            endif
          endif

          ! if cross product is less than zero, this cell doesn't work
          if (crossProduct.lt.0.0d0) exit cornerLoop

        enddo cornerLoop

        ! if cross products all positive, we found the location
        if (corner.gt.numCorners) then
          found = .true.
          nLoc  = n
          if (srcCorner.eq.1) nStrt = n

          ! if the first part of the segment was outside the interngrid the found point
          !   corresponds to the first point inside the interngrid. invert the segment
          !   to find first intersection with interngrid boundary
          if (outside) then
            xe   = xbeg
            ye   = ybeg
            nLoc = 0
          endif

          go to 10
        endif

        ! otherwise move on to next cell
      enddo cellLoop

      ! assume found is false here
      if (nStrt.ne.1) then
        nStrt = 1
        goto 5
      endif

      ! cell not found - assume beg point outside interngrid
      ! take baby steps along segment to find a point inside the interngrid
      s2 = s2 + s1
      if (s2.gt.1.0d0) go to 10
      outside = .true.
      xb   = xb + s1*dx
      yb   = yb + s1*dy
      go to 5
   
   10 continue

      ! now that a cell is found, search for the next intersection.
      ! loop over sides of the cell to find intersection with side
      ! must check all sides for coincidences or intersections
      if (found) then

        intrsctLoop: do corner = 1,numCorners
          nextCorner = mod(corner,numCorners) + 1

          x1 = cornerX(corner    )
          x2 = cornerX(nextcorner)
          y1 = cornerY(corner    )
          y2 = cornerY(nextCorner)

          ! set up linear system to solve for intersection
          mat1 = xe - xb
          mat2 = x1 - x2
          mat3 = ye - yb
          mat4 = y1 - y2
          rhs1 = x1 - xb
          rhs2 = y1 - yb

!          if     (mat1.gt. pi) then
!            mat1 = mat1 - pi2
!          elseif (mat1.lt.-pi) then
!            mat1 = mat1 + pi2
!          endif
!          if     (mat2.gt. pi) then
!            mat2 = mat2 - pi2
!          elseif (mat2.lt.-pi) then
!            mat2 = mat2 + pi2
!          endif
!          if     (rhs1.gt. pi) then
!            rhs1 = rhs1 - pi2
!          elseif (rhs1.lt.-pi) then
!            rhs1 = rhs1 + pi2
!          endif

          determ = mat1*mat4 - mat2*mat3

          ! if the determinant is zero, the segments are either parallel or
          !   coincident.  coincidences were detected above so do nothing.
          ! if the determinant is non-zero, solve for the linear parameters
          !   s for the intersection point on each line segment.
          ! if 0<s1,s2<1 then the segment intersects with this side.
          !   return the point of intersection (adding a small
          !   number so the intersection is off the interngrid line).
          if (abs(determ).gt.1.d-30) then

            s1 = (rhs1*mat4 - mat2*rhs2)/determ
            s2 = (mat1*rhs2 - rhs1*mat3)/determ

            if (s2.ge.0.0d0 .AND. s2.le.1.0d0 .AND. &
                s1.gt.0.0d0 .AND. s1.le.1.0d0) then

              ! recompute intersection based on full segment so intersections
              !   are consistent in cases where computing intersections between
              !   two interngrids
              if (.not. reverse) then
                mat1 = fullLine(3) - fullLine(1)
                mat3 = fullLine(4) - fullLine(2)
                rhs1 = x1 - fullLine(1)
                rhs2 = y1 - fullLine(2)
              else
                mat1 = fullLine(1) - fullLine(3)
                mat3 = fullLine(2) - fullLine(4)
                rhs1 = x1 - fullLine(3)
                rhs2 = y1 - fullLine(4)
              endif

!              if     (mat1.gt. pi) then
!                mat1 = mat1 - pi2
!              elseif (mat1.lt.-pi) then
!                mat1 = mat1 + pi2
!              endif
!              if     (rhs1.gt. pi) then
!                rhs1 = rhs1 - pi2
!              elseif (rhs1.lt.-pi) then
!                rhs1 = rhs1 + pi2
!              endif

              determ = mat1*mat4 - mat2*mat3

              ! sometimes due to roundoff, the previous determinant is non-zero,
              !   but the lines are actually coincident.  if this is the case,
              !   skip the rest.
              if (determ.ne.0.0d0) then
                s1 = (rhs1*mat4 - mat2*rhs2)/determ
                s2 = (mat1*rhs2 - rhs1*mat3)/determ
 
                xoff = abs(offset/determ)
                if ((s1 + xoff).gt.1.0d0) xoff = 1.0d0 - s1
                yoff = mat3*xoff
                xoff = mat1*xoff

                if (.not. reverse) then
                  xIntersect = fullLine(1) + mat1*s1
                  yIntersect = fullLine(2) + mat3*s1
                else
                  xIntersect = fullLine(3) + mat1*s1
                  yIntersect = fullLine(4) + mat3*s1
                endif
                exit intrsctLoop
              endif
            endif
          endif

          ! no intersection this side, move on to next side

        enddo intrsctLoop

        ! if intersection crosses pole threshold, reset intersection to
        !   threshold lat
!        if (yIntersect.gt.northThreshold .OR. &
!            yIntersect.lt.southThreshold) then
!          if (yIntersect.gt.0.0d0) then
!            yIntersect = northThreshold
!          else
!            yIntersect = southThreshold
!          endif
!          if (.not. reverse) then
!            s1 = (yIntersect - fullLine(2))/mat3
!            xIntersect = fullLine(1) + mat1*s1
!          else
!            s1 = (yIntersect - fullLine(4))/mat3
!            xIntersect = fullLine(3) + mat1*s1
!          endif
!          if (xoff.eq.0.0d0) then  ! no intersection from above
!            yoff = mat3*offset
!            xoff = mat1*offset
!          endif
!        endif

      endif ! found cell

      end subroutine ESMF_RegridConservXSphr

!-----------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridConservIntrsctPole"
!BOPI
! !IROUTINE: ESMF_RegridConservIntrsctPole
! !INTERFACE:

      subroutine ESMF_RegridConservIntrsctPole(nLoc,               &
                                 xIntersect, yIntersect,           &
                                 xoff, yoff, coinc,                &
                                 xbeg, ybeg, xend, yend, fullLine, &
                                 northThreshold, southThreshold,   &
                                 srchCornerX, srchCornerY,         &
                                 mask, rc) 

!
! !ARGUMENTS:

      integer, intent(out) :: nLoc
      real(ESMF_KIND_R8), intent(out) :: xIntersect
      real(ESMF_KIND_R8), intent(out) :: yIntersect
      real(ESMF_KIND_R8), intent(out) :: xoff
      real(ESMF_KIND_R8), intent(out) :: yoff
      logical, intent(out) :: coinc
      real(ESMF_KIND_R8), intent(in) :: xbeg
      real(ESMF_KIND_R8), intent(in) :: ybeg
      real(ESMF_KIND_R8), intent(in) :: xend
      real(ESMF_KIND_R8), intent(in) :: yend
      real(ESMF_KIND_R8), dimension(4), intent(in) :: fullLine
      real(ESMF_KIND_R8), intent(in) :: northThreshold
      real(ESMF_KIND_R8), intent(in) :: southThreshold
      real(ESMF_KIND_R8), dimension(:,:), intent(in) :: srchCornerX
      real(ESMF_KIND_R8), dimension(:,:), intent(in) :: srchCornerY
      logical, dimension(:), intent(in) :: mask
      integer, intent(out), optional :: rc      

! !DESCRIPTION:
!  This routine performs the same function as 
!  {\tt ESMF\_RegridConservIntersect} for the special case where
!  a polar coordinate transformation is required to resolve issues
!  related to the coordinate system singularity is spherical coordinate
!  systems.
!
!     The arguments are:
!     \begin{description}
!     \item[nLoc]
!        The n location in the searched interngrid containing the beginning
!        endpoint of the line segment.  If point could not be found,
!        returns a zero signifying point outside interngrid.
!     \item[xIntersect, yIntersect]
!        Coordinates of the next intersection of the input line segment
!        with the input interngrid cells.
!     \item[xoff, yoff]
!        Amount to offset intersection coordinates for next call to
!        this routine (to step into next cell).
!     \item[coinc]
!        Logical flag to denote cases where line segment is coincident
!        with cell side.
!     \item[xbeg, ybeg]
!        The x,y coordinates for the beginning endpoint of this segment.
!        x,y refer to lon,lat in radians for spherical coordinates.
!     \item[xend, yend]
!        The x,y coordinates for the second endpoint of this segment
!        in the same convention as the beginning endpoint.
!     \item[fullLine]
!        The starting and ending coordinates of the full line segment
!        in which the current segment is a subsegment.  This is necessary
!        to provide consistent intersection calculations for different
!        paths through the interngrid.
!     \item[northThreshold, southThreshold]
!        Northern and souther latitudes, poleward of which to use this
!        special interesection routine.  Used by this routine to
!        determine when crossing back into normal lat-lon domain.
!     \item[srchCornerX, srchCornerY]
!        Coordinate points of cell corners for the list of cells to search
!        for next intersection. x,y refer to lon, lat in radians for
!        spherical coordinates.
!     \item[mask]
!        Logical mask to flag which of the above cells are participating 
!        in regrid.
!     \item[coordSystem]
!        The {\tt ESMF_CoordSystem} for this interngrid to check for special
!        conditions in spherical coordinates.
!     \item[[rc]]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! !REQUIREMENTS:  TODO
!EOPI

      ! local variables
      integer :: &
        localrc,            &
        n,                  &! dummies for addresses
        nCells,             &! number of cells in search interngrid
        numCorners,         &! number of corners in each cell of search interngrid
        corner, nextCorner   ! loop index, next index

      logical :: & 
        reverse,           &! segment opposite direction of full segment
        outside,           &! point outside of search interngrid
        found               ! true if interngrid cell found

      real (ESMF_KIND_R8) ::         &
        xb, yb, xe, ye,         &! local coordinates for segment endpoints
        x1, y1, x2, y2,         &! coordinates for interngrid side endpoints
        dx, dy,                 &! difference in x,y for stepping along seg
        s1, s2, determ,         &! variables used for linear solve to
        mat1, mat2, mat3, mat4, &! matrix entries for intersect linear system
        rhs1, rhs2,             &! rhs for linear system to find intersect
        vec1X, vec1Y,           &! vectors for cross product tests
        vec2X, vec2Y,           &!
        crossProduct             ! cross product to use in various tests 

      real (ESMF_KIND_R8), dimension(4) :: &
        fullLineXY              ! xy coords of full line

      real (ESMF_KIND_R8), dimension(:), allocatable :: &
        xcorner, ycorner    ! x,y corner coordinates for a given cell

      ! initialize defaults, flags, etc.
      coinc   = .false.
      outside = .false.
      nLoc = 0 ! default is zero if no cell location found

      if (xend.eq.fullLine(1) .AND. yend.eq.fullLine(2)) then
        reverse = .true.
      else
        reverse = .false.
      endif

      xIntersect = xend
      yIntersect = yend

      xoff = 0.0d0
      yoff = 0.0d0

      call ESMF_LatLonToPoleXY(xb, yb, xbeg, ybeg)
      call ESMF_LatLonToPoleXY(xe, ye, xend, yend)

      numCorners = size(srchCornerX, 1)
      nCells     = size(srchCornerX, 2)
    
      allocate(xcorner(numCorners), &
               ycorner(numCorners), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "corner arrays", &
                                     ESMF_CONTEXT, rc)) return

      dx = xe - xb
      dy = ye - yb
      s1 = 0.01d0
      s2 = 0.0d0

      ! search for location of the beginning of the segment in input interngrid
      ! if a valid initial guess has been provided, check this cell now
      found = .false.

   !   do n = 1,interngrid%num_corners
   !      call ESMF_LatLonToPoleXY(xcorner(n), ycorner(n), &
   !                                  cornerX(n,i,j,k), &
   !                                  cornerY(n,i,j,k))
   !   end do
   !   found = SCRIP_crossProduct_test(xb, yb, xcorner, ycorner)
   !   if (found) return

   !
   ! if no initial guess or guess is wrong, perform search
   ! loop through interngrid to find cell containing search point
      cellLoop: do n = 1,nCells

          if (.not. mask(n)) cycle cellLoop

          ! first check cell bounding box before doing more thorough search.
          ! if point does not lie in cell bound box, skip to next cell.
          do corner = 1,numCorners
            call ESMF_LatLonToPoleXY(xcorner(corner), ycorner(corner), &
                                     srchCornerX(corner,n), srchCornerY(corner,n))
          enddo

          if (xb.lt.minval(xcorner) .OR. xb.gt.maxval(xcorner) .OR. &
              yb.lt.minval(ycorner) .OR. yb.gt.maxval(ycorner)) cycle cellLoop
          
          ! congratulations.  you have jumped through another hoop successfully.
          ! now do a full search
          cornerLoop: do corner = 1,numCorners
            nextCorner = MOD(corner,numCorners) + 1

            ! here we take the cross product of the vector making 
            ! up each cell side with the vector formed by the vertex
            ! and search point.  if all the cross products are 
            ! the same sign, the point is contained in the cell.
            x1 = xcorner(corner    )
            x2 = xcorner(nextCorner)
            y1 = ycorner(corner    )
            y2 = ycorner(nextCorner)

            vec1X = x2 - x1
            vec1Y = y2 - y1
            vec2X = xb - x1
            vec2Y = yb - y1

            ! if this side has zero length, skip the side
            if (vec1X.eq.0.0d0 .AND. vec1Y.eq.0.0d0) cycle cornerLoop

            ! if endpoint coincident with vertex, offset the endpoint before
            !   computing cross product
            if (vec2X.eq.0.0d0 .AND. vec2Y.eq.0.0d0) then
              vec2X = (xb + offset*(xe-xb)) - x1
              vec2Y = (yb + offset*(ye-yb)) - y1
            endif

            crossProduct = vec1X*vec2Y - vec2X*vec1Y

            if (crossProduct.eq.0.0d0) then

              ! if the cross product for a side is zero, the point 
              !   lies exactly on the side. perform another cross
              !   product between the side and the segment itself. 
              ! if this cross product is also zero, the line is 
              !   coincident with the cell boundary - perform the 
              !   dot product and only choose the cell if the dot 
              !   product is positive (parallel vs anti-parallel).
              vec2X = xe - xb
              vec2Y = ye - yb

              crossProduct = vec1X*vec2Y - vec2X*vec1Y

              if (crossProduct.eq.0.0d0) then
                coinc = .true.
                crossProduct = vec1X*vec2X + vec1Y*vec2Y
                if (reverse) crossProduct = -crossProduct
              endif
            endif

            ! if cross product is less than zero, this cell doesn't work
            if (crossProduct.lt.0.0d0) exit cornerLoop

          enddo cornerLoop

          ! if cross products all positive, we found the location
          if (corner.gt.numCorners) then
            found = .true.
            nLoc  = n
            go to 10
          endif

          ! otherwise move on to next cell

      enddo cellLoop

      ! cell not found - assume beg point outside interngrid
      ! take baby steps along segment to find a point inside the interngrid
      s2 = s2 + s1
      if (s2.gt.1.0d0) go to 10
      outside = .true.
      xb = xb + s1*dx
      yb = yb + s1*dy

   10 continue

      ! if the first part of the segment was outside the interngrid
      ! the found point corresponds to the first point inside the
      ! interngrid. invert the segment to find first intersection with
      ! interngrid boundary
      if (outside) then
        call ESMF_LatLonToPoleXY(xe, ye, xbeg, ybeg)
        nLoc = 0
      endif

      ! now that a cell is found, search for the next intersection.
      ! loop over sides of the cell to find intersection with side
      ! must check all sides for coincidences or intersections
      if (found) then 

        intrsctLoop: do corner = 1,numCorners
          nextCorner = mod(corner,numCorners) + 1

          x1 = xcorner(corner    )
          x2 = xcorner(nextCorner)
          y1 = ycorner(corner    )
          y2 = ycorner(nextCorner)

          ! set up linear system to solve for intersection
          mat1 = xe - xb
          mat2 = x1 - x2
          mat3 = ye - yb
          mat4 = y1 - y2
          rhs1 = x1 - xb
          rhs2 = y1 - yb

          determ = mat1*mat4 - mat2*mat3

          ! if the determinant is zero, the segments are either 
          !   parallel or coincident.  coincidences were detected 
          !   above so do nothing.
          ! if the determinant is non-zero, solve for the linear 
          !   parameters s for the intersection point on each line 
          !   segment.
          ! if 0<s1,s2<1 then the segment intersects with this side.
          !   return the point of intersection (adding a small
          !   number so the intersection is off the interngrid line).
          if (abs(determ).gt.1.d-30) then

            s1 = (rhs1*mat4 - mat2*rhs2)/determ
            s2 = (mat1*rhs2 - rhs1*mat3)/determ

            if (s2.ge.0.0d0 .AND. s2.le.1.0d0 .AND. &
                s1.gt.0.0d0 .AND. s1.le.1.0d0) then

              ! recompute intersection based on full 
              ! segment so intersections are consistent in cases
              ! where computing intersections between two interngrids
              call ESMF_LatLonToPoleXY(fullLineXY(1), fullLineXY(2), &
                                       fullLine  (1), fullLine  (2))
              call ESMF_LatLonToPoleXY(fullLineXY(3), fullLineXY(4), &
                                       fullLine  (3), fullLine  (4))

              if (.not. reverse) then
                mat1 = fullLineXY(3) - fullLineXY(1)
                mat3 = fullLineXY(4) - fullLineXY(2)
                rhs1 = x1 - fullLineXY(1)
                rhs2 = y1 - fullLineXY(2)
              else
                mat1 = fullLineXY(1) - fullLineXY(3)
                mat3 = fullLineXY(2) - fullLineXY(4)
                rhs1 = x1 - fullLineXY(3)
                rhs2 = y1 - fullLineXY(4)
              endif

              determ = mat1*mat4 - mat2*mat3

              ! sometimes due to roundoff, the previous determinant is non-zero,
              !   but the lines are actually coincident.  if this is the case,
              !   skip the rest.
              if (determ.ne.0.0d0) then
                s1 = (rhs1*mat4 - mat2*rhs2)/determ
                s2 = (mat1*rhs2 - rhs1*mat3)/determ

                xoff = abs(offset/determ)
                if ((s1 + xoff).gt.1.0d0) xoff = 1.0d0 - s1
                yoff = mat3*xoff
                xoff = mat1*xoff

                if (.not. reverse) then
                  xIntersect = fullLineXY(1) + mat1*s1
                  yIntersect = fullLineXY(2) + mat3*s1
                else
                  xIntersect = fullLineXY(3) + mat1*s1
                  yIntersect = fullLineXY(4) + mat3*s1
                endif

                ! convert back to lat/lon and set ref lat/lon to xbeg,xend
                ! use rhs1,2 for intersect lat/lon and mat1,2 for offsets
                rhs1 = xbeg
                rhs2 = ybeg
                mat1 = xbeg
                mat2 = ybeg
                call ESMF_PoleXYToLatLon(rhs1, rhs2, xIntersect, yIntersect)
                call ESMF_PoleXYToLatLon(mat1, mat2, xIntersect + xoff, &
                                                     yIntersect + yoff)
                xIntersect = rhs1
                yIntersect = rhs2
                xoff = mat1 - rhs1
                yoff = mat2 - rhs2

                exit intrsctLoop
              endif
           endif
         endif

         ! no intersection this side, move on to next side

       enddo intrsctLoop

       ! if intersection crosses pole threshold, reset intersection to
       !   threshold lat

       if ((yIntersect.gt.0.0d0 .AND. yIntersect.lt.northThreshold) .OR. &
           (yIntersect.lt.0.0d0 .AND. yIntersect.gt.southThreshold)) then

         if (yIntersect.gt.0.0d0) then
           yIntersect = northThreshold
         else
           yIntersect = southThreshold
         endif
         if (.not. reverse) then
           mat1 = fullLine(3) - fullLine(1)
           if     (mat1.gt. pi) then
             mat1 = mat1 - pi2
           elseif (mat1.lt.-pi) then
             mat1 = mat1 + pi2
           endif
           mat3 = fullLine(4) - fullLine(2)
           s1 = (yIntersect - fullLine(2))/mat3
           xIntersect = fullLine(1) + mat1*s1
         else
           mat1 = fullLine(1) - fullLine(3)
           if     (mat1.gt. pi) then
             mat1 = mat1 - pi2
           elseif (mat1.lt.-pi) then
             mat1 = mat1 + pi2
           endif
           mat3 = fullLine(2) - fullLine(4)
           s1 = (yIntersect - fullLine(4))/mat3
           xIntersect = fullLine(3) + mat1*s1
         endif
         if (xoff.eq.0.0d0) then  ! no intersection from above
           yoff = mat3*offset
           xoff = mat1*offset
         endif
       endif

      endif ! found cell

      ! clean up and exit
      deallocate(xcorner, &
                 ycorner, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate corner arrays", &
                                     ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridConservIntrsctPole

!----------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LatLonToPoleXY"
!BOPI
! !IROUTINE: ESMF_LatLonToPoleXY
! !INTERFACE:

      subroutine ESMF_LatLonToPoleXY(x, y, lon, lat)

! !ARGUMENTS:

      real (ESMF_KIND_R8), intent(in) :: lat
      real (ESMF_KIND_R8), intent(in) :: lon      
      real (ESMF_KIND_R8), intent(out) :: x
      real (ESMF_KIND_R8), intent(out) :: y 

! !DESCRIPTION:
!  Converts latitude and longitude to a Cartesian coordinate system
!  of a plane tangent to the pole.
!
!     The arguments are:
!     \begin{description}
!     \item[lat, lon]
!        Latitude and longitude coordinates of point 
!        to transform to x,y coordinates
!     \item[x, y]
!        X,Y coordinates of input point in new tangent plane coordinate
!        system.
!     \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      ! local variables
      real (ESMF_KIND_R8) :: rns           ! sign factor for transformation
      real (ESMF_KIND_R8) :: pi4           ! rns*pi/4    for transformation

      ! transformation to x,y coordinates of plane tangent to pole
      ! correct for opposite longitude angle for proper x,y orientation at
      ! south pole
      !x = cos(lat)*cos(lon)
      !y = cos(lat)*sin(lon)
      !if (lat.lt.0.0d0) y = -y

      if (lat.gt.0.0d0) then
        rns =  1.0d0
      else
        rns = -1.0d0
      endif
      pi4 = rns*0.25d0*pi

      x = rns*2.0d0*sin(pi4 - 0.5d0*lat)*cos(lon)
      y =     2.0d0*sin(pi4 - 0.5d0*lat)*sin(lon)
 
      ! all done

      end subroutine ESMF_LatLonToPoleXY

!----------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PoleXYToLatLon"
!BOPI
! !IROUTINE: ESMF_PoleXYToLatLon
! !INTERFACE:

      subroutine ESMF_PoleXYToLatLon(lon, lat, x, y)

! !ARGUMENTS:

      real (ESMF_KIND_R8), intent(inout) :: lat
      real (ESMF_KIND_R8), intent(inout) :: lon
      real (ESMF_KIND_R8), intent(in) :: x
      real (ESMF_KIND_R8), intent(in) :: y

! !DESCRIPTION:
!  Converts X,Y in a Cartesian coordinate system of a plane tangent to 
!  the pole back to latitude and longitude in spherical coords.
!
!     The arguments are:
!     \begin{description}
!     \item[x, y]
!        X,Y coordinates of input point in tangent plane coordinate
!        system.
!     \item[lat, lon]
!        Latitude and longitude coordinates of input point.
!        On input, lon contains a reference longitude to use for
!        resolving proper longitude range.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      ! local variables
      real (ESMF_KIND_R8) :: rns            ! sign factor for transformation
      real (ESMF_KIND_R8) :: pi4            ! rns*pi/4    for transformation
      real (ESMF_KIND_R8) :: lonTmp         ! temporary placeholder for input lon

      ! transformation
      if (lat.gt.0.0d0) then  ! north pole
        rns =  1.0d0
      else  ! south pole
        rns = -1.0d0
      endif
      pi4 = rns*0.25d0*pi
      lonTmp = lon  ! store input reference lon for later use
 
      lon = rns*atan2(y,x)

      if (lon.lt.0.0d0) lon = lon + pi2
 
      if     (abs(x).gt.1.d-10) then
        lat = (pi4 - asin(rns*0.5d0*x/cos(lon)))*2.0d0
      elseif (abs(y).gt.1.d-10) then
        lat = (pi4 - asin(    0.5d0*y/sin(lon)))*2.0d0
      else ! both x,y nearly zero so make this the pole point
           !  and use input reference longitude to determine proper lon
        lat = 2.0d0*pi4
        lon = lonTmp
      endif
 
      ! all done

      end subroutine ESMF_PoleXYToLatLon

!------------------------------------------------------------------------------

      end module ESMF_RegridConservMod
