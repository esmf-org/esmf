! $Id: ESMF_RegridBilinear.F90,v 1.67 2004/05/25 20:10:52 jwolfe Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
#define ESMF_FILENAME "ESMF_RegridBilinear.F90"
!
!     ESMF Bilinear Regrid Module
      module ESMF_RegridBilinearMod
!
!==============================================================================
!
! This file contains the Regrid class methods for bilinear regridding.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_RegridBilinearMod - Bilinear interpolation
!
! !DESCRIPTION:
!
! The code in this file implements the bilinear methods for the ESMF Regrid
! class.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod        ! ESMF base   class
      use ESMF_LogErrMod
      use ESMF_LocalArrayMod
      use ESMF_ArrayDataMapMod
      use ESMF_ArrayMod       ! ESMF array  class
      use ESMF_ArrayGetMod    ! ESMF array  class
      use ESMF_DistGridMod    ! ESMF distributed grid class
      use ESMF_PhysCoordMod   ! ESMF physical grid domain class
      use ESMF_PhysGridMod    ! ESMF physical grid class
      use ESMF_GridMod        ! ESMF grid   class
      use ESMF_FieldDataMapMod
      use ESMF_FieldMod       ! ESMF field  class
      use ESMF_BundleMod      ! ESMF bundle class
      use ESMF_RegridTypesMod ! ESMF regrid data structures
      implicit none

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

    public ESMF_RegridConstructBilinear ! create and fill a regrid object
                                        ! for a bilinear regridding
    public ESMF_RegridBilinearSearch
!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RegridBilinear.F90,v 1.67 2004/05/25 20:10:52 jwolfe Exp $'

!==============================================================================

      contains

!==============================================================================
!
! This section includes the bilinear Regrid construct methods.
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridConstructBilinear"
!BOPI
! !IROUTINE: ESMF_RegridConstructBilinear - Constructs bilinear Regrid structure 

! !INTERFACE:
      function ESMF_RegridConstructBilinear(srcArray, srcGrid, srcDataMap, &
                                            dstArray, dstGrid, dstDataMap, &
                                            parentDELayout, srcMask, dstMask, rc)
!
! !RETURN VALUE:
      type(ESMF_RouteHandle) :: ESMF_RegridConstructBilinear
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: srcArray
      type(ESMF_Grid), intent(in) :: srcGrid
      type(ESMF_FieldDataMap), intent(in) :: srcDataMap
      type(ESMF_Array), intent(in) :: dstArray
      type(ESMF_Grid), intent(in) :: dstGrid
      type(ESMF_FieldDataMap), intent(in) :: dstDataMap
      type(ESMF_DELayout), intent(in) :: parentDELayout
      type(ESMF_Mask), intent(in), optional :: srcMask
      type(ESMF_Mask), intent(in), optional :: dstMask
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a source field and destination field (and their attached
!     grids), this routine constructs a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     field to the destination field using a bilinear interpolation.  
!     Returns a pointer to a new {\tt Regrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[srcArray]
!          Field to be regridded.
!     \item[dstArray]
!          Resultant field where regridded source field will be stored.
! \item[TODO:]  make match actual arglist
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \item[[srcmask]]
!          Optional mask to specify or eliminate source points from
!          regridding.  Default is that all source points participate. 
!     \item[[dstmask]]
!          Optional mask to specify or eliminate destination points from
!          regridding.  Default is that all destination points participate. 
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      integer :: localrc                          ! Error status
      logical :: hassrcdata        ! does this DE contain localdata from src?
      logical :: hasdstdata        ! does this DE contain localdata from dst?
      integer :: start, stop, startComp, stopComp, indexMod(2)
      integer :: srcSizeX, srcSizeY, srcSizeXComp, srcSizeYComp, size
      integer :: i, numDomains, dstCounts(3), srcCounts(3)
      integer :: datarank
      integer, dimension(3) :: srcOrder, dstOrder
      logical, dimension(:), pointer :: srcUserMask, dstUserMask
      logical, dimension(:,:), pointer :: found
      integer(ESMF_KIND_I4), dimension(:,:), pointer :: foundCount, srcLocalMask
      integer(ESMF_KIND_I4), dimension(:), pointer :: srcGatheredMask
      real(ESMF_KIND_R8), dimension(:), pointer :: srcGatheredCoordX, &
                                                   srcGatheredCoordY
      real(ESMF_KIND_R8), dimension(:,:), pointer :: srcLocalCoordX, &
                                                     srcLocalCoordY
      real(ESMF_KIND_R8), dimension(:,:), pointer :: dstLocalCoordX, &
                                                     dstLocalCoordY
      type(ESMF_CoordSystem) :: coordSystem
      type(ESMF_Array) :: srcMaskArray
      type(ESMF_Array), dimension(:), pointer :: dstLocalCoordArray
      type(ESMF_Array), dimension(:), pointer :: srcLocalCoordArray
      type(ESMF_DomainList) :: recvDomainList, recvDomainListTot
      type(ESMF_RelLoc) :: srcRelLoc, dstRelLoc
      type(ESMF_Route) :: route, tempRoute
      type(ESMF_RouteHandle) :: rh
      type(ESMF_Regrid) :: tempRegrid
      type(ESMF_TransformValues) :: tv

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Construct an empty regrid structure
      rh = ESMF_RouteHandleCreate(rc=localrc)
 !      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return
      tempRegrid = ESMF_RegridCreate(rc=localrc)
 !      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return

      ! Set name and field pointers
      call ESMF_RegridSet(tempRegrid, srcArray=srcArray, dstArray=dstArray, &
                          method = ESMF_REGRID_METHOD_BILINEAR, rc=localrc)
 !      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return

      ! set reordering information
      srcOrder(:) = gridOrder(:,srcGrid%ptr%coordOrder%order,2)
      dstOrder(:) = gridOrder(:,dstGrid%ptr%coordOrder%order,2)

      ! get destination grid info
      !TODO: Get grid masks?
      call ESMF_FieldDataMapGet(dstDataMap, horzRelloc=dstRelLoc, rc=localrc)
 !      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return

      dstCounts(3) = 2
      call ESMF_GridGetDE(dstGrid, horzRelLoc=dstRelLoc, &
                          localCellCountPerDim=dstCounts(1:2), &
                          reorder=.false., rc=localrc)
 !      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return

      allocate(dstLocalCoordArray(2))    ! TODO:
      call ESMF_GridGetCoord(dstGrid, horzRelLoc=dstRelLoc, &
                             centerCoord=dstLocalCoordArray, &
                             reorder=.false., rc=localrc)
 !      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return

      call ESMF_ArrayGetData(dstLocalCoordArray(1), dstLocalCoordX, &
                             ESMF_DATA_REF, localrc)
      call ESMF_ArrayGetData(dstLocalCoordArray(2), dstLocalCoordY, &
                             ESMF_DATA_REF, localrc)

      ! get source grid info
      call ESMF_FieldDataMapGet(srcDataMap, horzRelloc=srcRelLoc, rc=localrc)
 !      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return

      srcCounts(3) = 2
      call ESMF_GridGetDE(srcGrid, horzRelLoc=srcRelLoc, &
                          localCellCountPerDim=srcCounts(1:2), &
                          reorder=.false., rc=localrc)
 !      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return

      allocate(srcLocalCoordArray(2))   ! TODO
      call ESMF_GridGetCoord(srcGrid, horzRelLoc=srcRelLoc, &
                             centerCoord=srcLocalCoordArray, &
                             reorder=.false., total=.true., rc=localrc)
 !      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return

      call ESMF_ArrayGetData(srcLocalCoordArray(1), srcLocalCoordX, &
                             ESMF_DATA_REF, localrc)
      call ESMF_ArrayGetData(srcLocalCoordArray(2), srcLocalCoordY, &
                             ESMF_DATA_REF, localrc)

      call ESMF_GridGetCellMask(srcGrid, srcMaskArray, relloc=srcRelLoc, &
                                rc=localrc)
      call ESMF_ArrayGetData(srcMaskArray, srcLocalMask, ESMF_DATA_REF, &
                             localrc)

      hassrcdata = .true.   ! temp for now
      hasdstdata = .true.   ! temp for now

   ! Calculate two separate Routes:
   !    the first will be used in the code to gather the data for running
   !              the regrid, saved in routehandle
   !    the second sends and receives total cell coordinate information and
   !              is used internal to this routine to get coordinate 
   !              information locally to calculate the regrid weights

      call ESMF_ArrayGet(srcArray, rank=datarank, rc=localrc)
      route = ESMF_RegridRouteConstruct(datarank, srcGrid, dstGrid, &
                                      recvDomainList, parentDELayout, &
                                      srcArray=srcArray, srcDataMap=srcDataMap, &
                                      dstArray=dstArray, dstDataMap=dstDataMap, &
                                      total=.false., rc=localrc)
 !      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return

      call ESMF_RouteHandleSet(rh, route1=route, rc=localrc)

      ! just do this to get a recDomainList with the right rank -- could be
      ! different using arrays
      tempRoute = ESMF_RegridRouteConstruct(2, srcGrid, dstGrid, &
                                            recvDomainList, parentDELayout, &
                                            srcDataMap=srcDataMap, &
                                            dstDataMap=dstDataMap, &
                                            reorder=.false., total=.false., &
                                            rc=localrc)
 !      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return

      ! but this is the one we want to use for gathering grid data
      tempRoute = ESMF_RegridRouteConstruct(2, srcGrid, dstGrid, &
                                            recvDomainListTot, parentDELayout, &
                                            srcDataMap=srcDataMap, &
                                            dstDataMap=dstDataMap, &
                                            reorder=.false., total=.true., &
                                            rc=localrc)
 !      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return

      ! Now use temporary route to gather necessary coordinates
      ! Create arrays for gathered coordinates 
      call ESMF_RouteGetRecvItems(tempRoute, size, localrc)
 !      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return

      allocate(srcGatheredCoordX(size))
      allocate(srcGatheredCoordY(size))
      allocate(srcGatheredMask(size))

      ! Execute Route now to gather grid center coordinates from source
      ! These arrays are just wrappers for the local coordinate data
      call ESMF_RouteRunF90PtrR821D(tempRoute, srcLocalCoordX, &
                                    srcGatheredCoordX, localrc)
 !      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return
      call ESMF_RouteRunF90PtrR821D(tempRoute, srcLocalCoordY, &
                                    srcGatheredCoordY, localrc)
 !      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return
      call ESMF_RouteRunF90PtrI421D(tempRoute, srcLocalMask, &
                                    srcGatheredMask, localrc)
 !      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return

      ! now all necessary data is local

      ! TODO: the *4 is to guarantee the max allocation possible is enough
      !  for bilinear interpolation.  eventually the addlinks routine should
      !  grow the arrays internally.
      size = ((dstCounts(1)*dstCounts(2)) + 1) * 4
      ! Create a Transform Values object
      tv = ESMF_TransformValuesCreate(size, rc)

      ! set up user masks and logical found arrays for search
      allocate(found(dstCounts(1),dstCounts(2)))
      allocate(foundCount(dstCounts(1),dstCounts(2)))
      found = .FALSE.
      foundCount = 0

      if (present(dstMask)) then
  !      dstUserMask = dstMask
      else
        allocate(dstUserMask(dstCounts(1)*dstCounts(2)))
        dstUserMask = .TRUE.
      endif
      if (present(srcMask)) then
  !      srcUserMask = srcMask
      else
        call ESMF_RouteGetRecvItems(tempRoute, size, localrc)
        allocate(srcUserMask(size))
        srcUserMask = .TRUE.
      endif
     
      ! Loop through domains for the search routine
      call ESMF_GridGet(srcGrid, horzCoordSystem=coordSystem, rc=localrc)
 !      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return

      numDomains = recvDomainListTot%num_domains
      indexMod = -1
      start = 1
      startComp = 1
      do i = 1,numDomains
        srcSizeXComp = recvDomainList%domains(i)%ai(1)%max &
                     - recvDomainList%domains(i)%ai(1)%min + 1
        srcSizeYComp = recvDomainList%domains(i)%ai(2)%max &
                     - recvDomainList%domains(i)%ai(2)%min + 1
        stopComp  = startComp + srcSizeXComp*srcSizeYComp - 1
        srcSizeX = recvDomainListTot%domains(i)%ai(1)%max &
                 - recvDomainListTot%domains(i)%ai(1)%min + 1
        srcSizeY = recvDomainListTot%domains(i)%ai(2)%max &
                 - recvDomainListTot%domains(i)%ai(2)%min + 1
        stop  = start + srcSizeX*srcSizeY - 1
        call ESMF_RegridBilinearSearch(tv, recvDomainListTot%domains(i), &
                                       coordSystem, srcSizeX, srcSizeY, &
                                       startComp-1, srcSizeXComp, &
                                       dstCounts(1), dstCounts(2), &
                                       indexMod, srcOrder, dstOrder, &
                                       found, foundCount, &
                                       srcGatheredCoordX(start:stop), &
                                       srcGatheredCoordY(start:stop), &
                                       dstLocalCoordX, dstLocalCoordY, &
                                       srcGatheredMask(start:stop), &
                                       srcUserMask(start:stop), dstUserMask, &
                                       localrc)
        start = stop + 1 
        startComp = stopComp + 1 
      enddo 

      call ESMF_RouteHandleSet(rh, tdata=tv, rc=localrc)
 !      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return

      ! clean up
      call ESMF_RouteDestroy(tempRoute, localrc)
      deallocate(srcGatheredCoordX)
      deallocate(srcGatheredCoordY)
      deallocate(srcGatheredMask)
      deallocate(dstLocalCoordArray)
      deallocate(srcLocalCoordArray)
      deallocate(found)
      deallocate(foundCount)
      deallocate(dstUserMask)
      deallocate(srcUserMask)
      
      ESMF_RegridConstructBilinear = rh

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_RegridConstructBilinear

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridBilinearSearch"
!BOPI
! !IROUTINE: ESMF_RegridBilinearSearch - Searches a bilinear Regrid structure

! !INTERFACE:
      subroutine ESMF_RegridBilinearSearch(tv, domain, coordSystem, &
                                           srcSizeX, srcSizeY, &
                                           srcStart, srcICount, &
                                           dstSizeX, dstSizeY, &
                                           indexMod, srcOrder, dstOrder, &
                                           found, foundCount, &
                                           srcCenterX, srcCenterY, &
                                           dstCenterX, dstCenterY, &
                                           srcGridMask, &
                                           srcUserMask, dstUserMask, rc)
!
! !ARGUMENTS:
      type(ESMF_TransformValues), intent(inout) :: tv
      type(ESMF_Domain), intent(in) :: domain
      type(ESMF_CoordSystem), intent(in) :: coordSystem
      integer, intent(in) :: srcSizeX  ! apparently these have to be first
      integer, intent(in) :: srcSizeY  ! so the compiler knows they're ints
      integer, intent(in) :: srcStart  ! when it goes to use them as dims
      integer, intent(in) :: srcICount
      integer, intent(in) :: dstSizeX  ! in the lines below.
      integer, intent(in) :: dstSizeY
      integer, dimension(:), intent(in) :: indexMod
      integer, dimension(:), intent(in) :: srcOrder
      integer, dimension(:), intent(in) :: dstOrder
      logical, dimension(dstSizeX,dstSizeY), intent(inout) :: found
      integer, dimension(dstSizeX,dstSizeY), intent(inout) :: foundCount
      real(ESMF_KIND_R8), dimension(srcSizeX,srcSizeY), intent(in) :: srcCenterX
      real(ESMF_KIND_R8), dimension(srcSizeX,srcSizeY), intent(in) :: srcCenterY
      real(ESMF_KIND_R8), dimension(dstSizeX,dstSizeY), intent(in) :: dstCenterX
      real(ESMF_KIND_R8), dimension(dstSizeX,dstSizeY), intent(in) :: dstCenterY
      integer, dimension(srcSizeX,srcSizeY), intent(in) :: srcGridMask
      logical, dimension(srcSizeX,srcSizeY), intent(in) :: srcUserMask
      logical, dimension(dstSizeX,dstSizeY), intent(in) :: dstUserMask
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a source field and destination field (and their attached
!     grids), this routine constructs a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     field to the destination field using a bilinear interpolation.
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
!          {\tt Regrid} name.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \item[[srcUserMask]]
!          Optional user-defined mask to specify or eliminate source points from
!          regridding.  Default is that all source points participate.
!     \item[[dstUserMmask]]
!          Optional user-defined mask to specify or eliminate destination points
!          from regridding.  Default is that all destination points participate.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      character(len=ESMF_MAXSTR) :: logMsg

      integer :: localrc                          ! Error status
      integer :: i, j, iter                       ! loop counters
      integer :: iii, jjj                         ! more loop counters
      integer :: ip1, jp1                         ! neighbor indices
      integer :: ibDst, ieDst                     ! beg, end of excl domain in
                                                  ! i-dir of dest grid
      integer :: jbDst, jeDst                     ! beg, end of excl domain in
                                                  ! j-dir of dest grid
      integer :: ibSrc, ieSrc                     ! beg, end of excl domain in
                                                  ! i-dir of source grid
      integer :: jbSrc, jeSrc                     ! beg, end of excl domain in
                                                  ! j-dir of source grid
      integer :: srcCount, srcValidCount
      integer :: srcAdd, dstAdd(2), srcTmp(2)     ! address in gathered source
                                                  ! and grid address in dest grid
         
      real (ESMF_KIND_R8) :: lonThresh
                           ! threshold for checking longitude crossing
      real (ESMF_KIND_R8) :: lonCycle
                           ! 360 for degrees, 2pi for radians
      real (ESMF_KIND_R8) :: dx1, dx2, dx3, dy1, dy2, dy3
                           ! differences for iterative scheme
      real (ESMF_KIND_R8) :: iguess, jguess
                           ! initial guess for location within grid box
      real (ESMF_KIND_R8) :: deli, delj
                           ! change in i,j position from last iteration
      real (ESMF_KIND_R8) :: mat1, mat2
                           ! matrix elements for 2x2 matrix in iterative scheme
      real (ESMF_KIND_R8) :: mat3, mat4
      real (ESMF_KIND_R8) :: dxp, dyp
      real (ESMF_KIND_R8) :: dstX, dstY
      real (ESMF_KIND_R8) :: determinant
      real (ESMF_KIND_R8) :: sumWts, zero, half, one, pi
      real (ESMF_KIND_R8), dimension(4) :: srcX
                                           ! x coordinate of bilinear box corners
      real (ESMF_KIND_R8), dimension(4) :: srcY
                                           ! y coordinate of bilinear box corners
      real (ESMF_KIND_R8), dimension(4) :: weights
                                           ! bilinear weights for single box

      integer, parameter :: maxIter = 100                  ! max iteration count
                                                           ! for i,j iteration
      real (ESMF_KIND_R8), parameter :: converge = 1.e-10  ! convergence criterion

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      zero = 0.
      half = 0.5
      one  = 1.
      pi   = 3.1416
 !     if (dst_phys_grid%coord_system == ESMF_COORD_SYSTEM_SPHERICAL) then
 !        if (units = 'degrees') then
 !           lonThresh = 270.0
 !           lonCycle  = 360.0
 !        else if (units = 'radians') then
 !           lonThresh = 1.5*pi
 !           lonCycle  = 2.0*pi
 !        endif
 !     endif

      ibDst = 1
      ieDst = dstSizeX
      jbDst = 1
      jeDst = dstSizeY
      ibSrc = domain%ai(1)%min
      ieSrc = domain%ai(1)%max - 1
      jbSrc = domain%ai(2)%min
      jeSrc = domain%ai(2)%max - 1

      do j=jbDst,jeDst
      do i=ibDst,ieDst
        dstX = dstCenterX(i,j)
        dstY = dstCenterY(i,j)
            
        ! only perform interpolation on un-masked and un-found points
        if (dstUserMask(i,j) .and. (.not.found(i,j))) then
          ! for this destination point, look for the proper neighbor cells in the
          ! source grid 
          search_loop: do jjj=jbSrc,jeSrc
            do iii=ibSrc,ieSrc
               
              ! assume ghost cells filled so no worries about boundaries
              ip1 = iii + 1
              jp1 = jjj + 1

              ! set up box used for bilinear interpolation
              srcX(1) = srcCenterX(iii,jjj)
              srcY(1) = srcCenterY(iii,jjj)
              srcX(2) = srcCenterX(ip1,jjj)
              srcY(2) = srcCenterY(ip1,jjj)
              srcX(3) = srcCenterX(ip1,jp1)
              srcY(3) = srcCenterY(ip1,jp1)
              srcX(4) = srcCenterX(iii,jp1)
              srcY(4) = srcCenterY(iii,jp1)

              ! check longitude domain in spherical coords
       !       if (dst_grid%coord_system == ESMF_COORD_SYSTEM_SPHERICAL) then
       !         if (srcX(1) - dstX >  lonThresh) &
       !             srcX(1) = srcX(1) - lonCycle
       !         if (srcX(1) - dstX < -lonThresh) &
       !             srcX(1) = srcX(1) + lonCycle
       !         if (srcX(2) - dstX >  lonThresh) &
       !             srcX(2) = srcX(2) - lonCycle
       !         if (srcX(2) - dstX < -lonThresh) &
       !             srcX(2) = srcX(2) + lonCycle
       !         if (srcX(3) - dstX >  lonThresh) &
       !             srcX(3) = srcX(3) - lonCycle
       !         if (srcX(3) - dstX < -lonThresh) &
       !             srcX(3) = srcX(3) + lonCycle
       !         if (srcX(4) - dstX >  lonThresh) &
       !             srcX(4) = srcX(4) - lonCycle
       !         if (srcX(4) - dstX < -lonThresh) &
       !             srcX(4) = srcX(4) + lonCycle
       !       endif

              ! check to see if point inside cell
              found(i,j) = ESMF_PhysGridPointInCell(dstX, dstY, srcX, srcY, localrc)

              if (found(i,j)) exit search_loop
            enddo            ! iii-loop on src DE
          enddo search_loop  ! jjj-loop on src DE
               
          ! if we've found a bilinear box containing the point continue with
          ! computation of weights
          if (found(i,j)) then

            ! check to see if src masks are true at all points
            ! check grid mask to see if it's a ghost cell
            srcCount = 0
            if (srcUserMask(iii,jjj) .and. (srcGridMask(iii,jjj).ne.1)) &
                srcCount = srcCount + 1
            if (srcUserMask(ip1,jjj) .and. (srcGridMask(ip1,jjj).ne.1)) &
                srcCount = srcCount + 1
            if (srcUserMask(iii,jp1) .and. (srcGridMask(iii,jp1).ne.1)) &
                srcCount = srcCount + 1
            if (srcUserMask(ip1,jp1) .and. (srcGridMask(ip1,jp1).ne.1)) &
                srcCount = srcCount + 1

            ! if all four are valid points, compute bilinear weights using
            ! iterative method
            if (srcCount == 4) then

              dx1 = srcX(2) - srcX(1)
              dx2 = srcX(4) - srcX(1)
              dx3 = srcX(3) - srcX(2) - dx2
              dy1 = srcY(2) - srcY(1)
              dy2 = srcY(4) - srcY(1)
              dy3 = srcY(3) - srcY(2) - dy2

              iguess = half
              jguess = half

              iter_loop1: do iter=1,maxIter

                ! compute deviation between search point and current guess
                dxp = dstX - srcX(1) - dx1*iguess - dx2*jguess &
                                     - dx3*iguess*jguess
                dyp = dstY - srcY(1) - dy1*iguess - dy2*jguess &
                                     - dy3*iguess*jguess
                                          
                ! compute matrix elements and determinant for deli, delj system
                ! of equations
                mat1 = dx1 + dx3*jguess
                mat2 = dx2 + dx3*iguess
                mat3 = dy1 + dy3*jguess
                mat4 = dy2 + dy3*iguess
                determinant = mat1*mat4 - mat2*mat3

                ! solve 2x2 system for deli, delj
                deli = (dxp*mat4 - dyp*mat2)/determinant
                delj = (dyp*mat1 - dxp*mat3)/determinant

                if (abs(deli) < converge .and. &
                    abs(delj) < converge) exit iter_loop1

                iguess = iguess + deli
                jguess = jguess + delj

              enddo iter_loop1

              if (iter <= maxIter) then

                !*** successfully found i,j - compute weights
                weights(1) = (one-iguess)*(one-jguess)
                weights(2) = iguess*(one-jguess)
                weights(3) = iguess*jguess
                weights(4) = (one-iguess)*jguess

              else  ! iteration failed
                print logMsg, "ERROR in ESMF_RegridBilinearSearch"
        !       call ESMF_LogWrite(logMsg, ESMF_LOG_ERROR)
                print logMsg, "Point coords: ",dstX,dstY
        !       call ESMF_LogWrite(logMsg, ESMF_LOG_INFO)
                print logMsg, "Source cell coords: ",srcX(:),srcY(:)
        !       call ESMF_LogWrite(logMsg, ESMF_LOG_INFO)
                print logMsg, "Current i,j : ",iguess, jguess
        !       call ESMF_LogWrite(logMsg, ESMF_LOG_INFO)
                print logMsg, "Iteration for i,j exceed max iteration count"
        !       call ESMF_LogWrite(logMsg, ESMF_LOG_INFO)
                if (present(rc)) rc = ESMF_RC_ARG_VALUE
                return
              endif

              ! count number of points that are not in the grid "halo" region
              ! Halos are still valid points for computing weights but should
              ! not be assigned weights or marked found.  Keep track of the total
              ! number of valid source points 
              ! TODO: make the grid mask use values that are more meaningful
              srcValidCount = 0
              if (srcGridMask(iii,jjj).eq.0) srcValidCount = srcValidCount + 1
              if (srcGridMask(ip1,jjj).eq.0) srcValidCount = srcValidCount + 1
              if (srcGridMask(iii,jp1).eq.0) srcValidCount = srcValidCount + 1
              if (srcGridMask(ip1,jp1).eq.0) srcValidCount = srcValidCount + 1
    
              if ((foundCount(i,j)+srcValidCount) .ne. srcCount) then
                found(i,j) = .false.
                foundCount(i,j) = foundCount(i,j) + srcValidCount
              endif

            ! if not all four points in box are valid (unmasked) default to a
            ! inversely distance-weighted average
            else if (srcCount > 0 .and. srcCount < 4) then

              sumWts = zero
              if (srcUserMask(iii,jjj) .and. (srcGridMask(iii,jjj).ne.1)) then
                weights(1) = ESMF_GridComputeDistance(srcX(1),srcY(1), &
                                                      dstX, dstY,      &
                                                      coordSystem, localrc)
                if (weights(1).eq.0.0) then
                  weights(1) = 1.0e20   ! TODO: exit loop instead
                else
                  weights(1) = 1./weights(1)
                endif
                sumWts = sumWts + weights(1)
              endif
              if (srcUserMask(ip1,jjj) .and. (srcGridMask(ip1,jjj).ne.1)) then
                weights(2) = ESMF_GridComputeDistance(srcX(2),srcY(2), &
                                                      dstX, dstY,      &
                                                      coordSystem, localrc)
                if (weights(2).eq.0.0) then
                  weights(2) = 1.0e20   ! TODO: exit loop instead
                else
                  weights(2) = 1./weights(2)
                endif
                sumWts = sumWts + weights(2)
              endif
              if (srcUserMask(ip1,jp1) .and. (srcGridMask(ip1,jp1).ne.1)) then
                weights(3) = ESMF_GridComputeDistance(srcX(3),srcY(3), &
                                                      dstX, dstY,      &
                                                      coordSystem, localrc)
                if (weights(3).eq.0.0) then
                  weights(3) = 1.0e20   ! TODO: exit loop instead
                else
                  weights(3) = 1./weights(3)
                endif
                sumWts = sumWts + weights(3)
              endif
              if (srcUserMask(iii,jp1) .and. (srcGridMask(iii,jp1).ne.1)) then
                weights(4) = ESMF_GridComputeDistance(srcX(4),srcY(4), &
                                                      dstX, dstY,      &
                                                      coordSystem, localrc)
                if (weights(4).eq.0.0) then
                  weights(4) = 1.0e20   ! TODO: exit loop instead
                else
                  weights(4) = 1./weights(4)
                endif
                sumWts = sumWts + weights(4)
              endif
              weights(:) = weights(:)/sumWts

              ! Halos are still valid points for computing weights but should
              ! not be assigned weights or marked found.  Keep track of the total
              ! number of valid computational source points 
              srcValidCount = 0
              if (srcGridMask(iii,jjj).eq.0) srcValidCount = srcValidCount + 1
              if (srcGridMask(ip1,jjj).eq.0) srcValidCount = srcValidCount + 1
              if (srcGridMask(iii,jp1).eq.0) srcValidCount = srcValidCount + 1
              if (srcGridMask(ip1,jp1).eq.0) srcValidCount = srcValidCount + 1
    
              if ((foundCount(i,j)+srcValidCount) .ne. srcCount) then
                found(i,j) = .false.
                foundCount(i,j) = foundCount(i,j) + srcValidCount
              endif

            endif

            ! now store this link into address, weight arrays
            if (srcUserMask(iii,jjj) .and. (srcGridMask(iii,jjj).eq.0)) then
              dstAdd(dstOrder(1)) = i
              dstAdd(dstOrder(2)) = j
              srcTmp(srcOrder(1)) = iii+indexMod(1)
              srcTmp(srcOrder(2)) = jjj+indexMod(2)
              srcAdd = (srcTmp(2)-1)*srcICount + srcTmp(1) + srcStart
              call ESMF_RegridAddLink(tv, srcAdd, dstAdd, weights(1), rc=rc)
            endif
            if (srcUserMask(ip1,jjj) .and. (srcGridMask(ip1,jjj).eq.0)) then
              dstAdd(dstOrder(1)) = i
              dstAdd(dstOrder(2)) = j
              srcTmp(srcOrder(1)) = ip1+indexMod(1)
              srcTmp(srcOrder(2)) = jjj+indexMod(2)
              srcAdd = (srcTmp(2)-1)*srcICount + srcTmp(1) + srcStart
              call ESMF_RegridAddLink(tv, srcAdd, dstAdd, weights(2), rc=rc)
            endif
            if (srcUserMask(ip1,jp1) .and. (srcGridMask(ip1,jp1).eq.0)) then
              dstAdd(dstOrder(1)) = i
              dstAdd(dstOrder(2)) = j
              srcTmp(srcOrder(1)) = ip1+indexMod(1)
              srcTmp(srcOrder(2)) = jp1+indexMod(2)
              srcAdd = (srcTmp(2)-1)*srcICount + srcTmp(1) + srcStart
              call ESMF_RegridAddLink(tv, srcAdd, dstAdd, weights(3), rc=rc)
            endif
            if (srcUserMask(iii,jp1) .and. (srcGridMask(iii,jp1).eq.0)) then
              dstAdd(dstOrder(1)) = i
              dstAdd(dstOrder(2)) = j
              srcTmp(srcOrder(1)) = iii+indexMod(1)
              srcTmp(srcOrder(2)) = jp1+indexMod(2)
              srcAdd = (srcTmp(2)-1)*srcICount + srcTmp(1) + srcStart
              call ESMF_RegridAddLink(tv, srcAdd, dstAdd, weights(4), rc=rc)
            endif

          endif ! found box
        endif ! dst mask            
      enddo ! i loop on dst grid DE
      enddo ! j loop on dst grid DE

      if (present(rc)) rc = ESMF_SUCCESS
      
      end subroutine ESMF_RegridBilinearSearch

!------------------------------------------------------------------------------

   end module ESMF_RegridBilinearMod
