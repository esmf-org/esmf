! $Id: ESMF_RegridBilinear.F90,v 1.35 2003/10/16 23:15:27 jwolfe Exp $
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
!     ESMF Bilinear Regrid Module
      module ESMF_RegridBilinearMod
!
!==============================================================================
!
! This file contains the Regrid class methods for bilinear regridding.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_Macros.inc>
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
      use ESMF_DELayoutMod
      use ESMF_LocalArrayMod
      use ESMF_ArrayBaseMod   ! ESMF array  class
      use ESMF_ArrayExpandMod ! ESMF array  class
      use ESMF_PhysCoordMod   ! ESMF physical grid domain class
      use ESMF_PhysGridMod    ! ESMF physical grid class
      use ESMF_DistGridMod    ! ESMF distributed grid class
      use ESMF_DataMapMod
      use ESMF_GridMod        ! ESMF grid   class
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
      '$Id: ESMF_RegridBilinear.F90,v 1.35 2003/10/16 23:15:27 jwolfe Exp $'

!==============================================================================

      contains

!==============================================================================
!
! This section includes the bilinear Regrid construct methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridConstructBilinear - Constructs bilinear Regrid structure 

! !INTERFACE:
      function ESMF_RegridConstructBilinear(srcArray, srcGrid, srcDataMap, &
                                            dstArray, dstGrid, dstDataMap, &
                                            srcMask, dstMask, blocking, rc)
!
! !RETURN VALUE:
      type(ESMF_RouteHandle) :: ESMF_RegridConstructBilinear
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: srcArray
      type(ESMF_Grid), intent(in) :: srcGrid
      type(ESMF_DataMap), intent(in) :: srcDataMap
      type(ESMF_Array), intent(inout) :: dstArray
      type(ESMF_Grid), intent(in) :: dstGrid
      type(ESMF_DataMap), intent(in) :: dstDataMap
      type(ESMF_Mask), intent(in), optional :: srcMask
      type(ESMF_Mask), intent(in), optional :: dstMask
      type(ESMF_Async), intent(inout), optional :: blocking
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
!     \item[[srcmask]]
!          Optional mask to specify or eliminate source points from
!          regridding.  Default is that all source points participate. 
!     \item[[dstmask]]
!          Optional mask to specify or eliminate destination points from
!          regridding.  Default is that all destination points participate. 
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      logical :: hassrcdata        ! does this DE contain localdata from src?
      logical :: hasdstdata        ! does this DE contain localdata from dst?
      integer :: start, stop, startComp, stopComp, my_DE, indexMod(2)
      integer :: srcSizeX, srcSizeY, size
      integer :: i, j, num_domains, dstCounts(3), srcCounts(3), ij
      logical, dimension(:), pointer :: srcUserMask, dstUserMask
      logical, dimension(:,:), pointer :: found
      integer, dimension(:,:), pointer :: foundCount, srcLocalMask
      integer, dimension(:), pointer :: srcGatheredMask
      real(ESMF_KIND_R8), dimension(:), pointer :: srcGatheredCoordX, srcGatheredCoordY
      real(ESMF_KIND_R8), dimension(:,:), pointer :: srcLocalCoordX, srcLocalCoordY
      real(ESMF_KIND_R8), dimension(:,:), pointer :: dstLocalCoordX, dstLocalCoordY
      real(ESMF_KIND_R8), dimension(:), pointer :: tempCrud
      real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: dstMin, dstMax
      real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: srcMin, srcMax
      type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: myAI, myTotalAI
      type(ESMF_CoordSystem) :: coordSystem
      type(ESMF_DataType) :: type
      type(ESMF_DataKind) :: kind
      type(ESMF_LocalArray) :: srcindex, dstindex, weights
      type(ESMF_LocalArray) :: srcLocalCoordXArray, srcLocalCoordYArray
      type(ESMF_LocalArray) :: srcGatheredCoordXArray, srcGatheredCoordYArray
      type(ESMF_Array) :: srcMaskArray
      type(ESMF_LocalArray) :: srcLocalMaskArray, srcGatheredMaskArray
      type(ESMF_Array), dimension(:), pointer :: dstLocalCoordArray
      type(ESMF_Array), dimension(:), pointer :: srcLocalCoordArray
      type(ESMF_DomainList) :: sendDomainList, recvDomainList
      type(ESMF_DomainList) :: sendDomainListTot, recvDomainListTot
      type(ESMF_DELayout) :: srcDELayout
      type(ESMF_RelLoc) :: srcRelLoc, dstRelLoc
      type(ESMF_Route) :: route, tempRoute
      type(ESMF_RouteHandle) :: rh
      type(ESMF_RegridType) :: temp_regrid
      type(ESMF_TransformValues) :: tv
      character (len = ESMF_MAXSTR) :: name

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Construct an empty regrid structure
      rh = ESMF_RouteHandleCreate(rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: RouteHandleCreate ", &
                 "returned failure"
        return
      endif

      ! Set name and field pointers
      call ESMF_RegridTypeSet(temp_regrid,                                   &
                              name=name, srcArray = srcArray,                &
                                         dstArray = dstArray,                &
                                         method = ESMF_RegridMethod_Bilinear, &
                                         rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: RegridTypeSet ", &
                 "returned failure"
        return
      endif
      
      ! Extract some layout information for use in this regrid.
      call ESMF_GridGetDE(srcGrid, ai_global=myAI, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: GridGetDE ", &
                 "returned failure"
        return
      endif
      call ESMF_GridGetDE(srcGrid, ai_global=myTotalAI, total=.true., rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: GridGetDE ", &
                 "returned failure"
        return
      endif
      call ESMF_GridGetDELayout(srcGrid, srcDELayout, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: GridGetDELayout ", &
                 "returned failure"
        return
      endif
      call ESMF_DELayoutGetDEID(srcDELayout, my_DE, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: DELayoutGetDEID ", &
                 "returned failure"
        return
      endif

      ! get destination grid info
      !TODO: Get grid masks?
      call ESMF_DataMapGet(dstDataMap, relloc=dstRelLoc, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: DataMapGetRelloc ", &
                 "returned failure"
        return
      endif
      dstCounts(3) = 2
      call ESMF_GridGetDE(dstGrid, local_axis_length=dstCounts(1:2), rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: GridGetDE ", &
                 "returned failure"
        return
      endif
      allocate(dstLocalCoordArray(2))    ! TODO:
      call ESMF_GridGetCoord(dstGrid, relloc=dstRelLoc, &
                             centerCoord=dstLocalCoordArray, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: GridGetCoord ", &
                 "returned failure"
        return
      endif
      call ESMF_ArrayGetData(dstLocalCoordArray(1), dstLocalCoordX, &
                             ESMF_DATA_REF, status)
      call ESMF_ArrayGetData(dstLocalCoordArray(2), dstLocalCoordY, &
                             ESMF_DATA_REF, status)

      ! get source grid info
      call ESMF_DataMapGet(srcDataMap, relloc=srcRelLoc, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: DataMapGetRelloc ", &
                 "returned failure"
        return
      endif
      srcCounts(3) = 2
      call ESMF_GridGetDE(srcGrid, local_axis_length=srcCounts(1:2), rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: GridGetDE ", &
                 "returned failure"
        return
      endif

      allocate(srcLocalCoordArray(2))   ! TODO
      call ESMF_GridGetCoord(srcGrid, relloc=srcRelLoc, &
                             centerCoord=srcLocalCoordArray, &
                             total=.true., rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: GridGetCoord ", &
                 "returned failure"
        return
      endif
      call ESMF_ArrayGetData(srcLocalCoordArray(1), srcLocalCoordX, &
                             ESMF_DATA_REF, status)
      call ESMF_ArrayGetData(srcLocalCoordArray(2), srcLocalCoordY, &
                             ESMF_DATA_REF, status)

      call ESMF_GridGetCellMask(srcGrid, srcMaskArray, relloc=srcRelLoc, &
                                rc=status)
      call ESMF_ArrayGetData(srcMaskArray, srcLocalMask, ESMF_DATA_REF, &
                             status)

      ! Calculate the intersections of this DE's bounding box with all others
      hassrcdata = .true.   ! temp for now
      hasdstdata = .true.   ! temp for now

   ! Calculate two separate Routes:
   !    the first will be used in the code to gather the data for running
   !              the regrid
   !    the second sends and receives total cell coordinate information and
   !              is used internal to this routine to get coordinate 
   !              information locally to calculate the regrid weights

      ! From each grid get the bounding box information on this DE
      call ESMF_GridGetPhysGrid(srcGrid, relloc=srcRelLoc, localMin=srcMin, &   
                                localMax=srcMax, rc=status)
      call ESMF_GridGetPhysGrid(dstGrid, relloc=dstRelLoc, localMin=dstMin, &
                                localMax=dstMax, rc=status)

   ! STORED ROUTE FOR MOVING DATA FOR REGRID RUN:
      ! calculate intersections
      call ESMF_GridBoxIntersectSend(dstGrid, srcGrid, srcMin, srcMax, &
                                     myAI, sendDomainList, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: GridBoxIntersectSend ", &
                 "returned failure"
        return
      endif
      call ESMF_GridBoxIntersectRecv(srcGrid, dstMin, dstMax, &
                                     recvDomainList, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: GridBoxIntersectRecv ", &
                 "returned failure"
        return
      endif

      ! Create Route
      ! TODO: this must be either a parent layout, or the src and dst layouts
      !  must be identical.
      route = ESMF_RouteCreate(srcDELayout, status)
      call ESMF_RoutePrecomputeDomList(route, 2, my_DE, sendDomainList, &
                                       recvDomainList, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: ", &
                 "RoutePrecomputeDomList returned failure"
        return
      endif
      ! set size of recv items in Route
      call ESMF_RouteSetRecvItems(route, recvDomainList%total_points, status)
      ! Save route in the routehandle object
      call ESMF_RouteHandleSet(rh, route1=route, rc=status)

   ! TEMPORARY INTERNAL ROUTE:
      ! calculate intersections
      call ESMF_GridBoxIntersectSend(dstGrid, srcGrid, srcMin, srcMax, &
                                     myTotalAI, sendDomainListTot, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: GridBoxIntersectSend ", &
                 "returned failure"
        return
      endif
      call ESMF_GridBoxIntersectRecv(srcGrid, dstMin, dstMax, &
                                     recvDomainListTot, .true., status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: GridBoxIntersectRecv ", &
                 "returned failure"
        return
      endif

      ! Create Route
      ! TODO: this must be either a parent layout, or the src and dst layouts
      !  must be identical.
      tempRoute = ESMF_RouteCreate(srcDELayout, status)
      call ESMF_RoutePrecomputeDomList(tempRoute, 2, my_DE, sendDomainListTot, &
                                       recvDomainListTot, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: ", &
                 "RoutePrecomputeDomList returned failure"
        return
      endif
      ! set size of recv items in Route
      call ESMF_RouteSetRecvItems(tempRoute, recvDomainListTot%total_points, status)

  ! Now use temporary route to gather necessary coordinates
      ! Create arrays for gathered coordinates 
      call ESMF_RouteGetRecvItems(tempRoute, size, status)
      allocate(srcGatheredCoordX(size))
      allocate(srcGatheredCoordY(size))
      allocate(srcGatheredMask(size))
      srcGatheredCoordXArray = ESMF_LocalArrayCreate(srcGatheredCoordX, &
                                                     ESMF_DATA_REF, status)
      srcGatheredCoordYArray = ESMF_LocalArrayCreate(srcGatheredCoordY, &
                                                     ESMF_DATA_REF, status)
      srcGatheredMaskArray   = ESMF_LocalArrayCreate(srcGatheredMask, &
                                                     ESMF_DATA_REF, status)

      ! Execute Route now to gather grid center coordinates from source
      ! These arrays are just wrappers for the local coordinate data
      srcLocalCoordXArray = ESMF_LocalArrayCreate(srcLocalCoordX, &
                                                  ESMF_DATA_COPY, status)
      srcLocalCoordYArray = ESMF_LocalArrayCreate(srcLocalCoordY, &
                                                  ESMF_DATA_COPY, status)
      srcLocalMaskArray   = ESMF_LocalArrayCreate(srcLocalMask, &
                                                  ESMF_DATA_COPY, status)
      call ESMF_RouteRun(tempRoute, srcLocalCoordXArray, &
                         srcGatheredCoordXArray, status)
      call ESMF_RouteRun(tempRoute, srcLocalCoordYArray, &
                         srcGatheredCoordYArray, status)
      call ESMF_RouteRun(tempRoute, srcLocalMaskArray, &
                         srcGatheredMaskArray, status)

      ! now all necessary data is local

      ! Create a Transform Values object
      tv = ESMF_TransformValuesCreate(rc)

      ! TODO: the *4 is to guarantee the max allocation possible is enough
      !  for bilinear interpolation.  eventually the addlinks routine should
      !  grow the arrays internally.
      size = ((dstCounts(1)*dstCounts(2)) + 1) * 4
      srcindex = ESMF_LocalArrayCreate(1, ESMF_DATA_INTEGER, ESMF_I4, &
                                          size, status)
      dstindex = ESMF_LocalArrayCreate(1, ESMF_DATA_INTEGER, ESMF_I4, &
                                          size*2, status) 
      allocate(tempCrud(size))
      weights = ESMF_LocalArrayCreate(tempCrud, ESMF_DATA_REF, status)
 
      ! set the values in the TV
      call ESMF_TransformValuesSet(tv, 0, srcindex=srcindex, dstindex=dstindex, &
                                   weights=weights, rc=status)

      ! set up user masks and logical found arrays for search
      allocate(found(dstCounts(1),dstCounts(2)))
      allocate(foundCount(dstCounts(1),dstCounts(2)))
      found = .FALSE.
      foundCount = 0

      if(present(dstMask)) then
  !      dstUserMask = dstMask
      else
        allocate(dstUserMask(dstCounts(1)*dstCounts(2)))
        dstUserMask = .TRUE.
      endif
      if(present(srcMask)) then
  !      srcUserMask = srcMask
      else
        size = recvDomainListTot%total_points
        allocate(srcUserMask(size))
        srcUserMask = .TRUE.
      endif
     
      ! Loop through domains for the search routine
      call ESMF_GridGet(srcGrid, horz_coord_system=coordSystem, rc=status)
      num_domains = recvDomainListTot%num_domains
      indexMod = -1
      start = 1
      startComp = 1
      do i = 1,num_domains
        srcSizeX = recvDomainList%domains(i)%ai(1)%max &
                 - recvDomainList%domains(i)%ai(1)%min + 1
        srcSizeY = recvDomainList%domains(i)%ai(2)%max &
                 - recvDomainList%domains(i)%ai(2)%min + 1
        stopComp  = startComp + srcSizeX*srcSizeY - 1
        srcSizeX = recvDomainListTot%domains(i)%ai(1)%max &
                 - recvDomainListTot%domains(i)%ai(1)%min + 1
        srcSizeY = recvDomainListTot%domains(i)%ai(2)%max &
                 - recvDomainListTot%domains(i)%ai(2)%min + 1
        stop  = start + srcSizeX*srcSizeY - 1
        call ESMF_RegridBilinearSearch(tv, recvDomainListTot%domains(i), &
                                       coordSystem, &
                                       srcSizeX, srcSizeY, startComp-1, &
                                       dstCounts(1), dstCounts(2), &
                                       indexMod, found, foundCount, &
                                       srcGatheredCoordX(start:stop), &
                                       srcGatheredCoordY(start:stop), &
                                       dstLocalCoordX, dstLocalCoordY, &
                                       srcGatheredMask(start:stop), &
                                       srcUserMask(start:stop), dstUserMask, status)
        start = stop + 1 
        startComp = stopComp + 1 
      enddo 

      call ESMF_RouteHandleSet(rh, tdata=tv, rc=status)

      call ESMF_LocalArrayDestroy(srcLocalCoordXArray, status)
      call ESMF_LocalArrayDestroy(srcLocalCoordYArray, status)
      call ESMF_LocalArrayDestroy(srcGatheredCoordXArray, status)
      call ESMF_LocalArrayDestroy(srcGatheredCoordYArray, status)
      ! deallocate(dstLocalCoordArray, status)
      ! deallocate(srcLocalCoordArray, status)
      
      ESMF_RegridConstructBilinear = rh

      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_RegridConstructBilinear

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridBilinearSearch - Searches a bilinear Regrid structure

! !INTERFACE:
      subroutine ESMF_RegridBilinearSearch(tv, domain, coordSystem, &
                                           srcSizeX, srcSizeY, srcStart, &
                                           dstSizeX, dstSizeY, indexMod, &
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
      integer, intent(in) :: dstSizeX  ! in the lines below.
      integer, intent(in) :: dstSizeY
      integer, dimension(:), intent(in) :: indexMod
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
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer ::           &
         i,j,n,iter,       &! loop counters
         iii,jjj,          &! more loop counters
         ip1,jp1,          &! neighbor indices
         ibDst, ieDst,     &! beg, end of exclusive domain in i-dir of dest grid
         jbDst, jeDst,     &! beg, end of exclusive domain in j-dir of dest grid
         ibSrc, ieSrc,     &! beg, end of exclusive domain in i-dir of source grid
         jbSrc, jeSrc,     &! beg, end of exclusive domain in j-dir of source grid
         dstICount, srcICount, &
         my_DE, srcCount, srcValidCount

      integer :: srcAdd,   &! address in gathered source grid
                 dstAdd(2)  ! address in dest grid
         
      real (ESMF_KIND_R8) ::  &
         lon_thresh,    &! threshold for checking longitude crossing
         lon_cycle,     &! 360 for degrees, 2pi for radians
         dx1, dx2, dx3, &! differences for iterative scheme
         dy1, dy2, dy3, &! differences for iterative scheme
         iguess, jguess,&! initial guess for location within grid box
         deli, delj,    &! change in i,j position from last iteration
         mat1, mat2,    &! matrix elements for 2x2 matrix in iterative scheme
         mat3, mat4,    &! ditto
         dxp, dyp,      &
         dstX, dstY,    &
         determinant,   &! determinant of above matrix
         sumWts,        &
         zero, half, one, pi

      real (ESMF_KIND_R8), dimension(4) ::     &
         srcX,         &! x coordinate of bilinear box corners
         srcY,         &! y coordinate of bilinear box corners
         weights        ! bilinear weights for single box

      integer, parameter :: &
         maxIter = 100   ! max iteration count for i,j iteration

      real (ESMF_KIND_R8), parameter :: &
         converge = 1.e-10  ! convergence criterion

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      zero = 0.
      half = 0.5
      one  = 1.
      pi   = 3.1416
 !     if (dst_phys_grid%coord_system == ESMF_CoordSystem_Spherical) then
 !        if (units = 'degrees') then
 !           lon_thresh = 270.0
 !           lon_cycle  = 360.0
 !        else if (units = 'radians') then
 !           lon_thresh = 1.5*pi
 !           lon_cycle  = 2.0*pi
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

      dstICount = ieDst - ibDst + 1
      srcICount = ieSrc - ibSrc + 1

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
       !       if (dst_grid%coord_system == ESMF_CoordSystem_Spherical) then
       !         if (srcX(1) - dstX >  lon_thresh) &
       !             srcX(1) = srcX(1) - lon_cycle
       !         if (srcX(1) - dstX < -lon_thresh) &
       !             srcX(1) = srcX(1) + lon_cycle
       !         if (srcX(2) - dstX >  lon_thresh) &
       !             srcX(2) = srcX(2) - lon_cycle
       !         if (srcX(2) - dstX < -lon_thresh) &
       !             srcX(2) = srcX(2) + lon_cycle
       !         if (srcX(3) - dstX >  lon_thresh) &
       !             srcX(3) = srcX(3) - lon_cycle
       !         if (srcX(3) - dstX < -lon_thresh) &
       !             srcX(3) = srcX(3) + lon_cycle
       !         if (srcX(4) - dstX >  lon_thresh) &
       !             srcX(4) = srcX(4) - lon_cycle
       !         if (srcX(4) - dstX < -lon_thresh) &
       !             srcX(4) = srcX(4) + lon_cycle
       !       endif

              ! check to see if point inside cell
              found(i,j) = ESMF_PhysGridPointInCell(dstX, dstY, srcX, srcY, status)

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
                print *,'Point coords: ',dstX,dstY
                print *,'Source cell coords: ',srcX(:),srcY(:)
                print *,'Current i,j : ',iguess, jguess
                print *,'Iteration for i,j exceed max iteration count'
                print *, "ERROR in ESMF_RegridBilinearSearch"
                rc = ESMF_FAILURE
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
            ! distance-weighted average
            else if (srcCount > 0 .and. srcCount < 4) then

              sumWts = zero
              if (srcUserMask(iii,jjj) .and. (srcGridMask(iii,jjj).ne.1)) then
                weights(1) = ESMF_GridComputeDistance(srcX(1),srcY(1), &
                                                      dstX, dstY,      &
                                                      coordSystem, status)
                sumWts = sumWts + weights(1)
              endif
              if (srcUserMask(ip1,jjj) .and. (srcGridMask(ip1,jjj).ne.1)) then
                weights(2) = ESMF_GridComputeDistance(srcX(2),srcY(2), &
                                                      dstX, dstY,      &
                                                      coordSystem, status)
                sumWts = sumWts + weights(2)
              endif
              if (srcUserMask(ip1,jp1) .and. (srcGridMask(ip1,jp1).ne.1)) then
                weights(3) = ESMF_GridComputeDistance(srcX(3),srcY(3), &
                                                      dstX, dstY,      &
                                                      coordSystem, status)
                sumWts = sumWts + weights(3)
              endif
              if (srcUserMask(iii,jp1) .and. (srcGridMask(iii,jp1).ne.1)) then
                srcCount   = srcCount + 1
                weights(4) = ESMF_GridComputeDistance(srcX(4),srcY(4), &
                                                      dstX, dstY,      &
                                                      coordSystem, status)
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
              dstAdd(1) = i
              dstAdd(2) = j
              srcAdd = (jjj-1+indexMod(2))*srcICount + (iii+indexMod(1)) &
                     + srcStart
              call ESMF_RegridAddLink(tv, srcAdd, dstAdd, weights(1), rc)
            endif
            if (srcUserMask(ip1,jjj) .and. (srcGridMask(ip1,jjj).eq.0)) then
              dstAdd(1) = i
              dstAdd(2) = j
              srcAdd = (jjj-1+indexMod(2))*srcICount + (ip1+indexMod(1)) &
                     + srcStart
              call ESMF_RegridAddLink(tv, srcAdd, dstAdd, weights(2), rc)
            endif
            if (srcUserMask(ip1,jp1) .and. (srcGridMask(ip1,jp1).eq.0)) then
              dstAdd(1) = i
              dstAdd(2) = j
              srcAdd = (jp1-1+indexMod(2))*srcICount + (ip1+indexMod(1)) &
                     + srcStart
              call ESMF_RegridAddLink(tv, srcAdd, dstAdd, weights(3), rc)
            endif
            if (srcUserMask(iii,jp1) .and. (srcGridMask(iii,jp1).eq.0)) then
              dstAdd(1) = i
              dstAdd(2) = j
              srcAdd = (jp1-1+indexMod(2))*srcICount + (iii+indexMod(1)) &
                     + srcStart
              call ESMF_RegridAddLink(tv, srcAdd, dstAdd, weights(4), rc)
            endif

          endif ! found box
        endif ! dst mask            
      enddo ! i loop on dst grid DE
      enddo ! j loop on dst grid DE

      if(rcpresent) rc = ESMF_SUCCESS
      
      end subroutine ESMF_RegridBilinearSearch

!------------------------------------------------------------------------------

   end module ESMF_RegridBilinearMod
