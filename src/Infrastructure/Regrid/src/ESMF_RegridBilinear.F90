! $Id: ESMF_RegridBilinear.F90,v 1.26 2003/09/23 21:43:24 jwolfe Exp $
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
      use ESMF_BaseMod      ! ESMF base   class
      use ESMF_DELayoutMod
      use ESMF_LocalArrayMod
      use ESMF_ArrayBaseMod ! ESMF array  class
      use ESMF_ArrayExpandMod ! ESMF array  class
      use ESMF_PhysGridMod  ! ESMF physical grid class
      use ESMF_DistGridMod  ! ESMF distributed grid class
      use ESMF_DataMapMod
      use ESMF_GridMod      ! ESMF grid   class
      use ESMF_FieldMod     ! ESMF field  class
      use ESMF_BundleMod    ! ESMF bundle class
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
      '$Id: ESMF_RegridBilinear.F90,v 1.26 2003/09/23 21:43:24 jwolfe Exp $'

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
      type(ESMF_Array), intent(in) :: srcarray
      type(ESMF_Grid), intent(in) :: srcgrid
      type(ESMF_DataMap), intent(in) :: srcdatamap
      type(ESMF_Array), intent(inout) :: dstarray
      type(ESMF_Grid), intent(in) :: dstgrid
      type(ESMF_DataMap), intent(in) :: dstdatamap
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
      integer :: start, stop, my_DE, coordSystem
      integer :: srcSizeX, srcSizeY, size
      integer :: size_xy(2)
      integer :: i, j, num_domains, dstCounts(3), srcCounts(3)
      logical, dimension(:), pointer :: srcMaskUse, dstMaskUse
      logical, dimension(:,:), pointer :: found
      real(ESMF_KIND_R8), dimension(:), pointer :: srcGatheredCoordX, srcGatheredCoordY
      real(ESMF_KIND_R8), dimension(:), pointer :: srcLocalCoordX, srcLocalCoordY
      real(ESMF_KIND_R8), dimension(:), pointer :: dstLocalCoordX, dstLocalCoordY
      real(ESMF_KIND_R8), dimension(:), pointer :: tempCrud
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: dstLocalCoord, srcLocalCoord
      real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: dstMin, dstMax
      real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: srcMin, srcMax
      type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: myAI
      type(ESMF_DataType) :: type
      type(ESMF_DataKind) :: kind
      type(ESMF_LocalArray) :: srcindex, dstindex, weights
      type(ESMF_LocalArray) :: srcLocalCoordXArray, srcLocalCoordYArray
      type(ESMF_LocalArray) :: srcGatheredCoordXArray, srcGatheredCoordYArray
      type(ESMF_LocalArray) :: dstLocalCoordArray, srcLocalCoordArray
      type(ESMF_DomainList) :: sendDomainList, recvDomainList
      type(ESMF_DELayout) :: srcDELayout
      type(ESMF_RelLoc) :: srcRelLoc, dstRelLoc
      type(ESMF_Route) :: route
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
      allocate(dstLocalCoord(dstCounts(1),dstCounts(2),dstCounts(3)))
      dstLocalCoordArray = ESMF_LocalArrayCreate(dstLocalCoord, ESMF_DATA_REF, &
                                                 status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: LocalArrayCreate ", &
                 "returned failure"
        return
      endif
      call ESMF_GridGetCoord(dstGrid, relloc=dstRelLoc, &
                             center_coord=dstLocalCoordArray, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: GridGetCoord ", &
                 "returned failure"
        return
      endif
      call ESMF_LocalArrayGetData(dstLocalCoordArray, dstLocalCoord, &
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
    !  type = ESMF_DATA_REAL
    !  kind = ESMF_R8
    !  centerCoordArray = ESMF_LocalArrayCreate(3, type, kind, counts, status)
    !  call ESMF_LocalArrayGetData(centerCoordArray, center_coord, &
    !                              ESMF_DATA_REF, status)
      allocate(srcLocalCoord(srcCounts(1),srcCounts(2),srcCounts(3)))
      srcLocalCoordArray = ESMF_LocalArrayCreate(srcLocalCoord, ESMF_DATA_REF, &
                                                 status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: LocalArrayCreate ", &
                 "returned failure"
        return
      endif
      call ESMF_GridGetCoord(srcGrid, relloc=srcRelLoc, &
                             center_coord=srcLocalCoordArray, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: GridGetCoord ", &
                 "returned failure"
        return
      endif
      call ESMF_LocalArrayGetData(srcLocalCoordArray, srcLocalCoord, &
                                  ESMF_DATA_REF, status)

      ! Calculate the intersections of this DE's bounding box with all others
      hassrcdata = .true.   ! temp for now
      hasdstdata = .true.   ! temp for now

      ! From the grid get the bounding box on this DE
      call ESMF_GridGetPhysGrid(srcGrid, srcRelLoc, local_min=srcMin, &   
                                local_max=srcMax, rc=status)
      call ESMF_GridBoxIntersectSend(dstGrid, srcGrid, srcMin, srcMax, myAI, &
                                     sendDomainList, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructBilinear: GridBoxIntersectSend ", &
                 "returned failure"
        return
      endif

      ! From the grid get the bounding box on this DE
      call ESMF_GridGetPhysGrid(dstGrid, dstRelLoc, local_min=dstMin, &
                                local_max=dstMax, rc=status)
      call ESMF_GridBoxIntersectRecv(srcGrid, dstMin, dstMax, &
                                     recvDomainList, status)
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
      ! Save route in the routehandle object
      call ESMF_RouteHandleSet(rh, route1=route, rc=status)

      ! Create arrays for gathered coordinates 
   !   type = ESMF_DATA_REAL
   !   kind = ESMF_R8
   !   size = recvDomainList%total_points
   !   srcCenterX = ESMF_LocalArrayCreate(1, type, kind, size, status)
   !   call ESMF_LocalArrayGetData(srcCenterX, src_center_x, &
   !                               ESMF_DATA_REF, status)
   !   srcCenterY = ESMF_LocalArrayCreate(1, type, kind, size, status)
   !   call ESMF_LocalArrayGetData(srcCenterY, src_center_y, &
   !                               ESMF_DATA_REF, status)
      size = recvDomainList%total_points
      allocate(srcGatheredCoordX(size))
      allocate(srcGatheredCoordY(size))
      srcGatheredCoordXArray = ESMF_LocalArrayCreate(srcGatheredCoordX, &
                                                     ESMF_DATA_REF, status)
      srcGatheredCoordYArray = ESMF_LocalArrayCreate(srcGatheredCoordY, &
                                                     ESMF_DATA_REF, status)

      ! create local arrays of the x and y coords to be used for the
      ! routed gather process.  Note: we currently have to create separate
      ! x and y arrays since the Route will be used to get other 1-D arrays
      ! elsewhere.  But at some point it would be good to have a modified
      ! Route that could accept vectors and halo widths
   !   type = ESMF_DATA_REAL
   !   kind = ESMF_R8
   !   size = counts(1)*counts(2)
   !   centerCoordXArray = ESMF_LocalArrayCreate(2, type, kind, counts, status)
   !   call ESMF_LocalArrayGetData(centerCoordXArray, centerCoordX, &
   !                               ESMF_DATA_REF, status)
   !   centerCoordYArray = ESMF_LocalArrayCreate(2, type, kind, counts, status)
   !   call ESMF_LocalArrayGetData(centerCoordYArray, centerCoordY, &
   !                               ESMF_DATA_REF, status)
      size = srcCounts(1)*srcCounts(2)
      allocate(srcLocalCoordX(size))
      allocate(srcLocalCoordY(size))
      srcLocalCoordXArray = ESMF_LocalArrayCreate(srcLocalCoordX, &
                                                  ESMF_DATA_REF, status)
      srcLocalCoordYArray = ESMF_LocalArrayCreate(srcLocalCoordY, &
                                                  ESMF_DATA_REF, status)
      srcLocalCoordX = pack(srcLocalCoord(:,:,1:1), mask=.true.)
      srcLocalCoordY = pack(srcLocalCoord(:,:,2:2), mask=.true.)

      ! Execute Route now to gather grid center coordinates from source
      ! This same Route will be executed at RegridRun time to bring over
      ! data values using the same patterns.
      ! TODO: load srcMask the same way
      call ESMF_RouteRun(route, srcLocalCoordXArray, srcGatheredCoordXArray, &
                         status)
      call ESMF_RouteRun(route, srcLocalCoordYArray, srcGatheredCoordYArray, &
                         status)

      ! now all necessary data is local

      ! Create a Transform Values object
      tv = ESMF_TransformValuesCreate(rc)
 

      ! TODO: the *4 is to guarentee the max allocation possible is enough
      !  for bilinear interpolation.  eventually the addlinks routine should
      !  grow the arrays internally.
      size_xy(1) = dstCounts(1)*4
      size_xy(2) = dstCounts(2)*4
      size = ((dstCounts(1)*dstCounts(2)) + 1) * 4
      !tv%domainlist = recvDomainList
      srcindex = ESMF_LocalArrayCreate(1, ESMF_DATA_INTEGER, ESMF_I4, &
                                          size, status)
      dstindex = ESMF_LocalArrayCreate(1, ESMF_DATA_INTEGER, ESMF_I4, &
                                          size, status) 
      allocate(tempCrud(size))
      weights = ESMF_LocalArrayCreate(tempCrud, ESMF_DATA_REF, status)
   !   tv%weights  = ESMF_LocalArrayCreate(2, ESMF_DATA_REAL, ESMF_R8, &
   !                                       size_x0, status)
 
      ! set the values in the TV
      call ESMF_TransformValuesSet(tv, 0, recvDomainList, &
                                   srcindex, dstindex, weights, rc)

      ! set up mask and logical found arrays for search
      allocate(found(dstCounts(1),dstCounts(2)))
      found = .FALSE.
      if(present(dstMask)) then
  !      dstMaskUse = dstMask
      else
        allocate(dstMaskUse(dstCounts(1)*dstCounts(2)))
        dstMaskUse = .TRUE.
      endif
      if(present(srcMask)) then
  !      srcMaskUse = srcMask
      else
        allocate(srcMaskUse(size))
        srcMaskUse = .TRUE.
      endif
     
      ! Loop through domains for the search routine
      size = dstCounts(1)*dstCounts(2)
      allocate(dstLocalCoordX(size))
      allocate(dstLocalCoordY(size))
      dstLocalCoordX = pack(dstLocalCoord(:,:,1:1), mask=.true.)
      dstLocalCoordY = pack(dstLocalCoord(:,:,2:2), mask=.true.)
      call ESMF_GridGet(srcGrid, horz_coord_system=coordSystem, rc=status)
      num_domains = recvDomainList%num_domains
      start = 1
      do i = 1,num_domains
        srcSizeX = recvDomainList%domains(i)%ai(1)%max &
                 - recvDomainList%domains(i)%ai(1)%min + 1
        srcSizeY = recvDomainList%domains(i)%ai(2)%max &
                 - recvDomainList%domains(i)%ai(2)%min + 1
        stop  = start + srcSizeX*srcSizeY - 1
        call ESMF_RegridBilinearSearch(tv, recvDomainList%domains(i), found, &
                                       coordSystem, &
                                       srcSizeX, srcSizeY, start-1, &
                                       dstCounts(1), dstCounts(2), &
                                       srcGatheredCoordX(start:stop), &
                                       srcGatheredCoordY(start:stop), &
                                       srcMaskUse(start:stop), &
                                       dstLocalCoordX, dstLocalCoordY, dstMaskUse, &
                                       status)
        start = stop + 1 
      enddo 

      call ESMF_RouteHandleSet(rh, tdata=tv, rc=status)

      call ESMF_LocalArrayDestroy(srcLocalCoordXArray, status)
      call ESMF_LocalArrayDestroy(srcLocalCoordYArray, status)
      call ESMF_LocalArrayDestroy(dstLocalCoordArray, status)
      call ESMF_LocalArrayDestroy(srcLocalCoordArray, status)
      call ESMF_LocalArrayDestroy(srcGatheredCoordXArray, status)
      call ESMF_LocalArrayDestroy(srcGatheredCoordYArray, status)
      
      ESMF_RegridConstructBilinear = rh

      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_RegridConstructBilinear

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridBilinearSearch - Searches a bilinear Regrid structure

! !INTERFACE:
      subroutine ESMF_RegridBilinearSearch(tv, domain, found, coordSystem, &
                                           srcSizeX, srcSizeY, srcStart, &
                                           dstSizeX, dstSizeY, &
                                           srcCenterX, srcCenterY, srcMask, &
                                           dstCenterX, dstCenterY, dstMask, rc)
!
! !ARGUMENTS:
      type(ESMF_TransformValues), intent(inout) :: tv
      type(ESMF_Domain), intent(in) :: domain
      integer, intent(in) :: coordSystem
      integer, intent(in) :: srcSizeX  ! apparently these have to be first
      integer, intent(in) :: srcSizeY  ! so the compiler knows they're ints
      integer, intent(in) :: srcStart  ! when it goes to use them as dims
      integer, intent(in) :: dstSizeX  ! in the lines below.
      integer, intent(in) :: dstSizeY
      logical, dimension(dstSizeX,dstSizeY), intent(inout) :: found
      real(ESMF_KIND_R8), dimension(srcSizeX,srcSizeY), intent(in) :: srcCenterX
      real(ESMF_KIND_R8), dimension(srcSizeX,srcSizeY), intent(in) :: srcCenterY
      logical, dimension(srcSizeX,srcSizeY), intent(in) :: srcMask
      real(ESMF_KIND_R8), dimension(dstSizeX,dstSizeY), intent(in) :: dstCenterX
      real(ESMF_KIND_R8), dimension(dstSizeX,dstSizeY), intent(in) :: dstCenterY
      logical, dimension(dstSizeX,dstSizeY), intent(in) :: dstMask
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
      integer ::           &
         i,j,n,iter,       &! loop counters
         iii,jjj,          &! more loop counters
         ip1,jp1,          &! neighbor indices
         ibDst, ieDst,     &! beg, end of exclusive domain in i-dir of dest grid
         jbDst, jeDst,     &! beg, end of exclusive domain in j-dir of dest grid
         ibSrc, ieSrc,     &! beg, end of exclusive domain in i-dir of source grid
         jbSrc, jeSrc,     &! beg, end of exclusive domain in j-dir of source grid
         dstICount, srcICount, &
         my_DE, srcCount

      integer :: srcAdd,   &! address in gathered source grid
                 dstAdd     ! address in dest grid
         
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
      ieSrc = domain%ai(1)%max
      jbSrc = domain%ai(2)%min
      jeSrc = domain%ai(2)%max

      dstICount = ieDst - ibDst + 1
      srcICount = ieSrc - ibSrc + 1

      do j=jbDst,jeDst
      do i=ibDst,ieDst
        dstX = dstCenterX(i,j)
        dstY = dstCenterY(i,j)
            
        ! only perform interpolation on un-masked and un-found points
        if (dstMask(i,j) .and. (.not.found(i,j))) then
          ! for this destination point, look for the proper neighbor cells in the
          ! source grid 
          search_loop: do jjj=jbSrc,jeSrc-1  ! TODO: -1 not right, just for debug
            do iii=ibSrc,ieSrc-1
               
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
              found(i,j) = ESMF_PhysGridPointInCell(dstX, dstY, srcX, srcY, rc)

              if (found(i,j)) exit search_loop
            enddo            ! iii-loop on src DE
          enddo search_loop  ! jjj-loop on src DE
               
          ! if we've found a bilinear box containing the point continue with
          ! computation of weights
          if (found(i,j)) then

            ! check to see if src mask is true at all points
            srcCount = 0
            if (srcMask(iii,jjj)) srcCount = srcCount + 1
            if (srcMask(ip1,jjj)) srcCount = srcCount + 1
            if (srcMask(iii,jp1)) srcCount = srcCount + 1
            if (srcMask(ip1,jp1)) srcCount = srcCount + 1

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

            ! if not all four points in box are valid (unmasked) default to a
            ! distance-weighted average
            else if (srcCount > 0 .and. srcCount < 4) then

              sumWts = zero
              if (srcmask(iii,jjj)) then
                weights(1) = ESMF_GridComputeDistance(srcX(1),srcY(1), &
                                                      dstX, dstY,      &
                                                      coordSystem, status)
                sumWts = sumWts + weights(1)
              endif
              if (srcmask(ip1,jjj)) then
                weights(2) = ESMF_GridComputeDistance(srcX(2),srcY(2), &
                                                      dstX, dstY,      &
                                                      coordSystem, status)
                sumWts = sumWts + weights(2)
              endif
              if (srcmask(ip1,jp1)) then
                weights(3) = ESMF_GridComputeDistance(srcX(3),srcY(3), &
                                                      dstX, dstY,      &
                                                      coordSystem, status)
                sumWts = sumWts + weights(3)
              endif
              if (srcmask(iii,jp1)) then
                srcCount   = srcCount + 1
                weights(4) = ESMF_GridComputeDistance(srcX(4),srcY(4), &
                                                      dstX, dstY,      &
                                                      coordSystem, status)
                sumWts = sumWts + weights(4)
              endif
              weights(:) = weights(:)/sumWts

            endif

            ! now store this link into address, weight arrays
            if (srcmask(iii,jjj)) then
              dstAdd = (j-1)*dstICount + i
              srcAdd = (jjj-1)*srcICount + iii + srcStart
              call ESMF_RegridAddLink(tv, srcAdd, dstAdd, weights(1), rc)
            endif
            if (srcmask(ip1,jjj)) then
              dstAdd = (j-1)*dstICount + i
              srcAdd = (jjj-1)*srcICount + ip1 + srcStart
              call ESMF_RegridAddLink(tv, srcAdd, dstAdd, weights(2), rc)
            endif
            if (srcmask(ip1,jp1)) then
              dstAdd = (j-1)*dstICount + i
              srcAdd = (jp1-1)*srcICount + ip1 + srcStart
              call ESMF_RegridAddLink(tv, srcAdd, dstAdd, weights(3), rc)
            endif
            if (srcmask(iii,jp1)) then
              dstAdd = (j-1)*dstICount + i
              srcAdd = (jp1-1)*srcICount + iii + srcStart
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
