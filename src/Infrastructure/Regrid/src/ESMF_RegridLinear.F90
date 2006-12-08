! $Id: ESMF_RegridLinear.F90,v 1.39 2006/12/08 23:36:08 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
#define ESMF_FILENAME "ESMF_RegridLinear.F90"
!
!     ESMF Linear Regrid Module
      module ESMF_RegridLinearMod
!
!==============================================================================
!
! This file contains the Regrid class methods for linear regridding.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!==============================================================================
!BOPI
! !MODULE: ESMF_RegridLinearMod - Linear interpolation
!
! !DESCRIPTION:
!
! The code in this file implements the linear methods for the ESMF Regrid
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

    public ESMF_RegridConstructLinear ! create and fill a regrid object
                                      ! for a linear regridding
    public ESMF_RegridLinearSearch
!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RegridLinear.F90,v 1.39 2006/12/08 23:36:08 theurich Exp $'

!==============================================================================

      contains

!==============================================================================
!
! This section includes the linear Regrid construct methods.
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridConstructLinear"
!BOPI
! !IROUTINE: ESMF_RegridConstructLinear - Constructs linear Regrid structure 

! !INTERFACE:
    subroutine ESMF_RegridConstructLinear(rh, &
                                          srcArray, srcGrid, srcDataMap, &
                                          hasSrcData, &
                                          dstArray, dstGrid, dstDataMap, &
                                          hasDstData, &
                                          parentVM, routeIndex, &
                                          srcMask, dstMask, rc) 
! !ARGUMENTS:
      type(ESMF_RouteHandle),  intent(inout) :: rh
      type(ESMF_InternArray),        intent(in ) :: srcArray
      type(ESMF_Grid),         intent(in ) :: srcGrid
      type(ESMF_FieldDataMap), intent(inout) :: srcDataMap
      logical,                 intent(inout) :: hasSrcData
      type(ESMF_InternArray),        intent(in ) :: dstArray
      type(ESMF_Grid),         intent(in ) :: dstGrid
      type(ESMF_FieldDataMap), intent(inout) :: dstDataMap
      logical,                 intent(inout) :: hasDstData
      type(ESMF_VM),           intent(in ) :: parentVM
      integer,                 intent(in ) :: routeIndex
      type(ESMF_Mask),         intent(in ), optional :: srcMask
      type(ESMF_Mask),         intent(in ), optional :: dstMask
      integer,                 intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a source field and destination field (and their attached
!     grids), this routine constructs a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     field to the destination field using a linear interpolation.  
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
!EOPI

      integer :: localrc                          ! Error status
      integer :: start, stop, startComp, stopComp
      integer :: srcSizeZ, srcSizeZComp, size
      integer :: i, num_domains, dstCounts(3), srcCounts(3)
      !integer :: j, ij 
      logical, dimension(:), pointer :: srcUserMask, dstUserMask
      logical, dimension(:), pointer :: found
      integer(ESMF_KIND_I4), dimension(:), pointer :: foundCount, srcLocalMask
      integer(ESMF_KIND_I4), dimension(:), pointer :: srcGatheredMask
      real(ESMF_KIND_R8), dimension(:), pointer :: srcGatheredCoordZ
      real(ESMF_KIND_R8), dimension(:), pointer :: srcLocalCoordZ
      real(ESMF_KIND_R8), dimension(:), pointer :: dstLocalCoordZ
      type(ESMF_CoordSystem) :: coordSystem
      type(ESMF_InternArray) :: srcMaskArray
      type(ESMF_InternArray), dimension(:), pointer :: dstLocalCoordArray
      type(ESMF_InternArray), dimension(:), pointer :: srcLocalCoordArray
      type(ESMF_DomainList) :: recvDomainList 
      !type(ESMF_DomainList) :: recvDomainListTot
      type(ESMF_RelLoc) :: srcRelLoc, dstRelLoc
      type(ESMF_Route) :: route, tempRoute
! TODO: currently the ESMF_Regrid object is not used anywhere, so all references
!       are commented out
!     type(ESMF_Regrid) :: tempRegrid
      type(ESMF_TransformValues) :: tv
!     character (len = ESMF_MAXSTR) :: name

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,srcDataMap)
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,dstDataMap)

! TODO: passed in now, not constructed here.
      !! Construct an empty regrid structure
      !rh = ESMF_RouteHandleCreate(rc=localrc)
      !if (ESMF_LogMsgFoundError(localrc, &
      !                          ESMF_ERR_PASSTHRU, &
      !                          ESMF_CONTEXT, rc)) return

! TODO: currently the ESMF_Regrid object is not used anywhere, so all references
!       are commented out
!     tempRegrid = ESMF_RegridCreateEmpty(rc=localrc)
!     if (ESMF_LogMsgFoundError(localrc, &
!                               ESMF_ERR_PASSTHRU, &
!                               ESMF_CONTEXT, rc)) return

!     ! Set name and field pointers
!     call ESMF_RegridSet(tempRegrid, name=name, &
!                         srcArray = srcArray, dstArray = dstArray, &
!                         method = ESMF_REGRID_METHOD_LINEAR, rc=localrc)
!     if (ESMF_LogMsgFoundError(localrc, &
!                               ESMF_ERR_PASSTHRU, &
!                               ESMF_CONTEXT, rc)) return
      
      ! get destination grid info
      !TODO: Get grid masks?
      call ESMF_FieldDataMapGet(dstDataMap, horzRelloc=dstRelLoc, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_GridGetDELocalInfo(dstGrid, horzRelLoc=dstRelLoc, &
                                   localCellCountPerDim=dstCounts, &
                                   reorder=.false., rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      allocate(dstLocalCoordArray(1), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "dstLocalCoordArray", &
                                     ESMF_CONTEXT, rc)) return

      call ESMF_GridGetCoord(dstGrid, vertrelloc=dstRelLoc, &
                             centerCoord=dstLocalCoordArray, &
                             reorder=.false., rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_InternArrayGetData(dstLocalCoordArray(1), dstLocalCoordZ, &
                             ESMF_DATA_REF, localrc)

      ! get source grid info
      call ESMF_FieldDataMapGet(srcDataMap, horzRelloc=srcRelLoc, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_GridGetDELocalInfo(srcGrid, horzRelLoc=srcRelLoc, &
                                   localCellCountPerDim=srcCounts, &
                                   reorder=.false., rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      allocate(srcLocalCoordArray(1), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "srcLocalCoordArray", &
                                     ESMF_CONTEXT, rc)) return

      call ESMF_GridGetCoord(srcGrid, vertrelloc=srcRelLoc, &
                             centerCoord=srcLocalCoordArray, &
                             reorder=.false., total=.true., rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_InternArrayGetData(srcLocalCoordArray(1), srcLocalCoordZ, &
                             ESMF_DATA_REF, localrc)

      call ESMF_GridGetCellMask(srcGrid, srcMaskArray, relloc=srcRelLoc, &
                                rc=localrc)
      call ESMF_InternArrayGetData(srcMaskArray, srcLocalMask, ESMF_DATA_REF, &
                             localrc)

   ! Calculate two separate Routes:
   !    the first will be used in the code to gather the data for running
   !              the regrid, saved in routehandle
   !    the second sends and receives total cell coordinate information and
   !              is used internal to this routine to get coordinate 
   !              information locally to calculate the regrid weights

      route = ESMF_RegridRouteConstruct(3, srcGrid, dstGrid, &
                                        recvDomainList, parentVM, &
                                        srcDataMap=srcDataMap, &
                                        dstDataMap=dstDataMap, &
                                        total=.false., rc=localrc)
      call ESMF_RouteHandleSet(rh, which_route=routeIndex, &
                               route=route, rc=localrc)

!      tempRoute = ESMF_RegridRouteConstruct(srcGrid, dstGrid, &
!                                            recvDomainListTot, parentVM, &
!                                            srcDataMap=srcDataMap, &
!                                            dstDataMap=dstDataMap, &
!                                            total=.true., rc=localrc)

      ! Now use temporary route to gather necessary coordinates
      ! Create arrays for gathered coordinates 
      call ESMF_RouteGetRecvItems(tempRoute, size, localrc)
      allocate(srcGatheredCoordZ(size), &
               srcGatheredMask  (size), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "src gathered arrays", &
                                     ESMF_CONTEXT, rc)) return

      ! Execute Route now to gather grid center coordinates from source
      ! These arrays are just wrappers for the local coordinate data
      call ESMF_RouteRunF90PtrR811D(tempRoute, srcLocalCoordZ, &
                                    srcGatheredCoordZ, localrc)
      call ESMF_RouteRunF90PtrI411D(tempRoute, srcLocalMask, &
                                    srcGatheredMask, localrc)

      ! now all necessary data is local

      ! TODO: the *2 is to guarantee the max allocation possible is enough
      !  for linear interpolation.  eventually the addlinks routine should
      !  grow the arrays internally.
      size = (dstCounts(3) + 1) * 2
      ! Create a Transform Values object
      tv = ESMF_TransformValuesCreate(size, rc)

      ! set up user masks and logical found arrays for search
      allocate(     found(dstCounts(3)), &
               foundCount(dstCounts(3)), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "found arrays", &
                                     ESMF_CONTEXT, rc)) return

      found = .FALSE.
      foundCount = 0

      if (present(dstMask)) then
  !      dstUserMask = dstMask
      else
        allocate(dstUserMask(dstCounts(3)), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "dstUserMask", &
                                       ESMF_CONTEXT, rc)) return
        dstUserMask = .TRUE.
      endif
      if (present(srcMask)) then
  !      srcUserMask = srcMask
      else
        call ESMF_RouteGetRecvItems(tempRoute, size, localrc)
        allocate(srcUserMask(size), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "srcUserMask", &
                                       ESMF_CONTEXT, rc)) return
        srcUserMask = .TRUE.
      endif
     
      ! Loop through domains for the search routine
      call ESMF_GridGet(srcGrid, vertCoordSystem=coordSystem, rc=localrc)
      num_domains = recvDomainList%num_domains
      start = 1
      startComp = 1
      do i = 1,num_domains
        srcSizeZComp = recvDomainList%domains(i)%ai(3)%max &
                     - recvDomainList%domains(i)%ai(3)%min + 1
        stopComp  = startComp + srcSizeZComp - 1
        srcSizeZ = recvDomainList%domains(i)%ai(3)%max &
                 - recvDomainList%domains(i)%ai(3)%min + 1
        stop  = start + srcSizeZ - 1
        call ESMF_RegridLinearSearch(tv, recvDomainList%domains(i), &
                                     coordSystem, srcSizeZ, startComp-1, &
                                     dstCounts(3), found, foundCount, &
                                     srcGatheredCoordZ(start:stop), &
                                     dstLocalCoordZ, &
                                     srcGatheredMask(start:stop), &
                                     srcUserMask(start:stop), dstUserMask, localrc)
        start = stop + 1 
        startComp = stopComp + 1 
      enddo 

      call ESMF_RouteHandleSet(rh, which_tv=routeIndex, tdata=tv, rc=localrc)

      ! clean up
      call ESMF_RouteDestroy(tempRoute, localrc)
      deallocate(srcGatheredCoordZ, &
                   srcGatheredMask, &
                             found, &
                        foundCount, &
                       dstUserMask, &
                       srcUserMask, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                     ESMF_CONTEXT, rc)) return
      
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridConstructLinear

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridLinearSearch"
!BOPI
! !IROUTINE: ESMF_RegridLinearSearch - Searches a linear Regrid structure

! !INTERFACE:
      subroutine ESMF_RegridLinearSearch(tv, domain, coordSystem, &
                                         srcSizeZ, srcStart, dstSizeZ, &
                                         found, foundCount, &
                                         srcCenterZ, dstCenterZ, &
                                         srcGridMask, srcUserMask, dstUserMask, &
                                         rc)
!
! !ARGUMENTS:
      type(ESMF_TransformValues), intent(inout) :: tv
      type(ESMF_Domain), intent(in) :: domain
      type(ESMF_CoordSystem), intent(in) :: coordSystem
      integer, intent(in) :: srcSizeZ  ! apparently these have to be first
      integer, intent(in) :: srcStart  ! when it goes to use them as dims
      integer, intent(in) :: dstSizeZ  ! in the lines below.
      logical, dimension(dstSizeZ), intent(inout) :: found
      integer, dimension(dstSizeZ), intent(inout) :: foundCount
      real(ESMF_KIND_R8), dimension(srcSizeZ), intent(in) :: srcCenterZ
      real(ESMF_KIND_R8), dimension(dstSizeZ), intent(in) :: dstCenterZ
      integer, dimension(srcSizeZ), intent(in) :: srcGridMask
      logical, dimension(srcSizeZ), intent(in) :: srcUserMask
      logical, dimension(dstSizeZ), intent(in) :: dstUserMask
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a source field and destination field (and their attached
!     grids), this routine constructs a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     field to the destination field using a linear interpolation.
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

      !integer :: localrc                          ! Error status
      integer ::           &
         k,                &! loop counters
         kkk,              &! more loop counters
         kp1,              &! neighbor indices
         kbDst, keDst,     &! beg, end of exclusive domain in k-dir of dest grid
         kbSrc, keSrc,     &! beg, end of exclusive domain in k-dir of source grid
         srcCount, srcValidCount

      integer :: srcAdd,   &! address in gathered source grid
                 dstAdd     ! address in dest grid
         
      real (ESMF_KIND_R8) ::  dstZ, sumWts

      real (ESMF_KIND_R8), dimension(2) ::     &
         srcZ,         &! z coordinate of linear interval ends
         weights        ! linear weights for single interval

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      kbDst = 1
      keDst = dstSizeZ
      kbSrc = domain%ai(3)%min
      keSrc = domain%ai(3)%max - 1

      do k=kbDst,keDst
        dstZ = dstCenterZ(k)
            
        ! only perform interpolation on un-masked and un-found points
        if (dstUserMask(k) .and. (.not.found(k))) then
          ! for this destination point, look for the proper neighbor cells in the
          ! source grid 
          search_loop: do kkk=kbSrc,keSrc
               
            ! assume ghost cells filled so no worries about boundaries
            kp1 = kkk + 1

            ! set up box used for linear interpolation
            srcZ(1) = srcCenterZ(kkk)
            srcZ(2) = srcCenterZ(kp1)

            ! check to see if point inside interval
            found(k) = (dstZ.ge.srcZ(1) .and. dstZ.lt.srcZ(2))

            if (found(k)) exit search_loop
          enddo search_loop  ! kkk-loop on src DE
               
          ! if we've found an interval containing the point continue with
          ! computation of weights
          if (found(k)) then

            ! check to see if src masks are true at all points
            ! check grid mask to see if it's a ghost cell
            srcCount = 0
            if (srcUserMask(kkk) .and. (srcGridMask(kkk).ne.1)) &
                srcCount = srcCount + 1
            if (srcUserMask(kp1) .and. (srcGridMask(kp1).ne.1)) &
                srcCount = srcCount + 1

            if (srcCount.ne.0) then
            ! compute linear weights
              sumWts = 0.0
              if (srcUserMask(kkk) .and. (srcGridMask(kkk).ne.1)) then
                weights(1) = abs(srcZ(1) - dstZ)
                if (weights(1).eq.0.0) then
                  weights(1) = 1.0e20   ! TODO: exit loop instead
                else
                  weights(1) = 1./weights(1)
                endif
                sumWts = sumWts + weights(1)
              endif
              if (srcUserMask(kp1) .and. (srcGridMask(kp1).ne.1)) then
                weights(2) = abs(srcZ(2) - dstZ)
                if (weights(2).eq.0.0) then
                  weights(2) = 1.0e20   ! TODO: exit loop instead
                else
                  weights(2) = 1./weights(2)
                endif
                sumWts = sumWts + weights(2)
              endif
              weights(:) = weights(:)/sumWts

              ! Halos are still valid points for computing weights but should
              ! not be assigned weights or marked found.  Keep track of the total
              ! number of valid computational source points 
              srcValidCount = 0
              if (srcGridMask(kkk).eq.0) srcValidCount = srcValidCount + 1
              if (srcGridMask(kp1).eq.0) srcValidCount = srcValidCount + 1
   
              if ((foundCount(k)+srcValidCount) .ne. srcCount) then
                found(k) = .false.
                foundCount(k) = foundCount(k) + srcValidCount
              endif

              ! now store this link into address, weight arrays
              if (srcUserMask(kkk) .and. (srcGridMask(kkk).eq.0)) then
                dstAdd = k
                srcAdd = kkk + srcStart
                call ESMF_RegridAddLink(tv, srcAdd, dstAdd, weights(1), rc)
              endif
              if (srcUserMask(kp1) .and. (srcGridMask(kp1).eq.0)) then
                dstAdd = k
                srcAdd = kp1 + srcStart
                call ESMF_RegridAddLink(tv, srcAdd, dstAdd, weights(2), rc)
              endif
            endif

          endif ! found box
        endif ! dst mask            
      enddo ! k loop on dst grid DE

      if (present(rc)) rc = ESMF_SUCCESS
      
      end subroutine ESMF_RegridLinearSearch

!------------------------------------------------------------------------------

   end module ESMF_RegridLinearMod
