! $Id: ESMF_RegridLinear.F90,v 1.13 2004/04/02 18:36:35 nscollins Exp $
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
!     ESMF Linear Regrid Module
      module ESMF_RegridLinearMod
!
!==============================================================================
!
! This file contains the Regrid class methods for linear regridding.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_Macros.inc>
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
      use ESMF_BaseMod        ! ESMF base   class
      use ESMF_LocalArrayMod
      use ESMF_DataMapMod
      use ESMF_ArrayMod       ! ESMF array  class
      use ESMF_ArrayGetMod    ! ESMF array  class
      use ESMF_DistGridMod    ! ESMF distributed grid class
      use ESMF_PhysCoordMod   ! ESMF physical grid domain class
      use ESMF_PhysGridMod    ! ESMF physical grid class
      use ESMF_GridMod        ! ESMF grid   class
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
      '$Id: ESMF_RegridLinear.F90,v 1.13 2004/04/02 18:36:35 nscollins Exp $'

!==============================================================================

      contains

!==============================================================================
!
! This section includes the linear Regrid construct methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridConstructLinear - Constructs linear Regrid structure 

! !INTERFACE:
      function ESMF_RegridConstructLinear(srcArray, srcGrid, srcDataMap, &
                                          dstArray, dstGrid, dstDataMap, &
                                          srcMask, dstMask, rc) 
!
! !RETURN VALUE:
      type(ESMF_RouteHandle) :: ESMF_RegridConstructLinear
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: srcArray
      type(ESMF_Grid), intent(inout) :: srcGrid
      type(ESMF_DataMap), intent(in) :: srcDataMap
      type(ESMF_Array), intent(inout) :: dstArray
      type(ESMF_Grid), intent(inout) :: dstGrid
      type(ESMF_DataMap), intent(in) :: dstDataMap
      type(ESMF_Mask), intent(in), optional :: srcMask
      type(ESMF_Mask), intent(in), optional :: dstMask
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
      integer :: start, stop, startComp, stopComp
      integer :: srcSizeZ, srcSizeZComp, size
      integer :: i, j, num_domains, dstCounts(3), srcCounts(3), ij
      logical, dimension(:), pointer :: srcUserMask, dstUserMask
      logical, dimension(:), pointer :: found
      integer(ESMF_KIND_I4), dimension(:), pointer :: foundCount, srcLocalMask
      integer(ESMF_KIND_I4), dimension(:), pointer :: srcGatheredMask
      real(ESMF_KIND_R8), dimension(:), pointer :: srcGatheredCoordZ
      real(ESMF_KIND_R8), dimension(:), pointer :: srcLocalCoordZ
      real(ESMF_KIND_R8), dimension(:), pointer :: dstLocalCoordZ
      type(ESMF_CoordSystem) :: coordSystem
      type(ESMF_Array) :: srcMaskArray
      type(ESMF_Array), pointer :: dstLocalCoordArray
      type(ESMF_Array), pointer :: srcLocalCoordArray
      type(ESMF_DomainList) :: recvDomainList, recvDomainListTot
      type(ESMF_RelLoc) :: srcRelLoc, dstRelLoc
      type(ESMF_Route) :: route, tempRoute
      type(ESMF_RouteHandle) :: rh
      type(ESMF_RegridType) :: tempRegrid
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
        print *, "ERROR in RegridConstructLinear: RouteHandleCreate ", &
                 "returned failure"
        return
      endif

      ! Set name and field pointers
      call ESMF_RegridTypeSet(tempRegrid, name=name, srcArray = srcArray, &
                              dstArray = dstArray, &
                              method = ESMF_RegridMethod_Linear, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructLinear: RegridTypeSet ", &
                 "returned failure"
        return
      endif
      
      ! get destination grid info
      !TODO: Get grid masks?
      call ESMF_DataMapGet(dstDataMap, horzRelloc=dstRelLoc, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructLinear: DataMapGetRelloc ", &
                 "returned failure"
        return
      endif
      call ESMF_GridGetDE(dstGrid, horzRelLoc=dstRelLoc, &
                          localCellCountPerDim=dstCounts, &
                          reorder=.false., rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructLinear: GridGetDE ", &
                 "returned failure"
        return
      endif

!      call ESMF_GridGetCoord(dstGrid, relloc=dstRelLoc, &
!                             vertCoord=dstLocalCoordArray, &
!                             reorder=.false., rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructLinear: GridGetCoord ", &
                 "returned failure"
        return
      endif
      call ESMF_ArrayGetData(dstLocalCoordArray, dstLocalCoordZ, &
                             ESMF_DATA_REF, status)

      ! get source grid info
      call ESMF_DataMapGet(srcDataMap, horzRelloc=srcRelLoc, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructLinear: DataMapGetRelloc ", &
                 "returned failure"
        return
      endif
      call ESMF_GridGetDE(srcGrid, horzRelLoc=srcRelLoc, &
                          localCellCountPerDim=srcCounts, &
                          reorder=.false., rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructLinear: GridGetDE ", &
                 "returned failure"
        return
      endif

!      call ESMF_GridGetCoord(srcGrid, relloc=srcRelLoc, &
!                             vertCoord=srcLocalCoordArray, &
!                             reorder=.false., total=.true., rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConstructLinear: GridGetCoord ", &
                 "returned failure"
        return
      endif
      call ESMF_ArrayGetData(srcLocalCoordArray, srcLocalCoordZ, &
                             ESMF_DATA_REF, status)

      call ESMF_GridGetCellMask(srcGrid, srcMaskArray, relloc=srcRelLoc, &
                                rc=status)
      call ESMF_ArrayGetData(srcMaskArray, srcLocalMask, ESMF_DATA_REF, &
                             status)

      hassrcdata = .true.   ! temp for now
      hasdstdata = .true.   ! temp for now

   ! Calculate two separate Routes:
   !    the first will be used in the code to gather the data for running
   !              the regrid, saved in routehandle
   !    the second sends and receives total cell coordinate information and
   !              is used internal to this routine to get coordinate 
   !              information locally to calculate the regrid weights

      route = ESMF_RegridRouteConstruct(3, srcGrid, dstGrid, &
                                        recvDomainList, &
                                        srcDataMap=srcDataMap, &
                                        dstDataMap=dstDataMap, &
                                        total=.false., rc=status)
      call ESMF_RouteHandleSet(rh, route1=route, rc=status)

!      tempRoute = ESMF_RegridRouteConstruct(srcGrid, dstGrid, &
!                                            recvDomainListTot, &
!                                            srcDataMap=srcDataMap, &
!                                            dstDataMap=dstDataMap, &
!                                            total=.true., rc=status)

      ! Now use temporary route to gather necessary coordinates
      ! Create arrays for gathered coordinates 
      call ESMF_RouteGetRecvItems(tempRoute, size, status)
      allocate(srcGatheredCoordZ(size))
      allocate(srcGatheredMask(size))

      ! Execute Route now to gather grid center coordinates from source
      ! These arrays are just wrappers for the local coordinate data
      call ESMF_RouteRunF90PtrR811D(tempRoute, srcLocalCoordZ, &
                                    srcGatheredCoordZ, status)
      call ESMF_RouteRunF90PtrI411D(tempRoute, srcLocalMask, &
                                    srcGatheredMask, status)

      ! now all necessary data is local

      ! TODO: the *2 is to guarantee the max allocation possible is enough
      !  for linear interpolation.  eventually the addlinks routine should
      !  grow the arrays internally.
      size = (dstCounts(3) + 1) * 2
      ! Create a Transform Values object
      tv = ESMF_TransformValuesCreate(size, rc)

      ! set up user masks and logical found arrays for search
      allocate(found(dstCounts(3)))
      allocate(foundCount(dstCounts(3)))
      found = .FALSE.
      foundCount = 0

      if(present(dstMask)) then
  !      dstUserMask = dstMask
      else
        allocate(dstUserMask(dstCounts(3)))
        dstUserMask = .TRUE.
      endif
      if(present(srcMask)) then
  !      srcUserMask = srcMask
      else
        call ESMF_RouteGetRecvItems(tempRoute, size, status)
        allocate(srcUserMask(size))
        srcUserMask = .TRUE.
      endif
     
      ! Loop through domains for the search routine
      call ESMF_GridGet(srcGrid, vertCoordSystem=coordSystem, rc=status)
      num_domains = recvDomainListTot%num_domains
      start = 1
      startComp = 1
      do i = 1,num_domains
        srcSizeZComp = recvDomainList%domains(i)%ai(3)%max &
                     - recvDomainList%domains(i)%ai(3)%min + 1
        stopComp  = startComp + srcSizeZComp - 1
        srcSizeZ = recvDomainListTot%domains(i)%ai(3)%max &
                 - recvDomainListTot%domains(i)%ai(3)%min + 1
        stop  = start + srcSizeZ - 1
        call ESMF_RegridLinearSearch(tv, recvDomainListTot%domains(i), &
                                     coordSystem, srcSizeZ, startComp-1, &
                                     dstCounts(3), found, foundCount, &
                                     srcGatheredCoordZ(start:stop), &
                                     dstLocalCoordZ, &
                                     srcGatheredMask(start:stop), &
                                     srcUserMask(start:stop), dstUserMask, status)
        start = stop + 1 
        startComp = stopComp + 1 
      enddo 

      call ESMF_RouteHandleSet(rh, tdata=tv, rc=status)

      ! clean up
      call ESMF_RouteDestroy(tempRoute, status)
      deallocate(found)
      deallocate(foundCount)
      deallocate(dstUserMask)
      deallocate(srcUserMask)
      
      ESMF_RegridConstructLinear = rh

      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_RegridConstructLinear

!------------------------------------------------------------------------------
!BOP
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
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer ::           &
         k,n,iter,         &! loop counters
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

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

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

      if(rcpresent) rc = ESMF_SUCCESS
      
      end subroutine ESMF_RegridLinearSearch

!------------------------------------------------------------------------------

   end module ESMF_RegridLinearMod
