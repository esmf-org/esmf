! $Id: ESMF_RegridBilinear.F90,v 1.15 2003/08/27 23:38:28 nscollins Exp $
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
      '$Id: ESMF_RegridBilinear.F90,v 1.15 2003/08/27 23:38:28 nscollins Exp $'

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
                                            srcmask, dstmask, blocking, rc)
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
      type(ESMF_Mask), intent(in), optional :: srcmask
      type(ESMF_Mask), intent(in), optional :: dstmask
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
      integer :: start, stop, my_DE
      integer :: src_size_x, src_size_y, dst_size_x, dst_size_y, size
      integer :: size_xy(2), size_x0(2)
      integer :: i, num_domains, counts(3)
      logical, dimension(:), pointer :: src_mask, dst_mask
      real(ESMF_IKIND_R8), dimension(:), pointer :: src_center_x, src_center_y
      real(ESMF_IKIND_R8), dimension(:,:), pointer :: centerCoordX, centerCoordY
      real(ESMF_IKIND_R8), dimension(:,:,:), pointer :: center_coord
      type(ESMF_DataType) :: type
      type(ESMF_DataKind) :: kind
      type(ESMF_LocalArray) :: srcCenterX, srcCenterY, centerCoordArray
      type(ESMF_DomainList), pointer :: sendDomainList, recvDomainList
      type(ESMF_DELayout) :: srcDELayout
      type(ESMF_RelLoc) :: srcRelLoc
      type(ESMF_Route) :: route
      type(ESMF_RouteHandle) :: rh
      type(ESMF_RegridType) :: temp_regrid
      type(ESMF_TransformValues), pointer :: tv
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
        print *, "ERROR in RegridConsByFieldBilinear: RouteHandleCreate ", &
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
        print *, "ERROR in RegridConsByFieldBilinear: RegridTypeSet ", &
                 "returned failure"
        return
      endif
      
      ! Extract some grid information for use in this regrid.
      !TODO: Get grid sizes?
      !TODO: Get grid masks?
      call ESMF_GridGetDELayout(srcGrid, srcDELayout, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConsByFieldBilinear: GridGetDELayout ", &
                 "returned failure"
        return
      endif
      call ESMF_DELayoutGetDEID(srcDELayout, my_DE, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConsByFieldBilinear: DELayoutGetDEID ", &
                 "returned failure"
        return
      endif
      call ESMF_DataMapGet(srcDataMap, relloc=srcRelLoc, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConsByFieldBilinear: DataMapGet ", &
                 "returned failure"
        return
      endif

      counts(1) = 2
      call ESMF_GridGetDE(srcGrid, local_axis_length=counts(2:3), rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConsByFieldBilinear: GridGetDE ", &
                 "returned failure"
        return
      endif

      type = ESMF_DATA_REAL
      kind = ESMF_KIND_R8
      centerCoordArray = ESMF_LocalArrayCreate(3, type, kind, counts, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConsByFieldBilinear: LocalArrayCreate ", &
                 "returned failure"
        return
      endif
      call ESMF_GridGetCoord(srcGrid, relloc=srcRelLoc, &
                             center_coord=centerCoordArray, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConsByFieldBilinear: GridGetCoord ", &
                 "returned failure"
        return
      endif
      call ESMF_LocalArrayGetData(centerCoordArray, center_coord, &
                                  ESMF_DATA_REF, status)

      ! Calculate the intersections of this DE's bounding box with all others
      !call ESMF_FieldBoxIntersect(src_field, dst_field, recvDomainlist, &
      !                            sendDomainList, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConsByFieldBilinear: FieldBoxIntersect ", &
                 "returned failure"
        return
      endif

      ! create Route
      ! TODO: should route be a route handle?
      call ESMF_RoutePrecomputeDomList(route, 2, my_DE, sendDomainList, &
                                       recvDomainList, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridConsByFieldBilinear: ", &
                 "RoutePrecomputeDomList returned failure"
        return
      endif

      ! use Route to gather grid center coordinates from source
      size = recvDomainList%total_size
      srcCenterX = ESMF_LocalArrayCreate(1, type, kind, size, status)
      srcCenterY = ESMF_LocalArrayCreate(1, type, kind, size, status)

      !call ESMF_RouteRun(route, center_coord(1,1,1), srcCenterX, status)
      !call ESMF_RouteRun(route, center_coord(2,1,1), srcCenterY, status)

      ! Get pointers to data inside the LocalArrays
      call ESMF_LocalArrayGetData(srcCenterX, src_center_x, ESMF_DATA_REF, status)
      call ESMF_LocalArrayGetData(srcCenterY, src_center_y, ESMF_DATA_REF, status)
      
      ! now all necessary data is local

      ! Allocate space for the search to put links between grids.
      ! TODO: this code should go into its own subroutine at some point.
      allocate(tv, stat=status)
      if (status .eq. 0) then
          print *, "error in allocation of TransformValues type"
          return
      endif

      ! initialize the rest of the tv structure.
      tv%numlinks = 0
      allocate(tv%countsperdomain(recvDomainList%num_domains), stat=status)
      ! TODO: check for allocation error here
   
      ! TODO: the *4 is to guarentee the max allocation possible is enough
      !  for bilinear interpolation.  eventually the addlinks routine should
      !  grow the arrays internally.
      size_xy(1) = src_size_x*4
      size_xy(2) = src_size_y*4
      size = src_size_x * src_size_y * 4
      size_x0(1) = size_xy(1)
      size_x0(2) = 1
      tv%srcindex = ESMF_LocalArrayCreate(1, ESMF_DATA_INTEGER, ESMF_KIND_I4, &
                                          size, status)
      tv%dstindex = ESMF_LocalArrayCreate(2, ESMF_DATA_INTEGER, ESMF_KIND_I4, &
                                          size_xy, status) 
      tv%weights = ESMF_LocalArrayCreate(2, ESMF_DATA_REAL, ESMF_KIND_R8, &
                                          size_x0, status)
 
      call ESMF_RouteHandleSet(rh, tdata=tv, rc=status)
    
      ! Loop through domains for the search routine
      num_domains = recvDomainList%num_domains
      start = 1
      do i = 1,num_domains
        src_size_x = recvDomainList%domains(i)%ai(1)%max &
                   - recvDomainList%domains(i)%ai(1)%min + 1
        src_size_y = recvDomainList%domains(i)%ai(2)%max &
                   - recvDomainList%domains(i)%ai(2)%min + 1
        stop  = start + src_size_x*src_size_y - 1
        call ESMF_RegridBilinearSearch(tv, src_center_x(start:stop), &
                                       src_center_y(start:stop), &
                                       src_mask(start:stop), &
                                       src_size_x, src_size_y, start, &
                                       centerCoordX, centerCoordY, dst_mask, &
                                       dst_size_x, dst_size_y, rc)
        start = stop + 1 
      enddo 

      call ESMF_LocalArrayDestroy(srcCenterX, status)
      call ESMF_LocalArrayDestroy(srcCenterY, status)
      
      ESMF_RegridConstructBilinear = rh

      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_RegridConstructBilinear

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridBilinearSearch - Searches a bilinear Regrid structure

! !INTERFACE:
      subroutine ESMF_RegridBilinearSearch(tv, srcCenterX, srcCenterY, srcMask, &
                                           srcSizeX, srcSizeY, srcStart, &
                                           dstCenterX, dstCenterY, dstMask, &
                                           dstSizeX, dstSizeY, rc)
!
! !ARGUMENTS:
      type(ESMF_TransformValues), intent(inout) :: tv
      integer, intent(in) :: srcSizeX  ! apparently these have to be first
      integer, intent(in) :: srcSizeY  ! so the compiler knows they're ints
      integer, intent(in) :: srcStart  ! when it goes to use them as dims
      integer, intent(in) :: dstSizeX  ! in the lines below.
      integer, intent(in) :: dstSizeY
      real(ESMF_IKIND_R8), dimension(srcSizeX,srcSizeY), intent(in) :: srcCenterX
      real(ESMF_IKIND_R8), dimension(srcSizeX,srcSizeY), intent(in) :: srcCenterY
      logical, dimension(srcSizeX,srcSizeY), intent(in) :: srcMask
      real(ESMF_IKIND_R8), dimension(dstSizeX,dstSizeY), intent(in) :: dstCenterX
      real(ESMF_IKIND_R8), dimension(dstSizeX,dstSizeY), intent(in) :: dstCenterY
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
      logical :: found
      integer ::           &
         i,j,n,iter,       &! loop counters
         iii,jjj,          &! more loop counters
         ip1,jp1,          &! neighbor indices
         nx_src,           &! dimension size of local DE in i-direction
         ny_src,           &! dimension size of local DE in j-direction
         ib_dst, ie_dst,   &! beg, end of exclusive domain in i-dir of dest grid
         jb_dst, je_dst,   &! beg, end of exclusive domain in j-dir of dest grid
         ib_src, ie_src,   &! beg, end of exclusive domain in i-dir of source grid
         jb_src, je_src,   &! beg, end of exclusive domain in j-dir of source grid
         my_DE, src_count

      integer, dimension(3) :: &
         src_add,          &! address in gathered source grid (i,j,DE)
         dst_add            ! address in dest grid (i,j,DE)
         
      real (ESMF_IKIND_R8) ::  &
         lon_thresh,    &! threshold for checking longitude crossing
         lon_cycle,     &! 360 for degrees, 2pi for radians
         dx1, dx2, dx3, &! differences for iterative scheme
         dy1, dy2, dy3, &! differences for iterative scheme
         iguess, jguess,&! initial guess for location within grid box
         deli, delj,    &! change in i,j position from last iteration
         mat1, mat2,    &! matrix elements for 2x2 matrix in iterative scheme
         mat3, mat4,    &! ditto
         dxp, dyp,      &
         dst_x, dst_y,  &
         determinant,   &! determinant of above matrix
         sum_wts,       &
         zero, half, one, pi

      real (ESMF_IKIND_R8), dimension(4) ::     &
         src_x,     &! x coordinate of bilinear box corners
         src_y,     &! y coordinate of bilinear box corners
         weights        ! bilinear weights for single box

      integer, dimension(:), allocatable :: &
         src_DE_overlap,    &! array to use for determining overlapping DEs
         src_DE_gather       ! array to keep track of source DEs to gather up

      real (ESMF_IKIND_R8), dimension(:,:,:), allocatable :: &
         src_center_x,      &! cell center x-coord for gathered source grid
         src_center_y        ! cell center y-coord for gathered source grid

      integer, parameter :: &
         max_iter = 100   ! max iteration count for i,j iteration

      real (ESMF_IKIND_R8), parameter :: &
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

      !   ib_dst = beginning index in 1st dir of exclusive domain on dst DE
      !   ie_dst = ending    index in 1st dir of exclusive domain on dst DE
      !   jb_dst = beginning index in 2nd dir of exclusive domain on dst DE
      !   je_dst = ending    index in 2nd dir of exclusive domain on dst DE

      !   dstmask     = get mask assoc with field from phys grid

      do j=jb_dst,je_dst
      do i=ib_dst,ie_dst
        dst_x = dstCenterX(i,j)
        dst_y = dstCenterY(i,j)
            
        ! only perform interpolation on un-masked points
        if (dstMask(i,j)) then
          ! for this destination point, look for the proper neighbor cells in the
          ! source grid 
          found = .false.
      !            ib_src = beginning index in 1st dir of exclusive domain on src DE
      !            ie_src = ending    index in 1st dir of exclusive domain on src DE
      !            jb_src = beginning index in 2nd dir of exclusive domain on src DE
      !            je_src = ending    index in 2nd dir of exclusive domain on src DE

          do jjj=jb_src,je_src
            do iii=ib_src,ie_src
               
              ! assume ghost cells filled so no worries about boundaries
              ip1 = iii + 1
              jp1 = jjj + 1

              ! set up box used for bilinear interpolation
              src_x(1) = srcCenterX(iii,jjj)
              src_y(1) = srcCenterY(iii,jjj)
              src_x(2) = srcCenterX(ip1,jjj)
              src_y(2) = srcCenterY(ip1,jjj)
              src_x(3) = srcCenterX(ip1,jp1)
              src_y(3) = srcCenterY(ip1,jp1)
              src_x(4) = srcCenterX(iii,jp1)
              src_y(4) = srcCenterY(iii,jp1)

              ! check longitude domain in spherical coords
       !       if (dst_grid%coord_system == ESMF_CoordSystem_Spherical) then
                if (src_x(1) - dst_x >  lon_thresh) &
                    src_x(1) = src_x(1) - lon_cycle
                if (src_x(1) - dst_x < -lon_thresh) &
                    src_x(1) = src_x(1) + lon_cycle
                if (src_x(2) - dst_x >  lon_thresh) &
                    src_x(2) = src_x(2) - lon_cycle
                if (src_x(2) - dst_x < -lon_thresh) &
                    src_x(2) = src_x(2) + lon_cycle
                if (src_x(3) - dst_x >  lon_thresh) &
                    src_x(3) = src_x(3) - lon_cycle
                if (src_x(3) - dst_x < -lon_thresh) &
                    src_x(3) = src_x(3) + lon_cycle
                if (src_x(4) - dst_x >  lon_thresh) &
                    src_x(4) = src_x(4) - lon_cycle
                if (src_x(4) - dst_x < -lon_thresh) &
                    src_x(4) = src_x(4) + lon_cycle
       !       endif

              ! check to see if point inside cell
       !       found = ESMF_PhysGridCellContainsPoint(dst_x, dst_y, &
       !                                              src_x, src_y, rc)

       !       if (found) exit search_loop
            enddo   ! i-loop on src DE
          enddo   ! j-loop on src DE
               
          ! if we've found a bilinear box containing the point continue with
          ! computation of weights
          if (found) then

            ! check to see if src mask is true at all points
            src_count = 0
            if (srcMask(iii,jjj)) src_count = src_count + 1
            if (srcMask(ip1,jjj)) src_count = src_count + 1
            if (srcMask(iii,jp1)) src_count = src_count + 1
            if (srcMask(ip1,jp1)) src_count = src_count + 1

            ! if all four are valid points, compute bilinear weights using
            ! iterative method
            if (src_count == 4) then

              dx1 = src_x(2) - src_x(1)
              dx2 = src_x(4) - src_x(1)
              dx3 = src_x(3) - src_x(2) - dx2
                     
              dy1 = src_y(2) - src_y(1)
              dy2 = src_y(4) - src_y(1)
              dy3 = src_y(3) - src_y(2) - dy2

              iguess = half
              jguess = half

              iter_loop1: do iter=1,max_iter

                ! compute deviation between search point and current guess
                dxp = dst_x - src_x(1) - dx1*iguess - dx2*jguess - &
                                         dx3*iguess*jguess
                dyp = dst_y - src_y(1) - dy1*iguess - dy2*jguess - &
                                         dy3*iguess*jguess
                                          
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

              if (iter <= max_iter) then

                !*** successfully found i,j - compute weights
                weights(1) = (one-iguess)*(one-jguess)
                weights(2) = iguess*(one-jguess)
                weights(3) = iguess*jguess
                weights(4) = (one-iguess)*jguess

              else  ! iteration failed
                print *,'Point coords: ',dst_x,dst_y
                print *,'Source cell coords: ',src_x(:),src_y(:)
                print *,'Current i,j : ',iguess, jguess
                print *,'Iteration for i,j exceed max iteration count'
                print *, "ERROR in ESMF_RegridBilinearSearch"
                rc = ESMF_FAILURE
                return
              endif

            ! if not all four points in box are valid (unmasked) default to a
            ! distance-weighted average
            else if (src_count > 0 .and. src_count < 4) then

              sum_wts = zero
              if (srcmask(iii,jjj)) then
   !             weights(1) = ESMF_GridComputeDistance(src_x(1),src_y(1), &
   !                                                   dst_x, dst_y,      &
   !                                                   src_grid%coord_system, status)
                sum_wts = sum_wts + weights(1)
              endif
              if (srcmask(ip1,jjj)) then
   !             weights(2) = ESMF_GridComputeDistance(src_x(2),src_y(2), &
   !                                                   dst_x, dst_y,      &
   !                                                   src_grid%coord_system, status)
                sum_wts = sum_wts + weights(2)
              endif
              if (srcmask(ip1,jp1)) then
   !             weights(3) = ESMF_GridComputeDistance(src_x(3),src_y(3), &
   !                                                   dst_x, dst_y,      &
   !                                                   src_grid%coord_system, status)
                sum_wts = sum_wts + weights(3)
              endif
              if (srcmask(iii,jp1)) then
                src_count = src_count + 1
   !             weights(4) = ESMF_GridComputeDistance(src_x(4),src_y(4), &
   !                                                   dst_x, dst_y,      &
   !                                                   src_grid%coord_system, status)
                sum_wts = sum_wts + weights(4)
              endif
              weights(:) = weights(:)/sum_wts

            endif

            ! now store this link into address, weight arrays
            if (srcmask(iii,jjj)) then
              dst_add(1) = i
              dst_add(2) = j
   !           dst_add(3) = dst_DEid
              src_add(1) = iii
              src_add(2) = jjj
   !           src_add(3) = src_DEid
              call ESMF_RegridAddLink(tv, src_add, dst_add, weights(1), rc)
            endif
            if (srcmask(ip1,jjj)) then
              dst_add(1) = i
              dst_add(2) = j
   !           dst_add(3) = dst_DEid
              src_add(1) = ip1
              src_add(2) = jjj
   !           src_add(3) = src_DEid
              call ESMF_RegridAddLink(tv, src_add, dst_add, weights(2), rc)
            endif
            if (srcmask(ip1,jp1)) then
              dst_add(1) = i
              dst_add(2) = j
   !           dst_add(3) = dst_DEid
              src_add(1) = ip1
              src_add(2) = jp1
   !           src_add(3) = src_DEid
              call ESMF_RegridAddLink(tv, src_add, dst_add, weights(3), rc)
            endif
            if (srcmask(iii,jp1)) then
              dst_add(1) = i
              dst_add(2) = j
   !           dst_add(3) = dst_DEid
              src_add(1) = iii
              src_add(2) = jp1
   !           src_add(3) = src_DEid
              call ESMF_RegridAddLink(tv, src_add, dst_add, weights(4), rc)
            endif

          endif ! found box
        endif ! dst mask            
      enddo ! i loop on dst grid DE
      enddo ! j loop on dst grid DE

      if(rcpresent) rc = ESMF_SUCCESS
      
      end subroutine ESMF_RegridBilinearSearch

!------------------------------------------------------------------------------

   end module ESMF_RegridBilinearMod
