! $Id: ESMF_RegridConserv.F90,v 1.12 2004/01/28 21:46:49 nscollins Exp $
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
!     ESMF Conserv Regrid Module
      module ESMF_RegridConservMod
!
!==============================================================================
!
! This file contains the Regrid class methods for conservative regridding.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
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
      use ESMF_BaseMod      ! ESMF base   class
      use ESMF_ArrayBaseMod ! ESMF array  class
      use ESMF_ArrayExpandMod ! ESMF array  class
      use ESMF_DistGridMod  ! ESMF distributed grid class
      use ESMF_PhysGridMod  ! ESMF physical grid class
      use ESMF_GridMod      ! ESMF grid   class
      use ESMF_FieldMod     ! ESMF field  class
      use ESMF_BundleMod    ! ESMF bundle class
      use ESMF_RegridTypesMod ! ESMF regrid data structures
      implicit none

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

    public ESMF_RegridConstructConserv ! create and fill a regrid object
                                        ! for a conservative regridding

!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RegridConserv.F90,v 1.12 2004/01/28 21:46:49 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_RegridConstructConserv

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_RegridConsByFieldConserv
         module procedure ESMF_RegridConsByBundleConserv

! !DESCRIPTION:
!     This interface provides a single entry to the Regrid construct methods 
!     specifically for a conservative regridding. 
!
!EOP
      end interface
!
!==============================================================================

      contains

!==============================================================================
!
! This section includes the conservative Regrid construct methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridConsByFieldConserv - Constructs conservative Regrid structure for a field pair

! !INTERFACE:
      function ESMF_RegridConsByFieldConserv(src_field, dst_field, &
                                             name, order, rc,      &
                                             src_mask,  dst_mask)
                                             !dst_frac)
!
! !RETURN VALUE:
      type(ESMF_RegridType) :: ESMF_RegridConsByFieldConserv
!
! !ARGUMENTS:

      type (ESMF_Field), intent(in) :: &
         src_field,          &! field to be regridded
         dst_field            ! destination (incl grid) of resulting regridded field

      character (len = *), intent(in) :: name

      integer, intent(in) :: order ! order (numerical accuracy) of regrid

      integer, intent(out), optional :: rc

      type (ESMF_Array), intent(in), optional :: &
         src_mask,          &! logical masks to determine which points
         dst_mask            !   take part in regridding

      !type (ESMF_Array), intent(out), optional :: &
      !   dst_frac            ! area fraction of destination grid cell 
      !                       !   covered by unmasked source grid cells
      !                       !   (eg ocean fraction in each atm cell for an
      !                       !   ocean to atmosphere regridding)

!
! !DESCRIPTION:
!     Given a source field and destination field (and their attached
!     grids), this routine constructs a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     field to the destination field using a conservative interpolation.  
!     Returns a pointer to a new {\tt Regrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[src\_field]
!          Field to be regridded.
!     \item[dst\_field]
!          Resultant field where regridded source field will be stored.
!     \item[name]
!          {\tt Regrid} name.
!     \item[order]
!          Order of conservative scheme.  The algorithm supports both
!          first (1) and second (2) order accurate schemes.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \item[[src\_mask]]
!          Logical mask to denote which source grid points take part in 
!          regridding.
!     \item[[dst\_mask]]
!          Logical mask to denote which destination grid points take part 
!          in regridding.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer ::           &
         i,j,n,iDE,iter,   &! loop counters
         iii,jjj,          &! more loop counters
         ip1,jp1,          &! neighbor indices
         tot_dst_DEs,      &! total num DEs in destination grid distribution
         tot_src_DEs,      &! total num DEs in source      grid distribution
         loc_dst_DEs,      &! num of local DEs in dest   distribution
         loc_src_DEs,      &! num of local DEs in source distribution
         nx_src,           &! dimension size of local DE in i-direction
         ny_src,           &! dimension size of local DE in j-direction
         noverlap_src_DEs, &! num overlapping source DEs
         ib_dst, ie_dst,   &! beg, end of exclusive domain in i-dir of dest grid
         jb_dst, je_dst,   &! beg, end of exclusive domain in j-dir of dest grid
         status             ! error flag

      integer, dimension(3) :: &
         srcAdd,           &! address in gathered source grid (i,j,DE)
         dstAdd             ! address in dest grid (i,j,DE)
         
      real (ESMF_KIND_R8) :: &
         lon_thresh,    &! threshold for checking longitude crossing
         lon_cycle,     &! 360 for degrees, 2pi for radians
         dx1, dx2, dx3, &! differences for iterative scheme
         dy1, dy2, dy3, &! differences for iterative scheme
         iguess, jguess,&! initial guess for location within grid box
         deli, delj,    &! change in i,j position from last iteration
         mat1, mat2,    &! matrix elements for 2x2 matrix in iterative scheme
         mat3, mat4,    &! ditto
         determinant     ! determinant of above matrix
         

      real (ESMF_KIND_R8), dimension(4) ::     &
         src_DE_bbox,  &! bounding box for domain on the source DE
         dst_DE_bbox,  &! bounding box for domain on the destination DE
         corner_x,     &! x coordinate of bilinear box corners
         corner_y,     &! y coordinate of bilinear box corners
         weights        ! bilinear weights for single box

      integer, dimension(:), allocatable :: &
         src_DE_overlap,    &! array to use for determining overlapping DEs
         src_DE_gather       ! array to keep track of source DEs to gather up

      real (ESMF_KIND_R8), dimension(:,:,:), allocatable :: &
         src_center_x,      &! cell center x-coord for gathered source grid
         src_center_y        ! cell center y-coord for gathered source grid

      integer, parameter :: &
         max_iter = 100   ! max iteration count for i,j iteration

      real (ESMF_KIND_R8), parameter :: &
         converge = 1.e-10  ! convergence criterion

!
!     Construct an empty regrid structure
!

      rc = ESMF_SUCCESS
      status = ESMF_SUCCESS

      call ESMF_RegridConstructEmpty(ESMF_RegridConsByFieldConserv, status)
      if (status /= ESMF_SUCCESS) rc = ESMF_FAILURE

      !
      ! Set name and field pointers
      !
      
      ! method needs to be set based on order parm
      !call ESMF_RegridTypeSet(ESMF_RegridConsByFieldConserv,          &
      !                        name=name, src_field = src_field,               &
      !                                   dst_field = dst_field,               &
      !                                   method = ESMF_RegridMethod_Conserv2, &
      !                                   rc=status)
      if (status /= ESMF_SUCCESS) rc = ESMF_FAILURE
      
      !
      ! Extract some grid information for use in this regrid.
      !
      
      !For both grids:
      !Get grid associated with this field
      !Get grid sizes
      !tot_dst_DEs = 
      !tot_src_DEs =
      !Get grid masks

      !
      ! if spherical coordinates, set up constants for longitude branch cut
      !
                    
      !if (dst_phys_grid%coord_system == ESMF_CoordSystem_Spherical) then
      !   if (units = 'degrees') then
      !      lon_thresh = 270.0
      !      lon_cycle  = 360.0
      !   else if (units = 'radians') then
      !      lon_thresh = 1.5*pi
      !      lon_cycle  = 2.0*pi
      !   endif
      !endif

      !
      !  For each destination grid DE, broadcast the bounding box
      !  and set a flag for each source grid DE that overlaps that
      !  bounding box
      !

      ! borrow some code from other working regrid modules

      !
      ! now we know which source DEs need to be gathered to 
      ! destination DEs - use this to set up a route
      !      
      
      !get layout?...
      !regrid%gather = ESMF_RouteCreate(layout, rc)
      !create Route
      
      !deallocate(src_DE_gather, src_DE_overlap)
      
      !
      ! use Route to gather grid center coordinates from source
      ! DEs that overlap local dest DEs
      !
      
      !allocate array(s) to hold src DE info to be gathered
      !allocate (src_center_x(nx_src,ny_src,noverlap_src_DEs), &
      !          src_center_y(nx_src,ny_src,noverlap_src_DEs))
      !....
      !call ESMF_RouteRun(regrid%gather, src_grid%center_x, src_center_x, rc)
      !call ESMF_RouteRun(regrid%gather, src_grid%center_y, src_center_y, rc)
      !need to get other grid info too ?
      
      !
      ! now all necessary data is (presumably) local
      !

      !
      ! the following is code extracted from SCRIP routines and is
      ! not ESMF ready
      !
      
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------
!
!   integer (int_kind), parameter :: &
!      max_subseg = 10000       ! max number of subsegments per segment
!                               ! to prevent infinite loop
!
!   integer (int_kind) :: &
!      i,j,k,ii,jj,kk,    &! dummy loop indices
!      n, nwgt,           &! generic counters
!      corner,            &! corner of cell that segment starts from
!      next_corn,         &! corner of cell that segment ends on
!      num_subseg,        &! number of subsegments 
!      nx_src, ny_src, nsbdm_src, &! source grid size
!      nx_dst, ny_dst, nsbdm_dst   ! dest   grid size
!
!   integer (int_kind), dimension(3) :: &
!      srcAdd,            &! current address for source      grid cell
!      dstAdd              ! current address for destination grid cell
!
!   logical (log_kind) :: &
!      lcoinc,            &! flag for coincident segments
!      lreverse            ! flag for reversing direction of segment
!
!   real (dbl_kind) ::            &
!      x_intersect, y_intersect,  &! coordinates of intersection
!      xbeg, xend, ybeg, yend,    &! endpoints of current segment
!      xref, yref,                &! reference x,y for current grid cell
!      xoff, yoff,                &! offsets to nudge past intersection
!      norm_factor                 ! factor for normalizing wts
!
!   real (dbl_kind), dimension(:,:,:), allocatable :: &
!      src_frac, dst_frac,  &! fraction of unmasked area on either grid
!      dst_centroid_x, dst_centroid_y,  &! centroid coordinates
!      src_centroid_x, src_centroid_y    ! for each grid cell
!
!   real (dbl_kind), dimension(4) :: &
!      full_line        ! coordinates for full cell side line segment
!
!   real (dbl_kind), dimension(3) :: &
!      weights          ! local regridding weight array
!
!-----------------------------------------------------------------------
!
!  allocate and initialize parts of the regrid structure not already
!  set on input
!
!-----------------------------------------------------------------------
!
!   nx_src    = regrid%src_grid%dim_size(1)
!   ny_src    = regrid%src_grid%dim_size(2)
!   nsbdm_src = regrid%src_grid%dim_size(3)
!   
!   nx_dst    = regrid%dst_grid%dim_size(1)
!   ny_dst    = regrid%dst_grid%dim_size(2)
!   nsbdm_dst = regrid%dst_grid%dim_size(3)
!
!   !***
!   !*** allocate and initialize area fraction arrays, centroid arrays
!   !***
!      
!   allocate(src_frac(nx_src, ny_src, nsbdm_src), &
!            dst_frac(nx_dst, ny_dst, nsbdm_dst))
!
!   src_frac = c0
!   dst_frac = c0
!
!   allocate(src_centroid_x(nx_src, ny_src, nsbdm_src), &
!            src_centroid_y(nx_src, ny_src, nsbdm_src), &
!            dst_centroid_x(nx_dst, ny_dst, nsbdm_dst), &
!            dst_centroid_y(nx_dst, ny_dst, nsbdm_dst))
!
!   src_centroid_x = c0
!   src_centroid_y = c0
!   dst_centroid_x = c0
!   dst_centroid_y = c0
!
!   !***
!   !*** initialize address, weight arrays
!   !***
!
!   nwgt = regrid%num_wts
!
!   regrid%srcAdd  = 0
!   regrid%dstAdd  = 0
!   regrid%weights = c0
!
!!-----------------------------------------------------------------------
!!
!!  loop through each cell on source grid and
!!  perform line integrals around each cell
!!
!!-----------------------------------------------------------------------
!
!   dstAdd = 0
!   do k=1,nsbdm_src
!   do j=1,ny_src
!   do i=1,nx_src
!
!      srcAdd(1) = i
!      srcAdd(2) = j
!      srcAdd(3) = k
!
!      xref = regrid%src_grid%center_x(i,j,k)
!      yref = regrid%src_grid%center_y(i,j,k)
!      
!      !***
!      !*** integrate around this cell
!      !***
!
!      corner_loop1: do corner = 1,regrid%src_grid%num_corners
!         next_corn = mod(corner,regrid%src_grid%num_corners) + 1
!
!         !***
!         !*** define endpoints of the current segment
!         !***
!
!         xbeg = regrid%src_grid%corner_x(corner   ,i,j,k)
!         ybeg = regrid%src_grid%corner_y(corner   ,i,j,k)
!         xend = regrid%src_grid%corner_x(next_corn,i,j,k)
!         yend = regrid%src_grid%corner_y(next_corn,i,j,k)
!         full_line(1) = xbeg
!         full_line(2) = ybeg
!         full_line(3) = xend
!         full_line(4) = yend
!
!         !***
!         !*** to ensure exact path taken during both
!         !*** sweeps, always integrate segments in the same 
!         !*** direction (south to north or west to east).
!         !***
!
!         lreverse = .false.
!         if ((yend < ybeg) .or. &
!             (yend == ybeg .and. xend < xbeg)) then 
!            xbeg = regrid%src_grid%corner_x(next_corn,i,j,k)
!            ybeg = regrid%src_grid%corner_y(next_corn,i,j,k)
!            xend = regrid%src_grid%corner_x(corner   ,i,j,k)
!            yend = regrid%src_grid%corner_y(corner   ,i,j,k)
!            lreverse = .true.
!         endif
!
!         !***
!         !*** save the beginning coordinates for later use and
!         !*** initialize sub-segment counter
!         !***
!         
!         x_intersect = xbeg
!         y_intersect = ybeg
!         xoff = c0
!         yoff = c0
!         num_subseg = 0
!
!         !***
!         !*** if this is a constant-x segment, skip the rest 
!         !*** since the line integral contribution will be zero.
!         !***
!
!         if (xend == xbeg) cycle corner_loop1
!
!         !***
!         !*** integrate along this segment, detecting intersections 
!         !*** and computing the line integral for each sub-segment
!         !***
!
!         do while (ybeg /= yend .or. xbeg /= xend)
!
!            !***
!            !*** prevent infinite loops if integration gets stuck
!            !*** near cell or threshold boundary
!            !***
!
!            num_subseg = num_subseg + 1
!            if (num_subseg > max_subseg) then
!               stop 'integration stalled: num_subseg exceeded limit'
!            endif
!
!            !***
!            !*** find next intersection of this segment with a grid
!            !*** line on the destination grid.  Offset from last
!            !*** intersection to nudge into next grid cell.
!            !***
!
!            xbeg = x_intersect + xoff
!            ybeg = y_intersect + yoff
!            call SCRIP_Grid_Intersect(dstAdd, x_intersect, y_intersect,   &
!                                      xoff, yoff, lcoinc, regrid%dst_grid, &
!                                      xbeg, ybeg, xend, yend, full_line)
!            ii = dstAdd(1)
!            jj = dstAdd(2)
!            kk = dstAdd(3)
!
!            !***
!            !*** compute line integral for this subsegment.
!            !***
!
!            call line_integral(weights,                              &
!                               xbeg, x_intersect, ybeg, y_intersect, &
!                               xref, yref)
!
!            !***
!            !*** if integrating in reverse order, change
!            !*** sign of weights
!            !***
!
!            if (lreverse) then
!               weights = -weights
!            endif
!
!            !***
!            !*** store the appropriate addresses and weights. 
!            !*** also add contributions to cell areas and centroids.
!            !***
!
!            if (dstAdd(1) /= 0 .and. regrid%src_grid%mask(i,j,k)) then
!               call SCRIP_Regrid_AddLink(regrid, srcAdd, dstAdd, weights)
!               src_frac( i, j, k) = src_frac( i, j, k) + weights(1)
!               dst_frac(ii,jj,kk) = dst_frac(ii,jj,kk) + weights(1)
!            endif
!
!            regrid%src_grid%area(i,j,k) = &
!            regrid%src_grid%area(i,j,k) + weights(1)
!            src_centroid_x(i,j,k) = &
!            src_centroid_x(i,j,k) + weights(2)
!            src_centroid_y(i,j,k) = &
!            src_centroid_y(i,j,k) + weights(3)
!
!            !***
!            !*** reset ybeg and xbeg for next subsegment.
!            !***
!
!         end do
!
!         !***
!         !*** end of segment
!         !***
!
!      end do corner_loop1
!
!      !***
!      !*** finished with this cell - start on next cell
!      !***
!
!   end do ! src add loop
!   end do
!   end do
!
!!-----------------------------------------------------------------------
!!
!!  integrate around each cell on destination grid
!!
!!-----------------------------------------------------------------------
!
!   srcAdd = 0
!   do kk=1,nsbdm_dst
!   do jj=1,ny_dst
!   do ii=1,nx_dst
!
!      dstAdd(1) = ii
!      dstAdd(2) = jj
!      dstAdd(3) = kk
!
!      xref = regrid%src_grid%center_x(ii,jj,kk)
!      yref = regrid%src_grid%center_y(ii,jj,kk)
!      
!      !***
!      !*** integrate around this cell
!      !***
!
!      corner_loop2: do corner = 1,regrid%dst_grid%num_corners
!         next_corn = mod(corner,regrid%dst_grid%num_corners) + 1
!
!         !***
!         !*** define endpoints of the current segment
!         !***
!
!         xbeg = regrid%dst_grid%corner_x(corner   ,ii,jj,kk)
!         ybeg = regrid%dst_grid%corner_y(corner   ,ii,jj,kk)
!         xend = regrid%dst_grid%corner_x(next_corn,ii,jj,kk)
!         yend = regrid%dst_grid%corner_y(next_corn,ii,jj,kk)
!         full_line(1) = xbeg
!         full_line(2) = ybeg
!         full_line(3) = xend
!         full_line(4) = yend
!
!         !***
!         !*** to ensure exact path taken during both
!         !*** sweeps, always integrate segments in the same 
!         !*** direction (south to north or west to east).
!         !***
!
!         lreverse = .false.
!         if ((yend < ybeg) .or. &
!             (yend == ybeg .and. xend < xbeg)) then 
!            xbeg = regrid%dst_grid%corner_x(next_corn,ii,jj,kk)
!            ybeg = regrid%dst_grid%corner_y(next_corn,ii,jj,kk)
!            xend = regrid%dst_grid%corner_x(corner   ,ii,jj,kk)
!            yend = regrid%dst_grid%corner_y(corner   ,ii,jj,kk)
!            lreverse = .true.
!         endif
!
!         !***
!         !*** initialize sub-segment counter and initial endpoint
!         !***
!         
!         x_intersect = xbeg
!         y_intersect = ybeg
!         xoff = c0
!         yoff = c0
!         num_subseg = 0
!
!         !***
!         !*** if this is a constant-x segment, skip the rest 
!         !*** since the line integral contribution will be c0.
!         !***
!
!         if (xend == xbeg) cycle corner_loop2
!
!         !***
!         !*** integrate along this segment, detecting intersections 
!         !*** and computing the line integral for each sub-segment
!         !***
!
!         do while (ybeg /= yend .or. xbeg /= xend)
!
!            !***
!            !*** prevent infinite loops if integration gets stuck
!            !*** near cell or threshold boundary
!            !***
!
!            num_subseg = num_subseg + 1
!            if (num_subseg > max_subseg) then
!               stop 'integration stalled: num_subseg exceeded limit'
!            endif
!
!            !***
!            !*** find next intersection of this segment with a grid
!            !*** line on the destination grid.  Offset from last
!            !*** intersection to nudge into next grid cell.
!            !***
!
!            xbeg = x_intersect + xoff
!            ybeg = y_intersect + yoff
!            call SCRIP_Grid_Intersect(srcAdd, x_intersect, y_intersect,    &
!                                      xoff, yoff, lcoinc, regrid%src_grid, &
!                                      xbeg, ybeg, xend, yend, full_line)
!            i = srcAdd(1)
!            j = srcAdd(2)
!            k = srcAdd(3)
!
!            !***
!            !*** compute line integral for this subsegment.
!            !***
!
!            call line_integral(weights,                              &
!                               xbeg, x_intersect, ybeg, y_intersect, &
!                               xref, yref)
!
!            !***
!            !*** if integrating in reverse order, change
!            !*** sign of weights
!            !***
!
!            if (lreverse) then
!               weights = -weights
!            endif
!
!            !***
!            !*** store the appropriate addresses and weights. 
!            !*** also add contributions to cell areas and centroids.
!            !***
!
!            if (srcAdd(1) /= 0) then
!               if (regrid%src_grid%mask(i,j,k)) then
!                  call SCRIP_Regrid_AddLink(regrid, srcAdd, dstAdd, weights)
!                  src_frac( i, j, k) = src_frac( i, j, k) + weights(1)
!                  dst_frac(ii,jj,kk) = dst_frac(ii,jj,kk) + weights(1)
!               endif
!            endif
!
!            regrid%dst_grid%area(ii,jj,kk) = &
!            regrid%dst_grid%area(ii,jj,kk) + weights(1)
!            dst_centroid_x(ii,jj,kk) = &
!            dst_centroid_x(ii,jj,kk) + weights(2)
!            dst_centroid_y(ii,jj,kk) = &
!            dst_centroid_y(ii,jj,kk) + weights(3)
!
!            !***
!            !*** reset ybeg and xbeg for next subsegment.
!            !***
!
!            xbeg = x_intersect
!            ybeg = y_intersect
!         end do
!
!         !***
!         !*** end of segment
!         !***
!
!      end do corner_loop2
!
!      !***
!      !*** finished with this cell - start on next cell
!      !***
!
!   end do ! dst add loops
!   end do
!   end do
!
!!-----------------------------------------------------------------------
!!
!!  correct for situations where N/S pole not explicitly included in
!!  grid (i.e. as a grid corner point). if pole is missing from only
!!  one grid, need to correct only the area and centroid of that 
!!  grid.  if missing from both, do complete weight calculation.
!!
!!-----------------------------------------------------------------------
!
!   !*** North Pole
!   weights(1) =  pi2
!   weights(2) =  pi*pi
!   weights(3) =  c0
!
!   srcAdd = 0
!   pole_loop1: do k=1,nsbdm_src
!   do j=1,ny_src
!   do i=1,nx_src
!      if (regrid%src_grid%area(i,j,k) < -1.5*pi .and. &
!          regrid%src_grid%center_y(i,j,k) > c0) then
!         srcAdd(1) = i
!         srcAdd(2) = j
!         srcAdd(3) = k
!         exit pole_loop1
!      endif
!   end do
!   end do
!   end do pole_loop1
!
!   dstAdd = 0
!   pole_loop2: do kk=1,nsbdm_dst
!   do jj=1,ny_dst
!   do ii=1,nx_dst
!      if (regrid%dst_grid%area(ii,jj,kk) < -1.5*pi .and. &
!          regrid%dst_grid%center_y(ii,jj,kk) > c0) then
!         dstAdd(1) = ii
!         dstAdd(2) = jj
!         dstAdd(3) = kk
!         exit pole_loop2
!      endif
!   end do
!   end do
!   end do pole_loop2
!
!   if (srcAdd(1) /=0) then
!      regrid%src_grid%area(i,j,k) = &
!      regrid%src_grid%area(i,j,k) + weights(1)
!      src_centroid_x(i,j,k) = &
!      src_centroid_x(i,j,k) + weights(2)
!      src_centroid_y(i,j,k) = &
!      src_centroid_y(i,j,k) + weights(3)
!   endif
!
!   if (dstAdd(1) /=0) then
!      regrid%dst_grid%area(ii,jj,kk) = &
!      regrid%dst_grid%area(ii,jj,kk) + weights(1)
!      dst_centroid_x(ii,jj,kk) = &
!      dst_centroid_x(ii,jj,kk) + weights(2)
!      dst_centroid_y(ii,jj,kk) = &
!      dst_centroid_y(ii,jj,kk) + weights(3)
!   endif
!
!   if (srcAdd(1) /= 0 .and. dstAdd(1) /=0) then
!      call SCRIP_Regrid_AddLink(regrid, srcAdd, dstAdd, weights)
!
!      src_frac( i, j, k) = src_frac( i, j, k) + weights(1)
!      dst_frac(ii,jj,kk) = dst_frac(ii,jj,kk) + weights(1)
!   endif
!
!   !*** South Pole
!   weights(1) =  pi2
!   weights(2) = -pi*pi
!   weights(3) =  c0
!
!   srcAdd = 0
!   pole_loop3: do k=1,nsbdm_src
!   do j=1,ny_src
!   do i=1,nx_src
!      if (regrid%src_grid%area(i,j,k) < -1.5*pi .and. &
!          regrid%src_grid%center_y(i,j,k) < c0) then
!         srcAdd(1) = i
!         srcAdd(2) = j
!         srcAdd(3) = k
!         exit pole_loop3
!      endif
!   end do
!   end do
!   end do pole_loop3
!
!   dstAdd = 0
!   pole_loop4: do kk=1,nsbdm_dst
!   do jj=1,ny_dst
!   do ii=1,nx_dst
!      if (regrid%dst_grid%area(ii,jj,kk) < -1.5*pi .and. &
!          regrid%dst_grid%center_y(ii,jj,kk) < c0) then
!         dstAdd(1) = ii
!         dstAdd(2) = jj
!         dstAdd(3) = kk
!         exit pole_loop4
!      endif
!   end do
!   end do
!   end do pole_loop4
!
!   if (srcAdd(1) /=0) then
!      regrid%src_grid%area(i,j,k) = &
!      regrid%src_grid%area(i,j,k) + weights(1)
!      src_centroid_x(i,j,k) = &
!      src_centroid_x(i,j,k) + weights(2)
!      src_centroid_y(i,j,k) = &
!      src_centroid_y(i,j,k) + weights(3)
!   endif
!
!   if (dstAdd(1) /=0) then
!      regrid%dst_grid%area(ii,jj,kk) = &
!      regrid%dst_grid%area(ii,jj,kk) + weights(1)
!      dst_centroid_x(ii,jj,kk) = &
!      dst_centroid_x(ii,jj,kk) + weights(2)
!      dst_centroid_y(ii,jj,kk) = &
!      dst_centroid_y(ii,jj,kk) + weights(3)
!   endif
!
!   if (srcAdd(1) /= 0 .and. dstAdd(1) /=0) then
!      call SCRIP_Regrid_AddLink(regrid, srcAdd, dstAdd, weights)
!
!      src_frac( i, j, k) = src_frac( i, j, k) + weights(1)
!      dst_frac(ii,jj,kk) = dst_frac(ii,jj,kk) + weights(1)
!   endif
!
!!-----------------------------------------------------------------------
!!
!!  finish centroid computation
!!
!!-----------------------------------------------------------------------
!
!   where (regrid%src_grid%area /= c0)
!      src_centroid_x = src_centroid_x/regrid%src_grid%area
!      src_centroid_y = src_centroid_y/regrid%src_grid%area
!   end where
!
!   where (regrid%dst_grid%area /= c0)
!      dst_centroid_x = dst_centroid_x/regrid%dst_grid%area
!      dst_centroid_y = dst_centroid_y/regrid%dst_grid%area
!   end where
!
!!-----------------------------------------------------------------------
!!
!!  include centroids in weights and normalize using destination
!!  area if requested
!!
!!-----------------------------------------------------------------------
!
!   do n=1,regrid%num_links
!      i  = regrid%srcAdd(1,n)
!      j  = regrid%srcAdd(2,n)
!      k  = regrid%srcAdd(3,n)
!      ii = regrid%dstAdd(1,n)
!      jj = regrid%dstAdd(2,n)
!      kk = regrid%dstAdd(3,n)
!      weights = regrid%weights(:,n)
!
!      !select case(norm_opt)
!      !case (norm_opt_dstarea)
!      !   if (grid2_area(grid2_add) /= c0) then
!      !      if (luse_grid2_area) then
!      !         norm_factor = one/grid2_area_in(grid2_add)
!      !      else
!      !         norm_factor = one/grid2_area(grid2_add)
!      !      endif
!      !   else
!      !      norm_factor = c0
!      !   endif
!      !case (norm_opt_frcarea)
!         if (dst_frac(ii,jj,kk) /= c0) then
!            norm_factor = c1/dst_frac(ii,jj,kk)
!
!            !if (luse_grid2_area) &
!            !   norm_factor = regrid%dst_grid%area(ii,jj,kk)/ &
!            !                 (dst_frac(ii,jj,kk)*grid2_area_in(ii,jj,kk))
!            !else
!            !endif
!         else
!            norm_factor = c0
!         endif
!      !case (norm_opt_none)
!      !   norm_factor = one
!      !end select
!
!      regrid%weights(1,n) =  weights(1)*norm_factor
!      regrid%weights(2,n) = (weights(2) -                       &
!                             weights(1)*src_centroid_x(i,j,k))* &
!                            norm_factor
!      regrid%weights(3,n) = (weights(3) -                       &
!                             weights(1)*src_centroid_y(i,j,k))* &
!                            norm_factor
!
!
!   end do
!
!   where (regrid%src_grid%area /= c0) src_frac = src_frac/ &
!                                                 regrid%src_grid%area
!   where (regrid%dst_grid%area /= c0) dst_frac = dst_frac/ &
!                                                 regrid%dst_grid%area
!
!!-----------------------------------------------------------------------
!!
!!  perform some error checking on final weights
!!
!!-----------------------------------------------------------------------
!
!   do k=1,nsbdm_src
!   do j=1,ny_src
!   do i=1,nx_src
!      if (regrid%src_grid%area(i,j,k) < .01) then
!         print *,'Source grid area error: ', &
!                 i,j,k,regrid%src_grid%area(i,j,k)
!      endif
!      if (src_centroid_y(i,j,k) < -p5*pi - .01 .or. &
!          src_centroid_y(i,j,k) >  p5*pi + .01) then
!         print *,'Source grid centroid lat error: ',&
!                 i,j,k,src_centroid_y(i,j,k)
!      endif
!      src_centroid_x(i,j,k) = c0
!      src_centroid_y(i,j,k) = c0
!   end do
!   end do
!   end do
!
!   do kk=1,nsbdm_dst
!   do jj=1,ny_dst
!   do ii=1,nx_dst
!      if (regrid%dst_grid%area(ii,jj,kk) < .01) then
!         print *,'Destination grid area error: ', &
!                 ii,jj,kk,regrid%dst_grid%area(ii,jj,kk)
!      endif
!      if (src_centroid_y(ii,jj,kk) < -p5*pi - .01 .or. &
!          src_centroid_y(ii,jj,kk) >  p5*pi + .01) then
!         print *,'Destination grid centroid lat error: ',&
!                 ii,jj,kk,dst_centroid_y(ii,jj,kk)
!      endif
!      dst_centroid_x(i,j,k) = c0
!      dst_centroid_y(i,j,k) = c0
!   end do
!   end do
!   end do
!
!   do n=1,regrid%numLinks
!      srcAdd = regrid%srcAdd(:,n)
!      dstAdd = regrid%dstAdd(:,n)
!        
!      if (regrid%weights(1,n) < -.01) then
!         print *,'Regrid weight < 0 ',srcAdd,dstAdd,regrid%weights(1,n)
!      endif
!      !if (norm_opt /= norm_opt_none .and. wts_map1(1,n) > 1.01) then
!      if (regrid%weights(1,n) > 1.01) then
!         print *,'Regrid weight > 1 ',srcAdd,dstAdd,regrid%weights(1,n)
!      endif
!      !*** sum the 1st weight for east dest grid point
!      dst_centroid_y(dstAdd(1),dstAdd(2),dstAdd(3)) = & 
!      dst_centroid_y(dstAdd(1),dstAdd(2),dstAdd(3)) + regrid%weights(1,n)
!   end do
!
!   !*** check whether weights sum to the correct values
!   do kk=1,nsbdm_dst
!   do jj=1,ny_dst
!   do ii=1,nx_dst
!      !select case(norm_opt)
!      !case (norm_opt_dstarea)
!      !   norm_factor = grid2_frac(grid2_add)
!      !case (norm_opt_frcarea)
!          norm_factor = c1
!      !case (norm_opt_none)
!      !   if (luse_grid2_area) then
!      !      norm_factor = grid2_area_in(grid2_add)
!      !   else
!      !      norm_factor = grid2_area(grid2_add)
!      !   endif
!      !end select
!      if (abs(dst_centroid_y(ii,jj,kk)-norm_factor) > .01) then
!         print *,'Error: sum of weights for regrid ',ii,jj,kk, &
!                  dst_centroid_y(ii,jj,kk),norm_factor
!      endif
!   end do
!   end do
!   end do
!
!!-----------------------------------------------------------------------
!!
!!  clean up and exit
!!
!!-----------------------------------------------------------------------
!
!   deallocate(src_frac, dst_frac, src_centroid_x, src_centroid_y, &
!                                  dst_centroid_x, dst_centroid_y)
!
!!-----------------------------------------------------------------------
!!EOC
!
! end subroutine SCRIP_Regrid_Create_Conserv
!
!!***********************************************************************
!!BOP
!! !IROUTINE: line_integral
!! !INTERFACE:
!
! subroutine line_integral(weights,                              &
!                          xbeg, xend, ybeg, yend, xref, yref)
!
!
!! !DESCRIPTION:
!!  This routine computes the line integral of the functions that
!!  result in the area overlaps and second-order weights (mean
!!  distance from centroid).  The line is defined by the input 
!!  lat/lon of the endpoints and the path is assumed to be linear in
!!  lat/lon space.  x is assumed to be longitude; y is latitude, both
!!  in radians.
!!
!! !REVISION HISTORY:
!!  same as module
!
!! !OUTPUT PARAMETERS:
!
!   real (dbl_kind), dimension(3), intent(out) :: &
!      weights    ! the line-integral contribution to each weight
!
!! !INPUT PARAMETERS:
!
!   real (dbl_kind), intent(in) :: &
!      xbeg, ybeg,     &! x,y coordinates of beginning endpoint
!      xend, yend,     &! x,y coordinates of end       endpoint
!      xref, yref       ! reference x,y to insure consistency in longitude
!
!!EOP
!!BOC
!!-----------------------------------------------------------------------
!!
!!  local variables
!!
!!-----------------------------------------------------------------------
!
!   real (dbl_kind) :: &
!      dphi,           &! longitude difference for integral
!      sinth1, sinth2, &! sines of latitude endpoints
!      costh1, costh2, &! cosines of latitude endpoint
!      f1, f2,         &! longitude weight function evaluated at endpoints
!      phi1, phi2,     &! longitude differences
!      fac, fint        ! for use in longitude integrals
!
!!-----------------------------------------------------------------------
!!
!!  weights for the general case based on a trapezoidal approx to
!!  the integrals.
!!
!!-----------------------------------------------------------------------
!
!   sinth1 = SIN(ybeg)
!   sinth2 = SIN(yend)
!   costh1 = COS(ybeg)
!   costh2 = COS(yend)
!
!   dphi = xbeg - xend
!   if (dphi >  pi) then
!      dphi = dphi - pi2
!   else if (dphi < -pi) then
!      dphi = dphi + pi2
!   endif
!   dphi = p5*dphi
!
!!-----------------------------------------------------------------------
!!
!!  the first weight is the area overlap integral. the third is
!!  the second-order latitude gradient weights.
!!
!!-----------------------------------------------------------------------
!
!   weights(1) = dphi*(sinth1 + sinth2)
!   weights(3) = dphi*(costh1 + costh2 + (ybeg*sinth1 + yend*sinth2))
!
!!-----------------------------------------------------------------------
!!
!!  the second weight is the second-order longitude gradient
!!  component.  must be careful of longitude range.
!!
!!-----------------------------------------------------------------------
!
!   f1 = p5*(costh1*sinth1 + ybeg)
!   f2 = p5*(costh2*sinth2 + yend)
!
!   phi1 = xbeg - xref
!   if (phi1 >  pi) then
!      phi1 = phi1 - pi2
!   else if (phi1 < -pi) then
!      phi1 = phi1 + pi2
!   endif
!
!   phi2 = xend - xref
!   if (phi2 >  pi) then
!      phi2 = phi2 - pi2
!   else if (phi2 < -pi) then
!      phi2 = phi2 + pi2
!   endif
!
!   if ((phi2-phi1) <  pi .and. (phi2-phi1) > -pi) then
!      weights(2) = dphi*(phi1*f1 + phi2*f2)
!   else
!      if (phi1 > c0) then
!         fac = pi
!      else
!         fac = -pi
!      endif
!      fint = f1 + (f2-f1)*(fac-phi1)/abs(dphi)
!      weights(2) = p5*phi1*(phi1-fac)*f1 - &
!                   p5*phi2*(phi2+fac)*f2 + &
!                   p5*fac*(phi1+phi2)*fint
!   endif
!
!!-----------------------------------------------------------------------
!!EOC
!
! end subroutine line_integral
!
      
      end function ESMF_RegridConsByFieldConserv

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridConsByBundleConserv - Constructs conservative Regrid structure for a bundle pair

! !INTERFACE:
      function ESMF_RegridConsByBundleConserv(src_bundle, dst_bundle, &
                                                   name, order, rc)
!
! !RETURN VALUE:
      type(ESMF_RegridType) :: ESMF_RegridConsByBundleConserv
!
! !ARGUMENTS:

      type (ESMF_Bundle), intent(in) :: &
         src_bundle,          &! field bundle to be regridded
         dst_bundle            ! destination (incl grid) of resulting regridded bundle

      character (len = *), intent(in) :: name

      integer, intent(in) :: order ! order (numerical accuracy) of regrid

      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     Given a source field bundle and destination field bundle (and their attached
!     grids), this routine constructs a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     bundle to the destination bundle using conservative interpolation.
!
!     The arguments are:
!     \begin{description}
!     \item[src\_bundle]
!          Field to be regridded.
!     \item[dst\_bundle]
!          Resultant field where regridded source field will be stored.
!     \item[name]
!          {\tt Regrid} name.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      !TODO: Insert code here
      type (ESMF_RegridType) :: temp

      ! prevent compiler errors on some architectures which
      ! insist functions have a return value
      ESMF_RegridConsByBundleConserv = temp
 
      end function ESMF_RegridConsByBundleConserv

!------------------------------------------------------------------------------

   end module ESMF_RegridConservMod
