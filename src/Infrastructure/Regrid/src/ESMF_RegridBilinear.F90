! $Id: ESMF_RegridBilinear.F90,v 1.5 2003/06/11 23:08:29 nscollins Exp $
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
      use ESMF_ArrayMod     ! ESMF array  class
      use ESMF_FieldMod     ! ESMF field  class
      use ESMF_BundleMod    ! ESMF bundle class
      use ESMF_GridMod      ! ESMF grid   class
      use ESMF_PhysGridMod  ! ESMF physical grid class
      use ESMF_DistGridMod  ! ESMF distributed grid class
      use ESMF_RegridTypesMod ! ESMF regrid data structures
      implicit none

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

    public ESMF_RegridConstructBilinear ! create and fill a regrid object
                                        ! for a bilinear regridding

!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RegridBilinear.F90,v 1.5 2003/06/11 23:08:29 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_RegridConstructBilinear

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_RegridConsByFieldBilinear
         module procedure ESMF_RegridConsByBundleBilinear

! !DESCRIPTION:
!     This interface provides a single entry to the Regrid construct methods 
!     specifically for a bilinear regridding. 
!
!EOP
      end interface
!
!==============================================================================

      contains

!==============================================================================
!
! This section includes the bilinear Regrid construct methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridConsByFieldBilinear - Constructs bilinear Regrid structure for a field pair

! !INTERFACE:
      function ESMF_RegridConsByFieldBilinear(src_field, dst_field, &
                                                     name, rc)
!
! !RETURN VALUE:
      type(ESMF_RegridType) :: ESMF_RegridConsByFieldBilinear
!
! !ARGUMENTS:

      type (ESMF_Field), intent(in) :: &
         src_field,          &! field to be regridded
         dst_field            ! destination (incl grid) of resulting regridded field

      character (len = *), intent(in) :: name

      integer, intent(out) :: rc
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
!     \item[src\_field]
!          Field to be regridded.
!     \item[dst\_field]
!          Resultant field where regridded source field will be stored.
!     \item[[name]]
!          {\tt Regrid} name.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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
         determinant     ! determinant of above matrix
         

      real (ESMF_IKIND_R8), dimension(4) ::     &
         src_DE_bbox,  &! bounding box for domain on the source DE
         dst_DE_bbox,  &! bounding box for domain on the destination DE
         corner_x,     &! x coordinate of bilinear box corners
         corner_y,     &! y coordinate of bilinear box corners
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

!
!     Construct an empty regrid structure
!

      rc = ESMF_SUCCESS
      status = ESMF_SUCCESS

      call ESMF_RegridConsEmpty(ESMF_RegridConsByFieldBilinear, status)
      if (status /= ESMF_SUCCESS) rc = ESMF_FAILURE

      !
      ! Set name and field pointers
      !
      
      call ESMF_RegridTypeSet(ESMF_RegridConsByFieldBilinear,          &
                              name=name, src_field = src_field,               &
                                         dst_field = dst_field,               &
                                         method = ESMF_RegridMethod_Bilinear, &
                                         rc=status)
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

      !allocate(src_DE_overlap(tot_src_DEs))
      !allocate(src_DE_gather(tot_dst_DEs,tot_src_DEs))

      !do iDE = 1, tot_dst_DEs
      !   dst_DEid   ! DE id for this DE
      !   dst_DE_bbox = zero
      !   if (this_DE_is_mine) then
      !      dst_DE_bbox(1) = get local_min(1) from dest PhysGrid  ! min x
      !      dst_DE_bbox(2) = get local_max(1) from dest PhysGrid  ! max x
      !      dst_DE_bbox(3) = get local_min(2) from dest PhysGrid  ! min y
      !      dst_DE_bbox(4) = get local_max(2) from dest PhysGrid  ! max y
            
            !
            ! correct for longitude crossings if spherical coords
            ! assume degrees and x is longitude
            !

            !if (dst_phys_grid%coord_system == ESMF_CoordSystem_Spherical) then
            !   if (dst_DE_bbox(2) - dst_DE_bbox(1) >  lon_thresh) &
            !      dst_DE_bbox(2) = dst_DE_bbox(2) - lon_cycle
            !   if (dst_DE_bbox(2) - dst_DE_bbox(1) < -lon_thresh) &
            !      dst_DE_bbox(2) = dst_DE_bbox(2) + lon_cycle
            !endif

         !endif ! this DE is mine
         
         !
         ! broadcast this bounding box array to all source grid DEs
         !
            
         !call broadcast(dst_DE_bbox, from this DE)
         
         !
         ! now every source grid DE checks its local domain to see
         ! if it overlaps with the dest grid DE domain
         !
         
         !src_DE_overlap(:) = 0

         !do n=1,loc_src_DEs
         !   src_DEid = ? get the DE id for this DE
         !   src_DE_bbox(1) = get local_min(1) from source PhysGrid  ! min x
         !   src_DE_bbox(2) = get local_max(1) from source PhysGrid  ! max x
         !   src_DE_bbox(3) = get local_min(2) from source PhysGrid  ! min y
         !   src_DE_bbox(4) = get local_max(2) from source PhysGrid  ! max y
            
            !
            ! make sure src bbox is in same longitude range as dst bbox
            ! assume degrees and x is longitude
            !

         !   if (src_phys_grid%coord_system == ESMF_CoordSystem_Spherical) then
         !      if (src_DE_bbox(1) - dst_DE_bbox(1) >  lon_thresh) &
         !         src_DE_bbox(1) = src_DE_bbox(1) - lon_cycle
         !      if (src_DE_bbox(1) - dst_DE_bbox(1) < -lon_thresh) &
         !         src_DE_bbox(1) = src_DE_bbox(1) + lon_cycle
         !      if (src_DE_bbox(2) - dst_DE_bbox(1) >  lon_thresh) &
         !         src_DE_bbox(2) = src_DE_bbox(2) - lon_cycle
         !      if (src_DE_bbox(2) - dst_DE_bbox(1) < -lon_thresh) &
         !         src_DE_bbox(2) = src_DE_bbox(2) + lon_cycle
         !   endif ! Spherical coords

         !   if  (src_DE_bbox(1) <= dst_DE_bbox(2) .and. &
         !        src_DE_bbox(2) >= dst_DE_bbox(1) .and. &
         !        src_DE_bbox(3) <= dst_DE_bbox(4) .and. &
         !        src_DE_bbox(4) >= dst_DE_bbox(3)) then    ! bboxes overlap
         !      src_DE_overlap(src_DEid) = 1
         !   endif
            
         !end do ! loop over local source DEs
         
         !
         ! sum the src_DE_overlap array across DEs to find out
         ! which source DEs overlap the destination DE
         !

         !src_DE_gather(iDE,:) = sum_across_src_DEs(src_DE_overlap(:))

      !end do ! loop over dest DEs

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
      ! now all necessary data is local
      !

      !
      ! loop through points in exclusive domain on local dest DEs
      !
      
      !do iDE =1,loc_dst_DEs
      !   ib_dst = beginning index in 1st dir of exclusive domain on dst DE
      !   ie_dst = ending    index in 1st dir of exclusive domain on dst DE
      !   jb_dst = beginning index in 2nd dir of exclusive domain on dst DE
      !   je_dst = ending    index in 2nd dir of exclusive domain on dst DE

      !   dst_center_x = get center x coords from appropriate dst phys grid
      !   dst_center_y = ...........y....etc
      !   dst_mask     = get mask assoc with field from phys grid

      !   do j=jb_dst,je_dst
      !   do i=ib_dst,ie_dst
      !      dst_x = dst_center_x(i,j)
      !      dst_y = dst_center_y(i,j)
            
      !      if (dst_mask(i,j)) then ! only perform interpolation on un-masked points

               !
               ! for this destination point, look for the proper
               ! neighbor cells in the source grid 
               !
               
      !         found = .false.
            
      !         search_loop: do n=1,noverlap_src_DEs
      !            ! check bbox of this DE - skip if no overlap
      !            check bbox...

      !            ib_src = beginning index in 1st dir of exclusive domain on src DE
      !            ie_src = ending    index in 1st dir of exclusive domain on src DE
      !            jb_src = beginning index in 2nd dir of exclusive domain on src DE
      !            je_src = ending    index in 2nd dir of exclusive domain on src DE

      !            src_center_x = get center x coords from this src phys grid
      !            src_center_y = ...........y....etc
         
      !            do jjj=jb_src,je_src
      !            do iii=ib_src,ie_src
               
      !               ip1 = iii + 1   ! assume ghost cells filled so no worries 
      !               jp1 = jjj + 1   !   about boundaries

                     !
                     ! set up box used for bilinear interpolation
                     !
                     
      !               src_x(1) = src_center_x(iii,jjj,n)
      !               src_y(1) = src_center_y(iii,jjj,n)
      !               src_x(2) = src_center_x(ip1,jjj,n)
      !               src_y(2) = src_center_y(ip1,jjj,n)
      !               src_x(3) = src_center_x(ip1,jp1,n)
      !               src_y(3) = src_center_y(ip1,jp1,n)
      !               src_x(4) = src_center_x(iii,jp1,n)
      !               src_y(4) = src_center_y(iii,jp1,n)

                     ! check longitude domain in spherical coords
                     
      !               if (dst_grid%coord_system == ESMF_CoordSystem_Spherical) then
      !                   if (src_x(1) - dst_x >  lon_thresh) &
      !                      src_x(1) = src_x(1) - lon_cycle
      !                   if (src_x(1) - dst_x < -lon_thresh) &
      !                      src_x(1) = src_x(1) + lon_cycle
      !                   if (src_x(2) - dst_x >  lon_thresh) &
      !                      src_x(2) = src_x(2) - lon_cycle
      !                   if (src_x(2) - dst_x < -lon_thresh) &
      !                      src_x(2) = src_x(2) + lon_cycle
      !                   if (src_x(3) - dst_x >  lon_thresh) &
      !                      src_x(3) = src_x(3) - lon_cycle
      !                   if (src_x(3) - dst_x < -lon_thresh) &
      !                      src_x(3) = src_x(3) + lon_cycle
      !                   if (src_x(4) - dst_x >  lon_thresh) &
      !                      src_x(4) = src_x(4) - lon_cycle
      !                   if (src_x(4) - dst_x < -lon_thresh) &
      !                      src_x(4) = src_x(4) + lon_cycle
      !               endif

                     !
                     ! check to see if point inside cell
                     !
                     
       !              found = ESMF_PhysGridCellContainsPoint(dst_x, dst_y, &
       !                                                     src_x, src_y, rc)

       !              if (found) exit search_loop
       !           end do   ! i-loop on src DE
       !           end do   ! j-loop on src DE
       !        end do search_loop ! loop over source DEs
               
               !
               !  if we've found a bilinear box containing the point
               !  continue with computation of weights
               !
               
       !        if (found) then
               
                  !
                  ! check to see if src mask is true at all points
                  !
                  
       !           src_count = 0
       !           if (src_mask(iii,jjj)) src_count = src_count + 1
       !           if (src_mask(ip1,jjj)) src_count = src_count + 1
       !           if (src_mask(iii,jp1)) src_count = src_count + 1
       !           if (src_mask(ip1,jp1)) src_count = src_count + 1
                  
                  !
                  ! if all four are valid points, compute bilinear
                  ! weights using iterative method
                  !
                  
       !           if (src_count == 4) then

       !              dx1 = src_x(2) - src_x(1)
       !              dx2 = src_x(4) - src_x(1)
       !              dx3 = src_x(3) - src_x(2) - dx2
                     
       !              dy1 = src_y(2) - src_y(1)
       !              dy2 = src_y(4) - src_y(1)
       !              dy3 = src_y(3) - src_y(2) - dy2

       !              iguess = half
       !              jguess = half

       !              iter_loop1: do iter=1,max_iter

                        ! compute deviation between search point and
                        ! current guess
                        
       !                 dxp = dst_x - src_x(1) - dx1*iguess - dx2*jguess - &
       !                                          dx3*iguess*jguess
       !                 dyp = dst_y - src_y(1) - dy1*iguess - dy2*jguess - &
       !                                          dy3*iguess*jguess
                                          
                        ! compute matrix elements and determinant
                        ! for deli, delj system of equations
                        
        !                mat1 = dx1 + dx3*jguess
        !                mat2 = dx2 + dx3*iguess
        !                mat3 = dy1 + dy3*jguess
        !                mat4 = dy2 + dy3*iguess

        !                determinant = mat1*mat4 - mat2*mat3

                        ! solve 2x2 system for deli, delj
                        
        !                deli = (dxp*mat4 - dyp*mat2)/determinant
        !                delj = (dyp*mat1 - dxp*mat3)/determinant

        !                if (abs(deli) < converge .and. &
        !                    abs(delj) < converge) exit iter_loop1

        !                iguess = iguess + deli
        !                jguess = jguess + delj

        !             end do iter_loop1

        !             if (iter <= max_iter) then
                        !*** successfully found i,j - compute weights

        !                weights(1) = (one-iguess)*(one-jguess)
        !                weights(2) = iguess*(one-jguess)
        !                weights(3) = iguess*jguess
        !                weights(4) = (one-iguess)*jguess

        !             else  ! iteration failed
        !                print *,'Point coords: ',xp,yp
        !                print *,'Source cell coords: ',src_x(:),src_y(:)
        !                print *,'Current i,j : ',iguess, jguess
        !                stop 'Iteration for i,j exceed max iteration count'
        !             endif


                  !
                  ! if not all four points in box are valid (unmasked)
                  ! default to a distance-weighted average
                  !

        !          else if (src_count > 0 .and. src_count < 4) then

        !             sum_wts = zero
        !             if (src_mask(iii,jjj)) then
        !                weights(1) = ESMF_GridComputeDistance( &
        !                                    src_x(1),src_y(1), &
        !                                    dst_x, dst_y,      &
        !                                    src_grid%coord_system, status)
        !                sum_wts = sum_wts + weights(1)
        !             endif
        !             if (src_mask(ip1,jjj)) then
        !                weights(2) = ESMF_GridComputeDistance( &
        !                                    src_x(2),src_y(2), &
        !                                    dst_x, dst_y,      &
        !                                    src_grid%coord_system, status)
        !                sum_wts = sum_wts + weights(2)
        !             endif
        !             if (src_mask(ip1,jp1)) then
        !                weights(3) = ESMF_GridComputeDistance( &
        !                                    src_x(3),src_y(3), &
        !                                    dst_x, dst_y,      &
        !                                    src_grid%coord_system, status)
        !                sum_wts = sum_wts + weights(3)
        !             endif
        !             if (src_mask(iii,jp1)) then
        !                src_count = src_count + 1
        !                weights(4) = ESMF_GridComputeDistance( &
        !                                    src_x(4),src_y(4), &
        !                                    dst_x, dst_y,      &
        !                                    src_grid%coord_system, status)
        !                sum_wts = sum_wts + weights(4)
        !             endif
        !             weights(:) = weights(:)/sum_wts

        !          endif
                  
                  !
                  !  now store this link into address, weight arrays
                  !

        !          if (src_mask(iii,jjj)) then
        !             dst_add(1) = i
        !             dst_add(2) = j
        !             dst_add(3) = dst_DEid
        !             src_add(1) = iii
        !             src_add(2) = jjj
        !             src_add(3) = src_DEid
        !             call ESMF_RegridAddLink(                               &
        !                             ESMF_RegridConsByFieldBilinear, &
        !                             src_add, dst_add, weights(1), rc)
        !          endif
        !          if (src_mask(ip1,jjj)) then
        !             dst_add(1) = i
        !             dst_add(2) = j
        !             dst_add(3) = dst_DEid
        !             src_add(1) = ip1
        !             src_add(2) = jjj
        !             src_add(3) = src_DEid
        !             call ESMF_RegridAddLink(                               &
        !                             ESMF_RegridConsByFieldBilinear, &
        !                             src_add, dst_add, weights(2), rc)
        !          endif
        !          if (src_mask(ip1,jp1)) then
        !             dst_add(1) = i
        !             dst_add(2) = j
        !             dst_add(3) = dst_DEid
        !             src_add(1) = ip1
        !             src_add(2) = jp1
        !             src_add(3) = src_DEid
        !             call ESMF_RegridAddLink(                               &
        !                             ESMF_RegridConsByFieldBilinear, &
        !                             src_add, dst_add, weights(3), rc)
        !          endif
        !          if (src_mask(iii,jp1)) then
        !             dst_add(1) = i
        !             dst_add(2) = j
        !             dst_add(3) = dst_DEid
        !             src_add(1) = iii
        !             src_add(2) = jp1
        !             src_add(3) = src_DEid
        !             call ESMF_RegridAddLink(                               &
        !                             ESMF_RegridConsByFieldBilinear, &
        !                             src_add, dst_add, weights(4), rc)
        !          endif

                  
        !       endif ! found box
               
        !    endif ! dst mask            
        ! end do ! i loop on dst grid DE
        ! end do ! j loop on dst grid DE
      !end do    ! loop over local dst DEs
      
      !deallocate(src_center_x, src_center_y)
      
      end function ESMF_RegridConsByFieldBilinear

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridConsByBundleBilinear - Constructs bilinear Regrid structure for a bundle pair

! !INTERFACE:
      function ESMF_RegridConsByBundleBilinear(src_bundle, dst_bundle, &
                                                      name, rc)
!
! !RETURN VALUE:
      type(ESMF_RegridType) :: ESMF_RegridConsByBundleBilinear
!
! !ARGUMENTS:

      type (ESMF_Bundle), intent(in) :: &
         src_bundle,          &! field bundle to be regridded
         dst_bundle            ! destination (incl grid) of resulting regridded bundle

      character (len = *), intent(in) :: name

      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     Given a source field bundle and destination field bundle (and their attached
!     grids), this routine constructs a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     bundle to the destination bundle using bilinear interpolation.
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
 
      end function ESMF_RegridConsByBundleBilinear

!------------------------------------------------------------------------------

   end module ESMF_RegridBilinearMod
