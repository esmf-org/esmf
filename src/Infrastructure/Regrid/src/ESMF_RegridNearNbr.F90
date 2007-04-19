! $Id: ESMF_RegridNearNbr.F90,v 1.23 2007/04/19 20:31:12 rosalind Exp $
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
#define ESMF_FILENAME "ESMF_RegridNearNbr.F90"
!
!     ESMF Nearest-Neighbor Distance-weighted Regrid Module
      module ESMF_RegridNearNbrMod
!
!==============================================================================
!
! This file contains the Regrid class methods for nearest-neighbor 
! distance-weighted regridding.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!==============================================================================
!BOPI
! !MODULE: ESMF_RegridNearNbrMod - Nearest-neighbor distance-weighted regrid
!
! !DESCRIPTION:
!
! The code in this file implements the methods for nearest-neighbor
! distance-weighted interpolation in the ESMF Regrid class.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_BaseMod      ! ESMF base   class
      use ESMF_InternArrayMod ! ESMF internal array  class
      use ESMF_PhysGridMod  ! ESMF physical grid class
      use ESMF_GridMod      ! ESMF grid   class
      use ESMF_FieldMod     ! ESMF field  class
      use ESMF_BundleMod
      use ESMF_RegridTypesMod ! ESMF regrid data structures
      implicit none

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

    public ESMF_RegridConstructNearNbr  ! create and fill a regrid object
                                        ! for a nearest-neighbor distance
                                        ! weighted regridding

!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RegridNearNbr.F90,v 1.23 2007/04/19 20:31:12 rosalind Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface ESMF_RegridConstructNearNbr

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_RegridConsByFieldNearNbr
         module procedure ESMF_RegridConsByBundleNearNbr

! !DESCRIPTION:
!     This interface provides a single entry to the Regrid construct methods 
!     specifically for a nearest-neighbor distance-weighted regridding. 
!
!EOPI
      end interface
!
!==============================================================================

      contains

!==============================================================================
!
! This section includes the nearest-neighbor Regrid construct methods.
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridConsByFieldNearNbr"
!BOPI
! !IROUTINE: ESMF_RegridConsByFieldNearNbr - Constructs nearest-neighbor Regrid structure for a field pair

! !INTERFACE:
      function ESMF_RegridConsByFieldNearNbr(src_field, dst_field, &
                                                    name, num_nbrs, rc)
!
! !RETURN VALUE:
      type(ESMF_RegridType) :: ESMF_RegridConsByFieldNearNbr
!
! !ARGUMENTS:

      type (ESMF_Field), intent(in) :: &
         src_field,          &! field to be regridded
         dst_field            ! destination (incl grid) of resulting regridded field

      character (len = *), intent(in) :: name

      integer, intent(in), optional :: &
         num_nbrs       ! number of near neighbors to use in interpolation

      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a source field and destination field (and their attached
!     grids), this routine constructs a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     field to the destination field using a nearest-neighbor distance-weighted
!     interpolation.  Returns a pointer to a new {\tt Regrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[src\_field]
!          Field to be regridded.
!     \item[dst\_field]
!          Resultant field where regridded source field will be stored.
!     \item[name]
!          {\tt Regrid} name.
!     \item[[num\_nbrs]]
!          Number of near neighbors to use in interpolation (default is 4).
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      !integer ::           &
      !  i,j,n,iDE,iter,   &! loop counters
      !  iii,jjj,          &! more loop counters
      !  inbr,nnbrs,       &! number of neighbors
      !  tot_dst_DEs,      &! total num DEs in destination grid distribution
      !  tot_src_DEs,      &! total num DEs in source      grid distribution
      !  loc_dst_DEs,      &! num of local DEs in dest   distribution
      !  loc_src_DEs,      &! num of local DEs in source distribution
      !  nx_src,           &! dimension size of local DE in i-direction
      !  ny_src,           &! dimension size of local DE in j-direction
      !  noverlap_src_DEs, &! num overlapping source DEs
      !  ib_dst, ie_dst,   &! beg, end of exclusive domain in i-dir of dest grid
      !  jb_dst, je_dst,   &! beg, end of exclusive domain in j-dir of dest grid

      integer ::           &
         localrc            ! error flag

      !integer, dimension(:,:), allocatable :: &
      !   srcAdd             ! src neighbor addresses (nnbr,3)

      !integer, dimension(:,:,:), allocatable :: &
      !   dstadd             ! address in dest grid (i,j,DE)
         
      !real(ESMF_KIND_R8), dimension(:), allocatable :: &
      !   weights,          &! normalized distance to each neighbor
      !   wgtstmp            ! temp for consistency with AddLink call

      !real(ESMF_KIND_R8) :: &
      !   distance,         &! distance to current neighbor
      !   x1, y1,           &! dst grid x,y coordinates
      !   x2, y2             ! src grid x,y coordinates

      !real (ESMF_KIND_R8), dimension(:,:,:), allocatable :: &
      !   src_center_x,      &! cell center x-coord for gathered source grid
      !   src_center_y        ! cell center y-coord for gathered source grid

!
!     Construct an empty regrid structure
!
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      call ESMF_RegridConstructEmpty(ESMF_RegridConsByFieldNearNbr, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      !
      ! Set name and field pointers
      !
      
      !call ESMF_RegridTypeSet(ESMF_RegridConsByFieldNearNbr,          &
      !                        name=name, src_field = src_field,       &
      !                                   dst_field = dst_field,       &
      !                                   method = ESMF_REGRID_METHOD_NEAR_NBR, &
      !                                   rc = localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      
      !
      ! Extract some grid information for use in this regrid.
      !
      
      !For both src,dst fields:
      !Get grid associated with this field
      !dst_grid = ?
      !src_grid = ?
      !call ESMF_GridGet(dst_grid, coord_system=coord_system, localrc)
      !Check that src grid in same coord system


      !
      !  Find all DEs on source grid whose domain overlaps
      !  local dest grid domains (using bounding boxes).
      !  Gather up grid info (primarily center coords) from 
      !  those source DEs that overlap.
      !

      ! find overlapping DEs

      !
      ! now we know which source DEs need to be gathered to 
      ! destination DEs - use this to set up a route
      !      
      
      !get layout?...
      !regrid%gather = ESMF_RouteCreate(layout, rc)
      !create Route
      
      !
      ! use Route to gather grid center coordinates from source
      ! DEs that overlap local dest DEs
      !
      
      !allocate array(s) to hold src DE info to be gathered
      !allocate (src_center_x(nx_src,ny_src,noverlap_src_DEs), &
      !          src_center_y(nx_src,ny_src,noverlap_src_DEs))
      !....
      !call ESMF_RouteRun(regrid%gather, src_grid%center_coord, &
      !                                  src_center_coord, rc)
      !src_center_x = src_center_coord(1,:,:,?)
      !src_center_y = src_center_coord(2,:,:,?)
      !need to get other grid info too ?
      
      !
      ! now all necessary data is local
      !

      !
      ! determine number of neighbors and allocate add, weight
      ! arrays
      !

      !if (present(num_nbrs)) then
      !   nnbrs = num_nbrs
      !else
      !   nnbrs = 4
      !endif

      !allocate(srcAdd(nnbrs,3), weights(nnbrs), wgtstmp(1))

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
      !      x1 = dst_center_x(i,j)
      !      y1 = dst_center_y(i,j)
            
      !      if (dst_mask(i,j)) then ! only perform interpolation on un-masked points

               !
               ! for each un-masked cell on the gathered source grid DEs,
               ! compute the distance and store the nearest neighbor
               ! addresses and distances
               !

      !         dstAdd(1) = i
      !         dstAdd(2) = j
      !         dstAdd(3) = iDE
      !         weights = 1.e+20
      !         do n=1,noverlap_src_DEs

      !            ib_src = beginning index in 1st dir of exclusive domain on src DE
      !            ie_src = ending    index in 1st dir of exclusive domain on src DE
      !            jb_src = beginning index in 2nd dir of exclusive domain on src DE
      !            je_src = ending    index in 2nd dir of exclusive domain on src DE

      !            do jjj=jb_src,je_src
      !            do iii=ib_src,ie_src
               
      !               x2 = src_center_x(iii,jjj,n)
      !               y2 = src_center_y(iii,jjj,n)
      !
      !               distance = ESMF_GridComputeDistance(x1,y1,x2,y2,&
      !                                               coord_system, localrc)

!
!                     check to see if this is closer than the other
!                     neighbors so far.  if so, replace one of the
!                     current neighbors
!

      !                check_loop: do nchk=1,nnbrs
      !                   if (distance < weights(nchk)) then
      !                      do inbr=num_neighbors,nchk+1,-1
      !                         srcAdd(inbr,:) = srcAdd(inbr-1,:)
      !                         weights(inbr) = weights(inbr-1)
      !                      end do
      !                      srcAdd(nchk,1) = iii
      !                      srcAdd(nchk,2) = jjj
      !                      srcAdd(nchk,3) = n
      !                      weights(nchk) = distance
      !                      exit check_loop
      !                   endif
      !                end do check_loop

       !           end do   ! i-loop on src DE
       !           end do   ! j-loop on src DE
       !        end do search_loop ! loop over source DEs
               
!
!               compute weights based on inverse distance
!               if source mask is false, eliminate those points
!

      !         dist_tot = zero
      !         do inbr=1,nnbrs
      !            iii = srcAdd(inbr,1)
      !            jjj = srcAdd(inbr,2)
      !            n   = srcAdd(inbr,3)
      !            if (src_grid_mask(iii,jjj,n)) then
      !               weights(inbr) = one/weights(inbr)
      !               dist_tot = dist_tot + weights(inbr)
      !            else
      !               srcAdd(inbr,:) = 0
      !               weights(inbr) = zero
      !            endif
      !         end do

!
!               normalize weights and store the link
!               if dist_tot = 0, all neighbors were masked so skip
!

      !         if (dist_tot /= zero) then
      !            do inbr=1,nnbrs
      !               if (srcAdd(inbr,1) /= 0) then
      !                  wgtstmp(1) = weights(inbr)/dist_tot
      !                  call ESMF_RegridAddLink(&
      !                           ESMF_RegridConsByFieldNearNbr, &
      !                           srcAdd, dstAdd, wgtstmp(1), rc)
      !               endif
      !            end do
      !         endif

        !    endif ! dst mask            
        ! end do ! i loop on dst grid DE
        ! end do ! j loop on dst grid DE
      !end do    ! loop over local dst DEs
      
      !deallocate(src_center_x, src_center_y)
      !deallocate(srcAdd, weights, wgtstmp)
      
      end function ESMF_RegridConsByFieldNearNbr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridConsByBundleNearNbr"
!BOPI
! !IROUTINE: ESMF_RegridConsByBundleNearNbr - Constructs nearest-neighbor Regrid structure for a bundle pair

! !INTERFACE:
      function ESMF_RegridConsByBundleNearNbr(src_bundle, dst_bundle, &
                                                      name, rc)
!
! !RETURN VALUE:
      type(ESMF_RegridType) :: ESMF_RegridConsByBundleNearNbr
!
! !ARGUMENTS:

      type (ESMF_Bundle), intent(in) :: &
         src_bundle,          &! field bundle to be regridded
         dst_bundle            ! destination (incl grid) of resulting regridded bundle

      character (len = *), intent(in) :: name

      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a source field bundle and destination field bundle (and their attached
!     grids), this routine constructs a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     bundle to the destination bundle using nearest-neighbor
!     distance-weighted interpolation.
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
!EOPI

      !TODO: Insert code here
      type (ESMF_RegridType) :: temp

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! prevent compiler errors on some architectures which
      ! insist functions have a return value
      ESMF_RegridConsByBundleNearNbr = temp

 
      end function ESMF_RegridConsByBundleNearNbr

!------------------------------------------------------------------------------

   end module ESMF_RegridNearNbrMod
