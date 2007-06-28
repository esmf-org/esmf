! $Id: ESMF_StaggerLoc.F90,v 1.2 2007/06/28 22:47:10 oehmke Exp $
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
#define ESMF_FILENAME "ESMF_StaggerLoc.F90"
!
!     ESMF Stagger Location
      module ESMF_StaggerLocMod
!
!==============================================================================
!
! This file contains the Stagger Location subroutines
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_StaggerMod - Stagger class
!
! !DESCRIPTION:
!
! The code in this file implements some routines for interacting with the general stagger.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_LogErrMod


!     NEED TO ADD MORE HERE
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     ! ESMF_StaggerLoc
!

      type ESMF_StaggerLoc
      sequence
        integer :: staggerloc
      end type



!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:
  public ESMF_StaggerLoc
  public ESMF_StaggerLocSetDim

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
  public ESMF_StaggerLocSet


!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:
! 

!    
!                 2----4----2
!                 |         |
!                 |         |
!                 3    1    3
!                 |         |
!                 |         |
!                 2----4----2
! 
!      Diagram Illustrating 2D Stagger Locations
!   (Numbers correspond to bracketed numbers in list.)
!
!
! The 2D predefined stagger locations are:
!
!  ESMF_STAGGERLOC_CENTER: The center of the cell [1].
!  ESMF_STAGGERLOC_CORNER: The corners of the cell [2].
!  ESMF_STAGGERLOC_EDGE1:  The edges offset from the center in the 1st dimension [3].
!  ESMF_STAGGERLOC_EDGE2:  The edges offset from the center in the 2nd dimension [4].
!
!
!    
!      5----7----5        2----4----2         5----7----5
!      |         |        |         |         |         |
!      |         |        |         |         |         | 
!      6    8    6        3    1    3         6    8    6
!      |         |        |         |         |         |
!      |         |        |         |         |         |
!      5----7----5        2----4----2         5----7----5
!
!       Cell Top          Cell Middle         Cell Bottom
!         
!
!           Diagram Illustrating 3D Stagger Locations
!       (Numbers correspond to bracketed numbers in list.)
!
!
! The 3D predefined stagger locations are:
!
! ESMF_STAGGERLOC_CENTER_VCENTER: The center of the 3D cell [1].
! ESMF_STAGGERLOC_CORNER_VCENTER: Half way up the vertical edges of the cell [2].
! ESMF_STAGGERLOC_EDGE1_VCENTER:  The center of the face bounded by edge 1 and
!                                 the vertical dimension [3].
! ESMF_STAGGERLOC_EDGE2_VCENTER:  The center of the face bounded by edge 2 and 
!                                 the vertical dimension [4]. 
! ESMF_STAGGERLOC_CORNER_VFACE:   The corners of the 3D cell [5].
! ESMF_STAGGERLOC_EDGE1_VFACE:    The center of the edges of the 3D cell parallel
!                                 offset from the center in the 1st dimension [6].
! ESMF_STAGGERLOC_EDGE2_VFACE:    The center of the edges of the 3D cell parallel
!                                 offset from the center in the 2nd dimension [7].
! ESMF_STAGGERLOC_CENTER_VFACE:   The center of the top and bottom face. The face 
!                                 bounded by the 1st and 2nd dimensions [8]. 
!
!!!!!

   !! 2D predefined stagger locations
   type (ESMF_StaggerLoc), parameter, public ::            &
      ESMF_STAGGERLOC_CENTER        = ESMF_StaggerLoc( 0), &     
      ESMF_STAGGERLOC_EDGE1         = ESMF_StaggerLoc( 1), &  
      ESMF_STAGGERLOC_EDGE2         = ESMF_StaggerLoc( 2), &
      ESMF_STAGGERLOC_CORNER        = ESMF_StaggerLoc( 3)

   !! 3D predefined stagger locations
   type (ESMF_StaggerLoc), parameter, public ::            &
      ESMF_STAGGERLOC_CENTER_VCENTER  = ESMF_StaggerLoc( 0), &
      ESMF_STAGGERLOC_EDGE1_VCENTER   = ESMF_StaggerLoc( 1), &
      ESMF_STAGGERLOC_EDGE2_VCENTER   = ESMF_StaggerLoc( 2), &
      ESMF_STAGGERLOC_CORNER_VCENTER  = ESMF_StaggerLoc( 3), &
      ESMF_STAGGERLOC_CENTER_VFACE    = ESMF_StaggerLoc( 4), &
      ESMF_STAGGERLOC_EDGE1_VFACE     = ESMF_StaggerLoc( 5), &
      ESMF_STAGGERLOC_EDGE2_VFACE     = ESMF_StaggerLoc( 6), &
      ESMF_STAGGERLOC_CORNER_VFACE    = ESMF_StaggerLoc( 7)


!EOPI
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_StaggerLoc.F90,v 1.2 2007/06/28 22:47:10 oehmke Exp $'


!==============================================================================

      contains



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocSet"
!BOP
! !IROUTINE: ESMF_StaggerLocSet - set a stagger location to a particular position in the cell.

! !INTERFACE:
      subroutine ESMF_StaggerLocSet(stagLoc,loc,rc)
!
! !ARGUMENTS:
      type (ESMF_StaggerLoc), intent(inout) :: stagLoc
      integer, intent(in) :: loc(:)
      integer, optional :: rc 

! !DESCRIPTION:
!   Sets a custom stagger location to a position in a cell by using the array
!    {\tt loc}. The values in the array should only be 0,1. If loc(i) is 0 it 
!    means the position should be in the center in that dimension. If loc(i) is 1 then
!    for dimension i, the position should be on the positive side of the cell. 
!    Using this scheme, {\tt ESMF\_StaggerLoc} should be able to specify
!    any of the common grid positions.  
!     The arguments are:
!     \begin{description}
!     \item[stagLoc]
!          Grid location to be initialized
!     \item[loc]
!          Array holding position data, note that dimensions beyond
!          those specified in loc are set to 0. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO
      integer :: i,sl,loc_size

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      !TODO: error checking here

      ! set stagger location value
      sl=0
      loc_size=size(loc)
      do i=1,loc_size
         sl=sl+loc(i)*2**(i-1)
      enddo

     ! Set stagger location value
      stagLoc%staggerloc=sl 

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StaggerLocSet


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocSetDim"
!BOP
! !IROUTINE: ESMF_StaggerLocSetDim - set one dimension of a stagger location to a particular position.

! !INTERFACE:
      subroutine ESMF_StaggerLocSetDim(stagLoc,dim,loc,rc)
!
! !ARGUMENTS:
      type (ESMF_StaggerLoc), intent(inout) :: stagLoc
      integer, intent(in) :: dim,loc
      integer, optional :: rc 

! !DESCRIPTION:
!   Sets a particular dimension of a custom stagger location to a position in a cell 
!    by using the variable {\tt loc}. The variable {\tt loc} should only be 0,1. 
!    If {\tt loc} is 0 it means the position 
!    should be in the center in that dimension. If {\tt loc} is +1 then
!    for the dimension, the position should be on the positive side of the cell. 
!    Using this scheme, {\tt ESMF\_StaggerLoc} should be able to specify
!    any of the common grid positions.  
!     The arguments are:
!     \begin{description}
!     \item[stagLoc]
!          Stagger location to be initialized
!     \item[dim]
!          Dimension to be changed (1-7).
!     \item[loc]
!          Position data should be either 0,+1.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

     ! Initialize return code; assume routine not implemented
     if (present(rc)) rc = ESMF_RC_NOT_IMPL

     !TODO: error checking here


     ! Set stagger location value
     stagLoc%staggerloc=loc*2**(dim-1)


     ! Set return values.
     if (present(rc)) rc = ESMF_SUCCESS


     end subroutine ESMF_StaggerLocSetDim



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocCustom"
!BOPI
! !IROUTINE: ESMF_StaggerLocCustom - create a custom stagger location which doesn't necessarily correspond to one of the standard points in a cell.

! !INTERFACE:
      subroutine ESMF_StaggerLocCustom(stagLoc,label,offsets,rc)
!
! !ARGUMENTS:
      type (ESMF_StaggerLoc), intent(inout) :: stagLoc
      integer , intent(in) :: label
      integer, intent(in),optional :: offsets(:)
      integer, optional :: rc 

! !DESCRIPTION:
!     Creates a custom stagger location which doesn't necessarily correspond to    
!     one of the standard locations in the cell. This is useful if you want to 
!     put data at a non-standard location. Note that because of their ambiguous location
!     these staggers won't be able to be used in some ESMF functionality. 
!
!     The arguments are:
!     \begin{description}
!     \item[stagLoc]
!          Grid location to be initialized
!     \item[label]
!          An integer which distinguishes this custom stagger from others. 
!     \item[offsets]
!          An array containing +1,0 which describes how this stagger should
!          be positioned in index space.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL


      end subroutine ESMF_StaggerLocCustom



      end module ESMF_StaggerLocMod

