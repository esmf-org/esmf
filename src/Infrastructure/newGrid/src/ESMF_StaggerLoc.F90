! $Id: ESMF_StaggerLoc.F90,v 1.2 2007/05/23 21:17:50 oehmke Exp $
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
!     ESMF Grid Generate Module
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
      use ESMF_BaseMod        ! ESMF base class
      use ESMF_IOSpecMod      ! ESMF I/O class
      use ESMF_LogErrMod
      use ESMF_LocalArrayMod  ! ESMF local array class
      
!     NEED TO ADD MORE HERE
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:



!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_StaggerLoc.F90,v 1.2 2007/05/23 21:17:50 oehmke Exp $'


!==============================================================================

      contains


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocInit"
!BOP
! !IROUTINE: ESMF_StaggerLocInit - Initialize a stagger location to center.

! !INTERFACE:
      subroutine ESMF_StaggerLocInit(stagLoc,rc)
!
! !ARGUMENTS:
      type (ESMF_StaggerLoc), intent(inout) :: stagLoc
      integer, optional :: rc 

! !DESCRIPTION:
!   Sets a stagger location to center.
!
!     The arguments are:
!     \begin{description}
!     \item[stagLoc]
!          Stagger location to be initialized
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO


      end subroutine ESMF_StaggerLocInit


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocSet"
!BOP
! !IROUTINE: ESMF_StaggerLocSet - set a stagger location to a particular position in the cell.

! !INTERFACE:
      subroutine ESMF_StaggerLocSet(stagLoc,where,rc)
!
! !ARGUMENTS:
      type (ESMF_StaggerLoc), intent(inout) :: stagLoc
      integer, intent(in) :: where(:)
      integer, optional :: rc 

! !DESCRIPTION:
!   Sets a custom stagger location to a position in a cell by using the array
!    {\tt where}. The values in the array should only be 0,1. If where(i) is 0 it 
!    means the position should be in the center in that dimension. If where(i) is 1 then
!    for dimension i, the position should be on the positive side of the cell. 
!    Using this scheme, {\tt ESMF\_StaggerLoc} should be able to specify
!    any of the common grid positions.  
!     The arguments are:
!     \begin{description}
!     \item[stagLoc]
!          Grid location to be initialized
!     \item[where]
!          Array holding position data, note that dimensions beyond
!          those specified in where are set to 0. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end subroutine ESMF_StaggerLocSet


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocSetDim"
!BOP
! !IROUTINE: ESMF_StaggerLocSetDim - set one dimension of a stagger location
to a particular position.

! !INTERFACE:
      subroutine ESMF_StaggerLocSetDim(stagLoc,dim,where,rc)
!
! !ARGUMENTS:
      type (ESMF_StaggerLoc), intent(inout) :: stagLoc
      integer, intent(in) :: dim,where
      integer, optional :: rc 

! !DESCRIPTION:
!   Sets a particular dimension of a custom stagger location to a position in a cell 
!    by using the variable {\tt where}. The variable {\tt where} should only be 0,1. 
!    If {\tt where} is 0 it means the position 
!    should be in the center in that dimension. If {\tt where} is +1 then
!    for the dimension, the position should be on the positive side of the cell. 
!    Using this scheme, {\tt ESMF\_StaggerLoc} should be able to specify
!    any of the common grid positions.  
!     The arguments are:
!     \begin{description}
!     \item[stagLoc]
!          Stagger location to be initialized
!     \item[dim]
!          Dimension to be changed.
!     \item[where]
!          Position data should be either -1,0,+1.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

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
!          An array containing +1,0,-1 which describes how this stagger should
!          be positioned in index space.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      end subroutine ESMF_StaggerLocCustom



      end module ESMF_StaggerLocMod

