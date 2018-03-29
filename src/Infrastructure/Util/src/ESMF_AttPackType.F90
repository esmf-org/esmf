! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
#define ESMF_FILENAME "ESMF_AttPackType.F90"
!
!     ESMF Stagger Location
      module ESMF_AttPackTypeMod
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
      use ESMF_UtilTypesMod
      use ESMF_InitMacrosMod    ! ESMF base class

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     ! ESMF_StaggerLoc
!

  type ESMF_AttPack
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
  !private
    type(ESMF_Pointer) :: this = ESMF_Pointer(0)
    ESMF_INIT_DECLARE
  end type




!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:
  public ESMF_AttPack
  public ESMF_AttributeGetInit

!EOPI
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'


!==============================================================================

      contains


    function ESMF_AttributeGetInit(attpack)
      type(ESMF_AttPack), intent(in), optional :: attpack
      ESMF_INIT_TYPE :: ESMF_AttributeGetInit
    
      if (present(attpack)) then
          ESMF_AttributeGetInit=ESMF_INIT_GET(attpack)
      else
          ESMF_AttributeGetInit=ESMF_INIT_CREATED
      endif
    
    end function ESMF_AttributeGetInit

end module ESMF_AttPackTypeMod
